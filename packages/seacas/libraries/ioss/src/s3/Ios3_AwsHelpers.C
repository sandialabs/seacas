#include <getopt.h>
#include <stdlib.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <numeric>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#include <aws/core/Aws.h>
#include <aws/core/auth/AWSCredentialsProvider.h>
#include <aws/core/auth/AWSCredentialsProviderChain.h>
#include <aws/core/utils/logging/LogLevel.h>
#include <aws/core/utils/stream/PreallocatedStreamBuf.h>

#include <aws/s3/S3Client.h>

#include <aws/s3/model/CreateBucketRequest.h>
#include <aws/s3/model/DeleteBucketPolicyRequest.h>
#include <aws/s3/model/DeleteBucketRequest.h>
#include <aws/s3/model/DeleteObjectRequest.h>
#include <aws/s3/model/GetBucketPolicyRequest.h>
#include <aws/s3/model/GetObjectRequest.h>
#include <aws/s3/model/HeadBucketRequest.h>
#include <aws/s3/model/ListObjectsRequest.h>
#include <aws/s3/model/PutBucketPolicyRequest.h>
#include <aws/s3/model/PutObjectRequest.h>

#include <aws/core/utils/threading/Executor.h>
#include <aws/transfer/TransferHandle.h>
#include <aws/transfer/TransferManager.h>

#include "Ios3_AwsHelpers.h"

namespace Ios3 {
  namespace helpers {

    void print_params(const HelperParameters &params)
    {
      std::cout << "use_ca_file = " << params.use_ca_file << std::endl;
      std::cout << "ca_file     = " << params.ca_file << std::endl;
      std::cout << "endpoint    = " << params.endpoint << std::endl;
      std::cout << "profile     = " << params.profile << std::endl;
    }

    static const char AwsHelperCredentialsProviderChainTag[] = "AwsHelperCredentialsProviderChain";

    /**
     * Creates an AWSCredentialsProviderChain which only uses EnvironmentAWSCredentialsProvider
     * and ProfileConfigFileAWSCredentialsProvider in that order.
     */
    class AwsHelperCredentialsProviderChain : public Aws::Auth::AWSCredentialsProviderChain
    {
    public:
      /**
       * Initializes the provider chain with EnvironmentAWSCredentialsProvider
       * and ProfileConfigFileAWSCredentialsProvider in that order.
       */
      AwsHelperCredentialsProviderChain() : AWSCredentialsProviderChain()
      {
        AddProvider(Aws::MakeShared<Aws::Auth::EnvironmentAWSCredentialsProvider>(
            AwsHelperCredentialsProviderChainTag));
        AddProvider(Aws::MakeShared<Aws::Auth::ProfileConfigFileAWSCredentialsProvider>(
            AwsHelperCredentialsProviderChainTag));
        // AddProvider(Aws::MakeShared<ProcessCredentialsProvider>(DefaultCredentialsProviderChainTag));
        // AddProvider(Aws::MakeShared<STSAssumeRoleWebIdentityCredentialsProvider>(DefaultCredentialsProviderChainTag));
        // AddProvider(Aws::MakeShared<SSOCredentialsProvider>(DefaultCredentialsProviderChainTag));
      }
      /**
       * Initializes the provider chain with EnvironmentAWSCredentialsProvider
       * and ProfileConfigFileAWSCredentialsProvider in that order.
       */
      AwsHelperCredentialsProviderChain(const char *profile) : AWSCredentialsProviderChain()
      {
        AddProvider(Aws::MakeShared<Aws::Auth::EnvironmentAWSCredentialsProvider>(
            AwsHelperCredentialsProviderChainTag));
        AddProvider(Aws::MakeShared<Aws::Auth::ProfileConfigFileAWSCredentialsProvider>(
            AwsHelperCredentialsProviderChainTag, profile));
        // AddProvider(Aws::MakeShared<ProcessCredentialsProvider>(DefaultCredentialsProviderChainTag));
        // AddProvider(Aws::MakeShared<STSAssumeRoleWebIdentityCredentialsProvider>(DefaultCredentialsProviderChainTag));
        // AddProvider(Aws::MakeShared<SSOCredentialsProvider>(DefaultCredentialsProviderChainTag));
      }
    };

    static int             context_count = 0;
    static Aws::SDKOptions options;

    void getParamsFromEnvVars(HelperParameters &params)
    {
      const std::string env_base_name{"IOSS_S3_"};
      const std::string env_name_endpoint{env_base_name + "ENDPOINT"};
      const std::string env_name_profile{env_base_name + "PROFILE"};
      const std::string env_name_ca_file{env_base_name + "CA_FILE"};
      const std::string env_name_use_ca_file{env_base_name + "USE_CA_FILE"};
      const std::string env_name_use_transfer_manager{env_base_name + "USE_TRANSFER_MANAGER"};
      const std::string env_name_enable_aws_tracing{env_base_name + "ENABLE_AWS_TRACING"};
      const std::string env_name_disable_ec2_lookup{env_base_name + "DISABLE_EC2_LOOKUP"};

      char *envvar;

      envvar = std::getenv(env_name_endpoint.c_str());
      if (envvar != nullptr) {
        params.endpoint = std::string(envvar);
      }
      envvar = std::getenv(env_name_profile.c_str());
      if (envvar != nullptr) {
        params.profile = std::string(envvar);
      }
      envvar = std::getenv(env_name_ca_file.c_str());
      if (envvar != nullptr) {
        params.ca_file = std::string(envvar);
      }
      envvar = std::getenv(env_name_use_ca_file.c_str());
      if (envvar != nullptr) {
        params.use_ca_file = envvar[0] == '0' ? false : true;
      }
      envvar = std::getenv(env_name_use_transfer_manager.c_str());
      if (envvar != nullptr) {
        params.use_transfer_manager = envvar[0] == '0' ? false : true;
      }
      envvar = std::getenv(env_name_enable_aws_tracing.c_str());
      if (envvar != nullptr) {
        params.enable_aws_tracing = envvar[0] == '0' ? false : true;
      }
      envvar = std::getenv(env_name_disable_ec2_lookup.c_str());
      if (envvar != nullptr) {
        params.disable_ec2_lookup = envvar[0] == '0' ? false : true;
      }
    }

    std::shared_ptr<HelperContext> createContext(const HelperParameters &params)
    {
      std::shared_ptr<HelperContext> context = std::make_shared<HelperContext>();

      context->use_transfer_manager = params.use_transfer_manager;

      // print_params(params);

      if (params.disable_ec2_lookup) {
        // AWS tries to call out to an EC2 server.  If your S3 service doesn't
        // have EC2, setting this envvar will eliminate a 1 second
        // startup delay while it waits for the connection to timeout.
        setenv("AWS_EC2_METADATA_DISABLED", "true", 1 /* overwrite */);
      }

      // this is not thread safe
      if (++context_count == 1) {
        // If the object store closes our connections, a sigpipe is
        // generated inside Curl that isn't handled by Curl.  This
        // option installs an AWS sigpipe handler that prevents an
        // exit due to the unhandled signal.
        options.httpOptions.installSigPipeHandler = true;
        if (params.enable_aws_tracing) {
          options.loggingOptions.logLevel = Aws::Utils::Logging::LogLevel::Trace;
        }
        InitAPI(options);
      }

      Aws::Client::ClientConfiguration config;
      config.endpointOverride = params.endpoint;
      config.profileName      = params.profile;
      // config.disableExpectHeader = true;
      config.requestTimeoutMs = 100000;
      if (params.use_ca_file) {
        config.caFile = params.ca_file;
      }

      // This creates a credentials provider chain that first looks for
      // the AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment
      // variables and then looks for a specific profile from the user's
      // config files ($HOME/.aws/credentials).  The default provider is a
      // provider chain that instantiates all the potential providers with
      // the default constructor.  It doesn't seem to use the
      // ClientConfiguration parameters, so it always looks for the
      // "default" profile.  This allows us to specify the provider we
      // want and any parameters we want.
      auto credProvider = Aws::MakeShared<AwsHelperCredentialsProviderChain>(
          "CredProvider", params.profile.c_str());

      context->client = Aws::MakeShared<Aws::S3::S3Client>(
          "S3Client", credProvider, config,
          Aws::Client::AWSAuthV4Signer::PayloadSigningPolicy::Never,
          false /* disable virtual addressing - otherwise client requests timeout */);

      context->executor =
          Aws::MakeShared<Aws::Utils::Threading::PooledThreadExecutor>("executor", 25);

      return context;
    }

    void destroyContext(std::shared_ptr<HelperContext> context)
    {
      context->transfer_manager.reset();
      context->client.reset();
      ;

      // this is not thread safe
      if (--context_count == 0) {
        ShutdownAPI(options);
      }

      // destroy the context held by the shared pointer
      context.reset();
    }

    // derived from example found here: https://en.cppreference.com/w/cpp/string/byte/tolower
    std::string tolower(std::string s)
    {
      std::transform(s.begin(), s.end(), s.begin(),
                     [](unsigned char c) { return std::tolower(c); });
      return s;
    }

    std::string replace_aws_illegal_chars(std::string s)
    {
      std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        char legal = c;
        if (!std::isalpha(c) && !std::isdigit(c) && c != '-') {
          legal = '-';
        }
        return legal;
      });
      return s;
    }

    std::string cleanBucketName(const std::string &name)
    {
      std::string trimmed_name{name};
      auto        pos = name.rfind("/");
      if (pos != std::string::npos) {
        trimmed_name = name.substr(pos + 1);
      }

      std::string::size_type n     = 0;
      std::string            valid = trimmed_name;

      valid = tolower(valid);

      // '.' is discouraged so replace with '-'
      n = valid.find('.');
      while (n != std::string::npos) {
        valid.at(n) = '-';
        n           = valid.find('.', n);
      }
      // can't start with "xn--"
      if (valid.find("xn--") == 0) {
        valid = 'x' + valid;
      }
      // can't end with "--s3alias"
      n = valid.rfind("--s3alias");
      if (n + 9 == valid.length()) {
        valid = valid.substr(0, n);
      }
      // can't end with "--ol-s3"
      n = valid.rfind("--ol-s3");
      if (n + 7 == valid.length()) {
        valid = valid.substr(0, n);
      }

      if (valid.length() < 3) {
        // minimum bucket name length is 3 chars
        valid = valid + "-zzz";
      }
      else if (valid.length() > 63) {
        // maximum bucket name length is 63 chars
        valid = valid.substr(0, 63);
      }

      // must start with a letter or number
      if (!std::isalnum(valid.front())) {
        if (valid.length() < 63) {
          valid = '0' + valid;
        }
        else {
          valid.front() = '0';
        }
      }
      // must end with a letter or number
      if (!std::isalnum(valid.back())) {
        if (valid.length() < 63) {
          valid = valid + '9';
        }
        else {
          valid.back() = '9';
        }
      }

      valid = replace_aws_illegal_chars(valid);

      return valid;
    }

    int createBucket(std::shared_ptr<HelperContext> context, const std::string &bucket)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::CreateBucketRequest request;
      request.SetBucket(clean_bucket_name);

      Aws::S3::Model::CreateBucketOutcome outcome = context->client->CreateBucket(request);
      if (!outcome.IsSuccess()) {
        auto err = outcome.GetError();
        std::cout << "Error: CreateBucket: " << err.GetExceptionName() << ": " << err.GetMessage()
                  << std::endl;
        rc = 1;
      }

      return rc;
    }

    int waitBucket(std::shared_ptr<HelperContext> context, const std::string &bucket,
                   uint64_t wait_usec)
    {
      int rc = 1;

      std::string clean_bucket_name = cleanBucketName(bucket);

      // convert microseconrds to nanoseconds then divide into 10 iterations
      uint64_t per_sleep_nsec = (wait_usec * 1000) / 10;
      unsigned timeoutCount   = 0;
      while (timeoutCount++ < 10) {
        Aws::S3::Model::HeadBucketRequest headBucketRequest;
        headBucketRequest.SetBucket(clean_bucket_name);
        Aws::S3::Model::HeadBucketOutcome headBucketOutcome =
            context->client->HeadBucket(headBucketRequest);
        if (headBucketOutcome.IsSuccess()) {
          rc = 0;
          break;
        }
        std::this_thread::sleep_for(std::chrono::nanoseconds(per_sleep_nsec));
      }

      return rc;
    }

    int deleteBucket(std::shared_ptr<HelperContext> context, const std::string &bucket)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::DeleteBucketRequest request;
      request.SetBucket(clean_bucket_name);

      Aws::S3::Model::DeleteBucketOutcome outcome = context->client->DeleteBucket(request);
      if (!outcome.IsSuccess()) {
        auto err = outcome.GetError();
        std::cout << "Error: DeleteBucket: " << err.GetExceptionName() << ": " << err.GetMessage()
                  << std::endl;
        rc = 1;
      }

      return rc;
    }

    int listBuckets(std::shared_ptr<HelperContext> context, std::vector<std::string> &bucket_names)
    {
      int rc = 0;

      auto outcome = context->client->ListBuckets();
      if (outcome.IsSuccess()) {
        auto buckets = outcome.GetResult().GetBuckets();
        for (auto &&b : buckets) {
          bucket_names.push_back(b.GetName());
        }
      }
      else {
        std::cout << "Error: ListBuckets: " << outcome.GetError() << std::endl;
        rc = 1;
      }

      return rc;
    }

    template <typename T, typename A>
    int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, const std::vector<T, A> &value)
    {
      int rc = 0;

      // PreallocatedStreamBuf doesn't have a read-only behavior, so the underlying data can't be
      // const.
      std::vector<T, A>                        *vec = const_cast<std::vector<T, A> *>(&value);
      Aws::Utils::Stream::PreallocatedStreamBuf streamBuffer(vec->data(), vec->size());
      std::shared_ptr<Aws::IOStream>            input_stream =
          Aws::MakeShared<Aws::IOStream>("SomeTag", &streamBuffer);

      std::string clean_bucket_name = cleanBucketName(bucket);

      if (context->use_transfer_manager) {
        Aws::Transfer::TransferManagerConfiguration transfer_config(context->executor.get());
        const uint64_t                              MB5 = 5 * 1024 * 1024UL;
        transfer_config.bufferSize                      = MB5;
        transfer_config.s3Client                        = context->client;
        context->transfer_manager = Aws::Transfer::TransferManager::Create(transfer_config);

        std::shared_ptr<Aws::Transfer::TransferHandle> uploadHandle =
            context->transfer_manager->UploadFile(input_stream, clean_bucket_name, key,
                                                  "binary/octet-stream",
                                                  Aws::Map<Aws::String, Aws::String>());
        uploadHandle->WaitUntilFinished();
        bool success = uploadHandle->GetStatus() == Aws::Transfer::TransferStatus::COMPLETED;

        if (!success) {
          auto err = uploadHandle->GetLastError();
          std::cout << "Error: TransferManager::UploadFile: " << err.GetMessage() << std::endl;
          rc = 1;
        }
      }
      else {
        Aws::S3::Model::PutObjectRequest request;
        request.SetBucket(clean_bucket_name);
        request.SetKey(key);

        request.SetBody(input_stream);
        request.SetContentLength(value.size());
        // request.SetContentType("binary/octet-stream");

        Aws::S3::Model::PutObjectOutcome outcome = context->client->PutObject(request);
        if (!outcome.IsSuccess()) {
          std::cout << "Error: PutObject: " << outcome.GetError().GetMessage() << std::endl;
          rc = 1;
        }
      }

      return rc;
    }

    template <typename T, typename A>
    int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, std::vector<T, A> &value)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::GetObjectRequest request;
      request.SetBucket(clean_bucket_name);
      request.SetKey(key);

      Aws::S3::Model::GetObjectOutcome outcome = context->client->GetObject(request);
      if (outcome.IsSuccess()) {
        auto   &retrieved_obj_body = outcome.GetResultWithOwnership().GetBody();
        int64_t contentLength      = outcome.GetResultWithOwnership().GetContentLength();
        int64_t total_bytes_read   = 0;
        int64_t bytes_left         = contentLength;
        if (contentLength > (int64_t)value.capacity()) {
          value.reserve(contentLength);
          value.resize(contentLength);
        }
        while ((bytes_left > 0) && (total_bytes_read < (int64_t)value.capacity())) {
          int64_t bytes_read =
              retrieved_obj_body.readsome((char *)&value[total_bytes_read], bytes_left);
          total_bytes_read += bytes_read;
          bytes_left -= bytes_read;
        }
      }
      else {
        std::cout << "Error: GetObject: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, const std::string &filename)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      if (context->use_transfer_manager) {
        Aws::Transfer::TransferManagerConfiguration transfer_config(context->executor.get());
        const uint64_t                              GB5 = 5 * 1024 * 1024 * 1024UL;
        transfer_config.bufferSize                      = GB5;
        transfer_config.s3Client                        = context->client;
        context->transfer_manager = Aws::Transfer::TransferManager::Create(transfer_config);

        std::shared_ptr<Aws::Transfer::TransferHandle> uploadHandle =
            context->transfer_manager->UploadFile(filename, clean_bucket_name, key,
                                                  "binary/octet-stream",
                                                  Aws::Map<Aws::String, Aws::String>());
        uploadHandle->WaitUntilFinished();
        bool success = uploadHandle->GetStatus() == Aws::Transfer::TransferStatus::COMPLETED;

        if (!success) {
          auto err = uploadHandle->GetLastError();
          std::cout << "Error: TransferManager::UploadFile: " << err.GetMessage() << std::endl;
          rc = 1;
        }
      }
      else {
        std::cout << "Error: PutValue: Putting from a file only works "
                  << "with the transfer manager." << std::endl;
        rc = 1;
      }

      return rc;
    }

    int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, std::string &filename)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::GetObjectRequest request;
      request.SetBucket(clean_bucket_name);
      request.SetKey(key);

      Aws::S3::Model::GetObjectOutcome outcome = context->client->GetObject(request);
      if (outcome.IsSuccess()) {
        std::ofstream   obj_ofs(filename);
        auto           &retrieved_obj_body    = outcome.GetResultWithOwnership().GetBody();
        constexpr int   max_read              = 16385;
        char            object_data[max_read] = {0};
        int64_t         contentLength         = outcome.GetResultWithOwnership().GetContentLength();
        std::streamsize total_bytes_read      = 0;
        std::streamsize bytes_left            = contentLength;

        while (bytes_left > 0) {
          std::streamsize bytes_read = retrieved_obj_body.readsome(object_data, max_read);
          total_bytes_read += bytes_read;
          bytes_left -= bytes_read;
          obj_ofs.write(object_data, bytes_read);
        }
      }
      else {
        std::cout << "Error: GetObject: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int deleteValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                    const std::string &key)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::DeleteObjectRequest request;
      request.SetBucket(clean_bucket_name);
      request.SetKey(key);

      Aws::S3::Model::DeleteObjectOutcome outcome = context->client->DeleteObject(request);
      if (!outcome.IsSuccess()) {
        std::cout << "Error: DeleteObject: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int listKeys(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 std::vector<std::string> &keys)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::ListObjectsRequest request;
      request.SetBucket(clean_bucket_name);

      Aws::S3::Model::ListObjectsOutcome outcome = context->client->ListObjects(request);
      if (outcome.IsSuccess()) {
        for (auto &&b : outcome.GetResult().GetContents()) {
          keys.push_back(b.GetKey());
        }
      }
      else {
        std::cout << "Error: ListObjects: " << outcome.GetError() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int listKeys(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key_prefix, std::vector<std::string> &keys)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::ListObjectsRequest request;
      request.SetBucket(clean_bucket_name);
      request.SetPrefix(key_prefix);

      Aws::S3::Model::ListObjectsOutcome outcome = context->client->ListObjects(request);
      if (outcome.IsSuccess()) {
        for (auto &&b : outcome.GetResult().GetContents()) {
          keys.push_back(b.GetKey());
        }
      }
      else {
        std::cout << "Error: ListObjects: " << outcome.GetError() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int putBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket,
                        const std::string &policy)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      std::shared_ptr<Aws::StringStream> request_body = Aws::MakeShared<Aws::StringStream>("");
      *request_body << policy;

      Aws::S3::Model::PutBucketPolicyRequest request;
      request.SetBucket(clean_bucket_name);
      request.SetBody(request_body);

      Aws::S3::Model::PutBucketPolicyOutcome outcome = context->client->PutBucketPolicy(request);
      if (!outcome.IsSuccess()) {
        std::cout << "Error: PutBucketPolicy: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int getBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket,
                        std::string &policy)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::GetBucketPolicyRequest request;
      request.SetBucket(clean_bucket_name);

      Aws::S3::Model::GetBucketPolicyOutcome outcome = context->client->GetBucketPolicy(request);
      if (outcome.IsSuccess()) {
        outcome.GetResult().GetPolicy() >> policy;
      }
      else {
        std::cout << "Error: GetBucketPolicy: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    int deleteBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket)
    {
      int rc = 0;

      std::string clean_bucket_name = cleanBucketName(bucket);

      Aws::S3::Model::DeleteBucketPolicyRequest request;
      request.SetBucket(clean_bucket_name);

      Aws::S3::Model::DeleteBucketPolicyOutcome outcome =
          context->client->DeleteBucketPolicy(request);
      if (!outcome.IsSuccess()) {
        std::cout << "Error: DeleteBucketPolicy: " << outcome.GetError().GetMessage() << std::endl;
        rc = 1;
      }

      return rc;
    }

    /*
     * Explicit Template Instantiation
     */

    template int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                          const std::string                                               &key,
                          const std::vector<unsigned char, std::allocator<unsigned char>> &value);
    template int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                          const std::string &key, const UninitializedVector<unsigned char> &value);

    template int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                          const std::string                                         &key,
                          std::vector<unsigned char, std::allocator<unsigned char>> &value);
    template int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                          const std::string &key, UninitializedVector<unsigned char> &value);

  } // namespace helpers
} // namespace Ios3
