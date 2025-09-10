#pragma once

#include <aws/core/Aws.h>
#include <aws/s3/S3Client.h>
#include <aws/transfer/TransferManager.h>

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace Ios3 {
  namespace helpers {

    struct HelperParameters
    {
      std::string endpoint{""};
      std::string profile{""};
      std::string ca_file{""};
      bool        use_ca_file{false};
      bool        use_transfer_manager{false};
      bool        enable_aws_tracing{false};
      bool        disable_ec2_lookup{true};
    };

    struct HelperContext
    {
      bool use_transfer_manager;

      // The Aws::SDKOptions struct contains SDK configuration options.
      // An instance of Aws::SDKOptions is passed to the Aws::InitAPI and
      // Aws::ShutdownAPI methods.  The same instance should be sent to both methods.
      Aws::SDKOptions options;

      std::shared_ptr<Aws::S3::S3Client>                           client;
      std::shared_ptr<Aws::Utils::Threading::PooledThreadExecutor> executor;
      std::shared_ptr<Aws::Transfer::TransferManager>              transfer_manager;
    };

    // From
    // https://stackoverflow.com/questions/21028299/is-this-behavior-of-vectorresizesize-type-n-under-c11-and-boost-container/21028912#21028912
    //
    // Allocator adaptor that interposes construct() calls to
    // convert value initialization into default initialization.
    template <typename T, typename A = std::allocator<T>> class default_init_allocator : public A
    {
      typedef std::allocator_traits<A> a_t;

    public:
      template <typename U> struct rebind
      {
        using other = default_init_allocator<U, typename a_t::template rebind_alloc<U>>;
      };

      using A::A;

      template <typename U>
      void construct(U *ptr) noexcept(std::is_nothrow_default_constructible<U>::value)
      {
        ::new (static_cast<void *>(ptr)) U;
      }
      template <typename U, typename... Args> void construct(U *ptr, Args &&...args)
      {
        a_t::construct(static_cast<A &>(*this), ptr, std::forward<Args>(args)...);
      }
    };

    // A vector that doesn't initialize new elements when resized.
    template <typename T> using UninitializedVector = std::vector<T, default_init_allocator<T>>;

    void print_params(const HelperParameters &params);

    std::string cleanBucketName(const std::string &name);

    void getParamsFromEnvVars(HelperParameters &params);

    std::shared_ptr<HelperContext> createContext(const HelperParameters &params);

    void destroyContext(std::shared_ptr<HelperContext> context);

    int createBucket(std::shared_ptr<HelperContext> context, const std::string &bucket);

    int waitBucket(std::shared_ptr<HelperContext> context, const std::string &bucket,
                   uint64_t wait_usec);

    int deleteBucket(std::shared_ptr<HelperContext> context, const std::string &bucket);

    int listBuckets(std::shared_ptr<HelperContext> context, std::vector<std::string> &bucket_names);

    template <typename T, typename A>
    int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, const std::vector<T, A> &value);

    template <typename T, typename A>
    int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, std::vector<T, A> &value);

    int putValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, const std::string &filename);

    int getValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key, std::string &filename);

    int deleteValue(std::shared_ptr<HelperContext> context, const std::string &bucket,
                    const std::string &key);

    int listKeys(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 std::vector<std::string> &keys);

    int listKeys(std::shared_ptr<HelperContext> context, const std::string &bucket,
                 const std::string &key_prefix, std::vector<std::string> &keys);

    int putBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket,
                        const std::string &policy);

    int getBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket,
                        std::string &policy);

    int deleteBucketPolicy(std::shared_ptr<HelperContext> context, const std::string &bucket);

  } // namespace helpers
} // namespace Ios3
