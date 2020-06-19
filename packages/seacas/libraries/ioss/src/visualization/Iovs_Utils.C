
#include <Iovs_Utils.h>
#include <cstring>
#include <Ioss_Utils.h>
#include <ParaViewCatalystIossAdapter.h>
#include <fstream>

#ifdef IOSS_DLOPEN_ENABLED
#include <dlfcn.h>
#endif

#include <libgen.h>
#include <sys/stat.h>

namespace Iovs {

    Utils::Utils() {
        this->dlHandle = nullptr;
        this->numCGNSCatalystOutputs = 0;
        this->numExodusCatalystOutputs = 0;
    }

    Utils::~Utils() {
#ifdef IOSS_DLOPEN_ENABLED
        if (this->dlHandle != nullptr) {
            dlclose(this->dlHandle);
        }
#endif
    }

    ParaViewCatalystIossAdapterBase *
    Utils::createParaViewCatalystIossAdapterInstance() {
        void* dlh = this->getDlHandle();

        if(!dlh) {
            return nullptr;
        }

        typedef ParaViewCatalystIossAdapterBase
            *(*PvCatSrrAdapterMakerFuncType)();

#ifdef __GNUC__
        __extension__
#endif
        PvCatSrrAdapterMakerFuncType mkr = \
            reinterpret_cast<PvCatSrrAdapterMakerFuncType>(\
                dlsym(dlh, "ParaViewCatalystIossAdapterCreateInstance"));
        if (mkr == nullptr) {
            throw std::runtime_error("dlsym call failed to load function "
                "'ParaViewCatalystIossAdapterCreateInstance'");
        }
        return (*mkr)();
    }

    ParaViewCatalystCGNSAdapterBase *
    Utils::createParaViewCatalystCGNSAdapterInstance() {
        void* dlh = this->getDlHandle();

        if(!dlh) {
            return nullptr;
        }

        typedef ParaViewCatalystCGNSAdapterBase
            *(*PvCatSrrAdapterMakerFuncType)();

#ifdef __GNUC__
        __extension__
#endif
        PvCatSrrAdapterMakerFuncType mkr =\
            reinterpret_cast<PvCatSrrAdapterMakerFuncType>(\
                dlsym(dlh, "ParaViewCatalystCGNSAdapterCreateInstance"));
        if (mkr == nullptr) {
            throw std::runtime_error("dlsym call failed to load function "
                "'ParaViewCatalystCGNSAdapterCreateInstance'");
        }
        return (*mkr)();
    }

    bool Utils::fileExists(const std::string &filepath) {
        struct stat buffer {};
        return (stat(filepath.c_str(), &buffer) == 0);
    }

    std::string Utils::getExodusDatabaseOutputFilePath(
        const std::string & inputDeckName,
            const Ioss::PropertyManager &properties) {
        return this->getDatabaseOutputFilePath(inputDeckName,
            this->numExodusCatalystOutputs, properties);
    }

    std::string Utils::getCGNSDatabaseOutputFilePath(
        const std::string & inputDeckName,
            const Ioss::PropertyManager &properties) {
        return this->getDatabaseOutputFilePath(inputDeckName,
            this->numCGNSCatalystOutputs, properties);
    }

    std::string Utils::getDatabaseOutputFilePath(
        const std::string & inputDeckName,
            int numberOfCatalystBlocks,
                const Ioss::PropertyManager &properties) {
        if (!properties.exists("CATALYST_OUTPUT_DIRECTORY")) {
            std::ostringstream s;
            s << inputDeckName << "." << numberOfCatalystBlocks
                << CATALYST_FILE_SUFFIX;
            return std::string(CATALYST_OUTPUT_DIRECTORY) + "/" + s.str();
        }
        else {
            return inputDeckName;
        }
    }

    int Utils::parseCatalystFile(const std::string &filepath,
        std::string &json_result) {

        ParaViewCatalystIossAdapterBase *pvc = nullptr;
        pvc = this->createParaViewCatalystIossAdapterInstance();
        CatalystParserInterface::parse_info pinfo;

        int ret = pvc->parseFile(filepath, pinfo);
        json_result = pinfo.json_result;

        delete pvc;
        return ret;
    }

    void* Utils::getDlHandle() {
        if(this->dlHandle == nullptr) {
            loadPluginLibrary();
        }
        return this->dlHandle;
    }

    void Utils::loadPluginLibrary() {

        std::string pluginLibraryPath = this->getCatalystPluginPath();

#ifdef IOSS_DLOPEN_ENABLED
        this->dlHandle = dlopen(pluginLibraryPath.c_str(),
            RTLD_NOW | RTLD_GLOBAL);
        if (this->dlHandle == nullptr) {
            throw std::runtime_error(dlerror());
        }
#else
        this->dlHandle = nullptr;
#endif
    }

    std::string Utils::getCatalystPluginPath() {

        if (getenv("CATALYST_PLUGIN") != nullptr) {
            return getenv("CATALYST_PLUGIN");
        }

        std::string catalystInsDir =\
            this->getCatalystAdapterInstallDirectory();

        if(!catalystInsDir.empty()) {
            return catalystInsDir + "/lib/" +\
                std::string(CATALYST_PLUGIN_DYNAMIC_LIBRARY);
        }

        return this->getSierraInstallDirectory() + "/" +\
            std::string(CATALYST_PLUGIN_DYNAMIC_LIBRARY);
    }

    std::string Utils::getCatalystPythonDriverPath() {

        std::string catalystInsDir =\
            this->getCatalystAdapterInstallDirectory();

        if(!catalystInsDir.empty()) {
            return catalystInsDir + "/python/" +\
                std::string(CATALYST_PLUGIN_PYTHON_MODULE);
        }

        return this->getSierraInstallDirectory() + "/" +\
            std::string(CATALYST_PLUGIN_PYTHON_MODULE);
    }

    std::string Utils::getSierraInstallDirectory() {

        std::string sierraInsDir;
        if (getenv("SIERRA_INSTALL_DIR") != nullptr) {
            sierraInsDir = getenv("SIERRA_INSTALL_DIR");
        }
        else {
            std::ostringstream errmsg;
            errmsg << "Environment variable SIERRA_INSTALL_DIR not set.\n"
                << " Unable to find ParaView catalyst dynamic library.\n";
            IOSS_ERROR(errmsg);
        }

        std::string sierraSystem;
        if (getenv("SIERRA_SYSTEM") != nullptr) {
            sierraSystem = getenv("SIERRA_SYSTEM");
        }
        else {
            std::ostringstream errmsg;
            errmsg << "Environment variable SIERRA_SYSTEM not set.\n"
                << " Unable to find ParaView catalyst dynamic library.\n";
            IOSS_ERROR(errmsg);
        }

        std::string sierraVersion;
        if (getenv("SIERRA_VERSION") != nullptr) {
            sierraVersion = getenv("SIERRA_VERSION");
        }
        else {
            std::ostringstream errmsg;
            errmsg << "Environment variable SIERRA_VERSION not set.\n"
                << " Unable to find ParaView catalyst dynamic library.\n";
            IOSS_ERROR(errmsg);
        }

        char* cbuf = realpath(sierraInsDir.c_str(), nullptr);
        std::string sierraInsPath = cbuf;
        free(cbuf);

        if (!fileExists(sierraInsPath)) {
            std::ostringstream errmsg;
            errmsg << "SIERRA_INSTALL_DIR directory does not exist.\n"
                << "Directory path: " << sierraInsPath << "\n"
                    << " Unable to find ParaView catalyst dynamic library.\n";
            IOSS_ERROR(errmsg);
        }

        char *cbase = strdup(sierraInsPath.c_str());
        char *cdir  = strdup(sierraInsPath.c_str());
        char *bname = basename(cbase);
        char *dname = dirname(cdir);

        while (strcmp(dname, "/") != 0 &&\
            strcmp(dname, ".") != 0 &&\
                strcmp(bname, "sierra") != 0) {
            bname = basename(dname);
            dname = dirname(dname);
        }

        if (strcmp(bname, "sierra") == 0) {
            sierraInsPath = dname;
        }

        free(cbase);
        free(cdir);

        return sierraInsPath + "/" + CATALYST_PLUGIN_PATH + "/" +\
            sierraSystem + "/" + sierraVersion;
    }

    std::string Utils::getCatalystAdapterInstallDirectory() {
        std::string catalystInsDir = "";
        if (getenv("CATALYST_ADAPTER_INSTALL_DIR") != nullptr) {
            std::string catalystInsDir = getenv("CATALYST_ADAPTER_INSTALL_DIR");

            if (!fileExists(catalystInsDir)) {
                std::ostringstream errmsg;
                errmsg << "CATALYST_ADAPTER_INSTALL_DIR directory does\n"
                    << "not exist. Directory path: " << catalystInsDir << "\n"
                       << "Unable to find ParaView catalyst dynamic library.\n";
                IOSS_ERROR(errmsg);
            }
            return catalystInsDir;
        }
    }

    void Utils::checkDbUsage(Ioss::DatabaseUsage db_usage) {
        std::ostringstream errmsg;
        if (db_usage == Ioss::WRITE_HEARTBEAT) {
            errmsg << "ParaView catalyst database type cannot be"
                << " used in a HEARTBEAT block.\n";
            IOSS_ERROR(errmsg);
        }
        else if (db_usage == Ioss::WRITE_HISTORY) {
            errmsg << "ParaView catalyst database type cannot be"
                << " used in a HISTORY block.\n";
            IOSS_ERROR(errmsg);
        }
        else if (db_usage == Ioss::READ_MODEL || \
            db_usage == Ioss::READ_RESTART) {

            errmsg << "ParaView catalyst database type cannot be"
                << " used to read a model.\n";
            IOSS_ERROR(errmsg);
        }
    }

    void Utils::createDatabaseOutputFile(const std::string &filename,
        MPI_Comm communicator) {

        std::ostringstream errmsg;
        int rank;
        MPI_Comm_rank(communicator, &rank);
        if (rank == 0) {
            if (!Utils::fileExists(filename)) {
                std::ofstream output_file;
                output_file.open(filename.c_str(),
                    std::ios::out | std::ios::trunc);

                if (!output_file) {
                    errmsg << "Unable to create output file: "
                        << filename << ".\n";
                    IOSS_ERROR(errmsg);
                }
                output_file.close();
            }
        }
    }

    void Utils::incrementNumCGNSCatalystOutputs() {
        this->numCGNSCatalystOutputs++;
    }

    void Utils::incrementNumExodusCatalystOutputs() {
        this->numExodusCatalystOutputs++;
    }

} // namespace Iovs
