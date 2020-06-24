
#include <Iovs_Utils.h>
#include <cstring>
#include <Ioss_Utils.h>
#include <ParaViewCatalystIossAdapter.h>
#include <fstream>
#include <CatalystManagerBase.h>

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
        this->catalystManager = nullptr;
    }

    Utils::~Utils() {
        if(this->catalystManager) {
            delete this->catalystManager;
        }
#ifdef IOSS_DLOPEN_ENABLED
        if (this->dlHandle != nullptr) {
            dlclose(this->dlHandle);
        }
#endif
    }

    CatalystManagerBase& Utils::getCatalystManager() {
        if(this->catalystManager == nullptr) {
            this->catalystManager = this->createCatalystManagerInstance();
        }
        return *this->catalystManager;
    }

    CatalystManagerBase* Utils::createCatalystManagerInstance() {
        void* dlh = this->getDlHandle();

        if(!dlh) {
            return nullptr;
        }

        typedef CatalystManagerBase
            *(*CatalystManagerInstanceFuncType)();

#ifdef __GNUC__
        __extension__
#endif
        CatalystManagerInstanceFuncType mkr = \
            reinterpret_cast<CatalystManagerInstanceFuncType>(\
                dlsym(dlh, "CreateCatalystManagerInstance"));
        if (mkr == nullptr) {
            throw std::runtime_error("dlsym call failed to load function "
                "'CreateCatalystManagerInstance'");
        }
        return (*mkr)();
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

    CatalystExodusMeshBase* Utils::createCatalystExodusMesh(
        const std::string & databaseFilename,
            const std::string & separatorCharacter,
                const Ioss::PropertyManager & props) {

        CatalystManagerBase::CatalystExodusMeshInit cmInit;

        cmInit.resultsOutputFilename = databaseFilename;

        if (props.exists("CATALYST_BLOCK_PARSE_JSON_STRING")) {
            cmInit.catalystSierraBlockJSON = props\
                .get("CATALYST_BLOCK_PARSE_JSON_STRING").get_string();
        }

        if (props.exists("CATALYST_SCRIPT")) {
            cmInit.catalystPythonFilename = props\
                .get("CATALYST_SCRIPT").get_string();
        }
        else {
            cmInit.catalystPythonFilename = this->getCatalystPythonDriverPath();
        }

        if (props.exists("CATALYST_SCRIPT_EXTRA_FILE")) {
            cmInit.catalystSierraData.push_back(props\
                .get("CATALYST_SCRIPT_EXTRA_FILE").get_string());
        }

        cmInit.underScoreVectors = true;
        if (props.exists("CATALYST_UNDERSCORE_VECTORS")) {
            cmInit.underScoreVectors = props\
                .get("CATALYST_UNDERSCORE_VECTORS").get_int();
        }

        cmInit.applyDisplacements = true;
        if (props.exists("CATALYST_APPLY_DISPLACEMENTS")) {
            cmInit.applyDisplacements = props\
                .get("CATALYST_APPLY_DISPLACEMENTS").get_int();
        }

        if (props.exists("CATALYST_BLOCK_PARSE_INPUT_DECK_NAME")) {
            cmInit.catalystSierraInputDeckName = props\
                .get("CATALYST_BLOCK_PARSE_INPUT_DECK_NAME").get_string();
        }

        cmInit.enableLogging = false;
        if (props.exists("CATALYST_ENABLE_LOGGING")) {
           cmInit.enableLogging = props\
               .get("CATALYST_ENABLE_LOGGING").get_int();
        }

        cmInit.debugLevel = 0;
        if (props.exists("CATALYST_DEBUG_LEVEL")) {
            cmInit.enableLogging = props.get("CATALYST_DEBUG_LEVEL").get_int();
        }

        cmInit.catalystOutputDirectory = CATALYST_OUTPUT_DIRECTORY;
        if (props.exists("CATALYST_OUTPUT_DIRECTORY")) {
            cmInit.catalystOutputDirectory = props\
                .get("CATALYST_OUTPUT_DIRECTORY").get_string();
        }

        cmInit.catalystSierraSeparatorCharacter = separatorCharacter;
        cmInit.restartTag = this->getRestartTag(databaseFilename);

        this->numExodusCatalystOutputs++;
        return this->getCatalystManager().createCatalystExodusMesh(cmInit);
    }

    std::string Utils::getRestartTag(const std::string & databaseFilename) {
        std::string restartTag;
        std::string::size_type pos = databaseFilename.rfind(".e-s");
        if (pos != std::string::npos) {
            if (pos + 3 <= databaseFilename.length()) {
                restartTag = databaseFilename.substr(pos + 3, 5);
            }
        }
        return restartTag;
    }

    bool Utils::fileExists(const std::string &filepath) {
        struct stat buffer {};
        return (stat(filepath.c_str(), &buffer) == 0);
    }

    std::string Utils::getExodusDatabaseOutputFilePath(
        const std::string & databaseFilename,
            const Ioss::PropertyManager &properties) {
        return this->getDatabaseOutputFilePath(databaseFilename,
            this->numExodusCatalystOutputs, properties);
    }

    std::string Utils::getCGNSDatabaseOutputFilePath(
        const std::string & databaseFilename,
            const Ioss::PropertyManager &properties) {
        return this->getDatabaseOutputFilePath(databaseFilename,
            this->numCGNSCatalystOutputs, properties);
    }

    std::string Utils::getDatabaseOutputFilePath(
        const std::string & databaseFilename,
            int numberOfCatalystBlocks,
                const Ioss::PropertyManager &properties) {
        if (!properties.exists("CATALYST_OUTPUT_DIRECTORY")) {
            std::ostringstream s;
            s << databaseFilename << "." << numberOfCatalystBlocks
                << CATALYST_FILE_SUFFIX;
            return std::string(CATALYST_OUTPUT_DIRECTORY) + "/" + s.str();
        }
        else {
            return databaseFilename;
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
            return catalystInsDir +\
                std::string(CATALYST_INSTALL_LIB_DIR) +\
                    CATALYST_PLUGIN_DYNAMIC_LIBRARY;
        }

        return this->getSierraInstallDirectory() + "/" +\
            std::string(CATALYST_PLUGIN_DYNAMIC_LIBRARY);
    }

    std::string Utils::getCatalystPythonDriverPath() {

        std::string catalystInsDir =\
            this->getCatalystAdapterInstallDirectory();

        if(!catalystInsDir.empty()) {
            return catalystInsDir +\
                std::string(CATALYST_INSTALL_PHACTORI_DIR) +\
                    CATALYST_PLUGIN_PYTHON_MODULE;
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

} // namespace Iovs
