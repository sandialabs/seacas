#ifndef Ioss_Application_h
#define Ioss_Application_h

#include <Ioss_DatabaseIO.h>
#include <Ioss_Region.h>
#include <string>

class IossApplication {
public:

    IossApplication(int argc, char **argv,
        const std::string& appName,
            const std::string& fileTypeName,
                const std::string& iossDatabaseType,
                    const std::string& fileTypeSuffix);

    IossApplication(const std::string& appName,
        const std::string& fileTypeName,
            const std::string& iossDatabaseType,
                const std::string& fileTypeSuffix);

    ~IossApplication();

    void runApplication();
 
    bool printIOSSRegionReportON();
    void setPrintIOSSRegionReport(bool status);

    bool outputCopyOfInputDatabaseON();
    void setOutputCopyOfInputDatabase(bool status);

    bool outputCatalystMeshON();
    bool setOutputCatalystMesh(bool status);

    bool outputParsedPhactoriJSONON();
    bool setOutputParsedPhactoriJSON(bool status);

    bool usePhactoriInputScriptON();
    std::string getPhactoriInputScript();
    void setPhactoriInputScript(const std::string& scriptFilePath);

    bool usePhactoriInputJSONON();
    std::string getPhactoriInputJSON();
    void setPhactoriInputJSON(const std::string& jsonFilePath);

    bool useParaViewExportedScriptON();
    std::string getParaViewExportedScript();
    void setParaViewExportedScript(const std::string& exportedScriptFilePath);

    int getMyRank();
    int getNumRanks();
    bool isRankZero();
    bool isSerial();
    bool decomposedMeshExists();
    std::string& getApplicationName();
    std::string& getFileName();
    Ioss::Region * getInputIOSSRegion();
    void copyInputIOSSDatabaseOnRank();
    void printMessage(const std::string& message);
    void printErrorMessage(const std::string& message);
    void printIOSSRegionReportForRank();
    void exitApplicationSuccess();
    void exitApplicationFailure();

private:
    IossApplication();
    void initialize(const std::string& appName,
        const std::string& fileTypeName,
            const std::string& iossDatabaseType,
                const std::string& fileTypeSuffix);
    void callCatalystIOSSDatabaseOnRank();
    void openInputIOSSDatabase();
    void processCommandLine(int argc, char **argv);
    void initializeMPI();
    void initializeMPI(int argc, char **argv);
    void initMPIRankAndSize();
    void finalizeMPI();
    void printUsageMessage();
    void checkForOnlyOneCatalystOutputPath();
    std::string getParallelFileName();
    std::string getPhactoriDefaultJSON();
    int myRank;
    int numRanks;
    bool printIOSSReport;
    bool copyDatabase;
    bool writeCatalystMesh;
    bool writeParsedPhactoriJSON;
    bool usePhactoriInputScript;
    bool usePhactoriInputJSON;
    bool useParaViewExportedScript;
    std::string phactoriInputScriptFilePath;
    std::string phactoriInputJSONFilePath;
    std::string paraViewExportedScriptFilePath;
    std::string fileName;
    std::string applicationName;
    std::string fileTypeName;
    std::string fileTypeSuffix;
    std::string iossDatabaseType;
    std::string copyOutputDatabaseName = "iossDatabaseCopy";
    std::string outputCatalystMeshFileName = "iossDatabaseCatalystMesh.vtm";
    std::string iossReportFileName = "IossRegionReport";
    std::string phactoriJSONFileName = "phactoriJSON.json";
    Ioss::Region * inputIOSSRegion;

#if defined(__APPLE__)
    const char *CATALYST_PLUGIN_DYNAMIC_LIBRARY =\
        "libParaViewCatalystIossAdapter.dylib";
#else
    const char *CATALYST_PLUGIN_DYNAMIC_LIBRARY =\
        "libParaViewCatalystIossAdapter.so";
#endif

};

#endif
