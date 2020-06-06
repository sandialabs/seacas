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
    ~IossApplication();
    int getMyRank();
    int getNumRanks();
    bool isRankZero();
    bool isSerial();
    bool printIOSSRegionReport();
    bool outputCopyOfInputDatabase();
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
    void openInputIOSSDatabase();
    void processCommandLine(int argc, char **argv);
    void initializeMPI(int argc, char **argv);
    void finalizeMPI();
    void printUsageMessage();
    std::string getParallelFileName();
    int myRank;
    int numRanks;
    bool printIOSSReport;
    bool copyDatabase;
    std::string fileName;
    std::string applicationName;
    std::string fileTypeName;
    std::string fileTypeSuffix;
    std::string iossDatabaseType;
    std::string copyOutputDatabaseName = "iossDatabaseCopy";
    Ioss::Region * inputIOSSRegion;
};

#endif
