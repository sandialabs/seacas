#include "IossApplication.h"
#include "IossRegionReport.h"
#include "CatalystPluginPaths.h"
#include <Ioss_DatabaseIO.h>
#include <Ioss_Region.h>
#include <Ioss_IOFactory.h>
#include <Ionit_Initializer.h>
#include <Ioss_Utils.h>
#include <Ioss_MeshCopyOptions.h>
#include <Ioss_FileInfo.h>
#include <mpi.h>
#include <cstdlib>
#include <iostream>
#include <unistd.h>
#include <fstream>
#include <iomanip>
#include <cstdlib>

IossApplication::IossApplication() {
    initialize();
    setenv("CATALYST_ADAPTER_INSTALL_DIR", CATALYST_PLUGIN_BUILD_DIR, false);
    std::string pluginLibPath = std::string(CATALYST_PLUGIN_BUILD_DIR) +\
        std::string("/") + CATALYST_PLUGIN_DYNAMIC_LIBRARY;
    setenv("CATALYST_PLUGIN", pluginLibPath.c_str(), false);
}

IossApplication::IossApplication(int argc, char **argv) {
    initialize();
    initializeMPI(argc, argv);
    processCommandLine(argc, argv);
    setenv("CATALYST_ADAPTER_INSTALL_DIR", CATALYST_PLUGIN_INSTALL_DIR, false);
}

void IossApplication::initialize() {
    myRank = 0;
    numRanks = 1;
    printIOSSReport = false;
    copyDatabase = false;
    writeCatalystMeshOneFile = false;
    writeCatalystMeshFilePerProc = false;
    fileName = "";
    inputIOSSRegion = nullptr;
    usePhactoriInputScript = false;
    usePhactoriInputJSON = false;
    useParaViewExportedScript = false;
    phactoriInputScriptFilePath = "";
    phactoriInputJSONFilePath = "";
    paraViewExportedScriptFilePath = "";
    catalystStartTimeStep = 0;
    useCatalystStartTimeStep = false;
    catalystStopTimeStep = 0;
    useCatalystStopTimeStep = false;
    forceCGNSOutput = false;
    forceExodusOutput = false;
    useIOSSInputDBType = false;
    iossInputDBType = "";
    hasCommandLineArguments = false;
    applicationExitCode = EXIT_SUCCESS;
}

IossApplication::~IossApplication() {
    if (inputIOSSRegion) {
        delete inputIOSSRegion;
    }
}

void IossApplication::runApplication() {
    checkForOnlyOneCatalystOutputPath();
    checkForOnlyOneCatalystOutputType();
    Ioss::Init::Initializer io;
    openInputIOSSDatabase();

    if (printIOSSRegionReportON()) {
        printIOSSRegionReportForRank();
    }

    if (outputCopyOfInputDatabaseON()) {
        copyInputIOSSDatabaseOnRank();
    }

    callCatalystIOSSDatabaseOnRank();

    exitApplicationSuccess();
}

int IossApplication::getApplicationExitCode() {
    return applicationExitCode;
}

int IossApplication::getMyRank() {
    return myRank;
}

int IossApplication::getNumRanks() {
    return numRanks;
}

bool IossApplication::isRankZero() {
    return myRank == 0;
}

bool IossApplication::isSerial() {
    return numRanks == 1;
}

void IossApplication::initializeMPI() {
    MPI_Init(nullptr, nullptr);
    initMPIRankAndSize();
}

void IossApplication::initializeMPI(int argc, char **argv) {
    MPI_Init(&argc, &argv);
    initMPIRankAndSize();
}

void IossApplication::initMPIRankAndSize() {
    MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
    MPI_Comm_size(MPI_COMM_WORLD, &numRanks);
}

void IossApplication::finalizeMPI() {
    MPI_Finalize();
}

void IossApplication::processCommandLine(int argc, char **argv) {
    hasCommandLineArguments = true;

    int c;
    char *cvalue = NULL;
    while ((c = getopt(argc, argv, "a:b:cd:e:hi:mnp:rs:")) != -1) {
        char *cvalue = nullptr;
        switch(c) {
            case 'a':
                cvalue = optarg;
                setCatalystStartTimeStep(atoi(cvalue));
                break;
            case 'b':
                cvalue = optarg;
                setCatalystStopTimeStep(atoi(cvalue));
                break;
            case 'c':
                copyDatabase = true;
                break;
            case 'd':
                cvalue = optarg;
                setForceExodusOutput(true);
                setIOSSInputDBType(cvalue);
                break;
            case 'e':
                cvalue = optarg;
                setForceCGNSOutput(true);
                setIOSSInputDBType(cvalue);
                break;
            case 'h':
                printUsageMessage();
                exitApplicationSuccess();
                break;
            case 'i':
                cvalue = optarg;
                setPhactoriInputJSON(cvalue);
                break;
            case 'm':
                setOutputCatalystMeshOneFile(true);
                break;
            case 'n':
                setOutputCatalystMeshFilePerProc(true);
                break;
            case 'p':
                cvalue = optarg;
                setPhactoriInputScript(cvalue);
                break;
            case 'r':
                printIOSSReport = true;
                break;
            case 's':
                cvalue = optarg;
                setParaViewExportedScript(cvalue);
                break;
            case '?':
                printErrorMessage("Unknown command line option -"\
                    + std::string(1, c) + "\n");
                printUsageMessage();
                exitApplicationFailure();
                break;
            default:
                printUsageMessage();
                exitApplicationFailure();
                break;
        }
    }

    if (optind == argc) {
        printErrorMessage("No input filename given on command line");
        printUsageMessage();
        exitApplicationFailure();
    }
    else if (optind != argc-1) {
        printErrorMessage("Expected one argument for input filename. " +\
            std::string("Got multiple arguments."));
        printUsageMessage();
        exitApplicationFailure();
    }
    else {
        fileName = argv[optind];
    }
}

void IossApplication::getStartStopTimeSteps(int numTimeSteps,
    int & startTimeStep, int & stopTimeStep) {

    if (useCatalystStartTimeStepON()) {
        startTimeStep = getCatalystStartTimeStep();
        if (startTimeStep < 1 ||
            startTimeStep > numTimeSteps) {
            printErrorMessage("Start time-step out of range");
            exitApplicationFailure();
        }
    }
    else {
        startTimeStep = 1;
    }

    if (useCatalystStopTimeStepON()) {
        stopTimeStep = getCatalystStopTimeStep();
        if (stopTimeStep < 1 ||
            stopTimeStep > numTimeSteps) {
            printErrorMessage("Stop time-step out of range");
            exitApplicationFailure();
        }
    }
    else {
        stopTimeStep = numTimeSteps;
    }

    if (startTimeStep > stopTimeStep) {
        printErrorMessage("Start time-step > stop time-step.");
        exitApplicationFailure();
    }
}

void IossApplication::checkForOnlyOneCatalystOutputPath() {

    int numTimesCatalystCalled = 0;
    if(usePhactoriInputScriptON()) {
        numTimesCatalystCalled++;
    }
    if(usePhactoriInputJSONON()) {
        numTimesCatalystCalled++;
    }
    if(outputCatalystMeshOneFileON()) {
        numTimesCatalystCalled++;
    }
    if(outputCatalystMeshFilePerProcON()) {
        numTimesCatalystCalled++;
    }
    if(useParaViewExportedScriptON()) {
        numTimesCatalystCalled++;
    }

    if(numTimesCatalystCalled > 1) {
        printErrorMessage("Catalyst called with more than one option.");
        printUsageMessage();
        exitApplicationFailure();
    }

    if(numTimesCatalystCalled == 1 &&
        (printIOSSRegionReportON() ||
            outputCopyOfInputDatabaseON())) {
        printErrorMessage("Catalyst called with report output.");
        printUsageMessage();
        exitApplicationFailure();
    }
}

void IossApplication::checkForOnlyOneCatalystOutputType() {
    if(forceCGNSOutputON() && forceExodusOutputON()) {
        printErrorMessage("Both CGNS and Exodus Catalyst output requested.");
        printUsageMessage();
        exitApplicationFailure();
    }
}

std::string& IossApplication::getFileName() {
    return fileName;
}

void IossApplication::setFileName(const std::string& name) {
    fileName = name;
}

bool IossApplication::printIOSSRegionReportON() {
    return printIOSSReport;
}

void IossApplication::setPrintIOSSRegionReport(bool status) {
    printIOSSReport = status;
}

bool IossApplication::outputCopyOfInputDatabaseON() {
    return copyDatabase;
}

void IossApplication::setOutputCopyOfInputDatabase(bool status) {
    copyDatabase = status;
}

bool IossApplication::outputCatalystMeshOneFileON() {
    return writeCatalystMeshOneFile;
}

bool IossApplication::setOutputCatalystMeshOneFile(bool status) {
    writeCatalystMeshOneFile = status;
}

bool IossApplication::outputCatalystMeshFilePerProcON() {
    return writeCatalystMeshFilePerProc;
}

bool IossApplication::setOutputCatalystMeshFilePerProc(bool status) {
    writeCatalystMeshFilePerProc = status;
}

bool IossApplication::usePhactoriInputScriptON() {
    return usePhactoriInputScript;
}

std::string IossApplication::getPhactoriInputScript() {
    return phactoriInputScriptFilePath;
}

void IossApplication::setPhactoriInputScript(
    const std::string& scriptFilePath) {
    phactoriInputScriptFilePath = scriptFilePath;
    usePhactoriInputScript = true;
}

bool IossApplication::usePhactoriInputJSONON() {
    return usePhactoriInputJSON;
}

std::string IossApplication::getPhactoriInputJSON() {
    return phactoriInputJSONFilePath;
}

void IossApplication::setPhactoriInputJSON(
    const std::string& jsonFilePath) {
    phactoriInputJSONFilePath = jsonFilePath;
    usePhactoriInputJSON = true;
}

bool IossApplication::useParaViewExportedScriptON() {
    return useParaViewExportedScript;
}

std::string IossApplication::getParaViewExportedScript() {
    return paraViewExportedScriptFilePath;
}

void IossApplication::setParaViewExportedScript(
    const std::string& exportedScriptFilePath) {
    paraViewExportedScriptFilePath = exportedScriptFilePath;
    useParaViewExportedScript = true;
}

bool IossApplication::useCatalystStartTimeStepON() {
    return useCatalystStartTimeStep;
}

int IossApplication::getCatalystStartTimeStep() {
    return catalystStartTimeStep;
}

void IossApplication::setCatalystStartTimeStep(int timeStep) {
    catalystStartTimeStep = timeStep;
    useCatalystStartTimeStep = true;
}

bool IossApplication::useCatalystStopTimeStepON() {
    return useCatalystStopTimeStep;
}

int IossApplication::getCatalystStopTimeStep() {
    return catalystStopTimeStep;
}

void IossApplication::setCatalystStopTimeStep(int timeStep) {
    catalystStopTimeStep = timeStep;
    useCatalystStopTimeStep = true;
}

bool IossApplication::forceCGNSOutputON() {
    return forceCGNSOutput;
}

bool IossApplication::setForceCGNSOutput(bool status) {
    forceCGNSOutput = status;
}

bool IossApplication::forceExodusOutputON() {
    return forceExodusOutput;
}

bool IossApplication::setForceExodusOutput(bool status) {
    forceExodusOutput = status;
}

bool IossApplication::useIOSSInputDBTypeON() {
    return useIOSSInputDBType;
}

std::string IossApplication::getIOSSInputDBType() {
    return iossInputDBType;
}

void IossApplication::setIOSSInputDBType(const std::string& dbType) {
    iossInputDBType = dbType;
    useIOSSInputDBType = true;
}

void IossApplication::printUsageMessage() {
    std::string um = "\nUSAGE\n\n" + applicationName;

    um += " [OPTIONS] [OUTPUT OPTIONS] <FILE>\n\n";

    um += "DESCRIPTION\n\n";
    um += "Read input file(s) and write to ParaView Catalyst using IOSS";

    um += "\n\nOPTIONS\n\n";

    um += "-a <n> call Catalyst starting at time-step n (starts at 1).\n\n";

    um += "-b <n> call Catalyst stopping at time-step n.\n\n";

    um += "[-d | -e] <db_type> Force IOSS input database type to db_type\n";
    um += "   -d for catalyst_exodus output or -e for catalyst_cgns output\n\n";

    um += "-h print usage message and exit program\n\n";

    um += "OUTPUT OPTIONS\n\n";
    um += " [-cr | -m | -n | -i <file> | -p <file> | -s <file>]\n\n";

    um += "-c copy input file(s) to one file per processor with output \n";
    um += "   filename(s) prefix " + copyOutputDatabaseName + "\n\n";

    um += "-i <file> run Catalyst with Phactori input JSON given in <file>.";
    um += "\n\n";

    um += "-m output Catalyst mesh representation of input file(s)\n";
    um += "   each time-step to a single file for all processors with output\n";
    um += "   filename " + outputCatalystMeshFileName + "_time_<n>.vtm";
    um += "\n\n";

    um += "-n output Catalyst mesh representation of input file(s)\n";
    um += "   each time-step to a file for each processor with output\n";
    um += "   filename " + outputCatalystMeshFileName;
    um += "_proc_<p>_time_<n>.vtm\n\n";

    um += "-p <file> run Catalyst with Phactori input command syntax given\n";
    um += "   in <file>\n\n";

    um += "-r print IOSS region report for input file(s) to one file\n";
    um += "   per processor with output filename(s) prefix ";
    um += iossReportFileName + "\n\n";

    um += "-s <file> run Catalyst with a ParaView exported Python script\n";
    um += "   given in <file>";

    um += "\n\nEXAMPLES\n\n";

    um += "mpiexec -np 1 " + applicationName + " -h\n";
    um += "    Print usage message and exit program.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -c -r <FILE>\n";
    um += "    Output copy of input mesh and IOSS region report.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " <FILE>\n";
    um += "    Run Catalyst with default Phactori JSON script to produce\n";
    um += "    eight axis aligned external camera images of the input mesh.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -m <FILE>\n";
    um += "    Output mesh representation for Catalyst.\n\n";

    um += "mpiexec -np 4 " + applicationName + " -i file.json <FILE>\n";
    um += "    Run Catalyst with Phactori JSON input from file.json.\n\n";

    um += "mpiexec -np 4 " + applicationName + " -p file.txt <FILE>\n";
    um += "    Run Catalyst with Phactori command syntax input from file.txt.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -s file.py <FILE>\n";
    um += "    Run Catalyst with ParaView exported Python script in file.py.";
 
    um += "\n\nFILE\n\n";
    um += "Exodus or CGNS input file name for a single file\n\n";
    um += "   (file_name.ex2 or file_name.cgns)\n\n";

    um += "Exodus or CGNS file name prefix for multiple files\n\n";
    um += "   (file_name.cgns for file_name.cgns.2.0, file_name.cgns.2.1)\n";
    um += "   (file_name.ex2 for file_name.ex2.2.0, file_name.ex2.2.1)\n\n";
    printMessage(um);
}

void IossApplication::exitApplicationSuccess() {
    applicationExitCode = EXIT_SUCCESS;
    if(hasCommandLineArguments) {
        exitApplication();
    }
}

void IossApplication::exitApplicationFailure() {
    applicationExitCode = EXIT_FAILURE;
    if(hasCommandLineArguments) {
        exitApplication();
    }
}

void IossApplication::exitApplication() {
    finalizeMPI();
    std::exit(getApplicationExitCode());
}

void IossApplication::printMessage(const std::string& message) {
    if (isRankZero()) {
        std::cout << message;
    }
}

void IossApplication::printErrorMessage(const std::string& message) {
    if (isRankZero()) {
        std::cerr << "\nERROR: " << message << "\n";
    }
}

void IossApplication::printIOSSRegionReportForRank() {
    std::string fn = iossReportFileName + "." +\
        std::to_string(getNumRanks()) + "." +\
            std::to_string(getMyRank()) + ".txt";
    std::ofstream ofs (fn, std::ofstream::out);
    Ioss::Region * region = getInputIOSSRegion();

    ofs << ioss_region_report::region_report(*region) << "\n";
    auto state_count = region->get_property("state_count").get_int();
    for(auto state=1; state <= state_count; ++state) {
        region->begin_state(state);
        ofs << ioss_region_report::region_report(*region) << "\n";
        region->end_state(state);
    }

    ofs.close();
}

std::string IossApplication::getParallelFileName() {
    if (isSerial()) {
      return getFileName();
    }

    std::stringstream nameStream;
    const int numZeroes = std::ceil(log10(double(getNumRanks())));

    nameStream << getFileName() << "." << getNumRanks()
        << "." << std::setfill('0') << std::setw(numZeroes) << getMyRank();

    return nameStream.str();
}

bool IossApplication::decomposedMeshExists() {
  int status = 0;

  if (isRankZero()) {
      std::string parallelFilename = getParallelFileName();
      std::ifstream fstream(parallelFilename);
      if (fstream.good()) {
          status = 1;
      }
      if (fstream.is_open()) {
          fstream.close();
      }
  }
  MPI_Bcast(&status, 1, MPI_INT, 0, MPI_COMM_WORLD);
  return status == 1;
}

Ioss::Region * IossApplication::getInputIOSSRegion() {
    return inputIOSSRegion;
}

void IossApplication::openInputIOSSDatabase() {
    Ioss::PropertyManager inputProperties;
    if (decomposedMeshExists()) {
        inputProperties.add(Ioss::Property("DECOMPOSITION_METHOD", "external"));
    }
    else {
        inputProperties.add(Ioss::Property("DECOMPOSITION_METHOD", "rib"));
    }

    Ioss::DatabaseIO * dbi = Ioss::IOFactory::create(
        getIOSSDatabaseType(), getFileName(), Ioss::READ_RESTART,
            (MPI_Comm)MPI_COMM_WORLD, inputProperties);
    if (dbi == nullptr || !dbi->ok(true)) {
        printErrorMessage("Unable to open input file(s) " +
            getFileName());
        exitApplicationFailure();
    }
    inputIOSSRegion = new Ioss::Region(dbi);
}

std::string IossApplication::getPhactoriDefaultJSON() {
    char const *phactoriDefaultJSON = R"pd({
        "camera blocks":{},
        "representation blocks": {},
        "operation blocks": {},
        "imageset blocks": {},
        "scatter plot blocks": {},
        "plot over time blocks": {},
        "marker blocks": {}
    })pd";
    return phactoriDefaultJSON;
}

void IossApplication::copyInputIOSSDatabaseOnRank() {
    std::string fn = copyOutputDatabaseName + "." + getFileSuffix();
    Ioss::PropertyManager outputProperties;
    outputProperties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
    Ioss::DatabaseIO * dbo = Ioss::IOFactory::create(
        getIOSSDatabaseType(), fn, Ioss::WRITE_RESULTS,
            (MPI_Comm) MPI_COMM_WORLD, outputProperties);
    if (dbo == nullptr || !dbo->ok(true)) {
        printErrorMessage("Unable to open output file(s) " +
            fn);
        exitApplicationFailure();
    }

    Ioss::Region * inputRegion = getInputIOSSRegion();
    Ioss::Region * outputRegion = new Ioss::Region(dbo, inputRegion->name());

    auto state_count = inputRegion->get_property("state_count").get_int();
    double min_time = inputRegion->get_state_time(1);
    double max_time = inputRegion->get_state_time(state_count);
    Ioss::MeshCopyOptions copyOptions;
    copyOptions.data_storage_type = 1;
    copyOptions.minimum_time = min_time;
    copyOptions.maximum_time = max_time;
    Ioss::Utils::copy_database(*inputRegion, *outputRegion, copyOptions);

    delete outputRegion;
}

void IossApplication::callCatalystIOSSDatabaseOnRank() {
    Ioss::PropertyManager outputProperties;

    if (usePhactoriInputScriptON()) {
        outputProperties.add(Ioss::Property("PHACTORI_INPUT_SYNTAX_SCRIPT",
            getPhactoriInputScript()));
    }
    else if (usePhactoriInputJSONON()) {
        outputProperties.add(Ioss::Property("PHACTORI_JSON_SCRIPT",
            getPhactoriInputJSON()));
    }
    else if (useParaViewExportedScriptON()) {
        outputProperties.add(Ioss::Property("CATALYST_SCRIPT",
            getParaViewExportedScript()));
    }
    else if (outputCatalystMeshOneFileON()) {
        outputProperties.add(Ioss::Property(
            "WRITE_CATALYST_MESH_ONE_FILE_WITH_PREFIX",
                outputCatalystMeshFileName));
    }
    else if (outputCatalystMeshFilePerProcON()) {
        outputProperties.add(Ioss::Property(
            "WRITE_CATALYST_MESH_FILE_PER_PROC_WITH_PREFIX",
                outputCatalystMeshFileName));
    }
    else {
        outputProperties.add(Ioss::Property("CATALYST_BLOCK_PARSE_JSON_STRING",
            getPhactoriDefaultJSON()));
    }

    outputProperties.add(Ioss::Property("CATALYST_BLOCK_PARSE_INPUT_DECK_NAME",
        applicationName));

    Ioss::DatabaseIO * dbo = Ioss::IOFactory::create(getCatalystDatabaseType(),
        "catalyst", Ioss::WRITE_RESULTS, (MPI_Comm)MPI_COMM_WORLD,
               outputProperties);
    if (dbo == nullptr || !dbo->ok(true)) {
        printErrorMessage("Unable to open catalyst database");
        exitApplicationFailure();
    }

    Ioss::Region * inputRegion = getInputIOSSRegion();
    Ioss::Region * outputRegion = new Ioss::Region(dbo, inputRegion->name());

    auto state_count = inputRegion->get_property("state_count").get_int();

    int startTimeStep;
    int stopTimeStep;
    getStartStopTimeSteps(state_count, startTimeStep, stopTimeStep);

    double min_time = inputRegion->get_state_time(startTimeStep);
    double max_time = inputRegion->get_state_time(stopTimeStep);

    Ioss::MeshCopyOptions copyOptions;
    copyOptions.data_storage_type = 1;
    copyOptions.minimum_time = min_time;
    copyOptions.maximum_time = max_time;
    Ioss::Utils::copy_database(*inputRegion, *outputRegion, copyOptions);

    delete outputRegion;
}

std::string IossApplication::getIOSSDatabaseType() {
    std::string retVal = getIOSSDatabaseTypeFromFile();
    if (useIOSSInputDBTypeON()) {
        retVal = getIOSSInputDBType();
    }
    return retVal;
}

std::string IossApplication::getIOSSDatabaseTypeFromFile() {
    Ioss::FileInfo file(fileName);
    auto extension = file.extension();
    if (extension == "e" || extension == "g" ||
        extension == "gen" || extension == "exo") {
      return "exodus";
    }
    else if (extension == "cgns") {
      return "cgns";
    }
    else {
      return "exodus";
    }
}

std::string IossApplication::getFileSuffix() {
    std::string dbType = getIOSSDatabaseType();
    if (dbType == "exodus") {
        return "ex2";
    }
    else if (dbType == "cgns") {
        return "cgns";
    }
    else {
        return dbType;
    }
}

std::string IossApplication::getCatalystDatabaseType() {
    std::string retVal = "catalyst_exodus";
    if (!forceExodusOutputON()) {
        if (getIOSSDatabaseType() == "cgns") {
            retVal = "catalyst_cgns";
        }
        if (forceCGNSOutputON()) {
            retVal = "catalyst_cgns";
        }
    }
    return retVal;
}
