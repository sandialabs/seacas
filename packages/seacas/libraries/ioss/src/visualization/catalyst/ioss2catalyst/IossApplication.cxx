#include "IossApplication.h"
#include "IossRegionReport.h"
#include "CatalystPluginPaths.h"
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
    this->initialize();
    this->initializeMPI();
    setenv("CATALYST_ADAPTER_INSTALL_DIR", CATALYST_PLUGIN_BUILD_DIR, false);
    std::string pluginLibPath = std::string(CATALYST_PLUGIN_BUILD_DIR) +\
        std::string("/") + CATALYST_PLUGIN_DYNAMIC_LIBRARY;
    setenv("CATALYST_PLUGIN", pluginLibPath.c_str(), false);
}

IossApplication::IossApplication(int argc, char **argv) {
    this->initialize();
    this->initializeMPI(argc, argv);
    this->processCommandLine(argc, argv);
    setenv("CATALYST_ADAPTER_INSTALL_DIR", CATALYST_PLUGIN_INSTALL_DIR, false);
}

void IossApplication::initialize() {
    this->myRank = 0;
    this->numRanks = 1;
    this->printIOSSReport = false;
    this->copyDatabase = false;
    this->writeCatalystMeshOneFile = false;
    this->writeCatalystMeshFilePerProc = false;
    this->fileName = "";
    this->inputIOSSRegion = nullptr;
    this->usePhactoriInputScript = false;
    this->usePhactoriInputJSON = false;
    this->useParaViewExportedScript = false;
    this->phactoriInputScriptFilePath = "";
    this->phactoriInputJSONFilePath = "";
    this->paraViewExportedScriptFilePath = "";
    this->catalystStartTimeStep = 0;
    this->useCatalystStartTimeStep = false;
    this->catalystStopTimeStep = 0;
    this->useCatalystStopTimeStep = false;
}

IossApplication::~IossApplication() {
    if (this->inputIOSSRegion) {
        delete this->inputIOSSRegion;
    }
}

void IossApplication::runApplication() {
    this->checkForOnlyOneCatalystOutputPath();
    Ioss::Init::Initializer io;
    this->openInputIOSSDatabase();

    if (this->printIOSSRegionReportON()) {
        this->printIOSSRegionReportForRank();
    }

    if (this->outputCopyOfInputDatabaseON()) {
        this->copyInputIOSSDatabaseOnRank();
    }

    this->callCatalystIOSSDatabaseOnRank();

    this->exitApplicationSuccess();
}

int IossApplication::getMyRank() {
    return this->myRank;
}

int IossApplication::getNumRanks() {
    return this->numRanks;
}

bool IossApplication::isRankZero() {
    return this->myRank == 0;
}

bool IossApplication::isSerial() {
    return this->numRanks == 1;
}

void IossApplication::initializeMPI() {
    MPI_Init(nullptr, nullptr);
    this->initMPIRankAndSize();
}

void IossApplication::initializeMPI(int argc, char **argv) {
    MPI_Init(&argc, &argv);
    this->initMPIRankAndSize();
}

void IossApplication::initMPIRankAndSize() {
    MPI_Comm_rank(MPI_COMM_WORLD, &this->myRank);
    MPI_Comm_size(MPI_COMM_WORLD, &this->numRanks);
}

void IossApplication::finalizeMPI() {
    MPI_Finalize();
}

void IossApplication::processCommandLine(int argc, char **argv) {
    int c;
    char *cvalue = NULL;
    while ((c = getopt (argc, argv, "a:b:chi:mnp:rs:")) != -1) {
        char *cvalue = nullptr;
        switch(c) {
            case 'a':
                cvalue = optarg;
                this->setCatalystStartTimeStep(atoi(cvalue));
                break;
            case 'b':
                cvalue = optarg;
                this->setCatalystStopTimeStep(atoi(cvalue));
                break;
            case 'c':
                this->copyDatabase = true;
                break;
            case 'h':
                this->printUsageMessage();
                this->exitApplicationSuccess();
                break;
            case 'i':
                cvalue = optarg;
                this->setPhactoriInputJSON(cvalue);
                break;
            case 'm':
                this->setOutputCatalystMeshOneFile(true);
                break;
            case 'n':
                this->setOutputCatalystMeshFilePerProc(true);
                break;
            case 'p':
                cvalue = optarg;
                this->setPhactoriInputScript(cvalue);
                break;
            case 'r':
                this->printIOSSReport = true;
                break;
            case 's':
                cvalue = optarg;
                this->setParaViewExportedScript(cvalue);
                break;
            case '?':
                this->printErrorMessage("Unknown command line option -"\
                    + std::string(1, c) + "\n");
                this->printUsageMessage();
                this->exitApplicationFailure();
                break;
            default:
                this->printUsageMessage();
                this->exitApplicationFailure();
                break;
        }
    }

    if (optind == argc) {
        this->printErrorMessage("No input filename given on command line");
        this->printUsageMessage();
        this->exitApplicationFailure();
    }
    else if (optind != argc-1) {
        this->printErrorMessage("Expected one argument for input filename. " +\
            std::string("Got multiple arguments."));
        this->printUsageMessage();
        this->exitApplicationFailure();
    }
    else {
        this->fileName = argv[optind];
    }
}

void IossApplication::getStartStopTimeSteps(int numTimeSteps,
    int & startTimeStep, int & stopTimeStep) {

    if (this->useCatalystStartTimeStepON()) {
        startTimeStep = this->getCatalystStartTimeStep();
        if (startTimeStep < 1 ||
            startTimeStep > numTimeSteps) {
            this->printErrorMessage("Start time-step out of range");
            this->exitApplicationFailure();
        }
    }
    else {
        startTimeStep = 1;
    }

    if (this->useCatalystStopTimeStepON()) {
        stopTimeStep = this->getCatalystStopTimeStep();
        if (stopTimeStep < 1 ||
            stopTimeStep > numTimeSteps) {
            this->printErrorMessage("Stop time-step out of range");
            this->exitApplicationFailure();
        }
    }
    else {
        stopTimeStep = numTimeSteps;
    }

    if (startTimeStep > stopTimeStep) {
        this->printErrorMessage("Start time-step > stop time-step.");
        this->exitApplicationFailure();
    }
}

void IossApplication::checkForOnlyOneCatalystOutputPath() {

    int numTimesCatalystCalled = 0;
    if(this->usePhactoriInputScriptON()) {
        numTimesCatalystCalled++;
    }
    if(this->usePhactoriInputJSONON()) {
        numTimesCatalystCalled++;
    }
    if(this->outputCatalystMeshOneFileON()) {
        numTimesCatalystCalled++;
    }
    if(this->outputCatalystMeshFilePerProcON()) {
        numTimesCatalystCalled++;
    }
    if(this->useParaViewExportedScriptON()) {
        numTimesCatalystCalled++;
    }

    if(numTimesCatalystCalled > 1) {
        this->printErrorMessage("Catalyst called with more than one option.");
        this->printUsageMessage();
        this->exitApplicationFailure();
    }

    if(numTimesCatalystCalled == 1 &&
        (this->printIOSSRegionReportON() ||
            this->outputCopyOfInputDatabaseON())) {
        this->printErrorMessage("Catalyst called with report output.");
        this->printUsageMessage();
        this->exitApplicationFailure();
    }
}

std::string& IossApplication::getFileName() {
    return this->fileName;
}

bool IossApplication::printIOSSRegionReportON() {
    return this->printIOSSReport;
}

void IossApplication::setPrintIOSSRegionReport(bool status) {
    this->printIOSSReport = status;
}

bool IossApplication::outputCopyOfInputDatabaseON() {
    return this->copyDatabase;
}

void IossApplication::setOutputCopyOfInputDatabase(bool status) {
    this->copyDatabase = status;
}

bool IossApplication::outputCatalystMeshOneFileON() {
    return this->writeCatalystMeshOneFile;
}

bool IossApplication::setOutputCatalystMeshOneFile(bool status) {
    this->writeCatalystMeshOneFile = status;
}

bool IossApplication::outputCatalystMeshFilePerProcON() {
    return this->writeCatalystMeshFilePerProc;
}

bool IossApplication::setOutputCatalystMeshFilePerProc(bool status) {
    this->writeCatalystMeshFilePerProc = status;
}

bool IossApplication::usePhactoriInputScriptON() {
    return this->usePhactoriInputScript;
}

std::string IossApplication::getPhactoriInputScript() {
    return this->phactoriInputScriptFilePath;
}

void IossApplication::setPhactoriInputScript(
    const std::string& scriptFilePath) {
    this->phactoriInputScriptFilePath = scriptFilePath;
    this->usePhactoriInputScript = true;
}

bool IossApplication::usePhactoriInputJSONON() {
    return this->usePhactoriInputJSON;
}

std::string IossApplication::getPhactoriInputJSON() {
    return this->phactoriInputJSONFilePath;
}

void IossApplication::setPhactoriInputJSON(
    const std::string& jsonFilePath) {
    this->phactoriInputJSONFilePath = jsonFilePath;
    this->usePhactoriInputJSON = true;
}

bool IossApplication::useParaViewExportedScriptON() {
    return this->useParaViewExportedScript;
}

std::string IossApplication::getParaViewExportedScript() {
    return this->paraViewExportedScriptFilePath;
}

void IossApplication::setParaViewExportedScript(
    const std::string& exportedScriptFilePath) {
    this->paraViewExportedScriptFilePath = exportedScriptFilePath;
    this->useParaViewExportedScript = true;
}

bool IossApplication::useCatalystStartTimeStepON() {
    return this->useCatalystStartTimeStep;
}

int IossApplication::getCatalystStartTimeStep() {
    return this->catalystStartTimeStep;
}

void IossApplication::setCatalystStartTimeStep(int timeStep) {
    this->catalystStartTimeStep = timeStep;
    this->useCatalystStartTimeStep = true;
}

bool IossApplication::useCatalystStopTimeStepON() {
    return this->useCatalystStopTimeStep;
}

int IossApplication::getCatalystStopTimeStep() {
    return this->catalystStopTimeStep;
}

void IossApplication::setCatalystStopTimeStep(int timeStep) {
    this->catalystStopTimeStep = timeStep;
    this->useCatalystStopTimeStep = true;
}

void IossApplication::printUsageMessage() {
    std::string um = "\nUSAGE\n\n" + applicationName;

    um += " [OPTIONS] [OUTPUT OPTIONS] <FILE>\n\n";

    um += "DESCRIPTION\n\n";
    um += "Read input file(s) and write to ParaView Catalyst using IOSS";

    um += "\n\nOPTIONS\n\n";

    um += "-a <n> call Catalyst starting at time-step n (starts at 1).\n\n";

    um += "-b <n> call Catalyst stopping at time-step n.\n\n";

    um += "-h print usage message and exit program\n\n";

    um += "OUTPUT OPTIONS are mutually exclusive\n\n";
    um += " [-cr | -m | -n | -i file | -p file | -s file]\n\n";

    um += "-c copy input file(s) to one file per processor with output \n";
    um += "   filename(s) prefix " + this->copyOutputDatabaseName + "\n\n";

    um += "-i <file> run Catalyst with Phactori input JSON given in <file>.";
    um += "\n\n";

    um += "-m output Catalyst mesh representation of input file(s)\n";
    um += "   each time-step to a single file for all processors with output\n";
    um += "   filename " + this->outputCatalystMeshFileName + "_time_<n>.vtm";
    um += "\n\n";

    um += "-n output Catalyst mesh representation of input file(s)\n";
    um += "   each time-step to a file for each processor with output\n";
    um += "   filename " + this->outputCatalystMeshFileName;
    um += "_proc_<p>_time_<n>.vtm\n\n";

    um += "-p <file> run Catalyst with Phactori input command syntax given\n";
    um += "   in <file>\n\n";

    um += "-r print IOSS region report for input file(s) to one file\n";
    um += "   per processor with output filename(s) prefix ";
    um += this->iossReportFileName + "\n\n";

    um += "-s <file> run Catalyst with a ParaView exported Python script\n";
    um += "   given in <file>";

    um += "\n\nEXAMPLES\n\n";

    um += "mpiexec -np 1 " + applicationName + " -h\n";
    um += "    Print usage message and exit program.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -c -r file\n";
    um += "    Output copy of input mesh and IOSS region report.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " file\n";
    um += "    Run Catalyst with default Phactori JSON script to produce\n";
    um += "    eight axis aligned external camera images of the input mesh.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -m file\n";
    um += "    Output mesh representation for Catalyst.\n\n";

    um += "mpiexec -np 4 " + applicationName + " -i file.json file\n";
    um += "    Run Catalyst with Phactori JSON input from file.json.\n\n";

    um += "mpiexec -np 4 " + applicationName + " -p file.txt file\n";
    um += "    Run Catalyst with Phactori command syntax input from file.txt.";
    um += "\n\n";

    um += "mpiexec -np 4 " + applicationName + " -s file.py file\n";
    um += "    Run Catalyst with ParaView exported Python script in file.py.";
 
    um += "\n\nFILE\n\n";
    um += "Exodus or CGNS input file name for a single file\n";
    um += " (file_name.ex2 or file_name.cgns)\n\n";

    um += "Exodus or CGNS file name prefix for multiple files\n";
    um += " (file_name.cgns.2.0, file_name.cgns.2.1 or \n";
    um += "  file_name.ex2.2.0, file_name.ex2.2.1)\n\n";
    this->printMessage(um);
}

void IossApplication::exitApplicationSuccess() {
    this->finalizeMPI();
    std::exit(EXIT_SUCCESS);
}

void IossApplication::exitApplicationFailure() {
    this->finalizeMPI();
    std::exit(EXIT_FAILURE);
}

void IossApplication::printMessage(const std::string& message) {
    if (this->isRankZero()) {
        std::cout << message;
    }
}

void IossApplication::printErrorMessage(const std::string& message) {
    if (this->isRankZero()) {
        std::cerr << "\nERROR: " << message << "\n";
    }
}

void IossApplication::printIOSSRegionReportForRank() {
    std::string fn = this->iossReportFileName + "." +\
        std::to_string(this->getNumRanks()) + "." +\
            std::to_string(this->getMyRank()) + ".txt";
    std::ofstream ofs (fn, std::ofstream::out);
    Ioss::Region * region = this->getInputIOSSRegion();

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
    if (this->isSerial()) {
      return this->getFileName();
    }

    std::stringstream nameStream;
    const int numZeroes = std::ceil(log10(double(this->getNumRanks())));

    nameStream << this->getFileName() << "." << this->getNumRanks()
        << "." << std::setfill('0') << std::setw(numZeroes) << this->getMyRank();

    return nameStream.str();
}

bool IossApplication::decomposedMeshExists() {
  int status = 0;

  if (this->isRankZero()) {
      std::string parallelFilename = this->getParallelFileName();
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
    return this->inputIOSSRegion;
}

void IossApplication::openInputIOSSDatabase() {
    Ioss::PropertyManager inputProperties;
    if (this->decomposedMeshExists()) {
        inputProperties.add(Ioss::Property("DECOMPOSITION_METHOD", "external"));
    }
    else {
        inputProperties.add(Ioss::Property("DECOMPOSITION_METHOD", "rib"));
    }

    Ioss::DatabaseIO * dbi = Ioss::IOFactory::create(
        getIOSSDatabaseTypeFromFile(), this->getFileName(), Ioss::READ_RESTART,
            (MPI_Comm)MPI_COMM_WORLD, inputProperties);
    if (dbi == nullptr || !dbi->ok(true)) {
        this->printErrorMessage("Unable to open input file(s) " +
            this->getFileName());
        this->exitApplicationFailure();
    }
    this->inputIOSSRegion = new Ioss::Region(dbi);
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
    std::string fn = this->copyOutputDatabaseName + "." + getFileSuffix();
    Ioss::PropertyManager outputProperties;
    outputProperties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
    Ioss::DatabaseIO * dbo = Ioss::IOFactory::create(
        getIOSSDatabaseTypeFromFile(), fn, Ioss::WRITE_RESULTS,
            (MPI_Comm) MPI_COMM_WORLD, outputProperties);
    if (dbo == nullptr || !dbo->ok(true)) {
        this->printErrorMessage("Unable to open output file(s) " +
            fn);
        this->exitApplicationFailure();
    }

    Ioss::Region * inputRegion = this->getInputIOSSRegion();
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

    if (this->usePhactoriInputScriptON()) {
        outputProperties.add(Ioss::Property("PHACTORI_INPUT_SYNTAX_SCRIPT",
            this->getPhactoriInputScript()));
    }
    else if (this->usePhactoriInputJSONON()) {
        outputProperties.add(Ioss::Property("PHACTORI_JSON_SCRIPT",
            this->getPhactoriInputJSON()));
    }
    else if (this->useParaViewExportedScriptON()) {
        outputProperties.add(Ioss::Property("CATALYST_SCRIPT",
            this->getParaViewExportedScript()));
    }
    else if (this->outputCatalystMeshOneFileON()) {
        outputProperties.add(Ioss::Property(
            "WRITE_CATALYST_MESH_ONE_FILE_WITH_PREFIX",
                this->outputCatalystMeshFileName));
    }
    else if (this->outputCatalystMeshFilePerProcON()) {
        outputProperties.add(Ioss::Property(
            "WRITE_CATALYST_MESH_FILE_PER_PROC_WITH_PREFIX",
                this->outputCatalystMeshFileName));
    }
    else {
        outputProperties.add(Ioss::Property("CATALYST_BLOCK_PARSE_JSON_STRING",
            this->getPhactoriDefaultJSON()));
    }

    outputProperties.add(Ioss::Property("CATALYST_BLOCK_PARSE_INPUT_DECK_NAME",
        applicationName));

    std::string catalystDatabaseType = "catalyst_exodus";
    if (getIOSSDatabaseTypeFromFile() == "cgns") {
        catalystDatabaseType = "catalyst_cgns";
    }

    Ioss::DatabaseIO * dbo = Ioss::IOFactory::create(catalystDatabaseType,
        "catalyst", Ioss::WRITE_RESULTS, (MPI_Comm)MPI_COMM_WORLD,
               outputProperties);
    if (dbo == nullptr || !dbo->ok(true)) {
        this->printErrorMessage("Unable to open catalyst database");
        this->exitApplicationFailure();
    }

    Ioss::Region * inputRegion = this->getInputIOSSRegion();
    Ioss::Region * outputRegion = new Ioss::Region(dbo, inputRegion->name());

    auto state_count = inputRegion->get_property("state_count").get_int();

    int startTimeStep;
    int stopTimeStep;
    this->getStartStopTimeSteps(state_count, startTimeStep, stopTimeStep);

    double min_time = inputRegion->get_state_time(startTimeStep);
    double max_time = inputRegion->get_state_time(stopTimeStep);

    Ioss::MeshCopyOptions copyOptions;
    copyOptions.data_storage_type = 1;
    copyOptions.minimum_time = min_time;
    copyOptions.maximum_time = max_time;
    Ioss::Utils::copy_database(*inputRegion, *outputRegion, copyOptions);

    delete outputRegion;
}

std::string IossApplication::getIOSSDatabaseTypeFromFile() {
    Ioss::FileInfo file(this->fileName);
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
    if(getIOSSDatabaseTypeFromFile() == "exodus") {
        return "ex2";
    }
    else {
        return "cgns";
    }
}
