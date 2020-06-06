#include "IossApplication.h"
#include "IossDatabaseCopier.h"
#include "IossRegionReport.h"
#include <Ioss_IOFactory.h>
#include <Ionit_Initializer.h>
#include <mpi.h>
#include <cstdlib>
#include <iostream>
#include <unistd.h>
#include <fstream>
#include <iomanip>

IossApplication::IossApplication(int argc, char **argv,
    const std::string& appName, const std::string& fileTypeName,
        const std::string& iossDatabaseType,
            const std::string& fileTypeSuffix) {
    this->myRank = 0;
    this->numRanks = 1;
    this->printIOSSReport = false;
    this->copyDatabase = false;
    this->fileName = "";
    this->applicationName = appName;
    this->fileTypeName = fileTypeName;
    this->fileTypeSuffix = fileTypeSuffix;
    this->iossDatabaseType = iossDatabaseType;
    this->inputIOSSRegion = nullptr;
    this->initializeMPI(argc, argv);
    this->processCommandLine(argc, argv);
    Ioss::Init::Initializer io;
    this->openInputIOSSDatabase();
}

IossApplication::~IossApplication() {
    if (this->inputIOSSRegion) {
        delete this->inputIOSSRegion;
    }
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

void IossApplication::initializeMPI(int argc, char **argv) {
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &this->myRank);
    MPI_Comm_size(MPI_COMM_WORLD, &this->numRanks);
}

void IossApplication::finalizeMPI() {
    MPI_Finalize();
}

void IossApplication::processCommandLine(int argc, char **argv) {
    int c;
    while ((c = getopt (argc, argv, "rch")) != -1) {
        switch(c) {
            case 'r':
                this->printIOSSReport = true;
                break;
            case 'c':
                this->copyDatabase = true;
                break;
            case 'h':
                this->printUsageMessage();
                this->exitApplicationSuccess();
            case '?':
                this->printErrorMessage("Unknown command line option -"\
                    + std::string(1, c) + "\n");
                this->printUsageMessage();
                this->exitApplicationFailure();
            default:
                this->printUsageMessage();
                this->exitApplicationFailure();
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

std::string& IossApplication::getApplicationName() {
    return this->applicationName;
}

std::string& IossApplication::getFileName() {
    return this->fileName;
}

bool IossApplication::printIOSSRegionReport() {
    return this->printIOSSReport;
}

bool IossApplication::outputCopyOfInputDatabase() {
    return this->copyDatabase;
}

void IossApplication::printUsageMessage() {
    std::string fn = this->copyOutputDatabaseName + "." + this->fileTypeSuffix;
    std::string um = "\nUsage:  " + this->applicationName +\
        " [OPTIONS] [FILE]\n\n";
    um += "DESCRIPTION\n\n";
    um += "Read input " + this->fileTypeName +\
        " file(s) and send to ParaView Catalyst";
    um += "\n\nOPTIONS\n\n";
    um += "-c copy input file(s) to one file per processor ";
    um += "with output filename(s) " + fn + "\n";
    um += "-r print IOSS region report for input file(s) to one ";
    um += "file per processor\n"; 
    um += "-h print this usage message and exit program";
    um += "\n\nFILE\n\n";
    um += this->fileTypeName + " input file name for a single file, ";
    um += "or the file name prefix for multiple files\n";
    um += "\n";
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
        std::cerr << message;
    }
}

void IossApplication::printIOSSRegionReportForRank() {
    std::string fn = "IossRegionReport." +\
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

    Ioss::DatabaseIO * dbi = Ioss::IOFactory::create(this->iossDatabaseType,
        this->getFileName(), Ioss::READ_RESTART,
            (MPI_Comm)MPI_COMM_WORLD, inputProperties);
    if (dbi == nullptr || !dbi->ok(true)) {
        this->printErrorMessage("Unable to open input " +\
            this->fileTypeName + " file(s) " + this->getFileName());
        this->exitApplicationFailure();
    }
    this->inputIOSSRegion = new Ioss::Region(dbi);
}

void IossApplication::copyInputIOSSDatabaseOnRank() {
    std::string fn = this->copyOutputDatabaseName + "." + this->fileTypeSuffix;
    Ioss::PropertyManager outputProperties;
    outputProperties.add(Ioss::Property("COMPOSE_RESULTS", "NO"));
    Ioss::DatabaseIO * dbo = Ioss::IOFactory::create(this->iossDatabaseType,
        fn, Ioss::WRITE_RESULTS, (MPI_Comm)MPI_COMM_WORLD,
               outputProperties);
    if (dbo == nullptr || !dbo->ok(true)) {
        this->printErrorMessage("Unable to open output " +\
            this->fileTypeName + " file(s) " + fn);
        this->exitApplicationFailure();
    }

    Ioss::Region * inputRegion = this->getInputIOSSRegion();
    Ioss::Region * outputRegion = new Ioss::Region(dbo, inputRegion->name());
    IossDatabaseCopier idc(inputRegion, outputRegion);
    idc.copyInputIOSSDatabaseToOutputIOSSDatabase();
    delete outputRegion;
}
