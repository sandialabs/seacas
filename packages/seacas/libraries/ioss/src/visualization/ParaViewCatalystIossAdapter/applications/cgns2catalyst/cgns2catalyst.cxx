#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "cgns2catalyst",
        "CGNS", "cgns", "cgns");

    if (ioapp.printIOSSRegionReport()) {
        ioapp.printIOSSRegionReportForRank();
    }

    if (ioapp.outputCopyOfInputDatabase()) {
        ioapp.copyInputIOSSDatabaseOnRank();
    }

    ioapp.exitApplicationSuccess();
}
