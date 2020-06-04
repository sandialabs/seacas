#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "exo2catalyst", "Exodus II", "exodus");

    if (ioapp.printIOSSRegionReport()) {
        ioapp.printIOSSRegionReportForRank();
    }

    if (ioapp.outputCopyOfInputDatabase()) {
        ioapp.copyInputIOSSDatabaseOnRank();
    }

    ioapp.exitApplicationSuccess();
}
