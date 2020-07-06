#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "faodel2catalyst_exodus",
        "Faodel Exodus", "faodel", "ex2");

    ioapp.runApplication();
}

