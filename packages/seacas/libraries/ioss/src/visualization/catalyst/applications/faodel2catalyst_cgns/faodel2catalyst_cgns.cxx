#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "faodel2catalyst_cgns",
        "Faodel CGNS", "faodel", "cgns");

    ioapp.runApplication();
}

