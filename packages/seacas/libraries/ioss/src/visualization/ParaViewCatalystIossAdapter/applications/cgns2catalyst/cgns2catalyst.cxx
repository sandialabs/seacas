#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "cgns2catalyst",
        "CGNS", "cgns", "cgns");

    ioapp.runApplication();
}
