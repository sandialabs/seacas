#include "IossApplication.h"

int main(int argc, char **argv) {
    IossApplication ioapp(argc, argv, "faodel2catalyst",
        "Faodel", "faodel", "fao");

    ioapp.runApplication();
}

