#include <Ionit_Initializer.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_Region.h>
#include <Ioss_Utils.h>

#include <assert.h>
#include <iostream>
#include <string>

int main(int argc, char *argv[]) {
#ifdef SEACAS_HAVE_MPI
  MPI_Init(&argc, &argv);
  ON_BLOCK_EXIT(MPI_Finalize);
#endif

  if (argc == 1) {
    std::cerr << "SYNTAX: " << argv[0]
              << " '--show-config' or 'filename'\n"
                 "        Will either show the Ioss build configuration\n"
                 "        Or will show a summary of the data in `filename`\n";
    std::exit(EXIT_SUCCESS);
  }

  Ioss::Init::Initializer io;
  std::string filename = argv[1];
  if (filename == "--show-config") {
    std::cerr << Ioss::IOFactory::show_configuration() << "\n";
  } else {
    auto dbtype = Ioss::Utils::get_type_from_file(filename);
    auto *dbi = Ioss::IOFactory::create(dbtype, filename, Ioss::READ_RESTART,
                                        Ioss::ParallelUtils::comm_world());

    if (dbi == NULL || !dbi->ok(true)) {
      std::exit(EXIT_FAILURE);
    }

    // NOTE: 'region' owns 'dbi' pointer at this time and will close
    // and call dbi destructor.
    Ioss::Region region(dbi, "example_region");
    region.output_summary(std::cerr);
  }
}
