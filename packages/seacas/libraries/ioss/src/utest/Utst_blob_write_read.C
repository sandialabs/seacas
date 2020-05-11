/*------------------------------------------------------------------------
 * Copyright (c) 2020 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 */

#include <Ioss_Blob.h>
#include <Ioss_DatabaseIO.h>
#include <Ioss_IOFactory.h>
#include <Ioss_Property.h>
#include <Ioss_Region.h>
#include <Ioss_Utils.h>

#include <Ionit_Initializer.h>

#include <algorithm>
#include <string>
#include <vector>

//--------------------------------------------------------------------
/*----------------------------------------------------------------------
 * IOSS Blob Example
 *
  ex_blob blob[] = {{100, "Tempus", 10}, // ID, Name, Size
                    {200, "IOSS", 20},
                    {300, "Solver", 15}};

  for (int i = 0; i < num_blob; i++) {
    EXCHECK(ex_put_blob(exoid, blob[i]));
  }

    char *red_var_names[] = {"Momentum_X", "Momentum_Y", "Momentum_Z", "Kinetic_Energy"};
    EXCHECK(ex_put_reduction_variable_names(exoid, EX_BLOB, num_red_blob_vars, red_var_names));
  }

  {
    char *var_names[] = {"X", "XDOT", "XDDOT"};
    EXCHECK(ex_put_variable_names(exoid, EX_BLOB, num_blob_vars, var_names));
  }
*/
void write_blob();
void read_blob();

std::vector<double> generate_data(double time, size_t size, double offset)
{
  std::vector<double> data;
  data.reserve(size);
  for (size_t i = 0; i < size; i++) {
    data.push_back(10.0 * i + 100 * time + offset);
  }
  return data;
}

int main(int argc, char *argv[])
{

  Ioss::Init::Initializer io; // Initialize IOSS library.

  write_blob();
  read_blob();
}

void write_blob()
{
  std::cout << "***** Writing Blob Example File...\n";
  Ioss::PropertyManager properties;
  Ioss::DatabaseIO *    dbo = Ioss::IOFactory::create(
      "exodus", "ioss_blob_example.e", Ioss::WRITE_RESTART, (MPI_Comm)MPI_COMM_WORLD, properties);
  if (dbo == NULL || !dbo->ok(true)) {
    std::exit(EXIT_FAILURE);
  }

  // NOTE: 'region' owns 'dbo' pointer at this time
  Ioss::Region region(dbo, "example_region");
  region.begin_mode(Ioss::STATE_DEFINE_MODEL);

  const size_t b1_size = 100;
  const size_t b2_size = 200;
  const size_t b3_size = 57;

  // Define a blob -- give name and size
  Ioss::Blob *blob1 = new Ioss::Blob(dbo, "Tempus", b1_size);
  region.add(blob1);

  // These are "entity attributes" for blob1. Non-transient (constant) property
  // applied to the blob, not each entry in the blob.
  std::vector<double> offsets{1.0, 2.0, 0.0};
  std::vector<double> scales{10.5, 11.5, 17.5};
  blob1->property_add(Ioss::Property("Offset", offsets, Ioss::Property::ATTRIBUTE));
  blob1->property_add(Ioss::Property("Scale", scales, Ioss::Property::ATTRIBUTE));

  Ioss::Blob *blob2 = new Ioss::Blob(dbo, "Solver", b2_size);
  region.add(blob2);

  Ioss::Blob *blob3 = new Ioss::Blob(dbo, "ABlob", b3_size);
  region.add(blob3);

  region.end_mode(Ioss::STATE_DEFINE_MODEL);

  region.begin_mode(Ioss::STATE_DEFINE_TRANSIENT);

  // Aaadd some transient fields to the blobs.  There will be a value per entry in the blob.
  Ioss::Field x("X", Ioss::Field::BasicType::REAL, "scalar", Ioss::Field::RoleType::TRANSIENT);
  Ioss::Field dx("XDOT", Ioss::Field::BasicType::REAL, "scalar", Ioss::Field::RoleType::TRANSIENT);
  Ioss::Field ddx("XDDOT", Ioss::Field::BasicType::REAL, "scalar",
                  Ioss::Field::RoleType::TRANSIENT);

  blob1->field_add(x);
  blob1->field_add(dx);
  blob1->field_add(ddx);

  // Blobs can have different fields
  blob2->field_add(x);
  blob2->field_add(dx);

  blob3->field_add(x);

  // Reduction Fields -- Single value per blob per timestep
  Ioss::Field momentum("Momentum", Ioss::Field::BasicType::REAL, "vector_3d",
                       Ioss::Field::RoleType::REDUCTION);
  Ioss::Field ke("kinetic_energy", Ioss::Field::BasicType::REAL, "scalar",
                 Ioss::Field::RoleType::REDUCTION);

  blob1->field_add(momentum);
  blob1->field_add(ke);

  blob2->field_add(momentum);
  blob2->field_add(ke);

  blob3->field_add(momentum);
  blob3->field_add(ke);

  region.end_mode(Ioss::STATE_DEFINE_TRANSIENT);

  region.begin_mode(Ioss::STATE_TRANSIENT);
  const size_t num_ts = 10;
  for (size_t ts = 0; ts < num_ts; ts++) {
    double time = ts / 10.0;
    auto   step = region.add_state(time);
    region.begin_state(step);

    const auto &blobs = region.get_blobs();
    int         idx   = 0;
    for (const auto *blob : blobs) {
      // Dummy data for the fields.  All the same here...
      // Would be different in reality.
      const size_t size = blob->entity_count();

      // Get the fields that are defined on this blob...
      Ioss::NameList fields;
      blob->field_describe(Ioss::Field::RoleType::TRANSIENT, &fields);
      for (const auto &field : fields) {
        std::vector<double> data = generate_data(time, size, idx++);
        blob->put_field_data(field, data);
      }

      // Reduction fields...
      Ioss::NameList red_fields;
      blob->field_describe(Ioss::Field::RoleType::REDUCTION, &red_fields);
      for (const auto &field : red_fields) {
        std::vector<double> data = generate_data(time, 3, idx++);
        blob->put_field_data(field, data);
      }
    }
    region.end_state(step);
  }
  region.end_mode(Ioss::STATE_TRANSIENT);
  // File closed when `region` goes out of scope.
}

void read_blob()
{
  std::cout << "\n***** Reading Blob Example File...\n";
  Ioss::PropertyManager properties;
  Ioss::DatabaseIO *    dbi = Ioss::IOFactory::create(
      "exodus", "ioss_blob_example.e", Ioss::READ_RESTART, (MPI_Comm)MPI_COMM_WORLD, properties);
  if (dbi == NULL || !dbi->ok(true)) {
    std::exit(EXIT_FAILURE);
  }

  // NOTE: 'region' owns 'dbi' pointer at this time
  Ioss::Region region(dbi, "example_region");

  // Print a summary of the properties and fields on each blob...
  const auto &blobs = region.get_blobs();
  for (const auto *blob : blobs) {
    std::cout << "\nBlob " << blob->name() << " contains: " << blob->entity_count()
              << " item(s).\n";

    Ioss::Utils::info_property(blob, Ioss::Property::ATTRIBUTE, "\tAttributes (Reduction): ");
    Ioss::Utils::info_fields(blob, Ioss::Field::TRANSIENT, "\n\tTransient: ");
    Ioss::Utils::info_fields(blob, Ioss::Field::REDUCTION, "\n\tTransient (Reduction):  ", "\t");
  }

  size_t ts_count = 0;
  if (region.property_exists("state_count")) {
    ts_count = region.get_property("state_count").get_int();
  }
  std::cout << "\nFile contains " << ts_count << " timesteps.\n";

  std::vector<Ioss::NameList> all_fields;
  std::vector<Ioss::NameList> all_red_fields;

  for (const auto *blob : blobs) {
    // Get the names of the fields that are defined on this blob...
    Ioss::NameList fields;
    blob->field_describe(Ioss::Field::RoleType::TRANSIENT, &fields);
    all_fields.push_back(fields);

    // Reduction fields...
    Ioss::NameList red_fields;
    blob->field_describe(Ioss::Field::RoleType::REDUCTION, &red_fields);
    all_red_fields.push_back(red_fields);
  }

  for (size_t step = 1; step <= ts_count; step++) {
    region.begin_state(step);
    double time = region.get_state_time(step); // Region steps are 1-based
    std::cout << "\n*** Step " << step << " is at time " << time << "\n";

    int                 idx = 0;
    std::vector<double> data;
    for (const auto *blob : blobs) {
      const auto &fields = all_fields[idx];
      std::cout << "\tBlob " << blob->name() << ":\n";
      for (const auto &field : fields) {
        blob->get_field_data(field, data);
        const auto minmax = std::minmax_element(begin(data), end(data));
        std::cout << "\t\tField " << field << ", Minimum Value = " << *minmax.first
                  << ", Maximum Value = " << *minmax.second << "\n";
      }

      const auto &red_fields = all_red_fields[idx++];
      for (const auto &field : red_fields) {
        blob->get_field_data(field, data);
        std::cout << "\t\tReduction Field " << field << ", Value = ";

        size_t comp_count = blob->get_field(field).raw_storage()->component_count();
        for (size_t i = 0; i < comp_count; i++) {
          std::cout << data[i] << " ";
        }
        std::cout << "\n";
      }
    }
  }
  // File closed when `region` goes out of scope.
}
