TRIBITS_PACKAGE_DEFINE_DEPENDENCIES(
  SUBPACKAGES_DIRS_CLASSIFICATIONS_OPTREQS
  Exodus      libraries/exodus        SS  REQUIRED
  Exodus_for  libraries/exodus_for    SS  OPTIONAL
  ExoIIv2for32 libraries/exoIIv2for32 SS  OPTIONAL
  Nemesis     libraries/nemesis       SS  OPTIONAL
  Ioss        libraries/ioss          SS  REQUIRED
  Chaco       libraries/chaco         SS  OPTIONAL
  Aprepro_lib libraries/aprepro_lib   SS  OPTIONAL
  Supes       libraries/supes         SS  OPTIONAL
  Suplib      libraries/suplib        SS  OPTIONAL
  SuplibC     libraries/suplib_c      SS  OPTIONAL
  SuplibCpp   libraries/suplib_cpp    SS  OPTIONAL
  SVDI        libraries/svdi          SS  OPTIONAL
  PLT         libraries/plt           SS  OPTIONAL
  Algebra     applications/algebra    SS  OPTIONAL
  Aprepro     applications/aprepro    SS  OPTIONAL
  Blot        applications/blot       SS  OPTIONAL
  Conjoin     applications/conjoin    SS  OPTIONAL
  Ejoin       applications/ejoin      SS  OPTIONAL
  Epu         applications/epu        SS  OPTIONAL
  Exo2mat     applications/exo2mat    SS  OPTIONAL
  Exodiff     applications/exodiff    SS  OPTIONAL
  Exomatlab   applications/exomatlab  SS  OPTIONAL
  Exotxt      applications/exotxt     SS  OPTIONAL
  Exo_format  applications/exo_format SS  OPTIONAL
  Ex1ex2v2    applications/ex1ex2v2   SS  OPTIONAL
  Fastq       applications/fastq      SS  OPTIONAL
  Gjoin       applications/gjoin      SS  OPTIONAL
  Gen3D       applications/gen3d      SS  OPTIONAL
  Genshell    applications/genshell   SS  OPTIONAL
  Grepos      applications/grepos     SS  OPTIONAL
  Grope       applications/grope      SS  OPTIONAL
  Mapvarlib   libraries/mapvarlib     SS  OPTIONAL
  Mapvar      applications/mapvar     SS  OPTIONAL
  Mapvar-kd   applications/mapvar-kd  SS  OPTIONAL
  Mat2exo     applications/mat2exo    SS  OPTIONAL
  Nemslice    applications/nem_slice  SS  OPTIONAL
  Nemspread   applications/nem_spread SS  OPTIONAL
  Numbers     applications/numbers    SS  OPTIONAL
  Slice	      applications/slice      SS  OPTIONAL
  Txtexo      applications/txtexo     SS  OPTIONAL
  Ex2ex1v2    applications/ex2ex1v2   SS  OPTIONAL
)

SET(LIB_OPTIONAL_DEP_TPLS MPI)

