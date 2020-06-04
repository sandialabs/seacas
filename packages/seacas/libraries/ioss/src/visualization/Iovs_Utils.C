
#include <Iovs_Utils.h>
#include <cstring>
#include <Ioss_Utils.h>

#ifdef IOSS_DLOPEN_ENABLED
#include <dlfcn.h>
#endif

#include <libgen.h>
#include <sys/stat.h>

namespace {

#if defined(__APPLE__)
  const char *CATALYST_PLUGIN_DYNAMIC_LIBRARY = "libParaViewCatalystIossAdapter.dylib";
#else
  const char *CATALYST_PLUGIN_DYNAMIC_LIBRARY = "libParaViewCatalystIossAdapter.so";
#endif

  const char *CATALYST_PLUGIN_PYTHON_MODULE = "PhactoriDriver.py";
  const char *CATALYST_PLUGIN_PATH          = "viz/catalyst/install";

} // end anonymous namespace

namespace Iovs {

  bool Utils::file_exists(const std::string &filepath)
  {
    struct stat buffer
    {
    };
    return (stat(filepath.c_str(), &buffer) == 0);
  }

  ParaViewCatalystIossAdapterBase *
  Utils::load_exodus_adapter_library(std::string &paraview_script_filename)
  {
    void* dlHandle = load_plugin_library(paraview_script_filename);

    if(!dlHandle) {
      return nullptr;
    }

    typedef ParaViewCatalystIossAdapterBase *(*PvCatSrrAdapterMakerFuncType)();

#ifdef __GNUC__
    __extension__
#endif
    PvCatSrrAdapterMakerFuncType mkr = reinterpret_cast<PvCatSrrAdapterMakerFuncType>(
        dlsym(dlHandle, "ParaViewCatalystIossAdapterCreateInstance"));
    if (mkr == nullptr) {
      throw std::runtime_error("dlsym call failed to load function "
                               "'ParaViewCatalystIossAdapterCreateInstance'");
    }

    return (*mkr)();
  }

  ParaViewCatalystCGNSAdapterBase *
  Utils::load_cgns_adapter_library(std::string &paraview_script_filename)
  {
    void* dlHandle = load_plugin_library(paraview_script_filename);

    if(!dlHandle) {
      return nullptr;
    }

    typedef ParaViewCatalystCGNSAdapterBase *(*PvCatSrrAdapterMakerFuncType)();

#ifdef __GNUC__
    __extension__
#endif
    PvCatSrrAdapterMakerFuncType mkr = reinterpret_cast<PvCatSrrAdapterMakerFuncType>(
        dlsym(dlHandle, "ParaViewCatalystCGNSAdapterCreateInstance"));
    if (mkr == nullptr) {
      throw std::runtime_error("dlsym call failed to load function "
                               "'ParaViewCatalystCGNSAdapterCreateInstance'");
    }

    return (*mkr)();
  }

  void *
  Utils::load_plugin_library(std::string &paraview_script_filename)
  {
    std::string plugin_library_path;
    std::string plugin_python_module_path;

    build_catalyst_plugin_paths(plugin_library_path, plugin_python_module_path,
                                CATALYST_PLUGIN_DYNAMIC_LIBRARY);

    if (getenv("CATALYST_PLUGIN") != nullptr) {
      plugin_library_path = getenv("CATALYST_PLUGIN");
    }

    if (paraview_script_filename.empty()) {
      paraview_script_filename = plugin_python_module_path;
    }

    if (!file_exists(paraview_script_filename)) {
      std::ostringstream errmsg;
      errmsg << "Catalyst Python module path does not exist.\n"
             << "Python module path: " << paraview_script_filename << "\n";
      IOSS_ERROR(errmsg);
    }

#ifdef IOSS_DLOPEN_ENABLED
    void* dlHandle = dlopen(plugin_library_path.c_str(), RTLD_NOW | RTLD_GLOBAL);
    if (dlHandle == nullptr) {
      throw std::runtime_error(dlerror());
    }

    return dlHandle;
#else
    return NULL;
#endif
  }

  void Utils::build_catalyst_plugin_paths(std::string &      plugin_library_path,
                                          std::string &      plugin_python_path,
                                          const std::string &plugin_library_name)
  {

    if (getenv("CATALYST_ADAPTER_INSTALL_DIR") != nullptr) {
      std::string catalyst_ins_dir = getenv("CATALYST_ADAPTER_INSTALL_DIR");

      if (!file_exists(catalyst_ins_dir)) {
        std::ostringstream errmsg;
        errmsg << "CATALYST_ADAPTER_INSTALL_DIR directory does not exist.\n"
               << "Directory path: " << catalyst_ins_dir << "\n"
               << "Unable to find ParaView catalyst dynamic library.\n";
        IOSS_ERROR(errmsg);
        return;
      }

      plugin_library_path = catalyst_ins_dir + "/lib/" + plugin_library_name;

      plugin_python_path = catalyst_ins_dir + "/python/" + CATALYST_PLUGIN_PYTHON_MODULE;
      return;
    }

    std::string sierra_ins_dir;
    if (getenv("SIERRA_INSTALL_DIR") != nullptr) {
      sierra_ins_dir = getenv("SIERRA_INSTALL_DIR");
    }
    else {
      std::ostringstream errmsg;
      errmsg << "Environment variable SIERRA_INSTALL_DIR not set.\n"
             << " Unable to find ParaView catalyst dynamic library.\n";
      IOSS_ERROR(errmsg);
      return;
    }

    std::string sierra_system;
    if (getenv("SIERRA_SYSTEM") != nullptr) {
      sierra_system = getenv("SIERRA_SYSTEM");
    }
    else {
      std::ostringstream errmsg;
      errmsg << "Environment variable SIERRA_SYSTEM not set.\n"
             << " Unable to find ParaView catalyst dynamic library.\n";
      IOSS_ERROR(errmsg);
      return;
    }

    std::string sierra_version;
    if (getenv("SIERRA_VERSION") != nullptr) {
      sierra_version = getenv("SIERRA_VERSION");
    }
    else {
      std::ostringstream errmsg;
      errmsg << "Environment variable SIERRA_VERSION not set.\n"
             << " Unable to find ParaView catalyst dynamic library.\n";
      IOSS_ERROR(errmsg);
      return;
    }


    char *      cbuf            = realpath(sierra_ins_dir.c_str(), nullptr);
    std::string sierra_ins_path = cbuf;
    free(cbuf);

    if (!file_exists(sierra_ins_path)) {
      std::ostringstream errmsg;
      errmsg << "SIERRA_INSTALL_DIR directory does not exist.\n"
             << "Directory path: " << sierra_ins_path << "\n"
             << " Unable to find ParaView catalyst dynamic library.\n";
      IOSS_ERROR(errmsg);
      return;
    }

    char *cbase = strdup(sierra_ins_path.c_str());
    char *cdir  = strdup(sierra_ins_path.c_str());
    char *bname = basename(cbase);
    char *dname = dirname(cdir);

    while (strcmp(dname, "/") != 0 && strcmp(dname, ".") != 0 && strcmp(bname, "sierra") != 0) {
      bname = basename(dname);
      dname = dirname(dname);
    }

    if (strcmp(bname, "sierra") == 0) {
      sierra_ins_path = dname;
    }

    free(cbase);
    free(cdir);

    plugin_library_path = sierra_ins_path + "/" + CATALYST_PLUGIN_PATH + "/" + sierra_system + "/" +
                          sierra_version + "/" + plugin_library_name;

    plugin_python_path = sierra_ins_path + "/" + CATALYST_PLUGIN_PATH + "/" + sierra_system + "/" +
                         sierra_version + "/" + CATALYST_PLUGIN_PYTHON_MODULE;
  }

} // namespace Iovs
