// Copyright(C) 1999-2021 National Technology & Engineering Solutions
// of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
// NTESS, the U.S. Government retains certain rights in this software.
//
// See packages/seacas/LICENSE for details

#include <sys_info.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#undef IN
#undef OUT
#else
#include <sys/utsname.h>
#endif

std::string sys_info(const std::string &codename)
{
  // Add 'uname' output to the passed in character string.
  // Maximum size of string is 'size' (not including terminating nullptr)
  // This is used as information data in the concatenated results file
  // to help in tracking when/where/... the file was created
  std::string info = "CONJOIN: ";
#ifdef _WIN32
  std::string info                                      = "EPU: ";
  char        machine_name[MAX_COMPUTERNAME_LENGTH + 1] = {0};
  DWORD       buf_len                                   = MAX_COMPUTERNAME_LENGTH + 1;
  ::GetComputerName(machine_name, &buf_len);
  info += machine_name;
  info += ", OS: ";

  std::string   os = "Microsoft Windows";
  OSVERSIONINFO osvi;

  ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

  if (GetVersionEx(&osvi)) {
    DWORD             build = osvi.dwBuildNumber & 0xFFFF;
    std::stringstream str;
    fmt::print(str, " {}.{} {} (Build {})", osvi.dwMajorVersion, osvi.dwMinorVersion,
               osvi.szCSDVersion, build);
    os += str.str();
  }
  info += os;
  const char *sinfo = info.c_str();
  copy_string(info_record, sinfo, size + 1);
#else
  struct utsname sys_info
  {
  };
  uname(&sys_info);

  info += sys_info.nodename;
  info += ", OS: ";
  info += sys_info.sysname;
  info += " ";
  info += sys_info.release;
  info += ", ";
  info += sys_info.version;
  info += ", Machine: ";
  info += sys_info.machine;
#endif
  return info;
}
