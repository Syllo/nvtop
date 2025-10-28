/*
 * Windows-specific process information retrieval
 * Copyright (C) 2025
 *
 * This file is part of Nvtop.
 */

// Windows headers must be included in this order
#include <psapi.h>
#include <sddl.h>
#include <tlhelp32.h>
#include <windows.h>

#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/get_process_info.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Get actual username from process ID using Windows APIs
void get_username_from_pid(pid_t pid, char **buffer) {
  *buffer = NULL;

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
  if (!hProcess) {
    return;
  }

  HANDLE hToken;
  if (!OpenProcessToken(hProcess, TOKEN_QUERY, &hToken)) {
    CloseHandle(hProcess);
    return;
  }

  DWORD dwSize = 0;
  GetTokenInformation(hToken, TokenUser, NULL, 0, &dwSize);

  if (dwSize > 0) {
    TOKEN_USER *pTokenUser = (TOKEN_USER *)malloc(dwSize);
    if (pTokenUser && GetTokenInformation(hToken, TokenUser, pTokenUser, dwSize, &dwSize)) {
      char name[256];
      char domain[256];
      DWORD nameSize = sizeof(name);
      DWORD domainSize = sizeof(domain);
      SID_NAME_USE sidType;

      if (LookupAccountSidA(NULL, pTokenUser->User.Sid, name, &nameSize, domain, &domainSize, &sidType)) {
        size_t total_len = strlen(name) + 1;
        *buffer = (char *)malloc(total_len);
        if (*buffer) {
          memcpy(*buffer, name, total_len);
        }
      }
    }
    free(pTokenUser);
  }

  CloseHandle(hToken);
  CloseHandle(hProcess);
}

// Get command line from process ID using Windows APIs
void get_command_from_pid(pid_t pid, char **buffer) {
  *buffer = NULL;

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
  if (!hProcess) {
    return;
  }

  // Try to get process image name
  char processPath[MAX_PATH];
  DWORD pathLen = sizeof(processPath);

  if (QueryFullProcessImageNameA(hProcess, 0, processPath, &pathLen)) {
    // Extract just the filename
    char *filename = strrchr(processPath, '\\');
    if (filename) {
      filename++; // Skip the backslash
    } else {
      filename = processPath;
    }

    size_t len = strlen(filename) + 1;
    *buffer = (char *)malloc(len);
    if (*buffer) {
      memcpy(*buffer, filename, len);
    }
  }

  CloseHandle(hProcess);
}

// Get memory usage from process ID
bool get_memory_usage_from_pid(pid_t pid, unsigned long *vmem, unsigned long *rmem) {
  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
  if (!hProcess) {
    return false;
  }

  PROCESS_MEMORY_COUNTERS_EX pmc;
  memset(&pmc, 0, sizeof(pmc));
  pmc.cb = sizeof(pmc);

  bool success = false;
  if (GetProcessMemoryInfo(hProcess, (PROCESS_MEMORY_COUNTERS *)&pmc, sizeof(pmc))) {
    *vmem = (unsigned long)pmc.PrivateUsage;
    *rmem = (unsigned long)pmc.WorkingSetSize;
    success = true;
  }

  CloseHandle(hProcess);
  return success;
}

void free_process_info_cache(void) {
  // Nothing to free in Windows version
}

// Fdinfo process monitoring stubs (Linux-specific feature not available on Windows)
void processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info) {
  (void)callback;
  (void)info;
}

void processinfo_drop_callback(const struct gpu_info *info) { (void)info; }

void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable) {
  (void)info;
  (void)enable;
}

void processinfo_sweep_fdinfos(void) {
  // Nothing to do on Windows
}

// Get process CPU and memory info
bool get_process_info(pid_t pid, struct process_cpu_usage *usage) {
  if (!usage)
    return false;

  HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
  if (!hProcess) {
    return false;
  }

  // Get CPU times
  FILETIME createTime, exitTime, kernelTime, userTime;
  if (GetProcessTimes(hProcess, &createTime, &exitTime, &kernelTime, &userTime)) {
    ULARGE_INTEGER kt, ut;
    kt.LowPart = kernelTime.dwLowDateTime;
    kt.HighPart = kernelTime.dwHighDateTime;
    ut.LowPart = userTime.dwLowDateTime;
    ut.HighPart = userTime.dwHighDateTime;

    // Convert from 100-nanosecond intervals to seconds
    usage->total_kernel_time = (double)kt.QuadPart / 10000000.0;
    usage->total_user_time = (double)ut.QuadPart / 10000000.0;
  } else {
    usage->total_kernel_time = 0.0;
    usage->total_user_time = 0.0;
  }

  // Get memory info
  PROCESS_MEMORY_COUNTERS_EX pmc;
  memset(&pmc, 0, sizeof(pmc));
  pmc.cb = sizeof(pmc);

  if (GetProcessMemoryInfo(hProcess, (PROCESS_MEMORY_COUNTERS *)&pmc, sizeof(pmc))) {
    usage->virtual_memory = (size_t)pmc.PrivateUsage;
    usage->resident_memory = (size_t)pmc.WorkingSetSize;
  } else {
    usage->virtual_memory = 0;
    usage->resident_memory = 0;
  }

  // Get current time
  memset(&usage->timestamp, 0, sizeof(usage->timestamp));

  CloseHandle(hProcess);
  return true;
}
