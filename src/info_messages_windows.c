/*
 * Windows-specific system information messages
 * Copyright (C) 2025
 *
 * This file is part of Nvtop.
 */

#include "list.h"
#include "nvtop/info_messages.h"
#include <stdio.h>
#include <string.h>
#include <windows.h>

void nvtop_get_os_info_string(char *buffer, size_t buffer_size) {
  OSVERSIONINFOEX osvi;
  ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

// Note: GetVersionEx is deprecated but still works for basic info
#pragma warning(suppress : 4996)
  if (GetVersionEx((OSVERSIONINFO *)&osvi)) {
    snprintf(buffer, buffer_size, "Windows %lu.%lu Build %lu", osvi.dwMajorVersion, osvi.dwMinorVersion,
             osvi.dwBuildNumber);
  } else {
    snprintf(buffer, buffer_size, "Windows (Unknown version)");
  }
}

void nvtop_get_kernel_version_string(char *buffer, size_t buffer_size) {
  OSVERSIONINFOEX osvi;
  ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

#pragma warning(suppress : 4996)
  if (GetVersionEx((OSVERSIONINFO *)&osvi)) {
    snprintf(buffer, buffer_size, "NT %lu.%lu.%lu", osvi.dwMajorVersion, osvi.dwMinorVersion, osvi.dwBuildNumber);
  } else {
    snprintf(buffer, buffer_size, "Unknown");
  }
}

void nvtop_get_hostname_string(char *buffer, size_t buffer_size) {
  DWORD size = (DWORD)buffer_size;
  if (!GetComputerNameA(buffer, &size)) {
    snprintf(buffer, buffer_size, "localhost");
  }
}

void get_info_messages(struct list_head *devices, unsigned *num_messages, const char ***messages) {
  // Stub implementation - no special info messages on Windows yet
  (void)devices;
  *num_messages = 0;
  *messages = NULL;
}
