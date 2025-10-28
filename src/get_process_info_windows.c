/*
 * Windows-specific process information retrieval stub
 * Copyright (C) 2025
 *
 * This file is part of Nvtop.
 */

#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/get_process_info.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Stub implementations for Windows - process info not fully implemented yet
void get_username_from_pid(pid_t pid, char **buffer) {
  *buffer = malloc(16);
  if (*buffer) {
    snprintf(*buffer, 16, "user%d", (int)pid);
  }
}

void get_command_from_pid(pid_t pid, char **buffer) {
  *buffer = malloc(64);
  if (*buffer) {
    snprintf(*buffer, 64, "process_%d", (int)pid);
  }
}

bool get_memory_usage_from_pid(pid_t pid, unsigned long *vmem, unsigned long *rmem) {
  // Stub - return fake values
  *vmem = 1024 * 1024; // 1MB
  *rmem = 512 * 1024;  // 512KB
  return true;
}

void free_process_info_cache(void) {
  // Nothing to free in stub version
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

bool get_process_info(pid_t pid, struct process_cpu_usage *usage) {
  if (!usage)
    return false;

  usage->total_user_time = 0.0;
  usage->total_kernel_time = 0.0;
  usage->virtual_memory = 1024 * 1024; // 1MB stub
  usage->resident_memory = 512 * 1024; // 512KB stub
  // Zero out timestamp
  memset(&usage->timestamp, 0, sizeof(usage->timestamp));

  return true;
}
