/*
 * Safe string utility functions for NVTOP
 * Copyright (C) 2025
 *
 * This file is part of Nvtop and provides secure string handling
 * to prevent buffer overflow vulnerabilities.
 */

#ifndef NVTOP_STRING_UTILS_H__
#define NVTOP_STRING_UTILS_H__

#include <stddef.h>
#include <string.h>

/**
 * Safe string copy that always null-terminates
 * @param dest Destination buffer
 * @param src Source string
 * @param dest_size Total size of destination buffer
 * @return Number of characters copied (excluding null terminator)
 */
static inline size_t safe_strncpy(char *dest, const char *src, size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size == 0) {
    return 0;
  }

  size_t i;
  for (i = 0; i < dest_size - 1 && src[i] != '\0'; i++) {
    dest[i] = src[i];
  }
  dest[i] = '\0';
  return i;
}

/**
 * Safe string concatenation that always null-terminates
 * @param dest Destination buffer
 * @param src Source string to append
 * @param dest_size Total size of destination buffer
 * @return true on success, false if truncation occurred
 */
static inline bool safe_strncat(char *dest, const char *src, size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size == 0) {
    return false;
  }

  // Find current length, but don't exceed buffer
  size_t dest_len = strnlen(dest, dest_size);
  if (dest_len >= dest_size - 1) {
    return false; // Already full
  }

  size_t available = dest_size - dest_len - 1;
  size_t src_len = strlen(src);

  if (src_len > available) {
    // Truncation will occur
    memcpy(dest + dest_len, src, available);
    dest[dest_size - 1] = '\0';
    return false;
  }

  memcpy(dest + dest_len, src, src_len + 1);
  return true;
}

/**
 * Safe formatted print to buffer
 * @param buffer Destination buffer
 * @param size Size of buffer
 * @param current_pos Current position in buffer
 * @param format Printf format string
 * @return New position in buffer, or -1 on error
 */
static inline int safe_snprintf_append(char *buffer, size_t size, int current_pos, const char *format, ...) {
  if (buffer == NULL || format == NULL || current_pos < 0 || (size_t)current_pos >= size) {
    return -1;
  }

  va_list args;
  va_start(args, format);

  int remaining = (int)(size - current_pos);
  int written = vsnprintf(buffer + current_pos, remaining, format, args);

  va_end(args);

  if (written < 0 || written >= remaining) {
    // Error or truncation
    buffer[size - 1] = '\0';
    return -1;
  }

  return current_pos + written;
}

/**
 * Validate PID is in reasonable range
 * @param pid Process ID to validate
 * @return true if valid, false otherwise
 */
static inline bool is_valid_pid(pid_t pid) {
  return pid > 0 && pid < 4194304; // Linux max PID
}

/**
 * Sanitize path component (basic check)
 * @param path Path to check
 * @return true if safe, false if contains suspicious patterns
 */
static inline bool is_safe_path_component(const char *path) {
  if (path == NULL) {
    return false;
  }

  // Check for path traversal attempts
  if (strstr(path, "..") != NULL) {
    return false;
  }

  // Check for null bytes (path injection)
  size_t len = strlen(path);
  for (size_t i = 0; i < len; i++) {
    if (path[i] == '\0') {
      return false;
    }
  }

  return true;
}

#endif // NVTOP_STRING_UTILS_H__
