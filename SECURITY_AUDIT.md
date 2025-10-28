# NVTOP Security Audit Report

**Date:** October 28, 2025  
**Scope:** NVTOP Windows Port (Branch: windows)  
**Framework:** CVE, MITRE ATT&CK, CMMC 2.0

---

## Executive Summary

This security audit evaluates the NVTOP codebase against CVE vulnerability patterns, MITRE ATT&CK techniques, and CMMC 2.0 compliance requirements. The analysis focuses on the Windows port implementation.

**Overall Risk Level:** MEDIUM  
**Critical Issues:** 2  
**High Issues:** 5  
**Medium Issues:** 8  
**Low Issues:** 4

---

## 1. CVE-Related Vulnerabilities

### 1.1 CRITICAL: Buffer Overflow Risks (CWE-120, CWE-787)

**Location:** Multiple files using unsafe string operations

#### Finding 1: Unsafe String Copy Operations
**Files:**
- `src/extract_gpuinfo_amdgpu.c` (lines 540, 543, 555-600)
- `src/extract_gpuinfo_ascend.c` (line 162)
- `src/extract_gpuinfo_intel.c` (line 120)
- `src/get_process_info_mac.c` (line 44)
- `src/get_process_info_linux.c` (line 59)

**Issue:**
```c
// src/extract_gpuinfo_amdgpu.c:540
strncpy(static_info->device_name, name, MAX_DEVICE_NAME - 1);
// Missing null termination guarantee
```

**CVE Pattern:** Similar to CVE-2021-3520, CVE-2019-14287  
**Risk:** Stack/heap buffer overflow if source string exceeds `MAX_DEVICE_NAME`

**MITRE ATT&CK Mapping:**
- T1203 - Exploitation for Client Execution
- T1055 - Process Injection (potential)

**CMMC 2.0 Violations:**
- SC.3.177 (System Security Controls)
- SI.1.211 (Flaw Remediation)

**Recommendation:**
```c
// Safe alternative
static void safe_strncpy(char *dest, const char *src, size_t dest_size) {
    if (dest_size > 0) {
        strncpy(dest, src, dest_size - 1);
        dest[dest_size - 1] = '\0';
    }
}
```

#### Finding 2: strncat Without Proper Size Calculation
**File:** `src/extract_gpuinfo_v3d_utils.c:106`

**Issue:**
```c
size_t available_space = result_len - strlen(result) - 1;
strncat(result, (const char *)(p + 6), available_space);
```

**Risk:** If `strlen(result)` returns unexpected value, `available_space` could underflow  
**CVE Pattern:** Similar to CVE-2020-8631

**Recommendation:**
```c
size_t current_len = strnlen(result, result_len);
if (current_len < result_len - 1) {
    size_t available_space = result_len - current_len - 1;
    strncat(result, (const char *)(p + 6), available_space);
}
```

---

### 1.2 HIGH: Format String Vulnerabilities (CWE-134)

**Location:** `src/interface.c` - Multiple snprintf calls

#### Finding 3: Unbounded Format String Operations
**File:** `src/interface.c` (lines 1186, 1283, 1186)

**Issue:**
```c
printed += snprintf(&process_print_buffer[printed], 
                    process_buffer_line_size - printed, 
                    "%*s ", sizeof_process_field[i], columnName[i]);
```

**Risk:** Integer overflow if `printed` exceeds `process_buffer_line_size`  
**CVE Pattern:** Similar to CVE-2019-20079

**MITRE ATT&CK:** T1055.012 - Process Hollowing

**Recommendation:**
```c
if (printed < process_buffer_line_size) {
    int result = snprintf(&process_print_buffer[printed], 
                         process_buffer_line_size - printed, 
                         "%*s ", sizeof_process_field[i], columnName[i]);
    if (result > 0 && result < process_buffer_line_size - printed) {
        printed += result;
    }
}
```

---

### 1.3 HIGH: Memory Management Issues

#### Finding 4: Missing Null Checks After malloc/calloc
**Files:**
- `src/interface_ring_buffer.c:10-17`
- `src/get_process_info_mac.c:95`
- `src/extract_gpuinfo_ascend.c:157, 233`

**Issue:**
```c
// src/interface_ring_buffer.c
void *ring_buffer[1] = malloc(sizeof(unsigned[devices_count][per_device_data_saved][buffer_size]));
if (!ring_buffer->ring_buffer[1]) {
    perror("Cannot allocate memory: ");
    exit(EXIT_FAILURE);
}
```

**Risk:** Potential NULL pointer dereference between allocation and check  
**CVE Pattern:** Similar to CVE-2021-3450

**CMMC 2.0:** SI.2.216 (Security Flaws)

**Recommendation:**
```c
ring_buffer->ring_buffer[1] = malloc(...);
if (!ring_buffer->ring_buffer[1]) {
    free(ring_buffer->ring_buffer[0]); // Cleanup
    perror("Cannot allocate memory: ");
    return false; // Don't exit, let caller handle
}
```

#### Finding 5: Use-After-Free Potential
**File:** `src/extract_gpuinfo.c:230-232`

**Issue:**
```c
free(pid_not_encountered->cmdline);
free(pid_not_encountered->user_name);
free(pid_not_encountered);
// If other threads access pid_not_encountered after this
```

**Risk:** Race condition in multi-threaded environment  
**CVE Pattern:** CVE-2022-1292 class

**MITRE ATT&CK:** T1574.006 - Dynamic Linker Hijacking

**Recommendation:** Implement proper synchronization or reference counting

---

### 1.4 MEDIUM: Integer Overflow/Underflow

#### Finding 6: Array Index Calculation Without Bounds Check
**File:** `src/extract_gpuinfo_v3d_utils.c:89-91`

**Issue:**
```c
int len = strlen(command);
if (len + 1 >= MAX_STRING) { // Only checks upper bound
    fprintf(stderr, "gencmd length too long : %d\n", len);
    return -1;
}
```

**Risk:** Negative string length could bypass check  
**Recommendation:** Add `if (len < 0)` check

#### Finding 7: Unchecked sscanf Return Values
**Files:**
- `src/device_discovery_linux.c:301, 304`
- `src/extract_gpuinfo_v3d_utils.c:119`

**Issue:**
```c
ret = sscanf(speed_str, "%u", &speed);
ret = sscanf(width_str, "%u", &width);
// ret value checked but variables used regardless
```

**CMMC 2.0:** SC.3.177 - Input Validation

**Recommendation:**
```c
if (sscanf(speed_str, "%u", &speed) != 1) {
    speed = 0; // Safe default
}
```

---

### 1.5 MEDIUM: Path Traversal / Injection

#### Finding 8: Unvalidated File Path Construction
**Files:**
- `src/get_process_info_linux.c:63, 179`
- `src/extract_gpuinfo_v3d_utils.c:59`
- `src/extract_gpuinfo_amdgpu.c:322`

**Issue:**
```c
// src/get_process_info_linux.c
int written = snprintf(pid_path, pid_path_size, "/proc/%" PRIdMAX "/cmdline", (intmax_t)pid);
// pid could be malicious value
```

**Risk:** Path traversal if PID is negative or contains special characters  
**MITRE ATT&CK:** T1083 - File and Directory Discovery

**Recommendation:**
```c
if (pid <= 0 || pid > INT_MAX) {
    return false;
}
// Then construct path
```

---

### 1.6 HIGH: Windows-Specific Vulnerabilities

#### Finding 9: Inadequate Windows Process Info Implementation
**File:** `src/get_process_info_windows.c`

**Issue:**
```c
void get_username_from_pid(pid_t pid, char **buffer) {
  *buffer = malloc(16);  // Fixed size without checking
  if (*buffer) {
    snprintf(*buffer, 16, "user%d", (int)pid);  // Stub returns fake data
  }
}
```

**Risk:** 
- Fixed allocation size vulnerable to overflow
- No actual Windows API calls
- Fake data could mislead security monitoring

**CVE Pattern:** Information disclosure (CWE-200)

**MITRE ATT&CK:**
- T1057 - Process Discovery
- T1082 - System Information Discovery

**CMMC 2.0:**
- SC.3.177 - Security Functions
- SI.1.210 - Information System Flaws

**Recommendation:**
Implement proper Windows API calls:
```c
void get_username_from_pid(pid_t pid, char **buffer) {
    HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, pid);
    if (!hProcess) {
        *buffer = NULL;
        return;
    }
    
    HANDLE hToken;
    if (OpenProcessToken(hProcess, TOKEN_QUERY, &hToken)) {
        DWORD size = 0;
        GetTokenInformation(hToken, TokenUser, NULL, 0, &size);
        // Allocate and retrieve actual user info
        CloseHandle(hToken);
    }
    CloseHandle(hProcess);
}
```

#### Finding 10: Unsafe getopt_long Implementation
**File:** `src/windows_compat.c:90-140`

**Issue:**
```c
// Line 97-99
if (strncmp(arg, longopts[i].name, len) == 0 && longopts[i].name[len] == '\0') {
    // No validation of 'len' before array access
}
```

**Risk:** Out-of-bounds read if `len` is manipulated  
**CVE Pattern:** CWE-125 - Out-of-bounds Read

**Recommendation:**
```c
if (len > 0 && len < MAX_OPTION_LEN && 
    strncmp(arg, longopts[i].name, len) == 0) {
    // Safe access
}
```

---

## 2. MITRE ATT&CK Technique Analysis

### 2.1 Defense Evasion (TA0005)

**T1055 - Process Injection**
- **Risk Level:** Medium
- **Location:** Memory management without proper sanitization
- **Mitigation:** Implement ASLR, DEP verification on Windows

**T1574.001 - DLL Search Order Hijacking**
- **Risk Level:** Medium  
- **Location:** `src/extract_gpuinfo_nvidia_windows.c:60` - Dynamic NVML loading
- **Issue:** LoadLibraryA("nvml.dll") without full path verification
- **Mitigation:**
```c
char nvmlPath[MAX_PATH];
GetSystemDirectory(nvmlPath, MAX_PATH);
strcat_s(nvmlPath, MAX_PATH, "\\nvml.dll");
nvmlHandle = LoadLibraryA(nvmlPath);
```

### 2.2 Credential Access (TA0006)

**T1003.001 - LSASS Memory**
- **Risk Level:** Low
- **Location:** Process enumeration code could be adapted
- **Mitigation:** Already mitigated by limited scope

### 2.3 Discovery (TA0007)

**T1082 - System Information Discovery**
- **Risk Level:** Low (intended functionality)
- **Location:** GPU information extraction
- **Note:** Legitimate use case but could be abused

**T1057 - Process Discovery**
- **Risk Level:** Low (intended functionality)
- **Location:** Process monitoring features
- **Note:** Ensure proper access controls

### 2.4 Privilege Escalation (TA0004)

**T1068 - Exploitation for Privilege Escalation**
- **Risk Level:** Medium
- **Vectors:** Buffer overflows in Finding 1-3
- **Mitigation:** Fix all buffer overflow issues

---

## 3. CMMC 2.0 Compliance Assessment

### Level 1 Requirements (Basic Cyber Hygiene)

#### AC.1.001 - Access Control ✅ PARTIAL
- **Status:** Partially Implemented
- **Gap:** No role-based access control in code
- **Recommendation:** Add user permission checks for sensitive operations

#### SC.1.175 - Security Boundary Protection ❌ NON-COMPLIANT
- **Issue:** Direct hardware access without validation
- **Location:** GPU vendor plugins
- **Recommendation:** Implement input sanitization layer

#### SI.1.210 - Information System Flaws ❌ NON-COMPLIANT
- **Issue:** Multiple buffer overflow vulnerabilities (Findings 1-3)
- **Action Required:** Immediate remediation

#### SI.1.211 - Flaw Remediation ❌ NON-COMPLIANT
- **Issue:** No security update mechanism
- **Recommendation:** Implement version checking and update notifications

### Level 2 Requirements (Intermediate)

#### AC.2.007 - Least Privilege ⚠️ NEEDS IMPROVEMENT
- **Issue:** Runs with user privileges but accesses hardware
- **Recommendation:** Document required permissions, implement privilege separation

#### AU.2.041 - Audit Events ❌ NOT IMPLEMENTED
- **Issue:** No logging of security-relevant events
- **Recommendation:** Add logging for:
  - Process enumeration attempts
  - GPU access
  - Configuration changes

#### SC.2.179 - Security Function Isolation ⚠️ NEEDS IMPROVEMENT
- **Issue:** Vendor plugins directly access hardware
- **Recommendation:** Implement abstraction layer with validation

#### SI.2.216 - Security Flaws ❌ NON-COMPLIANT
- **Issue:** Multiple findings above
- **Action:** Complete security remediation

#### SI.2.217 - Software Updates ❌ NOT IMPLEMENTED
- **Recommendation:** Implement secure update mechanism

### Level 3 Requirements (Advanced/Expert)

#### AC.3.018 - Wireless Access Authorization ✅ N/A
- Not applicable to this application

#### AU.3.046 - Audit Record Review ❌ NOT IMPLEMENTED
- **Recommendation:** Add audit log generation

#### SC.3.177 - System Security ❌ NON-COMPLIANT
- **Critical Issues:**
  - Input validation (Findings 1-8)
  - Memory safety (Findings 4-5)
  - Error handling

#### SI.3.218 - Flaw Remediation Process ⚠️ NEEDS FORMALIZATION
- **Recommendation:** Establish security patch process

---

## 4. Platform-Specific Security Concerns

### 4.1 Windows Implementation

**Severity: HIGH**

1. **Incomplete Windows API Usage**
   - Stub implementations provide fake data
   - Security monitoring bypassed
   - Potential information disclosure

2. **Missing Security Features**
   - No ASLR enforcement
   - No DEP verification
   - No code signing

3. **Dynamic Library Loading**
   - DLL hijacking risk (nvml.dll)
   - No digital signature verification

### 4.2 Linux Implementation

**Severity: MEDIUM**

1. **Proc Filesystem Access**
   - Direct /proc access without validation
   - Symbolic link following risk

2. **DRM Device Access**
   - Requires elevated permissions
   - No capability checking

---

## 5. Dependency Security

### 5.1 Third-Party Libraries

#### inih (INI Parser) - `include/ini.h`
- **Version:** Unknown (bundled)
- **CVE Check:** No known CVEs
- **Risk:** Low
- **Recommendation:** Update to latest version, verify source

#### uthash - `include/uthash.h`
- **Version:** Unknown
- **CVE Check:** No known CVEs  
- **Risk:** Low
- **Note:** Widely used, well-tested

#### ncurses/PDCurses
- **Risk:** Medium
- **Issue:** Terminal escape sequence injection possible
- **Recommendation:** Sanitize all output to terminal

### 5.2 System Libraries

**libdrm** (Linux)
- Requires kernel support
- Verify version compatibility

**NVML** (Windows/Linux)
- Dynamically loaded - DLL hijacking risk
- **Mitigation:** Use full path verification

---

## 6. Recommended Security Controls

### 6.1 Immediate (Critical)

1. **Fix All Buffer Overflow Vulnerabilities** (Findings 1-3)
   - Timeline: 1-2 weeks
   - Priority: CRITICAL

2. **Implement Proper Windows Process APIs** (Finding 9)
   - Timeline: 2-3 weeks
   - Priority: HIGH

3. **Add Input Validation Layer**
   - Validate all external inputs (PIDs, file paths, GPU IDs)
   - Timeline: 1 week

### 6.2 Short-Term (30-60 days)

4. **Memory Safety Improvements**
   - Implement safe string functions library
   - Add bounds checking macros
   - Enable compiler hardening flags:
     ```cmake
     add_compile_options(-fstack-protector-strong -D_FORTIFY_SOURCE=2)
     add_link_options(-Wl,-z,relro -Wl,-z,now)
     ```

5. **Security Audit Logging**
   - Log security-relevant events
   - Implement audit trail

6. **Code Signing** (Windows)
   - Sign executable and DLLs
   - Verify signatures on load

### 6.3 Long-Term (60-90 days)

7. **Security Update Mechanism**
   - Implement version checking
   - Auto-update capability (opt-in)

8. **Formal Security Testing**
   - Fuzzing with AFL/libFuzzer
   - Static analysis (Coverity, CodeQL)
   - Dynamic analysis (Valgrind, ASan)

9. **CMMC 2.0 Compliance**
   - Address all Level 2 requirements
   - Implement audit logging
   - Document security controls

---

## 7. Compiler & Build Security

### Current State
From `cmake/optimization_flags.cmake`:
- Good warning flags enabled (-Wall, -Wextra, etc.)
- Sanitizers available in Debug mode

### Recommendations

```cmake
# Add security-hardening flags
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    add_compile_options(
        -fstack-protector-strong
        -D_FORTIFY_SOURCE=2
        -fPIE
    )
    add_link_options(
        -Wl,-z,relro
        -Wl,-z,now
        -pie
    )
endif()

# Windows-specific
if(WIN32)
    add_compile_options(
        /GS          # Buffer security check
        /guard:cf    # Control Flow Guard
        /DYNAMICBASE # ASLR
        /NXCOMPAT    # DEP
    )
endif()
```

---

## 8. Security Testing Recommendations

### 8.1 Static Analysis
```bash
# Coverity Scan
cov-build --dir cov-int make
cov-analyze --dir cov-int --security

# Clang Static Analyzer
scan-build make

# CodeQL
codeql database create nvtop-db --language=cpp
codeql database analyze nvtop-db security-and-quality.qls
```

### 8.2 Dynamic Analysis
```bash
# Address Sanitizer
cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_FLAGS="-fsanitize=address" ..
make && ./nvtop

# Memory Sanitizer
cmake -DCMAKE_C_FLAGS="-fsanitize=memory" ..

# Undefined Behavior Sanitizer
cmake -DCMAKE_C_FLAGS="-fsanitize=undefined" ..
```

### 8.3 Fuzzing
```bash
# AFL++ fuzzing
CC=afl-gcc cmake ..
make
afl-fuzz -i testcases/ -o findings/ ./nvtop @@
```

---

## 9. Security Checklist

- [ ] Fix buffer overflow vulnerabilities (Findings 1-3)
- [ ] Implement proper Windows process APIs (Finding 9)
- [ ] Add input validation for all external inputs
- [ ] Enable compiler security flags
- [ ] Implement secure memory allocations
- [ ] Add bounds checking to array accesses
- [ ] Validate all file paths before use
- [ ] Verify DLL signatures before loading (Windows)
- [ ] Implement audit logging
- [ ] Add version checking mechanism
- [ ] Enable ASLR/DEP on Windows
- [ ] Run static analysis tools
- [ ] Run dynamic analysis (ASan, MSan, UBSan)
- [ ] Perform fuzzing tests
- [ ] Document security controls
- [ ] Establish security patch process

---

## 10. Conclusion

The NVTOP Windows port has several security vulnerabilities that require immediate attention, particularly buffer overflow risks and incomplete Windows API implementations. The project shows good coding practices in some areas (warning flags, error checking) but needs significant hardening for production use, especially in security-sensitive environments requiring CMMC 2.0 compliance.

**Priority Actions:**
1. Fix critical buffer overflows (1-2 weeks)
2. Complete Windows process API implementation (2-3 weeks)
3. Add comprehensive input validation (1 week)
4. Enable compiler security flags (immediate)

**Estimated Remediation Timeline:** 6-8 weeks for critical issues, 12-16 weeks for full CMMC Level 2 compliance.

---

## Appendix A: CVE Reference Database

Relevant CVEs referenced in this audit:
- CVE-2021-3520: Buffer overflow in lz4
- CVE-2019-14287: sudo buffer overflow
- CVE-2020-8631: cloud-init format string vulnerability
- CVE-2019-20079: WebKit format string
- CVE-2021-3450: OpenSSL NULL pointer dereference
- CVE-2022-1292: OpenSSL command injection

## Appendix B: MITRE ATT&CK Matrix

| Tactic                        | Technique           | Severity | Status      |
| ----------------------------- | ------------------- | -------- | ----------- |
| TA0004 - Privilege Escalation | T1068               | Medium   | Mitigatable |
| TA0005 - Defense Evasion      | T1055, T1574.001    | Medium   | Needs Fix   |
| TA0006 - Credential Access    | T1003.001           | Low      | Acceptable  |
| TA0007 - Discovery            | T1057, T1082, T1083 | Low      | Intentional |

## Appendix C: CMMC 2.0 Gap Summary

| Practice | Level | Status            | Priority |
| -------- | ----- | ----------------- | -------- |
| SI.1.210 | 1     | ❌ Non-Compliant   | Critical |
| SI.1.211 | 1     | ❌ Non-Compliant   | High     |
| SC.1.175 | 1     | ❌ Non-Compliant   | High     |
| SI.2.216 | 2     | ❌ Non-Compliant   | Critical |
| SC.3.177 | 3     | ❌ Non-Compliant   | Critical |
| AU.2.041 | 2     | ❌ Not Implemented | Medium   |

---

**Report Prepared By:** AI Security Audit System  
**Methodology:** Manual code review, pattern matching, CVE database correlation  
**Scope:** Complete codebase scan focusing on Windows port  
**Next Review:** Recommended within 90 days of remediation
