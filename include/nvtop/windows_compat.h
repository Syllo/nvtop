/*
 * Windows compatibility layer for POSIX functions
 * Copyright (C) 2025
 */

#ifndef NVTOP_WINDOWS_COMPAT_H
#define NVTOP_WINDOWS_COMPAT_H

#ifdef _WIN32

#include <direct.h>
#include <io.h>
#include <process.h>
#include <sys/types.h>
#include <windows.h>

// POSIX to Windows mappings
#define sleep(x) Sleep((x) * 1000)
#define usleep(x) Sleep((x) / 1000)
#define getpid() _getpid()
#ifndef snprintf
#define snprintf _snprintf
#endif
#define strdup _strdup
#define fileno _fileno
#define isatty _isatty
#define index strchr
#define mkdir(path, mode) _mkdir(path)

// ncurses header location for Windows
#define ncurses_h <ncursesw/ncurses.h>

// getopt replacement for Windows
extern char *optarg;
extern int optind, opterr, optopt;

struct option {
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

#define no_argument 0
#define required_argument 1
#define optional_argument 2

int getopt(int argc, char *const argv[], const char *optstring);
int getopt_long(int argc, char *const argv[], const char *optstring, const struct option *longopts, int *longindex);

// Signal handling stubs
#define SIGINT 2
#define SIGTERM 15
#define SIGWINCH 28
#define SIGCONT 18

typedef void (*sighandler_t)(int);
sighandler_t signal(int signum, sighandler_t handler);

#ifndef SIG_IGN
#define SIG_IGN ((sighandler_t)1)
#endif

// Atomic types
#if defined(_MSC_VER)
#define sig_atomic_t volatile long
#else
typedef volatile int sig_atomic_t;
#endif

#endif // _WIN32

#endif // NVTOP_WINDOWS_COMPAT_H
