/*
 * Windows getopt implementation
 * Public domain implementation
 */

#ifdef _WIN32

#include "nvtop/windows_compat.h"
#include <stdio.h>
#include <string.h>

char *optarg = NULL;
int optind = 1;
int opterr = 1;
int optopt = 0;

static char *nextchar = NULL;

int getopt(int argc, char *const argv[], const char *optstring) {
  if (optind >= argc) {
    return -1;
  }

  if (nextchar == NULL || *nextchar == '\0') {
    if (optind >= argc || argv[optind][0] != '-' || argv[optind][1] == '\0') {
      return -1;
    }

    if (strcmp(argv[optind], "--") == 0) {
      optind++;
      return -1;
    }

    nextchar = argv[optind] + 1;
  }

  char c = *nextchar++;
  char *temp = strchr(optstring, c);

  if (*nextchar == '\0') {
    optind++;
    nextchar = NULL;
  }

  if (temp == NULL || c == ':') {
    optopt = c;
    if (opterr && *optstring != ':') {
      fprintf(stderr, "%s: invalid option -- %c\n", argv[0], c);
    }
    return '?';
  }

  if (temp[1] == ':') {
    if (nextchar && *nextchar != '\0') {
      optarg = nextchar;
      optind++;
    } else if (temp[2] == ':') {
      // Optional argument
      optarg = NULL;
      optind++;
    } else {
      // Required argument
      if (optind < argc - 1) {
        optarg = argv[++optind];
        optind++;
      } else {
        optopt = c;
        if (opterr && *optstring != ':') {
          fprintf(stderr, "%s: option requires an argument -- %c\n", argv[0], c);
        }
        return ':';
      }
    }
    nextchar = NULL;
  }

  return c;
}

int getopt_long(int argc, char *const argv[], const char *optstring, const struct option *longopts, int *longindex) {
  if (optind >= argc) {
    return -1;
  }

  if (argv[optind][0] == '-' && argv[optind][1] == '-') {
    // Long option
    char *arg = argv[optind] + 2;
    char *eq = strchr(arg, '=');
    size_t len = eq ? (size_t)(eq - arg) : strlen(arg);

    for (int i = 0; longopts[i].name != NULL; i++) {
      if (strncmp(arg, longopts[i].name, len) == 0 && longopts[i].name[len] == '\0') {
        if (longindex) {
          *longindex = i;
        }

        optind++;

        if (longopts[i].has_arg == no_argument) {
          optarg = NULL;
          if (eq) {
            fprintf(stderr, "%s: option '--%s' doesn't allow an argument\n", argv[0], longopts[i].name);
            return '?';
          }
        } else if (longopts[i].has_arg == required_argument) {
          if (eq) {
            optarg = eq + 1;
          } else if (optind < argc) {
            optarg = argv[optind++];
          } else {
            fprintf(stderr, "%s: option '--%s' requires an argument\n", argv[0], longopts[i].name);
            return '?';
          }
        } else {
          // optional_argument
          optarg = eq ? eq + 1 : NULL;
        }

        if (longopts[i].flag) {
          *longopts[i].flag = longopts[i].val;
          return 0;
        }
        return longopts[i].val;
      }
    }

    fprintf(stderr, "%s: unrecognized option '--%s'\n", argv[0], arg);
    optind++;
    return '?';
  }

  // Short option
  return getopt(argc, argv, optstring);
}

// Signal handling stub
static sighandler_t signal_handlers[32] = {0};

sighandler_t signal(int signum, sighandler_t handler) {
  if (signum < 0 || signum >= 32) {
    return SIG_IGN;
  }
  sighandler_t old = signal_handlers[signum];
  signal_handlers[signum] = handler;

  // For Windows console control, set up handler for Ctrl+C
  if (signum == SIGINT) {
    // Could implement SetConsoleCtrlHandler here
  }

  return old;
}

#endif // _WIN32
