#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>

int main(void) {
  printf("Testing ncurses initialization on Windows...\n");
  printf("TERM environment variable: %s\n", getenv("TERM") ? getenv("TERM") : "(not set)");

  // Set TERM if not set
  if (getenv("TERM") == NULL) {
    _putenv("TERM=xterm-256color");
    printf("Set TERM=xterm-256color\n");
  }

  printf("Calling initscr()...\n");
  fflush(stdout);

  WINDOW *win = initscr();
  if (win == NULL) {
    fprintf(stderr, "ERROR: initscr() returned NULL\n");
    return 1;
  }

  printf("initscr() succeeded!\n");
  printw("Hello from ncurses!");
  refresh();
  napms(2000); // Sleep for 2 seconds

  endwin();
  printf("endwin() called, exiting normally\n");

  return 0;
}
