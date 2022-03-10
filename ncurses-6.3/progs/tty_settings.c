/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2016,2017 Free Software Foundation, Inc.                       *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/

/****************************************************************************
 *  Author: Thomas E. Dickey                                                *
 ****************************************************************************/

#define USE_LIBTINFO
#include <tty_settings.h>

#include <fcntl.h>

MODULE_ID("$Id: tty_settings.c,v 1.7 2021/10/08 23:53:32 tom Exp $")

static int my_fd;
static TTY original_settings;
static bool can_restore = FALSE;

static void
failed(const char *msg)
{
    int code = errno;

    (void) fprintf(stderr, "%s: %s: %s\n", _nc_progname, msg, strerror(code));
    restore_tty_settings();
    (void) fprintf(stderr, "\n");
    ExitProgram(ErrSystem(code));
    /* NOTREACHED */
}

static bool
get_tty_settings(int fd, TTY * tty_settings)
{
    bool success = TRUE;
    my_fd = fd;
    if (fd < 0 || GET_TTY(my_fd, tty_settings) < 0) {
	success = FALSE;
    }
    return success;
}

/*
 * Open a file descriptor on the current terminal, to obtain its settings.
 * stderr is less likely to be redirected than stdout; try that first.
 */
int
save_tty_settings(TTY * tty_settings, bool need_tty)
{
    if (!get_tty_settings(STDERR_FILENO, tty_settings) &&
	!get_tty_settings(STDOUT_FILENO, tty_settings) &&
	!get_tty_settings(STDIN_FILENO, tty_settings)) {
	if (need_tty) {
	    int fd = open("/dev/tty", O_RDWR);
	    if (!get_tty_settings(fd, tty_settings)) {
		failed("terminal attributes");
	    }
	} else {
	    my_fd = fileno(stdout);
	}
    } else {
	can_restore = TRUE;
	original_settings = *tty_settings;
    }
    return my_fd;
}

void
restore_tty_settings(void)
{
    if (can_restore)
	SET_TTY(my_fd, &original_settings);
}

/* Set the modes if they've changed. */
void
update_tty_settings(TTY * old_settings, TTY * new_settings)
{
    if (memcmp(new_settings, old_settings, sizeof(TTY))) {
	SET_TTY(my_fd, new_settings);
    }
}
