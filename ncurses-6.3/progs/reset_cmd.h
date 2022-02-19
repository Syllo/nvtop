/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2016 Free Software Foundation, Inc.                            *
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
 *  Author: Thomas E Dickey                                                 *
 ****************************************************************************/

/*
 * $Id: reset_cmd.h,v 1.9 2021/10/02 20:58:58 tom Exp $
 *
 * Utility functions for resetting terminal.
 */
#ifndef RESET_CMD_H
#define RESET_CMD_H 1
/* *INDENT-OFF* */

#define USE_LIBTINFO
#define __INTERNAL_CAPS_VISIBLE	/* we need to see has_hardware_tabs */
#include <progs.priv.h>

#undef CTRL
#define CTRL(x)	((x) & 0x1f)

extern bool send_init_strings(int /* fd */, TTY * /* old_settings */);
extern void print_tty_chars(TTY * /* old_settings */, TTY * /* new_settings */);
extern void reset_flush(void);
extern void reset_start(FILE * /* fp */, bool /* is_reset */, bool /* is_init */ );
extern void reset_tty_settings(int /* fd */, TTY * /* tty_settings */, int /* noset */);
extern void set_control_chars(TTY * /* tty_settings */, int /* erase */, int /* intr */, int /* kill */);
extern void set_conversions(TTY * /* tty_settings */);

#if HAVE_SIZECHANGE
extern void set_window_size(int /* fd */, short * /* high */, short * /* wide */);
#endif

extern const char *_nc_progname;

/* *INDENT-ON* */

#endif /* RESET_CMD_H */
