/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
 * Copyright 2017 Free Software Foundation, Inc.                            *
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
/*
 * $Id: popup_msg.c,v 1.10 2020/02/02 23:34:34 tom Exp $
 *
 * Show a multi-line message in a window which may extend beyond the screen.
 *
 * Thomas Dickey - 2017/4/15.
 */

#include <test.priv.h>

#include <popup_msg.h>

#if HAVE_NEWPAD

static WINDOW *old_window;

static void
begin_popup(void)
{
    doupdate();
    old_window = dupwin(curscr);
}

static void
end_popup(void)
{
    touchwin(old_window);
    wnoutrefresh(old_window);
    doupdate();
    delwin(old_window);
}

/*
 * Display a temporary window, e.g., to display a help-message.
 */
void
popup_msg(WINDOW *parent, const char *const *msg)
{
    int x0 = 4;
    int y0 = 2;
    int y1 = 0;
    int y2 = 0;
    int wide = getmaxx(parent) - ((x0 + 1) * 2);
    int high = getmaxy(parent) - ((y0 + 1) * 2);
    WINDOW *help;
    WINDOW *data;
    int n;
    int width = 0;
    int length;
    int last_y;
    int ch = ERR;

    for (n = 0; msg[n] != 0; ++n) {
	int check = (int) strlen(msg[n]);
	if (width < check)
	    width = check;
    }
    length = n;

    if ((help = newwin(high, wide, y0, x0)) == 0)
	return;
    if ((data = newpad(length + 1, width)) == 0) {
	delwin(help);
	return;
    }

    begin_popup();

    keypad(data, TRUE);

    for (n = 0; n < length; ++n) {
	waddstr(data, msg[n]);
	if ((n + 1) < length) {
	    waddch(data, '\n');
	}
    }
    y2 = getcury(data);
    last_y = (y2 - (high - 3));

    do {
	switch (ch) {
	case KEY_HOME:
	    y1 = 0;
	    break;
	case KEY_END:
	    y1 = last_y;
	    break;
	case KEY_PREVIOUS:
	case KEY_PPAGE:
	    if (y1 > 0) {
		y1 -= high / 2;
		if (y1 < 0)
		    y1 = 0;
	    } else {
		beep();
	    }
	    break;
	case KEY_NEXT:
	case KEY_NPAGE:
	    if (y1 < last_y) {
		y1 += high / 2;
		if (y1 > last_y)
		    y1 = last_y;
	    } else {
		beep();
	    }
	    break;
	case CTRL('P'):
	case KEY_UP:
	    if (y1 > 0)
		--y1;
	    else
		beep();
	    break;
	case CTRL('N'):
	case KEY_DOWN:
	    if (y1 < last_y)
		++y1;
	    else
		beep();
	    break;
	default:
	    beep();
	    break;
	case ERR:
	    break;
	}
	werase(help);
	box(help, 0, 0);
	wnoutrefresh(help);
	pnoutrefresh(data, y1, 0, y0 + 1, x0 + 1, high, wide);
	doupdate();
    } while ((ch = wgetch(data)) != ERR && ch != QUIT && ch != ESCAPE);
    werase(help);
    wrefresh(help);
    delwin(help);
    delwin(data);

    end_popup();
}

void
popup_msg2(WINDOW *parent, char **msg)
{
    popup_msg(parent, (const char *const *) msg);
}

#else
void
popup_msg(WINDOW *parent, const char *const *msg)
{
    (void) parent;
    (void) msg;
    beep();
}
#endif
