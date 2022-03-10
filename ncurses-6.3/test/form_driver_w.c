/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2013-2014,2017 Free Software Foundation, Inc.                  *
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
 *   Author:  Gaute Hope, 2013                                              *
 ****************************************************************************/

/*
 * $Id: form_driver_w.c,v 1.16 2020/02/02 23:34:34 tom Exp $
 *
 * Test form_driver_w (int, int, wchar_t), a wide char aware
 * replacement of form_driver.
 */

#include <test.priv.h>
#include <popup_msg.h>

#if USE_WIDEC_SUPPORT && USE_LIBFORM && (defined(NCURSES_VERSION_PATCH) && NCURSES_VERSION_PATCH >= 20131207)

#include <form.h>

int
main(void)
{
    static const char *help[] =
    {
	"Commands:",
	"  ^D,^Q,ESC           - quit program",
	"  <Tab>,<Down>        - move to next field",
	"  <BackTab>,<Up>      - move to previous field",
	0
    };

#define NUM_FIELDS 3
#define MyRow(n) (4 + (n) * 2)
#define MyCol(n) 10
    FIELD *field[NUM_FIELDS + 1];
    FORM *my_form;
    bool done = FALSE;
    int n;

    setlocale(LC_ALL, "");

    /* Initialize curses */
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);

    /* Initialize the fields */
    for (n = 0; n < NUM_FIELDS; ++n) {
	field[n] = new_field(1, 10, MyRow(n), 18, 0, 0);
	set_field_back(field[n], A_UNDERLINE);
	/* Print a line for the option  */
	field_opts_off(field[n], O_AUTOSKIP);
	/* Don't go to next field when this is filled */
    }
    field[n] = NULL;

    /* Create the form and post it */
    my_form = new_form(field);
    post_form(my_form);
    refresh();

    for (n = 0; n < NUM_FIELDS; ++n) {
	mvprintw(MyRow(n), MyCol(n), "Value %d:", n + 1);
    }

    /* Loop through to get user requests */
    while (!done) {
	wint_t ch;
	int ret = get_wch(&ch);

	mvprintw(MyRow(NUM_FIELDS),
		 MyCol(NUM_FIELDS),
		 "Got %d (%#x), type: %s",
		 (int) ch,
		 (int) ch,
		 (ret == KEY_CODE_YES)
		 ? "KEY_CODE_YES"
		 : ((ret == OK)
		    ? "OK"
		    : ((ret == ERR)
		       ? "ERR"
		       : "?")));
	clrtoeol();

	switch (ret) {
	case KEY_CODE_YES:
	    switch (ch) {
	    case KEY_DOWN:
		/* Go to next field */
		form_driver_w(my_form, KEY_CODE_YES, REQ_NEXT_FIELD);
		/* Go to the end of the present buffer */
		/* Leaves nicely at the last character */
		form_driver_w(my_form, KEY_CODE_YES, REQ_END_LINE);
		break;
	    case KEY_BTAB:
	    case KEY_UP:
		/* Go to previous field */
		form_driver_w(my_form, KEY_CODE_YES, REQ_PREV_FIELD);
		form_driver_w(my_form, KEY_CODE_YES, REQ_END_LINE);
		break;
	    default:
		break;
	    }
	    break;
	case OK:
	    switch (ch) {
	    case CTRL('D'):
	    case QUIT:
	    case ESCAPE:
		done = TRUE;
		break;
	    case '\t':
		form_driver_w(my_form, KEY_CODE_YES, REQ_NEXT_FIELD);
		form_driver_w(my_form, KEY_CODE_YES, REQ_END_LINE);
		break;
	    case HELP_KEY_1:
		popup_msg(form_win(my_form), help);
		break;
	    default:
		form_driver_w(my_form, OK, (wchar_t) ch);
		break;
	    }
	    break;
	}
    }

    /* Un post form and free the memory */
    unpost_form(my_form);
    free_form(my_form);
    for (n = 0; n < NUM_FIELDS; ++n) {
	free_field(field[n]);
    }

    endwin();
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the wide-ncurses and forms library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif /* USE_WIDEC_SUPPORT */
