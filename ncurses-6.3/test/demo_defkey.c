/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
 * Copyright 2002-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: demo_defkey.c,v 1.31 2020/12/26 17:55:13 tom Exp $
 *
 * Demonstrate the define_key() function.
 * Thomas Dickey - 2002/11/23
 */

#include <test.priv.h>

#if defined(NCURSES_VERSION) && NCURSES_EXT_FUNCS

#define MY_LOGFILE "demo_defkey.log"

/*
 * Log the most recently-written line to our logfile
 */
static void
log_last_line(WINDOW *win)
{
    FILE *fp;

    if ((fp = fopen(MY_LOGFILE, "a")) != 0) {
	char temp[256];
	int y, x, n;
	int need = sizeof(temp) - 1;
	if (need > COLS)
	    need = COLS;
	getyx(win, y, x);
	wmove(win, y - 1, 0);
	n = winnstr(win, temp, need);
	while (n-- > 0) {
	    if (isspace(UChar(temp[n])))
		temp[n] = '\0';
	    else
		break;
	}
	wmove(win, y, x);
	fprintf(fp, "%s\n", temp);
	fclose(fp);
    }
}

/*
 * Convert a character to visible form.
 */
static char *
visichar(int ch)
{
    static char temp[20];

    ch = UChar(ch);
    assert(ch >= 0 && ch < 256);
    if (ch == '\\') {
	_nc_STRCPY(temp, "\\\\", sizeof(temp));
    } else if (ch == '\033') {
	_nc_STRCPY(temp, "\\E", sizeof(temp));
    } else if (ch < ' ') {
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "\\%03o", ch);
    } else if (ch >= 127) {
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "\\%03o", ch);
    } else {
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "%c", ch);
    }
    return temp;
}

/*
 * Convert a string to visible form.
 */
static char *
visible(const char *string)
{
    char *result = 0;

    if (string != 0 && *string != '\0') {
	int pass;
	int n;
	size_t need = 1;

	for (pass = 0; pass < 2; ++pass) {
	    for (n = 0; string[n] != '\0'; ++n) {
		char temp[80];
		_nc_STRNCPY(temp, visichar(string[n]), sizeof(temp) - 2);
		if (pass) {
		    _nc_STRCAT(result, temp, need);
		} else {
		    need += strlen(temp);
		}
	    }
	    if (!pass)
		result = typeCalloc(char, need);
	}
    } else {
	result = typeCalloc(char, (size_t) 1);
    }
    return result;
}

static void
really_define_key(WINDOW *win, const char *new_string, int code)
{
    int rc;
    const char *code_name = keyname(code);
    char *old_string;
    char *vis_string = 0;
    char temp[80];

    if (code_name == 0) {
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "Keycode %d", code);
	code_name = temp;
    }

    if ((old_string = keybound(code, 0)) != 0) {
	wprintw(win, "%s is %s\n",
		code_name,
		vis_string = visible(old_string));
    } else {
	wprintw(win, "%s is not bound\n",
		code_name);
    }
    log_last_line(win);

    if (vis_string != 0) {
	free(vis_string);
	vis_string = 0;
    }

    vis_string = visible(new_string);
    if ((rc = key_defined(new_string)) > 0) {
	wprintw(win, "%s was bound to %s\n", vis_string, keyname(rc));
	log_last_line(win);
    } else if (new_string != 0 && rc < 0) {
	wprintw(win, "%s conflicts with longer strings\n", vis_string);
	log_last_line(win);
    }
    rc = define_key(new_string, code);
    if (rc == ERR) {
	wprintw(win, "%s unchanged\n", code_name);
	log_last_line(win);
    } else if (new_string != 0) {
	wprintw(win, "%s is now bound to %s\n",
		vis_string,
		code_name);
	log_last_line(win);
    } else if (old_string != 0) {
	wprintw(win, "%s deleted\n", code_name);
	log_last_line(win);
    }
    if (vis_string != 0)
	free(vis_string);
    if (old_string != 0)
	free(old_string);
}

static void
duplicate(WINDOW *win, NCURSES_CONST char *name, int code)
{
    char *value = tigetstr(name);

    if (value != 0) {
	const char *prefix = 0;

	if (!(strncmp) (value, "\033[", (size_t) 2)) {
	    prefix = "\033O";
	} else if (!(strncmp) (value, "\033O", (size_t) 2)) {
	    prefix = "\033[";
	}
	if (prefix != 0) {
	    char temp[BUFSIZ];
	    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
			"%s%s", prefix, value + 2);
	    really_define_key(win, temp, code);
	}
    }
}

static void
redefine(WINDOW *win, char *string, int code)
{
    really_define_key(win, string, code);
}

static void
remove_definition(WINDOW *win, int code)
{
    really_define_key(win, 0, code);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    char *fkeys[12];
    int n;
    int ch;
    WINDOW *win;

    unlink(MY_LOGFILE);

    initscr();
    (void) cbreak();		/* take input chars one at a time, no wait for \n */
    (void) noecho();		/* don't echo input */

    printw("This demo is best on xterm: it reverses the definitions for f1-f12,\n");
    printw("adds duplicate definitions for cursor application and normal modes,\n");
    printw("and removes any definitions for the mini keypad.  Type any of those:\n");
    refresh();

    win = newwin(LINES - 3, COLS, 3, 0);
    scrollok(win, TRUE);
    keypad(win, TRUE);
    wmove(win, 0, 0);

    /* we do the define_key() calls after keypad(), since the first call to
     * keypad() initializes the corresponding data.
     */
    for (n = 0; n < 12; ++n) {
	char name[20];
	_nc_SPRINTF(name, _nc_SLIMIT(sizeof(name)) "kf%d", n + 1);
	fkeys[n] = tigetstr(name);
    }
    for (n = 0; n < 12; ++n) {
	redefine(win, fkeys[11 - n], KEY_F(n + 1));
    }

    duplicate(win, "kcub1", KEY_LEFT);
    duplicate(win, "kcuu1", KEY_UP);
    duplicate(win, "kcud1", KEY_DOWN);
    duplicate(win, "kcuf1", KEY_RIGHT);

    remove_definition(win, KEY_A1);
    remove_definition(win, KEY_A3);
    remove_definition(win, KEY_B2);
    remove_definition(win, KEY_C1);
    remove_definition(win, KEY_C3);

    really_define_key(win, "\033O", 1023);

    while ((ch = wgetch(win)) != ERR) {
	const char *name = keyname(ch);
	wprintw(win, "Keycode %d, name %s\n",
		ch,
		name != 0 ? name : "<null>");
	log_last_line(win);
	wclrtoeol(win);
	if (ch == 'q')
	    break;
    }
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires the ncurses library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
