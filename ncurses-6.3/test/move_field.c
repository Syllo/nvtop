/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
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
 * $Id: move_field.c,v 1.9 2021/06/12 21:30:34 tom Exp $
 *
 * Demonstrate move_field().
 */

#include <test.priv.h>

#if USE_LIBFORM

#include <edit_field.h>
#include <popup_msg.h>

#ifdef HAVE_NETBSD_FORM_H
#define form_field_row(field) (field)->form_row
#define form_field_col(field) (field)->form_col
#else /* e.g., SVr4, ncurses */
#define form_field_row(field) (field)->frow
#define form_field_col(field) (field)->fcol
#endif

#define DO_DEMO	CTRL('F')	/* actual key for toggling demo-mode */
#define MY_DEMO	EDIT_FIELD('f')	/* internal request-code */

static char empty[] = "";
static FIELD *all_fields[100];
/* *INDENT-OFF* */
static struct {
    int code;
    int result;
    const char *help;
} commands[] = {
    { CTRL('A'),     REQ_BEG_FIELD,   "go to beginning of field" },
    { CTRL('D'),     REQ_DOWN_FIELD,  "move downward to field" },
    { CTRL('E'),     REQ_END_FIELD,   "go to end of field" },
    { CTRL('H'),     REQ_DEL_PREV,    "delete previous character" },
    { CTRL('I'),     REQ_NEXT_FIELD,  "go to next field" },
    { CTRL('K'),     REQ_CLR_EOF,     "clear to end of field" },
    { CTRL('N'),     REQ_NEXT_FIELD,  "go to next field" },
    { CTRL('P'),     REQ_PREV_FIELD,  "go to previous field" },
    { CTRL('Q'),     MY_QUIT,         "exit form" },
    { CTRL('U'),     REQ_UP_FIELD,    "move upward to field" },
    { CTRL('W'),     REQ_NEXT_WORD,   "go to next word" },
    { CTRL('X'),     REQ_CLR_FIELD,   "clear field" },
    { CTRL('['),     MY_QUIT,         "exit form" },
    { KEY_F(1),      MY_HELP,         "show this screen", },
    { KEY_BACKSPACE, REQ_DEL_PREV,    "delete previous character" },
    { KEY_BTAB,      REQ_PREV_FIELD,  "go to previous field" },
    { KEY_DOWN,      REQ_DOWN_CHAR,   "move down 1 character" },
    { KEY_END,       REQ_LAST_FIELD,  "go to last field" },
    { KEY_HOME,      REQ_FIRST_FIELD, "go to first field" },
    { KEY_LEFT,      REQ_LEFT_CHAR,   "move left 1 character" },
    { KEY_NEXT,      REQ_NEXT_FIELD,  "go to next field" },
    { KEY_PREVIOUS,  REQ_PREV_FIELD,  "go to previous field" },
    { KEY_RIGHT,     REQ_RIGHT_CHAR,  "move right 1 character" },
    { KEY_UP,        REQ_UP_CHAR,     "move up 1 character" },
    { DO_DEMO,       MY_DEMO,         "move current field with cursor keys" }
};
/* *INDENT-ON* */

static void
my_help_edit_field(void)
{
    int used = 0;
    unsigned n;
    char **msgs = typeCalloc(char *, 3 + SIZEOF(commands));

    msgs[used++] = strdup("Defined form edit/traversal keys:");
    for (n = 0; n < SIZEOF(commands); ++n) {
	char *msg;
	const char *name;
	const char *code = keyname(commands[n].code);
	size_t need = 5;
#ifdef NCURSES_VERSION
	if ((name = form_request_name(commands[n].result)) == 0)
#endif
	    name = commands[n].help;
	need = 5 + strlen(code) + strlen(name);
	msg = typeMalloc(char, need);
	_nc_SPRINTF(msg, _nc_SLIMIT(need) "%s -- %s", code, name);
	msgs[used++] = msg;
    }
    msgs[used++] =
	strdup("Arrow keys move within a field as you would expect.");
    msgs[used] = 0;
    popup_msg2(stdscr, msgs);
    for (n = 0; msgs[n] != 0; ++n) {
	free(msgs[n]);
    }
    free(msgs);
}

static FIELD *
make_label(const char *label, int frow, int fcol)
{
    FIELD *f = new_field(1, (int) strlen(label), frow, fcol, 0, 0);

    if (f) {
	set_field_buffer(f, 0, label);
	set_field_opts(f, (int) ((unsigned) field_opts(f) & ~O_ACTIVE));
    }
    return (f);
}

static FIELD *
make_field(int frow, int fcol, int rows, int cols)
{
    FIELD *f = new_field(rows, cols, frow, fcol, 0, 1);

    if (f) {
	set_field_back(f, A_UNDERLINE);
	init_edit_field(f, empty);
    }
    return (f);
}

static void
erase_form(FORM *f)
{
    WINDOW *w = form_win(f);
    WINDOW *s = form_sub(f);

    unpost_form(f);
    werase(w);
    wrefresh(w);
    delwin(s);
    delwin(w);
}

static FieldAttrs *
my_field_attrs(FIELD *f)
{
    return (FieldAttrs *) field_userptr(f);
}

static int
buffer_length(FIELD *f)
{
    return my_field_attrs(f)->row_lengths[0];
}

static void
set_buffer_length(FIELD *f, int length)
{
    my_field_attrs(f)->row_lengths[0] = length;
}

static int
offset_in_field(FORM *form)
{
    FIELD *field = current_field(form);
    int currow, curcol;

    form_getyx(form, currow, curcol);
    return curcol + currow * (int) field->dcols;
}

static void
inactive_field(FIELD *f)
{
    set_field_back(f, my_field_attrs(f)->background);
}

static int
my_edit_field(FORM *form, int *result)
{
    int ch = wgetch(form_win(form));
    int status;
    FIELD *before;
    unsigned n;
    int before_row;
    int before_col;
    int before_off = offset_in_field(form);

    form_getyx(form, before_row, before_col);
    before = current_field(form);
    set_field_back(before, A_NORMAL);
    if (ch <= KEY_MAX) {
	set_field_back(before, A_REVERSE);
    } else if (ch <= MAX_FORM_COMMAND) {
	inactive_field(before);
    }

    *result = ch;
    for (n = 0; n < SIZEOF(commands); ++n) {
	if (commands[n].code == ch) {
	    *result = commands[n].result;
	    break;
	}
    }

    status = form_driver(form, *result);

    if (status == E_OK) {
	bool modified = TRUE;
	int length = buffer_length(before);

	if (length < before_off)
	    length = before_off;
	switch (*result) {
	case REQ_CLR_EOF:
	    length = before_off;
	    break;
	case REQ_CLR_EOL:
	    if ((int) (before_row + 1) == (int) (before->rows))
		length = before_off;
	    break;
	case REQ_CLR_FIELD:
	    length = 0;
	    break;
	case REQ_DEL_CHAR:
	    if (length > before_off)
		--length;
	    break;
	case REQ_DEL_PREV:
	    if (length > 0) {
		if (before_col > 0) {
		    --length;
		} else if (before_row > 0) {
		    length -= (int) before->cols + before_col;
		}
	    }
	    break;
	case REQ_NEW_LINE:
	    length += (int) before->cols;
	    break;

	default:
	    modified = (ch < MIN_FORM_COMMAND
			&& isprint(ch));
	    break;
	}

	/*
	 * If we do not force a re-validation, then field_buffer 0 will
	 * be lagging by one character.
	 */
	if (modified && form_driver(form, REQ_VALIDATION) == E_OK && *result
	    < MIN_FORM_COMMAND)
	    ++length;

	set_buffer_length(before, length);
    }

    if (current_field(form) != before)
	inactive_field(before);
    return status;
}

static FIELD **
copy_fields(FIELD **source, size_t length)
{
    FIELD **target = calloc(length + 1, sizeof(FIELD *));
    memcpy(target, source, length * sizeof(FIELD *));
    return target;
}

/* display a status message to show what's happening */
static void
show_status(FORM *form, FIELD *field)
{
    WINDOW *sub = form_sub(form);
    int currow, curcol;

    getyx(stdscr, currow, curcol);
    mvprintw(LINES - 1, 0,
	     "Field at [%d,%d].  Press %s to quit moving.",
	     getbegy(sub) + form_field_row(field),
	     getbegx(sub) + form_field_col(field),
	     keyname(DO_DEMO));
    clrtobot();
    move(currow, curcol);
    refresh();
}

/*
 * Move the current label+field in response to cursor-keys (or h,j,k,l) until
 * a control/F is read.
 */
static void
do_demo(FORM *form)
{
    int count = field_count(form);
    FIELD *my_field = current_field(form);

    if (count > 0 && my_field != NULL) {
	size_t needed = (size_t) count;
	FIELD **old_fields = copy_fields(form_fields(form), needed);
	FIELD **new_fields = copy_fields(form_fields(form), needed);

	if (old_fields != NULL && new_fields != NULL) {
	    bool found = FALSE;
	    int ch;

	    /* TODO: move the label too, in parallel with the editing field */

	    /* remove the current field from the newer list */
	    for (ch = 0; ch <= count; ++ch) {
		if (found) {
		    new_fields[ch - 1] = new_fields[ch];
		} else if (new_fields[ch] == my_field) {
		    found = TRUE;
		}
	    }

	    if (found) {
		int currow, curcol;

		getyx(stdscr, currow, curcol);

		show_status(form, my_field);
		while ((ch = wgetch(form_win(form))) != DO_DEMO) {
		    int field_y = form_field_row(my_field);
		    int field_x = form_field_col(my_field);

		    switch (ch) {
		    case 'h':
		    case KEY_LEFT:
			if (field_x > 0)
			    field_x--;
			break;
		    case 'j':
		    case KEY_DOWN:
			field_y++;
			break;
		    case 'k':
		    case KEY_UP:
			if (field_y > 0)
			    field_y--;
			break;
		    case 'l':
		    case KEY_RIGHT:
			field_x++;
			break;
		    case CTRL('Q'):
		    case CTRL('['):
			ch = DO_DEMO;
			/* FALLTHRU */
		    case DO_DEMO:
			break;
		    default:
			continue;
		    }

		    if (ch == DO_DEMO)
			break;

		    /* alter connected fields temporarily to move the field */
		    unpost_form(form);
		    set_form_fields(form, new_fields);
		    post_form(form);

		    /* TODO: update screen position on success */
		    move_field(my_field, field_y, field_x);

		    /* restore the form's list of fields */
		    unpost_form(form);
		    set_form_fields(form, old_fields);
		    post_form(form);

		    show_status(form, my_field);
		}

		/* cleanup */
		move(LINES - 1, 0);
		clrtobot();
		move(currow, curcol);
		refresh();
	    }
	}
	free(old_fields);
	free(new_fields);
    }
}

static int
my_form_driver(FORM *form, int c)
{
    switch (c) {
    case MY_QUIT:
	if (form_driver(form, REQ_VALIDATION) == E_OK)
	    return (TRUE);
	break;
    case MY_HELP:
	my_help_edit_field();
	break;
    case MY_DEMO:
	do_demo(form);
	break;
    default:
	beep();
	break;
    }
    return (FALSE);
}

static void
demo_forms(void)
{
    FORM *form;
    int c;
    unsigned n = 0;
    const char *fname;

    /* describe the form */
    all_fields[n++] = make_label("Sample Form", 0, 15);

    fname = "Last Name";
    all_fields[n++] = make_label(fname, 2, 0);
    all_fields[n++] = make_field(3, 0, 1, 18);
    set_field_type(all_fields[n - 1], TYPE_ALPHA, 1);

    fname = "First Name";
    all_fields[n++] = make_label(fname, 2, 20);
    all_fields[n++] = make_field(3, 20, 1, 12);
    set_field_type(all_fields[n - 1], TYPE_ALPHA, 1);

    fname = "Middle Name";
    all_fields[n++] = make_label(fname, 2, 34);
    all_fields[n++] = make_field(3, 34, 1, 12);
    set_field_type(all_fields[n - 1], TYPE_ALPHA, 1);

    fname = "Comments";
    all_fields[n++] = make_label(fname, 5, 0);
    all_fields[n++] = make_field(6, 0, 4, 46);
    init_edit_field(all_fields[n - 1], empty);

    all_fields[n] = (FIELD *) 0;

    if ((form = new_form(all_fields)) != 0) {
	int finished = 0;

	post_form(form);

	while (!finished) {
	    switch (my_edit_field(form, &c)) {
	    case E_OK:
		break;
	    case E_UNKNOWN_COMMAND:
		finished = my_form_driver(form, c);
		break;
	    default:
		beep();
		break;
	    }
	}

	erase_form(form);

	free_form(form);
    }
    for (c = 0; all_fields[c] != 0; c++) {
	free_edit_field(all_fields[c]);
	free_field(all_fields[c]);
    }
    noraw();
    nl();
}

int
main(void)
{
    setlocale(LC_ALL, "");

    initscr();
    cbreak();
    noecho();
    raw();
    nonl();			/* lets us read ^M's */
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);

    if (has_colors()) {
	start_color();
	init_pair(1, COLOR_WHITE, COLOR_BLUE);
	init_pair(2, COLOR_GREEN, COLOR_BLACK);
	init_pair(3, COLOR_CYAN, COLOR_BLACK);
	bkgd((chtype) COLOR_PAIR(1));
	refresh();
    }

    demo_forms();

    endwin();
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the curses form library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
