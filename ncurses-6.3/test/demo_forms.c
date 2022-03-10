/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
 * Copyright 2003-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: demo_forms.c,v 1.58 2020/03/21 15:57:59 tom Exp $
 *
 * Demonstrate a variety of functions from the form library.
 * Thomas Dickey - 2003/4/26
 */

#include <test.priv.h>

#if USE_LIBFORM

#include <edit_field.h>

typedef struct {
    char *name;
    char *value;
} MY_DATA;

static MY_DATA *my_data;

static int d_option = 0;
static int j_value = 0;
static int m_value = 0;
static int o_value = 0;
static char *t_value = 0;

static void
failed(const char *s)
{
    perror(s);
    ExitProgram(EXIT_FAILURE);
}

static void
chomp(char *value)
{
    size_t have = strlen(value);
    while (have != 0 && (value[have - 1] == '\n' || value[have - 1] == '\r')) {
	value[--have] = '\0';
    }
}

static int
trimmed(const char *value)
{
    int result = (int) strlen(value);
    while (result > 0 && isspace(UChar(value[result - 1]))) {
	--result;
    }
    return result;
}

static char *
get_data(const char *name)
{
    char *result = t_value;
    if (my_data != 0) {
	int n;
	for (n = 0; my_data[n].name != 0; ++n) {
	    if (!strcmp(name, my_data[n].name)) {
		result = my_data[n].value;
		break;
	    }
	}
    }
    return result;
}

/*
 * Read (possibly) multi-line data with name+value pairs.
 */
static void
read_data(const char *filename)
{
    FILE *fp = fopen(filename, "r");

    if (fp != 0) {
	char buffer[BUFSIZ];
	char *colon;
	int more = 0;
	int item = 0;

	my_data = typeCalloc(MY_DATA, (size_t) 100);	/* FIXME */
	while (fgets(buffer, sizeof(buffer), fp) != 0) {
	    chomp(buffer);
	    if (more) {
		if (strcmp(buffer, ".")) {
		    char *prior = my_data[more - 1].value;
		    size_t need = strlen(buffer) + 2 + strlen(prior);
		    char *value = typeRealloc(char, need, prior);
		    if (value == 0)
			failed("realloc");
		    _nc_STRCAT(value, "\n", need);
		    _nc_STRCAT(value, buffer, need);
		    my_data[more - 1].value = value;
		} else {
		    more = 0;
		}
	    } else if (*buffer == '#') {
		continue;
	    } else if ((colon = strchr(buffer, ':')) != 0) {
		char *name;
		char *value;
		*colon++ = '\0';
		name = strdup(buffer);
		value = strdup(colon);
		if (name == 0 || value == 0)
		    failed("strdup");
		my_data[item].name = name;
		my_data[item].value = value;
		more = ++item;
	    } else {
		failed("expected a colon");
	    }
	}
	fclose(fp);
    } else {
	failed(filename);
    }
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

/*
 * Define each field with an extra one, for reflecting "actual" text.
 */
static FIELD *
make_field(const char *label, int frow, int fcol, int rows, int cols)
{
    FIELD *f = new_field(rows, cols, frow, fcol, o_value, 1);

    if (f) {
	set_field_back(f, A_UNDERLINE);
	/*
	 * If -j and -d options are combined, -j loses.  It is documented in
	 * "Character User Interface Programming", page 12-15 that setting
	 * O_STATIC off makes the form library ignore justification.
	 */
	set_field_just(f, j_value);
	if (d_option) {
	    if (has_colors()) {
		set_field_fore(f, (chtype) COLOR_PAIR(2));
		set_field_back(f, (A_UNDERLINE | (chtype) COLOR_PAIR(3)));
	    } else {
		set_field_fore(f, A_BOLD);
	    }
	    /*
	     * The field_opts_off() call dumps core with Solaris curses,
	     * but that is a known bug in Solaris' form library -TD
	     */
	    field_opts_off(f, O_STATIC);
	    set_max_field(f, m_value);
	}

	init_edit_field(f, get_data(label));
    }
    return (f);
}

static void
display_form(FORM *f)
{
    WINDOW *w;
    int rows, cols;

    scale_form(f, &rows, &cols);

    /*
     * Put the form at the upper-left corner of the display, with just a box
     * around it.
     */
    if ((w = newwin(rows + 2, cols + 4, 0, 0)) != (WINDOW *) 0) {
	set_form_win(f, w);
	set_form_sub(f, derwin(w, rows, cols, 1, 2));
	box(w, 0, 0);
	keypad(w, TRUE);

	if (post_form(f) != E_OK)
	    wrefresh(w);
    }
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

static void
show_insert_mode(bool insert_mode)
{
    MvAddStr(5, 57, (insert_mode
		     ? "form_status: insert "
		     : "form_status: overlay"));
}

#define O_SELECTABLE (O_ACTIVE | O_VISIBLE)

static FIELD *
another_field(FORM *form, FIELD *field)
{
    FIELD **f = form_fields(form);
    FIELD *result = 0;
    int n;

    for (n = 0; f[n] != 0; ++n) {
	if (f[n] != field) {
	    result = f[n];
	    field_opts_on(result, O_SELECTABLE);
	    break;
	}
    }
    return result;
}

static int
my_form_driver(FORM *form, int c)
{
    static bool insert_mode = TRUE;
    FIELD *field;

    switch (c) {
    case MY_QUIT:
	if (form_driver(form, REQ_VALIDATION) == E_OK)
	    return (TRUE);
	break;
    case MY_HELP:
	help_edit_field();
	break;
    case MY_EDT_MODE:
	if ((field = current_field(form)) != 0) {
	    set_current_field(form, another_field(form, field));
	    if ((unsigned) field_opts(field) & O_EDIT) {
		field_opts_off(field, O_EDIT);
		set_field_status(field, 0);
	    } else {
		field_opts_on(field, O_EDIT);
	    }
	    set_current_field(form, field);
	}
	break;
    case MY_INS_MODE:
	/* there should be a form_status() function, but there is none */
	if (!insert_mode) {
	    if (form_driver(form, REQ_INS_MODE) == E_OK) {
		insert_mode = TRUE;
	    }
	} else {
	    if (form_driver(form, REQ_OVL_MODE) == E_OK) {
		insert_mode = FALSE;
	    }
	}
	show_insert_mode(insert_mode);
	refresh();
	break;
    default:
	beep();
	break;
    }
    return (FALSE);
}

static void
show_current_field(WINDOW *win, FORM *form)
{
    FIELD *field;
    int field_rows, field_cols, field_max;
    int currow, curcol;

    if (has_colors()) {
	wbkgd(win, (chtype) COLOR_PAIR(1));
    }
    werase(win);
    form_getyx(form, currow, curcol);
    wprintw(win, "Cursor: %d,%d", currow, curcol);
    if (data_ahead(form))
	waddstr(win, " ahead");
    if (data_behind(form))
	waddstr(win, " behind");
    waddch(win, '\n');

    if ((field = current_field(form)) != 0) {
	FIELDTYPE *type;
	int nbuf;

	wprintw(win, "Page %d%s, Field %d/%d%s:",
		form_page(form),
		new_page(field) ? "*" : "",
		field_index(field), field_count(form),
		field_arg(field) ? "(arg)" : "");
	if ((type = field_type(field)) != 0) {
	    if (type == TYPE_ALNUM)
		waddstr(win, "ALNUM");
	    else if (type == TYPE_ALPHA)
		waddstr(win, "ALPHA");
	    else if (type == TYPE_ENUM)
		waddstr(win, "ENUM");
	    else if (type == TYPE_INTEGER)
		waddstr(win, "INTEGER");
#ifdef NCURSES_VERSION
	    else if (type == TYPE_IPV4)
		waddstr(win, "IPV4");
#endif
	    else if (type == TYPE_NUMERIC)
		waddstr(win, "NUMERIC");
	    else if (type == TYPE_REGEXP)
		waddstr(win, "REGEXP");
	    else
		waddstr(win, "other");
	}

	if ((unsigned) field_opts(field) & O_EDIT)
	    waddstr(win, " editable");
	else
	    waddstr(win, " readonly");

	if (field_status(field))
	    waddstr(win, " modified");

	if (dynamic_field_info(field, &field_rows, &field_cols, &field_max)
	    != ERR) {
	    wprintw(win, " size %dx%d (max %d)",
		    field_rows, field_cols, field_max);
	}

	waddch(win, ' ');
	(void) wattrset(win, AttrArg(field_fore(field), 0));
	waddstr(win, "fore");
	wattroff(win, (int) field_fore(field));

	waddch(win, '/');

	(void) wattrset(win, AttrArg(field_back(field), 0));
	waddstr(win, "back");
	wattroff(win, (int) field_back(field));

	wprintw(win, ", pad '%c'", field_pad(field));

	waddstr(win, "\n");
	for (nbuf = 0; nbuf <= 2; ++nbuf) {
	    char *buffer;
	    if ((buffer = field_buffer(field, nbuf)) != 0) {
		wprintw(win, "buffer %d:", nbuf);
		(void) wattrset(win, A_REVERSE);
		if (nbuf) {
		    waddnstr(win, buffer, trimmed(buffer));
		} else {
		    waddstr(win, buffer);
		}
		wattroff(win, A_REVERSE);
		waddstr(win, "\n");
	    }
	}
    }
    wrefresh(win);
}

static void
demo_forms(void)
{
    FORM *form;
    FIELD *f[100];		/* will memset to zero */
    int c;
    unsigned n = 0;
    int pg;
    const char *fname;
    static const char *my_enum[] =
    {"first", "second", "third", 0};

#ifdef NCURSES_MOUSE_VERSION
    mousemask(ALL_MOUSE_EVENTS, (mmask_t *) 0);
#endif

    help_edit_field();

    MvAddStr(4, 57, "Forms Entry Test");
    show_insert_mode(TRUE);

    refresh();

    /* describe the form */
    memset(f, 0, sizeof(f));
    for (pg = 0; pg < 4; ++pg) {
	char label[80];
	_nc_SPRINTF(label, _nc_SLIMIT(sizeof(label))
		    "Sample Form Page %d", pg + 1);
	f[n++] = make_label(label, 0, 15);
	set_new_page(f[n - 1], TRUE);

	switch (pg) {
	default:
	    fname = "Last Name";
	    f[n++] = make_label(fname, 2, 0);
	    f[n++] = make_field(fname, 3, 0, 1, 18);
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);

	    fname = "First Name";
	    f[n++] = make_label(fname, 2, 20);
	    f[n++] = make_field(fname, 3, 20, 1, 12);
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);

	    fname = "Middle Name";
	    f[n++] = make_label(fname, 2, 34);
	    f[n++] = make_field(fname, 3, 34, 1, 12);
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);
	    break;

	case 1:
	    fname = "Last Name";
	    f[n++] = make_label(fname, 2, 0);
	    f[n++] = make_field(fname, 3, 0, 1, 12);
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);

	    fname = "First Name";
	    f[n++] = make_label(fname, 2, 14);
	    f[n++] = make_field(fname, 3, 14, 1, 12);
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);

	    fname = "MI";
	    f[n++] = make_label(fname, 2, 28);
	    f[n++] = make_field(fname, 3, 28, 1, 1);
	    set_field_pad(f[n - 1], '?');
	    set_field_type(f[n - 1], TYPE_ALPHA, 1);

	    fname = "First/Second/Third";
	    f[n++] = make_label(fname, 2, 32);
	    f[n++] = make_field(fname, 3, 32, 1, 12);
	    set_field_type(f[n - 1], TYPE_ENUM, my_enum, 0, 0);
	    break;

	case 2:
	    fname = "Host Name";
	    f[n++] = make_label(fname, 2, 0);
	    f[n++] = make_field(fname, 3, 0, 1, 24);
	    set_field_type(f[n - 1], TYPE_ALNUM, 1);

#ifdef NCURSES_VERSION
	    fname = "IP Address";
	    f[n++] = make_label(fname, 2, 26);
	    f[n++] = make_field(fname, 3, 26, 1, 16);
	    set_field_type(f[n - 1], TYPE_IPV4, 1);
#endif
	    break;

	case 3:
	    fname = "Four digits";
	    f[n++] = make_label(fname, 2, 0);
	    f[n++] = make_field(fname, 3, 0, 1, 10);
	    set_field_type(f[n - 1], TYPE_INTEGER, 4, 0, 0);

	    fname = "Numeric";
	    f[n++] = make_label(fname, 2, 13);
	    f[n++] = make_field(fname, 3, 13, 1, 12);
	    set_field_type(f[n - 1], TYPE_NUMERIC, 3, -10000.0, 100000000.0);

	    fname = "Phone number";
	    f[n++] = make_label(fname, 2, 27);
	    f[n++] = make_field(fname, 3, 27, 1, 16);
	    set_field_type(f[n - 1], TYPE_REGEXP,
			   "^([0-9]-)?[0-9]{3}-[0-9]{3}-[0-9]{4} *$");;
	    break;
	}

	fname = "Comments";
	f[n++] = make_label(fname, 5, 0);
	f[n++] = make_field(fname, 6, 0, 4, 46);
	init_edit_field(f[n - 1], get_data(fname));
    }

    f[n] = (FIELD *) 0;

    if ((form = new_form(f)) != 0) {
	WINDOW *w;
	WINDOW *also;
	int finished = 0;

	display_form(form);

	w = form_win(form);
	also = newwin(getmaxy(stdscr) - getmaxy(w), COLS, getmaxy(w), 0);
	show_current_field(also, form);

	while (!finished) {
	    switch (edit_field(form, &c)) {
	    case E_OK:
		break;
	    case E_UNKNOWN_COMMAND:
		finished = my_form_driver(form, c);
		break;
	    default:
		beep();
		break;
	    }
	    show_current_field(also, form);
	}

	erase_form(form);

	free_form(form);
    }
    for (c = 0; f[c] != 0; c++) {
	free_edit_field(f[c]);
	free_field(f[c]);
    }
    noraw();
    nl();

#ifdef NCURSES_MOUSE_VERSION
    mousemask(0, (mmask_t *) 0);
#endif
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: demo_forms [options] [data file]"
	,""
	," -d        make fields dynamic"
	," -j value  justify (1=left, 2=center, 3=right)"
	," -m value  set maximum size of dynamic fields"
	," -o value  specify number of offscreen rows in new_field()"
	," -t value  specify text to fill fields initially"
    };
    unsigned int j;
    for (j = 0; j < SIZEOF(tbl); ++j)
	fprintf(stderr, "%s\n", tbl[j]);
    exit(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int ch;

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "dj:m:o:t:")) != -1) {
	switch (ch) {
	case 'd':
	    d_option = TRUE;
	    break;
	case 'j':
	    j_value = atoi(optarg);
	    if (j_value < NO_JUSTIFICATION
		|| j_value > JUSTIFY_RIGHT)
		usage();
	    break;
	case 'm':
	    m_value = atoi(optarg);
	    break;
	case 'o':
	    o_value = atoi(optarg);
	    break;
	case 't':
	    t_value = optarg;
	    break;
	default:
	    usage();

	}
    }
    while (optind < argc) {
	read_data(argv[optind++]);
    }

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
