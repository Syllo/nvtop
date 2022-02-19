/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2008-2016,2017 Free Software Foundation, Inc.                  *
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
 *  Author: Thomas E. Dickey                        2008                    *
 ****************************************************************************/

/*
 * tabs.c --  set terminal hard-tabstops
 */

#define USE_LIBTINFO
#include <progs.priv.h>
#include <tty_settings.h>

MODULE_ID("$Id: tabs.c,v 1.50 2021/10/10 00:54:41 tom Exp $")

static GCC_NORETURN void usage(void);

const char *_nc_progname;
static int max_cols;

static void
failed(const char *s)
{
    perror(s);
    ExitProgram(EXIT_FAILURE);
}

static int
putch(int c)
{
    return putchar(c);
}

static char *
skip_csi(char *value)
{
    if (UChar(*value) == 0x9b)
	++value;
    else if (!strncmp(value, "\033[", 2))
	value += 2;
    return value;
}

/*
 * If the terminal uses ANSI clear_all_tabs, then it is not necessary to first
 * move to the left margin before clearing tabs.
 */
static bool
ansi_clear_tabs(void)
{
    bool result = FALSE;
    if (VALID_STRING(clear_all_tabs)) {
	char *param = skip_csi(clear_all_tabs);
	if (!strcmp(param, "3g"))
	    result = TRUE;
    }
    return result;
}

static void
do_tabs(int *tab_list)
{
    int last = 1;
    int stop;
    bool first = TRUE;

    while ((stop = *tab_list++) > 0) {
	if (first) {
	    first = FALSE;
	    putchar('\r');
	}
	if (last < stop) {
	    while (last++ < stop) {
		if (last > max_cols)
		    break;
		putchar(' ');
	    }
	}
	if (stop <= max_cols) {
	    tputs(set_tab, 1, putch);
	    last = stop;
	} else {
	    break;
	}
    }
    putchar('\r');
}

/*
 * Decode a list of tab-stops from a string, returning an array of integers.
 * If the margin is positive (because the terminal does not support margins),
 * work around this by adding the margin to the decoded values.
 */
static int *
decode_tabs(const char *tab_list, int margin)
{
    int *result = typeCalloc(int, strlen(tab_list) + (unsigned) max_cols);
    int n = 0;
    int value = 0;
    int prior = 0;
    int ch;

    if (result == 0)
	failed("decode_tabs");

    if (margin < 0)
	margin = 0;

    while ((ch = *tab_list++) != '\0') {
	if (isdigit(UChar(ch))) {
	    value *= 10;
	    value += (ch - '0');
	} else if (ch == ',') {
	    result[n] = value + prior + margin;
	    if (n > 0 && result[n] <= result[n - 1]) {
		fprintf(stderr,
			"%s: tab-stops are not in increasing order: %d %d\n",
			_nc_progname, value, result[n - 1]);
		free(result);
		result = 0;
		break;
	    }
	    ++n;
	    value = 0;
	    prior = 0;
	} else if (ch == '+') {
	    if (n)
		prior = result[n - 1];
	}
    }

    if (result != 0) {
	/*
	 * If there is only one value, then it is an option such as "-8".
	 */
	if ((n == 0) && (value > 0)) {
	    int step = value;
	    value = 1;
	    while (n < max_cols - 1) {
		result[n++] = value + margin;
		value += step;
	    }
	}

	/*
	 * Add the last value, if any.
	 */
	result[n++] = value + prior + margin;
	result[n] = 0;
    }

    return result;
}

static void
print_ruler(int *tab_list, const char *new_line)
{
    int last = 0;
    int n;

    /* first print a readable ruler */
    for (n = 0; n < max_cols; n += 10) {
	int ch = 1 + (n / 10);
	char buffer[20];
	_nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
		    "----+----%c",
		    ((ch < 10)
		     ? (ch + '0')
		     : (ch + 'A' - 10)));
	printf("%.*s", ((max_cols - n) > 10) ? 10 : (max_cols - n), buffer);
    }
    printf("%s", new_line);

    /* now, print '*' for each stop */
    for (n = 0, last = 0; (tab_list[n] > 0) && (last < max_cols); ++n) {
	int stop = tab_list[n];

	while (++last < stop) {
	    if (last <= max_cols) {
		putchar('-');
	    } else {
		break;
	    }
	}
	if (last <= max_cols) {
	    putchar('*');
	    last = stop;
	} else {
	    break;
	}
    }
    while (++last <= max_cols)
	putchar('-');
    printf("%s", new_line);
}

/*
 * Write an '*' on each tabstop, to demonstrate whether it lines up with the
 * ruler.
 */
static void
write_tabs(int *tab_list, const char *new_line)
{
    int stop;

    while ((stop = *tab_list++) > 0 && stop <= max_cols) {
	fputs((stop == 1) ? "*" : "\t*", stdout);
    };
    /* also show a tab _past_ the stops */
    if (stop < max_cols)
	fputs("\t+", stdout);
    fputs(new_line, stdout);
}

/*
 * Trim leading/trailing blanks, as well as blanks after a comma.
 * Convert embedded blanks to commas.
 */
static char *
trimmed_tab_list(const char *source)
{
    char *result = strdup(source);
    if (result != 0) {
	int j, k, last;

	for (j = k = last = 0; result[j] != 0; ++j) {
	    int ch = UChar(result[j]);
	    if (isspace(ch)) {
		if (last == '\0') {
		    continue;
		} else if (isdigit(last) || last == ',') {
		    ch = ',';
		}
	    } else if (ch == ',') {
		;
	    } else {
		if (last == ',')
		    result[k++] = (char) last;
		result[k++] = (char) ch;
	    }
	    last = ch;
	}
	result[k] = '\0';
    }
    return result;
}

static bool
comma_is_needed(const char *source)
{
    bool result = FALSE;

    if (source != 0) {
	size_t len = strlen(source);
	if (len != 0)
	    result = (source[len - 1] != ',');
    } else {
	result = FALSE;
    }
    return result;
}

/*
 * Add a command-line parameter to the tab-list.  It can be blank- or comma-
 * separated (or a mixture).  For simplicity, empty tabs are ignored, e.g.,
 *	tabs 1,,6,11
 *	tabs 1,6,11
 * are treated the same.
 */
static const char *
add_to_tab_list(char **append, const char *value)
{
    char *result = *append;
    char *copied = trimmed_tab_list(value);

    if (copied != 0 && *copied != '\0') {
	const char *comma = ",";
	size_t need = 1 + strlen(copied);

	if (*copied == ',')
	    comma = "";
	else if (!comma_is_needed(*append))
	    comma = "";

	need += strlen(comma);
	if (*append != 0)
	    need += strlen(*append);

	result = malloc(need);
	if (result == 0)
	    failed("add_to_tab_list");

	*result = '\0';
	if (*append != 0) {
	    _nc_STRCPY(result, *append, need);
	    free(*append);
	}
	_nc_STRCAT(result, comma, need);
	_nc_STRCAT(result, copied, need);

	*append = result;
    }
    free(copied);
    return result;
}

/*
 * If the terminal supports it, (re)set the left margin and return true.
 * Otherwise, return false.
 */
static bool
do_set_margin(int margin, bool no_op)
{
    bool result = FALSE;

    if (margin == 0) {		/* 0 is special case for resetting */
	if (VALID_STRING(clear_margins)) {
	    result = TRUE;
	    if (!no_op)
		tputs(clear_margins, 1, putch);
	}
    } else if (margin-- < 0) {	/* margin will be 0-based from here on */
	result = TRUE;
    } else if (VALID_STRING(set_left_margin)) {
	result = TRUE;
	if (!no_op) {
	    /*
	     * assuming we're on the first column of the line, move the cursor
	     * to the column at which we will set a margin.
	     */
	    if (VALID_STRING(column_address)) {
		tputs(TIPARM_1(column_address, margin), 1, putch);
	    } else if (margin >= 1) {
		if (VALID_STRING(parm_right_cursor)) {
		    tputs(TIPARM_1(parm_right_cursor, margin), 1, putch);
		} else {
		    while (margin-- > 0)
			putch(' ');
		}
	    }
	    tputs(set_left_margin, 1, putch);
	}
    } else if (VALID_STRING(set_left_margin_parm)) {
	result = TRUE;
	if (!no_op) {
	    if (VALID_STRING(set_right_margin_parm)) {
		tputs(TIPARM_1(set_left_margin_parm, margin), 1, putch);
	    } else {
		tputs(TIPARM_2(set_left_margin_parm, margin, max_cols), 1, putch);
	    }
	}
    } else if (VALID_STRING(set_lr_margin)) {
	result = TRUE;
	if (!no_op) {
	    tputs(TIPARM_2(set_lr_margin, margin, max_cols), 1, putch);
	}
    }
    return result;
}

/*
 * Check for illegal characters in the tab-list.
 */
static bool
legal_tab_list(const char *tab_list)
{
    bool result = TRUE;

    if (tab_list != 0 && *tab_list != '\0') {
	if (comma_is_needed(tab_list)) {
	    int n;

	    for (n = 0; tab_list[n] != '\0'; ++n) {
		int ch = UChar(tab_list[n]);

		if (!(isdigit(ch) || ch == ',' || ch == '+')) {
		    fprintf(stderr,
			    "%s: unexpected character found '%c'\n",
			    _nc_progname, ch);
		    result = FALSE;
		    break;
		}
	    }
	} else {
	    fprintf(stderr, "%s: trailing comma found '%s'\n", _nc_progname, tab_list);
	    result = FALSE;
	}
    } else {
	/* if no list given, default to "tabs -8" */
    }
    return result;
}

static char *
skip_list(char *value)
{
    while (*value != '\0' &&
	   (isdigit(UChar(*value)) ||
	    isspace(UChar(*value)) ||
	    strchr("+,", UChar(*value)) != 0)) {
	++value;
    }
    return value;
}

static void
usage(void)
{
#define DATA(s) s "\n"
    static const char msg[] =
    {
	DATA("Usage: tabs [options] [tabstop-list]")
	DATA("")
	DATA("Options:")
	DATA("  -0       reset tabs")
	DATA("  -8       set tabs to standard interval")
	DATA("  -a       Assembler, IBM S/370, first format")
	DATA("  -a2      Assembler, IBM S/370, second format")
	DATA("  -c       COBOL, normal format")
	DATA("  -c2      COBOL compact format")
	DATA("  -c3      COBOL compact format extended")
	DATA("  -d       debug (show ruler with expected/actual tab positions)")
	DATA("  -f       FORTRAN")
	DATA("  -n       no-op (do not modify terminal settings)")
	DATA("  -p       PL/I")
	DATA("  -s       SNOBOL")
	DATA("  -u       UNIVAC 1100 Assembler")
	DATA("  -T name  use terminal type 'name'")
	DATA("  -V       print version")
	DATA("")
	DATA("A tabstop-list is an ordered list of column numbers, e.g., 1,11,21")
	DATA("or 1,+10,+10 which is the same.")
    };
#undef DATA

    fflush(stdout);
    fputs(msg, stderr);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int rc = EXIT_FAILURE;
    bool debug = FALSE;
    bool no_op = FALSE;
    bool change_tty = FALSE;
    int n, ch;
    NCURSES_CONST char *term_name = 0;
    char *append = 0;
    const char *tab_list = 0;
    const char *new_line = "\n";
    int margin = -1;
    TTY tty_settings;
    int fd;

    _nc_progname = _nc_rootname(argv[0]);

    if ((term_name = getenv("TERM")) == 0)
	term_name = "ansi+tabs";

    /* cannot use getopt, since some options are two-character */
    for (n = 1; n < argc; ++n) {
	char *option = argv[n];
	switch (option[0]) {
	case '-':
	    while ((ch = *++option) != '\0') {
		switch (ch) {
		case 'a':
		    switch (*++option) {
		    default:
		    case '\0':
			tab_list = "1,10,16,36,72";
			option--;
			/* Assembler, IBM S/370, first format */
			break;
		    case '2':
			tab_list = "1,10,16,40,72";
			/* Assembler, IBM S/370, second format */
			break;
		    }
		    break;
		case 'c':
		    switch (*++option) {
		    default:
		    case '\0':
			tab_list = "1,8,12,16,20,55";
			option--;
			/* COBOL, normal format */
			break;
		    case '2':
			tab_list = "1,6,10,14,49";
			/* COBOL compact format */
			break;
		    case '3':
			tab_list = "1,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,67";
			/* COBOL compact format extended */
			break;
		    }
		    break;
		case 'd':	/* ncurses extension */
		    debug = TRUE;
		    break;
		case 'f':
		    tab_list = "1,7,11,15,19,23";
		    /* FORTRAN */
		    break;
		case 'n':	/* ncurses extension */
		    no_op = TRUE;
		    break;
		case 'p':
		    tab_list = "1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61";
		    /* PL/I */
		    break;
		case 's':
		    tab_list = "1,10,55";
		    /* SNOBOL */
		    break;
		case 'u':
		    tab_list = "1,12,20,44";
		    /* UNIVAC 1100 Assembler */
		    break;
		case 'T':
		    ++n;
		    if (*++option != '\0') {
			term_name = option;
		    } else {
			term_name = argv[n];
			option--;
		    }
		    option += ((int) strlen(option)) - 1;
		    continue;
		case 'V':
		    puts(curses_version());
		    ExitProgram(EXIT_SUCCESS);
		default:
		    if (isdigit(UChar(*option))) {
			char *copy = strdup(option);
			*skip_list(copy) = '\0';
			tab_list = copy;
			option = skip_list(option) - 1;
		    } else {
			usage();
		    }
		    break;
		}
	    }
	    break;
	case '+':
	    if ((ch = *++option) != '\0') {
		int digits = 0;
		int number = 0;

		switch (ch) {
		case 'm':
		    /*
		     * The "+mXXX" option is unimplemented because only the long-obsolete
		     * att510d implements smgl, which is needed to support
		     * this option.
		     */
		    while ((ch = *++option) != '\0') {
			if (isdigit(UChar(ch))) {
			    ++digits;
			    number = number * 10 + (ch - '0');
			} else {
			    usage();
			}
		    }
		    if (digits == 0)
			number = 10;
		    margin = number;
		    break;
		default:
		    /* special case of relative stops separated by spaces? */
		    if (option == argv[n] + 1) {
			tab_list = add_to_tab_list(&append, argv[n]);
		    }
		    break;
		}
	    }
	    break;
	default:
	    if (append != 0) {
		if (tab_list != (const char *) append) {
		    /* one of the predefined options was used */
		    free(append);
		    append = 0;
		}
	    }
	    tab_list = add_to_tab_list(&append, option);
	    break;
	}
    }

    fd = save_tty_settings(&tty_settings, FALSE);

    setupterm(term_name, fd, (int *) 0);

    max_cols = (columns > 0) ? columns : 80;
    if (margin > 0)
	max_cols -= margin;

    if (!VALID_STRING(clear_all_tabs)) {
	fprintf(stderr,
		"%s: terminal type '%s' cannot reset tabs\n",
		_nc_progname, term_name);
    } else if (!VALID_STRING(set_tab)) {
	fprintf(stderr,
		"%s: terminal type '%s' cannot set tabs\n",
		_nc_progname, term_name);
    } else if (legal_tab_list(tab_list)) {
	int *list;

	if (tab_list == NULL)
	    tab_list = add_to_tab_list(&append, "8");

	if (!no_op) {
#if defined(TERMIOS) && defined(OCRNL)
	    /* set tty modes to -ocrnl to allow \r */
	    if (isatty(STDOUT_FILENO)) {
		TTY new_settings = tty_settings;
		new_settings.c_oflag &= (unsigned)~OCRNL;
		update_tty_settings(&tty_settings, &new_settings);
		change_tty = TRUE;
		new_line = "\r\n";
	    }
#endif

	    if (!ansi_clear_tabs())
		putch('\r');
	    tputs(clear_all_tabs, 1, putch);
	}

	if (margin >= 0) {
	    putch('\r');
	    if (margin > 0) {
		/* reset existing margin before setting margin, to reduce
		 * problems moving left of the current margin.
		 */
		if (do_set_margin(0, no_op))
		    putch('\r');
	    }
	    if (do_set_margin(margin, no_op))
		margin = -1;
	}

	list = decode_tabs(tab_list, margin);

	if (list != 0) {
	    if (!no_op)
		do_tabs(list);
	    if (debug) {
		fflush(stderr);
		printf("tabs %s%s", tab_list, new_line);
		print_ruler(list, new_line);
		write_tabs(list, new_line);
	    }
	    free(list);
	} else if (debug) {
	    fflush(stderr);
	    printf("tabs %s%s", tab_list, new_line);
	}
	if (!no_op) {
	    if (change_tty) {
		restore_tty_settings();
	    }
	}
	rc = EXIT_SUCCESS;
    }
    if (append != 0)
	free(append);
    ExitProgram(rc);
}
