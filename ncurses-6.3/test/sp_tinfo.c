/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
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
 * $Id: sp_tinfo.c,v 1.23 2020/02/02 23:34:34 tom Exp $
 *
 * TOTO: add option for non-sp-funcs interface
 */

#define USE_TINFO
#include <test.priv.h>

#if HAVE_TPUTS_SP
/*
 * The higher-level curses library stores a TERMINAL* inside SCREEN, but the
 * latter is opaque.  This structure helps us keep the two associated.
 */
typedef struct {
    const char *name;
    FILE *fp;
    SCREEN *sp;
    TERMINAL *term;
    int (*outc) (SCREEN *, int);
} MYDATA;

static bool opt_n = FALSE;	/* true to suppress new_prescr */
static bool opt_t = FALSE;	/* true to use termcap */

static int
my_outc(SCREEN *sp, int ch)
{
    (void) sp;
    return fputc(ch, stdout);
}

static int
my_errc(SCREEN *sp, int ch)
{
    (void) sp;
    return fputc(ch, stderr);
}

static MYDATA *
initialize(const char *name, FILE *output)
{
    MYDATA *result = typeCalloc(MYDATA, 1);
    int error;

    result->fp = output;
    result->name = name;
    result->outc = (fileno(output) == 1) ? my_outc : my_errc;
    result->sp = opt_n ? NULL : new_prescr();

    if (opt_t) {
	char *temp = strdup(name);
	tgetent_sp(result->sp, temp, name);
	free(temp);
    } else {
	setupterm((NCURSES_CONST char *) name, fileno(output), &error);
    }
    result->term = cur_term;

    return result;
}

static void
show_flag(MYDATA * data, const char *name, int value)
{
    if (value < 0) {
	fprintf(data->fp, " %s = (unknown)\n", name);
    } else if (value == 0) {
	fprintf(data->fp, " %s = false\n", name);
    } else {
	fprintf(data->fp, " %s = true\n", name);
    }
}

#define TC_PARMS data->sp, (NCURSES_CONST char *)tc
#define TI_PARMS data->sp, (NCURSES_CONST char *)ti

static void
show_cap_flag(MYDATA * data, const char *ti, const char *tc)
{
    const char *name = (opt_t ? tc : ti);
    show_flag(data, name, (opt_t
			   ? tgetflag_sp(TC_PARMS)
			   : tigetflag_sp(TI_PARMS)));
}

static void
show_number(MYDATA * data, const char *name, int value)
{
    if (value <= -2) {
	fprintf(data->fp, " %s = (unknown)\n", name);
    } else if (value <= -1) {
	fprintf(data->fp, " %s = (missing)\n", name);
    } else {
	fprintf(data->fp, " %s = %d\n", name, value);
    }
}

static void
show_cap_number(MYDATA * data, const char *ti, const char *tc)
{
    const char *name = (opt_t ? tc : ti);
    show_number(data, name, (opt_t
			     ? tgetnum_sp(TC_PARMS)
			     : tigetnum_sp(TI_PARMS)));
}

static void
show_string(MYDATA * data, const char *name, const char *value)
{
    fprintf(data->fp, " %s = ", name);
    if (value == 0) {
	fprintf(data->fp, "(missing)");
    } else if (value == (char *) -1) {
	fprintf(data->fp, "(canceled)");
    } else {
	int ch;
	while ((ch = UChar(*value++)) != '\0') {
	    if (ch < 32) {
		fprintf(data->fp, "^%c", ch | '@');
	    } else if (ch == 127) {
		fprintf(data->fp, "^?");
	    } else if (ch > 127) {
		fprintf(data->fp, "\\%03o", ch);
	    } else {
		fprintf(data->fp, "%c", ch);
	    }
	}
    }
    fprintf(data->fp, "\n");
}

static void
show_cap_string(MYDATA * data, const char *ti, const char *tc)
{
    const char *name = (opt_t ? tc : ti);
    char tcapjunk[1024];
    char *tcap_ptr = tcapjunk;
    show_string(data, name, (opt_t
			     ? tgetstr_sp(TC_PARMS, &tcap_ptr)
			     : tigetstr_sp(TI_PARMS)));
}

static void
show_char(MYDATA * data, const char *name, int value)
{
    if (value < 0) {
	show_string(data, name, "(missing)");
    } else {
	char temp[2];
	temp[0] = (char) value;
	temp[1] = '\0';
	show_string(data, name, temp);
    }
}

static void
do_stuff(MYDATA * data)
{
    SCREEN *sp = data->sp;
#if NCURSES_EXT_FUNCS
    char *s;
    int my_code = 1234;
    const char *my_text = "\033[?m";
#endif

    set_curterm_sp(sp, data->term);

    /* putp always goes to standard output */
    putp_sp(sp, "Hello ");
    putp_sp(sp, data->name);
    putp_sp(sp, "!\n");

    fprintf(data->fp, "Term: %s\n", termname_sp(sp));
    fprintf(data->fp, "Long: %s\n", longname_sp(sp));
    show_cap_flag(data, "am", "am");
    show_cap_number(data, "lines", "li");
    show_cap_string(data, "clear", "cl");
    show_cap_string(data, "tbc", "ct");
    show_flag(data, "has_ic", has_ic_sp(sp));
    show_flag(data, "has_il", has_il_sp(sp));
    show_number(data, "baudrate", baudrate_sp(sp));
    show_char(data, "erase ch", erasechar_sp(sp));
    show_char(data, "kill ch", killchar_sp(sp));
    show_string(data, "unctrl", unctrl_sp(sp, 033));
    fflush(data->fp);

#if NCURSES_EXT_FUNCS
    define_key_sp(sp, my_text, my_code);
    has_key_sp(sp, 0);
    key_defined_sp(sp, my_text);
    if ((s = keybound_sp(sp, my_code, 0)) != 0)
	free(s);
#endif
    keyname_sp(sp, '?');
#if NCURSES_EXT_FUNCS
    keyok_sp(sp, my_code, FALSE);
    keyok_sp(sp, my_code, TRUE);
#endif

    savetty_sp(sp);

    def_shell_mode_sp(sp);

    /*
     * These functions are low-level settings for ncurses.
     */
#if NCURSES_EXT_FUNCS
    set_tabsize_sp(sp, 5);	/* waddch */
#endif
    typeahead_sp(sp, FALSE);	/* waddch */
    use_env_sp(sp, FALSE);	/* newterm */
    use_tioctl_sp(sp, FALSE);	/* newterm */
    intrflush_sp(sp, 0, 0);	/* wgetch */
    flushinp_sp(sp);		/* waddch */
    halfdelay_sp(sp, 5);	/* wgetch */

    /*
     * These manipulate the terminal modes, mainly for wgetch.
     */
    cbreak_sp(sp);
    raw_sp(sp);
    def_prog_mode_sp(sp);

    delay_output_sp(sp, 200);

    napms_sp(sp, 10);

    nocbreak_sp(sp);
    noqiflush_sp(sp);
    noraw_sp(sp);
    qiflush_sp(sp);

    resetty_sp(sp);

    tputs_sp(sp, "{reset-mode}\n", 0, data->outc);

    reset_prog_mode_sp(sp);

    curs_set_sp(sp, 0);
    tputs_sp(sp, "{prog-mode}\n", 0, data->outc);

    reset_shell_mode_sp(sp);

    tputs_sp(sp, "{shell-mode}\n", 0, data->outc);
}

static void
cleanup(MYDATA * data)
{
    set_curterm(data->term);
    del_curterm(data->term);
#if !NO_LEAKS
    free(data->sp);		/* cannot use delscreen in tinfo */
#endif
    free(data);
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: sp_tinfo [output] [error]",
	"",
	"Options:",
	" -n   suppress call to new_prescr()",
	" -t   use termcap functions rather than terminfo",
	NULL
    };
    size_t n;
    for (n = 0; n < SIZEOF(tbl); ++n) {
	fprintf(stderr, "%s\n", tbl[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    MYDATA *my_out;
    MYDATA *my_err;
    int n;

    while ((n = getopt(argc, argv, "nt")) != -1) {
	switch (n) {
	case 'n':
	    opt_n = TRUE;
	    break;
	case 't':
	    opt_t = TRUE;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    argv += (optind - 1);
    argc -= (optind - 1);

    if (argc > 3)
	usage();

    my_out = initialize((argc > 1) ? argv[1] : "vt100", stdout);
    my_err = initialize((argc > 2) ? argv[2] : "ansi", stderr);

    do_stuff(my_out);
    do_stuff(my_err);

    cleanup(my_out);
    cleanup(my_err);

    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    fprintf(stderr,
	    "This program requires the low-level ncurses sp-funcs tputs_sp\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
