/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2017,2018 Free Software Foundation, Inc.                  *
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
 *  Author: Zeyd M. Ben-Halim <zmbenhal@netcom.com> 1992,1995               *
 *     and: Eric S. Raymond <esr@snark.thyrsus.com>                         *
 *     and: Thomas E. Dickey                        1996 on                 *
 ****************************************************************************/

/*
 *	tic.c --- Main program for terminfo compiler
 *			by Eric S. Raymond
 *			and Thomas E Dickey
 *
 */

#include <progs.priv.h>
#include <sys/stat.h>

#include <dump_entry.h>
#include <tparm_type.h>
#include <hashed_db.h>
#include <parametrized.h>
#include <transform.h>

MODULE_ID("$Id: tic.c,v 1.307 2021/10/05 08:07:05 tom Exp $")

#define STDIN_NAME "<stdin>"

const char *_nc_progname = "tic";

static FILE *log_fp;
static FILE *tmp_fp;
static bool capdump = FALSE;	/* running as infotocap? */
static bool infodump = FALSE;	/* running as captoinfo? */
static bool showsummary = FALSE;
static unsigned debug_level;
static char **namelst = 0;
static const char *to_remove;

#if NCURSES_XNAMES
static bool using_extensions = FALSE;
#endif

static void (*save_check_termtype) (TERMTYPE2 *, bool);
static void check_termtype(TERMTYPE2 *tt, bool);

static const char usage_string[] = "\
[-e names] \
[-o dir] \
[-R name] \
[-v[n]] \
[-V] \
[-w[n]] \
[-\
1\
a\
C\
D\
c\
f\
G\
g\
I\
K\
L\
N\
r\
s\
T\
t\
U\
x\
] \
source-file\n";

#if NO_LEAKS
static void
free_namelist(char **src)
{
    if (src != 0) {
	int n;
	for (n = 0; src[n] != 0; ++n)
	    free(src[n]);
	free(src);
    }
}
#endif

static void
cleanup(void)
{
#if NO_LEAKS
    free_namelist(namelst);
    _nc_leaks_dump_entry();
#endif
    if (tmp_fp != 0)
	fclose(tmp_fp);
    if (to_remove != 0) {
	int rc;

#if HAVE_REMOVE
	rc = remove(to_remove);
#else
	rc = unlink(to_remove);
#endif
	if (rc != 0)
	    perror(to_remove);
    }
}

static void
failed(const char *msg)
{
    perror(msg);
    ExitProgram(EXIT_FAILURE);
}

static void
usage(void)
{
#define DATA(s) s "\n"
    static const char options_string[] =
    {
	DATA("Options:")
	DATA("  -0         format translation output all capabilities on one line")
	DATA("  -1         format translation output one capability per line")
#if NCURSES_XNAMES
	DATA("  -a         retain commented-out capabilities (sets -x also)")
#endif
	DATA("  -C         translate entries to termcap source form")
	DATA("  -D         print list of tic's database locations (first must be writable)")
	DATA("  -c         check only, validate input without compiling or translating")
	DATA("  -e<names>  translate/compile only entries named by comma-separated list")
	DATA("  -f         format complex strings for readability")
	DATA("  -G         format %{number} to %'char'")
	DATA("  -g         format %'char' to %{number}")
	DATA("  -I         translate entries to terminfo source form")
	DATA("  -K         translate entries to termcap source form with BSD syntax")
	DATA("  -L         translate entries to full terminfo source form")
	DATA("  -N         disable smart defaults for source translation")
	DATA("  -o<dir>    set output directory for compiled entry writes")
	DATA("  -Q[n]      dump compiled description")
	DATA("  -q    brief listing, removes headers")
	DATA("  -R<name>   restrict translation to given terminfo/termcap version")
	DATA("  -r         force resolution of all use entries in source translation")
	DATA("  -s         print summary statistics")
	DATA("  -T         remove size-restrictions on compiled description")
#if NCURSES_XNAMES
	DATA("  -t         suppress commented-out capabilities")
#endif
	DATA("  -U         suppress post-processing of entries")
	DATA("  -V         print version")
	DATA("  -W         wrap long strings according to -w[n] option")
	DATA("  -v[n]      set verbosity level")
	DATA("  -w[n]      set format width for translation output")
#if NCURSES_XNAMES
	DATA("  -x         treat unknown capabilities as user-defined")
#endif
	DATA("")
	DATA("Parameters:")
	DATA("  <file>     file to translate or compile")
    };
#undef DATA

    fprintf(stderr, "Usage: %s %s\n", _nc_progname, usage_string);
    fputs(options_string, stderr);
    ExitProgram(EXIT_FAILURE);
}

#define L_BRACE '{'
#define R_BRACE '}'
#define S_QUOTE '\''

static void
write_it(ENTRY * ep)
{
    unsigned n;
    int ch;
    char *s, *d, *t;
    char result[MAX_ENTRY_SIZE];

    /*
     * Look for strings that contain %{number}, convert them to %'char',
     * which is shorter and runs a little faster.
     */
    for (n = 0; n < STRCOUNT; n++) {
	s = ep->tterm.Strings[n];
	if (VALID_STRING(s)
	    && strchr(s, L_BRACE) != 0) {
	    d = result;
	    t = s;
	    while ((ch = *t++) != 0) {
		*d++ = (char) ch;
		if (ch == '\\') {
		    if ((*d++ = *t++) == '\0')
			break;
		} else if ((ch == '%')
			   && (*t == L_BRACE)) {
		    char *v = 0;
		    long value = strtol(t + 1, &v, 0);
		    if (v != 0
			&& *v == R_BRACE
			&& value > 0
			&& value != '\\'	/* FIXME */
			&& value < 127
			&& isprint((int) value)) {
			*d++ = S_QUOTE;
			*d++ = (char) value;
			*d++ = S_QUOTE;
			t = (v + 1);
		    }
		}
	    }
	    *d = 0;
	    if (strlen(result) < strlen(s))
		_nc_STRCPY(s, result, strlen(s) + 1);
	}
    }

    _nc_set_type(_nc_first_name(ep->tterm.term_names));
    _nc_curr_line = (int) ep->startline;
    _nc_write_entry(&ep->tterm);
}

static bool
immedhook(ENTRY * ep GCC_UNUSED)
/* write out entries with no use capabilities immediately to save storage */
{
#if !HAVE_BIG_CORE
    /*
     * This is strictly a core-economy kluge.  The really clean way to handle
     * compilation is to slurp the whole file into core and then do all the
     * name-collision checks and entry writes in one swell foop.  But the
     * terminfo master file is large enough that some core-poor systems swap
     * like crazy when you compile it this way...there have been reports of
     * this process taking *three hours*, rather than the twenty seconds or
     * less typical on my development box.
     *
     * So.  This hook *immediately* writes out the referenced entry if it
     * has no use capabilities.  The compiler main loop refrains from
     * adding the entry to the in-core list when this hook fires.  If some
     * other entry later needs to reference an entry that got written
     * immediately, that's OK; the resolution code will fetch it off disk
     * when it can't find it in core.
     *
     * Name collisions will still be detected, just not as cleanly.  The
     * write_entry() code complains before overwriting an entry that
     * postdates the time of tic's first call to write_entry().  Thus
     * it will complain about overwriting entries newly made during the
     * tic run, but not about overwriting ones that predate it.
     *
     * The reason this is a hook, and not in line with the rest of the
     * compiler code, is that the support for termcap fallback cannot assume
     * it has anywhere to spool out these entries!
     *
     * The _nc_set_type() call here requires a compensating one in
     * _nc_parse_entry().
     *
     * If you define HAVE_BIG_CORE, you'll disable this kluge.  This will
     * make tic a bit faster (because the resolution code won't have to do
     * disk I/O nearly as often).
     */
    if (ep->nuses == 0) {
	int oldline = _nc_curr_line;

	write_it(ep);
	_nc_curr_line = oldline;
	free(ep->tterm.str_table);
	return (TRUE);
    }
#endif /* HAVE_BIG_CORE */
    return (FALSE);
}

static void
put_translate(int c)
/* emit a comment char, translating terminfo names to termcap names */
{
    static bool in_name = FALSE;
    static size_t used;

    if (in_name) {
	static size_t have;
	static char *namebuf, *suffix;

	if (used + 1 >= have) {
	    have += 132;
	    if ((namebuf = typeRealloc(char, have, namebuf)) == NULL)
		  failed("put_translate namebuf");
	    if ((suffix = typeRealloc(char, have, suffix)) == NULL)
		  failed("put_translate suffix");
	}
	if (c == '\n' || c == '@') {
	    namebuf[used++] = '\0';
	    (void) putchar('<');
	    (void) fputs(namebuf, stdout);
	    putchar(c);
	    in_name = FALSE;
	} else if (c != '>') {
	    namebuf[used++] = (char) c;
	} else {		/* ah! candidate name! */
	    char *up;
	    NCURSES_CONST char *tp;

	    namebuf[used++] = '\0';
	    in_name = FALSE;

	    suffix[0] = '\0';
	    if ((up = strchr(namebuf, '#')) != 0
		|| (up = strchr(namebuf, '=')) != 0
		|| ((up = strchr(namebuf, '@')) != 0 && up[1] == '>')) {
		_nc_STRCPY(suffix, up, have);
		*up = '\0';
	    }

	    if ((tp = nametrans(namebuf)) != 0) {
		(void) putchar(':');
		(void) fputs(tp, stdout);
		(void) fputs(suffix, stdout);
		(void) putchar(':');
	    } else {
		/* couldn't find a translation, just dump the name */
		(void) putchar('<');
		(void) fputs(namebuf, stdout);
		(void) fputs(suffix, stdout);
		(void) putchar('>');
	    }
	}
    } else {
	used = 0;
	if (c == '<') {
	    in_name = TRUE;
	} else {
	    putchar(c);
	}
    }
}

/* Returns a string, stripped of leading/trailing whitespace */
static char *
stripped(char *src)
{
    char *dst = 0;

    while (isspace(UChar(*src)))
	src++;

    if (*src != '\0') {
	if ((dst = strdup(src)) == NULL) {
	    failed("strdup");
	} else {
	    size_t len = strlen(dst);
	    while (--len != 0 && isspace(UChar(dst[len])))
		dst[len] = '\0';
	}
    }
    return dst;
}

static FILE *
open_tempfile(char *filename)
{
    FILE *result = 0;

    _nc_STRCPY(filename, "/tmp/XXXXXX", PATH_MAX);
#if HAVE_MKSTEMP
    {
	int oldmask = (int) umask(077);
	int fd = mkstemp(filename);
	if (fd >= 0)
	    result = fdopen(fd, "w");
	umask((mode_t) oldmask);
    }
#else
    if (tmpnam(filename) != 0)
	result = safe_fopen(filename, "w");
#endif
    return result;
}

static FILE *
copy_input(FILE *source, const char *filename, char *alt_file)
{
    char my_altfile[PATH_MAX];
    FILE *result = 0;
    FILE *target;
    int ch;

    if (alt_file == NULL)
	alt_file = my_altfile;

    if (source == NULL) {
	failed("copy_input (source)");
    } else if ((target = open_tempfile(alt_file)) == NULL) {
	failed("copy_input (target)");
    } else {
	clearerr(source);
	for (;;) {
	    ch = fgetc(source);
	    if (feof(source)) {
		break;
	    } else if (ferror(source)) {
		failed(filename);
	    } else if (ch == 0) {
		/* don't loop in case someone wants to convert /dev/zero */
		fprintf(stderr, "%s: %s is not a text-file\n", _nc_progname, filename);
		ExitProgram(EXIT_FAILURE);
	    }
	    fputc(ch, target);
	}
	fclose(source);
	/*
	 * rewind() does not force the target file's data to disk (not does
	 * fflush()...).  So open a second stream on the data and then close
	 * the one that we were writing on before starting to read from the
	 * second stream.
	 */
	result = safe_fopen(alt_file, "r+");
	fclose(target);
	to_remove = strdup(alt_file);
    }
    return result;
}

static FILE *
open_input(const char *filename, char *alt_file)
{
    FILE *fp;
    struct stat sb;
    int mode;

    if (!strcmp(filename, "-")) {
	fp = copy_input(stdin, STDIN_NAME, alt_file);
    } else if (stat(filename, &sb) == -1) {
	fprintf(stderr, "%s: %s %s\n", _nc_progname, filename, strerror(errno));
	ExitProgram(EXIT_FAILURE);
    } else if ((mode = (sb.st_mode & S_IFMT)) == S_IFDIR
	       || (mode != S_IFREG && mode != S_IFCHR && mode != S_IFIFO)) {
	fprintf(stderr, "%s: %s is not a file\n", _nc_progname, filename);
	ExitProgram(EXIT_FAILURE);
    } else {
	fp = safe_fopen(filename, "r");

	if (fp == NULL) {
	    fprintf(stderr, "%s: Can't open %s\n", _nc_progname, filename);
	    ExitProgram(EXIT_FAILURE);
	}
	if (mode != S_IFREG) {
	    if (alt_file != 0) {
		FILE *fp2 = copy_input(fp, filename, alt_file);
		fp = fp2;
	    } else {
		fprintf(stderr, "%s: %s is not a file\n", _nc_progname, filename);
		ExitProgram(EXIT_FAILURE);
	    }
	}
    }
    return fp;
}

/* Parse the "-e" option-value into a list of names */
static char **
make_namelist(char *src)
{
    char **dst = 0;

    char *s, *base;
    unsigned pass, n, nn;
    char buffer[BUFSIZ];

    if (src == NULL) {
	/* EMPTY */ ;
    } else if (strchr(src, '/') != 0) {		/* a filename */
	FILE *fp = open_input(src, (char *) 0);

	for (pass = 1; pass <= 2; pass++) {
	    nn = 0;
	    while (fgets(buffer, sizeof(buffer), fp) != 0) {
		if ((s = stripped(buffer)) != 0) {
		    if (dst != 0)
			dst[nn] = s;
		    else
			free(s);
		    nn++;
		}
	    }
	    if (pass == 1) {
		if ((dst = typeCalloc(char *, nn + 1)) == NULL)
		      failed("make_namelist");
		rewind(fp);
	    }
	}
	fclose(fp);
    } else {			/* literal list of names */
	for (pass = 1; pass <= 2; pass++) {
	    for (n = nn = 0, base = src;; n++) {
		int mark = src[n];
		if (mark == ',' || mark == '\0') {
		    if (pass == 1) {
			nn++;
		    } else {
			src[n] = '\0';
			if ((s = stripped(base)) != 0)
			    dst[nn++] = s;
			base = &src[n + 1];
		    }
		}
		if (mark == '\0')
		    break;
	    }
	    if (pass == 1) {
		if ((dst = typeCalloc(char *, nn + 1)) == NULL)
		      failed("make_namelist");
	    }
	}
    }
    if (showsummary && (dst != 0)) {
	fprintf(log_fp, "Entries that will be compiled:\n");
	for (n = 0; dst[n] != 0; n++)
	    fprintf(log_fp, "%u:%s\n", n + 1, dst[n]);
    }
    return dst;
}

static bool
matches(char **needle, const char *haystack)
/* does entry in needle list match |-separated field in haystack? */
{
    bool code = FALSE;

    if (needle != 0) {
	size_t n;

	for (n = 0; needle[n] != 0; n++) {
	    if (_nc_name_match(haystack, needle[n], "|")) {
		code = TRUE;
		break;
	    }
	}
    } else
	code = TRUE;
    return (code);
}

static char *
valid_db_path(const char *nominal)
{
    struct stat sb;
#if USE_HASHED_DB
    char suffix[] = DBM_SUFFIX;
    size_t need = strlen(nominal) + sizeof(suffix);
    char *result = malloc(need);

    if (result == NULL)
	failed("valid_db_path");
    _nc_STRCPY(result, nominal, need);
    if (strcmp(result + need - sizeof(suffix), suffix)) {
	_nc_STRCAT(result, suffix, need);
    }
#else
    char *result = strdup(nominal);
#endif

    DEBUG(1, ("** stat(%s)", result));
    if (stat(result, &sb) >= 0) {
#if USE_HASHED_DB
	if (!S_ISREG(sb.st_mode)
	    || access(result, R_OK | W_OK) != 0) {
	    DEBUG(1, ("...not a writable file"));
	    free(result);
	    result = 0;
	}
#else
	if (!S_ISDIR(sb.st_mode)
	    || access(result, R_OK | W_OK | X_OK) != 0) {
	    DEBUG(1, ("...not a writable directory"));
	    free(result);
	    result = 0;
	}
#endif
    } else {
	/* check if parent is directory and is writable */
	unsigned leaf = _nc_pathlast(result);

	DEBUG(1, ("...not found"));
	if (leaf) {
	    char save = result[leaf];
	    result[leaf] = 0;
	    if (stat(result, &sb) >= 0
		&& S_ISDIR(sb.st_mode)
		&& access(result, R_OK | W_OK | X_OK) == 0) {
		result[leaf] = save;
	    } else {
		DEBUG(1, ("...parent directory %s is not writable", result));
		free(result);
		result = 0;
	    }
	} else {
	    DEBUG(1, ("... no parent directory"));
	    free(result);
	    result = 0;
	}
    }
    return result;
}

/*
 * Show the databases to which tic could write.  The location to which it
 * writes is always the first one.  If none are writable, print an error
 * message.
 */
static void
show_databases(const char *outdir)
{
    bool specific = (outdir != 0) || getenv("TERMINFO") != 0;
    char *result;
    const char *tried = 0;

    if (outdir == NULL) {
	outdir = _nc_tic_dir(0);
    }
    if ((result = valid_db_path(outdir)) != 0) {
	printf("%s\n", result);
	free(result);
    } else {
	tried = outdir;
    }

    if ((outdir = _nc_home_terminfo())) {
	if ((result = valid_db_path(outdir)) != 0) {
	    printf("%s\n", result);
	    free(result);
	} else if (!specific) {
	    tried = outdir;
	}
    }

    /*
     * If we can write in neither location, give an error message.
     */
    if (tried) {
	fflush(stdout);
	fprintf(stderr, "%s: %s (no permission)\n", _nc_progname, tried);
	ExitProgram(EXIT_FAILURE);
    }
}

static void
add_digit(int *target, int source)
{
    *target = (*target * 10) + (source - '0');
}

int
main(int argc, char *argv[])
{
    char my_tmpname[PATH_MAX];
    char my_altfile[PATH_MAX];
    int v_opt = -1;
    int smart_defaults = TRUE;
    char *termcap;
    ENTRY *qp;

    int this_opt, last_opt = '?';

    int outform = F_TERMINFO;	/* output format */
    int sortmode = S_TERMINFO;	/* sort_mode */

    int width = 60;
    int height = 65535;
    bool formatted = FALSE;	/* reformat complex strings? */
    bool literal = FALSE;	/* suppress post-processing? */
    int numbers = 0;		/* format "%'char'" to/from "%{number}" */
    bool forceresolve = FALSE;	/* force resolution */
    bool limited = TRUE;
    char *tversion = (char *) NULL;
    const char *source_file = "terminfo";
    char *outdir = (char *) NULL;
    bool check_only = FALSE;
    bool suppress_untranslatable = FALSE;
    int quickdump = 0;
    bool quiet = FALSE;
    bool wrap_strings = FALSE;

    log_fp = stderr;

    _nc_progname = _nc_rootname(argv[0]);
    atexit(cleanup);

    if ((infodump = same_program(_nc_progname, PROG_CAPTOINFO)) != FALSE) {
	outform = F_TERMINFO;
	sortmode = S_TERMINFO;
    }
    if ((capdump = same_program(_nc_progname, PROG_INFOTOCAP)) != FALSE) {
	outform = F_TERMCAP;
	sortmode = S_TERMCAP;
    }
#if NCURSES_XNAMES
    use_extended_names(FALSE);
#endif
    _nc_strict_bsd = 0;

    /*
     * Processing arguments is a little complicated, since someone made a
     * design decision to allow the numeric values for -w, -v options to
     * be optional.
     */
    while ((this_opt = getopt(argc, argv,
			      "0123456789CDIKLNQR:TUVWace:fGgo:qrstvwx")) != -1) {
	if (isdigit(this_opt)) {
	    switch (last_opt) {
	    case 'Q':
		add_digit(&quickdump, this_opt);
		break;
	    case 'v':
		add_digit(&v_opt, this_opt);
		break;
	    case 'w':
		add_digit(&width, this_opt);
		break;
	    default:
		switch (this_opt) {
		case '0':
		    last_opt = this_opt;
		    width = 65535;
		    height = 1;
		    break;
		case '1':
		    last_opt = this_opt;
		    width = 0;
		    break;
		default:
		    usage();
		}
	    }
	    continue;
	}
	switch (this_opt) {
	case 'K':
	    _nc_strict_bsd = 1;
	    /* the initial version of -K in 20110730 fell-thru here, but the
	     * same flag is useful when reading sources -TD
	     */
	    break;
	case 'C':
	    capdump = TRUE;
	    outform = F_TERMCAP;
	    sortmode = S_TERMCAP;
	    break;
	case 'D':
	    debug_level = VtoTrace(v_opt);
	    set_trace_level(debug_level);
	    show_databases(outdir);
	    ExitProgram(EXIT_SUCCESS);
	    break;
	case 'I':
	    infodump = TRUE;
	    outform = F_TERMINFO;
	    sortmode = S_TERMINFO;
	    break;
	case 'L':
	    infodump = TRUE;
	    outform = F_VARIABLE;
	    sortmode = S_VARIABLE;
	    break;
	case 'N':
	    smart_defaults = FALSE;
	    literal = TRUE;
	    break;
	case 'Q':
	    quickdump = 0;
	    break;
	case 'R':
	    tversion = optarg;
	    break;
	case 'T':
	    limited = FALSE;
	    break;
	case 'U':
	    literal = TRUE;
	    break;
	case 'V':
	    puts(curses_version());
	    ExitProgram(EXIT_SUCCESS);
	case 'W':
	    wrap_strings = TRUE;
	    break;
	case 'c':
	    check_only = TRUE;
	    break;
	case 'e':
	    namelst = make_namelist(optarg);
	    break;
	case 'f':
	    formatted = TRUE;
	    break;
	case 'G':
	    numbers = 1;
	    break;
	case 'g':
	    numbers = -1;
	    break;
	case 'o':
	    outdir = optarg;
	    break;
	case 'q':
	    quiet = TRUE;
	    break;
	case 'r':
	    forceresolve = TRUE;
	    break;
	case 's':
	    showsummary = TRUE;
	    break;
	case 'v':
	    v_opt = 0;
	    break;
	case 'w':
	    width = 0;
	    break;
#if NCURSES_XNAMES
	case 't':
	    _nc_disable_period = FALSE;
	    suppress_untranslatable = TRUE;
	    break;
	case 'a':
	    _nc_disable_period = TRUE;
	    /* FALLTHRU */
	case 'x':
	    use_extended_names(TRUE);
	    using_extensions = TRUE;
	    break;
#endif
	default:
	    usage();
	}
	last_opt = this_opt;
    }

    debug_level = VtoTrace(v_opt);
    set_trace_level(debug_level);

    if (_nc_tracing) {
	save_check_termtype = _nc_check_termtype2;
	_nc_check_termtype2 = check_termtype;
    }
#if !HAVE_BIG_CORE
    /*
     * Aaargh! immedhook seriously hoses us!
     *
     * One problem with immedhook is it means we can't do -e.  Problem
     * is that we can't guarantee that for each terminal listed, all the
     * terminals it depends on will have been kept in core for reference
     * resolution -- in fact it is certain the primitive types at the end
     * of reference chains *won't* be in core unless they were explicitly
     * in the select list themselves.
     */
    if (namelst && (!infodump && !capdump)) {
	(void) fprintf(stderr,
		       "%s: Sorry, -e can't be used without -I or -C\n",
		       _nc_progname);
	ExitProgram(EXIT_FAILURE);
    }
#endif /* HAVE_BIG_CORE */

    if (optind < argc) {
	source_file = argv[optind++];
	if (optind < argc) {
	    fprintf(stderr,
		    "%s: Too many file names.  Usage:\n\t%s %s",
		    _nc_progname,
		    _nc_progname,
		    usage_string);
	    ExitProgram(EXIT_FAILURE);
	}
    } else {
	if (infodump == TRUE) {
	    /* captoinfo's no-argument case */
	    source_file = "/etc/termcap";
	    if ((termcap = getenv("TERMCAP")) != 0
		&& (namelst = make_namelist(getenv("TERM"))) != 0) {
		if (access(termcap, F_OK) == 0) {
		    /* file exists */
		    source_file = termcap;
		} else {
		    if ((tmp_fp = open_tempfile(my_tmpname)) != 0) {
			source_file = my_tmpname;
			fprintf(tmp_fp, "%s\n", termcap);
			fclose(tmp_fp);
			tmp_fp = open_input(source_file, (char *) 0);
			to_remove = source_file;
		    } else {
			failed("tmpnam");
		    }
		}
	    }
	} else {
	    /* tic */
	    fprintf(stderr,
		    "%s: File name needed.  Usage:\n\t%s %s",
		    _nc_progname,
		    _nc_progname,
		    usage_string);
	    ExitProgram(EXIT_FAILURE);
	}
    }

    if (tmp_fp == NULL) {
	tmp_fp = open_input(source_file, my_altfile);
	if (!strcmp(source_file, "-")) {
	    source_file = STDIN_NAME;
	}
    }

    if (infodump || check_only) {
	dump_init(tversion,
		  (smart_defaults
		   ? outform
		   : F_LITERAL),
		  sortmode,
		  wrap_strings, width, height,
		  debug_level, formatted || check_only, check_only, quickdump);
    } else if (capdump) {
	dump_init(tversion,
		  outform,
		  sortmode,
		  wrap_strings, width, height,
		  debug_level, FALSE, FALSE, FALSE);
    }

    /* parse entries out of the source file */
    _nc_set_source(source_file);
#if !HAVE_BIG_CORE
    if (!(check_only || infodump || capdump))
	_nc_set_writedir(outdir);
#endif /* HAVE_BIG_CORE */
    _nc_read_entry_source(tmp_fp, (char *) NULL,
			  !smart_defaults || literal, FALSE,
			  ((check_only || infodump || capdump)
			   ? NULLHOOK
			   : immedhook));

    /* do use resolution */
    if (check_only || (!infodump && !capdump) || forceresolve) {
	if (!_nc_resolve_uses2(TRUE, literal) && !check_only) {
	    ExitProgram(EXIT_FAILURE);
	}
    }

    /* length check */
    if (check_only && limited && (capdump || infodump)) {
	for_entry_list(qp) {
	    if (matches(namelst, qp->tterm.term_names)) {
		int len = fmt_entry(&qp->tterm, NULL, FALSE, TRUE, infodump, numbers);

		if (len > (infodump ? MAX_TERMINFO_LENGTH : MAX_TERMCAP_LENGTH))
		    (void) fprintf(stderr,
				   "%s: resolved %s entry is %d bytes long\n",
				   _nc_progname,
				   _nc_first_name(qp->tterm.term_names),
				   len);
	    }
	}
    }

    /* write or dump all entries */
    if (check_only) {
	/* this is in case infotocap() generates warnings */
	_nc_curr_col = _nc_curr_line = -1;

	for_entry_list(qp) {
	    if (matches(namelst, qp->tterm.term_names)) {
		/* this is in case infotocap() generates warnings */
		_nc_set_type(_nc_first_name(qp->tterm.term_names));
		_nc_curr_line = (int) qp->startline;
		repair_acsc(&qp->tterm);
		dump_entry(&qp->tterm, suppress_untranslatable,
			   limited, numbers, NULL);
	    }
	}
    } else {
	if (!infodump && !capdump) {
	    _nc_set_writedir(outdir);
	    for_entry_list(qp) {
		if (matches(namelst, qp->tterm.term_names))
		    write_it(qp);
	    }
	} else {
	    /* this is in case infotocap() generates warnings */
	    _nc_curr_col = _nc_curr_line = -1;

	    for_entry_list(qp) {
		if (matches(namelst, qp->tterm.term_names)) {
		    long j = qp->cend - qp->cstart;
		    int len = 0;

		    /* this is in case infotocap() generates warnings */
		    _nc_set_type(_nc_first_name(qp->tterm.term_names));

		    if (!quiet) {
			(void) fseek(tmp_fp, qp->cstart, SEEK_SET);
			while (j-- > 0) {
			    int ch = fgetc(tmp_fp);
			    if (ch == EOF || ferror(tmp_fp)) {
				break;
			    } else if (infodump) {
				(void) putchar(ch);
			    } else {
				put_translate(ch);
			    }
			}
		    }

		    repair_acsc(&qp->tterm);
		    dump_entry(&qp->tterm, suppress_untranslatable,
			       limited, numbers, NULL);
		    for (j = 0; j < (long) qp->nuses; j++)
			dump_uses(qp->uses[j].name, !capdump);
		    len = show_entry();
		    if (debug_level != 0 && !limited)
			printf("# length=%d\n", len);
		}
	    }
	    if (!namelst && _nc_tail && !quiet) {
		int c, oldc = '\0';
		bool in_comment = FALSE;
		bool trailing_comment = FALSE;

		(void) fseek(tmp_fp, _nc_tail->cend, SEEK_SET);
		while ((c = fgetc(tmp_fp)) != EOF) {
		    if (oldc == '\n') {
			if (c == '#') {
			    trailing_comment = TRUE;
			    in_comment = TRUE;
			} else {
			    in_comment = FALSE;
			}
		    }
		    if (trailing_comment
			&& (in_comment || (oldc == '\n' && c == '\n')))
			putchar(c);
		    oldc = c;
		}
	    }
	}
    }

    /* Show the directory into which entries were written, and the total
     * number of entries
     */
    if (showsummary
	&& (!(check_only || infodump || capdump))) {
	int total = _nc_tic_written();
	if (total != 0)
	    fprintf(log_fp, "%d entries written to %s\n",
		    total,
		    _nc_tic_dir((char *) 0));
	else
	    fprintf(log_fp, "No entries written\n");
    }
    ExitProgram(EXIT_SUCCESS);
}

/*
 * This bit of legerdemain turns all the terminfo variable names into
 * references to locations in the arrays Booleans, Numbers, and Strings ---
 * precisely what's needed (see comp_parse.c).
 */
#undef CUR
#define CUR tp->

/*
 * Check if the alternate character-set capabilities are consistent.
 */
static void
check_acs(TERMTYPE2 *tp)
{
    int vt100_smacs = 0;
    int vt100_rmacs = 0;
    int vt100_enacs = 0;

    /*
     * ena_acs is not always necessary, but if it is present, the enter/exit
     * capabilities should be.
     */
    ANDMISSING(ena_acs, enter_alt_charset_mode);
    ANDMISSING(ena_acs, exit_alt_charset_mode);
    PAIRED(exit_alt_charset_mode, exit_alt_charset_mode);

    /*
     * vt100-like is frequently used, but perhaps ena_acs is missing, etc.
     */
    if (VALID_STRING(enter_alt_charset_mode)) {
	vt100_smacs = (!strcmp("\033(0", enter_alt_charset_mode)
		       ? 2
		       : (!strcmp("\016", enter_alt_charset_mode)
			  ? 1
			  : 0));
    }
    if (VALID_STRING(exit_alt_charset_mode)) {
	vt100_rmacs = (!strcmp("\033(B", exit_alt_charset_mode)
		       ? 2
		       : (!strcmp("\017", exit_alt_charset_mode)
			  ? 1
			  : 0));
    }
    if (VALID_STRING(ena_acs)) {
	vt100_enacs = (!strcmp("\033(B\033)0", ena_acs)
		       ? 2
		       : 0);
    }
    if (vt100_rmacs && vt100_smacs && (vt100_rmacs != vt100_smacs)) {
	_nc_warning("rmacs/smacs are inconsistent");
    }
    if ((vt100_rmacs == 2) && (vt100_smacs == 2) && vt100_enacs) {
	_nc_warning("rmacs/smacs make enacs redundant");
    }
    if ((vt100_rmacs == 1) && (vt100_smacs == 1) && !vt100_enacs) {
	_nc_warning("VT100-style rmacs/smacs require enacs");
    }

    if (VALID_STRING(acs_chars)) {
	const char *boxes = "lmkjtuvwqxn";
	char mapped[256];
	char missing[256];
	const char *p;
	char *q;

	memset(mapped, 0, sizeof(mapped));
	for (p = acs_chars; *p != '\0'; p += 2) {
	    if (p[1] == '\0') {
		_nc_warning("acsc has odd number of characters");
		break;
	    }
	    mapped[UChar(p[0])] = p[1];
	}

	if (mapped[UChar('I')] && !mapped[UChar('i')]) {
	    _nc_warning("acsc refers to 'I', which is probably an error");
	}

	for (p = boxes, q = missing; *p != '\0'; ++p) {
	    if (!mapped[UChar(p[0])]) {
		*q++ = p[0];
	    }
	}
	*q = '\0';

	assert(strlen(missing) <= strlen(boxes));
	if (*missing != '\0' && strcmp(missing, boxes)) {
	    _nc_warning("acsc is missing some line-drawing mapping: %s", missing);
	}
    }
}

static char *
safe_strdup(const char *value)
{
    if (value == NULL)
	value = "";
    return strdup(value);
}

static bool
same_color(NCURSES_CONST char *oldcap, NCURSES_CONST char *newcap, int limit)
{
    bool result = FALSE;
    if (limit > 16)
	limit = 16;
    if (limit >= 8) {
	int n;
	int same;
	for (n = same = 0; n < limit; ++n) {
	    char *oldvalue = safe_strdup(TIPARM_1(oldcap, n));
	    char *newvalue = safe_strdup(TIPARM_1(newcap, n));
	    same += !strcmp(oldvalue, newvalue);
	    free(oldvalue);
	    free(newvalue);
	}
	result = (same == limit);
    }
    return result;
}

/*
 * Check if the color capabilities are consistent
 */
static void
check_colors(TERMTYPE2 *tp)
{
    char *value;

    if ((max_colors > 0) != (max_pairs > 0)
	|| ((max_colors > max_pairs) && !VALID_STRING(initialize_pair)))
	_nc_warning("inconsistent values for max_colors (%d) and max_pairs (%d)",
		    max_colors, max_pairs);

    PAIRED(set_foreground, set_background);
    PAIRED(set_a_foreground, set_a_background);
    PAIRED(set_color_pair, initialize_pair);

    if (VALID_STRING(set_foreground)
	&& VALID_STRING(set_a_foreground)) {
	if (!_nc_capcmp(set_foreground, set_a_foreground)) {
	    _nc_warning("expected setf/setaf to be different");
	} else if (same_color(set_foreground, set_a_foreground, max_colors)) {
	    _nc_warning("setf/setaf are equivalent");
	}
    }

    if (VALID_STRING(set_background)
	&& VALID_STRING(set_a_background)) {
	if (!_nc_capcmp(set_background, set_a_background)) {
	    _nc_warning("expected setb/setab to be different");
	} else if (same_color(set_background, set_a_background, max_colors)) {
	    _nc_warning("setb/setab are equivalent");
	}
    }

    /* see: has_colors() */
    if (VALID_NUMERIC(max_colors) && VALID_NUMERIC(max_pairs)
	&& ((VALID_STRING(set_foreground)
	     && VALID_STRING(set_background))
	    || (VALID_STRING(set_a_foreground)
		&& VALID_STRING(set_a_background))
	    || set_color_pair)) {
	if (!VALID_STRING(orig_pair) && !VALID_STRING(orig_colors))
	    _nc_warning("expected either op/oc string for resetting colors");
    }
    if (can_change) {
	if (!VALID_STRING(initialize_pair) &&
	    !VALID_STRING(initialize_color)) {
	    _nc_warning("expected initc or initp because ccc is given");
	}
    } else {
	if (VALID_STRING(initialize_pair) ||
	    VALID_STRING(initialize_color)) {
	    _nc_warning("expected ccc because initc is given");
	}
    }
    value = tigetstr("RGB");
    if (VALID_STRING(value)) {
	int r, g, b;
	char bad;
	int code = sscanf(value, "%d/%d/%d%c", &r, &g, &b, &bad);
	if (code != 3 || r <= 0 || g <= 0 || b <= 0) {
	    _nc_warning("unexpected value for RGB capability: %s", value);
	}
    }
}

static int
csi_length(const char *value)
{
    int result = 0;

    if (value[0] == '\033' && value[1] == '[') {
	result = 2;
    } else if (UChar(value[0]) == 0x9a) {
	result = 1;
    }
    return result;
}

static char
keypad_final(const char *string)
{
    char result = '\0';

    if (VALID_STRING(string)
	&& *string++ == '\033'
	&& *string++ == 'O'
	&& strlen(string) == 1) {
	result = *string;
    }

    return result;
}

static long
keypad_index(const char *string)
{
    int ch;
    long result = -1;

    if ((ch = keypad_final(string)) != '\0') {
	const char *list = "PQRSwxymtuvlqrsPpn";	/* app-keypad except "Enter" */
	char *test = (strchr) (list, ch);
	if (test != 0)
	    result = (long) (test - list);
    }
    return result;
}

/*
 * list[] is down, up, left, right
 * "left" may be ^H rather than \E[D
 * "down" may be ^J rather than \E[B
 * But up/right are generally consistently escape sequences for ANSI terminals.
 */
static void
check_ansi_cursor(char *list[4])
{
    int j, k;
    bool skip[4];
    bool repeated = FALSE;

    for (j = 0; j < 4; ++j) {
	skip[j] = FALSE;
	for (k = 0; k < j; ++k) {
	    if (j != k
		&& !strcmp(list[j], list[k])) {
		char *value = _nc_tic_expand(list[k], TRUE, 0);
		_nc_warning("repeated cursor control %s", value);
		repeated = TRUE;
	    }
	}
    }
    if (!repeated) {
	char *up = list[1];
	size_t prefix = (size_t) csi_length(up);
	size_t suffix;

	if (prefix) {
	    suffix = prefix;
	    while (up[suffix] && isdigit(UChar(up[suffix])))
		++suffix;
	}
	if (prefix && up[suffix] == 'A') {
	    skip[1] = TRUE;
	    if (!strcmp(list[0], "\n"))
		skip[0] = TRUE;
	    if (!strcmp(list[2], "\b"))
		skip[2] = TRUE;

	    for (j = 0; j < 4; ++j) {
		int want;

		if (skip[j] || strlen(list[j]) == 1)
		    continue;
		if (memcmp(list[j], up, prefix)) {
		    char *value = _nc_tic_expand(list[j], TRUE, 0);
		    _nc_warning("inconsistent prefix for %s", value);
		    continue;
		}
		if (strlen(list[j]) < suffix) {
		    char *value = _nc_tic_expand(list[j], TRUE, 0);
		    _nc_warning("inconsistent length for %s, expected %d",
				value, (int) suffix + 1);
		    continue;
		}
		want = "BADC"[j];
		if (list[j][suffix] != want) {
		    char *value = _nc_tic_expand(list[j], TRUE, 0);
		    _nc_warning("inconsistent suffix for %s, expected %c, have %c",
				value, want, list[j][suffix]);
		}
	    }
	}
    }
}

#define EXPECTED(name) if (!PRESENT(name)) _nc_warning("expected " #name)
#define UNEXPECTED(name) if (PRESENT(name)) _nc_warning("unexpected " #name ", for %s", why)

static void
check_noaddress(TERMTYPE2 *tp, const char *why)
{
    UNEXPECTED(column_address);
    UNEXPECTED(cursor_address);
    UNEXPECTED(cursor_home);
    UNEXPECTED(cursor_mem_address);
    UNEXPECTED(cursor_to_ll);
    UNEXPECTED(row_address);
    UNEXPECTED(row_address);
}

static void
check_cursor(TERMTYPE2 *tp)
{
    int count;
    char *list[4];

    if (hard_copy) {
	check_noaddress(tp, "hard_copy");
    } else if (generic_type) {
	check_noaddress(tp, "generic_type");
    } else if (strchr(tp->term_names, '+') == NULL) {
	int y = 0;
	int x = 0;
	if (PRESENT(column_address))
	    ++y;
	if (PRESENT(cursor_address))
	    y = x = 10;
	if (PRESENT(cursor_home))
	    ++y, ++x;
	if (PRESENT(cursor_mem_address))
	    y = x = 10;
	if (PRESENT(cursor_to_ll))
	    ++y, ++x;
	if (PRESENT(row_address))
	    ++x;
	if (PRESENT(cursor_down))
	    ++y;
	if (PRESENT(cursor_up))
	    ++y;
	if (PRESENT(cursor_left))
	    ++x;
	if (PRESENT(cursor_right))
	    ++x;
	if (x < 2 && y < 2) {
	    _nc_warning("terminal lacks cursor addressing");
	} else {
	    if (x < 2)
		_nc_warning("terminal lacks cursor column-addressing");
	    if (y < 2)
		_nc_warning("terminal lacks cursor row-addressing");
	}
    }

    /* it is rare to have an insert-line feature without a matching delete */
    ANDMISSING(parm_insert_line, insert_line);
    ANDMISSING(parm_delete_line, delete_line);
    ANDMISSING(parm_insert_line, parm_delete_line);

    /* if we have a parameterized form, then the non-parameterized is easy */
    ANDMISSING(parm_down_cursor, cursor_down);
    ANDMISSING(parm_up_cursor, cursor_up);
    ANDMISSING(parm_left_cursor, cursor_left);
    ANDMISSING(parm_right_cursor, cursor_right);

    /* Given any of a set of cursor movement, the whole set should be present.
     * Technically this is not true (we could use cursor_address to fill in
     * unsupported controls), but it is likely.
     */
    count = 0;
    if (PRESENT(parm_down_cursor)) {
	list[count++] = parm_down_cursor;
    }
    if (PRESENT(parm_up_cursor)) {
	list[count++] = parm_up_cursor;
    }
    if (PRESENT(parm_left_cursor)) {
	list[count++] = parm_left_cursor;
    }
    if (PRESENT(parm_right_cursor)) {
	list[count++] = parm_right_cursor;
    }
    if (count == 4) {
	check_ansi_cursor(list);
    } else if (count != 0) {
	EXPECTED(parm_down_cursor);
	EXPECTED(parm_up_cursor);
	EXPECTED(parm_left_cursor);
	EXPECTED(parm_right_cursor);
    }

    count = 0;
    if (PRESENT(cursor_down)) {
	list[count++] = cursor_down;
    }
    if (PRESENT(cursor_up)) {
	list[count++] = cursor_up;
    }
    if (PRESENT(cursor_left)) {
	list[count++] = cursor_left;
    }
    if (PRESENT(cursor_right)) {
	list[count++] = cursor_right;
    }
    if (count == 4) {
	check_ansi_cursor(list);
    } else if (count != 0) {
	count = 0;
	if (PRESENT(cursor_down) && strcmp(cursor_down, "\n"))
	    ++count;
	if (PRESENT(cursor_left) && strcmp(cursor_left, "\b"))
	    ++count;
	if (PRESENT(cursor_up) && strlen(cursor_up) > 1)
	    ++count;
	if (PRESENT(cursor_right) && strlen(cursor_right) > 1)
	    ++count;
	if (count) {
	    EXPECTED(cursor_down);
	    EXPECTED(cursor_up);
	    EXPECTED(cursor_left);
	    EXPECTED(cursor_right);
	}
    }
}

#define MAX_KP 5
/*
 * Do a quick sanity-check for vt100-style keypads to see if the 5-key keypad
 * is mapped inconsistently.
 */
static void
check_keypad(TERMTYPE2 *tp)
{
    char show[80];

    if (VALID_STRING(key_a1) &&
	VALID_STRING(key_a3) &&
	VALID_STRING(key_b2) &&
	VALID_STRING(key_c1) &&
	VALID_STRING(key_c3)) {
	char final[MAX_KP + 1];
	long list[MAX_KP];
	int increase = 0;
	int j;

	final[0] = keypad_final(key_a1);
	final[1] = keypad_final(key_a3);
	final[2] = keypad_final(key_b2);
	final[3] = keypad_final(key_c1);
	final[4] = keypad_final(key_c3);
	final[5] = '\0';

	/* special case: legacy coding using 1,2,3,0,. on the bottom */
	assert(strlen(final) <= MAX_KP);
	if (!strcmp(final, "qsrpn"))
	    return;

	list[0] = keypad_index(key_a1);
	list[1] = keypad_index(key_a3);
	list[2] = keypad_index(key_b2);
	list[3] = keypad_index(key_c1);
	list[4] = keypad_index(key_c3);

	/* check that they're all vt100 keys */
	for (j = 0; j < MAX_KP; ++j) {
	    if (list[j] < 0) {
		return;
	    }
	}

	/* check if they're all in increasing order */
	for (j = 1; j < MAX_KP; ++j) {
	    if (list[j] > list[j - 1]) {
		++increase;
	    }
	}

	if (increase != (MAX_KP - 1)) {
	    long last;

	    show[0] = '\0';

	    for (j = 0, last = -1; j < MAX_KP; ++j) {
		int k;
		int kk;
		long test;

		for (k = 0, kk = -1, test = 100; k < 5; ++k) {
		    if (list[k] > last &&
			list[k] < test) {
			test = list[k];
			kk = k;
		    }
		}
		last = test;
		assert(strlen(show) < (MAX_KP * 4));
		switch (kk) {
		case 0:
		    _nc_STRCAT(show, " ka1", sizeof(show));
		    break;
		case 1:
		    _nc_STRCAT(show, " ka3", sizeof(show));
		    break;
		case 2:
		    _nc_STRCAT(show, " kb2", sizeof(show));
		    break;
		case 3:
		    _nc_STRCAT(show, " kc1", sizeof(show));
		    break;
		case 4:
		    _nc_STRCAT(show, " kc3", sizeof(show));
		    break;
		}
	    }

	    _nc_warning("vt100 keypad order inconsistent: %s", show);
	}

    } else if (VALID_STRING(key_a1) ||
	       VALID_STRING(key_a3) ||
	       VALID_STRING(key_b2) ||
	       VALID_STRING(key_c1) ||
	       VALID_STRING(key_c3)) {
	show[0] = '\0';
	if (keypad_index(key_a1) >= 0)
	    _nc_STRCAT(show, " ka1", sizeof(show));
	if (keypad_index(key_a3) >= 0)
	    _nc_STRCAT(show, " ka3", sizeof(show));
	if (keypad_index(key_b2) >= 0)
	    _nc_STRCAT(show, " kb2", sizeof(show));
	if (keypad_index(key_c1) >= 0)
	    _nc_STRCAT(show, " kc1", sizeof(show));
	if (keypad_index(key_c3) >= 0)
	    _nc_STRCAT(show, " kc3", sizeof(show));
	if (*show != '\0')
	    _nc_warning("vt100 keypad map incomplete:%s", show);
    }

    /*
     * These warnings are useful for consistency checks - it is possible that
     * there are real terminals with mismatches in these
     */
    ANDMISSING(key_ic, key_dc);
}

static void
check_printer(TERMTYPE2 *tp)
{
    (void) tp;
#if defined(enter_doublewide_mode) && defined(exit_doublewide_mode)
    PAIRED(enter_doublewide_mode, exit_doublewide_mode);
#endif
#if defined(enter_italics_mode) && defined(exit_italics_mode)
    PAIRED(enter_italics_mode, exit_italics_mode);
#endif
#if defined(enter_leftward_mode) && defined(exit_leftward_mode)
    PAIRED(enter_leftward_mode, exit_leftward_mode);
#endif
#if defined(enter_micro_mode) && defined(exit_micro_mode)
    PAIRED(enter_micro_mode, exit_micro_mode);
#endif
#if defined(enter_shadow_mode) && defined(exit_shadow_mode)
    PAIRED(enter_shadow_mode, exit_shadow_mode);
#endif
#if defined(enter_subscript_mode) && defined(exit_subscript_mode)
    PAIRED(enter_subscript_mode, exit_subscript_mode);
#endif
#if defined(enter_superscript_mode) && defined(exit_superscript_mode)
    PAIRED(enter_superscript_mode, exit_superscript_mode);
#endif
#if defined(enter_upward_mode) && defined(exit_upward_mode)
    PAIRED(enter_upward_mode, exit_upward_mode);
#endif

#if defined(start_char_set_def) && defined(stop_char_set_def)
    ANDMISSING(start_char_set_def, stop_char_set_def);
#endif

    /*
     * If we have a parameterized form, then the non-parameterized is easy.
     * note: parameterized/non-parameterized margin settings are unrelated.
     */
#if defined(parm_down_micro) && defined(micro_down)
    ANDMISSING(parm_down_micro, micro_down);
#endif
#if defined(parm_left_micro) && defined(micro_left)
    ANDMISSING(parm_left_micro, micro_left);
#endif
#if defined(parm_right_micro) && defined(micro_right)
    ANDMISSING(parm_right_micro, micro_right);
#endif
#if defined(parm_up_micro) && defined(micro_up)
    ANDMISSING(parm_up_micro, micro_up);
#endif
}

#if NCURSES_XNAMES
static bool
uses_SGR_39_49(const char *value)
{
    return (strstr(value, "39;49") != 0
	    || strstr(value, "49;39") != 0);
}

/*
 * Check consistency of termcap extensions related to "screen".
 */
static void
check_screen(TERMTYPE2 *tp)
{
    if (_nc_user_definable) {
	int have_XT = tigetflag("XT");
	int have_XM = tigetflag("XM");
	int have_bce = back_color_erase;
	bool have_kmouse = FALSE;
	bool use_sgr_39_49 = FALSE;
	const char *name_39_49 = "orig_pair or orig_colors";
	char *name = _nc_first_name(tp->term_names);
	bool is_screen = !strncmp(name, "screen", 6);
	bool screen_base = (is_screen
			    && strchr(name, '.') == NULL);

	if (!VALID_BOOLEAN(have_bce)) {
	    have_bce = FALSE;
	}
	if (!VALID_BOOLEAN(have_XM)) {
	    have_XM = FALSE;
	}
	if (!VALID_BOOLEAN(have_XT)) {
	    have_XT = FALSE;
	}
	if (VALID_STRING(key_mouse)) {
	    have_kmouse = !strcmp("\033[M", key_mouse);
	}
	if (have_bce) {
	    if (VALID_STRING(orig_pair)) {
		name_39_49 = "orig_pair";
		use_sgr_39_49 = uses_SGR_39_49(orig_pair);
	    }
	    if (!use_sgr_39_49 && VALID_STRING(orig_colors)) {
		name_39_49 = "orig_colors";
		use_sgr_39_49 = uses_SGR_39_49(orig_colors);
	    }
	}

	if (have_XM && have_XT) {
	    _nc_warning("screen's XT capability conflicts with XM");
	} else if (have_XT && screen_base) {
	    _nc_warning("screen's \"screen\" entries should not have XT set");
	} else if (have_XT) {
	    if (!have_kmouse && is_screen) {
		if (VALID_STRING(key_mouse)) {
		    _nc_warning("value of kmous inconsistent with screen's usage");
		} else {
		    _nc_warning("expected kmous capability with XT");
		}
	    }
	    if (max_colors > 0) {
		if (!have_bce) {
		    _nc_warning("expected bce capability with XT");
		} else if (!use_sgr_39_49) {
		    _nc_warning("expected %s capability with XT "
				"to have 39/49 parameters", name_39_49);
		}
	    }
	    if (VALID_STRING(to_status_line))
		_nc_warning("\"tsl\" capability is redundant, given XT");
	} else {
	    if (have_kmouse
		&& !have_XM
		&& !screen_base && strchr(name, '+') == NULL) {
		_nc_warning("expected XT to be set, given kmous");
	    }
	}
    }
}
#else
#define check_screen(tp)	/* nothing */
#endif

/*
 * Returns the expected number of parameters for the given capability.
 */
static int
expected_params(const char *name)
{
#define DATA(name,count) { { name }, count }
    /* *INDENT-OFF* */
    static const struct {
	const char name[9];
	int count;
    } table[] = {
	DATA( "S0",		1 ),	/* 'screen' extension */
	DATA( "birep",		2 ),
	DATA( "chr",		1 ),
	DATA( "colornm",	1 ),
	DATA( "cpi",		1 ),
	DATA( "csnm",		1 ),
	DATA( "csr",		2 ),
	DATA( "cub",		1 ),
	DATA( "cud",		1 ),
	DATA( "cuf",		1 ),
	DATA( "cup",		2 ),
	DATA( "cuu",		1 ),
	DATA( "cvr",		1 ),
	DATA( "cwin",		5 ),
	DATA( "dch",		1 ),
	DATA( "defc",		3 ),
	DATA( "dial",		1 ),
	DATA( "dispc",		1 ),
	DATA( "dl",		1 ),
	DATA( "ech",		1 ),
	DATA( "getm",		1 ),
	DATA( "hpa",		1 ),
	DATA( "ich",		1 ),
	DATA( "il",		1 ),
	DATA( "indn",		1 ),
	DATA( "initc",		4 ),
	DATA( "initp",		7 ),
	DATA( "lpi",		1 ),
	DATA( "mc5p",		1 ),
	DATA( "mrcup",		2 ),
	DATA( "mvpa",		1 ),
	DATA( "pfkey",		2 ),
	DATA( "pfloc",		2 ),
	DATA( "pfx",		2 ),
	DATA( "pfxl",		3 ),
	DATA( "pln",		2 ),
	DATA( "qdial",		1 ),
	DATA( "rcsd",		1 ),
	DATA( "rep",		2 ),
	DATA( "rin",		1 ),
	DATA( "sclk",		3 ),
	DATA( "scp",		1 ),
	DATA( "scs",		1 ),
	DATA( "scsd",		2 ),
	DATA( "setab",		1 ),
	DATA( "setaf",		1 ),
	DATA( "setb",		1 ),
	DATA( "setcolor",	1 ),
	DATA( "setf",		1 ),
	DATA( "sgr",		9 ),
	DATA( "sgr1",		6 ),
	DATA( "slength",	1 ),
	DATA( "slines",		1 ),
	DATA( "smgbp",		1 ),	/* 2 if smgtp is not given */
	DATA( "smglp",		1 ),
	DATA( "smglr",		2 ),
	DATA( "smgrp",		1 ),
	DATA( "smgtb",		2 ),
	DATA( "smgtp",		1 ),
	DATA( "tsl",		1 ),
	DATA( "u6",		-1 ),
	DATA( "vpa",		1 ),
	DATA( "wind",		4 ),
	DATA( "wingo",		1 ),
    };
    /* *INDENT-ON* */
#undef DATA

    unsigned n;
    int result = 0;		/* function-keys, etc., use none */

    for (n = 0; n < SIZEOF(table); n++) {
	if (!strcmp(name, table[n].name)) {
	    result = table[n].count;
	    break;
	}
    }

    return result;
}

/*
 * Check for user-capabilities that happen to be used in ncurses' terminal
 * database.
 */
#if NCURSES_XNAMES
static struct user_table_entry const *
lookup_user_capability(const char *name)
{
    struct user_table_entry const *result = 0;
    if (*name != 'k') {
	result = _nc_find_user_entry(name);
    }
    return result;
}
#endif

/*
 * If a given name is likely to be a user-capability, return the number of
 * parameters it would be used with.  If not, return -1.
 *
 * ncurses assumes that u6 could be used for getting the cursor-position, but
 * that is not implemented.  Make a special case for that, to quiet needless
 * warnings.
 *
 * The other string-capability extensions (see terminfo.src) which could have
 * parameters such as "Ss", "%u", are not used by ncurses.  But we check those
 * anyway, to validate the terminfo database.
 */
static int
is_user_capability(const char *name)
{
    int result = -1;
    if (name[0] == 'u' &&
	(name[1] >= '0' && name[1] <= '9') &&
	name[2] == '\0') {
	result = (name[1] == '6') ? 2 : 0;
    }
#if NCURSES_XNAMES
    else if (using_extensions) {
	struct user_table_entry const *p = lookup_user_capability(name);
	if (p != 0) {
	    result = (int) p->ute_argc;
	}
    }
#endif
    return result;
}

static bool
line_capability(const char *name)
{
    bool result = FALSE;
    static const char *table[] =
    {
	"csr",			/* change_scroll_region          */
	"clear",		/* clear_screen                  */
	"ed",			/* clr_eos                       */
	"cwin",			/* create_window                 */
	"cup",			/* cursor_address                */
	"cud1",			/* cursor_down                   */
	"home",			/* cursor_home                   */
	"mrcup",		/* cursor_mem_address            */
	"ll",			/* cursor_to_ll                  */
	"cuu1",			/* cursor_up                     */
	"dl1",			/* delete_line                   */
	"hd",			/* down_half_line                */
	"flash",		/* flash_screen                  */
	"ff",			/* form_feed                     */
	"il1",			/* insert_line                   */
	"nel",			/* newline                       */
	"dl",			/* parm_delete_line              */
	"cud",			/* parm_down_cursor              */
	"indn",			/* parm_index                    */
	"il",			/* parm_insert_line              */
	"rin",			/* parm_rindex                   */
	"cuu",			/* parm_up_cursor                */
	"mc0",			/* print_screen                  */
	"vpa",			/* row_address                   */
	"ind",			/* scroll_forward                */
	"ri",			/* scroll_reverse                */
	"hu",			/* up_half_line                  */
    };
    size_t n;
    for (n = 0; n < SIZEOF(table); ++n) {
	if (!strcmp(name, table[n])) {
	    result = TRUE;
	    break;
	}
    }
    return result;
}

/*
 * Make a quick sanity check for the parameters which are used in the given
 * strings.  If there are no "%p" tokens, then there should be no other "%"
 * markers.
 */
static void
check_params(TERMTYPE2 *tp, const char *name, const char *value, int extended)
{
    int expected = expected_params(name);
    int actual = 0;
    int n;
    bool params[1 + NUM_PARM];
    const char *s = value;

#ifdef set_left_margin_parm
    if (!strcmp(name, "smgrp")
	&& !VALID_STRING(set_left_margin_parm))
	expected = 2;
#endif
#ifdef set_right_margin_parm
    if (!strcmp(name, "smglp")
	&& !VALID_STRING(set_right_margin_parm))
	expected = 2;
#endif
#ifdef set_top_margin_parm
    if (!strcmp(name, "smgbp")
	&& !VALID_STRING(set_top_margin_parm))
	expected = 2;
#endif
#ifdef set_bottom_margin_parm
    if (!strcmp(name, "smgtp")
	&& !VALID_STRING(set_bottom_margin_parm))
	expected = 2;
#endif

    for (n = 0; n <= NUM_PARM; n++)
	params[n] = FALSE;

    while (*s != 0) {
	if (*s == '%') {
	    if (*++s == '\0') {
		_nc_warning("expected character after %% in %s", name);
		break;
	    } else if (*s == 'p') {
		if (*++s == '\0' || !isdigit((int) *s)) {
		    _nc_warning("expected digit after %%p in %s", name);
		    return;
		} else {
		    n = (*s - '0');
		    if (n > actual)
			actual = n;
		    params[n] = TRUE;
		}
	    }
	}
	s++;
    }

#if NCURSES_XNAMES
    if (extended) {
	int check = is_user_capability(name);
	if (check != actual && (check >= 0 && actual >= 0)) {
	    _nc_warning("extended %s capability has %d parameters, expected %d",
			name, actual, check);
	} else if (debug_level > 1) {
	    _nc_warning("extended %s capability has %d parameters, as expected",
			name, actual);
	}
	expected = actual;
    }
#else
    (void) extended;
#endif

    if (params[0]) {
	_nc_warning("%s refers to parameter 0 (%%p0), which is not allowed", name);
    }
    if (value == set_attributes || expected < 0) {
	;
    } else if (expected != actual) {
	_nc_warning("%s uses %d parameters, expected %d", name,
		    actual, expected);
	for (n = 1; n < actual; n++) {
	    if (!params[n])
		_nc_warning("%s omits parameter %d", name, n);
	}
    }

    /*
     * Counting "%p" markers does not account for termcap expressions which
     * may not have been fully translated.  Also, tparm does its own analysis.
     * Report differences here.
     */
    _nc_reset_tparm(NULL);
    if (actual >= 0) {
	char *p_is_s[NUM_PARM];
	int popcount;
	int analyzed = _nc_tparm_analyze(NULL, value, p_is_s, &popcount);
	if (analyzed < popcount) {
	    analyzed = popcount;
	}
	if (actual != analyzed && expected != analyzed) {
#if NCURSES_XNAMES
	    int user_cap = is_user_capability(name);
	    if ((user_cap == analyzed) && using_extensions) {
		;		/* ignore */
	    } else if (user_cap >= 0) {
		_nc_warning("tparm will use %d parameters for %s, expected %d",
			    analyzed, name, user_cap);
	    } else
#endif
	    {
		_nc_warning("tparm analyzed %d parameters for %s, expected %d",
			    analyzed, name, actual);
	    }
	} else if (expected > 0
		   && actual == expected
		   && guess_tparm_type(expected, p_is_s) == Numbers) {
	    int limit = 1;

	    if (!strcmp(name, "setf")
		|| !strcmp(name, "setb")
		|| !strcmp(name, "setaf")
		|| !strcmp(name, "setab")) {
		if ((limit = max_colors) > 256)
		    limit = 256;
	    } else if (line_capability(name)) {
		limit = 24;
	    } else if (is_user_capability(name) < 0) {
		limit = 80;
	    }
	    for (n = 0; n < limit; ++n) {
		_nc_reset_tparm(NULL);
		(void) TPARM_9(value, n, n, n, n, n, n, n, n, n);
		if (_nc_tparm_err) {
		    _nc_warning("problem%s in tparm(%s, %d, ...)",
				(_nc_tparm_err == 1) ? "" : "s",
				name, n);
		    if (debug_level < 2)
			break;
		}
	    }
	}
    }
}

/*
 * Check for DEC VT100 private mode for reverse video.
 */
static const char *
skip_DECSCNM(const char *value, int *flag)
{
    *flag = -1;
    if (value != 0) {
	int skip = csi_length(value);
	if (skip > 0 &&
	    value[skip++] == '?' &&
	    value[skip++] == '5') {
	    if (value[skip] == 'h') {
		*flag = 1;
	    } else if (value[skip] == 'l') {
		*flag = 0;
	    }
	    value += skip + 1;
	}
    }
    return value;
}

static void
check_delays(TERMTYPE2 *tp, const char *name, const char *value)
{
    const char *p, *q;
    const char *first = 0;
    const char *last = 0;

    for (p = value; *p != '\0'; ++p) {
	if (p[0] == '$' && p[1] == '<') {
	    const char *base = p + 2;
	    const char *mark = 0;
	    bool mixed = FALSE;
	    int proportional = 0;
	    int mandatory = 0;

	    first = p;

	    for (q = base; *q != '\0'; ++q) {
		if (*q == '>') {
		    if (mark == NULL)
			mark = q;
		    break;
		} else if (*q == '*' || *q == '/') {
		    if (*q == '*')
			++proportional;
		    if (*q == '/')
			++mandatory;
		    if (mark == NULL)
			mark = q;
		} else if (!(isalnum(UChar(*q)) || strchr("+-.", *q) != 0)) {
		    break;
		} else if (proportional || mandatory) {
		    mixed = TRUE;
		}
	    }
	    last = *q ? (q + 1) : q;
	    if (*q != '\0') {
		float check_f;
		char check_c;
		int rc = sscanf(base, "%f%c", &check_f, &check_c);
		if ((rc != 2) || (mark != NULL && (check_c != *mark)) || mixed) {
		    _nc_warning("syntax error in %s delay '%.*s'", name,
				(int) (q - base), base);
		} else if (*name == 'k') {
		    _nc_warning("function-key %s has delay", name);
		} else if (proportional && !line_capability(name)) {
		    _nc_warning("non-line capability using proportional delay: %s", name);
		} else if (!xon_xoff &&
			   !mandatory &&
			   strchr(_nc_first_name(tp->term_names), '+') == NULL) {
		    _nc_warning("%s in %s is used since no xon/xoff",
				(proportional
				 ? "proportional delay"
				 : "delay"),
				name);
		}
	    } else {
		p = q - 1;	/* restart scan */
	    }
	}
    }

    if (!strcmp(name, "flash") ||
	!strcmp(name, "beep")) {

	if (first != 0) {
	    if (first == value || *last == 0) {
		/*
		 * Delay is on one end or the other.
		 */
		_nc_warning("expected delay embedded within %s", name);
	    }
	} else {
	    int flag;

	    /*
	     * Check for missing delay when using VT100 reverse-video.
	     * A real VT100 might not need this, but terminal emulators do.
	     */
	    if ((p = skip_DECSCNM(value, &flag)) != 0 &&
		flag > 0 &&
		skip_DECSCNM(p, &flag) != 0 &&
		flag == 0) {
		_nc_warning("expected a delay in %s", name);
	    }
	}
    }
}

static char *
check_1_infotocap(const char *name, NCURSES_CONST char *value, int count)
{
    int k;
    int ignored;
    long numbers[1 + NUM_PARM];
    char *strings[1 + NUM_PARM];
    char *p_is_s[NUM_PARM];
    char *result;
    char blob[NUM_PARM * 10];
    char *next = blob;
    TParams expect;
    TParams actual;
    int nparam;

    *next++ = '\0';
    for (k = 1; k <= NUM_PARM; k++) {
	numbers[k] = count;
	_nc_SPRINTF(next,
		    _nc_SLIMIT(sizeof(blob) - (size_t) (next - blob))
		    "XYZ%d", count);
	strings[k] = next;
	next += strlen(next) + 1;
    }

    _nc_reset_tparm(NULL);
    expect = tparm_type(name);
    nparam = _nc_tparm_analyze(NULL, value, p_is_s, &ignored);
    actual = guess_tparm_type(nparam, p_is_s);

    if (expect != actual) {
	_nc_warning("%s has mismatched parameters", name);
	actual = Other;
    }

    _nc_reset_tparm(NULL);
    switch (actual) {
    case Num_Str:
	result = TPARM_2(value, numbers[1], strings[2]);
	break;
    case Num_Str_Str:
	result = TPARM_3(value, numbers[1], strings[2], strings[3]);
	break;
    case Numbers:
#define myParam(n) numbers[n]
	result = TIPARM_9(value,
			  myParam(1),
			  myParam(2),
			  myParam(3),
			  myParam(4),
			  myParam(5),
			  myParam(6),
			  myParam(7),
			  myParam(8),
			  myParam(9));
#undef myParam
	break;
    case Other:
    default:
#define myParam(n) (p_is_s[n - 1] != 0 ? ((TPARM_ARG) strings[n]) : numbers[n])
	result = TPARM_9(value,
			 myParam(1),
			 myParam(2),
			 myParam(3),
			 myParam(4),
			 myParam(5),
			 myParam(6),
			 myParam(7),
			 myParam(8),
			 myParam(9));
#undef myParam
	break;
    }
    return strdup(result);
}

#define IsDelay(ch) ((ch) == '.' || isdigit(UChar(ch)))

static const char *
parse_delay_value(const char *src, double *delays, int *always)
{
    int star = 0;

    *delays = 0.0;
    if (always)
	*always = 0;

    while (isdigit(UChar(*src))) {
	(*delays) = (*delays) * 10 + (*src++ - '0');
    }
    if (*src == '.') {
	int gotdot = 1;

	++src;
	while (isdigit(UChar(*src))) {
	    gotdot *= 10;
	    (*delays) += (*src++ - '0') / gotdot;
	}
    }
    while (*src == '*' || *src == '/') {
	if (always == NULL && *src == '/')
	    break;
	if (*src++ == '*') {
	    star = 1;
	} else {
	    *always = 1;
	}
    }
    if (star)
	*delays = -(*delays);
    return src;
}

static const char *
parse_ti_delay(const char *ti, double *delays)
{
    *delays = 0.0;
    while (*ti != '\0') {
	if (*ti == '\\') {
	    ++ti;
	}
	if (ti[0] == '$'
	    && ti[1] == '<'
	    && IsDelay(UChar(ti[2]))) {
	    int ignored;
	    const char *last = parse_delay_value(ti + 2, delays, &ignored);
	    if (*last == '>') {
		ti = last;
	    }
	} else {
	    ++ti;
	}
    }
    return ti;
}

static const char *
parse_tc_delay(const char *tc, double *delays)
{
    return parse_delay_value(tc, delays, (int *) 0);
}

/*
 * Compare terminfo- and termcap-strings, factoring out delays.
 */
static bool
same_ti_tc(const char *ti, const char *tc, bool * embedded)
{
    bool same = TRUE;
    double ti_delay = 0.0;
    double tc_delay = 0.0;
    const char *ti_last;

    *embedded = FALSE;
    ti_last = parse_ti_delay(ti, &ti_delay);
    tc = parse_tc_delay(tc, &tc_delay);

    while ((ti < ti_last) && *tc) {
	if (*ti == '\\' && ispunct(UChar(ti[1]))) {
	    ++ti;
	    if ((*ti == '^') && !strncmp(tc, "\\136", 4)) {
		ti += 1;
		tc += 4;
		continue;
	    }
	} else if (ti[0] == '$' && ti[1] == '<') {
	    double no_delay;
	    const char *ss = parse_ti_delay(ti, &no_delay);
	    if (ss != ti) {
		*embedded = TRUE;
		ti = ss;
		continue;
	    }
	}
	if (*tc == '\\' && ispunct(UChar(tc[1]))) {
	    ++tc;
	}
	if (*ti++ != *tc++) {
	    same = FALSE;
	    break;
	}
    }

    if (*embedded) {
	if (same) {
	    same = FALSE;
	} else {
	    *embedded = FALSE;	/* report only one problem */
	}
    }

    return same;
}

/*
 * Check terminfo to termcap translation.
 */
static void
check_infotocap(TERMTYPE2 *tp, int i, const char *value)
{
    const char *name = ExtStrname(tp, i, strnames);
    int params = ((i < (int) SIZEOF(parametrized))
		  ? parametrized[i]
		  : ((*value == 'k')
		     ? 0
		     : has_params(value, FALSE)));
    char *ti_value = NULL;
    char *tc_value;
    bool embedded;

    assert(SIZEOF(parametrized) == STRCOUNT);
    if (!VALID_STRING(value) || (ti_value = strdup(value)) == NULL) {
	_nc_warning("tic-expansion of %s failed", name);
    } else if ((tc_value = _nc_infotocap(name, ti_value, params)) == ABSENT_STRING) {
	_nc_warning("tic-conversion of %s failed", name);
    } else if (params > 0) {
	int limit = 5;
	int count;
	bool first = TRUE;

	if (!strcmp(name, "setf")
	    || !strcmp(name, "setb")
	    || !strcmp(name, "setaf")
	    || !strcmp(name, "setab")) {
	    if ((limit = max_colors) > 256)
		limit = 256;
	}
	for (count = 0; count < limit; ++count) {
	    char *ti_check = check_1_infotocap(name, ti_value, count);
	    char *tc_check = check_1_infotocap(name, tc_value, count);

	    if (strcmp(ti_check, tc_check)) {
		if (first) {
		    fprintf(stderr, "check_infotocap(%s)\n", name);
		    fprintf(stderr, "...ti '%s'\n", _nc_visbuf2(0, ti_value));
		    fprintf(stderr, "...tc '%s'\n", _nc_visbuf2(0, tc_value));
		    first = FALSE;
		}
		_nc_warning("tparm-conversion of %s(%d) differs between\n\tterminfo %s\n\ttermcap  %s",
			    name, count,
			    _nc_visbuf2(0, ti_check),
			    _nc_visbuf2(1, tc_check));
	    }
	    free(ti_check);
	    free(tc_check);
	}
    } else if (params == 0 && !same_ti_tc(ti_value, tc_value, &embedded)) {
	if (embedded) {
	    _nc_warning("termcap equivalent of %s cannot use embedded delay", name);
	} else {
	    _nc_warning("tic-conversion of %s changed value\n\tfrom %s\n\tto   %s",
			name, ti_value, tc_value);
	}
    }
    free(ti_value);
}

static char *
skip_delay(char *s)
{
    while (*s == '/' || isdigit(UChar(*s)))
	++s;
    return s;
}

/*
 * Skip a delay altogether, e.g., when comparing a simple string to sgr,
 * the latter may have a worst-case delay on the end.
 */
static char *
ignore_delays(char *s)
{
    int delaying = 0;

    do {
	switch (*s) {
	case '$':
	    if (delaying == 0)
		delaying = 1;
	    break;
	case '<':
	    if (delaying == 1)
		delaying = 2;
	    break;
	case '\0':
	    delaying = 0;
	    break;
	default:
	    if (delaying) {
		s = skip_delay(s);
		if (*s == '>')
		    ++s;
		delaying = 0;
	    }
	    break;
	}
	if (delaying)
	    ++s;
    } while (delaying);
    return s;
}

#define DATA(name) { #name }
static const char sgr_names[][11] =
{
    DATA(none),
    DATA(standout),
    DATA(underline),
    DATA(reverse),
    DATA(blink),
    DATA(dim),
    DATA(bold),
    DATA(invis),
    DATA(protect),
    DATA(altcharset),
    ""
};
#undef DATA

/*
 * An sgr string may contain several settings other than the one we're
 * interested in, essentially sgr0 + rmacs + whatever.  As long as the
 * "whatever" is contained in the sgr string, that is close enough for our
 * sanity check.
 */
static bool
similar_sgr(int num, char *a, char *b)
{
    char *base_a = a;
    char *base_b = b;
    int delaying = 0;

    while (*b != 0) {
	while (*a != *b) {
	    if (*a == 0) {
		if (num < 0) {
		    ;
		} else if (b[0] == '$'
			   && b[1] == '<') {
		    _nc_warning("did not find delay %s", _nc_visbuf(b));
		} else {
		    _nc_warning("checking sgr(%s) %s\n\tcompare to %s\n\tunmatched %s",
				sgr_names[num], _nc_visbuf2(1, base_a),
				_nc_visbuf2(2, base_b),
				_nc_visbuf2(3, b));
		}
		return FALSE;
	    } else if (delaying) {
		a = skip_delay(a);
		b = skip_delay(b);
	    } else if ((*b == '0' || (*b == ';')) && *a == 'm') {
		b++;
	    } else {
		a++;
	    }
	}
	switch (*a) {
	case '$':
	    if (delaying == 0)
		delaying = 1;
	    break;
	case '<':
	    if (delaying == 1)
		delaying = 2;
	    break;
	default:
	    delaying = 0;
	    break;
	}
	a++;
	b++;
    }
    /* ignore delays on the end of the string */
    a = ignore_delays(a);
    return ((num != 0) || (*a == 0));
}

static void
check_tparm_err(int num)
{
    if (_nc_tparm_err)
	_nc_warning("tparam error in sgr(%d): %s", num, sgr_names[num]);
}

static char *
check_sgr(TERMTYPE2 *tp, char *zero, int num, char *cap, const char *name)
{
    char *test;

    _nc_tparm_err = 0;
    test = TIPARM_9(set_attributes,
		    num == 1,
		    num == 2,
		    num == 3,
		    num == 4,
		    num == 5,
		    num == 6,
		    num == 7,
		    num == 8,
		    num == 9);
    if (test != 0) {
	if (PRESENT(cap)) {
	    if (!similar_sgr(num, test, cap)) {
		_nc_warning("%s differs from sgr(%d)\n\t%s=%s\n\tsgr(%d)=%s",
			    name, num,
			    name, _nc_visbuf2(1, cap),
			    num, _nc_visbuf2(2, test));
	    }
	} else if (_nc_capcmp(test, zero)) {
	    _nc_warning("sgr(%d) present, but not %s", num, name);
	}
    } else if (PRESENT(cap)) {
	_nc_warning("sgr(%d) missing, but %s present", num, name);
    }
    check_tparm_err(num);
    return test;
}

#define CHECK_SGR(num,name) check_sgr(tp, zero, num, name, #name)

#ifdef TRACE
/*
 * If tic is compiled with TRACE, we'll be able to see the output from the
 * DEBUG() macro.  But since it doesn't use traceon(), it always goes to
 * the standard error.  Use this function to make it simpler to follow the
 * resulting debug traces.
 */
static void
show_where(unsigned level)
{
    if (_nc_tracing >= DEBUG_LEVEL(level)) {
	char my_name[MAX_NAME_SIZE];
	_nc_get_type(my_name);
	_tracef("\"%s\", line %d, '%s'",
		_nc_get_source(),
		_nc_curr_line, my_name);
    }
}

#else
#define show_where(level)	/* nothing */
#endif

typedef struct {
    int keycode;
    const char *name;
    const char *value;
} NAME_VALUE;

static NAME_VALUE *
get_fkey_list(TERMTYPE2 *tp)
{
    NAME_VALUE *result = typeMalloc(NAME_VALUE, NUM_STRINGS(tp) + 1);
    const struct tinfo_fkeys *all_fkeys = _nc_tinfo_fkeys;
    int used = 0;
    unsigned j;

    if (result == NULL)
	failed("get_fkey_list");

    for (j = 0; all_fkeys[j].code; j++) {
	char *a = tp->Strings[all_fkeys[j].offset];
	if (VALID_STRING(a)) {
	    result[used].keycode = (int) all_fkeys[j].code;
	    result[used].name = strnames[all_fkeys[j].offset];
	    result[used].value = a;
	    ++used;
	}
    }
#if NCURSES_XNAMES
    for (j = STRCOUNT; j < NUM_STRINGS(tp); ++j) {
	const char *name = ExtStrname(tp, (int) j, strnames);
	if (*name == 'k') {
	    result[used].keycode = -1;
	    result[used].name = name;
	    result[used].value = tp->Strings[j];
	    ++used;
	}
    }
#endif
    result[used].keycode = 0;
    return result;
}

static void
show_fkey_name(NAME_VALUE * data)
{
    if (data->keycode > 0) {
	fprintf(stderr, " %s", keyname(data->keycode));
	fprintf(stderr, " (capability \"%s\")", data->name);
    } else {
	fprintf(stderr, " capability \"%s\"", data->name);
    }
}

/*
 * A terminal entry may contain more than one keycode assigned to a given
 * string (e.g., KEY_END and KEY_LL).  But curses will only return one (the
 * last one assigned).
 */
static void
check_conflict(TERMTYPE2 *tp)
{
    bool conflict = FALSE;

    if (!(_nc_syntax == SYN_TERMCAP && capdump)) {
	char *check = calloc((size_t) (NUM_STRINGS(tp) + 1), sizeof(char));
	NAME_VALUE *given = get_fkey_list(tp);
	unsigned j, k;

	if (check == NULL)
	    failed("check_conflict");

	for (j = 0; given[j].keycode; ++j) {
	    const char *a = given[j].value;
	    bool first = TRUE;

	    if (!VALID_STRING(a))
		continue;

	    for (k = j + 1; given[k].keycode; k++) {
		const char *b = given[k].value;

		if (!VALID_STRING(b))
		    continue;
		if (check[k])
		    continue;

		if (!_nc_capcmp(a, b)) {
		    check[j] = 1;
		    check[k] = 1;
		    if (first) {
			if (!conflict) {
			    _nc_warning("conflicting key definitions (using the last)");
			    conflict = TRUE;
			}
			fprintf(stderr, "...");
			show_fkey_name(given + j);
			fprintf(stderr, " is the same as");
			show_fkey_name(given + k);
			first = FALSE;
		    } else {
			fprintf(stderr, ", ");
			show_fkey_name(given + k);
		    }
		}
	    }
	    if (!first)
		fprintf(stderr, "\n");
	}
#if NCURSES_XNAMES
	if (using_extensions) {
	    /* *INDENT-OFF* */
	    static struct {
		const char *xcurses;
		const char *shifted;
	    } table[] = {
		{ "kDC",  NULL },
		{ "kDN",  "kind" },
		{ "kEND", NULL },
		{ "kHOM", NULL },
		{ "kLFT", NULL },
		{ "kNXT", NULL },
		{ "kPRV", NULL },
		{ "kRIT", NULL },
		{ "kUP",  "kri" },
		{ NULL,   NULL },
	    };
	    /* *INDENT-ON* */
	    /*
	     * SVr4 curses defines the "xcurses" names listed above except for
	     * the special cases in the "shifted" column.  When using these
	     * names for xterm's extensions, that was confusing, and resulted
	     * in adding extended capabilities with "2" (shift) suffix.  This
	     * check warns about unnecessary use of extensions for this quirk.
	     */
	    for (j = 0; given[j].keycode; ++j) {
		const char *find = given[j].name;
		int value;
		char ch;

		if (!VALID_STRING(given[j].value))
		    continue;

		for (k = 0; table[k].xcurses; ++k) {
		    const char *test = table[k].xcurses;
		    size_t size = strlen(test);

		    if (!strncmp(find, test, size) && strcmp(find, test)) {
			switch (sscanf(find + size, "%d%c", &value, &ch)) {
			case 1:
			    if (value == 2) {
				_nc_warning("expected '%s' rather than '%s'",
					    (table[k].shifted
					     ? table[k].shifted
					     : test), find);
			    } else if (value < 2 || value > 15) {
				_nc_warning("expected numeric 2..15 '%s'", find);
			    }
			    break;
			default:
			    _nc_warning("expected numeric suffix for '%s'", find);
			    break;
			}
			break;
		    }
		}
	    }
	}
#endif
	free(given);
	free(check);
    }
}

/*
 * Exiting a video mode should not duplicate sgr0
 */
static void
check_exit_attribute(const char *name, char *test, char *trimmed, char *untrimmed)
{
    if (VALID_STRING(test) && (trimmed != 0)) {
	if (similar_sgr(-1, trimmed, test) ||
	    similar_sgr(-1, untrimmed, test)) {
	    _nc_warning("%s matches exit_attribute_mode", name);
	}
    }
}

/*
 * Returns true if the string looks like a standard SGR string.
 */
static bool
is_sgr_string(char *value)
{
    bool result = FALSE;

    if (VALID_STRING(value)) {
	int skip = csi_length(value);

	if (skip) {
	    int ch;

	    result = TRUE;
	    value += skip;
	    while ((ch = UChar(*value++)) != '\0') {
		if (isdigit(ch) || ch == ';') {
		    ;
		} else if (ch == 'm' && *value == '\0') {
		    ;
		} else {
		    result = FALSE;
		    break;
		}
	    }
	}
    }
    return result;
}

/*
 * Check if the given capability contains a given SGR attribute.
 */
static void
check_sgr_param(TERMTYPE2 *tp, int code, const char *name, char *value)
{
    if (VALID_STRING(value)) {
	int ncv = ((code != 0) ? (1 << (code - 1)) : 0);
	char *test = tgoto(value, 0, 0);
	if (is_sgr_string(test)) {
	    int param = 0;
	    int count = 0;
	    int skips = 0;
	    int color = (value == set_a_foreground ||
			 value == set_a_background ||
			 value == set_foreground ||
			 value == set_background);
	    while (*test != 0) {
		if (isdigit(UChar(*test))) {
		    param = 10 * param + (*test - '0');
		    ++count;
		} else {
		    if (count) {
			/*
			 * Avoid unnecessary warning for xterm 256color codes.
			 */
			if (color && (param == 38 || param == 48))
			    skips = 3;
			if ((skips-- <= 0) && (param == code))
			    break;
		    }
		    count = 0;
		    param = 0;
		}
		++test;
	    }
	    if (count != 0 && param == code) {
		if (code == 0 ||
		    no_color_video < 0 ||
		    !(no_color_video & ncv)) {
		    _nc_warning("\"%s\" SGR-attribute used in %s",
				sgr_names[code],
				name);
		}
	    }
	}
    }
}

#if NCURSES_XNAMES
static int
standard_type(const char *name)
{
    int result = -1;
    const struct name_table_entry *np;

    if ((np = _nc_find_entry(name, _nc_get_hash_table(0))) != 0) {
	result = np->nte_type;
    }
    return result;
}

static const char *
name_of_type(int type)
{
    const char *result = "unknown";
    switch (type) {
    case BOOLEAN:
	result = "boolean";
	break;
    case NUMBER:
	result = "number";
	break;
    case STRING:
	result = "string";
	break;
    }
    return result;
}

static void
check_user_capability_type(const char *name, int actual)
{
    if (lookup_user_capability(name) == 0) {
	int expected = standard_type(name);
	if (expected >= 0) {
	    _nc_warning("expected %s to be %s, but actually %s",
			name,
			name_of_type(actual),
			name_of_type(expected)
		);
	} else if (*name != 'k') {
	    _nc_warning("undocumented %s capability %s",
			name_of_type(actual),
			name);
	}
    }
}
#endif

/* other sanity-checks (things that we don't want in the normal
 * logic that reads a terminfo entry)
 */
static void
check_termtype(TERMTYPE2 *tp, bool literal)
{
    unsigned j;

    check_conflict(tp);

    for_each_string(j, tp) {
	char *a = tp->Strings[j];
	if (VALID_STRING(a)) {
	    const char *name = ExtStrname(tp, (int) j, strnames);
	    /*
	     * If we expect parameters, or if there might be parameters,
	     * check for consistent number of parameters.
	     */
	    if (j >= SIZEOF(parametrized) ||
		is_user_capability(name) >= 0 ||
		parametrized[j] > 0) {
		check_params(tp, name, a, (j >= STRCOUNT));
	    }
	    check_delays(tp, ExtStrname(tp, (int) j, strnames), a);
	    if (capdump) {
		check_infotocap(tp, (int) j, a);
	    }
	}
    }
#if NCURSES_XNAMES
    /* in extended mode, verify that each extension is expected type */
    for_each_ext_boolean(j, tp) {
	check_user_capability_type(ExtBoolname(tp, (int) j, strnames), BOOLEAN);
    }
    for_each_ext_number(j, tp) {
	check_user_capability_type(ExtNumname(tp, (int) j, strnames), NUMBER);
    }
    for_each_ext_string(j, tp) {
	check_user_capability_type(ExtStrname(tp, (int) j, strnames), STRING);
    }
#endif /* NCURSES_XNAMES */

    check_acs(tp);
    check_colors(tp);
    check_cursor(tp);
    check_keypad(tp);
    check_printer(tp);
    check_screen(tp);

    /*
     * These are probably both or none.
     */
    PAIRED(parm_index, parm_rindex);
    PAIRED(parm_ich, parm_dch);

    /*
     * These may be mismatched because the terminal description relies on
     * restoring the cursor visibility by resetting it.
     */
    ANDMISSING(cursor_invisible, cursor_normal);
    ANDMISSING(cursor_visible, cursor_normal);

    if (PRESENT(cursor_visible) && PRESENT(cursor_normal)
	&& !_nc_capcmp(cursor_visible, cursor_normal))
	_nc_warning("cursor_visible is same as cursor_normal");

    /*
     * From XSI & O'Reilly, we gather that sc/rc are required if csr is
     * given, because the cursor position after the scrolling operation is
     * performed is undefined.
     */
    ANDMISSING(change_scroll_region, save_cursor);
    ANDMISSING(change_scroll_region, restore_cursor);

    /*
     * If we can clear tabs, we should be able to initialize them.
     */
    ANDMISSING(clear_all_tabs, set_tab);

    if (PRESENT(set_attributes)) {
	char *zero = 0;

	_nc_tparm_err = 0;
	if (PRESENT(exit_attribute_mode)) {
	    zero = strdup(CHECK_SGR(0, exit_attribute_mode));
	} else {
	    zero = strdup(TIPARM_9(set_attributes, 0, 0, 0, 0, 0, 0, 0, 0, 0));
	}
	check_tparm_err(0);

	if (zero != 0) {
	    CHECK_SGR(1, enter_standout_mode);
	    CHECK_SGR(2, enter_underline_mode);
	    CHECK_SGR(3, enter_reverse_mode);
	    CHECK_SGR(4, enter_blink_mode);
	    CHECK_SGR(5, enter_dim_mode);
	    CHECK_SGR(6, enter_bold_mode);
	    CHECK_SGR(7, enter_secure_mode);
	    CHECK_SGR(8, enter_protected_mode);
	    CHECK_SGR(9, enter_alt_charset_mode);
	    free(zero);
	} else {
	    _nc_warning("sgr(0) did not return a value");
	}
    } else if (PRESENT(exit_attribute_mode) &&
	       set_attributes != CANCELLED_STRING) {
	if (_nc_syntax == SYN_TERMINFO)
	    _nc_warning("missing sgr string");
    }
#define CHECK_SGR0(name) check_exit_attribute(#name, name, check_sgr0, exit_attribute_mode)
    if (PRESENT(exit_attribute_mode)) {
	char *check_sgr0 = _nc_trim_sgr0(tp);

	if (check_sgr0 == NULL || *check_sgr0 == '\0') {
	    _nc_warning("trimmed sgr0 is empty");
	} else {
	    show_where(2);
	    if (check_sgr0 != exit_attribute_mode) {
		DEBUG(2,
		      ("will trim sgr0\n\toriginal sgr0=%s\n\ttrimmed  sgr0=%s",
		       _nc_visbuf2(1, exit_attribute_mode),
		       _nc_visbuf2(2, check_sgr0)));
	    } else {
		DEBUG(2,
		      ("will not trim sgr0\n\toriginal sgr0=%s",
		       _nc_visbuf(exit_attribute_mode)));
	    }
	}
#if defined(exit_italics_mode)
	CHECK_SGR0(exit_italics_mode);
#endif
	CHECK_SGR0(exit_standout_mode);
	CHECK_SGR0(exit_underline_mode);
	if (check_sgr0 != exit_attribute_mode) {
	    free(check_sgr0);
	}
    }
#define CHECK_SGR_PARAM(code, name) check_sgr_param(tp, (int)code, #name, name)
    for (j = 0; *sgr_names[j] != '\0'; ++j) {
	CHECK_SGR_PARAM(j, set_a_foreground);
	CHECK_SGR_PARAM(j, set_a_background);
	CHECK_SGR_PARAM(j, set_foreground);
	CHECK_SGR_PARAM(j, set_background);
    }
#ifdef TRACE
    show_where(2);
    if (!auto_right_margin) {
	DEBUG(2,
	      ("can write to lower-right directly"));
    } else if (PRESENT(enter_am_mode) && PRESENT(exit_am_mode)) {
	DEBUG(2,
	      ("can write to lower-right by suppressing automargin"));
    } else if ((PRESENT(enter_insert_mode) && PRESENT(exit_insert_mode))
	       || PRESENT(insert_character) || PRESENT(parm_ich)) {
	DEBUG(2,
	      ("can write to lower-right by using inserts"));
    } else {
	DEBUG(2,
	      ("cannot write to lower-right"));
    }
#endif

    /*
     * Some standard applications (e.g., vi) and some non-curses
     * applications (e.g., jove) get confused if we have both ich1 and
     * smir/rmir.  Let's be nice and warn about that, too, even though
     * ncurses handles it.
     */
    if ((PRESENT(enter_insert_mode) || PRESENT(exit_insert_mode))
	&& PRESENT(insert_character)) {
	_nc_warning("non-curses applications may be confused by ich1 with smir/rmir");
    }

    /*
     * Finally, do the non-verbose checks
     */
    if (save_check_termtype != 0)
	save_check_termtype(tp, literal);
}
