/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 1998-2016,2017 Free Software Foundation, Inc.                  *
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
 *     and: Thomas E. Dickey                        1996-on                 *
 ****************************************************************************/

/*
 * Notes:
 * The initial adaptation from 4.4BSD Lite sources in September 1995 used 686
 * lines from that version, and made changes/additions for 150 lines.  There
 * was no reformatting, so with/without ignoring whitespace, the amount of
 * change is the same.
 *
 * Comparing with current (2009) source, excluding this comment:
 * a) 209 lines match identically to the 4.4BSD Lite sources, with 771 lines
 *    changed/added.
 * a) Ignoring whitespace, the current version still uses 516 lines from the
 *    4.4BSD Lite sources, with 402 lines changed/added.
 *
 * Raymond's original comment on this follows...
 */

/*
 * tset.c - terminal initialization utility
 *
 * This code was mostly swiped from 4.4BSD tset, with some obsolescent
 * cruft removed and substantial portions rewritten.  A Regents of the
 * University of California copyright applies to some portions of the
 * code, and is reproduced below:
 */
/*-
 * Copyright (c) 1980, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <reset_cmd.h>
#include <termcap.h>
#include <transform.h>
#include <tty_settings.h>

#if HAVE_GETTTYNAM && HAVE_TTYENT_H
#include <ttyent.h>
#endif
#ifdef NeXT
char *ttyname(int fd);
#endif

MODULE_ID("$Id: tset.c,v 1.130 2021/10/02 18:08:09 tom Exp $")

#ifndef environ
extern char **environ;
#endif

const char *_nc_progname = "tset";

#define LOWERCASE(c) ((isalpha(UChar(c)) && isupper(UChar(c))) ? tolower(UChar(c)) : (c))

static GCC_NORETURN void exit_error(void);

static int
CaselessCmp(const char *a, const char *b)
{				/* strcasecmp isn't portable */
    while (*a && *b) {
	int cmp = LOWERCASE(*a) - LOWERCASE(*b);
	if (cmp != 0)
	    break;
	a++, b++;
    }
    return LOWERCASE(*a) - LOWERCASE(*b);
}

static GCC_NORETURN void
exit_error(void)
{
    restore_tty_settings();
    (void) fprintf(stderr, "\n");
    fflush(stderr);
    ExitProgram(EXIT_FAILURE);
    /* NOTREACHED */
}

static GCC_NORETURN void
err(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    (void) fprintf(stderr, "%s: ", _nc_progname);
    (void) vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit_error();
    /* NOTREACHED */
}

static GCC_NORETURN void
failed(const char *msg)
{
    char temp[BUFSIZ];
    size_t len = strlen(_nc_progname) + 2;

    if ((int) len < (int) sizeof(temp) - 12) {
	_nc_STRCPY(temp, _nc_progname, sizeof(temp));
	_nc_STRCAT(temp, ": ", sizeof(temp));
    } else {
	_nc_STRCPY(temp, "tset: ", sizeof(temp));
    }
    _nc_STRNCAT(temp, msg, sizeof(temp), sizeof(temp) - strlen(temp) - 2);
    perror(temp);
    exit_error();
    /* NOTREACHED */
}

/* Prompt the user for a terminal type. */
static const char *
askuser(const char *dflt)
{
    static char answer[256];

    /* We can get recalled; if so, don't continue uselessly. */
    clearerr(stdin);
    if (feof(stdin) || ferror(stdin)) {
	(void) fprintf(stderr, "\n");
	exit_error();
	/* NOTREACHED */
    }

    for (;;) {
	char *p;

	if (dflt)
	    (void) fprintf(stderr, "Terminal type? [%s] ", dflt);
	else
	    (void) fprintf(stderr, "Terminal type? ");
	(void) fflush(stderr);

	if (fgets(answer, sizeof(answer), stdin) == 0) {
	    if (dflt == 0) {
		exit_error();
		/* NOTREACHED */
	    }
	    return (dflt);
	}

	if ((p = strchr(answer, '\n')) != 0)
	    *p = '\0';
	if (answer[0])
	    return (answer);
	if (dflt != 0)
	    return (dflt);
    }
}

/**************************************************************************
 *
 * Mapping logic begins here
 *
 **************************************************************************/

/* Baud rate conditionals for mapping. */
#define	GT		0x01
#define	EQ		0x02
#define	LT		0x04
#define	NOT		0x08
#define	GE		(GT | EQ)
#define	LE		(LT | EQ)

typedef struct map {
    struct map *next;		/* Linked list of maps. */
    const char *porttype;	/* Port type, or "" for any. */
    const char *type;		/* Terminal type to select. */
    int conditional;		/* Baud rate conditionals bitmask. */
    int speed;			/* Baud rate to compare against. */
} MAP;

static MAP *cur, *maplist;

#define DATA(name,value) { { name }, value }

typedef struct speeds {
    const char string[8];
    int speed;
} SPEEDS;

#if defined(EXP_WIN32_DRIVER)
static const SPEEDS speeds[] =
{
    {"0", 0}
};
#else
static const SPEEDS speeds[] =
{
    DATA("0", B0),
    DATA("50", B50),
    DATA("75", B75),
    DATA("110", B110),
    DATA("134", B134),
    DATA("134.5", B134),
    DATA("150", B150),
    DATA("200", B200),
    DATA("300", B300),
    DATA("600", B600),
    DATA("1200", B1200),
    DATA("1800", B1800),
    DATA("2400", B2400),
    DATA("4800", B4800),
    DATA("9600", B9600),
    /* sgttyb may define up to this point */
#ifdef B19200
    DATA("19200", B19200),
#endif
#ifdef B38400
    DATA("38400", B38400),
#endif
#ifdef B19200
    DATA("19200", B19200),
#endif
#ifdef B38400
    DATA("38400", B38400),
#endif
#ifdef B19200
    DATA("19200", B19200),
#else
#ifdef EXTA
    DATA("19200", EXTA),
#endif
#endif
#ifdef B38400
    DATA("38400", B38400),
#else
#ifdef EXTB
    DATA("38400", EXTB),
#endif
#endif
#ifdef B57600
    DATA("57600", B57600),
#endif
#ifdef B76800
    DATA("76800", B57600),
#endif
#ifdef B115200
    DATA("115200", B115200),
#endif
#ifdef B153600
    DATA("153600", B153600),
#endif
#ifdef B230400
    DATA("230400", B230400),
#endif
#ifdef B307200
    DATA("307200", B307200),
#endif
#ifdef B460800
    DATA("460800", B460800),
#endif
#ifdef B500000
    DATA("500000", B500000),
#endif
#ifdef B576000
    DATA("576000", B576000),
#endif
#ifdef B921600
    DATA("921600", B921600),
#endif
#ifdef B1000000
    DATA("1000000", B1000000),
#endif
#ifdef B1152000
    DATA("1152000", B1152000),
#endif
#ifdef B1500000
    DATA("1500000", B1500000),
#endif
#ifdef B2000000
    DATA("2000000", B2000000),
#endif
#ifdef B2500000
    DATA("2500000", B2500000),
#endif
#ifdef B3000000
    DATA("3000000", B3000000),
#endif
#ifdef B3500000
    DATA("3500000", B3500000),
#endif
#ifdef B4000000
    DATA("4000000", B4000000),
#endif
};
#undef DATA
#endif

static int
tbaudrate(char *rate)
{
    const SPEEDS *sp = 0;
    size_t n;

    /* The baudrate number can be preceded by a 'B', which is ignored. */
    if (*rate == 'B')
	++rate;

    for (n = 0; n < SIZEOF(speeds); ++n) {
	if (n > 0 && (speeds[n].speed <= speeds[n - 1].speed)) {
	    /* if the speeds are not increasing, likely a numeric overflow */
	    break;
	}
	if (!CaselessCmp(rate, speeds[n].string)) {
	    sp = speeds + n;
	    break;
	}
    }
    if (sp == 0)
	err("unknown baud rate %s", rate);
    return (sp->speed);
}

/*
 * Syntax for -m:
 * [port-type][test baudrate]:terminal-type
 * The baud rate tests are: >, <, @, =, !
 */
static void
add_mapping(const char *port, char *arg)
{
    MAP *mapp;
    char *copy, *p;
    const char *termp;
    char *base = 0;

    copy = strdup(arg);
    mapp = typeMalloc(MAP, 1);
    if (copy == 0 || mapp == 0)
	failed("malloc");

    assert(copy != 0);
    assert(mapp != 0);

    mapp->next = 0;
    if (maplist == 0)
	cur = maplist = mapp;
    else {
	cur->next = mapp;
	cur = mapp;
    }

    mapp->porttype = arg;
    mapp->conditional = 0;

    arg = strpbrk(arg, "><@=!:");

    if (arg == 0) {		/* [?]term */
	mapp->type = mapp->porttype;
	mapp->porttype = 0;
	goto done;
    }

    if (arg == mapp->porttype)	/* [><@=! baud]:term */
	termp = mapp->porttype = 0;
    else
	termp = base = arg;

    for (;; ++arg) {		/* Optional conditionals. */
	switch (*arg) {
	case '<':
	    if (mapp->conditional & GT)
		goto badmopt;
	    mapp->conditional |= LT;
	    break;
	case '>':
	    if (mapp->conditional & LT)
		goto badmopt;
	    mapp->conditional |= GT;
	    break;
	case '@':
	case '=':		/* Not documented. */
	    mapp->conditional |= EQ;
	    break;
	case '!':
	    mapp->conditional |= NOT;
	    break;
	default:
	    goto next;
	}
    }

  next:
    if (*arg == ':') {
	if (mapp->conditional)
	    goto badmopt;
	++arg;
    } else {			/* Optional baudrate. */
	arg = strchr(p = arg, ':');
	if (arg == 0)
	    goto badmopt;
	*arg++ = '\0';
	mapp->speed = tbaudrate(p);
    }

    mapp->type = arg;

    /* Terminate porttype, if specified. */
    if (termp != 0)
	*base = '\0';

    /* If a NOT conditional, reverse the test. */
    if (mapp->conditional & NOT)
	mapp->conditional = ~mapp->conditional & (EQ | GT | LT);

    /* If user specified a port with an option flag, set it. */
  done:
    if (port) {
	if (mapp->porttype) {
	  badmopt:
	    err("illegal -m option format: %s", copy);
	}
	mapp->porttype = port;
    }
    free(copy);
#ifdef MAPDEBUG
    (void) printf("port: %s\n", mapp->porttype ? mapp->porttype : "ANY");
    (void) printf("type: %s\n", mapp->type);
    (void) printf("conditional: ");
    p = "";
    if (mapp->conditional & GT) {
	(void) printf("GT");
	p = "/";
    }
    if (mapp->conditional & EQ) {
	(void) printf("%sEQ", p);
	p = "/";
    }
    if (mapp->conditional & LT)
	(void) printf("%sLT", p);
    (void) printf("\nspeed: %d\n", mapp->speed);
#endif
}

/*
 * Return the type of terminal to use for a port of type 'type', as specified
 * by the first applicable mapping in 'map'.  If no mappings apply, return
 * 'type'.
 */
static const char *
mapped(const char *type)
{
    MAP *mapp;
    int match;

    for (mapp = maplist; mapp; mapp = mapp->next)
	if (mapp->porttype == 0 || !strcmp(mapp->porttype, type)) {
	    switch (mapp->conditional) {
	    case 0:		/* No test specified. */
		match = TRUE;
		break;
	    case EQ:
		match = ((int) ospeed == mapp->speed);
		break;
	    case GE:
		match = ((int) ospeed >= mapp->speed);
		break;
	    case GT:
		match = ((int) ospeed > mapp->speed);
		break;
	    case LE:
		match = ((int) ospeed <= mapp->speed);
		break;
	    case LT:
		match = ((int) ospeed < mapp->speed);
		break;
	    default:
		match = FALSE;
	    }
	    if (match)
		return (mapp->type);
	}
    /* No match found; return given type. */
    return (type);
}

/**************************************************************************
 *
 * Entry fetching
 *
 **************************************************************************/

/*
 * Figure out what kind of terminal we're dealing with, and then read in
 * its termcap entry.
 */
static const char *
get_termcap_entry(int fd, char *userarg)
{
    int errret;
    char *p;
    const char *ttype;
#if HAVE_GETTTYNAM
    struct ttyent *t;
#else
    FILE *fp;
#endif
    char *ttypath;

    (void) fd;

    if (userarg) {
	ttype = userarg;
	goto found;
    }

    /* Try the environment. */
    if ((ttype = getenv("TERM")) != 0)
	goto map;

    if ((ttypath = ttyname(fd)) != 0) {
	p = _nc_basename(ttypath);
#if HAVE_GETTTYNAM
	/*
	 * We have the 4.3BSD library call getttynam(3); that means
	 * there's an /etc/ttys to look up device-to-type mappings in.
	 * Try ttyname(3); check for dialup or other mapping.
	 */
	if ((t = getttynam(p))) {
	    ttype = t->ty_type;
	    goto map;
	}
#else
	if ((fp = fopen("/etc/ttytype", "r")) != 0
	    || (fp = fopen("/etc/ttys", "r")) != 0) {
	    char buffer[BUFSIZ];
	    char *s, *t, *d;

	    while (fgets(buffer, sizeof(buffer) - 1, fp) != 0) {
		for (s = buffer, t = d = 0; *s; s++) {
		    if (isspace(UChar(*s)))
			*s = '\0';
		    else if (t == 0)
			t = s;
		    else if (d == 0 && s != buffer && s[-1] == '\0')
			d = s;
		}
		if (t != 0 && d != 0 && !strcmp(d, p)) {
		    ttype = strdup(t);
		    fclose(fp);
		    goto map;
		}
	    }
	    fclose(fp);
	}
#endif /* HAVE_GETTTYNAM */
    }

    /* If still undefined, use "unknown". */
    ttype = "unknown";

  map:ttype = mapped(ttype);

    /*
     * If not a path, remove TERMCAP from the environment so we get a
     * real entry from /etc/termcap.  This prevents us from being fooled
     * by out of date stuff in the environment.
     */
  found:
    if ((p = getenv("TERMCAP")) != 0 && !_nc_is_abs_path(p)) {
	/* 'unsetenv("TERMCAP")' is not portable.
	 * The 'environ' array is better.
	 */
	int n;
	for (n = 0; environ[n] != 0; n++) {
	    if (!strncmp("TERMCAP=", environ[n], (size_t) 8)) {
		while ((environ[n] = environ[n + 1]) != 0) {
		    n++;
		}
		break;
	    }
	}
    }

    /*
     * ttype now contains a pointer to the type of the terminal.
     * If the first character is '?', ask the user.
     */
    if (ttype[0] == '?') {
	if (ttype[1] != '\0')
	    ttype = askuser(ttype + 1);
	else
	    ttype = askuser(0);
    }
    /* Find the terminfo entry.  If it doesn't exist, ask the user. */
    while (setupterm((NCURSES_CONST char *) ttype, fd, &errret)
	   != OK) {
	if (errret == 0) {
	    (void) fprintf(stderr, "%s: unknown terminal type %s\n",
			   _nc_progname, ttype);
	    ttype = 0;
	} else {
	    (void) fprintf(stderr,
			   "%s: can't initialize terminal type %s (error %d)\n",
			   _nc_progname, ttype, errret);
	    ttype = 0;
	}
	ttype = askuser(ttype);
    }
#if BROKEN_LINKER
    tgetflag("am");		/* force lib_termcap.o to be linked for 'ospeed' */
#endif
    return (ttype);
}

/**************************************************************************
 *
 * Main sequence
 *
 **************************************************************************/

/*
 * Convert the obsolete argument forms into something that getopt can handle.
 * This means that -e, -i and -k get default arguments supplied for them.
 */
static void
obsolete(char **argv)
{
    for (; *argv; ++argv) {
	char *parm = argv[0];

	if (parm[0] == '-' && parm[1] == '\0') {
	    argv[0] = strdup("-q");
	    continue;
	}

	if ((parm[0] != '-')
	    || (argv[1] && argv[1][0] != '-')
	    || (parm[1] != 'e' && parm[1] != 'i' && parm[1] != 'k')
	    || (parm[2] != '\0'))
	    continue;
	switch (argv[0][1]) {
	case 'e':
	    argv[0] = strdup("-e^H");
	    break;
	case 'i':
	    argv[0] = strdup("-i^C");
	    break;
	case 'k':
	    argv[0] = strdup("-k^U");
	    break;
	}
    }
}

static void
print_shell_commands(const char *ttype)
{
    const char *p;
    int len;
    char *var;
    char *leaf;
    /*
     * Figure out what shell we're using.  A hack, we look for an
     * environmental variable SHELL ending in "csh".
     */
    if ((var = getenv("SHELL")) != 0
	&& ((len = (int) strlen(leaf = _nc_basename(var))) >= 3)
	&& !strcmp(leaf + len - 3, "csh"))
	p = "set noglob;\nsetenv TERM %s;\nunset noglob;\n";
    else
	p = "TERM=%s;\n";
    (void) printf(p, ttype);
}

static void
usage(void)
{
#define SKIP(s)			/* nothing */
#define KEEP(s) s "\n"
    static const char msg[] =
    {
	KEEP("")
	KEEP("Options:")
	SKIP("  -a arpanet  (obsolete)")
	KEEP("  -c          set control characters")
	SKIP("  -d dialup   (obsolete)")
	KEEP("  -e ch       erase character")
	KEEP("  -I          no initialization strings")
	KEEP("  -i ch       interrupt character")
	KEEP("  -k ch       kill character")
	KEEP("  -m mapping  map identifier to type")
	SKIP("  -p plugboard (obsolete)")
	KEEP("  -Q          do not output control key settings")
	KEEP("  -q          display term only, do no changes")
	KEEP("  -r          display term on stderr")
	SKIP("  -S          (obsolete)")
	KEEP("  -s          output TERM set command")
	KEEP("  -V          print curses-version")
	KEEP("  -w          set window-size")
	KEEP("")
	KEEP("If neither -c/-w are given, both are assumed.")
    };
#undef KEEP
#undef SKIP
    (void) fprintf(stderr, "Usage: %s [options] [terminal]\n", _nc_progname);
    fputs(msg, stderr);
    ExitProgram(EXIT_FAILURE);
    /* NOTREACHED */
}

static char
arg_to_char(void)
{
    return (char) ((optarg[0] == '^' && optarg[1] != '\0')
		   ? ((optarg[1] == '?') ? '\177' : CTRL(optarg[1]))
		   : optarg[0]);
}

int
main(int argc, char **argv)
{
    int ch, noinit, noset, quiet, Sflag, sflag, showterm;
    const char *ttype;
    int terasechar = -1;	/* new erase character */
    int intrchar = -1;		/* new interrupt character */
    int tkillchar = -1;		/* new kill character */
    int my_fd;
    bool opt_c = FALSE;		/* set control-chars */
    bool opt_w = FALSE;		/* set window-size */
    TTY mode, oldmode;

    _nc_progname = _nc_rootname(*argv);
    obsolete(argv);
    noinit = noset = quiet = Sflag = sflag = showterm = 0;
    while ((ch = getopt(argc, argv, "a:cd:e:Ii:k:m:p:qQrSsVw")) != -1) {
	switch (ch) {
	case 'c':		/* set control-chars */
	    opt_c = TRUE;
	    break;
	case 'a':		/* OBSOLETE: map identifier to type */
	    add_mapping("arpanet", optarg);
	    break;
	case 'd':		/* OBSOLETE: map identifier to type */
	    add_mapping("dialup", optarg);
	    break;
	case 'e':		/* erase character */
	    terasechar = arg_to_char();
	    break;
	case 'I':		/* no initialization strings */
	    noinit = 1;
	    break;
	case 'i':		/* interrupt character */
	    intrchar = arg_to_char();
	    break;
	case 'k':		/* kill character */
	    tkillchar = arg_to_char();
	    break;
	case 'm':		/* map identifier to type */
	    add_mapping(0, optarg);
	    break;
	case 'p':		/* OBSOLETE: map identifier to type */
	    add_mapping("plugboard", optarg);
	    break;
	case 'Q':		/* don't output control key settings */
	    quiet = 1;
	    break;
	case 'q':		/* display term only */
	    noset = 1;
	    break;
	case 'r':		/* display term on stderr */
	    showterm = 1;
	    break;
	case 'S':		/* OBSOLETE: output TERM & TERMCAP */
	    Sflag = 1;
	    break;
	case 's':		/* output TERM set command */
	    sflag = 1;
	    break;
	case 'V':		/* print curses-version */
	    puts(curses_version());
	    ExitProgram(EXIT_SUCCESS);
	case 'w':		/* set window-size */
	    opt_w = TRUE;
	    break;
	case '?':
	default:
	    usage();
	}
    }

    argc -= optind;
    argv += optind;

    if (argc > 1)
	usage();

    if (!opt_c && !opt_w)
	opt_c = opt_w = TRUE;

    my_fd = save_tty_settings(&mode, TRUE);
    oldmode = mode;
#ifdef TERMIOS
    ospeed = (NCURSES_OSPEED) cfgetospeed(&mode);
#elif defined(EXP_WIN32_DRIVER)
    ospeed = 0;
#else
    ospeed = (NCURSES_OSPEED) mode.sg_ospeed;
#endif

    if (same_program(_nc_progname, PROG_RESET)) {
	reset_start(stderr, TRUE, FALSE);
	reset_tty_settings(my_fd, &mode, noset);
    } else {
	reset_start(stderr, FALSE, TRUE);
    }

    ttype = get_termcap_entry(my_fd, *argv);

    if (!noset) {
#if HAVE_SIZECHANGE
	if (opt_w) {
	    set_window_size(my_fd, &lines, &columns);
	}
#endif
	if (opt_c) {
	    set_control_chars(&mode, terasechar, intrchar, tkillchar);
	    set_conversions(&mode);

	    if (!noinit) {
		if (send_init_strings(my_fd, &oldmode)) {
		    (void) putc('\r', stderr);
		    (void) fflush(stderr);
		    (void) napms(1000);		/* Settle the terminal. */
		}
	    }

	    update_tty_settings(&oldmode, &mode);
	}
    }

    if (noset) {
	(void) printf("%s\n", ttype);
    } else {
	if (showterm)
	    (void) fprintf(stderr, "Terminal type is %s.\n", ttype);
	/*
	 * If erase, kill and interrupt characters could have been
	 * modified and not -Q, display the changes.
	 */
	if (!quiet) {
	    print_tty_chars(&oldmode, &mode);
	}
    }

    if (Sflag)
	err("The -S option is not supported under terminfo.");

    if (sflag) {
	print_shell_commands(ttype);
    }

    ExitProgram(EXIT_SUCCESS);
}
