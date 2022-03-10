/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 2017,2018 Free Software Foundation, Inc.                       *
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
 * $Id: picsmap.c,v 1.139 2021/05/08 15:56:05 tom Exp $
 *
 * Author: Thomas E. Dickey
 *
 * A little more interesting than "dots", read a simple image into memory and
 * measure the time taken to paint it normally vs randomly.
 *
 * TODO improve use of rgb-names using tsearch.
 *
 * TODO add option to dump picture in non-optimized mode, e.g., like tput.
 * TODO write cells/second to stderr (or log)
 * TODO write picture left-to-right/top-to-bottom
 * TODO write picture randomly
 * TODO add one-shot option vs repeat-count before exiting
 * TODO add option "-xc" for init_color vs init_extended_color
 * TODO add option "-xa" for init_pair vs alloc_pair
 * TODO use pad to allow pictures larger than screen
 * TODO add option to just use convert (which can scale) vs builtin xbm/xpm.
 * TODO add scr_dump and scr_restore calls
 * TODO add option for assume_default_colors
 */
#include <test.priv.h>

#include <sys/types.h>
#include <sys/stat.h>

#if HAVE_STDINT_H
#include <stdint.h>
#define my_intptr_t	intptr_t
#else
#define my_intptr_t	long
#endif

#if HAVE_TSEARCH
#include <search.h>
#endif

#undef CUR			/* use only the curses interface */

#define  L_BLOCK '['
#define  R_BLOCK ']'

#define  L_CURLY '{'
#define  R_CURLY '}'

#define MaxSCALE	1000	/* input curses ranges 0..1000 */
#define MaxRGB		255	/* output color ranges 0..255 */
#define okCOLOR(n)	((n) >= 0 && (n) < COLORS)
#define okSCALE(n)	((n) >= 0 && (n) <= MaxSCALE)
#define Scaled256(n)	(NCURSES_COLOR_T) (int)(((double)(n) * MaxSCALE) / 255)
#define ScaledColor(n)	(NCURSES_COLOR_T) (int)(((double)(n) * MaxSCALE) / scale)

#ifndef RGB_PATH
#define RGB_PATH "/etc/X11/rgb.txt"
#endif

#include <picsmap.h>

typedef struct {
    size_t file;
    size_t name;
    size_t list;
    size_t data;
    size_t head;
    size_t pair;
    size_t cell;
} HOW_MUCH;

#undef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))

/*
 * tfind will return null on failure, so we map subscripts starting at one.
 */
#define P2I(n) (((int)(my_intptr_t)(n)) - 1)
#define I2P(n) (void *)(my_intptr_t)((n) + 1)

#define pause_curses() if (in_curses) stop_curses()

#define debugmsg if (debugging) logmsg
#define debugmsg2 if (debugging) logmsg2

static GCC_NORETURN void cleanup(int);
static void giveup(const char *fmt, ...) GCC_PRINTFLIKE(1, 2);
static void logmsg(const char *fmt, ...) GCC_PRINTFLIKE(1, 2);
static void logmsg2(const char *fmt, ...) GCC_PRINTFLIKE(1, 2);
static void warning(const char *fmt, ...) GCC_PRINTFLIKE(1, 2);
static int gather_c_values(int);

static FILE *logfp = 0;
static double aspect_ratio = 0.6;
static bool in_curses = FALSE;
static bool debugging = FALSE;
static bool quiet = FALSE;
static int slow_time = -1;
static RGB_NAME *rgb_table;
static RGB_DATA *all_colors;
static HOW_MUCH how_much;

static int reading_last;
static int reading_size;
static FG_NODE *reading_ncols;

#if HAVE_TSEARCH
static void *reading_ntree;
#endif

#if HAVE_ALLOC_PAIR && USE_EXTENDED_COLOR
#define USE_EXTENDED_COLORS 1
static bool use_extended_pairs = FALSE;
static bool use_extended_colors = FALSE;
#else
#define USE_EXTENDED_COLORS 0
#endif

static void
logmsg(const char *fmt, ...)
{
    if (logfp != 0) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(logfp, fmt, ap);
	va_end(ap);
	fputc('\n', logfp);
	fflush(logfp);
    }
}

static void
logmsg2(const char *fmt, ...)
{
    if (logfp != 0) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(logfp, fmt, ap);
	va_end(ap);
	fflush(logfp);
    }
}

static void
close_log(void)
{
    if (logfp != 0) {
	logmsg("Allocations:");
	logmsg("%8ld file", (long) how_much.file);
	logmsg("%8ld name", (long) how_much.name);
	logmsg("%8ld list", (long) how_much.list);
	logmsg("%8ld data", (long) how_much.data);
	logmsg("%8ld head", (long) how_much.head);
	logmsg("%8ld pair", (long) how_much.pair);
	logmsg("%8ld cell", (long) how_much.cell);
	logmsg("%8ld window", LINES * COLS * (long) sizeof(NCURSES_CH_T));
	fclose(logfp);
	logfp = 0;
    }
}

static void
cleanup(int code)
{
    pause_curses();
    close_log();
    ExitProgram(code);
    /* NOTREACHED */
}

static void
failed(const char *msg)
{
    int save = errno;
    perror(msg);
    logmsg("failed with %s", strerror(save));
    cleanup(EXIT_FAILURE);
}

static void
warning(const char *fmt, ...)
{
    if (logfp != 0) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(logfp, fmt, ap);
	va_end(ap);
	fputc('\n', logfp);
	fflush(logfp);
    } else {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	cleanup(EXIT_FAILURE);
    }
}

static void
free_data(char **data)
{
    if (data != 0) {
	free(data[0]);
	free(data);
    }
}

static PICS_HEAD *
free_pics_head(PICS_HEAD * pics)
{
    if (pics != 0) {
	free(pics->fgcol);
	free(pics->cells);
	free(pics->name);
	free(pics);
	pics = 0;
    }
    return pics;
}

static void
begin_c_values(int size)
{
    reading_last = 0;
    reading_size = size;
    reading_ncols = typeCalloc(FG_NODE, size + 1);
    how_much.pair += (sizeof(FG_NODE) * (size_t) size);
    /* black is always the first slot, to work around P2I/I2P logic */
    gather_c_values(0);
}

#if HAVE_TSEARCH
static int
compare_c_values(const void *p, const void *q)
{
    const int a = P2I(p);
    const int b = P2I(q);
    return (reading_ncols[a].fgcol - reading_ncols[b].fgcol);
}

#ifdef DEBUG_TSEARCH
static void
check_c_values(int ln)
{
    static int oops = 5;
    FG_NODE **ft;
    int n;
    for (n = 0; n < reading_last; ++n) {
	ft = tfind(I2P(n), &reading_ntree, compare_c_values);
	if (ft != 0) {
	    int q = P2I(*ft);
	    if (reading_ncols[q].fgcol != reading_ncols[n].fgcol) {
		logmsg("@%d, %d:%d (%d) %d %d fgcol %06X %06X", ln, n,
		       reading_last - 1,
		       reading_size,
		       q, n,
		       reading_ncols[n].fgcol,
		       reading_ncols[q].fgcol);
	    }
	} else {
	    logmsg("@%d, %d:%d (%d) ? %d null %06X", ln, n,
		   reading_last - 1,
		   reading_size,
		   n,
		   reading_ncols[n].fgcol);
	    if (oops-- <= 0)
		return;
	}
    }
}
#else
#define check_c_values(n)	/* nothing */
#endif
#endif

static int
gather_c_values(int fg)
{
    int found = -1;
#if HAVE_TSEARCH
    FG_NODE **ft;
    int next = reading_last;

    reading_ncols[next].fgcol = fg;
    reading_ncols[next].count = 0;

    check_c_values(__LINE__);
    if ((ft = tfind(I2P(next), &reading_ntree, compare_c_values)) != 0) {
	found = P2I(*ft);
    } else {
	if (reading_last + 2 >= reading_size) {
	    int more = ((MAX(reading_last, reading_size) + 2) * 3) / 2;
	    int last = reading_last + 1;
	    FG_NODE *p = typeRealloc(FG_NODE, more, reading_ncols);
	    if (p == 0)
		goto done;

	    reading_size = more;
	    reading_ncols = p;
	    memset(reading_ncols + last, 0,
		   sizeof(FG_NODE) * (size_t) (more - last));
	    check_c_values(__LINE__);
	}
	++reading_last;
	how_much.pair += sizeof(FG_NODE);
	if ((ft = tsearch(I2P(next), &reading_ntree, compare_c_values)) != 0) {
	    found = P2I(*ft);
	    if (found != next)
		logmsg("OOPS expected slot %d, got %d", next, found);
	    debugmsg("allocated color #%d as #%06X", next, fg);
	    check_c_values(__LINE__);
	}
    }
#else
    int n;

    for (n = 0; n < reading_last; ++n) {
	if (reading_ncols[n].fgcol == fg) {
	    found = n;
	    break;
	}
    }
    if (found < 0) {
	if (reading_last + 2 >= reading_size) {
	    int more = ((reading_last + 2) * 3) / 2;
	    FG_NODE *p = typeRealloc(FG_NODE, more, reading_ncols);
	    if (p == 0)
		goto done;

	    how_much.pair -= (sizeof(FG_NODE) * (size_t) reading_size);
	    how_much.pair += (sizeof(FG_NODE) * (size_t) more);
	    reading_size = more;
	    reading_ncols = p;
	    memset(reading_ncols + reading_last, 0,
		   sizeof(FG_NODE) * (size_t) (more - reading_last));
	}
	reading_ncols[reading_last].fgcol = fg;
	found = reading_last++;
    }
#endif
  done:
    return found;
}

static void
finish_c_values(PICS_HEAD * head)
{
    head->colors = reading_last;
    head->fgcol = reading_ncols;

    reading_last = 0;
    reading_size = 0;
    reading_ncols = 0;
}

static void
dispose_c_values(void)
{
#if HAVE_TSEARCH
    if (reading_ntree != 0) {
	int n;
	for (n = 0; n < reading_last; ++n) {
	    tdelete(I2P(n), &reading_ntree, compare_c_values);
	}
	reading_ntree = 0;
    }
#endif
    if (reading_ncols != 0) {
	free(reading_ncols);
	reading_ncols = 0;
    }
    reading_last = 0;
    reading_size = 0;
}

static int
is_file(const char *filename, struct stat *sb)
{
    int result = 0;
    if (stat(filename, sb) == 0
	&& (sb->st_mode & S_IFMT) == S_IFREG
	&& sb->st_size != 0) {
	result = 1;
    }
    debugmsg("is_file(%s) %d", filename, result);
    return result;
}

/*
 * Simplify reading xbm/xpm files by first making an array of lines.  Blank
 * lines are filtered out.
 */
static char **
read_file(const char *filename)
{
    char **result = 0;
    struct stat sb;

    if (!quiet) {
	pause_curses();
	printf("** %s\n", filename);
    }

    if (is_file(filename, &sb)) {
	size_t size = (size_t) sb.st_size;
	char *blob = typeCalloc(char, size + 1);
	bool binary = FALSE;
	unsigned k = 0;

	result = typeCalloc(char *, size + 1);
	how_much.file += ((size + 1) * 2);

	if (blob != 0 && result != 0) {
	    FILE *fp = fopen(filename, "r");
	    if (fp != 0) {
		logmsg("opened %s", filename);

		if (fread(blob, sizeof(char), size, fp) == size) {
		    bool had_line = TRUE;
		    unsigned j;

		    for (j = 0; (size_t) j < size; ++j) {
			if (blob[j] == '\0' ||
			    (UChar(blob[j]) < 32 &&
			     !isspace(UChar(blob[j]))) ||
			    (UChar(blob[j]) >= 128 && UChar(blob[j]) < 160)) {
			    binary = TRUE;
			}
			if (blob[j] == '\n') {
			    blob[j] = '\0';
			    if (k && !binary) {
				debugmsg2("[%5d] %s\n", k, result[k - 1]);
			    }
			    had_line = TRUE;
			} else if (had_line) {
			    had_line = FALSE;
			    result[k++] = blob + j;
			}
		    }
		    result[k] = 0;
		    if (k && !binary) {
			debugmsg2("[%5d] %s\n", k, result[k - 1]);
		    }
		}
		fclose(fp);
	    } else {
		logmsg("cannot open %s", filename);
	    }
	}
	if (k == 0) {
	    debugmsg("...file is empty");
	    free(blob);
	    free(result);
	    result = 0;
	} else if (binary) {
	    debugmsg("...file is non-text");
	}
    }
    return result;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: picsmap [options] [imagefile [...]]"
	,"Read/display one or more xbm/xpm files (possibly use \"convert\")"
	,""
	,"Options:"
	,"  -a ratio     aspect-ratio correction for ImageMagick"
#if HAVE_USE_DEFAULT_COLORS
	,"  -d           invoke use_default_colors"
#endif
	,"  -L           add debugging information to logfile"
	,"  -l logfile   write informational messages to logfile"
	,"  -p palette   color-palette file (default \"$TERM.dat\")"
	,"  -q           less verbose"
	,"  -r rgb-path  xpm uses X rgb color-names (default \"" RGB_PATH "\")"
	,"  -s SECS      pause for SECS seconds after display vs getch"
#if USE_EXTENDED_COLORS
	,"  -x [pc]      use extension (p=extended-pairs, c=extended-colors)"
	,"               Either/both extension may be given"
#endif
    };
    size_t n;

    pause_curses();

    fflush(stdout);
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    cleanup(EXIT_FAILURE);
}

static void
giveup(const char *fmt, ...)
{
    va_list ap;

    pause_curses();
    fflush(stdout);

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    va_end(ap);

    if (logfp) {
	va_start(ap, fmt);
	vfprintf(logfp, fmt, ap);
	fputc('\n', logfp);
	va_end(ap);
	fflush(logfp);
    }

    usage();
}

/*
 * Palette files are named for $TERM values.  However, there are fewer palette
 * files than $TERM's.  Although there are known problems (some cannot even get
 * black and white correct), for the purpose of comparison, pretending that
 * those map into "xterm" is useful.
 */
static char **
read_palette(const char *filename)
{
    static const char *data_dir = DATA_DIR;
    char **result = 0;
    size_t last = strlen(filename);
    size_t need = (strlen(data_dir) + 20 + last);
    char *full_name = malloc(need);
    char *s;
    struct stat sb;

    if (full_name != 0) {
	int tries;
	for (tries = 0; tries < 8; ++tries) {

	    *(s = full_name) = '\0';
	    if (tries & 1) {
		if (strchr(filename, '/') == 0) {
		    _nc_SPRINTF(full_name, _nc_SLIMIT(need) "%s/", data_dir);
		} else {
		    continue;
		}
	    }
	    s += strlen(s);
	    if (((size_t) (s - full_name) + last + 1) >= need)
		continue;

	    _nc_STRCAT(full_name, filename, need);
	    if (tries & 4) {
		char *t = s;
		char *tc;
		int num;
		char chr;
		int found = 0;
		while (*t != '\0') {
		    if (*t == '-') {
			if (sscanf(t, "-%d%c", &num, &chr) == 2 &&
			    chr == 'c' &&
			    (tc = strchr(t, chr)) != 0 &&
			    !(strncmp) (tc, "color", 5)) {
			    found = 1;
			}
			break;
		    }
		    ++t;
		}
		if (found && (t != s)
		    && (strncmp) (s, "xterm", (size_t) (t - s))) {
		    _nc_SPRINTF(s, _nc_SLIMIT(need - (size_t) (s - full_name))
				"xterm%s", filename + (t - s));
		} else {
		    continue;
		}
	    }

	    if (tries & 2) {
		int len = (int) strlen(filename);
		if (len <= 4 || strcmp(filename + len - 4, ".dat")) {
		    _nc_STRCAT(full_name, ".dat", need);
		} else {
		    continue;
		}
	    }
	    if (is_file(full_name, &sb))
		goto ok;
	}
	goto failed;
      ok:
	result = read_file(full_name);
      failed:
	free(full_name);
    }
    return result;
}

static void
init_palette(const char *palette_file)
{
    if (palette_file != 0) {
	char **data = read_palette(palette_file);

	all_colors = typeMalloc(RGB_DATA, (unsigned) COLORS);
	how_much.data += (sizeof(RGB_DATA) * (unsigned) COLORS);

#if HAVE_COLOR_CONTENT
	{
	    int cp;
	    for (cp = 0; cp < COLORS; ++cp) {
		color_content((short) cp,
			      &all_colors[cp].red,
			      &all_colors[cp].green,
			      &all_colors[cp].blue);
	    }
	}
#else
	memset(all_colors, 0, sizeof(RGB_DATA) * (size_t) COLORS);
#endif
	if (data != 0) {
	    int n;
	    int red, green, blue;
	    int scale = MaxSCALE;
	    int c;
	    for (n = 0; data[n] != 0; ++n) {
		if (sscanf(data[n], "scale:%d", &c) == 1) {
		    scale = c;
		} else if (sscanf(data[n], "%d:%d %d %d",
				  &c,
				  &red,
				  &green,
				  &blue) == 4
			   && okCOLOR(c)
			   && okSCALE(red)
			   && okSCALE(green)
			   && okSCALE(blue)) {
		    /* *INDENT-EQLS* */
		    all_colors[c].red   = ScaledColor(red);
		    all_colors[c].green = ScaledColor(green);
		    all_colors[c].blue  = ScaledColor(blue);
		}
	    }
	}
	free_data(data);
	/* *INDENT-EQLS* */
    } else if (COLORS > 1) {
	int power2 = 1;
	int shift = 0;

	while (power2 < COLORS) {
	    ++shift;
	    power2 <<= 1;
	}

	if ((power2 != COLORS) || ((shift % 3) != 0)) {
	    if (all_colors == 0) {
		init_palette(getenv("TERM"));
		if (all_colors == 0) {
		    giveup("With %d colors, you need a palette-file", COLORS);
		}
	    }
	}
    }
}

/*
 * Map the 24-bit RGB value to a color index if using a palette, otherwise to a
 * direct color value.
 */
static int
map_color(int value)
{
    int result = value;

    if (result < 0) {
	result = -1;
    } else {
	/* *INDENT-EQLS* */
	int red   = (value & 0xff0000) >> 16;
	int green = (value & 0x00ff00) >> 8;
	int blue  = (value & 0x0000ff) >> 0;

	if (all_colors != 0) {
#define Diff2(n,m) ((m) - all_colors[n].m) * ((m) - all_colors[n].m)
#define Diff2S(n) Diff2(n,red) + Diff2(n,green) + Diff2(n,blue)
	    int d2 = Diff2S(0);
	    int n;

	    /* *INDENT-EQLS* */
	    red   = Scaled256(red);
	    green = Scaled256(green);
	    blue  = Scaled256(blue);

	    for (result = 0, n = 1; n < COLORS; ++n) {
		int d = Diff2(n, red) + Diff2(n, green) + Diff2(n, blue);
		if (d < d2) {
		    d2 = d;
		    result = n;
		}
	    }
	} else {		/* direct color */
	    int power2 = 1;
	    int shifts = 8;

	    while (power2 < COLORS) {
		power2 <<= 3;
		shifts--;
	    }

	    if (shifts > 0) {
		/* TODO: round up */
		red >>= shifts;
		green >>= shifts;
		blue >>= shifts;
		result = ((red << (2 * (8 - shifts)))
			  + (green << (8 - shifts))
			  + blue);
	    }
	}
    }
    return result;
}

static int
bytes_of(int value)
{
    if (value & 7) {
	value |= 7;
	value++;
    }
    return value;
}

static int match_c(const char *, const char *, ...) GCC_SCANFLIKE(2,3);

static char *
skip_s(char *s)
{
    while (isspace(UChar(*s)))
	s++;
    return s;
}

static const char *
skip_cs(const char *s)
{
    while (isspace(UChar(*s)))
	s++;
    return s;
}

static char *
skip_word(char *s)
{
    s = skip_s(s);
    while (isgraph(UChar(*s)))
	s++;
    return s;
}

static int
match_c(const char *source, const char *pattern, ...)
{
    int limit = (int) strlen(source);
    const char *last_s = source + limit;
    va_list ap;
    int ch;
    int *ip;
    char *cp;
    long lv;

    va_start(ap, pattern);

    limit = -1;
    while (*pattern != '\0') {
	ch = UChar(*pattern++);
	/* blank in the pattern matches zero-or-more blanks in source */
	if (isspace(ch)) {
	    source = skip_cs(source);
	    continue;
	}
	/* %c, %d, %s are like sscanf except for special treatment of blanks */
	if (ch == '%' && *pattern != '\0' && strchr("cdnsx", *pattern)) {
	    bool found = FALSE;
	    ch = *pattern++;
	    switch (ch) {
	    case 'c':
		cp = va_arg(ap, char *);
		do {
		    *cp++ = *source++;
		} while (--limit > 0);
		break;
	    case 'd':
	    case 'x':
		limit = -1;
		ip = va_arg(ap, int *);
		lv = strtol(source, &cp, ch == 'd' ? 10 : 16);
		if (cp != 0 && cp != source) {
		    *ip = (int) lv;
		    source = cp;
		} else {
		    goto finish;
		}
		break;
	    case 'n':
		/* not really sscanf... */
		limit = *va_arg(ap, int *);
		break;
	    case 's':
		limit = -1;
		cp = va_arg(ap, char *);
		while (*source != '\0') {
		    ch = UChar(*source);
		    if (isspace(ch)) {
			break;
		    } else if (found && (ch == *skip_cs(pattern))) {
			break;
		    } else {
			*cp++ = *source++;
			found = TRUE;
		    }
		}
		*cp = '\0';
		break;
	    }
	    continue;
	}
	/* other characters are matched literally */
	if (*source++ != ch) {
	    break;
	}
    }
  finish:

    va_end(ap);
    if (source > last_s)
	source = last_s;
    return (*source || *pattern) ? 0 : 1;
}

static int
match_colors(const char *source, int cpp, char *arg1, char *arg2, char *arg3)
{
    int result = 0;

    /* most files use a quasi-fixed format */
    if (match_c(source, " \"%n%c %s %s \" , ", &cpp, arg1, arg2, arg3)) {
	arg1[cpp] = '\0';
	result = 1;
    } else {
	const char *s = skip_cs(source);
	size_t have = strlen(source);

	if (*s++ == '"' && have > ((size_t) cpp + 2)) {
	    memcpy(arg1, s, (size_t) cpp);
	    s += cpp;
	    while (*s++ == '\t') {
		char *t;
		for (t = arg2; (*s != '\0') && strchr("\t\"", *s) == 0;) {
		    if (*s == ' ') {
			s = skip_cs(s);
			break;
		    }
		    *t++ = *s++;
		    *t = '\0';
		}
		for (t = arg3; (*s != '\0') && strchr("\t\"", *s) == 0;) {
		    *t++ = *s++;
		    *t = '\0';
		}
		if (!strcmp(arg2, "c")) {
		    result = 1;
		    break;
		}
	    }
	}
    }
    return result;
}

static RGB_NAME *
parse_rgb(char **data)
{
    char buf[BUFSIZ];
    int n;
    unsigned long r, g, b;
    char *s, *t;
    size_t item = 0;
    size_t need;
    RGB_NAME *result = 0;

    for (need = 0; data[need] != 0; ++need) ;

    result = typeCalloc(RGB_NAME, need + 2);
    how_much.name += (sizeof(RGB_NAME) * (need + 2));

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(t = data[n]) >= sizeof(buf) - 1)
	    continue;
	if (*(s = skip_s(t)) == '!')
	    continue;

	r = strtoul(s, &t, 10);
	s = skip_s(t);
	g = strtoul(s, &t, 10);
	s = skip_s(t);
	b = strtoul(s, &t, 10);
	s = skip_s(t);

	result[item].name = s;
	t = s + strlen(s);
	while (t-- != s && isspace(UChar(*t))) {
	    *t = '\0';
	}
	result[item].value = (int) ((r & 0xff) << 16 |
				    (g & 0xff) << 8 |
				    (b & 0xff));
	++item;
    }

    result[item].name = "none";
    result[item].value = -1;

    return result;
}

#define LOWERCASE(c) ((isalpha(UChar(c)) && isupper(UChar(c))) ? tolower(UChar(c)) : (c))

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

static RGB_NAME *
lookup_rgb(const char *name)
{
    RGB_NAME *result = 0;
    if (rgb_table != 0) {
	int n;
	for (n = 0; rgb_table[n].name != 0; ++n) {
	    if (!CaselessCmp(name, rgb_table[n].name)) {
		result = &rgb_table[n];
		break;
	    }
	}
    }
    return result;
}

static PICS_HEAD *
parse_xbm(char **data)
{
    int n;
    int state = 0;
    char buf[2048];
    int num;
    char ch;
    char *s;
    char *t;
    PICS_HEAD *result;
    size_t which = 0;
    size_t cells = 0;

    debugmsg("called parse_xbm");

    result = typeCalloc(PICS_HEAD, 1);
    how_much.head += sizeof(PICS_HEAD);

    begin_c_values(2);
    gather_c_values(0);
    gather_c_values(0xffffff);

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(s = data[n]) >= sizeof(buf) - 1)
	    continue;
	switch (state) {
	case 0:
	case 1:
	case 2:
	    if (sscanf(s, "#define %1024s %d%c", buf, &num, &ch) >= 2) {
		if ((t = strstr(buf, "_width")) != 0) {
		    state |= 1;
		    result->wide = (short) bytes_of(num);
		} else if ((t = strstr(buf, "_height")) != 0) {
		    state |= 2;
		    result->high = (short) num;
		} else {
		    break;
		}
		*t = '\0';
		if (result->name) {
		    if (strcmp(result->name, buf)) {
			goto finish;
		    }
		} else {
		    result->name = strdup(buf);
		}
	    }
	    break;
	case 3:
	    if (sscanf(s, "static char %1024[^_ ]_bits[]%c", buf, &ch) >= 1) {
		if (strcmp(result->name, buf)) {
		    goto finish;
		}
		state = 4;
		cells = (size_t) (result->wide * result->high);

		result->cells = typeCalloc(PICS_CELL, cells);
		how_much.cell += (sizeof(PICS_CELL) * cells);

		if ((s = strchr(s, L_CURLY)) == 0)
		    break;
		++s;
	    } else {
		break;
	    }
	case 4:
	    while (*s != '\0') {
		while (isspace(UChar(*s))) {
		    ++s;
		}
		if (isdigit(UChar(*s))) {
		    long value = strtol(s, &t, 0);
		    int b;
		    if (t != s || value > MaxRGB || value < 0) {
			s = t;
		    } else {
			state = -1;
			goto finish;
		    }
		    for (b = 0; b < 8; ++b) {
			if (((1L << b) & value) != 0) {
			    result->cells[which].ch = '*';
			    result->cells[which].fg = 1;
			    reading_ncols[1].count++;
			} else {
			    result->cells[which].ch = ' ';
			    result->cells[which].fg = 0;
			    reading_ncols[0].count++;
			}
			if (++which > cells) {
			    state = -1;
			    goto finish;
			}
		    }
		}
		if (*s == R_CURLY) {
		    state = 5;
		    goto finish;
		} else if (*s == ',') {
		    ++s;
		}
	    }
	    break;
	default:
	    break;
	}
    }
  finish:
    if (state < 4) {
	debugmsg("...state was only %d", state);
	if (result) {
	    result = free_pics_head(result);
	}
    } else {
	finish_c_values(result);
    }
    return result;
}

static PICS_HEAD *
parse_xpm(char **data)
{
    int state = 0;
    PICS_HEAD *result;
    RGB_NAME *by_name;
    int n;
    int cells = 0;
    int cpp = 1;		/* chars per pixel */
    int num[6];
    int found;
    int which = 0;
    int num_colors = 0;
    char ch;
    const char *cs;
    char *s;
    char buf[BUFSIZ];
    char arg1[BUFSIZ];
    char arg2[BUFSIZ];
    char arg3[BUFSIZ];
    char **list = 0;

    debugmsg("called parse_xpm");

    result = typeCalloc(PICS_HEAD, 1);
    how_much.head += sizeof(PICS_HEAD);

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(s = data[n]) >= sizeof(buf) - 1)
	    continue;
	switch (state) {
	case 0:
	    if (match_c(s, " /* XPM */ ")) {
		state = 1;
	    }
	    break;
	case 1:
	    if (match_c(s, " static char * %s [] = %c ", arg1, &ch) &&
		ch == L_CURLY) {
		result->name = strdup(arg1);
		state = 2;
	    }
	    break;
	case 2:
	    if (match_c(s, " \" %d %d %d %d \" , ",
			num + 0, num + 1, num + 2, num + 3) ||
		match_c(s, " \" %d %d %d %d %d %d \" , ",
			num + 0, num + 1, num + 2, num + 3, num + 4, num + 5)) {
		result->wide = (short) num[0];
		result->high = (short) num[1];
		result->colors = num[2];

		begin_c_values(num[2]);

		cells = (result->wide * result->high);

		result->cells = typeCalloc(PICS_CELL, cells);
		how_much.cell += sizeof(PICS_CELL) * (size_t) cells;

		list = typeCalloc(char *, result->colors + 1);
		how_much.list += sizeof(char *) * (size_t) (result->colors + 1);

		cpp = num[3];
		state = 3;
	    }
	    break;
	case 3:
	    if (!match_colors(s, cpp, arg1, arg2, arg3)) {
		break;
	    }
	    num_colors++;
	    free(list[reading_last]);
	    list[reading_last] = strdup(arg1);
	    if ((by_name = lookup_rgb(arg3)) != 0) {
		found = gather_c_values(by_name->value);
	    } else if (*arg3 == '#') {
		char *rgb = arg3 + 1;
		unsigned long value = strtoul(rgb, &s, 16);
		switch ((int) strlen(rgb)) {
		case 6:
		    break;
		case 12:
		    value = (((value >> 24) & 0xff0000L)
			     | ((value >> 16) & 0xff00L)
			     | ((value >> 8) & 0xffL));
		    break;
		default:
		    warning("unexpected rgb value %s", rgb);
		    break;
		}
		found = gather_c_values((int) value);
	    } else {
		found = gather_c_values(0);	/* actually an error */
	    }
	    debugmsg("  [%d:%d] %06X", num_colors, result->colors,
		     reading_ncols[(found >= 0) ? found : 0].fgcol);
	    if (num_colors >= result->colors) {
		finish_c_values(result);
		state = 4;
		if (list[0] == 0)
		    list[0] = strdup("\033");
	    }
	    break;
	case 4:
	    if (*(cs = skip_cs(s)) == '"') {
		++cs;
		while (*cs != '\0' && *cs != '"') {
		    int c;

		    /* FIXME - factor out */
		    for (c = 0; c < result->colors; ++c) {
			if (list[c] == 0) {
			    /* should not happen... */
			    continue;
			}
			if (!(strncmp) (cs, list[c], (size_t) cpp)) {
			    result->cells[which].ch = list[c][0];
			    result->cells[which].fg = c;
			    result->fgcol[c].count++;
			    break;
			}
		    }

		    if (result->cells[which].ch == 0) {
			result->cells[which].ch = '?';
			result->cells[which].fg = 0;
		    }

		    if (++which >= cells) {
			state = 5;
			break;
		    }
		    for (c = cpp; c > 0; --c, ++cs) {
			if (*cs == '\0')
			    break;
		    }
		}
	    }
	    break;
	}
    }

    if (result && list) {
	for (n = 0; n < result->colors; ++n)
	    free(list[n]);
	free(list);
    }

    if (state < 5) {
	debugmsg("...state was only %d", state);
	result = free_pics_head(result);
    }

    if (result) {
	debugmsg("...allocated %d colors", result->colors);
    }

    return result;
}

/*
 * The obscurely-named "convert" is provided by ImageMagick
 */
static PICS_HEAD *
parse_img(const char *filename)
{
    size_t need = strlen(filename) + 256;
    char *cmd = malloc(need);
    FILE *pp;
    char buffer[BUFSIZ];
    char dummy[BUFSIZ];
    bool okay = TRUE;
    PICS_HEAD *result;
    int pic_x = 0;
    int pic_y = 0;
    int width = in_curses ? COLS : 80;

    _nc_SPRINTF(cmd, _nc_SLIMIT(need) "identify \"%s\"", filename);
    if (quiet)
	_nc_STRCAT(cmd, " 2>/dev/null", need);

    logmsg("...opening pipe to %s", cmd);

    result = typeCalloc(PICS_HEAD, 1);
    how_much.head += sizeof(PICS_HEAD);

    if ((pp = popen(cmd, "r")) != 0) {
	if (fgets(buffer, sizeof(buffer), pp) != 0) {
	    size_t n = strlen(filename);
	    debugmsg2("...read %s", buffer);
	    if (strlen(buffer) > n &&
		!(strncmp) (buffer, filename, n) &&
		isspace(UChar(buffer[n])) &&
		sscanf(skip_word(buffer + n), " %dx%d ", &pic_x, &pic_y) == 2) {
		/* distort image to make it show normally on terminal */
		pic_x = (int) ((double) pic_x / aspect_ratio);
	    } else {
		pic_x = pic_y = 0;
	    }
	}
	pclose(pp);
    }
    if (pic_x <= 0 || pic_y <= 0)
	goto finish;

    _nc_SPRINTF(cmd, _nc_SLIMIT(need)
		"convert " "-resize %dx%d\\! " "-thumbnail %dx \"%s\" "
		"-define txt:compliance=SVG txt:-",
		pic_x, pic_y, width, filename);
    if (quiet)
	_nc_STRCAT(cmd, " 2>/dev/null", need);

    logmsg("...opening pipe to %s", cmd);
    if ((pp = popen(cmd, "r")) != 0) {
	int count = 0;
	int col = 0;
	int row = 0;
	int len = 0;
	while (fgets(buffer, sizeof(buffer), pp) != 0) {
	    debugmsg2("[%5d] %s", count + 1, buffer);
	    if (strlen(buffer) > 160) {		/* 80 columns would be enough */
		okay = FALSE;
		break;
	    }
	    if (count++ == 0) {
		if (match_c(buffer,
			    "# ImageMagick pixel enumeration: %d,%d,%d,%s ",
			    &col, &row, &len, dummy)) {
		    result->name = strdup(filename);
		    result->wide = (short) col;
		    result->high = (short) row;

		    begin_c_values(256);

		    result->cells = typeCalloc(PICS_CELL, (size_t) (col * row));
		    how_much.cell += (sizeof(PICS_CELL) * (size_t) (col * row));
		} else {
		    okay = FALSE;
		    break;
		}
	    } else {
		/* subsequent lines begin "col,row: (r,g,b,a) #RGB" */
		int r, g, b, nocolor;
		unsigned check;
		char *t;
		char *s = t = strchr(buffer, '#');

		if (s != 0) {
		    /* after the "#RGB", there are differences - just ignore */
		    while (*s != '\0' && !isspace(UChar(*s)))
			++s;
		    *++s = '\0';
		}
		if (match_c(buffer,
			    "%d,%d: (%d,%d,%d,%d) #%x ",
			    &col, &row,
			    &r, &g, &b, &nocolor,
			    &check)) {
		    int which, c;

		    if ((s - t) > 8)	/* 6 hex digits vs 8 */
			check /= 256;
		    if (r > MaxRGB ||
			g > MaxRGB ||
			b > MaxRGB ||
			check != (unsigned) ((r << 16) | (g << 8) | b)) {
			okay = FALSE;
			break;
		    }
		    c = gather_c_values((int) check);
		    which = col + (row * result->wide);
		    result->cells[which].ch = ((in_curses ||
						check == 0xffffff)
					       ? ' '
					       : '#');
		    if (c >= 0 && c < reading_last) {
			result->cells[which].fg = c;
			reading_ncols[c].count++;
		    } else {
			result->cells[which].fg = -1;
		    }
		} else {
		    okay = FALSE;
		    break;
		}
	    }
	}
	finish_c_values(result);
	pclose(pp);
	if (okay) {
	    /* FIXME - is this trimming needed? */
	    for (len = result->colors; len > 3; len--) {
		if (result->fgcol[len - 1].fgcol == 0) {
		    result->colors = len - 1;
		} else {
		    break;
		}
	    }
	}
    }
  finish:
    free(cmd);

    if (!okay) {
	result = free_pics_head(result);
    }

    return result;
}

static PICS_HEAD *
read_picture(const char *filename, char **data)
{
    PICS_HEAD *pics;
    if ((pics = parse_xbm(data)) == 0) {
	dispose_c_values();
	if ((pics = parse_xpm(data)) == 0) {
	    dispose_c_values();
	    if ((pics = parse_img(filename)) == 0) {
		dispose_c_values();
		free_data(data);
		warning("unexpected file-format for \"%s\"", filename);
	    } else if (pics->high == 0 || pics->wide == 0) {
		dispose_c_values();
		free_data(data);
		pics = free_pics_head(pics);
		warning("no picture found in \"%s\"", filename);
	    }
	}
    }
    return pics;
}

#define fg_color(pics,n) (pics->fgcol[n].fgcol)

static void
dump_picture(PICS_HEAD * pics)
{
    int y, x;

    printf("Name %s\n", pics->name);
    printf("Size %dx%d\n", pics->high, pics->wide);
    printf("Color\n");
    for (y = 0; y < pics->colors; ++y) {
	if (fg_color(pics, y) < 0) {
	    printf(" %3d: %d\n", y, fg_color(pics, y));
	} else {
	    printf(" %3d: #%06x\n", y, fg_color(pics, y));
	}
    }
    for (y = 0; y < pics->high; ++y) {
	for (x = 0; x < pics->wide; ++x) {
	    putchar(pics->cells[y * pics->wide + x].ch);
	}
	putchar('\n');
    }
}

#ifndef USE_DISPLAY_DRIVER
static void
init_display(const char *palette_path, int opt_d)
{
    (void) opt_d;
    if (isatty(fileno(stdout))) {
	in_curses = TRUE;
	initscr();
	cbreak();
	noecho();
	curs_set(0);
	if (has_colors()) {
	    start_color();
#if HAVE_USE_DEFAULT_COLORS
	    if (opt_d)
		use_default_colors();
#endif
	    init_palette(palette_path);
	}
	scrollok(stdscr, FALSE);
	stop_curses();
    }
}

static void
show_picture(PICS_HEAD * pics)
{
    int y, x;
    int n;

    debugmsg("called show_picture");
    logmsg("...using %dx%d screen", LINES, COLS);
#if HAVE_RESET_COLOR_PAIRS
    reset_color_pairs();
#elif HAVE_CURSCR
    wclear(curscr);
    clear();
#endif
    if (has_colors()) {
	logmsg("...using %d colors", pics->colors);
	for (n = 0; n < pics->colors; ++n) {
	    int my_pair = (n + 1);
	    int my_color = map_color(fg_color(pics, n));
#if USE_EXTENDED_COLORS
	    if (use_extended_pairs) {
		init_extended_pair(my_pair, my_color, my_color);
	    } else
#endif
	    {
		my_pair &= 0x7fff;
		my_color &= 0x7fff;
		init_pair((short) my_pair, (short) my_color, (short) my_color);
	    }
	}
	attrset(COLOR_PAIR(1));
	erase();
    }
    for (y = 0; y < pics->high; ++y) {
	if (y >= LINES)
	    break;
	move(y, 0);

	for (x = 0; x < pics->wide; ++x) {
	    int my_pair;

	    if (x >= COLS)
		break;
	    n = (y * pics->wide + x);
	    my_pair = pics->cells[n].fg + 1;
#if USE_EXTENDED_COLORS
	    if (use_extended_pairs) {
		cchar_t temp;
		wchar_t wch[2];
		wch[0] = (wchar_t) pics->cells[n].ch;
		wch[1] = 0;
		setcchar(&temp, wch, A_NORMAL, (short) my_pair, &my_pair);
		add_wch(&temp);
	    } else
#endif
	    {
		attrset(COLOR_PAIR(my_pair));
		addch((chtype) pics->cells[n].ch);
	    }
	}
    }
    if (slow_time >= 0) {
	refresh();
	if (slow_time > 0) {
#ifdef NCURSES_VERSION
	    napms(1000 * slow_time);
#else
	    sleep((unsigned) slow_time);
#endif
	}
    } else {
	wmove(stdscr, 0, 0);
	getch();
    }
    if (!quiet)
	endwin();
}
#endif

static int
compare_fg_counts(const void *a, const void *b)
{
    const FG_NODE *p = (const FG_NODE *) a;
    const FG_NODE *q = (const FG_NODE *) b;
    return (q->count - p->count);
}

static void
report_colors(PICS_HEAD * pics)
{
    int accum;
    double level;
    int j;
    int shift;
    int total;
    char buffer[256];

    if (logfp == 0)
	return;

    qsort(pics->fgcol, (size_t) pics->colors, sizeof(FG_NODE), compare_fg_counts);
    /*
     * For debugging, show a (short) list of the colors used.
     */
    if (debugging && (pics->colors < 1000)) {
	int digits = 0;
	int high;
	int wide = 4;
	for (j = pics->colors; j != 0; j /= 10) {
	    ++digits;
	    if (j < 10)
		++digits;
	}
	if (digits > 8)
	    digits = 8;
	logmsg("These colors were used:");
	high = (pics->colors + wide - 1) / wide;
	for (j = 0; j < high && j < pics->colors; ++j) {
	    int k;
	    char *s = buffer;
	    *s = '\0';
	    for (k = 0; k < wide; ++k) {
		int n = j + (k * high);
		size_t want = (sizeof(buffer) - (size_t) (s - buffer));
		if (want < 100 || want >= sizeof(buffer))
		    break;
		if (n >= pics->colors)
		    break;
		if (k) {
		    *s++ = ' ';
		    if (digits < 8) {
			_nc_SPRINTF(s, _nc_SLIMIT(want) "%*s", 8 - digits,
				    " ");
			s += strlen(s);
		    }
		}
		if (pics->fgcol[n].fgcol >= 0) {
		    _nc_SPRINTF(s, _nc_SLIMIT(want) "%3d #%06X %*d", n,
				pics->fgcol[n].fgcol,
				digits, pics->fgcol[n].count);
		} else {
		    _nc_SPRINTF(s, _nc_SLIMIT(want) "%3d (empty) %*d", n,
				digits, pics->fgcol[n].count);
		}
		s += strlen(s);
		if ((s - buffer) > 100)
		    break;
	    }
	    logmsg("%s", buffer);
	}
    }

    /*
     * Given the list of colors sorted by the number of times they are used,
     * log a short report showing the number of colors for 90%, 99%, 99.9%,
     * etc.
     */
    logmsg("Number of colors versus number of cells");
    total = pics->high * pics->wide;
    accum = 0;
    level = 0.1;
    shift = 1;
    for (j = 0; j < pics->colors; ++j) {
	accum += pics->fgcol[j].count;
	if (accum >= (total * (1.0 - level))) {
	    int after = (shift > 2) ? shift - 2 : 0;
	    logmsg("%8d colors (%.1f%%) in %d cells (%.*f%%)",
		   j + 1,
		   (100.0 * (j + 1)) / pics->colors,
		   accum,
		   after, (100.0 * accum) / total);
	    if (accum >= total)
		break;
	    level /= 10.0;
	    shift++;
	}
    }
}

int
main(int argc, char *argv[])
{
    int n;
    int opt_d = FALSE;
    char ignore_ch;
    const char *palette_path = 0;
    const char *rgb_path = RGB_PATH;

    while ((n = getopt(argc, argv, "a:dLl:p:qr:s:x:")) != -1) {
	switch (n) {
	case 'a':
	    if (sscanf(optarg, "%lf%c", &aspect_ratio, &ignore_ch) != 1
		|| aspect_ratio < 0.1
		|| aspect_ratio > 10.) {
		fprintf(stderr, "Expected a number in [0.1 to 10.]: %s\n", optarg);
		usage();
	    }
	    break;
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    opt_d = TRUE;
	    break;
#endif
	case 'L':
	    debugging = TRUE;
	    break;
	case 'l':
	    if ((logfp = fopen(optarg, "a")) == 0)
		failed(optarg);
	    break;
	case 'p':
	    palette_path = optarg;
	    break;
	case 'q':
	    quiet = TRUE;
	    break;
	case 'r':
	    rgb_path = optarg;
	    break;
	case 's':
	    slow_time = atoi(optarg);
	    break;
#if USE_EXTENDED_COLORS
	case 'x':
	    {
		char *s = optarg;
		while (*s) {
		    switch (*s++) {
		    case 'p':
			use_extended_pairs = TRUE;
			break;
		    case 'c':
			use_extended_colors = TRUE;
			break;
		    default:
			usage();
			break;
		    }
		}
	    }
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }

    if (optind < argc) {
	char **rgb_data = read_file(rgb_path);

	if (rgb_data)
	    rgb_table = parse_rgb(rgb_data);

	init_display(palette_path, opt_d);
	if (optind >= argc)
	    giveup("expected at least one image filename");

	for (n = optind; n < argc; ++n) {
	    PICS_HEAD *pics;
	    char **data = read_file(argv[n]);

	    if (data == 0) {
		warning("cannot read \"%s\"", argv[n]);
		continue;
	    }
	    if ((pics = read_picture(argv[n], data)) != 0) {
		if (in_curses) {
		    show_picture(pics);
		} else {
		    dump_picture(pics);
		}
		report_colors(pics);
		dispose_c_values();
		free_data(data);
		free_pics_head(pics);
	    }
	}
	free_data(rgb_data);
	free(rgb_table);
	free(all_colors);
    } else {
	usage();
    }

    cleanup(EXIT_SUCCESS);
}
