/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2014,2017 Free Software Foundation, Inc.                  *
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
 *	Name: Towers of Hanoi.
 *
 *	Desc:
 *		This is a playable copy of towers of hanoi.
 *		Its sole purpose is to demonstrate my Amiga Curses package.
 *		This program should compile on any system that has Curses.
 *		'hanoi'		will give a manual game with 7 playing pieces.
 *		'hanoi n'	will give a manual game with n playing pieces.
 *		'hanoi n a' will give an auto solved game with n playing pieces.
 *
 *	Author: Simon J Raybould	(sie@fulcrum.bt.co.uk).
 * 	(This version has been slightly modified by the ncurses maintainers.)
 *
 *	Date: 05.Nov.90
 *
 * $Id: hanoi.c,v 1.42 2021/05/08 20:44:44 tom Exp $
 */

#include <test.priv.h>
#include <math.h>

#define NPEGS			3	/* This is not configurable !! */
#define MINTILES		3
#define MAXTILES		9
#define DEFAULTTILES		7
#define TOPLINE			6
#define BASELINE		16
#define STATUSLINE		(LINES-3)
#define LEFTPEG			19
#define MIDPEG			39
#define RIGHTPEG		59

#define LENTOIND(x)		(((int)(x)-1)/2)
#define OTHER(a,b)		(3-((a)+(b)))

struct Peg {
    size_t Length[MAXTILES];
    int Count;
};

static struct Peg Pegs[NPEGS];
static int PegPos[] =
{
    LEFTPEG,
    MIDPEG,
    RIGHTPEG
};
static short TileColour[] =
{
    COLOR_GREEN,		/* Length 3 */
    COLOR_MAGENTA,		/* Length 5 */
    COLOR_RED,			/* Length 7 */
    COLOR_BLUE,			/* Length 9 */
    COLOR_CYAN,			/* Length 11 */
    COLOR_YELLOW,		/* Length 13 */
    COLOR_GREEN,		/* Length 15 */
    COLOR_MAGENTA,		/* Length 17 */
    COLOR_RED,			/* Length 19 */
};
static int NTiles = 0;
static int NMoves = 0;
static bool AutoFlag = FALSE;

static int
InvalidMove(int From, int To)
{
    if (From >= NPEGS)
	return TRUE;
    if (From < 0)
	return TRUE;
    if (To >= NPEGS)
	return TRUE;
    if (To < 0)
	return TRUE;
    if (From == To)
	return TRUE;
    if (!Pegs[From].Count)
	return TRUE;
    if (Pegs[To].Count &&
	Pegs[From].Length[Pegs[From].Count - 1] >
	Pegs[To].Length[Pegs[To].Count - 1])
	return TRUE;
    return FALSE;
}

static void
InitTiles(void)
{
    int Size, SlotNo;

    for (Size = NTiles * 2 + 1, SlotNo = 0; Size >= 3; Size -= 2)
	Pegs[0].Length[SlotNo++] = (size_t) Size;

    Pegs[0].Count = NTiles;
    Pegs[1].Count = 0;
    Pegs[2].Count = 0;
}

static void
DisplayTiles(void)
{
    int Line, peg, SlotNo;
    char TileBuf[BUFSIZ];

    erase();
    MvAddStr(1, 24, "T O W E R S   O F   H A N O I");
    MvAddStr(3, 34, "SJR 1990");
    MvPrintw(19, 5, "Moves : %d of %.0f", NMoves, pow(2.0, (float) NTiles) - 1);
    (void) attrset(A_REVERSE);
    MvAddStr(BASELINE, 8,
	     "                                                               ");

    for (Line = TOPLINE; Line < BASELINE; Line++) {
	MvAddCh(Line, LEFTPEG, ' ');
	MvAddCh(Line, MIDPEG, ' ');
	MvAddCh(Line, RIGHTPEG, ' ');
    }
    MvAddCh(BASELINE, LEFTPEG, '1');
    MvAddCh(BASELINE, MIDPEG, '2');
    MvAddCh(BASELINE, RIGHTPEG, '3');
    (void) attrset(A_NORMAL);

    /* Draw tiles */
    for (peg = 0; peg < NPEGS; peg++) {
	for (SlotNo = 0; SlotNo < Pegs[peg].Count; SlotNo++) {
	    size_t len = Pegs[peg].Length[SlotNo];
	    if (len < sizeof(TileBuf) - 1 && len < (size_t) PegPos[peg]) {
		memset(TileBuf, ' ', len);
		TileBuf[len] = '\0';
		if (has_colors())
		    (void) attrset(AttrArg(COLOR_PAIR(LENTOIND(len)), 0));
		else
		    (void) attrset(A_REVERSE);
		MvAddStr(BASELINE - (SlotNo + 1),
			 (PegPos[peg] - (int) len / 2),
			 TileBuf);
	    }
	}
    }
    (void) attrset(A_NORMAL);
    refresh();
}

static int
GetMove(int *From, int *To)
{
    MvAddStr(STATUSLINE, 0, "Next move ('q' to quit) from ");
    clrtoeol();
    refresh();
    if ((*From = getch()) == 'q')
	return TRUE;
    *From -= ('0' + 1);
    addstr(" to ");
    clrtoeol();
    refresh();

    if ((*To = getch()) == 'q')
	return TRUE;
    *To -= ('0' + 1);
    refresh();
    if (!AutoFlag)
	napms(500);

    move(STATUSLINE, 0);
    clrtoeol();
    refresh();
    return FALSE;
}

static void
MakeMove(int From, int To)
{
    Pegs[From].Count--;
    Pegs[To].Length[Pegs[To].Count] = Pegs[From].Length[Pegs[From].Count];
    Pegs[To].Count++;
    NMoves++;
    DisplayTiles();
}

static void
AutoMove(int From, int To, int Num)
{
    if (Num == 1) {
	MakeMove(From, To);
	napms(500);
    } else {
	AutoMove(From, OTHER(From, To), Num - 1);
	MakeMove(From, To);
	napms(500);
	AutoMove(OTHER(From, To), To, Num - 1);
    }
}

static int
Solved(int NumTiles)
{
    int i;

    for (i = 1; i < NPEGS; i++)
	if (Pegs[i].Count == NumTiles)
	    return TRUE;
    return FALSE;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: hanoi [options] [[<No Of Tiles>] [a]]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors"
#endif
	," -n NUM   set number of tiles (positional param is deprecated)"
	," -X       solve automatically (positional \"a\" is deprecated)"
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    int ch, FromCol, ToCol;

#if HAVE_USE_DEFAULT_COLORS
    bool d_option = FALSE;
#endif

    NTiles = DEFAULTTILES;
    while ((ch = getopt(argc, argv, "dn:X")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
	case 'n':
	    NTiles = atoi(optarg);
	    break;
	case 'X':
	    AutoFlag = TRUE;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    setlocale(LC_ALL, "");

    switch (argc - optind) {
    case 2:
	if (strcmp(argv[optind + 1], "a")) {
	    usage();
	}
	AutoFlag = TRUE;
	/* FALLTHRU */
    case 1:
	NTiles = atoi(argv[optind]);
	/* FALLTHRU */
    case 0:
	break;
    default:
	usage();
    }

    if (NTiles > MAXTILES || NTiles < MINTILES) {
	fprintf(stderr, "Range %d to %d\n", MINTILES, MAXTILES);
	usage();
    }

    initscr();
    if (has_colors()) {
	int i;
	short bg = COLOR_BLACK;
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() == OK))
	    bg = -1;
#endif
	for (i = 0; i < 9; i++)
	    init_pair((short) (i + 1), bg, TileColour[i]);
    }
    cbreak();
    if (LINES < 24) {
	endwin();
	fprintf(stderr, "Min screen length 24 lines\n");
	ExitProgram(EXIT_FAILURE);
    }
    if (AutoFlag) {
	curs_set(0);
	leaveok(stdscr, TRUE);	/* Attempt to remove cursor */
    }
    InitTiles();
    DisplayTiles();
    if (AutoFlag) {
	do {
	    noecho();
	    AutoMove(0, 2, NTiles);
	} while (!Solved(NTiles));
	sleep(2);
    } else {
	echo();
	for (;;) {
	    if (GetMove(&FromCol, &ToCol))
		break;
	    if (InvalidMove(FromCol, ToCol)) {
		MvAddStr(STATUSLINE, 0, "Invalid Move !!");
		refresh();
		beep();
		continue;
	    }
	    MakeMove(FromCol, ToCol);
	    if (Solved(NTiles)) {
		MvPrintw(STATUSLINE, 0,
			 "Well Done !! You did it in %d moves", NMoves);
		refresh();
		sleep(5);
		break;
	    }
	}
    }
    stop_curses();
    ExitProgram(EXIT_SUCCESS);
}
