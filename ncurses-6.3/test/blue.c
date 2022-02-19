/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
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
/*****************************************************************************
 *                                                                           *
 *                         B l u e   M o o n                                 *
 *                         =================                                 *
 *                               V2.2                                        *
 *                   A patience game by T.A.Lister                           *
 *            Integral screen support by Eric S. Raymond                     *
 *                                                                           *
 *****************************************************************************/

/*
 * $Id: blue.c,v 1.54 2021/03/20 16:06:15 tom Exp $
 */

#include <test.priv.h>

#include <time.h>

#if HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

#define NOCARD		(-1)

#define ACE		0
#define KING		12
#define SUIT_LENGTH	13

#define HEARTS		0
#define SPADES		1
#define DIAMONDS	2
#define CLUBS		3
#define NSUITS		4

#define GRID_WIDTH	14	/*    13+1  */
#define GRID_LENGTH	56	/* 4*(13+1) */
#define PACK_SIZE	52

#define BASEROW		1
#define PROMPTROW	11

#define RED_ON_WHITE    1
#define BLACK_ON_WHITE  2
#define BLUE_ON_WHITE   3

static GCC_NORETURN void die(int onsig);

static int deck_size = PACK_SIZE;	/* initial deck */
static int deck[PACK_SIZE];

static int grid[GRID_LENGTH];	/* card layout grid */
static int freeptr[4];		/* free card space pointers */

static int deal_number = 0;

static chtype ranks[SUIT_LENGTH][2] =
{
    {' ', 'A'},
    {' ', '2'},
    {' ', '3'},
    {' ', '4'},
    {' ', '5'},
    {' ', '6'},
    {' ', '7'},
    {' ', '8'},
    {' ', '9'},
    {'1', '0'},
    {' ', 'J'},
    {' ', 'Q'},
    {' ', 'K'}
};

static int letters[4] =
{
    'h',			/* hearts */
    's',			/* spades */
    'd',			/* diamonds */
    'c',			/* clubs */
};

#if HAVE_LANGINFO_CODESET

#if HAVE_TIGETSTR
static int glyphs[] =
{
    '\003',			/* hearts */
    '\006',			/* spades */
    '\004',			/* diamonds */
    '\005',			/* clubs */
};
#endif

#if USE_WIDEC_SUPPORT
static int uglyphs[] =
{
    0x2665,			/* hearts */
    0x2660,			/* spades */
    0x2666,			/* diamonds */
    0x2663			/* clubs */
};
#endif
#endif /* HAVE_LANGINFO_CODESET */

static int *suits = letters;	/* this may change to glyphs below */

static void
die(int onsig)
{
    (void) signal(onsig, SIG_IGN);
    endwin();
    ExitProgram(EXIT_SUCCESS);
}

static void
init_vars(void)
{
    int i;

    deck_size = PACK_SIZE;
    for (i = 0; i < PACK_SIZE; i++)
	deck[i] = i;
    for (i = 0; i < 4; i++)
	freeptr[i] = i * GRID_WIDTH;
}

static void
shuffle(int size)
{
    int numswaps, swapnum;

    numswaps = size * 10;	/* an arbitrary figure */

    for (swapnum = 0; swapnum < numswaps; swapnum++) {
	int i = rand() % size;
	int j = rand() % size;
	int temp = deck[i];
	deck[i] = deck[j];
	deck[j] = temp;
    }
}

static void
deal_cards(void)
{
    int card = 0, value, csuit, crank, suit, aces[4];

    memset(aces, 0, sizeof(aces));
    for (suit = HEARTS; suit <= CLUBS; suit++) {
	int ptr = freeptr[suit];
	grid[ptr++] = NOCARD;	/* 1st card space is blank */
	while ((ptr % GRID_WIDTH) != 0) {
	    value = deck[card++];
	    crank = value % SUIT_LENGTH;
	    csuit = value / SUIT_LENGTH;
	    if (crank == ACE)
		aces[csuit] = ptr;
	    grid[ptr++] = value;
	}
    }

    if (deal_number == 1)	/* shift the aces down to the 1st column */
	for (suit = HEARTS; suit <= CLUBS; suit++) {
	    grid[suit * GRID_WIDTH] = suit * SUIT_LENGTH;
	    grid[aces[suit]] = NOCARD;
	    freeptr[suit] = aces[suit];
	}
}

static void
printcard(int value)
{
    AddCh(' ');
    if (value == NOCARD) {
	(void) addstr("   ");
    } else {
	int which = (value / SUIT_LENGTH);
	int isuit = (value % SUIT_LENGTH);
	chtype color = (chtype) COLOR_PAIR(((which % 2) == 0)
					   ? RED_ON_WHITE
					   : BLACK_ON_WHITE);

	AddCh(ranks[isuit][0] | (chtype) COLOR_PAIR(BLUE_ON_WHITE));
	AddCh(ranks[isuit][1] | (chtype) COLOR_PAIR(BLUE_ON_WHITE));

#ifdef NCURSES_VERSION
	(attron) ((int) color);	/* quieter compiler warnings */
#else
	attron(color);		/* PDCurses, etc., either no macro or wrong */
#endif
#if USE_WIDEC_SUPPORT
	{
	    wchar_t values[2];
	    values[0] = (wchar_t) suits[which];
	    values[1] = 0;
	    addwstr(values);
	}
#else
	AddCh(suits[which]);
#endif
#ifdef NCURSES_VERSION
	(attroff) ((int) color);
#else
	attroff(color);
#endif
    }
    AddCh(' ');
}

static void
display_cards(int deal)
{
    int row, card;

    clear();
    (void) printw(
		     "Blue Moon 2.1 - by Tim Lister & Eric Raymond - Deal %d.\n",
		     deal);
    for (row = HEARTS; row <= CLUBS; row++) {
	move(BASEROW + row + row + 2, 1);
	for (card = 0; card < GRID_WIDTH; card++)
	    printcard(grid[row * GRID_WIDTH + card]);
    }

    move(PROMPTROW + 2, 0);
    refresh();
#define P(x)	(void)printw("%s\n", x)
    P("   This 52-card solitaire starts with  the entire deck shuffled and dealt");
    P("out in four rows.  The aces are then moved to the left end of the layout,");
    P("making 4 initial free spaces.  You may move to a space only the card that");
    P("matches the left neighbor in suit, and is one greater in rank.  Kings are");
    P("high, so no cards may be placed to their right (they create dead spaces).");
    P("  When no moves can be made,  cards still out of sequence are  reshuffled");
    P("and dealt face up after the ends of the partial sequences, leaving a card");
    P("space after each sequence, so that each row looks like a partial sequence");
    P("followed by a space, followed by enough cards to make a row of 14.       ");
    P("  A moment's reflection will show that this game cannot take more than 13");
    P("deals. A good score is 1-3 deals, 4-7 is average, 8 or more is poor.     ");
#undef P
    refresh();
}

static int
find(int card)
{
    int i;

    if ((card < 0) || (card >= PACK_SIZE))
	return (NOCARD);
    for (i = 0; i < GRID_LENGTH; i++)
	if (grid[i] == card)
	    return i;
    return (NOCARD);
}

static void
movecard(int src, int dst)
{
    grid[dst] = grid[src];
    grid[src] = NOCARD;

    move(BASEROW + (dst / GRID_WIDTH) * 2 + 2, (dst % GRID_WIDTH) * 5 + 1);
    printcard(grid[dst]);

    move(BASEROW + (src / GRID_WIDTH) * 2 + 2, (src % GRID_WIDTH) * 5 + 1);
    printcard(grid[src]);

    refresh();
}

static void
play_game(void)
{
    int dead = 0, i, j;
    char c;
    int selection[4], card;

    while (dead < 4) {
	dead = 0;
	for (i = 0; i < 4; i++) {
	    card = grid[freeptr[i] - 1];

	    if (((card % SUIT_LENGTH) == KING)
		||
		(card == NOCARD))
		selection[i] = NOCARD;
	    else
		selection[i] = find(card + 1);

	    if (selection[i] == NOCARD)
		dead++;
	};

	if (dead < 4) {
	    char live[NSUITS + 1], *lp = live;

	    for (i = 0; i < 4; i++) {
		if (selection[i] != NOCARD) {
		    move(BASEROW + (selection[i] / GRID_WIDTH) * 2 + 3,
			 (selection[i] % GRID_WIDTH) * 5);
		    (void) printw("   %c ", (*lp++ = (char) ('a' + i)));
		}
	    };
	    *lp = '\0';

	    if (strlen(live) == 1) {
		move(PROMPTROW, 0);
		(void) printw(
				 "Making forced moves...                                 ");
		refresh();
		(void) sleep(1);
		c = live[0];
	    } else {
		char buf[BUFSIZ];

		_nc_SPRINTF(buf, _nc_SLIMIT(sizeof(buf))
			    "Type [%s] to move, r to redraw, q or INTR to quit: ",
			    live);

		do {
		    move(PROMPTROW, 0);
		    (void) addstr(buf);
		    move(PROMPTROW, (int) strlen(buf));
		    clrtoeol();
		    AddCh(' ');
		} while
		    (((c = (char) getch()) < 'a' || c > 'd')
		     && (c != 'r')
		     && (c != 'q'));
	    }

	    for (j = 0; j < 4; j++)
		if (selection[j] != NOCARD) {
		    move(BASEROW + (selection[j] / GRID_WIDTH) * 2 + 3,
			 (selection[j] % GRID_WIDTH) * 5);
		    (void) printw("     ");
		}

	    if (c == 'r')
		display_cards(deal_number);
	    else if (c == 'q')
		die(SIGINT);
	    else {
		i = c - 'a';
		if (selection[i] == NOCARD)
		    beep();
		else {
		    movecard(selection[i], freeptr[i]);
		    freeptr[i] = selection[i];
		}
	    }
	}
    }

    move(PROMPTROW, 0);
    (void) standout();
    (void) printw("Finished deal %d - type any character to continue...", deal_number);
    (void) standend();
    (void) getch();
}

static int
collect_discards(void)
{
    int row, col, cardno = 0, gridno;

    for (row = HEARTS; row <= CLUBS; row++) {
	int finish = 0;
	for (col = 1; col < GRID_WIDTH; col++) {
	    gridno = row * GRID_WIDTH + col;

	    if ((grid[gridno] != (grid[gridno - 1] + 1)) && (finish == 0)) {
		finish = 1;
		freeptr[row] = gridno;
	    };

	    if ((finish != 0) && (grid[gridno] != NOCARD))
		deck[cardno++] = grid[gridno];
	}
    }
    return cardno;
}

static void
game_finished(int deal)
{
    clear();
    (void) printw("You finished the game in %d deals. This is ", deal);
    (void) standout();
    if (deal < 2)
	(void) addstr("excellent");
    else if (deal < 4)
	(void) addstr("good");
    else if (deal < 8)
	(void) addstr("average");
    else
	(void) addstr("poor");
    (void) standend();
    (void) addstr(".         ");
    refresh();
}

#if HAVE_LANGINFO_CODESET
/*
 * This program first appeared in ncurses in January 1995.  At that point, the
 * Linux console was able to display CP437 graphic characters, e.g., in the
 * range 0-31.  As of 2016, most Linux consoles are running with the UTF-8
 * (partial) support.  Incidentally, that makes all of the cards diamonds.
 */
static void
use_pc_display(void)
{
    char *check = nl_langinfo(CODESET);
    if (!strcmp(check, "UTF-8")) {
#if USE_WIDEC_SUPPORT
	suits = uglyphs;
#endif
    } else {
#if HAVE_TIGETSTR
	if (!strcmp(check, "IBM437") ||
	    !strcmp(check, "CP437") ||
	    !strcmp(check, "IBM850") ||
	    !strcmp(check, "CP850")) {
	    char *smacs = tigetstr("smacs");
	    char *smpch = tigetstr("smpch");
	    /*
	     * The ncurses library makes this check to decide whether to allow
	     * the alternate character set for the (normally) nonprinting codes.
	     */
	    if (smacs != 0 && smpch != 0 && !strcmp(smacs, smpch)) {
		suits = glyphs;
	    }
	}
#endif
    }
}
#else
#define use_pc_display()	/* nothing */
#endif /* HAVE_LANGINFO_CODESET */

int
main(int argc, char *argv[])
{
    setlocale(LC_ALL, "");

    use_pc_display();

    InitAndCatch(initscr(), die);

    start_color();
    init_pair(RED_ON_WHITE, COLOR_RED, COLOR_WHITE);
    init_pair(BLUE_ON_WHITE, COLOR_BLUE, COLOR_WHITE);
    init_pair(BLACK_ON_WHITE, COLOR_BLACK, COLOR_WHITE);

    cbreak();

    if (argc == 2)
	srand((unsigned) atoi(argv[1]));
    else
	srand((unsigned) time((time_t *) 0));

    init_vars();

    do {
	deal_number++;
	shuffle(deck_size);
	deal_cards();
	display_cards(deal_number);
	play_game();
    }
    while
	((deck_size = collect_discards()) != 0);

    game_finished(deal_number);

    die(SIGINT);
    /*NOTREACHED */
}

/* blue.c ends here */
