/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2013,2017 Free Software Foundation, Inc.                  *
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
 * Knight's Tour - a brain game
 *
 * The original of this game was anonymous.  It had an unbelievably bogus
 * interface, you actually had to enter square coordinates!  Redesign by
 * Eric S. Raymond <esr@snark.thyrsus.com> July 22 1995.  Mouse support
 * added September 20th 1995.
 *
 * $Id: knight.c,v 1.49 2021/05/08 19:32:15 tom Exp $
 */

#include <test.priv.h>

/* board size */
#define YLIMIT		8
#define XLIMIT		8
#define MAXMOVES	(ylimit * xlimit)

/* where to start the instructions */
#define INSTRY		2
#define INSTRX		35

/* corner of board */
#define BOARDY		2
#define BOARDX		0

/* notification line */
#define NOTIFYY		21

/* virtual color values */
#define TRAIL_COLOR	1
#define PLUS_COLOR	2
#define MINUS_COLOR	3

#define CX(x)		(2 + 4 * (x))
#define CY(y)		(1 + 2 * (y))
#define cellmove(y, x)	wmove(boardwin, CY(y), CX(x))
#define CXINV(x)	(((x) - 1) / 4)
#define CYINV(y)	(((y) - 2) / 2)

typedef struct {
    int x, y;
} HISTORY;

typedef int SQUARES[YLIMIT][XLIMIT];

static WINDOW *boardwin;	/* the board window */
static WINDOW *helpwin;		/* the help window */
static WINDOW *msgwin;		/* the message window */

#if HAVE_USE_DEFAULT_COLORS
static bool d_option;
#endif

static chtype minus = '-';	/* possible-move character */
static chtype oldch;
static chtype plus = '+';	/* cursor hot-spot character */
static chtype trail = '#';	/* trail character */

static int ylimit = YLIMIT;
static int xlimit = XLIMIT;
static int maxmoves = (YLIMIT * XLIMIT);

static int count_tries;		/* count of trials so far */
static int test_test;		/* FIXME */
/* *INDENT-OFF* */
static const struct {
    int y;
    int x;
} offsets[] = {
    {  2,  1 },
    {  1,  2 },
    { -1,  2 },
    { -2,  1 },
    { -2, -1 },
    { -1, -2 },
    {  1, -2 },
    {  2, -1 },
};
#define MAX_OFFSET	(unsigned)SIZEOF(offsets)
/* *INDENT-ON* */

static void
init_program(void)
{
    setlocale(LC_ALL, "");

    srand((unsigned) getpid());
    initscr();
    cbreak();			/* immediate char return */
    noecho();			/* no immediate echo */

    maxmoves = MAXMOVES;
    boardwin = newwin(ylimit * 2 + 1, xlimit * 4 + 1, BOARDY, BOARDX);
    helpwin = newwin(0, 0, INSTRY, INSTRX);
    msgwin = newwin(1, INSTRX - 1, NOTIFYY, 0);

    scrollok(msgwin, TRUE);
    keypad(boardwin, TRUE);

    if (has_colors()) {
	int bg = COLOR_BLACK;

	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() == OK))
	    bg = -1;
#endif

	(void) init_pair(TRAIL_COLOR, (short) COLOR_CYAN, (short) bg);
	(void) init_pair(PLUS_COLOR, (short) COLOR_RED, (short) bg);
	(void) init_pair(MINUS_COLOR, (short) COLOR_GREEN, (short) bg);

	trail |= (chtype) COLOR_PAIR(TRAIL_COLOR);
	plus |= (chtype) COLOR_PAIR(PLUS_COLOR);
	minus |= (chtype) COLOR_PAIR(MINUS_COLOR);
    }
#ifdef NCURSES_MOUSE_VERSION
    (void) mousemask(BUTTON1_CLICKED, (mmask_t *) NULL);
#endif /* NCURSES_MOUSE_VERSION */
#if defined(PDCURSES)
    mouse_set(BUTTON1_RELEASED);
#endif

    oldch = minus;
}

static void
help1(void)
/* game explanation -- initial help screen */
{
    (void) waddstr(helpwin, "Knight's move is a solitaire puzzle.  Your\n");
    (void) waddstr(helpwin, "objective is to visit each square of the  \n");
    (void) waddstr(helpwin, "chessboard exactly once by making knight's\n");
    (void) waddstr(helpwin, "moves (one square right or left followed  \n");
    (void) waddstr(helpwin, "by two squares up or down, or two squares \n");
    (void) waddstr(helpwin, "right or left followed by one square up or\n");
    (void) waddstr(helpwin, "down).  You may start anywhere.\n\n");

    (void) waddstr(helpwin, "Use arrow keys to move the cursor around.\n");
    (void) waddstr(helpwin, "When you want to move your knight to the \n");
    (void) waddstr(helpwin, "cursor location, press <space> or Enter.\n");
    (void) waddstr(helpwin, "Illegal moves will be rejected with an  \n");
    (void) waddstr(helpwin, "audible beep.\n\n");
    (void) waddstr(helpwin, "The program will detect if you solve the\n");
    (void) waddstr(helpwin, "puzzle; also inform you when you run out\n");
    (void) waddstr(helpwin, "of legal moves.\n\n");

    MvWAddStr(helpwin, NOTIFYY - INSTRY, 0,
	      "Press `?' to go to keystroke help.");
}

static void
help2(void)
/* keystroke help screen */
{
    (void) waddstr(helpwin, "Possible moves are shown with `-'.\n\n");

    (void) waddstr(helpwin, "You can move around with the arrow keys or\n");
    (void) waddstr(helpwin, "with the rogue/hack movement keys.  Other\n");
    (void) waddstr(helpwin, "commands allow you to undo moves or redraw.\n");
    (void) waddstr(helpwin, "Your mouse may work; try left-button to\n");
    (void) waddstr(helpwin, "move to the square under the pointer.\n\n");

    (void) waddstr(helpwin, "x,q -- exit             y k u    7 8 9\n");
    (void) waddstr(helpwin, "r -- redraw screen       \\|/      \\|/ \n");
    (void) waddstr(helpwin, "bksp -- undo move       h-+-l    4-+-6\n");
    (void) waddstr(helpwin, "a -- autojump            /|\\      /|\\ \n");
    if (ylimit <= 6) {
	(void) waddstr(helpwin, "R -- solve (slow)       b j n    1 2 3\n");
    } else {
	(void) waddstr(helpwin, "                        b j n    1 2 3\n");
    }

    (void) waddstr(helpwin, "\nYou can place your knight on the selected\n");
    (void) waddstr(helpwin, "square with spacebar, Enter, or the keypad\n");
    (void) waddstr(helpwin, "center key.  Use F/B to review the path.\n");

    MvWAddStr(helpwin, NOTIFYY - INSTRY, 0,
	      "Press `?' to go to game explanation");
}

static void
show_help(bool * keyhelp)
{
    werase(helpwin);
    if (*keyhelp) {
	help1();
	*keyhelp = FALSE;
    } else {
	help2();
	*keyhelp = TRUE;
    }
    wrefresh(helpwin);
}

static inline bool
isValidYX(int y, int x)
{
    return (y >= 0 && y < ylimit && x >= 0 && x < xlimit) ? TRUE : FALSE;
}

static inline bool
isUnusedYX(SQUARES squares, int y, int x)
{
    return (isValidYX(y, x) && (!squares[y][x]) ? TRUE : FALSE);
}

static bool
boardIsFilled(SQUARES squares, int y, int x)
{
    unsigned n;

    for (n = 0; n < MAX_OFFSET; n++) {
	if (isUnusedYX(squares, y + offsets[n].y, x + offsets[n].x)) {
	    return FALSE;
	}
    }
    return TRUE;
}

static void
drawBoard(void)
{
    int i, j;

    MvAddStr(0, 20, "KNIGHT'S MOVE -- a logical solitaire");

    move(BOARDY, BOARDX);
    waddch(boardwin, ACS_ULCORNER);
    for (j = 0; j < (ylimit - 1); j++) {
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_TTEE);
    }
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_URCORNER);

    for (i = 1; i < ylimit; i++) {
	move(BOARDY + i * 2 - 1, BOARDX);
	waddch(boardwin, ACS_VLINE);
	for (j = 0; j < xlimit; j++) {
	    waddch(boardwin, ' ');
	    waddch(boardwin, ' ');
	    waddch(boardwin, ' ');
	    waddch(boardwin, ACS_VLINE);
	}
	move(BOARDY + i * 2, BOARDX);
	waddch(boardwin, ACS_LTEE);
	for (j = 0; j < xlimit - 1; j++) {
	    waddch(boardwin, ACS_HLINE);
	    waddch(boardwin, ACS_HLINE);
	    waddch(boardwin, ACS_HLINE);
	    waddch(boardwin, ACS_PLUS);
	}
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_RTEE);
    }

    move(BOARDY + i * 2 - 1, BOARDX);
    waddch(boardwin, ACS_VLINE);
    for (j = 0; j < xlimit; j++) {
	waddch(boardwin, ' ');
	waddch(boardwin, ' ');
	waddch(boardwin, ' ');
	waddch(boardwin, ACS_VLINE);
    }

    move(BOARDY + i * 2, BOARDX);
    waddch(boardwin, ACS_LLCORNER);
    for (j = 0; j < xlimit - 1; j++) {
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_HLINE);
	waddch(boardwin, ACS_BTEE);
    }
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_HLINE);
    waddch(boardwin, ACS_LRCORNER);
}

static void
mark_possibles(SQUARES squares, int y, int x, chtype mark)
{
    unsigned n;

    for (n = 0; n < MAX_OFFSET; n++) {
	if (isUnusedYX(squares, y + offsets[n].y, x + offsets[n].x)) {
	    cellmove(y + offsets[n].y, x + offsets[n].x);
	    waddch(boardwin, mark);
	}
    }
}

static bool
find_next_move(SQUARES squares, HISTORY * doneData, int doneSize, int *y, int *x)
{
    bool result = FALSE;

    if (doneSize > 1) {
	unsigned j;
	int oldy = doneData[doneSize - 1].y;
	int oldx = doneData[doneSize - 1].x;
	int found = -1;
	int first = -1;
	int next = -1;

	for (j = 0; j < MAX_OFFSET * 2; j++) {
	    unsigned k = j % MAX_OFFSET;
	    int newy = oldy + offsets[k].y;
	    int newx = oldx + offsets[k].x;
	    if (isUnusedYX(squares, newy, newx)) {
		if (first < 0)
		    first = (int) k;
		if (newy == *y
		    && newx == *x) {
		    found = (int) k;
		} else if (found >= 0) {
		    next = (int) k;
		    break;
		}
	    }
	}
	if (found < 0)
	    next = first;
	if (next >= 0) {
	    *y = oldy + offsets[next].y;
	    *x = oldx + offsets[next].x;
	}
	result = TRUE;
    }
    return result;
}

static void
count_next_moves(SQUARES squares, int count_moves, int y, int x)
{
    int count = 0;
    unsigned j;

    wprintw(msgwin, "\nMove %d", count_moves);
    for (j = 0; j < MAX_OFFSET; j++) {
	int newy = y + offsets[j].y;
	int newx = x + offsets[j].x;
	if (isUnusedYX(squares, newy, newx)) {
	    ++count;
	}
    }
    wprintw(msgwin, ", gives %d choices", count);
    wclrtoeol(msgwin);
}

static void
unmarkcell(int row, int column)
{
    cellmove(row, column);
    waddch(boardwin, '\b');
    waddch(boardwin, ' ');
    waddch(boardwin, minus);
    waddch(boardwin, ' ');
}

static void
markcell(chtype tchar, int row, int column)
{
    cellmove(row, column);
    waddch(boardwin, '\b');
    waddch(boardwin, tchar);
    waddch(boardwin, tchar);
    waddch(boardwin, tchar);
}

static void
drawMove(SQUARES squares, int count_moves, chtype tchar, int oldy, int oldx, int
	 row, int column)
/* place the stars, update board & currents */
{
    if (count_moves <= 1) {
	int i, j;

	for (i = 0; i < ylimit; i++) {
	    for (j = 0; j < xlimit; j++) {
		if (count_moves == 0) {
		    unmarkcell(i, j);
		} else {
		    cellmove(i, j);
		    if (winch(boardwin) == minus)
			waddch(boardwin, ' ');
		}
	    }
	}
    } else {
	markcell(tchar, oldy, oldx);
	mark_possibles(squares, oldy, oldx, ' ');
    }

    if (row >= 0 && column >= 0) {
	markcell(trail, row, column);
	mark_possibles(squares, row, column, minus);
	squares[row][column] = TRUE;
    }

    wprintw(msgwin, "\nMove %d", count_moves);
    if (count_tries != count_moves)
	wprintw(msgwin, " (%d tries)", count_tries);
    wclrtoeol(msgwin);
}

static int
iabs(int num)
{
    if (num < 0)
	return (-num);
    else
	return (num);
}

static bool
evaluate_move(SQUARES squares, HISTORY * doneData, int doneSize, int row, int column)
{
    if (doneSize <= 1)
	return (TRUE);
    else if (squares[row][column] == TRUE) {
	waddstr(msgwin, "\nYou've already been there.");
	return (FALSE);
    } else {
	int rdif = iabs(row - doneData[doneSize - 1].y);
	int cdif = iabs(column - doneData[doneSize - 1].x);

	if (!((rdif == 1) && (cdif == 2)) && !((rdif == 2) && (cdif == 1))) {
	    waddstr(msgwin, "\nThat's not a legal knight's move.");
	    return (FALSE);
	}
    }

    return (TRUE);
}

static int
completed(SQUARES squares)
{
    int i, j, count = 0;

    for (i = 0; i < ylimit; i++) {
	for (j = 0; j < xlimit; j++) {
	    if (squares[i][j] != 0) {
		count += 1;
	    }
	}
    }
    return ((count == maxmoves) ? -1 : count);
}

static void
no_previous_move(void)
{
    waddstr(msgwin, "\nNo previous move.");
    beep();
}

/* Recursively try all possible moves, starting from (y,x) */
static int
recurBack(SQUARES squares, int y, int x, int total)
{
    int longest = total;
    int best_x = x;
    int best_y = y;
    int result;

    if (total < maxmoves) {
	unsigned k;

	for (k = 0; k < MAX_OFFSET; k++) {
	    int try_x = x + offsets[k].x;
	    int try_y = y + offsets[k].y;
	    if (isUnusedYX(squares, try_y, try_x)) {
		++test_test;
		squares[try_y][try_x] = total + 1;
		result = recurBack(squares, try_y, try_x, total + 1);
		if (result > longest) {
		    longest = result;
		    best_x = try_x;
		    best_y = try_y;
		}
		if (result >= maxmoves)
		    break;
		squares[try_y][try_x] = 0;	/* allow retry... */
	    }
	}
    }

    result = total;
    if (longest > total) {
	result = longest;
	squares[best_y][best_x] = total + 1;
	(void) recurBack(squares, best_y, best_x, total + 1);
	if (result < maxmoves)
	    squares[best_y][best_x] = 0;
    }

    return result;
}

/*
 * Solve the Knight Tour problem using backtracking, returning the length of
 * the resulting solution.  If this is invoked from a point where the remaining
 * choices cannot complete the tour, the result will fall short.
 */
static int
useBacktracking(SQUARES result, HISTORY * doneData, int doneSize)
{
    int y = 0, x = 0, n;
    SQUARES squares;
    int total;
    int actual = doneSize - 1;

    memset(squares, 0, sizeof(squares));
    for (n = 1; n <= actual; ++n) {
	y = doneData[n].y;
	x = doneData[n].x;
	squares[y][x] = n;
    }

    total = recurBack(squares, y, x, actual);
    if (total > actual) {
	for (y = 0; y < ylimit; ++y) {
	    for (x = 0; x < xlimit; ++x) {
		result[y][x] = squares[y][x];
		if ((n = squares[y][x]) != 0) {
		    doneData[n].y = y;
		    doneData[n].x = x;
		}
	    }
	}
    }
    return total;
}

static int
reviewHistory(HISTORY * history, int count_moves, int review, int *ny, int *nx)
{
    if (review < 0) {
	beep();
	review = 0;
    } else if (review > count_moves - 2) {
	beep();
	review = count_moves - 2;
    } else {
	*ny = history[count_moves - review - 1].y;
	*nx = history[count_moves - review - 1].x;
	wprintw(msgwin, "\nReview %d:%d.", count_moves - review - 1,
		count_moves - 1);
	wrefresh(msgwin);
    }
    return review;
}

static void
play(void)
/* play the game */
{
    bool keyhelp;		/* TRUE if keystroke help is up */
    int i, j, count;
    int lastcol;		/* last location visited */
    int lastrow;
    int ny = 0, nx = 0;
    int review = 0;		/* review history */
    int test_size;
    int rw = 0, col = 0;	/* current row and column */

    do {
	SQUARES squares;
	HISTORY history[(YLIMIT * XLIMIT) + 1];
	int count_moves = 0;	/* count of moves so far */

	/* clear screen and draw board */
	werase(boardwin);
	werase(helpwin);
	werase(msgwin);
	drawBoard();
	help1();
	wnoutrefresh(stdscr);
	wnoutrefresh(helpwin);
	wnoutrefresh(msgwin);
	wnoutrefresh(boardwin);
	doupdate();

	for (i = 0; i < ylimit; i++) {
	    for (j = 0; j < xlimit; j++) {
		unmarkcell(i, j);
	    }
	}
	memset(squares, 0, sizeof(squares));
	memset(history, 0, sizeof(history));
	history[0].y = history[0].x = -1;
	history[1].y = history[1].x = -1;
	lastrow = lastcol = -2;
	count_moves = 1;
	count_tries = 1;
	keyhelp = FALSE;
	show_help(&keyhelp);

	for (;;) {
	    if (rw != lastrow || col != lastcol) {
		if (lastrow >= 0 && lastcol >= 0) {
		    cellmove(lastrow, lastcol);
		    if (squares[lastrow][lastcol])
			waddch(boardwin, trail);
		    else
			waddch(boardwin, oldch);
		}

		cellmove(rw, col);
		oldch = winch(boardwin);

		lastrow = rw;
		lastcol = col;
	    }
	    cellmove(rw, col);
	    waddch(boardwin, plus);
	    cellmove(rw, col);

	    wrefresh(msgwin);

	    switch (wgetch(boardwin)) {
	    case 'k':
	    case '8':
	    case KEY_UP:
		ny = rw + ylimit - 1;
		nx = col;
		break;
	    case 'j':
	    case '2':
	    case KEY_DOWN:
		ny = rw + 1;
		nx = col;
		break;
	    case 'h':
	    case '4':
	    case KEY_LEFT:
		ny = rw;
		nx = col + xlimit - 1;
		break;
	    case 'l':
	    case '6':
	    case KEY_RIGHT:
		ny = rw;
		nx = col + 1;
		break;
	    case 'y':
	    case '7':
	    case KEY_A1:
		ny = rw + ylimit - 1;
		nx = col + xlimit - 1;
		break;
	    case 'b':
	    case '1':
	    case KEY_C1:
		ny = rw + 1;
		nx = col + xlimit - 1;
		break;
	    case 'u':
	    case '9':
	    case KEY_A3:
		ny = rw + ylimit - 1;
		nx = col + 1;
		break;
	    case 'n':
	    case '3':
	    case KEY_C3:
		ny = rw + 1;
		nx = col + 1;
		break;

#ifdef KEY_MOUSE
	    case KEY_MOUSE:
#ifdef NCURSES_MOUSE_VERSION
		{
		    MEVENT myevent;

		    getmouse(&myevent);
		    if (myevent.y >= CY(0) && myevent.y <= CY(ylimit)
			&& myevent.x >= CX(0) && myevent.x <= CX(xlimit)) {
			nx = CXINV(myevent.x);
			ny = CYINV(myevent.y);
			ungetch('\n');
			break;
		    } else {
			beep();
			continue;
		    }
		}
#endif /* NCURSES_MOUSE_VERSION */
#ifdef PDCURSES
		{
		    int test_y, test_x;
		    request_mouse_pos();
		    test_y = MOUSE_Y_POS + 0;
		    test_x = MOUSE_X_POS + 1;
		    if (test_y >= CY(0) && test_y <= CY(ylimit)
			&& test_x >= CX(0) && test_x <= CX(xlimit)) {
			ny = CYINV(test_y);
			nx = CXINV(test_x);
			wmove(helpwin, 0, 0);
			wrefresh(helpwin);
			ungetch('\n');
		    }
		    break;
		}
#endif /* PDCURSES */
#endif /* KEY_MOUSE */

	    case KEY_B2:
	    case '\n':
	    case ' ':
		review = 0;
		if (evaluate_move(squares, history, count_moves, rw, col)) {
		    drawMove(squares,
			     count_moves,
			     trail,
			     history[count_moves - 1].y,
			     history[count_moves - 1].x,
			     rw, col);
		    history[count_moves].y = (short) rw;
		    history[count_moves].x = (short) col;
		    count_moves++;
		    count_tries++;

		    if (boardIsFilled(squares, rw, col)) {
			if (completed(squares) < 0) {
			    waddstr(msgwin, "\nYou won.");
			} else {
			    waddstr(msgwin,
				    "\nNo further moves are possible.");
			}
		    }
		} else {
		    beep();
		}
		break;

	    case KEY_UNDO:
	    case KEY_BACKSPACE:
	    case '\b':
		review = 0;
		if (count_moves <= 0) {
		    no_previous_move();
		} else if (count_moves <= 1) {
		    ny = history[count_moves].y;
		    nx = history[count_moves].x;
		    if (nx < 0 || ny < 0) {
			ny = (lastrow >= 0) ? lastrow : 0;
			nx = (lastcol >= 0) ? lastcol : 0;
		    }
		    count_moves = 0;
		    squares[ny][nx] = FALSE;
		    oldch = minus;
		    drawMove(squares, count_moves, ' ', ny, nx, -1, -1);
		    count_moves = 1;
		    count_tries = 1;
		    no_previous_move();
		} else {
		    int oldy = history[count_moves - 1].y;
		    int oldx = history[count_moves - 1].x;

		    if (!squares[rw][col]) {
			cellmove(rw, col);
			waddch(boardwin, ' ');
		    }

		    squares[oldy][oldx] = FALSE;
		    --count_moves;
		    ny = history[count_moves - 1].y;
		    nx = history[count_moves - 1].x;
		    if (nx < 0 || ny < 0) {
			ny = oldy;
			nx = oldx;
		    }
		    drawMove(squares, count_moves, ' ', oldy, oldx, ny, nx);

		    /* avoid problems if we just changed the current cell */
		    cellmove(lastrow, lastcol);
		    oldch = winch(boardwin);
		}
		break;

	    case 'a':
		nx = col;
		ny = rw;
		if (find_next_move(squares, history, count_moves, &ny, &nx))
		    count_next_moves(squares, count_moves, ny, nx);
		else
		    beep();
		break;

	    case 'F':
		review = reviewHistory(history, count_moves, review - 1,
				       &ny, &nx);
		break;

	    case 'B':
		review = reviewHistory(history, count_moves, review + 1,
				       &ny, &nx);
		break;

	    case 'R':
		if (ylimit <= 6) {
		    wprintw(msgwin, "\nworking...");
		    wrefresh(msgwin);
		    test_test = 0;
		    test_size = useBacktracking(squares, history, count_moves);
		    wprintw(msgwin, "\nOk %d:%d (%d tests)",
			    test_size, maxmoves, test_test);
		    review = 0;
		    while (count_moves <= test_size) {
			markcell(trail,
				 ny = history[count_moves].y,
				 nx = history[count_moves].x);
			count_moves++;
		    }
		} else {
		    wprintw(msgwin, "\nBoard is too large.");
		}
		wrefresh(msgwin);
		break;

#if HAVE_CURSCR
	    case KEY_REDO:
	    case '\f':
	    case 'r':
		clearok(curscr, TRUE);
		wnoutrefresh(stdscr);
		wnoutrefresh(boardwin);
		wnoutrefresh(msgwin);
		wnoutrefresh(helpwin);
		doupdate();
		break;
#endif

	    case 'q':
	    case 'x':
		goto dropout;

	    case HELP_KEY_1:
		show_help(&keyhelp);
		break;

	    default:
		beep();
		break;
	    }

	    col = nx % xlimit;
	    rw = ny % ylimit;
	}

      dropout:
	if ((count = completed(squares)) < 0)
	    wprintw(msgwin, "\nYou won.  Care to try again? ");
	else
	    wprintw(msgwin, "\n%d squares filled.  Try again? ", count);
	wclrtoeol(msgwin);
    } while
	(tolower(wgetch(msgwin)) == 'y');
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: knight [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors"
#endif
	," -n NUM   set board-size to NUM*NUM (default 8x8)"
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int ch;

    while ((ch = getopt(argc, argv, "dn:")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
	case 'n':
	    ch = atoi(optarg);
	    if (ch < 3 || ch > 8) {
		fprintf(stderr, "board size %d is outside [3..8]\n", ch);
		usage();
	    }
	    xlimit = ylimit = ch;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    if (optind < argc)
	usage();

    init_program();

    play();

    endwin();
    ExitProgram(EXIT_SUCCESS);
}
