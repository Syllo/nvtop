/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 2003-2014,2017 Free Software Foundation, Inc.                  *
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
 * $Id: edit_field.c,v 1.31 2020/02/02 23:34:34 tom Exp $
 *
 * A wrapper for form_driver() which keeps track of the user's editing changes
 * for each field, and makes the resulting length available as a
 * null-terminated string in field_buffer(field,1).
 *
 * Thomas Dickey - 2003/4/26.
 */

#include <test.priv.h>

#if USE_LIBFORM

#include <edit_field.h>
#include <popup_msg.h>

static struct {
    int code;
    int result;
    const char *help;
} commands[] = {

    {
	CTRL('A'), REQ_NEXT_CHOICE, ""
    },
    {
	CTRL('B'), REQ_PREV_WORD, "go to previous word"
    },
    {
	CTRL('C'), REQ_CLR_EOL, "clear to end of line"
    },
    {
	CTRL('D'), REQ_DOWN_FIELD, "move downward to field"
    },
    {
	CTRL('E'), REQ_END_FIELD, "go to end of field"
    },
    {
	CTRL('F'), REQ_NEXT_PAGE, "go to next page"
    },
    {
	CTRL('G'), REQ_DEL_WORD, "delete current word"
    },
    {
	CTRL('H'), REQ_DEL_PREV, "delete previous character"
    },
    {
	CTRL('I'), REQ_INS_CHAR, "insert character"
    },
    {
	CTRL('K'), REQ_CLR_EOF, "clear to end of field"
    },
    {
	CTRL('L'), REQ_LEFT_FIELD, "go to field to left"
    },
    {
	CTRL('M'), REQ_NEW_LINE, "insert/overlay new line"
    },
    {
	CTRL('N'), REQ_NEXT_FIELD, "go to next field"
    },
    {
	CTRL('O'), REQ_INS_LINE, "insert blank line at cursor"
    },
    {
	CTRL('P'), REQ_PREV_FIELD, "go to previous field"
    },
    {
	CTRL('Q'), MY_QUIT, "exit form"
    },
    {
	CTRL('R'), REQ_RIGHT_FIELD, "go to field to right"
    },
    {
	CTRL('S'), REQ_BEG_FIELD, "go to beginning of field"
    },
    {
	CTRL('T'), MY_EDT_MODE, "toggle O_EDIT mode, clear field status",
    },
    {
	CTRL('U'), REQ_UP_FIELD, "move upward to field"
    },
    {
	CTRL('V'), REQ_DEL_CHAR, "delete character"
    },
    {
	CTRL('W'), REQ_NEXT_WORD, "go to next word"
    },
    {
	CTRL('X'), REQ_CLR_FIELD, "clear field"
    },
    {
	CTRL('Y'), REQ_DEL_LINE, "delete line"
    },
    {
	CTRL('Z'), REQ_PREV_CHOICE, ""
    },
    {
	CTRL('['), MY_QUIT, "exit form"
    },
    {
	CTRL(']'), MY_INS_MODE, "toggle REQ_INS_MODE/REQ_OVL_MODE",
    },
    {
	KEY_F(1), MY_HELP, "show this screen",
    },
    {
	KEY_BACKSPACE, REQ_DEL_PREV, "delete previous character"
    },
    {
	KEY_DOWN, REQ_DOWN_CHAR, "move down 1 character"
    },
    {
	KEY_END, REQ_LAST_FIELD, "go to last field"
    },
    {
	KEY_HOME, REQ_FIRST_FIELD, "go to first field"
    },
    {
	KEY_LEFT, REQ_LEFT_CHAR, "move left 1 character"
    },
    {
	KEY_LL, REQ_LAST_FIELD, "go to last field"
    },
    {
	KEY_NEXT, REQ_NEXT_FIELD, "go to next field"
    },
    {
	KEY_NPAGE, REQ_NEXT_PAGE, "go to next page"
    },
    {
	KEY_PPAGE, REQ_PREV_PAGE, "go to previous page"
    },
    {
	KEY_PREVIOUS, REQ_PREV_FIELD, "go to previous field"
    },
    {
	KEY_RIGHT, REQ_RIGHT_CHAR, "move right 1 character"
    },
    {
	KEY_UP, REQ_UP_CHAR, "move up 1 character"
    }
};

/*
 * Display a temporary window listing the keystroke-commands we recognize.
 */
void
help_edit_field(void)
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
    set_field_back(f, field_attrs(f)->background);
}

FieldAttrs *
field_attrs(FIELD *f)
{
    return (FieldAttrs *) field_userptr(f);
}

static int
buffer_length(FIELD *f)
{
    return field_attrs(f)->row_lengths[0];
}

static void
set_buffer_length(FIELD *f, int length)
{
    field_attrs(f)->row_lengths[0] = length;
}

/*
 * The userptr is used in edit_field.c's inactive_field(), as well as for
 * keeping track of the actual lengths of lines in a multiline field.
 */
void
init_edit_field(FIELD *f, char *value)
{
    char empty[1];
    FieldAttrs *ptr = field_attrs(f);
    if (ptr == 0) {
	int rows, cols, frow, fcol, nrow, nbuf;

	ptr = typeCalloc(FieldAttrs, (size_t) 1);
	ptr->background = field_back(f);
	if (field_info(f, &rows, &cols, &frow, &fcol, &nrow, &nbuf) == E_OK) {
	    ptr->row_count = nrow;
	    ptr->row_lengths = typeCalloc(int, (size_t) nrow + 1);
	}
    }
    if (value == 0) {
	value = empty;
	*value = '\0';
    }
    set_field_userptr(f, (void *) ptr);
    set_field_buffer(f, 0, value);	/* will be formatted */
    set_field_buffer(f, 1, value);	/* will be unformatted */
    set_buffer_length(f, (int) strlen(value));
}

int
edit_field(FORM *form, int *result)
{
    int ch = wgetch(form_win(form));
    int status;
    FIELD *before;
    unsigned n;
    int length;
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

	length = buffer_length(before);
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
#if 0
	    /* FIXME: finish these */
	case REQ_DEL_LINE:	/* delete line */
	case REQ_DEL_WORD:	/* delete word at cursor */
	case REQ_INS_CHAR:	/* insert blank char at cursor */
	case REQ_INS_LINE:	/* insert blank line at cursor */
	case REQ_INS_MODE:	/* begin insert mode */
	case REQ_OVL_MODE:	/* begin overlay mode */
#endif
	    /* ignore all of the motion commands */
	case REQ_SCR_BCHAR:	/* FALLTHRU */
	case REQ_SCR_BHPAGE:	/* FALLTHRU */
	case REQ_SCR_BLINE:	/* FALLTHRU */
	case REQ_SCR_BPAGE:	/* FALLTHRU */
	case REQ_SCR_FCHAR:	/* FALLTHRU */
	case REQ_SCR_FHPAGE:	/* FALLTHRU */
	case REQ_SCR_FLINE:	/* FALLTHRU */
	case REQ_SCR_FPAGE:	/* FALLTHRU */
	case REQ_SCR_HBHALF:	/* FALLTHRU */
	case REQ_SCR_HBLINE:	/* FALLTHRU */
	case REQ_SCR_HFHALF:	/* FALLTHRU */
	case REQ_SCR_HFLINE:	/* FALLTHRU */
	case REQ_BEG_FIELD:	/* FALLTHRU */
	case REQ_BEG_LINE:	/* FALLTHRU */
	case REQ_DOWN_CHAR:	/* FALLTHRU */
	case REQ_DOWN_FIELD:	/* FALLTHRU */
	case REQ_END_FIELD:	/* FALLTHRU */
	case REQ_END_LINE:	/* FALLTHRU */
	case REQ_FIRST_FIELD:	/* FALLTHRU */
	case REQ_FIRST_PAGE:	/* FALLTHRU */
	case REQ_LAST_FIELD:	/* FALLTHRU */
	case REQ_LAST_PAGE:	/* FALLTHRU */
	case REQ_LEFT_CHAR:	/* FALLTHRU */
	case REQ_LEFT_FIELD:	/* FALLTHRU */
	case REQ_NEXT_CHAR:	/* FALLTHRU */
	case REQ_NEXT_CHOICE:	/* FALLTHRU */
	case REQ_NEXT_FIELD:	/* FALLTHRU */
	case REQ_NEXT_LINE:	/* FALLTHRU */
	case REQ_NEXT_PAGE:	/* FALLTHRU */
	case REQ_NEXT_WORD:	/* FALLTHRU */
	case REQ_PREV_CHAR:	/* FALLTHRU */
	case REQ_PREV_CHOICE:	/* FALLTHRU */
	case REQ_PREV_FIELD:	/* FALLTHRU */
	case REQ_PREV_LINE:	/* FALLTHRU */
	case REQ_PREV_PAGE:	/* FALLTHRU */
	case REQ_PREV_WORD:	/* FALLTHRU */
	case REQ_RIGHT_CHAR:	/* FALLTHRU */
	case REQ_RIGHT_FIELD:	/* FALLTHRU */
	case REQ_SFIRST_FIELD:	/* FALLTHRU */
	case REQ_SLAST_FIELD:	/* FALLTHRU */
	case REQ_SNEXT_FIELD:	/* FALLTHRU */
	case REQ_SPREV_FIELD:	/* FALLTHRU */
	case REQ_UP_CHAR:	/* FALLTHRU */
	case REQ_UP_FIELD:	/* FALLTHRU */
	case REQ_VALIDATION:	/* FALLTHRU */
	    modified = FALSE;
	    break;

	default:
	    modified = FALSE;
	    if (ch >= MIN_FORM_COMMAND) {
		beep();
	    } else if (isprint(ch)) {
		modified = TRUE;
	    }
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

void
free_edit_field(FIELD *f)
{
    FieldAttrs *ptr = field_attrs(f);
    if (ptr != 0) {
	free(ptr->row_lengths);
	free(ptr);
    }
}
#else

extern void no_edit_field(void);

void
no_edit_field(void)
{
}

#endif
