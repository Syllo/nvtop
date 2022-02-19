/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 1998-2014,2016 Free Software Foundation, Inc.                  *
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
 *   Author:  Juergen Pfeifer, 1996                                         *
 *      and:  Thomas E. Dickey, 1998                                        *
 *      and:  Nicolas Boulenguez, 2011                                      *
 ****************************************************************************/

/*
    Version Control
    $Id: gen.c,v 1.77 2020/08/16 18:05:05 tom Exp $
  --------------------------------------------------------------------------*/
/*
  This program prints on its standard output the source for the
  Terminal_Interface.Curses_Constants Ada package specification. This pure
  package only exports C constants to the Ada compiler.
 */

#ifdef HAVE_CONFIG_H
#include <ncurses_cfg.h>
#else
#include <ncurses.h>
#endif

#include <stdlib.h>
#include <string.h>

#include <menu.h>
#include <form.h>

#undef UCHAR
#undef UINT
#undef ULONG

typedef unsigned char UCHAR;
typedef unsigned int UINT;
typedef unsigned long ULONG;

/* These global variables will be set by main () */
static int little_endian;
static const char *my_program_invocation_name = NULL;

static void
my_error(const char *message)
{
  fprintf(stderr, "%s: %s\n", my_program_invocation_name, message);
  exit(EXIT_FAILURE);
}

static void
print_constant(FILE * fp,
	       const char *name,
	       UINT value)
{
  fprintf(fp, "   %-28s : constant := %u;\n", name, value);
}

static void
print_long_val(FILE * fp,
	       const char *name,
	       long value)
{
  fprintf(fp, "   %-28s : constant := %ld;\n", name, value);
}

static void
print_size_of(FILE * fp,
	      const char *name,
	      size_t value)
{
  fprintf(fp, "   %-28s : constant := %lu;\n", name, value);
}

#define PRINT_NAMED_CONSTANT(name) \
  print_long_val (fp, #name, name)

static void
print_comment(FILE * fp, const char *message)
{
  fprintf(fp, "\n   --  %s\n\n", message);
}

/*
 * Make sure that KEY_MIN and KEY_MAX are defined.
 * main () will protest if KEY_MIN == 256
 */
#ifndef KEY_MAX
#  define KEY_MAX 0777
#endif
#ifndef KEY_MIN
#  define KEY_MIN 0401
#endif

static UCHAR
bit_is_set(const UCHAR * const data,
	   const UINT offset)
{
  const UCHAR byte = data[offset >> 3];
  UINT bit;

  if (little_endian)
    bit = offset;		/* offset */
  else				/* or */
    bit = ~offset;		/* 7 - offset */
  bit &= 7;			/* modulo 8 */
  return (UCHAR) (byte & (1 << bit));
}

/* Find lowest and highest used offset in a byte array. */
/* Returns 0 if and only if all bits are unset. */
static int
find_pos(const UCHAR * const data,
	 const UINT sizeof_data,
	 UINT * const low,
	 UINT * const high)
{
  const UINT last = (sizeof_data << 3) - 1;
  UINT offset;

  for (offset = last; !bit_is_set(data, offset); offset--)
    if (!offset)		/* All bits are 0. */
      return 0;
  *high = offset;

  for (offset = 0; !bit_is_set(data, offset); offset++)
    {
    }
  *low = offset;

  return -1;
}

#define PRINT_BITMASK(c_type, ada_name, mask_macro)                     \
  {                                                                     \
    UINT first, last;                                                   \
    c_type mask = (mask_macro);                                         \
    if (!find_pos ((UCHAR *)&mask, sizeof (mask), &first, &last))       \
      my_error ("failed to locate " ada_name);                          \
    print_constant (fp, ada_name "_First", first);                      \
    print_constant (fp, ada_name "_Last", last);                        \
  }

#define PRINT_NAMED_BITMASK(c_type, mask_macro)         \
  PRINT_BITMASK (c_type, #mask_macro, mask_macro)

#define STRUCT_OFFSET(record, field)                                    \
  {                                                                     \
    UINT first, last;                                                   \
    record mask;                                                        \
    memset (&mask, 0, sizeof (mask));                                   \
    memset (&mask.field, 0xff, sizeof(mask.field));                     \
    if (!find_pos ((UCHAR *)&mask, sizeof (mask), &first, &last))       \
      my_error ("failed to locate" #record "_" #field);                 \
    print_constant (fp, #record "_" #field "_First", first);            \
    print_constant (fp, #record "_" #field "_Last", last);              \
  }

/*--------------------*/
/*  Start of main (). */
/*--------------------*/

int
main(int argc, const char *argv[])
{
  FILE *fp = 0;
  const int x = 0x12345678;

  little_endian = (*((const char *)&x) == 0x78);

  my_program_invocation_name = argv[0];

  if (KEY_MIN == 256)
    my_error("unexpected value for KEY_MIN: 256");

  if (argc == 3)
    {
      fp = fopen(argv[2], "wb");
    }
  else if (argc == 2)
    {
      fp = stdout;
    }
  else
    {
      my_error("Only one or two arguments expected (DFT_ARG_SUFFIX)");
    }

  if ((strlen(argv[0]) + strlen(__FILE__)) > 25)
    {
      fprintf(fp, "--  Generated by the C program %.40s.\n",
	      my_program_invocation_name);
    }
  else
    {
      fprintf(fp, "--  Generated by the C program %s (source %s).\n",
	      my_program_invocation_name,
	      __FILE__);
    }
  fprintf(fp, "--  Do not edit this file directly.\n");
  fprintf(fp, "--  The values provided here may vary on your system.\n");
  fprintf(fp, "\n");
  fprintf(fp, "with System;\n");
  fprintf(fp, "package Terminal_Interface.Curses_Constants is\n");
  fprintf(fp, "   pragma Pure;\n");
  fprintf(fp, "\n");

  fprintf(fp, "   DFT_ARG_SUFFIX : constant String := \"%s\";\n", argv[1]);
  fprintf(fp,
	  "   Bit_Order : constant System.Bit_Order := System.%s_Order_First;\n",
	  little_endian ? "Low" : "High");
  print_size_of(fp, "Sizeof_Bool", 8 * sizeof(bool));

  PRINT_NAMED_CONSTANT(OK);
  PRINT_NAMED_CONSTANT(ERR);
  fprintf(fp,
	  "   pragma Warnings (Off); -- redefinition of Standard.True and False\n");
  PRINT_NAMED_CONSTANT(TRUE);
  PRINT_NAMED_CONSTANT(FALSE);
  fprintf(fp, "   pragma Warnings (On);\n");

  print_comment(fp, "Version of the ncurses library from extensions(3NCURSES)");
  PRINT_NAMED_CONSTANT(NCURSES_VERSION_MAJOR);
  PRINT_NAMED_CONSTANT(NCURSES_VERSION_MINOR);
  fprintf(fp, "   Version : constant String := \"%d.%d\";\n",
	  NCURSES_VERSION_MAJOR, NCURSES_VERSION_MINOR);

  print_comment(fp, "Character non-color attributes from attr(3NCURSES)");
  fprintf(fp, "   --  attr_t and chtype may be signed in C.\n");
  fprintf(fp, "   type attr_t is mod 2 ** %lu;\n", (long unsigned)(8 * sizeof(attr_t)));
  PRINT_NAMED_BITMASK(attr_t, A_CHARTEXT);
  PRINT_NAMED_BITMASK(attr_t, A_COLOR);
  PRINT_BITMASK(attr_t, "Attr", A_ATTRIBUTES & ~A_COLOR);
  PRINT_NAMED_BITMASK(attr_t, A_STANDOUT);
  PRINT_NAMED_BITMASK(attr_t, A_UNDERLINE);
  PRINT_NAMED_BITMASK(attr_t, A_REVERSE);
  PRINT_NAMED_BITMASK(attr_t, A_BLINK);
  PRINT_NAMED_BITMASK(attr_t, A_DIM);
  PRINT_NAMED_BITMASK(attr_t, A_BOLD);
  PRINT_NAMED_BITMASK(attr_t, A_PROTECT);
  PRINT_NAMED_BITMASK(attr_t, A_INVIS);
  PRINT_NAMED_BITMASK(attr_t, A_ALTCHARSET);
  PRINT_NAMED_BITMASK(attr_t, A_HORIZONTAL);
  PRINT_NAMED_BITMASK(attr_t, A_LEFT);
  PRINT_NAMED_BITMASK(attr_t, A_LOW);
  PRINT_NAMED_BITMASK(attr_t, A_RIGHT);
  PRINT_NAMED_BITMASK(attr_t, A_TOP);
  PRINT_NAMED_BITMASK(attr_t, A_VERTICAL);
  print_size_of(fp, "chtype_Size", 8 * sizeof(chtype));

  print_comment(fp, "predefined color numbers from color(3NCURSES)");
  PRINT_NAMED_CONSTANT(COLOR_BLACK);
  PRINT_NAMED_CONSTANT(COLOR_RED);
  PRINT_NAMED_CONSTANT(COLOR_GREEN);
  PRINT_NAMED_CONSTANT(COLOR_YELLOW);
  PRINT_NAMED_CONSTANT(COLOR_BLUE);
  PRINT_NAMED_CONSTANT(COLOR_MAGENTA);
  PRINT_NAMED_CONSTANT(COLOR_CYAN);
  PRINT_NAMED_CONSTANT(COLOR_WHITE);

  print_comment(fp, "ETI return codes from ncurses.h");
  PRINT_NAMED_CONSTANT(E_OK);
  PRINT_NAMED_CONSTANT(E_SYSTEM_ERROR);
  PRINT_NAMED_CONSTANT(E_BAD_ARGUMENT);
  PRINT_NAMED_CONSTANT(E_POSTED);
  PRINT_NAMED_CONSTANT(E_CONNECTED);
  PRINT_NAMED_CONSTANT(E_BAD_STATE);
  PRINT_NAMED_CONSTANT(E_NO_ROOM);
  PRINT_NAMED_CONSTANT(E_NOT_POSTED);
  PRINT_NAMED_CONSTANT(E_UNKNOWN_COMMAND);
  PRINT_NAMED_CONSTANT(E_NO_MATCH);
  PRINT_NAMED_CONSTANT(E_NOT_SELECTABLE);
  PRINT_NAMED_CONSTANT(E_NOT_CONNECTED);
  PRINT_NAMED_CONSTANT(E_REQUEST_DENIED);
  PRINT_NAMED_CONSTANT(E_INVALID_FIELD);
  PRINT_NAMED_CONSTANT(E_CURRENT);

  print_comment(fp, "Input key codes not defined in any ncurses manpage");
  PRINT_NAMED_CONSTANT(KEY_MIN);
  PRINT_NAMED_CONSTANT(KEY_MAX);
#ifdef KEY_CODE_YES
  PRINT_NAMED_CONSTANT(KEY_CODE_YES);
#endif

  print_comment(fp, "Input key codes from getch(3NCURSES)");
  PRINT_NAMED_CONSTANT(KEY_BREAK);
  PRINT_NAMED_CONSTANT(KEY_DOWN);
  PRINT_NAMED_CONSTANT(KEY_UP);
  PRINT_NAMED_CONSTANT(KEY_LEFT);
  PRINT_NAMED_CONSTANT(KEY_RIGHT);
  PRINT_NAMED_CONSTANT(KEY_HOME);
  PRINT_NAMED_CONSTANT(KEY_BACKSPACE);
  PRINT_NAMED_CONSTANT(KEY_F0);
#define PRINT_NAMED_FUNC_KEY(name) print_constant(fp, "KEY_F"#name, KEY_F(name))
  PRINT_NAMED_FUNC_KEY(1);
  PRINT_NAMED_FUNC_KEY(2);
  PRINT_NAMED_FUNC_KEY(3);
  PRINT_NAMED_FUNC_KEY(4);
  PRINT_NAMED_FUNC_KEY(5);
  PRINT_NAMED_FUNC_KEY(6);
  PRINT_NAMED_FUNC_KEY(7);
  PRINT_NAMED_FUNC_KEY(8);
  PRINT_NAMED_FUNC_KEY(9);
  PRINT_NAMED_FUNC_KEY(10);
  PRINT_NAMED_FUNC_KEY(11);
  PRINT_NAMED_FUNC_KEY(12);
  PRINT_NAMED_FUNC_KEY(13);
  PRINT_NAMED_FUNC_KEY(14);
  PRINT_NAMED_FUNC_KEY(15);
  PRINT_NAMED_FUNC_KEY(16);
  PRINT_NAMED_FUNC_KEY(17);
  PRINT_NAMED_FUNC_KEY(18);
  PRINT_NAMED_FUNC_KEY(19);
  PRINT_NAMED_FUNC_KEY(20);
  PRINT_NAMED_FUNC_KEY(21);
  PRINT_NAMED_FUNC_KEY(22);
  PRINT_NAMED_FUNC_KEY(23);
  PRINT_NAMED_FUNC_KEY(24);
  PRINT_NAMED_CONSTANT(KEY_DL);
  PRINT_NAMED_CONSTANT(KEY_IL);
  PRINT_NAMED_CONSTANT(KEY_DC);
  PRINT_NAMED_CONSTANT(KEY_IC);
  PRINT_NAMED_CONSTANT(KEY_EIC);
  PRINT_NAMED_CONSTANT(KEY_CLEAR);
  PRINT_NAMED_CONSTANT(KEY_EOS);
  PRINT_NAMED_CONSTANT(KEY_EOL);
  PRINT_NAMED_CONSTANT(KEY_SF);
  PRINT_NAMED_CONSTANT(KEY_SR);
  PRINT_NAMED_CONSTANT(KEY_NPAGE);
  PRINT_NAMED_CONSTANT(KEY_PPAGE);
  PRINT_NAMED_CONSTANT(KEY_STAB);
  PRINT_NAMED_CONSTANT(KEY_CTAB);
  PRINT_NAMED_CONSTANT(KEY_CATAB);
  PRINT_NAMED_CONSTANT(KEY_ENTER);
  PRINT_NAMED_CONSTANT(KEY_SRESET);
  PRINT_NAMED_CONSTANT(KEY_RESET);
  PRINT_NAMED_CONSTANT(KEY_PRINT);
  PRINT_NAMED_CONSTANT(KEY_LL);
  PRINT_NAMED_CONSTANT(KEY_A1);
  PRINT_NAMED_CONSTANT(KEY_A3);
  PRINT_NAMED_CONSTANT(KEY_B2);
  PRINT_NAMED_CONSTANT(KEY_C1);
  PRINT_NAMED_CONSTANT(KEY_C3);
  PRINT_NAMED_CONSTANT(KEY_BTAB);
  PRINT_NAMED_CONSTANT(KEY_BEG);
  PRINT_NAMED_CONSTANT(KEY_CANCEL);
  PRINT_NAMED_CONSTANT(KEY_CLOSE);
  PRINT_NAMED_CONSTANT(KEY_COMMAND);
  PRINT_NAMED_CONSTANT(KEY_COPY);
  PRINT_NAMED_CONSTANT(KEY_CREATE);
  PRINT_NAMED_CONSTANT(KEY_END);
  PRINT_NAMED_CONSTANT(KEY_EXIT);
  PRINT_NAMED_CONSTANT(KEY_FIND);
  PRINT_NAMED_CONSTANT(KEY_HELP);
  PRINT_NAMED_CONSTANT(KEY_MARK);
  PRINT_NAMED_CONSTANT(KEY_MESSAGE);
  PRINT_NAMED_CONSTANT(KEY_MOVE);
  PRINT_NAMED_CONSTANT(KEY_NEXT);
  PRINT_NAMED_CONSTANT(KEY_OPEN);
  PRINT_NAMED_CONSTANT(KEY_OPTIONS);
  PRINT_NAMED_CONSTANT(KEY_PREVIOUS);
  PRINT_NAMED_CONSTANT(KEY_REDO);
  PRINT_NAMED_CONSTANT(KEY_REFERENCE);
  PRINT_NAMED_CONSTANT(KEY_REFRESH);
  PRINT_NAMED_CONSTANT(KEY_REPLACE);
  PRINT_NAMED_CONSTANT(KEY_RESTART);
  PRINT_NAMED_CONSTANT(KEY_RESUME);
  PRINT_NAMED_CONSTANT(KEY_SAVE);
  PRINT_NAMED_CONSTANT(KEY_SBEG);
  PRINT_NAMED_CONSTANT(KEY_SCANCEL);
  PRINT_NAMED_CONSTANT(KEY_SCOMMAND);
  PRINT_NAMED_CONSTANT(KEY_SCOPY);
  PRINT_NAMED_CONSTANT(KEY_SCREATE);
  PRINT_NAMED_CONSTANT(KEY_SDC);
  PRINT_NAMED_CONSTANT(KEY_SDL);
  PRINT_NAMED_CONSTANT(KEY_SELECT);
  PRINT_NAMED_CONSTANT(KEY_SEND);
  PRINT_NAMED_CONSTANT(KEY_SEOL);
  PRINT_NAMED_CONSTANT(KEY_SEXIT);
  PRINT_NAMED_CONSTANT(KEY_SFIND);
  PRINT_NAMED_CONSTANT(KEY_SHELP);
  PRINT_NAMED_CONSTANT(KEY_SHOME);
  PRINT_NAMED_CONSTANT(KEY_SIC);
  PRINT_NAMED_CONSTANT(KEY_SLEFT);
  PRINT_NAMED_CONSTANT(KEY_SMESSAGE);
  PRINT_NAMED_CONSTANT(KEY_SMOVE);
  PRINT_NAMED_CONSTANT(KEY_SNEXT);
  PRINT_NAMED_CONSTANT(KEY_SOPTIONS);
  PRINT_NAMED_CONSTANT(KEY_SPREVIOUS);
  PRINT_NAMED_CONSTANT(KEY_SPRINT);
  PRINT_NAMED_CONSTANT(KEY_SREDO);
  PRINT_NAMED_CONSTANT(KEY_SREPLACE);
  PRINT_NAMED_CONSTANT(KEY_SRIGHT);
  PRINT_NAMED_CONSTANT(KEY_SRSUME);
  PRINT_NAMED_CONSTANT(KEY_SSAVE);
  PRINT_NAMED_CONSTANT(KEY_SSUSPEND);
  PRINT_NAMED_CONSTANT(KEY_SUNDO);
  PRINT_NAMED_CONSTANT(KEY_SUSPEND);
  PRINT_NAMED_CONSTANT(KEY_UNDO);
  PRINT_NAMED_CONSTANT(KEY_MOUSE);
  PRINT_NAMED_CONSTANT(KEY_RESIZE);

  print_comment(fp, "alternate character codes (ACS) from addch(3NCURSES)");
#define PRINT_ACS(name) print_size_of (fp, #name, (size_t)(&name - &acs_map[0]))
  PRINT_ACS(ACS_ULCORNER);
  PRINT_ACS(ACS_LLCORNER);
  PRINT_ACS(ACS_URCORNER);
  PRINT_ACS(ACS_LRCORNER);
  PRINT_ACS(ACS_LTEE);
  PRINT_ACS(ACS_RTEE);
  PRINT_ACS(ACS_BTEE);
  PRINT_ACS(ACS_TTEE);
  PRINT_ACS(ACS_HLINE);
  PRINT_ACS(ACS_VLINE);
  PRINT_ACS(ACS_PLUS);
  PRINT_ACS(ACS_S1);
  PRINT_ACS(ACS_S9);
  PRINT_ACS(ACS_DIAMOND);
  PRINT_ACS(ACS_CKBOARD);
  PRINT_ACS(ACS_DEGREE);
  PRINT_ACS(ACS_PLMINUS);
  PRINT_ACS(ACS_BULLET);
  PRINT_ACS(ACS_LARROW);
  PRINT_ACS(ACS_RARROW);
  PRINT_ACS(ACS_DARROW);
  PRINT_ACS(ACS_UARROW);
  PRINT_ACS(ACS_BOARD);
  PRINT_ACS(ACS_LANTERN);
  PRINT_ACS(ACS_BLOCK);
  PRINT_ACS(ACS_S3);
  PRINT_ACS(ACS_S7);
  PRINT_ACS(ACS_LEQUAL);
  PRINT_ACS(ACS_GEQUAL);
  PRINT_ACS(ACS_PI);
  PRINT_ACS(ACS_NEQUAL);
  PRINT_ACS(ACS_STERLING);

  print_comment(fp, "Menu_Options from opts(3MENU)");
  PRINT_NAMED_BITMASK(Menu_Options, O_ONEVALUE);
  PRINT_NAMED_BITMASK(Menu_Options, O_SHOWDESC);
  PRINT_NAMED_BITMASK(Menu_Options, O_ROWMAJOR);
  PRINT_NAMED_BITMASK(Menu_Options, O_IGNORECASE);
  PRINT_NAMED_BITMASK(Menu_Options, O_SHOWMATCH);
  PRINT_NAMED_BITMASK(Menu_Options, O_NONCYCLIC);
  print_size_of(fp, "Menu_Options_Size", 8 * sizeof(Menu_Options));

  print_comment(fp, "Item_Options from menu_opts(3MENU)");
  PRINT_NAMED_BITMASK(Item_Options, O_SELECTABLE);
  print_size_of(fp, "Item_Options_Size", 8 * sizeof(Item_Options));

  print_comment(fp, "Field_Options from field_opts(3FORM)");
  PRINT_NAMED_BITMASK(Field_Options, O_VISIBLE);
  PRINT_NAMED_BITMASK(Field_Options, O_ACTIVE);
  PRINT_NAMED_BITMASK(Field_Options, O_PUBLIC);
  PRINT_NAMED_BITMASK(Field_Options, O_EDIT);
  PRINT_NAMED_BITMASK(Field_Options, O_WRAP);
  PRINT_NAMED_BITMASK(Field_Options, O_BLANK);
  PRINT_NAMED_BITMASK(Field_Options, O_AUTOSKIP);
  PRINT_NAMED_BITMASK(Field_Options, O_NULLOK);
  PRINT_NAMED_BITMASK(Field_Options, O_PASSOK);
  PRINT_NAMED_BITMASK(Field_Options, O_STATIC);
  print_size_of(fp, "Field_Options_Size", 8 * sizeof(Field_Options));

  print_comment(fp, "Field_Options from opts(3FORM)");
  PRINT_NAMED_BITMASK(Field_Options, O_NL_OVERLOAD);
  PRINT_NAMED_BITMASK(Field_Options, O_BS_OVERLOAD);
  /*  Field_Options_Size is defined below */

  print_comment(fp, "MEVENT structure from mouse(3NCURSES)");
  STRUCT_OFFSET(MEVENT, id);
  STRUCT_OFFSET(MEVENT, x);
  STRUCT_OFFSET(MEVENT, y);
  STRUCT_OFFSET(MEVENT, z);
  STRUCT_OFFSET(MEVENT, bstate);
  print_size_of(fp, "MEVENT_Size", 8 * sizeof(MEVENT));

  print_comment(fp, "mouse events from mouse(3NCURSES)");
  {
    mmask_t all_events;

#define PRINT_MOUSE_EVENT(event)                \
    print_constant (fp, #event, event);         \
    all_events |= event

    all_events = 0;
    PRINT_MOUSE_EVENT(BUTTON1_RELEASED);
    PRINT_MOUSE_EVENT(BUTTON1_PRESSED);
    PRINT_MOUSE_EVENT(BUTTON1_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON1_DOUBLE_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON1_TRIPLE_CLICKED);
#ifdef BUTTON1_RESERVED_EVENT
    PRINT_MOUSE_EVENT(BUTTON1_RESERVED_EVENT);
#endif
    print_constant(fp, "all_events_button_1", (UINT) all_events);

    all_events = 0;
    PRINT_MOUSE_EVENT(BUTTON2_RELEASED);
    PRINT_MOUSE_EVENT(BUTTON2_PRESSED);
    PRINT_MOUSE_EVENT(BUTTON2_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON2_DOUBLE_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON2_TRIPLE_CLICKED);
#ifdef BUTTON2_RESERVED_EVENT
    PRINT_MOUSE_EVENT(BUTTON2_RESERVED_EVENT);
#endif
    print_constant(fp, "all_events_button_2", (UINT) all_events);

    all_events = 0;
    PRINT_MOUSE_EVENT(BUTTON3_RELEASED);
    PRINT_MOUSE_EVENT(BUTTON3_PRESSED);
    PRINT_MOUSE_EVENT(BUTTON3_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON3_DOUBLE_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON3_TRIPLE_CLICKED);
#ifdef BUTTON3_RESERVED_EVENT
    PRINT_MOUSE_EVENT(BUTTON3_RESERVED_EVENT);
#endif
    print_constant(fp, "all_events_button_3", (UINT) all_events);

    all_events = 0;
    PRINT_MOUSE_EVENT(BUTTON4_RELEASED);
    PRINT_MOUSE_EVENT(BUTTON4_PRESSED);
    PRINT_MOUSE_EVENT(BUTTON4_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON4_DOUBLE_CLICKED);
    PRINT_MOUSE_EVENT(BUTTON4_TRIPLE_CLICKED);
#ifdef BUTTON4_RESERVED_EVENT
    PRINT_MOUSE_EVENT(BUTTON4_RESERVED_EVENT);
#endif
    print_constant(fp, "all_events_button_4", (UINT) all_events);
  }
  PRINT_NAMED_CONSTANT(BUTTON_CTRL);
  PRINT_NAMED_CONSTANT(BUTTON_SHIFT);
  PRINT_NAMED_CONSTANT(BUTTON_ALT);
  PRINT_NAMED_CONSTANT(REPORT_MOUSE_POSITION);
  PRINT_NAMED_CONSTANT(ALL_MOUSE_EVENTS);

  print_comment(fp, "trace selection from trace(3NCURSES)");
  PRINT_NAMED_BITMASK(UINT, TRACE_TIMES);
  PRINT_NAMED_BITMASK(UINT, TRACE_TPUTS);
  PRINT_NAMED_BITMASK(UINT, TRACE_UPDATE);
  PRINT_NAMED_BITMASK(UINT, TRACE_MOVE);
  PRINT_NAMED_BITMASK(UINT, TRACE_CHARPUT);
  PRINT_NAMED_BITMASK(UINT, TRACE_CALLS);
  PRINT_NAMED_BITMASK(UINT, TRACE_VIRTPUT);
  PRINT_NAMED_BITMASK(UINT, TRACE_IEVENT);
  PRINT_NAMED_BITMASK(UINT, TRACE_BITS);
  PRINT_NAMED_BITMASK(UINT, TRACE_ICALLS);
  PRINT_NAMED_BITMASK(UINT, TRACE_CCALLS);
  PRINT_NAMED_BITMASK(UINT, TRACE_DATABASE);
  PRINT_NAMED_BITMASK(UINT, TRACE_ATTRS);
  print_size_of(fp, "Trace_Size", 8 * sizeof(UINT));

  fprintf(fp, "end Terminal_Interface.Curses_Constants;\n");
  exit(EXIT_SUCCESS);
}
