// * this is for making emacs happy: -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2011,2017 Free Software Foundation, Inc.                  *
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
 *   Author: Juergen Pfeifer, 1997                                          *
 ****************************************************************************/

#include "internal.h"
#include "cursesm.h"
#include "cursesapp.h"

MODULE_ID("$Id: cursesm.cc,v 1.27 2021/04/17 18:11:08 tom Exp $")

NCursesMenuItem::~NCursesMenuItem() THROWS(NCursesException)
{
  if (item)
    OnError(::free_item(item));
}

bool
NCursesMenuItem::action()
{
  return FALSE;
}

NCursesMenuCallbackItem::~NCursesMenuCallbackItem() THROWS(NCursesException)
{
}

bool
NCursesMenuCallbackItem::action()
{
  if (p_fct)
    return p_fct (*this);
  else
    return FALSE;
}

/* Internal hook functions. They will route the hook
 * calls to virtual methods of the NCursesMenu class,
 * so in C++ providing a hook is done simply by
 * implementing a virtual method in a derived class
 */
void
_nc_xx_mnu_init(MENU *m)
{
  NCursesMenu::getHook(m)->On_Menu_Init();
}

void
_nc_xx_mnu_term(MENU *m)
{
  NCursesMenu::getHook(m)->On_Menu_Termination();
}

void
_nc_xx_itm_init(MENU *m)
{
  NCursesMenu* M = NCursesMenu::getHook(m);
  M->On_Item_Init (*(M->current_item ()));
}

void
_nc_xx_itm_term(MENU *m)
{
  NCursesMenu* M = NCursesMenu::getHook(m);
  M->On_Item_Termination (*(M->current_item ()));
}

/* Construct an ITEM* array from an array of NCursesMenuItem
 * objects.
 */
ITEM**
NCursesMenu::mapItems(NCursesMenuItem* nitems[])
{
  int itemCount = 0,lcv;

  for (lcv=0; nitems[lcv]->item; ++lcv)
    ++itemCount;

  ITEM** itemArray = new ITEM*[itemCount + 1];

  for (lcv=0;nitems[lcv]->item;++lcv) {
    itemArray[lcv] = nitems[lcv]->item;
  }
  itemArray[lcv] = NULL;

  my_items = nitems;

  if (menu)
    delete[] ::menu_items(menu);
  return itemArray;
}

void
NCursesMenu::InitMenu(NCursesMenuItem* nitems[],
		      bool with_frame,
		      bool autoDelete_Items)
{
  int mrows, mcols;

  keypad(TRUE);
  meta(TRUE);

  b_framed = with_frame;
  b_autoDelete = autoDelete_Items;

  menu = static_cast<MENU*>(0);
  menu = ::new_menu(mapItems(nitems));
  if (!menu)
    OnError (E_SYSTEM_ERROR);

  UserHook* hook = new UserHook;
  hook->m_user   = NULL;
  hook->m_back   = this;
  hook->m_owner  = menu;
  ::set_menu_userptr(menu, static_cast<void*>(hook));

  ::set_menu_init (menu, _nc_xx_mnu_init);
  ::set_menu_term (menu, _nc_xx_mnu_term);
  ::set_item_init (menu, _nc_xx_itm_init);
  ::set_item_term (menu, _nc_xx_itm_term);

  scale(mrows, mcols);
  ::set_menu_win(menu, w);

  if (with_frame) {
    if ((mrows > height()-2) || (mcols > width()-2))
      OnError(E_NO_ROOM);
    sub = new NCursesWindow(*this,mrows,mcols,1,1,'r');
    ::set_menu_sub(menu, sub->w);
    b_sub_owner = TRUE;
  }
  else {
    sub = static_cast<NCursesWindow*>(0);
    b_sub_owner = FALSE;
  }
  setDefaultAttributes();
}

void
NCursesMenu::setDefaultAttributes()
{
  NCursesApplication* S = NCursesApplication::getApplication();
  if (S) {
    ::set_menu_fore(menu, S->foregrounds());
    ::set_menu_back(menu, S->backgrounds());
    ::set_menu_grey(menu, S->inactives());
  }
}

NCursesMenu::~NCursesMenu() THROWS(NCursesException)
{
  UserHook* hook = reinterpret_cast<UserHook*>(::menu_userptr(menu));
  delete hook;
  if (b_sub_owner) {
    ::set_menu_sub(menu, static_cast<WINDOW *>(0));
    delete sub;
  }
  if (menu) {
    ITEM** itms = ::menu_items(menu);
    int cnt = count();

    OnError(::set_menu_items(menu, static_cast<ITEM**>(0)));

    if (b_autoDelete) {
      if (cnt>0) {
	for (int i=0; i <= cnt; i++)
	  delete my_items[i];
      }
      delete[] my_items;
    }

    ::free_menu(menu);
    // It's essential to do this after free_menu()
    delete[] itms;
  }
}

void
NCursesMenu::setSubWindow(NCursesWindow& nsub)
{
  if (!isDescendant(nsub))
    OnError(E_SYSTEM_ERROR);
  else {
    if (b_sub_owner)
      delete sub;
    sub = &nsub;
    ::set_menu_sub(menu,sub->w);
  }
}

bool
NCursesMenu::set_pattern (const char *pat)
{
  int res = ::set_menu_pattern (menu, pat);
  switch(res) {
  case E_OK:
    break;
  case E_NO_MATCH:
    return FALSE;
  default:
    OnError (res);
  }
  return TRUE;
}

// call the menu driver and do basic error checking.
int
NCursesMenu::driver (int c)
{
  int res = ::menu_driver (menu, c);
  switch (res) {
  case E_OK:
  case E_REQUEST_DENIED:
  case E_NOT_SELECTABLE:
  case E_UNKNOWN_COMMAND:
  case E_NO_MATCH:
    break;
  default:
    OnError (res);
  }
  return (res);
}

static const int CMD_QUIT   = MAX_COMMAND + 1;
static const int CMD_ACTION = MAX_COMMAND + 2;
//
// -------------------------------------------------------------------------
// Provide a default key virtualization. Translate the keyboard
// code c into a menu request code.
// The default implementation provides a hopefully straightforward
// mapping for the most common keystrokes and menu requests.
// -------------------------------------------------------------------------
int
NCursesMenu::virtualize(int c)
{
  switch(c) {
  case CTRL('X')     : return(CMD_QUIT);              // eXit

  case KEY_DOWN      : return(REQ_DOWN_ITEM);
  case CTRL('N')     : return(REQ_NEXT_ITEM);         // Next
  case KEY_UP        : return(REQ_UP_ITEM);
  case CTRL('P')     : return(REQ_PREV_ITEM);         // Previous

  case CTRL('U')     : return(REQ_SCR_ULINE);         // Up
  case CTRL('D')     : return(REQ_SCR_DLINE);         // Down
  case CTRL('F')     : return(REQ_SCR_DPAGE);         // Forward
  case CTRL('B')     : return(REQ_SCR_UPAGE);         // Backward

  case CTRL('Y')     : return(REQ_CLEAR_PATTERN);
  case CTRL('H')     : return(REQ_BACK_PATTERN);
  case CTRL('A')     : return(REQ_NEXT_MATCH);
  case CTRL('E')     : return(REQ_PREV_MATCH);
  case CTRL('T')     : return(REQ_TOGGLE_ITEM);

  case CTRL('J')     :
  case CTRL('M')     : return(CMD_ACTION);

  case KEY_HOME      : return(REQ_FIRST_ITEM);
  case KEY_LEFT      : return(REQ_LEFT_ITEM);
  case KEY_RIGHT     : return(REQ_RIGHT_ITEM);
  case KEY_END       : return(REQ_LAST_ITEM);
  case KEY_BACKSPACE : return(REQ_BACK_PATTERN);
  case KEY_NPAGE     : return(REQ_SCR_DPAGE);
  case KEY_PPAGE     : return(REQ_SCR_UPAGE);

  default:
    return(c);
  }
}

NCursesMenuItem*
NCursesMenu::operator()(void)
{
  int drvCmnd;
  int c;
  bool b_action = FALSE;

  post();
  show();
  refresh();

  while (!b_action && ((drvCmnd = virtualize((c = getKey()))) != CMD_QUIT)) {
    int err;

    switch((err = driver(drvCmnd))) {
    case E_REQUEST_DENIED:
      On_Request_Denied(c);
      break;
    case E_NOT_SELECTABLE:
      On_Not_Selectable(c);
      break;
    case E_UNKNOWN_COMMAND:
      if (drvCmnd == CMD_ACTION) {
	if (options() & O_ONEVALUE) {
	  NCursesMenuItem* itm = current_item();
	  assert(itm != 0);
	  if (itm->options() & O_SELECTABLE)
	    {
	      b_action = itm->action();
	      refresh();
	    }
	  else
	    On_Not_Selectable(c);
	}
	else {
	  int n = count();
	  for(int i=0; i<n; i++) {
	    NCursesMenuItem* itm = my_items[i];
	    if (itm->value()) {
	      b_action |= itm->action();
	      refresh();
	    }
	  }
	}
      } else
	On_Unknown_Command(c);
      break;
    case E_NO_MATCH:
      On_No_Match(c);
      break;
    case E_OK:
      break;
    default:
      OnError(err);
    }
  }

  unpost();
  hide();
  refresh();
  if (options() & O_ONEVALUE)
    return my_items[::item_index (::current_item (menu))];
  else
    return NULL;
}

void
NCursesMenu::On_Menu_Init()
{
}

void
NCursesMenu::On_Menu_Termination()
{
}

void
NCursesMenu::On_Item_Init(NCursesMenuItem& item)
{
  (void) item;
}

void
NCursesMenu::On_Item_Termination(NCursesMenuItem& item)
{
  (void) item;
}

void
NCursesMenu::On_Request_Denied(int c) const
{
  (void) c;
  ::beep();
}

void
NCursesMenu::On_Not_Selectable(int c) const
{
  (void) c;
  ::beep();
}

void
NCursesMenu::On_No_Match(int c) const
{
  (void) c;
  ::beep();
}

void
NCursesMenu::On_Unknown_Command(int c) const
{
  (void) c;
  ::beep();
}
