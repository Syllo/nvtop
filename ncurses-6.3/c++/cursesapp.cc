// * this is for making emacs happy: -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 1998-2007,2008 Free Software Foundation, Inc.                  *
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
 *      and: Thomas E. Dickey                                               *
 ****************************************************************************/

#include "internal.h"
#include "cursesapp.h"

MODULE_ID("$Id: cursesapp.cc,v 1.18 2020/07/18 19:57:11 anonymous.maarten Exp $")

void
NCursesApplication::init(bool bColors)
{
  if (bColors)
    NCursesWindow::useColors();

  if (Root_Window->colors() > 1) {
    b_Colors = TRUE;
    Root_Window->setcolor(1);
    Root_Window->setpalette(COLOR_YELLOW,COLOR_BLUE);
    Root_Window->setcolor(2);
    Root_Window->setpalette(COLOR_CYAN,COLOR_BLUE);
    Root_Window->setcolor(3);
    Root_Window->setpalette(COLOR_BLACK,COLOR_BLUE);
    Root_Window->setcolor(4);
    Root_Window->setpalette(COLOR_BLACK,COLOR_CYAN);
    Root_Window->setcolor(5);
    Root_Window->setpalette(COLOR_BLUE,COLOR_YELLOW);
    Root_Window->setcolor(6);
    Root_Window->setpalette(COLOR_BLACK,COLOR_GREEN);
  }
  else
    b_Colors = FALSE;

  Root_Window->bkgd(' '|window_backgrounds());
}

NCursesApplication* NCursesApplication::theApp = 0;
NCursesWindow* NCursesApplication::titleWindow = 0;
NCursesApplication::SLK_Link* NCursesApplication::slk_stack = 0;


NCursesWindow *&NCursesApplication::getTitleWindow() {
  return titleWindow;
}

NCursesApplication::~NCursesApplication() THROWS(NCursesException)
{
  Soft_Label_Key_Set* S;

  delete titleWindow;
  titleWindow = 0;

  while( (S=top()) ) {
    pop();
    delete S;
  }

  delete Root_Window;
  Root_Window = 0;

  ::endwin();
}

NCursesApplication* NCursesApplication::getApplication() {
  return theApp;
}

int NCursesApplication::rinit(NCursesWindow& w)
{
  titleWindow = &w;
  return OK;
}

void NCursesApplication::push(Soft_Label_Key_Set& S)
{
  SLK_Link* L = new SLK_Link;
  assert(L != 0);
  L->prev = slk_stack;
  L->SLKs = &S;
  slk_stack = L;
  if (Root_Window)
    S.show();
}

bool NCursesApplication::pop()
{
  if (slk_stack) {
    SLK_Link* L = slk_stack;
    slk_stack = slk_stack->prev;
    delete L;
    if (Root_Window) {
      Soft_Label_Key_Set* xx = top();
      if (xx != 0)
        xx->show();
    }
  }
  return (slk_stack ? FALSE : TRUE);
}

Soft_Label_Key_Set* NCursesApplication::top() const
{
  if (slk_stack)
    return slk_stack->SLKs;
  else
    return static_cast<Soft_Label_Key_Set*>(0);
}

int NCursesApplication::operator()(void)
{
  bool bColors = b_Colors;
  Soft_Label_Key_Set* S = 0;

  int ts = titlesize();
  if (ts>0)
    NCursesWindow::ripoffline(ts,rinit);
  Soft_Label_Key_Set::Label_Layout fmt = useSLKs();
  if (fmt!=Soft_Label_Key_Set::None) {
    S = new Soft_Label_Key_Set(fmt);
    assert(S != 0);
    init_labels(*S);
  }

  Root_Window = new NCursesWindow(::stdscr);
  init(bColors);

  if (ts>0)
    title();
  if (fmt!=Soft_Label_Key_Set::None) {
    push(*S);
  }

  return run();
}

NCursesApplication::NCursesApplication(bool bColors)
  : b_Colors(bColors),
    Root_Window(NULL)
{
  if (theApp)
    THROW(new NCursesException("Application object already created."));
  else
    theApp = this;
}
