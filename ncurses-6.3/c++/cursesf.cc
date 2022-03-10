// * this is for making emacs happy: -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2005,2011 Free Software Foundation, Inc.                  *
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
#include "cursesf.h"
#include "cursesapp.h"

MODULE_ID("$Id: cursesf.cc,v 1.26 2021/04/17 18:11:08 tom Exp $")

NCursesFormField::~NCursesFormField () THROWS(NCursesException)
{
  if (field)
    OnError(::free_field (field));
}

/* Construct a FIELD* array from an array of NCursesFormField
 * objects.
 */
FIELD**
NCursesForm::mapFields(NCursesFormField* nfields[])
{
  int fieldCount = 0,lcv;
  FIELD** old_fields;

  assert(nfields != 0);

  for (lcv=0; nfields[lcv]->field; ++lcv)
    ++fieldCount;

  FIELD** fields = new FIELD*[fieldCount + 1];

  for (lcv=0;nfields[lcv]->field;++lcv) {
    fields[lcv] = nfields[lcv]->field;
  }
  fields[lcv] = NULL;

  my_fields = nfields;

  if (form && (old_fields = ::form_fields(form))) {
    ::set_form_fields(form, static_cast<FIELD**>(0));
    delete[] old_fields;
  }
  return fields;
}

void NCursesForm::setDefaultAttributes()
{
  NCursesApplication* S = NCursesApplication::getApplication();

  int n = count();
  if (n > 0) {
    for(int i=0; i<n; i++) {
      NCursesFormField* f = (*this)[i];
      if ((f->options() & (O_EDIT|O_ACTIVE))==(O_EDIT|O_ACTIVE)) {
	if (S) {
	  f->set_foreground(S->foregrounds());
	  f->set_background(S->backgrounds());
	}
	f->set_pad_character('_');
      }
      else {
	if (S)
	  f->set_background(S->labels());
      }
    }
  }

  if (S) {
    bkgd(' '|S->dialog_backgrounds());
    if (sub)
      sub->bkgd(' '|S->dialog_backgrounds());
  }
}

void
NCursesForm::InitForm(NCursesFormField* nfields[],
		      bool with_frame,
		      bool autoDelete_Fields)
{
  int mrows, mcols;

  keypad(TRUE);
  meta(TRUE);

  b_framed = with_frame;
  b_autoDelete = autoDelete_Fields;

  form = static_cast<FORM*>(0);
  form = ::new_form(mapFields(nfields));
  if (!form)
    OnError (E_SYSTEM_ERROR);

  UserHook* hook = new UserHook;
  hook->m_user   = NULL;
  hook->m_back   = this;
  hook->m_owner  = form;
  ::set_form_userptr(form, reinterpret_cast<void*>(hook));

  ::set_form_init  (form, _nc_xx_frm_init);
  ::set_form_term  (form, _nc_xx_frm_term);
  ::set_field_init (form, _nc_xx_fld_init);
  ::set_field_term (form, _nc_xx_fld_term);

  scale(mrows, mcols);
  ::set_form_win(form, w);

  if (with_frame) {
    if ((mrows > height()-2) || (mcols > width()-2))
      OnError(E_NO_ROOM);
    sub = new NCursesWindow(*this,mrows,mcols,1,1,'r');
    ::set_form_sub(form, sub->w);
    b_sub_owner = TRUE;
  }
  else {
    sub = static_cast<NCursesWindow*>(0);
    b_sub_owner = FALSE;
  }
  options_on(O_NL_OVERLOAD);
  setDefaultAttributes();
}

NCursesForm::~NCursesForm() THROWS(NCursesException)
{
  UserHook* hook = reinterpret_cast<UserHook*>(::form_userptr(form));
  delete hook;
  if (b_sub_owner) {
    delete sub;
    ::set_form_sub(form, static_cast<WINDOW *>(0));
  }
  if (form) {
    FIELD** fields = ::form_fields(form);
    int cnt = count();

    OnError(::set_form_fields(form, static_cast<FIELD**>(0)));

    if (b_autoDelete) {
      if (cnt>0) {
	for (int i=0; i <= cnt; i++)
	  delete my_fields[i];
      }
      delete[] my_fields;
    }

    ::free_form(form);
    // It's essential to do this after free_form()
    delete[] fields;
  }
}

void
NCursesForm::setSubWindow(NCursesWindow& nsub)
{
  if (!isDescendant(nsub))
    OnError(E_SYSTEM_ERROR);
  else {
    if (b_sub_owner)
      delete sub;
    sub = &nsub;
    ::set_form_sub(form,sub->w);
  }
}

/* Internal hook functions. They will route the hook
 * calls to virtual methods of the NCursesForm class,
 * so in C++ providing a hook is done simply by
 * implementing a virtual method in a derived class
 */
void
_nc_xx_frm_init(FORM *f)
{
  NCursesForm::getHook(f)->On_Form_Init();
}

void
_nc_xx_frm_term(FORM *f)
{
  NCursesForm::getHook(f)->On_Form_Termination();
}

void
_nc_xx_fld_init(FORM *f)
{
  NCursesForm* F = NCursesForm::getHook(f);
  F->On_Field_Init (*(F->current_field ()));
}

void
_nc_xx_fld_term(FORM *f)
{
  NCursesForm* F = NCursesForm::getHook(f);
  F->On_Field_Termination (*(F->current_field ()));
}

void
NCursesForm::On_Form_Init()
{
}

void
NCursesForm::On_Form_Termination()
{
}

void
NCursesForm::On_Field_Init(NCursesFormField& field)
{
  (void) field;
}

void
NCursesForm::On_Field_Termination(NCursesFormField& field)
{
  (void) field;
}

// call the form driver and do basic error checking.
int
NCursesForm::driver (int c)
{
  int res = ::form_driver (form, c);
  switch (res) {
  case E_OK:
  case E_REQUEST_DENIED:
  case E_INVALID_FIELD:
  case E_UNKNOWN_COMMAND:
    break;
  default:
    OnError (res);
  }
  return (res);
}

void NCursesForm::On_Request_Denied(int c) const
{
  (void) c;
  ::beep();
}

void NCursesForm::On_Invalid_Field(int c) const
{
  (void) c;
  ::beep();
}

void NCursesForm::On_Unknown_Command(int c) const
{
  (void) c;
  ::beep();
}

static const int CMD_QUIT = MAX_COMMAND + 1;

NCursesFormField*
NCursesForm::operator()(void)
{
  int drvCmnd;
  int c;

  post();
  show();
  refresh();

  while (((drvCmnd = virtualize((c = getKey()))) != CMD_QUIT)) {
    int err;
    switch((err = driver(drvCmnd))) {
    case E_REQUEST_DENIED:
      On_Request_Denied(c);
      break;
    case E_INVALID_FIELD:
      On_Invalid_Field(c);
      break;
    case E_UNKNOWN_COMMAND:
      On_Unknown_Command(c);
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
  return my_fields[::field_index (::current_field (form))];
}

// Provide a default key virtualization. Translate the keyboard
// code c into a form request code.
// The default implementation provides a hopefully straightforward
// mapping for the most common keystrokes and form requests.
int
NCursesForm::virtualize(int c)
{
  switch(c) {

  case KEY_HOME      : return(REQ_FIRST_FIELD);
  case KEY_END       : return(REQ_LAST_FIELD);

  case KEY_DOWN      : return(REQ_DOWN_CHAR);
  case KEY_UP        : return(REQ_UP_CHAR);
  case KEY_LEFT      : return(REQ_PREV_CHAR);
  case KEY_RIGHT     : return(REQ_NEXT_CHAR);

  case KEY_NPAGE     : return(REQ_NEXT_PAGE);
  case KEY_PPAGE     : return(REQ_PREV_PAGE);

  case KEY_BACKSPACE : return(REQ_DEL_PREV);
  case KEY_ENTER     : return(REQ_NEW_LINE);
  case KEY_CLEAR     : return(REQ_CLR_FIELD);

  case CTRL('X')     : return(CMD_QUIT);        // eXit

  case CTRL('F')     : return(REQ_NEXT_FIELD);  // Forward
  case CTRL('B')     : return(REQ_PREV_FIELD);  // Backward
  case CTRL('L')     : return(REQ_LEFT_FIELD);  // Left
  case CTRL('R')     : return(REQ_RIGHT_FIELD); // Right
  case CTRL('U')     : return(REQ_UP_FIELD);    // Up
  case CTRL('D')     : return(REQ_DOWN_FIELD);  // Down

  case CTRL('W')     : return(REQ_NEXT_WORD);
  case CTRL('T')     : return(REQ_PREV_WORD);

  case CTRL('A')     : return(REQ_BEG_FIELD);
  case CTRL('E')     : return(REQ_END_FIELD);

  case CTRL('I')     : return(REQ_INS_CHAR);
  case CTRL('M')     :
  case CTRL('J')     : return(REQ_NEW_LINE);
  case CTRL('O')     : return(REQ_INS_LINE);
  case CTRL('V')     : return(REQ_DEL_CHAR);
  case CTRL('H')     : return(REQ_DEL_PREV);
  case CTRL('Y')     : return(REQ_DEL_LINE);
  case CTRL('G')     : return(REQ_DEL_WORD);
  case CTRL('K')     : return(REQ_CLR_EOF);

  case CTRL('N')     : return(REQ_NEXT_CHOICE);
  case CTRL('P')     : return(REQ_PREV_CHOICE);

  default:
    return(c);
  }
}
//
// -------------------------------------------------------------------------
// User Defined Fieldtypes
// -------------------------------------------------------------------------
//
bool _nc_xx_fld_fcheck(FIELD *f, const void *u)
{
  (void) f;
  NCursesFormField* F = reinterpret_cast<NCursesFormField*>(const_cast<void *>(u));
  assert(F != 0);
  UserDefinedFieldType* udf = reinterpret_cast<UserDefinedFieldType*>(F->fieldtype());
  assert(udf != 0);
  return udf->field_check(*F);
}

bool _nc_xx_fld_ccheck(int c, const void *u)
{
  NCursesFormField* F = reinterpret_cast<NCursesFormField*>(const_cast<void *>(u));
  assert(F != 0);
  UserDefinedFieldType* udf =
    reinterpret_cast<UserDefinedFieldType*>(F->fieldtype());
  assert(udf != 0);
  return udf->char_check(c);
}

void* _nc_xx_fld_makearg(va_list* va)
{
  return va_arg(*va,NCursesFormField*);
}

FIELDTYPE* UserDefinedFieldType::generic_fieldtype =
  ::new_fieldtype(_nc_xx_fld_fcheck,
		  _nc_xx_fld_ccheck);


UserDefinedFieldType::UserDefinedFieldType() : NCursesFieldType(generic_fieldtype) {
}

FIELDTYPE* UserDefinedFieldType_With_Choice::generic_fieldtype_with_choice =
  ::new_fieldtype(_nc_xx_fld_fcheck,
		  _nc_xx_fld_ccheck);


UserDefinedFieldType_With_Choice::UserDefinedFieldType_With_Choice() {
  fieldtype = generic_fieldtype_with_choice;
}

bool _nc_xx_next_choice(FIELD *f, const void *u)
{
  (void) f;
  NCursesFormField* F = reinterpret_cast<NCursesFormField*>(const_cast<void *>(u));
  assert(F != 0);
  UserDefinedFieldType_With_Choice* udf =
    reinterpret_cast<UserDefinedFieldType_With_Choice*>(F->fieldtype());
  assert(udf != 0);
  return udf->next(*F);
}

bool _nc_xx_prev_choice(FIELD *f, const void *u)
{
  (void) f;
  NCursesFormField* F = reinterpret_cast<NCursesFormField*>(const_cast<void *>(u));
  assert(F != 0);
  UserDefinedFieldType_With_Choice* udf =
    reinterpret_cast<UserDefinedFieldType_With_Choice*>(F->fieldtype());
  assert(udf != 0);
  return udf->previous(*F);
}

class UDF_Init
{
private:
  int code;
  static UDF_Init* I;

public:
  UDF_Init()
    : code(0)
  {
    code = ::set_fieldtype_arg(UserDefinedFieldType::generic_fieldtype,
			       _nc_xx_fld_makearg,
			       NULL,
			       NULL);
    if (code==E_OK)
      code = ::set_fieldtype_arg
	(UserDefinedFieldType_With_Choice::generic_fieldtype_with_choice,
	 _nc_xx_fld_makearg,
	 NULL,
	 NULL);
    if (code==E_OK)
      code = ::set_fieldtype_choice
	(UserDefinedFieldType_With_Choice::generic_fieldtype_with_choice,
	 _nc_xx_next_choice,
	 _nc_xx_prev_choice);
  }
};

UDF_Init* UDF_Init::I = new UDF_Init();


