/* The lwlib interface to Motif widgets.
   Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <X11/CoreP.h>
#include <X11/CompositeP.h>

#include "lwlib-Xm.h"
#include "lwlib-utils.h"

#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/DrawingA.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ArrowB.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>

static void xm_pull_down_callback (/* Widget, XtPointer, XtPointer */);
static void xm_internal_update_other_instances (/* Widget, XtPointer,
						      XtPointer */);
static void xm_generic_callback (/* Widget, XtPointer, XtPointer */);
static void xm_nosel_callback (/* Widget, XtPointer, XtPointer */);
static void xm_pop_down_callback (/* Widget, XtPointer, XtPointer */);

static void xm_update_menu (/* widget_instance*, Widget, widget_value*,
				  Boolean) */);


/* Structures to keep destroyed instances */
typedef struct _destroyed_instance 
{
  char*		name;
  char*		type;
  Widget 	widget;
  Widget	parent;
  Boolean	pop_up_p;
  struct _destroyed_instance*	next;
} destroyed_instance;

static destroyed_instance*
all_destroyed_instances = NULL;

static destroyed_instance*
make_destroyed_instance (name, type, widget, parent, pop_up_p)
     char* name;
     char* type;
     Widget widget;
     Widget parent;
     Boolean pop_up_p;
{
  destroyed_instance* instance =
    (destroyed_instance*)malloc (sizeof (destroyed_instance));
  instance->name = safe_strdup (name);
  instance->type = safe_strdup (type);
  instance->widget = widget;
  instance->parent = parent;
  instance->pop_up_p = pop_up_p;
  instance->next = NULL;
  return instance;
}
			 
static void
free_destroyed_instance (instance)
     destroyed_instance* instance;
{
  free (instance->name);
  free (instance->type);
  free (instance);
}

/* motif utility functions */
Widget
first_child (widget)
     Widget widget;
{
  return ((CompositeWidget)widget)->composite.children [0];
}

Boolean
lw_motif_widget_p (widget)
     Widget widget;
{
  return 
    XtClass (widget) == xmDialogShellWidgetClass
      || XmIsPrimitive (widget) || XmIsManager (widget) || XmIsGadget (widget);
}

static XmString
resource_motif_string (widget, name)
     Widget widget;
     char* name;
{
  XtResource resource;
  XmString result = 0;
  
  resource.resource_name = name;
  resource.resource_class = XmCXmString;
  resource.resource_type = XmRXmString;
  resource.resource_size = sizeof (XmString);
  resource.resource_offset = 0;
  resource.default_type = XtRImmediate;
  resource.default_addr = 0;

  XtGetSubresources (widget, (XtPointer)&result, "dialogString",
		     "DialogString", &resource, 1, NULL, 0);
  return result;
}

static void
destroy_all_children (widget)
     Widget widget;
{
  Widget* children;
  unsigned int number;
  int i;

  children = XtCompositeChildren (widget, &number);
  if (children)
    {
      /* Unmanage all children and destroy them.  They will only be 
       * really destroyed when we get out of DispatchEvent. */
      for (i = 0; i < number; i++)
	{
	  Widget child = children [i];
	  if (!child->core.being_destroyed)
	    {
	      XtUnmanageChild (child);
	      XtDestroyWidget (child);
	    }
	}
      XtFree ((char *) children);
    }
}

/* update the label of anything subclass of a label */
static void
xm_update_label (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  XmString res_string = 0;
  XmString built_string = 0;
  XmString key_string = 0;
  Arg al [256];
  int ac;
  
  ac = 0;

  if (val->value)
    {
      res_string = resource_motif_string (widget, val->value);

      if (res_string)
	{
	  XtSetArg (al [ac], XmNlabelString, res_string); ac++;
	}
      else
	{
	  built_string =
	    XmStringCreateLtoR (val->value, XmSTRING_DEFAULT_CHARSET);
	  XtSetArg (al [ac], XmNlabelString, built_string); ac++;
	}
      XtSetArg (al [ac], XmNlabelType, XmSTRING); ac++;
    }
  
  if (val->key)
    {
      key_string = XmStringCreateLtoR (val->key, XmSTRING_DEFAULT_CHARSET);
      XtSetArg (al [ac], XmNacceleratorText, key_string); ac++;
    }

  if (ac)
    XtSetValues (widget, al, ac);

  if (built_string)
    XmStringFree (built_string);

  if (key_string)
    XmStringFree (key_string);
}

/* update of list */
static void
xm_update_list (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  widget_value* cur;
  int i;
  XtRemoveAllCallbacks (widget, XmNsingleSelectionCallback);
  XtAddCallback (widget, XmNsingleSelectionCallback, xm_generic_callback,
		 instance);
  for (cur = val->contents, i = 0; cur; cur = cur->next)
    if (cur->value)
      {
	XmString xmstr = XmStringCreate (cur->value, XmSTRING_DEFAULT_CHARSET);
	i += 1;
	XmListAddItem (widget, xmstr, 0);
	if (cur->selected)
	  XmListSelectPos (widget, i, False);
	XmStringFree (xmstr);
      }
}

/* update of buttons */
static void
xm_update_pushbutton (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  XtVaSetValues (widget, XmNalignment, XmALIGNMENT_CENTER, 0);
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
}

static void
xm_update_cascadebutton (widget_instance* instance, Widget widget,
			 widget_value* val)
{
  /* Should also rebuild the menu by calling ...update_menu... */
  XtRemoveAllCallbacks (widget, XmNcascadingCallback);
  XtAddCallback (widget, XmNcascadingCallback, xm_pull_down_callback,
		 instance);
}

/* update toggle and radiobox */
static void
xm_update_toggle (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_internal_update_other_instances, instance);
  XtVaSetValues (widget, XmNset, val->selected,
		 XmNalignment, XmALIGNMENT_BEGINNING, 0);
}

static void
xm_update_radiobox (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;

{
  Widget toggle;
  widget_value* cur;

  /* update the callback */
  XtRemoveAllCallbacks (widget, XmNentryCallback);
  XtAddCallback (widget, XmNentryCallback, xm_generic_callback, instance);

  /* first update all the toggles */
  /* Energize kernel interface is currently bad.  It sets the selected widget
     with the selected flag but returns it by its name.  So we currently
     have to support both setting the selection with the selected slot
     of val contents and setting it with the "value" slot of val.  The latter
     has a higher priority.  This to be removed when the kernel is fixed. */
  for (cur = val->contents; cur; cur = cur->next)
    {
      toggle = XtNameToWidget (widget, cur->value);
      if (toggle)
	{
	  XtVaSetValues (toggle, XmNsensitive, cur->enabled, 0);
	  if (!val->value && cur->selected)
	    XtVaSetValues (toggle, XmNset, cur->selected, 0);
	  if (val->value && strcmp (val->value, cur->value))
	    XtVaSetValues (toggle, XmNset, False, 0);
	}
    }

  /* The selected was specified by the value slot */
  if (val->value)
    {
      toggle = XtNameToWidget (widget, val->value);
      if (toggle)
	XtVaSetValues (toggle, XmNset, True, 0);
    }
}

/* update a popup menu, pulldown menu or a menubar */
static Boolean
all_dashes_p (s)
     char* s;
{
  char* t;
  for (t = s; *t; t++)
    if (*t != '-')
      return False;
  return True;
}

static void
make_menu_in_widget (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  Widget* children = 0;
  int num_children;
  int child_index;
  widget_value* cur;
  Widget button = 0;
  Widget menu;
  Arg al [256];
  int ac;
  Boolean menubar_p;

  /* Allocate the children array */
  for (num_children = 0, cur = val; cur; num_children++, cur = cur->next);
  children = (Widget*)XtMalloc (num_children * sizeof (Widget));

  /* tricky way to know if this RowColumn is a menubar or a pulldown... */
  menubar_p = False;
  XtSetArg (al[0], XmNisHomogeneous, &menubar_p);
  XtGetValues (widget, al, 1);

  /* add the unmap callback for popups and pulldowns */
  /*** this sounds bogus ***/
  if (!menubar_p)
    XtAddCallback (XtParent (widget), XmNpopdownCallback,
		   xm_pop_down_callback, (XtPointer)instance);

  for (child_index = 0, cur = val; cur; child_index++, cur = cur->next)
    {    
      ac = 0;
      XtSetArg (al [ac], XmNsensitive, cur->enabled); ac++;
      XtSetArg (al [ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
      XtSetArg (al [ac], XmNuserData, cur->call_data); ac++;
      
      if (instance->pop_up_p && !cur->contents && !cur->call_data
	  && !all_dashes_p (cur->name))
	{
	  ac = 0;
	  XtSetArg (al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
	  button = XmCreateLabel (widget, cur->name, al, ac);
	}
      else if (all_dashes_p (cur->name))
	{
	  button = XmCreateSeparator (widget, cur->name, NULL, 0);
	}
      else if (!cur->contents)
	{
	  if (menubar_p)
	    button = XmCreateCascadeButton (widget, cur->name, al, ac);
	  else if (!cur->call_data)
	    button = XmCreateLabel (widget, cur->name, al, ac);
	  else
	    button = XmCreatePushButtonGadget (widget, cur->name, al, ac);

	  xm_update_label (instance, button, cur);

	  /* don't add a callback to a simple label */
	  if (cur->call_data)
	    XtAddCallback (button, XmNactivateCallback, xm_generic_callback,
			   (XtPointer)instance);
	}
      else
	{
	  menu = XmCreatePulldownMenu (widget, cur->name, NULL, 0);
	  make_menu_in_widget (instance, menu, cur->contents);
	  XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	  button = XmCreateCascadeButton (widget, cur->name, al, ac);

	  xm_update_label (instance, button, cur);

	  XtAddCallback (button, XmNcascadingCallback, xm_pull_down_callback,
			 (XtPointer)instance);
	}

      children [child_index] = button;
    }

  XtManageChildren (children, num_children);

  /* Last entry is the help button.  Has to be done after managing
   * the buttons otherwise the menubar is only 4 pixels high... */
  if (button)
    {
      ac = 0;
      XtSetArg (al [ac], XmNmenuHelpWidget, button); ac++;
      XtSetValues (widget, al, ac);
    }

  XtFree ((char *) children);
}

static void
update_one_menu_entry (instance, widget, val, deep_p)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
     Boolean deep_p;
{
  Arg al [256];
  int ac;
  Widget menu;
  widget_value* contents;

  if (val->change == NO_CHANGE)
    return;

  /* update the sensitivity and userdata */
  /* Common to all widget types */
  XtVaSetValues (widget,
		 XmNsensitive, val->enabled,
		 XmNuserData, val->call_data,
		 0);

  /* update the menu button as a label. */
  if (val->change >= VISIBLE_CHANGE)
    xm_update_label (instance, widget, val);

  /* update the pulldown/pullaside as needed */
  ac = 0;
  menu = NULL;
  XtSetArg (al [ac], XmNsubMenuId, &menu); ac++;
  XtGetValues (widget, al, ac);
  
  contents = val->contents;

  if (!menu)
    {
      if (contents)
	{
	  menu = XmCreatePulldownMenu (XtParent (widget), XtName (widget), NULL, 0);
	  make_menu_in_widget (instance, menu, contents);
	  ac = 0;
	  XtSetArg (al [ac], XmNsubMenuId, menu); ac++;
	  XtSetValues (widget, al, ac);
	}
    }
  else if (!contents)
    {
      ac = 0;
      XtSetArg (al [ac], XmNsubMenuId, NULL); ac++;
      XtSetValues (widget, al, ac);
      XtDestroyWidget (menu);
    }
  else if (deep_p && contents->change != NO_CHANGE)
    xm_update_menu (instance, menu, val, 1);
}

static void
xm_update_menu (instance, widget, val, deep_p)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
     Boolean deep_p;
{
  /* Widget is a RowColumn widget whose contents have to be updated
   * to reflect the list of items in val->contents */
  if (val->contents->change == STRUCTURAL_CHANGE)
    {
      destroy_all_children (widget);
      make_menu_in_widget (instance, widget, val->contents);
    }
  else
    {
      /* Update all the buttons of the RowColumn in order. */
      Widget* children;
      unsigned int num_children;
      int i;
      widget_value* cur;

      children = XtCompositeChildren (widget, &num_children);
      if (children)
	{
	  for (i = 0, cur = val->contents; i < num_children; i++)
	    {
	      if (!cur)
		abort ();
	      if (children [i]->core.being_destroyed
		  || strcmp (XtName (children [i]), cur->name))
		continue;
	      update_one_menu_entry (instance, children [i], cur, deep_p);
	      cur = cur->next;
	    }
	  XtFree ((char *) children);
	}
      if (cur)
	abort ();
    }
}


/* update text widgets */

static void
xm_update_text (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  XmTextSetString (widget, val->value ? val->value : "");
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_internal_update_other_instances, instance);
}

static void
xm_update_text_field (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  XmTextFieldSetString (widget, val->value ? val->value : "");
  XtRemoveAllCallbacks (widget, XmNactivateCallback);
  XtAddCallback (widget, XmNactivateCallback, xm_generic_callback, instance);
  XtRemoveAllCallbacks (widget, XmNvalueChangedCallback);
  XtAddCallback (widget, XmNvalueChangedCallback,
		 xm_internal_update_other_instances, instance);
}


/* update a motif widget */

void
xm_update_one_widget (instance, widget, val, deep_p)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
     Boolean deep_p;
{
  WidgetClass class;
  
  /* Mark as not edited */
  val->edited = False;

  /* Common to all widget types */
  XtVaSetValues (widget,
		 XmNsensitive, val->enabled,
		 XmNuserData, val->call_data,
		 0);
  
  /* Common to all label like widgets */
  if (XtIsSubclass (widget, xmLabelWidgetClass))
    xm_update_label (instance, widget, val);
  
  class = XtClass (widget);
  /* Class specific things */
  if (class == xmPushButtonWidgetClass ||
      class == xmArrowButtonWidgetClass)
    {
      xm_update_pushbutton (instance, widget, val);
    }
  else if (class == xmCascadeButtonWidgetClass)
    {
      xm_update_cascadebutton (instance, widget, val);
    }
  else if (class == xmToggleButtonWidgetClass
	   || class == xmToggleButtonGadgetClass)
    {
      xm_update_toggle (instance, widget, val);
    }
  else if (class == xmRowColumnWidgetClass)
    {
      Boolean radiobox = 0;
      int ac = 0;
      Arg al [1];
      
      XtSetArg (al [ac], XmNradioBehavior, &radiobox); ac++;
      XtGetValues (widget, al, ac);
      
      if (radiobox)
	xm_update_radiobox (instance, widget, val);
      else
	xm_update_menu (instance, widget, val, deep_p);
    }
  else if (class == xmTextWidgetClass)
    {
      xm_update_text (instance, widget, val);
    }
  else if (class == xmTextFieldWidgetClass)
    {
      xm_update_text_field (instance, widget, val);
    }
  else if (class == xmListWidgetClass)
    {
      xm_update_list (instance, widget, val);
    }
}

/* getting the value back */
void
xm_update_one_value (instance, widget, val)
     widget_instance* instance;
     Widget widget;
     widget_value* val;
{
  WidgetClass class = XtClass (widget);
  widget_value *old_wv;

  /* copy the call_data slot into the "return" widget_value */
  for (old_wv = instance->info->val->contents; old_wv; old_wv = old_wv->next)
    if (!strcmp (val->name, old_wv->name))
      {
	val->call_data = old_wv->call_data;
	break;
      }
  
  if (class == xmToggleButtonWidgetClass || class == xmToggleButtonGadgetClass)
    {
      XtVaGetValues (widget, XmNset, &val->selected, 0);
      val->edited = True;
    }
  else if (class == xmTextWidgetClass)
    {
      if (val->value)
	free (val->value);
      val->value = XmTextGetString (widget);
      val->edited = True;
    }
  else if (class == xmTextFieldWidgetClass)
    {
      if (val->value)
	free (val->value);
      val->value = XmTextFieldGetString (widget);
      val->edited = True;
    }
  else if (class == xmRowColumnWidgetClass)
    {
      Boolean radiobox = 0;
      int ac = 0;
      Arg al [1];
      
      XtSetArg (al [ac], XmNradioBehavior, &radiobox); ac++;
      XtGetValues (widget, al, ac);
      
      if (radiobox)
	{
	  CompositeWidget radio = (CompositeWidget)widget;
	  int i;
	  for (i = 0; i < radio->composite.num_children; i++)
	    {
	      int set = False;
	      Widget toggle = radio->composite.children [i];
	      
	      XtVaGetValues (toggle, XmNset, &set, 0);
	      if (set)
		{
		  if (val->value)
		    free (val->value);
		  val->value = safe_strdup (XtName (toggle));
		}
	    }
	  val->edited = True;
	}
    }
  else if (class == xmListWidgetClass)
    {
      int pos_cnt;
      int* pos_list;
      if (XmListGetSelectedPos (widget, &pos_list, &pos_cnt))
	{
	  int i;
	  widget_value* cur;
	  for (cur = val->contents, i = 0; cur; cur = cur->next)
	    if (cur->value)
	      {
		int j;
		cur->selected = False;
		i += 1;
		for (j = 0; j < pos_cnt; j++)
		  if (pos_list [j] == i)
		    {
		      cur->selected = True;
		      val->value = safe_strdup (cur->name);
		    }
	      }
	  val->edited = 1;
	  XtFree ((char *) pos_list);
	}
    }
}


/* This function is for activating a button from a program.  It's wrong because
   we pass a NULL argument in the call_data which is not Motif compatible.
   This is used from the XmNdefaultAction callback of the List widgets to
   have a dble-click put down a dialog box like the button woudl do. 
   I could not find a way to do that with accelerators.
 */
static void
activate_button (Widget widget, XtPointer closure, XtPointer call_data)
{
  Widget button = (Widget)closure;
  XtCallCallbacks (button, XmNactivateCallback, NULL);
}

/* creation functions */

/* dialogs */
static Widget
make_dialog (name, parent, pop_up_p, shell_title, icon_name, text_input_slot,
	     radio_box, list, left_buttons, right_buttons)
     char* name;
     Widget parent;
     Boolean pop_up_p;
     char* shell_title;
     char* icon_name;
     Boolean text_input_slot;
     Boolean radio_box;
     Boolean list;
     int left_buttons;
     int right_buttons;
{
  Widget result;
  Widget form;
  Widget row;
  Widget icon;
  Widget icon_separator;
  Widget message;
  Widget value = 0;
  Widget separator;
  Widget button = 0;
  Widget children [16];		/* for the final XtManageChildren */
  int	 n_children;
  Arg 	al[64];			/* Arg List */
  int 	ac;			/* Arg Count */
  int 	i;
  
  if (pop_up_p)
    {
      ac = 0;
      XtSetArg(al[ac], XmNtitle, shell_title); ac++;
      XtSetArg(al[ac], XtNallowShellResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmUNMAP); ac++;
      result = XmCreateDialogShell (parent, "dialog", al, ac);
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
/*      XtSetArg(al[ac], XmNautoUnmanage, TRUE); ac++; */ /* ####is this ok? */
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      form = XmCreateForm (result, shell_title, al, ac);
    }
  else
    {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      form = XmCreateForm (parent, shell_title, al, ac);
      result = form;
    }

  n_children = left_buttons + right_buttons + 1;
  ac = 0;
  XtSetArg(al[ac], XmNpacking, n_children == 3? 
	   XmPACK_COLUMN: XmPACK_TIGHT); ac++;
  XtSetArg(al[ac], XmNorientation, n_children == 3? 
	   XmVERTICAL: XmHORIZONTAL); ac++;
  XtSetArg(al[ac], XmNnumColumns, left_buttons + right_buttons + 1); ac++;
  XtSetArg(al[ac], XmNmarginWidth, 0); ac++;
  XtSetArg(al[ac], XmNmarginHeight, 0); ac++;
  XtSetArg(al[ac], XmNspacing, 13); ac++;
  XtSetArg(al[ac], XmNadjustLast, False); ac++;
  XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
  XtSetArg(al[ac], XmNisAligned, True); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 13); ac++;
  row = XmCreateRowColumn (form, "row", al, ac);
  
  n_children = 0;
  for (i = 0; i < left_buttons; i++)
    {
      char button_name [16];
      sprintf (button_name, "button%d", i + 1);
      ac = 0;
      if (i == 0)
	{
	  XtSetArg(al[ac], XmNhighlightThickness, 1); ac++;
	  XtSetArg(al[ac], XmNshowAsDefault, TRUE); ac++;
	}
      XtSetArg(al[ac], XmNmarginWidth, 10); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      children [n_children] = XmCreatePushButton (row, button_name, al, ac);

      if (i == 0)
	{
	  button = children [n_children];
	  ac = 0;
	  XtSetArg(al[ac], XmNdefaultButton, button); ac++;
	  XtSetValues (row, al, ac);
	}

      n_children++;
    }

  /* invisible seperator button */
  ac = 0;
  XtSetArg (al[ac], XmNmappedWhenManaged, FALSE); ac++;
  children [n_children] = XmCreateLabel (row, "separator_button", al, ac);
  n_children++;
  
  for (i = 0; i < right_buttons; i++)
    {
      char button_name [16];
      sprintf (button_name, "button%d", left_buttons + i + 1);
      ac = 0;
      XtSetArg(al[ac], XmNmarginWidth, 10); ac++;
      XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP); ac++;
      children [n_children] = XmCreatePushButton (row, button_name, al, ac);
      if (! button) button = children [n_children];
      n_children++;
    }
  
  XtManageChildren (children, n_children);
  
  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomWidget, row); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 0); ac++;
  separator = XmCreateSeparator (form, "", al, ac);

  ac = 0;
  XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNtopOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  icon = XmCreateLabel (form, icon_name, al, ac);

  ac = 0;
  XtSetArg(al[ac], XmNmappedWhenManaged, FALSE); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopOffset, 6); ac++;
  XtSetArg(al[ac], XmNtopWidget, icon); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 6); ac++;
  XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  icon_separator = XmCreateLabel (form, "", al, ac);

  if (text_input_slot)
    {
      ac = 0;
      XtSetArg(al[ac], XmNcolumns, 50); ac++;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateTextField (form, "value", al, ac);
    }
  else if (radio_box)
    {
      Widget radio_butt;
      ac = 0;
      XtSetArg(al[ac], XmNmarginWidth, 0); ac++;
      XtSetArg(al[ac], XmNmarginHeight, 0); ac++;
      XtSetArg(al[ac], XmNspacing, 13); ac++;
      XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER); ac++;
      XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateRadioBox (form, "radiobutton1", al, ac);
      ac = 0;
      i = 0;
      radio_butt = XmCreateToggleButtonGadget (value, "radio1", al, ac);
      children [i++] = radio_butt;
      radio_butt = XmCreateToggleButtonGadget (value, "radio2", al, ac);
      children [i++] = radio_butt;
      radio_butt = XmCreateToggleButtonGadget (value, "radio3", al, ac);
      children [i++] = radio_butt;
      XtManageChildren (children, i);
    }
  else if (list)
    {
      ac = 0;
      XtSetArg(al[ac], XmNvisibleItemCount, 5); ac++;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
      XtSetArg(al[ac], XmNbottomWidget, separator); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNleftOffset, 13); ac++;
      XtSetArg(al[ac], XmNleftWidget, icon); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightOffset, 13); ac++;
      value = XmCreateScrolledList (form, "list", al, ac);

      /* this is the easiest way I found to have the dble click in the
	 list activate the default button */
      XtAddCallback (value, XmNdefaultActionCallback, activate_button, button);
    }
  
  ac = 0;
  XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNtopOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 13); ac++;
  XtSetArg(al[ac], XmNbottomWidget,
	   text_input_slot || radio_box || list ? value : separator); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNleftOffset, 13); ac++;
  XtSetArg(al[ac], XmNleftWidget, icon); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 13); ac++;
  message = XmCreateLabel (form, "message", al, ac);
  
  if (list)
    XtManageChild (value);

  i = 0;
  children [i] = row; i++;
  children [i] = separator; i++;
  if (text_input_slot || radio_box)
    {
      children [i] = value; i++;
    }
  children [i] = message; i++;
  children [i] = icon; i++;
  children [i] = icon_separator; i++;
  XtManageChildren (children, i);
  
  if (text_input_slot || list)
    {
      XtInstallAccelerators (value, button);
      XtSetKeyboardFocus (result, value);
    }
  else
    {
      XtInstallAccelerators (form, button);
      XtSetKeyboardFocus (result, button);
    }
  
  return result;
}

static destroyed_instance*
find_matching_instance (instance)
     widget_instance* instance;
{
  destroyed_instance*	cur;
  destroyed_instance*	prev;
  char*	type = instance->info->type;
  char*	name = instance->info->name;

  for (prev = NULL, cur = all_destroyed_instances;
       cur;
       prev = cur, cur = cur->next)
    {
      if (!strcmp (cur->name, name)
	  && !strcmp (cur->type, type)
	  && cur->parent == instance->parent
	  && cur->pop_up_p == instance->pop_up_p)
	{
	  if (prev)
	    prev->next = cur->next;
	  else
	    all_destroyed_instances = cur->next;
	  return cur;
	}
      /* do some cleanup */
      else if (!cur->widget)
	{
	  if (prev)
	    prev->next = cur->next;
	  else
	    all_destroyed_instances = cur->next;
	  free_destroyed_instance (cur);
	  cur = prev ? prev : all_destroyed_instances;
	}
    }
  return NULL;
}

static void
mark_dead_instance_destroyed (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  destroyed_instance* instance = (destroyed_instance*)closure;
  instance->widget = NULL;
}

static void
recenter_widget (widget)
     Widget widget;
{
  Widget parent = XtParent (widget);
  Screen* screen = XtScreen (widget);
  Dimension screen_width = WidthOfScreen (screen);
  Dimension screen_height = HeightOfScreen (screen);
  Dimension parent_width = 0;
  Dimension parent_height = 0;
  Dimension child_width = 0;
  Dimension child_height = 0;
  Position x;
  Position y;

  XtVaGetValues (widget, XtNwidth, &child_width, XtNheight, &child_height, 0);
  XtVaGetValues (parent, XtNwidth, &parent_width, XtNheight, &parent_height,
		 0);

  x = (((Position)parent_width) - ((Position)child_width)) / 2;
  y = (((Position)parent_height) - ((Position)child_height)) / 2;
  
  XtTranslateCoords (parent, x, y, &x, &y);

  if (x + child_width > screen_width)
    x = screen_width - child_width;
  if (x < 0)
    x = 0;

  if (y + child_height > screen_height)
    y = screen_height - child_height;
  if (y < 0)
    y = 0;

  XtVaSetValues (widget, XtNx, x, XtNy, y, 0);
}

static Widget
recycle_instance (instance)
     destroyed_instance* instance;
{
  Widget widget = instance->widget;

  /* widget is NULL if the parent was destroyed. */
  if (widget)
    {
      Widget focus;
      Widget separator;

      /* Remove the destroy callback as the instance is not in the list
	 anymore */
      XtRemoveCallback (instance->parent, XtNdestroyCallback,
			mark_dead_instance_destroyed,
			(XtPointer)instance);

      /* Give the focus to the initial item */
      focus = XtNameToWidget (widget, "*value");
      if (!focus)
	focus = XtNameToWidget (widget, "*button1");
      if (focus)
	XtSetKeyboardFocus (widget, focus);
      
      /* shrink the separator label back to their original size */
      separator = XtNameToWidget (widget, "*separator_button");
      if (separator)
	XtVaSetValues (separator, XtNwidth, 5, XtNheight, 5, 0);

      /* Center the dialog in its parent */
      recenter_widget (widget);
    }
  free_destroyed_instance (instance);
  return widget;
}

Widget
xm_create_dialog (instance)
     widget_instance* instance;
{
  char* 	name = instance->info->type;
  Widget 	parent = instance->parent;
  Widget	widget;
  Boolean 	pop_up_p = instance->pop_up_p;
  char*		shell_name = 0;
  char* 	icon_name;
  Boolean	text_input_slot = False;
  Boolean	radio_box = False;
  Boolean	list = False;
  int		total_buttons;
  int		left_buttons = 0;
  int		right_buttons = 1;
  destroyed_instance*	dead_one;

  /* try to find a widget to recycle */
  dead_one = find_matching_instance (instance);
  if (dead_one)
    {
      Widget recycled_widget = recycle_instance (dead_one);
      if (recycled_widget)
	return recycled_widget;
    }

  switch (name [0]){
  case 'E': case 'e':
    icon_name = "dbox-error";
    shell_name = "Error";
    break;

  case 'I': case 'i':
    icon_name = "dbox-info";
    shell_name = "Information";
    break;

  case 'L': case 'l':
    list = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'P': case 'p':
    text_input_slot = True;
    icon_name = "dbox-question";
    shell_name = "Prompt";
    break;

  case 'Q': case 'q':
    icon_name = "dbox-question";
    shell_name = "Question";
    break;
  }
  
  total_buttons = name [1] - '0';

  if (name [3] == 'T' || name [3] == 't')
    {
      text_input_slot = False;
      radio_box = True;
    }
  else if (name [3])
    right_buttons = name [4] - '0';
  
  left_buttons = total_buttons - right_buttons;
  
  widget = make_dialog (name, parent, pop_up_p,
			shell_name, icon_name, text_input_slot, radio_box,
			list, left_buttons, right_buttons);

  XtAddCallback (widget, XmNpopdownCallback, xm_nosel_callback,
		 (XtPointer) instance);
  return widget;
}

static Widget
make_menubar (instance)
     widget_instance* instance;
{
  return XmCreateMenuBar (instance->parent, instance->info->name, NULL, 0);
}

static void
remove_grabs (shell, closure, call_data)
     Widget shell;
     XtPointer closure;
     XtPointer call_data;
{
  Widget menu = (Widget) closure;
  XmRemoveFromPostFromList (menu, XtParent (XtParent (menu)));
}

static Widget
make_popup_menu (instance)
     widget_instance* instance;
{
  Widget parent = instance->parent;
  Window parent_window = parent->core.window;
  Widget result;

  /* sets the parent window to 0 to fool Motif into not generating a grab */
  parent->core.window = 0;
  result = XmCreatePopupMenu (parent, instance->info->name, NULL, 0);
  XtAddCallback (XtParent (result), XmNpopdownCallback, remove_grabs,
		 (XtPointer)result);
  parent->core.window = parent_window;
  return result;
}
static Widget
make_main (instance)
     widget_instance* instance;
{
  Widget parent = instance->parent;
  Widget result;
  Arg al[2];
  int ac;

  ac = 0;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  XtSetArg (al[ac], XmNspacing, 0); ac++;
  result = XmCreateMainWindow (parent, instance->info->name, al, ac);
  return result;
}

/* Table of functions to create widgets */

#ifdef ENERGIZE

/* interface with the XDesigner generated functions */
typedef Widget (*widget_maker) (Widget);
extern Widget create_project_p_sheet (Widget parent);
extern Widget create_debugger_p_sheet (Widget parent);
extern Widget create_breaklist_p_sheet (Widget parent);
extern Widget create_le_browser_p_sheet (Widget parent);
extern Widget create_class_browser_p_sheet (Widget parent);
extern Widget create_call_browser_p_sheet (Widget parent);
extern Widget create_build_dialog (Widget parent);
extern Widget create_editmode_dialog (Widget parent);
extern Widget create_search_dialog (Widget parent);
extern Widget create_project_display_dialog (Widget parent);

static Widget
make_one (widget_instance* instance, widget_maker fn)
{
  Widget result;
  Arg 	al [64];
  int 	ac = 0;

  if (instance->pop_up_p)
    {
      XtSetArg (al [ac], XmNallowShellResize, TRUE); ac++;
      result = XmCreateDialogShell (instance->parent, "dialog", NULL, 0);
      XtAddCallback (result, XmNpopdownCallback, &xm_nosel_callback,
		     (XtPointer) instance);
      (*fn) (result);
    }
  else
    {
      result = (*fn) (instance->parent);
      XtRealizeWidget (result);
    }
  return result;
}

static Widget
make_project_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_project_p_sheet);
}

static Widget
make_debugger_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_debugger_p_sheet);
}

static Widget
make_breaklist_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_breaklist_p_sheet);
}

static Widget
make_le_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_le_browser_p_sheet);
}

static Widget
make_class_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_class_browser_p_sheet);
}

static Widget
make_call_browser_p_sheet (widget_instance* instance)
{
  return make_one (instance, create_call_browser_p_sheet);
}

static Widget
make_build_dialog (widget_instance* instance)
{
  return make_one (instance, create_build_dialog);
}

static Widget
make_editmode_dialog (widget_instance* instance)
{
  return make_one (instance, create_editmode_dialog);
}

static Widget
make_search_dialog (widget_instance* instance)
{
  return make_one (instance, create_search_dialog);
}

static Widget
make_project_display_dialog (widget_instance* instance)
{
  return make_one (instance, create_project_display_dialog);
}

#endif /* ENERGIZE */

widget_creation_entry
xm_creation_table [] = 
{
  {"menubar", 			make_menubar},
  {"popup",			make_popup_menu},
  {"main",			make_main},
#ifdef ENERGIZE
  {"project_p_sheet",		make_project_p_sheet},
  {"debugger_p_sheet",		make_debugger_p_sheet},
  {"breaklist_psheet",		make_breaklist_p_sheet},
  {"leb_psheet",       		make_le_browser_p_sheet},
  {"class_browser_psheet",	make_class_browser_p_sheet},
  {"ctree_browser_psheet",	make_call_browser_p_sheet},
  {"build",			make_build_dialog},
  {"editmode",			make_editmode_dialog},
  {"search",			make_search_dialog},
  {"project_display",		make_project_display_dialog},
#endif /* ENERGIZE */
  {NULL, NULL}
};

/* Destruction of instances */
void
xm_destroy_instance (instance)
     widget_instance* instance;
{
  Widget widget = instance->widget;
  /* recycle the dialog boxes */
  /* Disable the recycling until we can find a way to have the dialog box
     get reasonable layout after we modify its contents. */
  if (0
      && XtClass (widget) == xmDialogShellWidgetClass)
    {
      destroyed_instance* dead_instance =
	make_destroyed_instance (instance->info->name,
				 instance->info->type,
				 instance->widget,
				 instance->parent,
				 instance->pop_up_p);
      dead_instance->next = all_destroyed_instances;
      all_destroyed_instances = dead_instance;
      XtUnmanageChild (first_child (instance->widget));
      XFlush (XtDisplay (instance->widget));
      XtAddCallback (instance->parent, XtNdestroyCallback,
		     mark_dead_instance_destroyed, (XtPointer)dead_instance);
    }
  else
    {
      /* This might not be necessary now that the nosel is attached to
	 popdown instead of destroy, but it can't hurt. */
      XtRemoveCallback (instance->widget, XtNdestroyCallback,
			xm_nosel_callback, (XtPointer)instance);
      XtDestroyWidget (instance->widget);
    }
}

/* popup utility */
void
xm_popup_menu (widget)
     Widget widget;
{
  XButtonPressedEvent dummy;
  XEvent* event;

  dummy.type = ButtonPress;
  dummy.serial = 0;
  dummy.send_event = 0;
  dummy.display = XtDisplay (widget);
  dummy.window = XtWindow (XtParent (widget));
  dummy.time = 0;
  dummy.button = 0;
  XQueryPointer (dummy.display, dummy.window, &dummy.root,
		 &dummy.subwindow, &dummy.x_root, &dummy.y_root,
		 &dummy.x, &dummy.y, &dummy.state);
  event = (XEvent *) &dummy;

  if (event->type == ButtonPress || event->type == ButtonRelease)
    {
      /* This is so totally ridiculous: there's NO WAY to tell Motif
	 that *any* button can select a menu item.  Only one button
	 can have that honor.
       */
      char *trans = 0;
      if      (event->xbutton.state & Button5Mask) trans = "<Btn5Down>";
      else if (event->xbutton.state & Button4Mask) trans = "<Btn4Down>";
      else if (event->xbutton.state & Button3Mask) trans = "<Btn3Down>";
      else if (event->xbutton.state & Button2Mask) trans = "<Btn2Down>";
      else if (event->xbutton.state & Button1Mask) trans = "<Btn1Down>";
      if (trans) XtVaSetValues (widget, XmNmenuPost, trans, 0);
      XmMenuPosition (widget, (XButtonPressedEvent *) event);
    }
  XtManageChild (widget);
}

static void
set_min_dialog_size (w)
     Widget w;
{
  short width;
  short height;
  XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, 0);
  XtVaSetValues (w, XmNminWidth, width, XmNminHeight, height, 0);
}

void
xm_pop_instance (instance, up)
     widget_instance* instance;
     Boolean up;
{
  Widget widget = instance->widget;

  if (XtClass (widget) == xmDialogShellWidgetClass)
    {
      Widget widget_to_manage = first_child (widget);
      if (up)
	{
	  XtManageChild (widget_to_manage);
	  set_min_dialog_size (widget);
	  XtSetKeyboardFocus (instance->parent, widget);
	}
      else
	XtUnmanageChild (widget_to_manage);
    }
  else
    {
      if (up)
	XtManageChild (widget);
      else
	XtUnmanageChild (widget);	
    }
}


/* motif callback */ 

enum do_call_type { pre_activate, selection, no_selection, post_activate };

static void
do_call (widget, closure, type)
     Widget widget;
     XtPointer closure;
     enum do_call_type type;
{
  Arg al [256];
  int ac;
  XtPointer user_data;
  widget_instance* instance = (widget_instance*)closure;
  Widget instance_widget;
  LWLIB_ID id;

  if (!instance)
    return;
  if (widget->core.being_destroyed)
    return;

  instance_widget = instance->widget;
  if (!instance_widget)
    return;

  id = instance->info->id;
  ac = 0;
  user_data = NULL;
  XtSetArg (al [ac], XmNuserData, &user_data); ac++;
  XtGetValues (widget, al, ac);
  switch (type)
    {
    case pre_activate:
      if (instance->info->pre_activate_cb)
	instance->info->pre_activate_cb (widget, id, user_data);
      break;
    case selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (widget, id, user_data);
      break;
    case no_selection:
      if (instance->info->selection_cb)
	instance->info->selection_cb (widget, id, (XtPointer) -1);
      break;
    case post_activate:
      if (instance->info->post_activate_cb)
	instance->info->post_activate_cb (widget, id, user_data);
      break;
    default:
      abort ();
    }
}

/* Like lw_internal_update_other_instances except that it does not do
   anything if its shell parent is not managed.  This is to protect 
   lw_internal_update_other_instances to dereference freed memory
   if the widget was ``destroyed'' by caching it in the all_destroyed_instances
   list */
static void
xm_internal_update_other_instances (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  Widget parent;
  for (parent = widget; parent; parent = XtParent (parent))
    if (XtIsShell (parent))
      break;
    else if (!XtIsManaged (parent))
      return;
   lw_internal_update_other_instances (widget, closure, call_data);
}

static void
xm_generic_callback (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  lw_internal_update_other_instances (widget, closure, call_data);
  do_call (widget, closure, selection);
}

static void
xm_nosel_callback (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  /* This callback is only called when a dialog box is dismissed with the wm's
     destroy button (WM_DELETE_WINDOW.)  We want the dialog box to be destroyed
     in that case, not just unmapped, so that it releases its keyboard grabs.
     But there are problems with running our callbacks while the widget is in
     the process of being destroyed, so we set XmNdeleteResponse to XmUNMAP
     instead of XmDESTROY and then destroy it ourself after having run the
     callback.
   */
  do_call (widget, closure, no_selection);
  XtDestroyWidget (widget);
}

static void
xm_pull_down_callback (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  do_call (widget, closure, pre_activate);
}

static void
xm_pop_down_callback (widget, closure, call_data)
     Widget widget;
     XtPointer closure;
     XtPointer call_data;
{
  widget_instance *instance = (widget_instance *) closure;

  if ((!instance->pop_up_p && (XtParent (widget) == instance->widget))
      || (XtParent (widget) == instance->parent))
    do_call (widget, closure, post_activate);
}


/* set the keyboard focus */
void
xm_set_keyboard_focus (parent, w)
     Widget parent;
     Widget w;
{
  XmProcessTraversal (w, 0);
  XtSetKeyboardFocus (parent, w);
}

/* Motif hack to set the main window areas. */
void
xm_set_main_areas (parent, menubar, work_area)
     Widget parent;
     Widget menubar;
     Widget work_area;
{
  XmMainWindowSetAreas (parent,
			menubar,	/* menubar (maybe 0) */
			0,		/* command area (psheets) */
			0,		/* horizontal scroll */
			0,              /* vertical scroll */
			work_area);	/* work area */
}

/* Motif hack to control resizing on the menubar. */
void
xm_manage_resizing (w, flag)
     Widget w;
     Boolean flag;
{
  if (flag)
    {
      /* Enable the edit widget for resizing. */
      Arg al[1];
      
      XtSetArg (al[0], XtNallowShellResize, 0);
      XtSetValues (w, al, 1);
    }
  else
    {
      /* Disable the edit widget from resizing. */
      Arg al[1];
      
      XtSetArg (al[0], XtNallowShellResize, 0);
      XtSetValues (w, al, 1);
    }
}
