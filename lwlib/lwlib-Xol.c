
#include "../src/lisp.h"

#include "lwlib-Xol.h"
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CompositeP.h>
#include <X11/Shell.h>
#include <Xol/Menu.h>
#include <Xol/OpenLook.h>
#include <Xol/MenuButton.h>
#include <Xol/OblongButt.h>
#include <Xol/ControlAre.h>
#include <Xol/Stub.h>
#include <Xol/StaticText.h>

/* forward declarations */
static void
update_menu_widget (widget_instance* instance, Widget widget,
		    widget_value* val);

/* Menu callbacks */
static void
pre_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  OlVirtualEvent ve = (OlVirtualEvent)call_data;
  widget_instance* instance = (widget_instance*)client_data;

  if (w->core.being_destroyed)
    return;

  if (XtParent (w) == instance->widget)
    {
      if (ve->xevent->type == ButtonPress && instance->info->pre_activate_cb)
	instance->info->pre_activate_cb (instance->widget, instance->info->id,
					 NULL);
    }
}

static void
post_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  widget_instance* instance = (widget_instance*)client_data;
  
  if (w->core.being_destroyed)
    return;
  
  if (instance->info->post_activate_cb)
    instance->info->post_activate_cb (w, instance->info->id, NULL);
}

static void
pick_hook (Widget w, caddr_t client_data, caddr_t call_data)
{
  widget_instance* instance = 0;
  widget_value* val = (widget_value*)client_data;

  if (w->core.being_destroyed)
    return;

  XtVaGetValues (w, XtNuserData, &instance, 0);

  if (!instance)
    return;

  if (instance->info->selection_cb && val && val->enabled
      && !val->contents)
    instance->info->selection_cb (w, instance->info->id, val->call_data);
}

/* creation functions */
static Widget
xol_create_menubar (widget_instance* instance)
{
  Widget widget =
    XtVaCreateWidget (instance->info->name, controlAreaWidgetClass,
		      instance->parent, 0);
  return widget;
}

static Widget
xol_create_popup_menu (widget_instance* instance)
{
  Widget popup_shell =
    XtCreatePopupShell (instance->info->name, menuShellWidgetClass,
			instance->parent, NULL, 0);
  return popup_shell;
}

widget_creation_entry 
xol_creation_table [] =
{
  {"menubar", xol_create_menubar},
  {"popup", xol_create_popup_menu},
  {NULL, NULL}
};

Widget 
xol_create_dialog (widget_instance* instance)
{
  return NULL;
}

Boolean
lw_olit_widget_p (Widget widget)
{
  return True;
}

/* update functions */
static void
destroy_all_children (Widget widget)
{
  Widget* children;
  unsigned int number;
  int i;

  children = (Widget *) XtCompositeChildren (widget, &number);
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
      XtFree (children);
    }
}

static Boolean
all_dashes_p (char* s)
{
  char* t;
  for (t = s; *t; t++)
    if (*t != '-')
      return False;
  return True;
}

static void
make_menu_in_widget (widget_instance* instance, Widget widget,
		     widget_value* val)
{
  widget_value* cur;
  Widget button;
  Arg al [256];
  int ac;
  String label;

  for (cur = val; cur; cur = cur->next)
    {    
      ac = 0;
      XtSetArg (al [ac], XtNsensitive, cur->enabled); ac++;
      XtSetArg (al [ac], XtNuserData, instance); ac++;
      XtSetArg (al [ac], XtNacceleratorText, cur->key); ac++;
      
/*      label = (char *) resource_string (widget, cur->name);*/
      label = cur->name;
      if (label)
	{
	  XtSetArg (al [ac], XtNlabel, label); ac++;
	}

      if (all_dashes_p (cur->name))
	{
	  /* no separator in OpenLook just make some space. */
	  XtSetArg (al [ac], XtNheight, 5); ac++;
	  XtSetArg (al [ac], XtNwidth, 5); ac++;
	  button = XtCreateWidget (cur->name, stubWidgetClass, widget, al, ac);
	}
      else if (!cur->contents)
	{
	  if (!cur->call_data)
	    button =
	      XtCreateManagedWidget (cur->name, staticTextWidgetClass, widget,
				     al, ac);
	  else
	    {
	      button =
		XtCreateManagedWidget (cur->name, oblongButtonWidgetClass,
				       widget, al, ac);
	      XtAddCallback (button, XtNselect, pick_hook, cur);
	    }
	}
      else
	{
	  Widget menu = NULL;
	  button =
	    XtCreateManagedWidget (cur->name, menuButtonWidgetClass, widget,
				   al, ac);
	  XtVaGetValues (button, XtNmenuPane, &menu, 0);
	  if (!menu)
	    abort ();
	  make_menu_in_widget (instance, menu, cur->contents);
	  OlAddCallback (button, XtNconsumeEvent, pre_hook, instance);
	}
    }
}

static void
update_one_menu_entry (widget_instance* instance, Widget widget,
		       widget_value* val)
{
  Arg al [256];
  int ac;
  Widget menu;
  widget_value* contents;

  if (val->change == NO_CHANGE)
    return;

  /* update the sensitivity */
  XtVaSetValues (widget, XtNsensitive, val->enabled, 0);

  /* update the pulldown/pullaside as needed */
  ac = 0;
  menu = NULL;
  XtVaGetValues (widget, XtNmenuPane, &menu, 0);
  contents = val->contents;

  if (!menu)
    {
      if (contents)
	{
	  /* in OLIT this would have to be a structural change on the
	     button. */
	  abort ();
	}
    }
  else if (!contents)
    {
      /* in OLIT this would have to be a structural change on the button. */
      abort ();
    }
  else if (contents->change != NO_CHANGE)
    update_menu_widget (instance, menu, val);
}

static void
update_menu_widget (widget_instance* instance, Widget widget,
		    widget_value* val)

{
  if (val->change == STRUCTURAL_CHANGE
      || val->contents->change == STRUCTURAL_CHANGE)
    {
      destroy_all_children (widget);
      make_menu_in_widget (instance, widget, val->contents);
    }
  else
    {
      /* Update all the buttons of the composite widget in order. */
      Widget* children;
      unsigned int num_children;
      int i;
      widget_value* cur;
      
      children = (Widget *) XtCompositeChildren (widget, &num_children);
      if (children)
	{
	  for (i = 0, cur = val->contents; i < num_children; i++)
	    {
	      if (!cur)
		abort ();
	      if (children [i]->core.being_destroyed
		  || strcmp (XtName (children [i]), cur->name))
		continue;
	      update_one_menu_entry (instance, children [i], cur);
	      cur = cur->next;
	    }
	  XtFree (children);
	}
      if (cur)
	abort ();
    }
}

void
xol_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p)
{
  Widget menu = widget;

  if (XtIsShell (widget))
    XtVaGetValues (widget, XtNmenuPane, &menu, 0);

  update_menu_widget (instance, menu, val);
}

void
xol_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val)
{
  return;
}

void
xol_pop_instance (widget_instance* instance, Boolean up)
{
}

void
xol_popup_menu (Widget widget)
{
  OlMenuPost (widget);
}

/* Destruction of instances */
void
xol_destroy_instance (widget_instance* instance)
{
  XtDestroyWidget (instance->widget);
}

