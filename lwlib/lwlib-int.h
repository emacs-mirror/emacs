#ifndef LWLIB_INTERNAL_H
#define LWLIB_INTERNAL_H

#include "lwlib.h"

/*
extern char *strdup (const char *);
extern int strcasecmp (const char *, const char *);
*/

typedef struct _widget_instance
{
  Widget		widget;
  Widget		parent;
  Boolean		pop_up_p;
  struct _widget_info*		info;
  struct _widget_instance*	next;
} widget_instance;

typedef struct _widget_info
{
  char*			type;
  char*			name;
  LWLIB_ID		id;
  widget_value*		val;
  Boolean		busy;
  lw_callback		pre_activate_cb;
  lw_callback		selection_cb;
  lw_callback		post_activate_cb;
  struct _widget_instance*	instances;
  struct _widget_info*		next;
} widget_info;

typedef Widget
(*widget_creation_function) (widget_instance* instance);

typedef struct _widget_creation_entry
{
  char*				type;
  widget_creation_function	function;
} widget_creation_entry;

/* update all other instances of a widget.  Can be used in a callback when
   a wiget has been used by the user */
void
lw_internal_update_other_instances (Widget widget, XtPointer closure,
				    XtPointer call_data);

/* get the widget_value for a widget in a given instance */
widget_value*
lw_get_widget_value_for_widget (widget_instance* instance, Widget w);

#endif /* LWLIB_INTERNAL_H */

