#ifndef LWLIB_XLW_H
#define LWLIB_XLW_H

#include "lwlib-int.h"

extern widget_creation_entry xlw_creation_table [];
extern widget_creation_function xlw_create_dialog;

Boolean
lw_lucid_widget_p __P ((Widget widget));

void
xlw_update_one_widget __P ((widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p));

void
xlw_update_one_value __P ((widget_instance* instance, Widget widget,
		      widget_value* val));

void
xlw_destroy_instance __P ((widget_instance* instance));

void
xlw_pop_instance __P ((widget_instance* instance, Boolean up));

void
xlw_popup_menu __P ((Widget widget, XEvent * event));

#endif /* LWLIB_XLW_H */

/* arch-tag: e5b1511d-8992-4dad-b947-a2440d8f10a2
   (do not change this comment) */
