#ifndef LWLIB_XAW_H
#define LWLIB_XAW_H

#include "lwlib-int.h"

extern widget_creation_entry xaw_creation_table [];

Widget 
xaw_create_dialog (widget_instance* instance);

Boolean
lw_xaw_widget_p (Widget widget);

void
xaw_update_one_widget (widget_instance *instance, Widget widget,
		       widget_value *val, Boolean deep_p);

void
xaw_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val);

void
xaw_destroy_instance (widget_instance* instance);

void
xaw_popup_menu (Widget widget);

void
xaw_pop_instance (widget_instance* instance, Boolean up);

#endif /* LWLIB_XAW_H */
