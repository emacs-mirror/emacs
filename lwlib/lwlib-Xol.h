#ifndef LWLIB_XOL_H
#define LWLIB_XOL_H

#include "lwlib-int.h"

extern widget_creation_entry xol_creation_table [];
extern Widget xol_create_dialog (widget_instance *);

Boolean
lw_olit_widget_p (Widget widget);

void
xol_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p);

void
xol_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val);

void
xol_destroy_instance (widget_instance* instance);

void
xol_pop_instance (widget_instance* instance, Boolean up);

void
xol_popup_menu (Widget widget);

#endif /* LWLIB_XOL_H */
