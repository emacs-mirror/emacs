#ifndef LWLIB_XM_H
#define LWLIB_XM_H

#include "lwlib-internal.h"

extern widget_creation_entry xm_creation_table [];

Widget 
xm_create_dialog (widget_instance* instance);

Boolean
lw_motif_widget_p (Widget widget);

void
xm_update_one_widget (widget_instance* instance, Widget widget,
		      widget_value* val, Boolean deep_p);

void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val);

void
xm_destroy_instance (widget_instance* instance);

void
xm_set_keyboard_focus (Widget parent, Widget w);

void
xm_popup_menu (Widget widget);

void
xm_pop_instance (widget_instance* instance, Boolean up);

#endif /* LWLIB_XM_H */
