#ifndef LWLIB_XM_H
#define LWLIB_XM_H

#include "lwlib-int.h"

extern widget_creation_entry xm_creation_table [];

Widget 
xm_create_dialog __P ((widget_instance* instance));

Boolean
lw_motif_widget_p __P ((Widget widget));

void
xm_update_one_widget __P ((widget_instance* instance, Widget widget,
		      widget_value* val, Boolean deep_p));

void
xm_update_one_value __P ((widget_instance* instance, Widget widget,
		     widget_value* val));

void
xm_destroy_instance __P ((widget_instance* instance));

void
xm_set_keyboard_focus __P ((Widget parent, Widget w));

void
xm_popup_menu __P ((Widget widget, XEvent *event));

void
xm_pop_instance __P ((widget_instance* instance, Boolean up));

void
xm_set_main_areas __P ((Widget parent, Widget menubar, Widget work_area));

void
xm_manage_resizing __P ((Widget w, Boolean flag));

#endif /* LWLIB_XM_H */
