#ifndef LWLIB_XAW_H
#define LWLIB_XAW_H

#include "lwlib-int.h"

extern widget_creation_entry xaw_creation_table [];

Widget
xaw_create_dialog __P ((widget_instance*));

Boolean
lw_xaw_widget_p __P ((Widget));

void
xaw_update_one_widget __P ((widget_instance *, Widget, widget_value *, Boolean));

void
xaw_update_one_value __P ((widget_instance *, Widget, widget_value *));

void
xaw_destroy_instance __P ((widget_instance *));

void
xaw_popup_menu __P ((Widget, XEvent *));

void
xaw_pop_instance __P ((widget_instance *, Boolean));

#endif /* LWLIB_XAW_H */

/* arch-tag: 7c0fb4de-afd9-4112-9214-24b663cc1870
   (do not change this comment) */
