#ifndef LWLIB_XAW_H
#define LWLIB_XAW_H

#include "lwlib-int.h"

extern widget_creation_entry xaw_creation_table [];

Widget 
xaw_create_dialog ();

Boolean
lw_xaw_widget_p ();

void
xaw_update_one_widget ();

void
xaw_update_one_value ();

void
xaw_destroy_instance ();

void
xaw_popup_menu ();

void
xaw_pop_instance ();

#endif /* LWLIB_XAW_H */
