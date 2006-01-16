#include "copyright.h"

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.  */

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuAddSelection - Adds a selection to an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuAddSelection(display, menu, p_num, data, label, active, help)
    Display *display;
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    char *data;			/* Data value. */
    char *label;		/* Selection label. */
    int active;			/* Make selection active? */
    char *help;			/* Help string */
{
    register XMPane *pane;	/* Pane containing the new selection. */
    register XMSelect *select;	/* Newly created selection. */


    int label_length;		/* Label lenght in characters. */
    int label_width;		/* Label width in pixels. */

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }
    /*
     * Find the right pane.
     */
    pane = _XMGetPanePtr(menu, p_num);
    if (pane == NULL) return(XM_FAILURE);

    /*
     * Calloc the XMSelect structure.
     */
    select = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (select == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }
    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->s_fnt_info, label, label_length);

    /*
     * Fill the XMSelect structure.
     */
    if (!strcmp (label, "--") || !strcmp (label, "---"))
      {
	select->type = SEPARATOR;
	select->active = 0;
      }
    else
      {
	select->type = SELECTION;
	select->active = active;
      }

    select->serial = -1;
    select->label = label;
    select->label_width = label_width;
    select->label_length = label_length;
    select->data = data;
    select->parent_p = pane;
    select->help_string = help;

    /*
     * Insert the selection at the end of the selection list.
     */
    emacs_insque(select, pane->s_list->prev);

    /*
     * Update the selection count.
     */
    pane->s_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return((pane->s_count - 1));
}

/* arch-tag: 0161f024-c739-440d-9498-050280c6c355
   (do not change this comment) */
