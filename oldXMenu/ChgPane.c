#include "copyright.h"

/* $Header: /u/src/emacs/19.0/oldXMenu/RCS/ChgPane.c,v 1.1 1992/04/11 22:10:17 jimb Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuChangePane - Change the label of a  menu pane.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuChangePane(menu, p_num, label)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    char *label;		/* Selection label. */
{
    register XMPane *p_ptr;	/* XMPane pointer. */

    int label_length;		/* Label length in characters. */
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
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->p_fnt_info, label, label_length);

    /*
     * Change the pane data.
     */
    p_ptr->label = label;
    p_ptr->label_width = label_width;
    p_ptr->label_length = label_length;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just changed.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}
