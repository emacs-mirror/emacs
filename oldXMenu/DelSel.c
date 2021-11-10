/* Copyright    Massachusetts Institute of Technology    1985	*/

/*

Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/




/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuDeleteSelection - Deletes a selection from an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include "XMenuInt.h"

int
XMenuDeleteSelection(register Display *display, register XMenu *menu, register int p_num, register int s_num)
                              	/* Previously opened display. */
                         	/* Menu object to be modified. */
                       		/* Pane number to be deleted. */
                       		/* Selection number to be deleted. */
{
    register XMPane *p_ptr;	/* Pointer to pane being deleted. */
    register XMSelect *s_ptr;	/* Pointer to selections being deleted. */

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the right selection.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, s_num);
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Remove the selection from the association table.
     */
    XDeleteAssoc(display, menu->assoc_tab, s_ptr->window);

    /*
     * Remove the selection from the parent pane's selection
     * list and update the selection count.
     */
    emacs_remque(s_ptr);
    p_ptr->s_count--;

    /*
     * Destroy the selection transparency.
     */
    if (s_ptr->window) XDestroyWindow(display, s_ptr->window);

    /*
     * Free the selection's XMSelect structure.
     */
    free(s_ptr);

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just deleted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}

