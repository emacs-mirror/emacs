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
 *	XMenuSetSelection - Set a menu selection to be active or inactive.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuSetSelection(register XMenu *menu, register int p_num, register int s_num, int active)
                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
                       		/* Selection number to modified. */
               			/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

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
     * Set its active switch.
     */
    s_ptr->active = active;

    /*
     * Return the selection number just set.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}

