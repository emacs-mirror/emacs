/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetPane - Set a menu pane to be active or inactive.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuSetPane(menu, p_num, active)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    register int active;	/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Set its active switch.
     */
    p_ptr->active = active;
    if (p_ptr->active == False) p_ptr->activated = False;

    /*
     * Return the pane number just set.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}

/* arch-tag: 90bc8d90-031d-41a2-a2c6-04bf94efbc90
   (do not change this comment) */
