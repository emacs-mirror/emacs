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
 * 	XMenuRecompute - Recompute XMenu object dependencies.
 *
 *	Author:		Tony Della Fera, DEC
 *			September, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuRecompute(Display *display, register XMenu *menu)
                     
                         	/* Menu object to be recomputed. */
{
    register XMPane *p_ptr;	/* Pane pointer. */
    register XMSelect *s_ptr;	/* Selection pointer. */

    register int p_num;		/* Pane serial number. */
    register int s_num;		/* Selection serial number. */

    /*
     * If there are no panes in the menu then return failure
     * because the menu is not initialized.
     */
    if (menu->p_count == 0) {
	_XMErrorCode = XME_NOT_INIT;
	return(XM_FAILURE);
    }

    /*
     * Recompute menu wide global values: pane window size,
     * selection size and maximum selection count.
     */
    _XMRecomputeGlobals(display, menu);

    /*
     * For each pane in the menu...
     */

    p_num = 0;
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	/*
	 * Recompute pane dependencies.
	 */
	if (_XMRecomputePane(display, menu, p_ptr, p_num) == _FAILURE) {
	    return(XM_FAILURE);
	}
        p_num++;

	/*
	 * For each selection in the pane...
	 */
	s_num = 0;
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_ptr->next
	) {
	    /*
	     * Recompute selection dependencies.
	     */
	    if (_XMRecomputeSelection(display, menu, s_ptr, s_num) == _FAILURE) {
		return(XM_FAILURE);
	    }
	    s_num++;
	}
    }

    /*
     * Recompute menu size.
     */
    if (menu->menu_style == CENTER) {
	menu->width = menu->p_width + (menu->p_bdr_width << 1);
    }
    else {
	menu->width = menu->p_width + (menu->p_bdr_width << 1) +
	    ((menu->p_count - 1) * menu->p_x_off);
    }
    menu->height = menu->p_height + (menu->p_bdr_width << 1) +
	((menu->p_count - 1) * menu->p_y_off);

    /*
     * Reset the recompute flag.
     */
    menu->recompute = 0;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(XM_SUCCESS);
}

