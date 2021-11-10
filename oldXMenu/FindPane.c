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
 *	XMenuFindPane - Find the first menu pane who's label matches a
 *			particular string.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 22, 1986
 *
 */

#include "XMenuInt.h"
#include <string.h>

int
XMenuFindPane(register XMenu *menu, register char *label)
{
    register XMPane *p_ptr;
    register int i = 0;

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the pane who's label matches the given label.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	if (p_ptr->label_length == 0) {
	    if (*label == '\0') {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	else {
	    if (strncmp (label, p_ptr->label, p_ptr->label_length) == 0) {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	i++;
    }

    /*
     * If we get here then we have not found
     * a match.
     */
    _XMErrorCode = XME_P_NOT_FOUND;
    return (XM_FAILURE);
}
