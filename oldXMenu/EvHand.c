#include "copyright.h"

/* $Header: /cvsroot/emacs/emacs/oldXMenu/EvHand.c,v 1.1 1999/10/03 19:35:01 fx Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuEventHandler - Set the XMenu asynchronous event handler.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include "XMenuInt.h"

XMenuEventHandler(handler)
    int (*handler)();
{
    /*
     * Set the global event handler variable.
     */
    _XMEventHandler = handler;
}

/* arch-tag: 8d614c8c-94d9-43c8-8e32-c438a3c8a8a3
   (do not change this comment) */
