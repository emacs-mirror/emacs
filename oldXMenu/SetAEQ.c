#include "copyright.h"

/* $Header: /cvsroot/emacs/emacs/oldXMenu/SetAEQ.c,v 1.1 1999/10/03 19:35:12 fx Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetAEQ - Set Asynchronous event queuing mode.
 *		      When enabled asynchronous events will be queue while
 *		      a menu is being displayed and restored to the X
 *		      event queue when the menu is taken down.
 *
 *	Author:		Tony Della Fera, DEC
 *			March 12, 1986
 *
 */

#include "XMenuInt.h"

XMenuSetAEQ(menu, aeq)
    register XMenu *menu;	/* Menu object to be modified. */
    register int aeq;		/* AEQ mode? */
{
    /*
     * Set the AEQ mode switch.
     */
    menu->aeq = aeq;
}

/* arch-tag: 48fc22b4-0722-4852-a044-788444e4a9dc
   (do not change this comment) */
