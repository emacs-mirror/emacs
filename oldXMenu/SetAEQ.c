#include "copyright.h"

/* $Header: /u/src/emacs/19.0/oldXMenu/RCS/SetAEQ.c,v 1.1 1992/04/11 22:10:20 jimb Exp $ */
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
