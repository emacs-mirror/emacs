#include "copyright.h"

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.  */

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetFreeze - Forcibly set the menu freeze mode switch
 *			 overriding the Xdefaults setting.
 *			 This is necessary in some situations.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 29, 1986
 *
 */

#include "XMenuInt.h"

XMenuSetFreeze(menu, freeze)
    register XMenu *menu;	/* Menu object to be modified. */
    register int freeze;	/* Freeze mode? */
{
    /*
     * Set the freeze mode switch.
     */
    menu->freeze = freeze;
}

/* arch-tag: 69c5670b-3a46-4c78-8fdb-305936d79772
   (do not change this comment) */
