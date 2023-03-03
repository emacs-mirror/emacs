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
 *	XMenuInternal.h - Internal menu system include file for the
 *			MIT Project Athena XMenu X window system
 *			menu package.
 *
 *	Author:		Tony Della Fera, DEC
 *			October, 1985
 */

#ifndef _XMenuInternal_h_
#define _XMenuInternal_h_

#include <config.h>

#include <attribute.h>

/* Avoid warnings about redefining NULL by including <stdio.h> first;
   the other file which wants to define it (<stddef.h> on Ultrix
   systems) can deal if NULL is already defined, but <stdio.h> can't.  */
#include <stdio.h>
#include <X11/Xlib.h>
#include "X10.h"
#include "XMenu.h"

#define min(x, y)	((x) <= (y) ? (x) : (y))
#define max(x, y)	((x) >= (y) ? (x) : (y))
#define abs(a)		((a) < 0 ? -(a) : (a))

#define _X_FAILURE	-1

#define _SUCCESS	1
#define _FAILURE	-1

/*
 * XMenu internal event handler variable.
 */
extern int (*_XMEventHandler)(XEvent*);

#ifndef Pixel
#define Pixel unsigned long
#endif

/*
 * Internal routine declarations.
 */
void _XMWinQueInit(void);
int _XMWinQueAddPane(Display *display, XMenu *menu, XMPane *p_ptr);
int _XMWinQueAddSelection(Display *display, XMenu *menu, XMSelect *s_ptr);
int _XMWinQueFlush(Display *display, XMenu *menu, XMPane *pane, XMSelect *select);
XMPane *_XMGetPanePtr(XMenu *menu, int p_num);
XMSelect *_XMGetSelectionPtr(XMPane *p_ptr, int s_num);
void _XMRecomputeGlobals(Display *display, XMenu *menu);
int _XMRecomputePane(Display *display, XMenu *menu, XMPane *p_ptr, int p_num);
int _XMRecomputeSelection(Display *display, XMenu *menu, XMSelect *s_ptr, int s_num);
void _XMTransToOrigin(Display *display, XMenu *menu, XMPane *p_ptr, XMSelect *s_ptr, int x_pos, int y_pos, int *orig_x, int *orig_y);
void _XMRefreshPane(Display *display, XMenu *menu, XMPane *pane);
void _XMRefreshSelection(Display *display, XMenu *menu, XMSelect *select);
void emacs_insque (void *elem, void *prev);
void emacs_remque (void *elem);
void XDestroyAssocTable(XAssocTable *table);
void XMakeAssoc(Display *dpy, XAssocTable *table, XID x_id, void *data);
void XDeleteAssoc(Display *dpy, XAssocTable *table, XID x_id);

#endif
/* Don't add stuff after this #endif */
