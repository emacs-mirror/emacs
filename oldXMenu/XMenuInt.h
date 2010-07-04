/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


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
int _XMWinQueInit(void);		/* No value actually returned. */
int _XMWinQueAddPane(register Display *display, register XMenu *menu, register XMPane *p_ptr);
int _XMWinQueAddSelection(register Display *display, register XMenu *menu, register XMSelect *s_ptr);
int _XMWinQueFlush(register Display *display, register XMenu *menu, register XMPane *pane, XMSelect *select);
XMPane *_XMGetPanePtr(register XMenu *menu, register int p_num);
XMSelect *_XMGetSelectionPtr(register XMPane *p_ptr, register int s_num);
int _XMRecomputeGlobals(register Display *display, register XMenu *menu);	/* No value actually returned. */
int _XMRecomputePane(register Display *display, register XMenu *menu, register XMPane *p_ptr, register int p_num);
int _XMRecomputeSelection(register Display *display, register XMenu *menu, register XMSelect *s_ptr, register int s_num);
int _XMTransToOrigin(Display *display, register XMenu *menu, register XMPane *p_ptr, register XMSelect *s_ptr, int x_pos, int y_pos, int *orig_x, int *orig_y);		/* No value actually returned. */
int _XMRefreshPane(register Display *display, register XMenu *menu, register XMPane *pane);		/* No value actually returned. */

#endif
/* Don't add stuff after this #endif */

/* arch-tag: 00640af1-9386-48b5-a4be-35620b8cd3aa
   (do not change this comment) */
