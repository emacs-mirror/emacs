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
extern int (*_XMEventHandler)();

#ifndef Pixel
#define Pixel unsigned long
#endif

/*
 * Internal routine declarations.
 */
int _XMWinQueInit();		/* No value actually returned. */
int _XMWinQueAddPane();
int _XMWinQueAddSelection();
int _XMWinQueFlush();
XMPane *_XMGetPanePtr();
XMSelect *_XMGetSelectionPtr();
int _XMRecomputeGlobals();	/* No value actually returned. */
int _XMRecomputePane();
int _XMRecomputeSelection();
int _XMTransToOrigin();		/* No value actually returned. */
int _XMRefreshPane();		/* No value actually returned. */
int _XMRefreshSelections();	/* No value actually returned. */
int _XMHighlightSelection();	/* No value actually returned. */

#endif
/* Don't add stuff after this #endif */

/* arch-tag: 00640af1-9386-48b5-a4be-35620b8cd3aa
   (do not change this comment) */
