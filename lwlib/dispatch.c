/* Defines a function to find the Widget that XtDispatchEvent() would use.
   Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

/* 
 *   The function XtWidgetToDispatchTo(), given an XEvent, returns the 
 *   widget that XtDispatchEvent() would send that event to if called now.
 *   This file copies much code from the X11r4 Xt source, and is thus a
 *   portability problem.  It also requires data structures defined in
 *   IntrinsicI.h, which is a non-exported Xt header file, so you can't
 *   compile this file unless you have the Xt sources online.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <IntrinsicI.h>   /* Don't change this: see comments in Imakefile. */
#include <X11/Xatom.h>
#include "dispatch.h"

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#ifdef XlibSpecificationRelease
#if XlibSpecificationRelease >= 5
#define HAVE_X11R5
#endif
#endif

/* ##  All of the code on this page was copied from the X11R5 lib/Xt/Event.c,
   ##  but is compatible with X11R4; the code in Event.c is different, but
   ##  functionally equivalent for our purposes.
 */

#if __STDC__
#define Const const
#else
#define Const /**/
#endif

#define NonMaskableMask ((EventMask)0x80000000L)

#define COMP_EXPOSE   (widget->core.widget_class->core_class.compress_exposure)
#define COMP_EXPOSE_TYPE (COMP_EXPOSE & 0x0f)
#define GRAPHICS_EXPOSE  ((XtExposeGraphicsExpose & COMP_EXPOSE) || \
			  (XtExposeGraphicsExposeMerged & COMP_EXPOSE))
#define NO_EXPOSE        (XtExposeNoExpose & COMP_EXPOSE)


/* -- lots of stuff we don't need to copy, omitted -- */


static EventMask Const masks[] = {
	0,			    /* Error, should never see  */
	0,			    /* Reply, should never see  */
	KeyPressMask,		    /* KeyPress			*/
	KeyReleaseMask,		    /* KeyRelease		*/
	ButtonPressMask,	    /* ButtonPress		*/
	ButtonReleaseMask,	    /* ButtonRelease		*/
	PointerMotionMask	    /* MotionNotify		*/
		| ButtonMotionMask,
	EnterWindowMask,	    /* EnterNotify		*/
	LeaveWindowMask,	    /* LeaveNotify		*/
	FocusChangeMask,	    /* FocusIn			*/
	FocusChangeMask,	    /* FocusOut			*/
	KeymapStateMask,	    /* KeymapNotify		*/
	ExposureMask,		    /* Expose			*/
	NonMaskableMask,	    /* GraphicsExpose, in GC    */
	NonMaskableMask,	    /* NoExpose, in GC		*/
	VisibilityChangeMask,       /* VisibilityNotify		*/
	SubstructureNotifyMask,     /* CreateNotify		*/
	StructureNotifyMask	    /* DestroyNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* UnmapNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* MapNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* MapRequest		*/
	StructureNotifyMask	    /* ReparentNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* ConfigureNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* ConfigureRequest		*/
	StructureNotifyMask	    /* GravityNotify		*/
		| SubstructureNotifyMask,
	ResizeRedirectMask,	    /* ResizeRequest		*/
	StructureNotifyMask	    /* CirculateNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* CirculateRequest		*/
	PropertyChangeMask,	    /* PropertyNotify		*/
	NonMaskableMask,	    /* SelectionClear		*/
	NonMaskableMask,	    /* SelectionRequest		*/
	NonMaskableMask,	    /* SelectionNotify		*/
	ColormapChangeMask,	    /* ColormapNotify		*/
	NonMaskableMask,	    /* ClientMessage		*/
	NonMaskableMask		    /* MappingNotify		*/
};

#ifndef HAVE_X11R5

static /* in R5, this is not static, so we don't need to define it at all */
EventMask _XtConvertTypeToMask (eventType)
    int		eventType;
{
    eventType &= 0x7f;	/* Events sent with XSendEvent have high bit set. */
    if (eventType < XtNumber(masks))
	return masks[eventType];
    else
	return 0;
}

#endif /* not HAVE_X11R5 */

/* -- _XtOnGrabList() omitted -- */


static Widget LookupSpringLoaded(grabList)
    XtGrabList	grabList;
{
    XtGrabList	gl;

    for (gl = grabList; gl != NULL; gl = gl->next) {
	if (gl->spring_loaded)
	  if (XtIsSensitive(gl->widget))
	    return gl->widget;
	  else
	    return NULL;
	if (gl->exclusive) break;
    }
    return NULL;
}



/* This function is new. */

static Boolean WouldDispatchEvent(event, widget, mask, pd)
     register XEvent    *event;
     Widget    widget;
     EventMask mask;
     XtPerDisplay pd;
{
  XtEventRec *p;   
  Boolean would_dispatched = False;
  
  if ((mask == ExposureMask) ||
      ((event->type == NoExpose) && NO_EXPOSE) ||
      ((event->type == GraphicsExpose) && GRAPHICS_EXPOSE) )
    if (widget->core.widget_class->core_class.expose != NULL )
      return True;
  
  
  if ((mask == VisibilityChangeMask) &&
      XtClass(widget)->core_class.visible_interest) 
    return True;
  
  for (p=widget->core.event_table; p != NULL; p = p->next) 
    if ((mask & p->mask) != 0
#ifndef HAVE_X11R5
	|| (mask == 0 && p->non_filter)
#endif
	)
      return True;

  return False;
}


/* ####  This function is mostly copied from DecideToDispatch().
 */

typedef enum _GrabType {pass, ignore, remap} GrabType;

Widget
XtWidgetToDispatchTo (XEvent* event)
{
  register    Widget widget;
  EventMask   mask;
  GrabType    grabType;
  Widget	dspWidget;
  Time	time = 0;
  XtPerDisplay pd;
  XtPerDisplayInput pdi;
  XtGrabList  grabList;
  
  widget = XtWindowToWidget (event->xany.display, event->xany.window);
  pd = _XtGetPerDisplay(event->xany.display);
  pdi = _XtGetPerDisplayInput(event->xany.display);
  grabList = *_XtGetGrabList(pdi);
  
  mask = _XtConvertTypeToMask(event->xany.type);
  grabType = pass;
  switch (event->xany.type & 0x7f) {
  case KeyPress:
  case KeyRelease:	grabType = remap; break;
  case ButtonPress:
  case ButtonRelease:	grabType = remap; break;
  case MotionNotify:	grabType = ignore;
#define XKnownButtons (Button1MotionMask|Button2MotionMask|Button3MotionMask|\
                       Button4MotionMask|Button5MotionMask)
			mask |= (event->xmotion.state & XKnownButtons);
#undef XKnownButtons
				break;
  case EnterNotify:	grabType = ignore; break;
  }
  
  if (widget == NULL) {
    if (grabType != remap) return False;
    /* event occurred in a non-widget window, but we've promised also
       to dispatch it to the nearest accessible spring_loaded widget */
    else if ((widget = LookupSpringLoaded(grabList)) != NULL)
      return widget;
    return False;
  }

  switch(grabType) {
  case pass:
    return widget;
    
  case ignore:
    if ((grabList == NULL || _XtOnGrabList(widget,grabList))
	&& XtIsSensitive(widget)) {
      return widget;
    }
    return NULL;
    
  case remap:
    
    {
      Widget was_dispatched_to= NULL;
      extern Widget _XtFindRemapWidget();
      extern void _XtUngrabBadGrabs();
      
      dspWidget = _XtFindRemapWidget(event, widget, mask, pdi);
      
      if ((grabList == NULL || 
	   _XtOnGrabList(dspWidget, grabList)) &&
	  XtIsSensitive(dspWidget)) {
	if (WouldDispatchEvent (event, dspWidget, mask, pd))
	  was_dispatched_to = dspWidget;
      }
      
      /* Also dispatch to nearest accessible spring_loaded. */
      /* Fetch this afterward to reflect modal list changes */
      grabList = *_XtGetGrabList(pdi);
      widget = LookupSpringLoaded(grabList);
      if (widget != NULL && widget != dspWidget) {
	if (!was_dispatched_to)
	  was_dispatched_to =  widget;
      }
      
      return was_dispatched_to;
    }
  }
  /* should never reach here */
  return NULL;
}
