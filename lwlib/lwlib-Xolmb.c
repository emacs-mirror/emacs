/* An OLIT menubar widget, by Chuck Thompson <cthomp@cs.uiuc.edu>
   Copyright (C) 1993 Lucid, Inc.

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
Boston, MA 02111-1307, USA.  */


#include "../src/lisp.h"

#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/CompositeP.h>
#include <X11/Composite.h>
#include "lwlib-Xol-mbP.h"
#include "lwlib-Xol-mb.h"

#define HORIZ_SPACING	4
#define VERT_SPACING	4

static void		Initialize();
static void		Resize();
static void		ChangeManaged();
static Boolean		SetValues();
static XtGeometryResult	GeometryManager();
static XtGeometryResult	PreferredSize();
static void		do_layout();
static XtGeometryResult try_layout();

lwMenuBarClassRec lwMenubarClassRec = 
{
  {
    /* core_class members */
    
    (WidgetClass) &compositeClassRec,	/* superclass			*/
    "Menubar",				/* class_name			*/
    sizeof(lwMenuBarRec),		/* widget_size			*/
    NULL,				/* class_initialize		*/
    NULL,				/* class_part_initialize	*/
    FALSE,				/* class_inited			*/
    Initialize,				/* initialize			*/
    NULL,				/* initialize_hook		*/
    XtInheritRealize,			/* realize			*/
    NULL,				/* actions			*/
    0,					/* num_actions			*/
    NULL,				/* resources			*/
    0,					/* num_resources		*/
    NULLQUARK,				/* xrm_class			*/
    TRUE,				/* compress_motion		*/
    XtExposeCompressMaximal,		/* compress_exposure		*/
    TRUE,				/* compress_enterleave		*/
    FALSE,				/* visible_interest		*/
    NULL,				/* destroy			*/
    Resize,				/* resize			*/
    NULL,				/* expose			*/
    NULL,				/* set_values			*/
    NULL,				/* set_values_hook		*/
    XtInheritSetValuesAlmost,		/* set_values_almost		*/
    NULL,				/* get_values_hook		*/
    NULL,				/* accept_focus			*/
    XtVersion,				/* version			*/
    NULL,				/* callback_private		*/
    NULL,				/* tm_table			*/
    PreferredSize,			/* query_geometry		*/
    NULL,				/* display_accelerator		*/
    NULL,				/* extension			*/
  },
  {
    /* composite_class members */
    
    GeometryManager,			/* geometry_manager		*/
    ChangeManaged,			/* change_managed		*/
    XtInheritInsertChild,		/* insert_child			*/
    XtInheritDeleteChild,		/* delete_child			*/
    NULL,				/* extension			*/
  },
  {
    /* Menubar class members */
    
    0,					/* empty			*/
  }
};
WidgetClass lwMenubarWidgetClass = (WidgetClass) &lwMenubarClassRec;


static void Initialize (request, new)
     lwMenuBarWidget	request, new;
{
  if (request->core.width <= 0)
    new->core.width = 1;
  if (request->core.height <= 0)
    new->core.height = 23;
}

static void
Resize (w)
     lwMenuBarWidget	w;
{
  do_layout(w);
}

static void
do_layout (parent)
     lwMenuBarWidget parent;
{
  Widget	child;
  int		cnt;
  int		managed_children = 0;
  int		managed_width = 0;
  int		new_pos = 0;

  /*
   * Determine number of children which will fit on one line.
   * For now we ignore the rest, making sure they are unmanaged.
   */

  cnt = 0;
  while ((cnt < (int) parent->composite.num_children) &&
	 (managed_width < (int) parent->core.width))
    {
      child = parent->composite.children[cnt++];
      managed_children++;
      managed_width += child->core.width + child->core.border_width * 2 +
	HORIZ_SPACING;
    }
  
  if (managed_width > (int) parent->core.width)
    managed_children--;
  
  /*
   * Correct positioning of children.
   */

  cnt = 0;
  while (managed_children)
    {
      child = parent->composite.children[cnt++];

      if (!child->core.managed)
	XtManageChild (child);

      if ((child->core.x != new_pos) || (child->core.y != 0))
	XtMoveWidget (child, new_pos, 0);
      new_pos += child->core.width + child->core.border_width * 2 +
	HORIZ_SPACING;

      managed_children--;
    }
  
  /*
   * Make sure all remaining children are unmanaged.
   */

  while (cnt < parent->composite.num_children)
    {
      child = parent->composite.children[cnt];
      
      if (child->core.managed)
	XtUnmanageChild (child);

      if ((child->core.x != parent->core.width) || 
	  (child->core.y != parent->core.height))
	XtMoveWidget (child, parent->core.width, parent->core.height);

      cnt++;
    }
}


static XtGeometryResult
PreferredSize (w, request, preferred)
     lwMenuBarWidget	w;
     XtWidgetGeometry	*request, *preferred;
{
  Widget	child;
  int		cnt;

  /*
   * If no changes are being made to the width or height, just agree.
   */

  if (!(request->request_mode & CWWidth) &&
      !(request->request_mode & CWHeight))
    return (XtGeometryYes);
  
  /*
   * Right now assume everything goes in one row.  Calculate the
   * minimum required width and height.
   */

  preferred->width = 0;
  preferred->height = 0;
  
  for (cnt = 0; cnt < w->composite.num_children; cnt++)
    {
      child = w->composite.children[cnt];
      if (child->core.managed)
	{
	  preferred->width += child->core.width + child->core.border_width*2 +
	    HORIZ_SPACING;
	  if (preferred->height < (Dimension) (child->core.height +
	      child->core.border_width * 2))
	    preferred->height = child->core.height +
	      child->core.border_width * 2;
	}
    }
  
  preferred->request_mode = CWWidth | CWHeight;

  /*
   * Case:  both height and width requested
   */

  if ((request->request_mode & CWWidth) &&
      (request->request_mode & CWHeight))
    {
      /*
       * Ok if same or bigger.
       */

      if (preferred->width <= request->width &&
	  preferred->height <= request->height)
	{
	  preferred->width = request->width;
	  return (XtGeometryYes);
	}
      
      /*
       * If both dimensions are too small, say no.
       */

      else
	if (preferred->width > request->width &&
	    preferred->height > request->height)
	  return (XtGeometryNo);
      
      /*
       * Otherwise one must be right, so say almost.
       */

      else
	return (XtGeometryAlmost);
    }
  
  /*
   * If only one dimension is requested, either its OK or it isn't.
   */

  else
    {
      if (request->request_mode & CWWidth)
	{
	  if (preferred->width <= request->width)
	    {
	      preferred->width = request->width;
	      return (XtGeometryYes);
	    }
	  else
	    return (XtGeometryNo);
	}
      else if (request->request_mode & CWHeight)
	{
	  if (preferred->height <= request->height)
	    {
	      return (XtGeometryYes);
	    }
	  else
	    return (XtGeometryNo);
	}

      return (XtGeometryYes);
    }
}
  

static XtGeometryResult
GeometryManager (w, request, reply)
     Widget		w;
     XtWidgetGeometry	*request;
     XtWidgetGeometry	*reply;
{
  
  lwMenuBarWidget	parent = (lwMenuBarWidget) w->core.parent;
  
  /*
   * If the widget wants to move, just say no.
   */

  if ((request->request_mode & CWX && request->x != w->core.x) ||
   (request->request_mode & CWY && request->y != w->core.y))
    return (XtGeometryNo);
  
  /*
   * Since everything "fits" for now, grant all requests.
   */

  if (request->request_mode & CWWidth)
    w->core.width = request->width;
  if (request->request_mode & CWHeight)
    w->core.height = request->height;
  if (request->request_mode & CWBorderWidth)
    w->core.border_width = request->border_width;
  
  do_layout (parent);
  return (XtGeometryYes);
}


static XtGeometryResult
try_layout (parent)
     lwMenuBarWidget parent;
{
  Widget	child;
  int		cnt;
  int		managed_children = 0;
  int		managed_width = 0;
  int		new_pos = 0;

  /*
   * Determine number of children which will fit on one line.
   * For now we ignore the rest, making sure they are unmanaged.
   */

  cnt = 0;
  while ((cnt < (int) parent->composite.num_children) &&
	 (managed_width < (int) parent->core.width))
    {
      child = parent->composite.children[cnt++];
      if (child->core.managed)
	{
	  managed_children++;
	  managed_width += child->core.width + child->core.border_width * 2 +
	    HORIZ_SPACING;
	}
    }

  if (managed_width > (int) parent->core.width)
    return (XtGeometryNo);
  else
    return (XtGeometryYes);
}


     
static void
ChangeManaged (w)
     lwMenuBarWidget	w;
{
  XtGeometryResult	result;
  
  result = try_layout (w);

  if (result != XtGeometryYes)
    {
      XtUnmanageChild (w->composite.children[w->composite.num_children - 1]);
      XtMoveWidget (w->composite.children[w->composite.num_children-1],
		    w->core.width, w->core.height);
    }
  
  do_layout (w);
}
