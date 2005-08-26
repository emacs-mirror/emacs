/* Interface of a lightweight menubar widget.
   Copyright (C) 2000, 2002, 2004  Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef _XlwMenu_h
#define _XlwMenu_h

/***********************************************************************
 *
 * XlwMenu Widget
 *
 ***********************************************************************/

#include "lwlib.h"

/* Resource names used by the XlwMenu widget */
#define XtNdisabledForeground "disabledForeground"
#define XtCDisabledForeground "DisabledForeground"
#define XtNbuttonForeground "buttonForeground"
#define XtCButtonForeground "ButtonForeground"
#define XtNmargin "margin"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"
#define XtNarrowSpacing "arrowSpacing"
#define XtNmenu "menu"
#define XtCMenu "Menu"
#define XtNopen "open"
#define XtNselect "select"
#define XtNhighlightCallback "highlightCallback"
#define XtNmenuBorderWidth "menuBorderWidth"
#define XtNhorizontal "horizontal"
#define XtCHorizontal "Horizontal"
#define XtNcursor "cursor"
#define XtNCursor "Cursor"
#define XtNshowGrip "showGrip"
#define XtCShowGrip "ShowGrip"
#define XtNresizeToPreferred "resizeToPreferred"
#define XtCResizeToPreferred "ResizeToPreferred"
#define XtNallowResize "allowResize"
#define XtCAllowResize "AllowResize"

/* Motif-compatible resource names */
#define XmNshadowThickness	"shadowThickness"
#define XmCShadowThickness	"ShadowThickness"
#define XmNtopShadowColor	"topShadowColor"
#define XmCTopShadowColor	"TopShadowColor"
#define XmNbottomShadowColor	"bottomShadowColor"
#define XmCBottomShadowColor	"BottomShadowColor"
#define XmNtopShadowPixmap	"topShadowPixmap"
#define XmCTopShadowPixmap	"TopShadowPixmap"
#define XmNbottomShadowPixmap	"bottomShadowPixmap"
#define XmCBottomShadowPixmap	"BottomShadowPixmap"
#define XmRHorizontalDimension	"HorizontalDimension"

typedef struct _XlwMenuRec *XlwMenuWidget;
typedef struct _XlwMenuClassRec *XlwMenuWidgetClass;

extern WidgetClass xlwMenuWidgetClass;

#endif /* _XlwMenu_h */

/* arch-tag: 0c019735-d61b-4080-be85-4fdd6e50ae07
   (do not change this comment) */
