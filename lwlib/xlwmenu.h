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

void
pop_up_menu __P ((XlwMenuWidget, XButtonPressedEvent*));

#endif /* _XlwMenu_h */
