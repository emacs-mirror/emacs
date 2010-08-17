#ifndef _LWLIB_UTILS_H_
#define _LWLIB_UTILS_H_

void XtNoClearRefreshWidget (Widget);

typedef void (*XtApplyToWidgetsProc) (Widget, XtPointer);
typedef void* (*XtApplyUntilToWidgetsProc) (Widget, XtPointer);

void XtApplyToWidgets (Widget, XtApplyToWidgetsProc, XtPointer);
void *XtApplyUntilToWidgets (Widget, XtApplyUntilToWidgetsProc, XtPointer);

Widget *XtCompositeChildren (Widget, unsigned int *);

/* returns True is the widget is being destroyed, False otherwise */
Boolean
XtWidgetBeingDestroyedP (Widget widget);

void XtSafelyDestroyWidget (Widget);

#endif /* _LWLIB_UTILS_H_ */

/* arch-tag: 705efd86-9319-4447-80f6-16aa5b349809
   (do not change this comment) */
