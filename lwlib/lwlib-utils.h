#ifndef _LWLIB_UTILS_H_
#define _LWLIB_UTILS_H_

void XtNoClearRefreshWidget __P ((Widget));

typedef void (*XtApplyToWidgetsProc) __P ((Widget, XtPointer));
typedef void* (*XtApplyUntilToWidgetsProc) __P ((Widget, XtPointer));

void XtApplyToWidgets __P ((Widget, XtApplyToWidgetsProc, XtPointer));
void *XtApplyUntilToWidgets __P ((Widget, XtApplyUntilToWidgetsProc, XtPointer));

Widget *XtCompositeChildren __P ((Widget, unsigned int *));

/* returns True is the widget is being destroyed, False otherwise */
Boolean
XtWidgetBeingDestroyedP __P ((Widget widget));

void XtSafelyDestroyWidget __P ((Widget));

#endif /* _LWLIB_UTILS_H_ */
