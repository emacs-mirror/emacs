#ifndef __NTINEVT_H__
#define __NTINEVT_H__

int win32_read_socket _P_((int sd, struct input_event *bufp, int numchars,
                         int waitp, int expected));
void win32_mouse_position _P_((FRAME_PTR *f,
                             Lisp_Object *bar_window,
                             enum scroll_bar_part *part,
                             Lisp_Object *x,
                             Lisp_Object *y,
                             unsigned long *time));

#endif
