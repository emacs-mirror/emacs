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

#ifdef USE_CAIRO

#include <cairo.h>
#include <fontconfig/fontconfig.h>

typedef struct {
  cairo_scaled_font_t *scaled_font;
  int ascent, descent, height, max_advance_width;
} XftFont;

typedef cairo_t XftDraw;

typedef struct {
  unsigned long pixel;
  struct {unsigned short red, green, blue, alpha;} color;
} XftColor;

#ifdef HAVE_XRENDER
#include <X11/extensions/Xrender.h>
#else
typedef struct {
  unsigned short width, height;
  short x, y, xOff, yOff;
} XGlyphInfo;
#endif

#define XftFontOpenName crxft_font_open_name
extern XftFont *crxft_font_open_name (Display *, int, const char *);
#define XftFontClose(dpy, pub) crxft_font_close (pub)
extern void crxft_font_close (XftFont *);
#define XftDrawCreate(dpy, drawable, visual, colormap) \
  crxft_draw_create (dpy, drawable, visual)
extern cairo_t *crxft_draw_create (Display *, Drawable, Visual *);
#define XftDrawDestroy cairo_destroy
#define XftDrawRect crxft_draw_rect
extern void crxft_draw_rect (cairo_t *, const XftColor *, int, int,
			     unsigned int, unsigned int);
#define XftDrawStringUtf8 crxft_draw_string
extern void crxft_draw_string (cairo_t *, const XftColor *, XftFont *,
			       int, int, const FcChar8 *, int);
#define XftTextExtentsUtf8(dpy, pub, string, len, extents) \
  crxft_text_extents (pub, string, len, extents)
extern void crxft_text_extents (XftFont *, const FcChar8 *, int, XGlyphInfo *);

#endif	/* USE_CAIRO */
#endif /* _LWLIB_UTILS_H_ */
