#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include "XTests.h"
#include <stdio.h>

static Display *dpy;

static void
quit (dpy)
     Display *dpy;
{
  XCloseDisplay (dpy);
  exit (0);
}

static Colormap screen_colormap;

static unsigned long
obtain_color (color)
     char *color;
{
  int exists;
  XColor color_def;

  if (!screen_colormap)
    screen_colormap = DefaultColormap (dpy, DefaultScreen (dpy));

  exists = XParseColor (dpy, screen_colormap, color, &color_def)
    && XAllocColor (dpy, screen_colormap, &color_def);
  if (exists)
    return color_def.pixel;

  fprintf (stderr, "Can't get color; using black.");
  return BlackPixel (dpy, DefaultScreen (dpy));
}

static char *visual_strings[] =
{
  "StaticGray ",
  "GrayScale  ",
  "StaticColor",
  "PseudoColor",
  "TrueColor  ",
  "DirectColor"
};

main (argc,argv)
     int argc;
     char *argv[];
{
  char *dpy_string;
  int n;
  long mask;
  Visual *my_visual;
  XVisualInfo *vinfo, visual_template;
  XEvent event;
  Window window;
  Screen *scr;
  XGCValues gc_values;
  GC fill_gc, pix_gc, line_xor_gc, line_xor_inv_gc;
  int i;
  int x, y, width, height, geometry, gravity;
  char *geo;
  char default_geo[] = "80x40+0+0";
  int depth;
  Pixmap pix;
  char *string = "Kill the head and the body will die.";
  char dash_list[] = {6, 4, 6, 4};
  int dashes = 4;

  if (argc < 2)
    dpy_string = "localhost:0.0";
  else
    dpy_string = argv[1];

  if (argc >= 3)
    {
      XSizeHints hints;

      printf ("Geometry: %s\t(default: %s)\n", argv[2], default_geo);
      geo = argv[2];
      XWMGeometry (dpy, DefaultScreen (dpy), geo, default_geo,
		   3, &hints, &x, &y, &width, &height, &gravity);
    }

  dpy = XOpenDisplay (dpy_string);
  if (!dpy)
    {
      printf ("Can' open display %s\n", dpy_string);
      exit (1);
    }

  window = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
				300, 300, 300, 300, 1,
				BlackPixel (dpy, DefaultScreen (dpy)),
				WhitePixel (dpy, DefaultScreen (dpy)));
  XSelectInput (dpy, window, ButtonPressMask | KeyPressMask
		| EnterWindowMask | LeaveWindowMask);

  gc_values.foreground = obtain_color ("blue");
  gc_values.background = WhitePixel (dpy, DefaultScreen (dpy));
  fill_gc = XCreateGC (dpy, window, GCForeground | GCBackground,
		       &gc_values);

  gc_values.foreground = obtain_color ("red");
  gc_values.function = GXor;
  gc_values.line_width = 3;
  gc_values.line_style = LineOnOffDash;
  gc_values.cap_style = CapRound;
  gc_values.join_style = JoinRound;
  line_xor_gc = XCreateGC (dpy, window,
			   GCForeground | GCBackground | GCLineStyle
			   | GCJoinStyle | GCCapStyle | GCLineWidth
			   | GCFunction,
			   &gc_values);
  XSetDashes (dpy, line_xor_gc, 0, dash_list, dashes);

  gc_values.background = WhitePixel (dpy, DefaultScreen (dpy));
  gc_values.foreground = obtain_color ("blue");
  line_xor_inv_gc = XCreateGC (dpy, window,
			       GCForeground | GCBackground
			       | GCLineWidth | GCFunction,
			       &gc_values);

  depth = DefaultDepthOfScreen (ScreenOfDisplay (dpy, DefaultScreen (dpy)));
  pix = XCreateBitmapFromData (dpy, window, page_glyf_bits,
			       page_glyf_width, page_glyf_height);

  XMapWindow (dpy, window);
  XFlush (dpy);

  while (1)
    {
      XNextEvent (dpy, &event);
      switch (event.type)
	{
	case ButtonPress:
#if 0
	  if (event.xbutton.state && ShiftMask)
#endif
	    switch (event.xbutton.button)
	      {
	      case Button1:
		XDrawLine (dpy, window, line_xor_gc, 25, 75, 125, 75);
		XFlush (dpy);
		XDrawLine (dpy, window, line_xor_gc, 25, 75, 125, 75);
		break;

	      case Button2:
		XDrawLine (dpy, window, line_xor_gc, 25, 75, 125, 75);
		break;

	      case Button3:
		XDrawLine (dpy, window, line_xor_gc, 25, 75, 125, 75);
		break;
	      }
	  break;

	case KeyPress:
	  {
	    char buf[20];
	    int n;
	    XComposeStatus status;
	    KeySym keysym;

	    n = XLookupString (&event, buf, 20, &keysym,
			       (XComposeStatus *) &status);

	    if (n == 1 && buf[0] == 'q')
	      quit (dpy);
	  }
	  break;

	case EnterNotify:
	  XCopyPlane (dpy, pix, window, fill_gc, 0, 0,
		      page_glyf_width, page_glyf_height, 100, 100, 1L);
	  XFillRectangle (dpy, window, fill_gc, 50, 50, 50, 50);
	  break;

	case LeaveNotify:
	  XClearWindow (dpy, window);
	  break;
	}

      XFlush (dpy);
    }
}
