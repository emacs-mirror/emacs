/* This file overrides the R4 or R5 mit/lib/Xt/Initialize.c, except that
   the functions lwlib_GetFileDatabase(), lwlib_CombineFileDatabase(), and
   lwlib_xrdb_initialize() are called.  By doing this silly cpp hack, we
   avoid version skew problems.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#ifdef XlibSpecificationRelease
#if XlibSpecificationRelease >= 5
#define HAVE_X11R5
#endif
#endif

extern struct _XrmHashBucketRec *lwlib_GetFileDatabase ();
extern void lwlib_xrdb_initialize ();

/* Replace all calls to XrmGetFileDatabase() with lwlib_GetFileDatabase(),
   calls to XrmCombineFileDatabase() with lwlib_CombineFileDatabase(), and
   rename the defined _XtDisplayInitialize() function.
 */
#define XrmGetFileDatabase lwlib_GetFileDatabase
#define XrmCombineFileDatabase lwlib_CombineFileDatabase
#define _XtDisplayInitialize _orig_XtDisplayInitialize

/* Suck in the original code.  Don't change this: see comments in Imakefile. */
#include "Initialize.c"

#undef XrmGetFileDatabase
#undef XrmCombineFileDatabase
#undef _XtDisplayInitialize

/* Now provide a definition of _XtDisplayInitialize() which invokes the
   original code after calling our initialization hook.  Note that the R4
   and R5 versions of _XtDisplayInitialize() take different arguments.
 */

#ifndef HAVE_X11R5

void _XtDisplayInitialize(dpy, pd, name, class, urlist, num_urs, argc, argv)
	Display *dpy;
        XtPerDisplay pd;
	String name, class;
	XrmOptionDescRec *urlist;
	Cardinal num_urs;
	Cardinal *argc;
	char *argv[];
{
  lwlib_xrdb_initialize(dpy);
  _orig_XtDisplayInitialize(dpy, pd, name, class, urlist, num_urs, argc, argv);
}

#else /* HAVE_X11R5 */

void _XtDisplayInitialize(dpy, pd, name, urlist, num_urs, argc, argv)
	Display *dpy;
        XtPerDisplay pd;
	String name;
	XrmOptionDescRec *urlist;
	Cardinal num_urs;
	int *argc;
	char **argv;
{
  lwlib_xrdb_initialize(dpy);
  _orig_XtDisplayInitialize(dpy, pd, name, urlist, num_urs, argc, argv);
}

#endif	/* HAVE_X11R5 */
