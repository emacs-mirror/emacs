/* A general interface to the widgets of different toolkits.
   Copyright (C) 1992, 1993 Lucid, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This code reads a resource database file and filters it through cpp
   with the same set of preprocessor defines that `xrdb' uses.
   Call lwlib_xrdb_initialize(dpy) once, and then call the function
   lwlib_GetFileDatabase() instead of XrmGetFileDatabase(), 
   and lwlib_CombineFileDatabase() instead of XrmCombineFileDatabase().
 */

#ifndef __STDC_EXTENDED__
#define __STDC_EXTENDED__
#endif

#include <stdio.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/Xmu/SysUtil.h>
#include <sys/stat.h>

extern char *index ();

static int
file_p (path)
     char *path;
{
  struct stat status;

  return (access (path, R_OK) == 0		/* exists and is readable */
	  && stat (path, &status) == 0		/* get the status */
	  && (status.st_mode & S_IFDIR) == 0);	/* not a directory */
}

#ifndef CPP_PROGRAM
#define CPP_PROGRAM "/lib/cpp"
#endif

static char cpp_string [BUFSIZ];
static char *cpp_file = 0;

#define Resolution(pixels, mm) ((((pixels) * 100000 / (mm)) + 50) / 100)

void
lwlib_xrdb_initialize (display)
     Display *display;
{
  Screen *screen;
  Visual *visual;
  char server [255];
  char *colon, *s;

#define Push(str)  \
  (strncpy (s, str, sizeof(str)), s += (sizeof(str)-1))

#define Print(str, thing)  \
  (sprintf (s, str, thing), s = index (s, 0))

  s = cpp_string;
  Push (CPP_PROGRAM);

  Push (" -DCLIENTHOST=");
  XmuGetHostname (s, sizeof (cpp_string) - (s - cpp_string));
  s = index (s, 0);
  Push (" -DSERVERHOST=");
  strcpy (s, XDisplayName (DisplayString (display)));
  colon = index (s, ':');
  if (colon == s)
    {
      XmuGetHostname (s, sizeof (cpp_string) - (s - cpp_string));
      s = index (s, 0);
    }
  else if (colon)
    s = colon;
  else
    s = index (s, 0);
  
  Print (" -DVERSION=%d", ProtocolVersion(display));
  Print (" -DREVISION=%d", ProtocolRevision(display));
  Print (" -DVENDOR=\"%s\"", ServerVendor(display));
  Print (" -DRELEASE=%d", VendorRelease(display));
  screen = DefaultScreenOfDisplay(display);
  visual = DefaultVisualOfScreen(screen);
  Print (" -DWIDTH=%d", screen->width);
  Print (" -DHEIGHT=%d", screen->height);
  Print (" -DX_RESOLUTION=%d", Resolution(screen->width,screen->mwidth));
  Print (" -DY_RESOLUTION=%d", Resolution(screen->height,screen->mheight));
  Print (" -DPLANES=%d", DisplayPlanes(display, DefaultScreen(display)));
  Print (" -DBITS_PER_RGB=%d", visual->bits_per_rgb);
  switch(visual->class) {
  case StaticGray:	Print (" -DCLASS=%s", "StaticGray");	break;
  case GrayScale:	Print (" -DCLASS=%s", "GrayScale");	break;
  case StaticColor:	Print (" -DCLASS=%s", "StaticColor");
			Print (" -DCOLOR", 0); 			break;
  case PseudoColor:	Print (" -DCLASS=%s", "PseudoColor");
			Print (" -DCOLOR", 0);			break;
  case TrueColor:	Print (" -DCLASS=%s", "TrueColor");
			Print (" -DCOLOR", 0);			break;
  case DirectColor:	Print (" -DCLASS=%s", "DirectColor");
			Print (" -DCOLOR", 0);			break;
  default:
    fprintf (stderr, "unexpected visual class=%d\n", visual->class);
    exit (-1);
  }
  *s++ = ' ';
  *s = 0;
  cpp_file = s;
}

XrmDatabase
lwlib_GetFileDatabase (path)
     char *path;
{
  XrmDatabase db = 0;
  char line [BUFSIZ];
  char *s;
  FILE *file;

  if (! file_p (path))
    return 0;

  strcpy (cpp_file, path);
  if (! (file = popen (cpp_string, "r")))
    {
      fprintf (stderr,
	       "couldn't execute %s; resource file %s file not munged.\n",
	       CPP_PROGRAM, path);
      return XrmGetFileDatabase (path);
    }
  while (s = fgets (line, sizeof (line), file))
    {
      char ch, *tail;
      if (*s == '!') continue;
      for (; ((ch = *s) != '\n') && isspace(ch); s++);
      if ((ch == '\0') || (ch == '\n') || (ch == '#')) continue;
      tail = s + strlen (s);
      if (tail - s < 3) continue;   /* this would be syntactically incorrect */
      while (*(tail-1) == '\n' &&   /* handle \ at end of line */
	     *(tail-2) == '\\')
	{
	  if (! fgets (tail, sizeof (line) - (tail - line), file))
	    continue;
	  tail += strlen (tail);
	}
      XrmPutLineResource (&db, s);
    }
  pclose (file);
  return db;
}

#ifdef THIS_IS_X11R5

int
lwlib_CombineFileDatabase (path, target_db, override)
     char *path;
     XrmDatabase *target_db;
     Bool override;
{
  XrmDatabase source_db = lwlib_GetFileDatabase (path);
  if (! source_db)
    return (! file_p (path));
  XrmCombineDatabase (source_db, target_db, override);
  return 1;
}

#endif /* r5 */
