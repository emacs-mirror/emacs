/* Deal with the X Resource Manager.
   Copyright (C) 1990, 1993-1994, 2000-2017 Free Software Foundation,
   Inc.

Author: Joseph Arceneaux
Created: 4/90

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <unistd.h>
#include <errno.h>
#include <epaths.h>
#include <stdlib.h>
#include <stdio.h>

#include "lisp.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

/* X file search path processing.  */


/* The string which gets substituted for the %C escape in XFILESEARCHPATH
   and friends, or zero if none was specified.  */
static char *x_customization_string;


/* Return the value of the emacs.customization (Emacs.Customization)
   resource, for later use in search path decoding.  If we find no
   such resource, return zero.  */
static char *
x_get_customization_string (XrmDatabase db, const char *name,
			    const char *class)
{
  char *full_name = alloca (strlen (name) + sizeof "customization" + 3);
  char *full_class = alloca (strlen (class) + sizeof "Customization" + 3);
  char *result;

  sprintf (full_name,  "%s.%s", name,  "customization");
  sprintf (full_class, "%s.%s", class, "Customization");

  result = x_get_string_resource (db, full_name, full_class);
  return result ? xstrdup (result) : NULL;
}

/* Expand all the Xt-style %-escapes in STRING, whose length is given
   by STRING_LEN.  Here are the escapes we're supposed to recognize:

	%N	The value of the application's class name
	%T	The value of the type parameter ("app-defaults" in this
		context)
	%S	The value of the suffix parameter ("" in this context)
	%L	The language string associated with the specified display
		(We use the "LANG" environment variable here, if it's set.)
	%l	The language part of the display's language string
		(We treat this just like %L.  If someone can tell us what
		 we're really supposed to do, dandy.)
	%t	The territory part of the display's language string
	        (This never gets used.)
	%c	The codeset part of the display's language string
	        (This never gets used either.)
	%C	The customization string retrieved from the resource
		database associated with display.
		(This is x_customization_string.)

   Return the resource database if its file was read successfully, and
   refers to %L only when the LANG environment variable is set, or
   otherwise provided by X.

   ESCAPED_SUFFIX is postpended to STRING if it is non-zero.
   %-escapes in ESCAPED_SUFFIX are expanded.

   Return NULL otherwise.  */

static XrmDatabase
magic_db (const char *string, ptrdiff_t string_len, const char *class,
	  const char *escaped_suffix)
{
  XrmDatabase db;
  char *lang = getenv ("LANG");

  ptrdiff_t path_size = 100;
  char *path = xmalloc (path_size);
  ptrdiff_t path_len = 0;

  const char *p = string;

  while (p < string + string_len)
    {
      /* The chunk we're about to stick on the end of result.  */
      const char *next = p;
      ptrdiff_t next_len = 1;

      if (*p == '%')
	{
	  p++;

	  if (p >= string + string_len)
	    next_len = 0;
	  else
	    switch (*p)
	      {
	      case '%':
		next = "%";
		next_len = 1;
		break;

	      case 'C':
		if (x_customization_string)
		  {
		    next = x_customization_string;
		    next_len = strlen (next);
		  }
		else
		  next_len = 0;
		break;

	      case 'N':
		next = class;
		next_len = strlen (class);
		break;

	      case 'T':
		next = "app-defaults";
		next_len = strlen (next);
		break;

	      default:
	      case 'S':
		next_len = 0;
		break;

	      case 'L':
	      case 'l':
		if (! lang)
		  {
		    xfree (path);
		    return NULL;
		  }

		next = lang;
		next_len = strlen (next);
		break;

	      case 't':
	      case 'c':
		xfree (path);
		return NULL;
	      }
	}

      /* Do we have room for this component followed by a '\0'?  */
      if (path_size - path_len <= next_len)
	path = xpalloc (path, &path_size, path_len - path_size + next_len + 1,
			-1, sizeof *path);

      memcpy (path + path_len, next, next_len);
      path_len += next_len;

      p++;

      /* If we've reached the end of the string, append ESCAPED_SUFFIX.  */
      if (p >= string + string_len && escaped_suffix)
	{
	  string = escaped_suffix;
	  string_len = strlen (string);
	  p = string;
	  escaped_suffix = NULL;
	}
    }

  path[path_len] = '\0';
  db = XrmGetFileDatabase (path);
  xfree (path);
  return db;
}


static char *
gethomedir (void)
{
  struct passwd *pw;
  char *ptr;
  char *copy;

  if ((ptr = getenv ("HOME")) == NULL)
    {
      if ((ptr = getenv ("LOGNAME")) != NULL
	  || (ptr = getenv ("USER")) != NULL)
	pw = getpwnam (ptr);
      else
	pw = getpwuid (getuid ());

      if (pw)
	ptr = pw->pw_dir;
    }

  if (ptr == NULL)
    return xstrdup ("/");

  ptrdiff_t len = strlen (ptr);
  copy = xmalloc (len + 2);
  strcpy (copy + len, "/");
  return memcpy (copy, ptr, len);
}


/* Find the first element of SEARCH_PATH which exists and is readable,
   after expanding the %-escapes.  Return 0 if we didn't find any, and
   the path name of the one we found otherwise.  */

static XrmDatabase
search_magic_path (const char *search_path, const char *class,
		   const char *escaped_suffix)
{
  const char *s, *p;

  for (s = search_path; *s; s = p)
    {
      for (p = s; *p && *p != ':'; p++)
	;

      if (p > s)
	{
	  XrmDatabase db = magic_db (s, p - s, class, escaped_suffix);
	  if (db)
	    return db;
	}
      else if (*p == ':')
	{
	  static char const ns[] = "%N%S";
	  XrmDatabase db = magic_db (ns, strlen (ns), class, escaped_suffix);
	  if (db)
	    return db;
	}

      if (*p == ':')
	p++;
    }

  return 0;
}

/* Producing databases for individual sources.  */

static XrmDatabase
get_system_app (const char *class)
{
  const char *path;

  path = getenv ("XFILESEARCHPATH");
  if (! path) path = PATH_X_DEFAULTS;

  return search_magic_path (path, class, 0);
}


static XrmDatabase
get_fallback (Display *display)
{
  return NULL;
}


static XrmDatabase
get_user_app (const char *class)
{
  XrmDatabase db = 0;
  const char *path;

  /* Check for XUSERFILESEARCHPATH.  It is a path of complete file
     names, not directories.  */
  path = getenv ("XUSERFILESEARCHPATH");
  if (path)
    db = search_magic_path (path, class, 0);

  if (! db)
    {
      /* Check for APPLRESDIR; it is a path of directories.  In each,
	 we have to search for LANG/CLASS and then CLASS.  */
      path = getenv ("XAPPLRESDIR");
      if (path)
	{
	  db = search_magic_path (path, class, "/%L/%N");
	  if (!db)
	    db = search_magic_path (path, class, "/%N");
	}
    }

  if (! db)
    {
      /* Check in the home directory.  This is a bit of a hack; let's
	 hope one's home directory doesn't contain any %-escapes.  */
      char *home = gethomedir ();
      db = search_magic_path (home, class, "%L/%N");
      if (! db)
	db = search_magic_path (home, class, "%N");
      xfree (home);
    }

  return db;
}

static char const xdefaults[] = ".Xdefaults";

static XrmDatabase
get_user_db (Display *display)
{
  XrmDatabase db;
  char *xdefs;

#ifdef PBaseSize		/* Cheap way to test for X11R4 or later.  */
  xdefs = XResourceManagerString (display);
#else
  xdefs = display->xdefaults;
#endif

  if (xdefs != NULL)
    db = XrmGetStringDatabase (xdefs);
  else
    {
      /* Use ~/.Xdefaults.  */
      char *home = gethomedir ();
      ptrdiff_t homelen = strlen (home);
      char *filename = xrealloc (home, homelen + sizeof xdefaults);
      strcpy (filename + homelen, xdefaults);
      db = XrmGetFileDatabase (filename);
      xfree (filename);
    }

#ifdef HAVE_XSCREENRESOURCESTRING
  /* Get the screen-specific resources too.  */
  xdefs = XScreenResourceString (DefaultScreenOfDisplay (display));
  if (xdefs != NULL)
    {
      XrmMergeDatabases (XrmGetStringDatabase (xdefs), &db);
      XFree (xdefs);
    }
#endif

  return db;
}

static XrmDatabase
get_environ_db (void)
{
  XrmDatabase db;
  char *p = getenv ("XENVIRONMENT");
  char *filename = 0;

  if (!p)
    {
      /* Use ~/.Xdefaults-HOSTNAME.  */
      char *home = gethomedir ();
      ptrdiff_t homelen = strlen (home);
      Lisp_Object system_name = Fsystem_name ();
      ptrdiff_t filenamesize = (homelen + sizeof xdefaults
				+ 1 + SBYTES (system_name));
      p = filename = xrealloc (home, filenamesize);
      lispstpcpy (stpcpy (stpcpy (filename + homelen, xdefaults), "-"),
		  system_name);
    }

  db = XrmGetFileDatabase (p);

  xfree (filename);

  return db;
}

/* External interface.  */

/* Types of values that we can find in a database */

#define XrmStringType "String"	/* String representation */
static XrmRepresentation x_rm_string;	/* Quark representation */

/* Load X resources based on the display and a possible -xrm option. */

XrmDatabase
x_load_resources (Display *display, const char *xrm_string,
		  const char *myname, const char *myclass)
{
  XrmDatabase user_database;
  XrmDatabase rdb;
  XrmDatabase db;
  char line[256];

#if defined USE_MOTIF || !defined HAVE_XFT || !defined USE_LUCID
  const char *helv = "-*-helvetica-medium-r-*--*-120-*-*-*-*-iso8859-1";
#endif

#ifdef USE_MOTIF
  const char *courier = "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1";
#endif

  x_rm_string = XrmStringToQuark (XrmStringType);
#ifndef USE_X_TOOLKIT
  /* pmr@osf.org says this shouldn't be done if USE_X_TOOLKIT.
     I suspect it's because the toolkit version does this elsewhere.  */
  XrmInitialize ();
#endif
  rdb = XrmGetStringDatabase ("");

  /* Add some font defaults.  If the font `helv' doesn't exist, widgets
     will use some other default font.  */
#ifdef USE_MOTIF

  sprintf (line, "%s.pane.background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fontList: %s", myclass, helv);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*menu*background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*menubar*background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*verticalScrollBar.background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*verticalScrollBar.troughColor: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*horizontalScrollBar.background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*horizontalScrollBar.troughColor: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s.dialog*.background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb.Text.background: white", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb.FilterText.background: white", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb*DirList.background: white", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb*ItemsList.background: white", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb*background: grey75", myclass);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb.Text.fontList: %s", myclass, courier);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb.FilterText.fontList: %s", myclass, courier);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb*ItemsList.fontList: %s", myclass, courier);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "%s*fsb*DirList.fontList: %s", myclass, courier);
  XrmPutLineResource (&rdb, line);

  /* Set double click time of list boxes in the file selection
     dialog from `double-click-time'.  */
  if (INTEGERP (Vdouble_click_time) && XINT (Vdouble_click_time) > 0)
    {
      sprintf (line, "%s*fsb*DirList.doubleClickInterval: %"pI"d",
	       myclass, XFASTINT (Vdouble_click_time));
      XrmPutLineResource (&rdb, line);
      sprintf (line, "%s*fsb*ItemsList.doubleClickInterval: %"pI"d",
	       myclass, XFASTINT (Vdouble_click_time));
      XrmPutLineResource (&rdb, line);
    }

#else /* not USE_MOTIF */

  sprintf (line, "Emacs.dialog*.background: grey75");
  XrmPutLineResource (&rdb, line);
#if !defined (HAVE_XFT) || !defined (USE_LUCID)
  sprintf (line, "Emacs.dialog*.font: %s", helv);
  XrmPutLineResource (&rdb, line);
  sprintf (line, "*XlwMenu*font: %s", helv);
  XrmPutLineResource (&rdb, line);
#endif
  sprintf (line, "*XlwMenu*background: grey75");
  XrmPutLineResource (&rdb, line);
  sprintf (line, "Emacs*verticalScrollBar.background: grey75");
  XrmPutLineResource (&rdb, line);
  sprintf (line, "Emacs*horizontalScrollBar.background: grey75");
  XrmPutLineResource (&rdb, line);

#endif /* not USE_MOTIF */

  user_database = get_user_db (display);

  /* Figure out what the "customization string" is, so we can use it
     to decode paths.  */
  xfree (x_customization_string);
  x_customization_string
    = x_get_customization_string (user_database, myname, myclass);

  /* Get application system defaults */
  db = get_system_app (myclass);
  if (db != NULL)
    XrmMergeDatabases (db, &rdb);

  /* Get Fallback resources */
  db = get_fallback (display);
  if (db != NULL)
    XrmMergeDatabases (db, &rdb);

  /* Get application user defaults */
  db = get_user_app (myclass);
  if (db != NULL)
    XrmMergeDatabases (db, &rdb);

  /* get User defaults */
  if (user_database != NULL)
    XrmMergeDatabases (user_database, &rdb);

  /* Get Environment defaults. */
  db = get_environ_db ();
  if (db != NULL)
    XrmMergeDatabases (db, &rdb);

  /* Last, merge in any specification from the command line. */
  if (xrm_string != NULL)
    {
      db = XrmGetStringDatabase (xrm_string);
      if (db != NULL)
	XrmMergeDatabases (db, &rdb);
    }

  return rdb;
}


/* Retrieve the value of the resource specified by NAME with class CLASS
   and of type TYPE from database RDB.  The value is returned in RET_VALUE. */

static int
x_get_resource (XrmDatabase rdb, const char *name, const char *class,
		XrmRepresentation expected_type, XrmValue *ret_value)
{
  XrmValue value;
  XrmName namelist[100];
  XrmClass classlist[100];
  XrmRepresentation type;

  XrmStringToNameList (name, namelist);
  XrmStringToClassList (class, classlist);

  if (XrmQGetResource (rdb, namelist, classlist, &type, &value) == True
      && (type == expected_type))
    {
      if (type == x_rm_string)
	ret_value->addr = (char *) value.addr;
      else
	memcpy (ret_value->addr, value.addr, ret_value->size);

      return value.size;
    }

  return 0;
}

/* Retrieve the string resource specified by NAME with CLASS from
   database RDB. */

char *
x_get_string_resource (XrmDatabase rdb, const char *name, const char *class)
{
  XrmValue value;

  if (inhibit_x_resources)
    /* --quick was passed, so this is a no-op.  */
    return NULL;

  if (x_get_resource (rdb, name, class, x_rm_string, &value))
    return (char *) value.addr;

  return 0;
}

/* Stand-alone test facilities.  */

#ifdef TESTRM

typedef char **List;
#define arg_listify(len, list) (list)
#define car(list) (*(list))
#define cdr(list) (list + 1)
#define NIL(list) (! *(list))
#define free_arglist(list)

static List
member (char *elt, List list)
{
  List p;

  for (p = list; ! NIL (p); p = cdr (p))
    if (! strcmp (elt, car (p)))
      return p;

  return p;
}

static void
fatal (char *msg, char *prog)
{
  fprintf (stderr, msg, prog);
  exit (1);
}

int
main (int argc, char **argv)
{
  Display *display;
  char *displayname, *resource_string, *class, *name;
  XrmDatabase xdb;
  List arg_list, lp;

  arg_list = arg_listify (argc, argv);

  lp = member ("-d", arg_list);
  if (!NIL (lp))
    displayname = car (cdr (lp));
  else
    displayname = "localhost:0.0";

  lp = member ("-xrm", arg_list);
  resource_string = NIL (lp) ? 0 : car (cdr (lp));

  lp = member ("-c", arg_list);
  if (! NIL (lp))
    class = car (cdr (lp));
  else
    class = "Emacs";

  lp = member ("-n", arg_list);
  if (! NIL (lp))
    name = car (cdr (lp));
  else
    name = "emacs";

  free_arglist (arg_list);

  if (!(display = XOpenDisplay (displayname)))
    fatal ("Can't open display '%s'\n", XDisplayName (displayname));

  xdb = x_load_resources (display, resource_string, name, class);

  /* In a real program, you'd want to also do this: */
  display->db = xdb;

  while (true)
    {
      char query_name[90];
      char query_class[90];

      printf ("Name: ");
      gets (query_name);

      if (strlen (query_name))
	{
	  char *value;

	  printf ("Class: ");
	  gets (query_class);

	  value = x_get_string_resource (xdb, query_name, query_class);

	  if (value != NULL)
	    printf ("\t%s(%s):  %s\n\n", query_name, query_class, value);
	  else
	    printf ("\tNo Value.\n\n");
	}
      else
	break;
    }
  printf ("\tExit.\n\n");

  XCloseDisplay (display);

  return 0;
}
#endif /* TESTRM */
