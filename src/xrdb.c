/* Deal with the X Resource Manager.
   Copyright (C) 1990, 1993, 1994 Free Software Foundation.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by jla, 4/90 */

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>

#if 1 /* I'd really appreciate it if this code could go away...  -JimB */
/* this avoids lossage in the `dual-universe' headers on AT&T SysV X11 */
#ifdef USG5
#ifndef SYSV
#define SYSV
#endif
#include <unistd.h>
#endif /* USG5 */

#endif /* 1 */

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#if 0
#include <X11/Xos.h>
#endif
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif
#include <sys/stat.h>

#if !defined(S_ISDIR) && defined(S_IFDIR)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

extern char *getenv ();

/* This does cause trouble on AIX.  I'm going to take the comment at
   face value.  */
#if 0
extern short getuid ();		/* If this causes portability problems,
				   I think we should just delete it; it'll
				   default to `int' anyway.  */
#endif

#ifdef DECLARE_GETPWUID_WITH_UID_T
extern struct passwd *getpwuid (uid_t);
extern struct passwd *getpwnam (const char *);
#else
extern struct passwd *getpwuid ();
extern struct passwd *getpwnam ();
#endif

extern char *get_system_name ();

/* Make sure not to #include anything after these definitions.  Let's
   not step on anyone's prototypes.  */
#ifdef emacs
#define malloc xmalloc
#define realloc xrealloc
#define free xfree
#endif

char *x_get_string_resource ();
static int file_p ();


/* X file search path processing.  */


/* The string which gets substituted for the %C escape in XFILESEARCHPATH
   and friends, or zero if none was specified.  */
char *x_customization_string;


/* Return the value of the emacs.customization (Emacs.Customization)
   resource, for later use in search path decoding.  If we find no
   such resource, return zero.  */
char *
x_get_customization_string (db, name, class)
     XrmDatabase db;
     char *name, *class;
{
  char *full_name
    = (char *) alloca (strlen (name) + sizeof ("customization") + 3);
  char *full_class
    = (char *) alloca (strlen (class) + sizeof ("Customization") + 3);
  char *result;

  sprintf (full_name,  "%s.%s", name,  "customization");
  sprintf (full_class, "%s.%s", class, "Customization");

  result = x_get_string_resource (db, full_name, full_class);

  if (result)
    {
      char *copy = (char *) malloc (strlen (result) + 1);
      strcpy (copy, result);
      return copy;
    }
  else
    return 0;
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

   Return the expanded file name if it exists and is readable, and
   refers to %L only when the LANG environment variable is set, or
   otherwise provided by X.

   ESCAPED_SUFFIX and SUFFIX are postpended to STRING if they are
   non-zero.  %-escapes in ESCAPED_SUFFIX are expanded; STRING is left
   alone.

   Return NULL otherwise.  */

static char *
magic_file_p (string, string_len, class, escaped_suffix, suffix)
     char *string;
     int string_len;
     char *class, *escaped_suffix, *suffix;
{
  char *lang = getenv ("LANG");

  int path_size = 100;
  char *path = (char *) malloc (path_size);
  int path_len = 0;

  char *p = string;

  while (p < string + string_len)
    {
      /* The chunk we're about to stick on the end of result.  */
      char *next;
      int next_len;

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
		next = (x_customization_string
			? x_customization_string
			: "");
		next_len = strlen (next);
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
		    free (path);
		    return NULL;
		  }
		
		next = lang;
		next_len = strlen (next);
		break;
	      
	      case 't':
	      case 'c':
		free (path);
		return NULL;
	      }
	}
      else
	next = p, next_len = 1;

      /* Do we have room for this component followed by a '\0' ?  */
      if (path_len + next_len + 1 > path_size)
	{
	  path_size = (path_len + next_len + 1) * 2;
	  path = (char *) realloc (path, path_size);
	}
      
      bcopy (next, path + path_len, next_len);
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

  /* Perhaps we should add the SUFFIX now.  */
  if (suffix)
    {
      int suffix_len = strlen (suffix);

      if (path_len + suffix_len + 1 > path_size)
	{
	  path_size = (path_len + suffix_len + 1);
	  path = (char *) realloc (path, path_size);
	}

      bcopy (suffix, path + path_len, suffix_len);
      path_len += suffix_len;
    }

  path[path_len] = '\0';

  if (! file_p (path))
    {
      free (path);
      return NULL;
    }

  return path;
}


static char *
gethomedir ()
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
    return "/";

  copy = (char *) malloc (strlen (ptr) + 2);
  strcpy (copy, ptr);
  strcat (copy, "/");

  return copy;
}


static int
file_p (path)
     char *path;
{
  struct stat status;

  return (access (path, 4) == 0			/* exists and is readable */
	  && stat (path, &status) == 0		/* get the status */
	  && (S_ISDIR (status.st_mode)) == 0);	/* not a directory */
}


/* Find the first element of SEARCH_PATH which exists and is readable,
   after expanding the %-escapes.  Return 0 if we didn't find any, and 
   the path name of the one we found otherwise.  */

static char *
search_magic_path (search_path, class, escaped_suffix, suffix)
     char *search_path, *class, *escaped_suffix, *suffix;
{
  register char *s, *p;

  for (s = search_path; *s; s = p)
    {
      for (p = s; *p && *p != ':'; p++)
	;
      
      if (p > s)
	{
	  char *path = magic_file_p (s, p - s, class, escaped_suffix, suffix);
	  if (path)
	    return path;
	}
      else if (*p == ':')
	{
	  char *path;

	  s = "%N%S";
	  path = magic_file_p (s, strlen (s), class, escaped_suffix, suffix);
	  if (path)
	    return path;
	}

      if (*p == ':')
	p++;
    }

  return 0;
}

/* Producing databases for individual sources.  */

#define X_DEFAULT_SEARCH_PATH "/usr/lib/X11/%L/%T/%N%C%S:/usr/lib/X11/%l/%T/%N%C%S:/usr/lib/X11/%T/%N%C%S:/usr/lib/X11/%L/%T/%N%S:/usr/lib/X11/%l/%T/%N%S:/usr/lib/X11/%T/%N%S"

static XrmDatabase
get_system_app (class)
     char *class;
{
  XrmDatabase db = NULL;
  char *path;

  path = getenv ("XFILESEARCHPATH");
  if (! path) path = X_DEFAULT_SEARCH_PATH;

  path = search_magic_path (path, class, 0, 0);
  if (path)
    {
      db = XrmGetFileDatabase (path);
      free (path);
    }

  return db;
}


static XrmDatabase
get_fallback (display)
     Display *display;
{
  XrmDatabase db;

  return NULL;
}


static XrmDatabase
get_user_app (class)
     char *class;
{
  char *path;
  char *file = 0;

  /* Check for XUSERFILESEARCHPATH.  It is a path of complete file
     names, not directories.  */
  if (((path = getenv ("XUSERFILESEARCHPATH"))
       && (file = search_magic_path (path, class, 0, 0)))

      /* Check for APPLRESDIR; it is a path of directories.  In each,
	 we have to search for LANG/CLASS and then CLASS.  */
      || ((path = getenv ("XAPPLRESDIR"))
	  && ((file = search_magic_path (path, class, "/%L/%N", 0))
	      || (file = search_magic_path (path, class, "/%N", 0))))
      
      /* Check in the home directory.  This is a bit of a hack; let's
	 hope one's home directory doesn't contain any %-escapes.  */
      || (path = gethomedir (),
	  ((file = search_magic_path (path, class, "%L/%N", 0))
	   || (file = search_magic_path (path, class, "%N", 0)))))
    {
      XrmDatabase db = XrmGetFileDatabase (file);
      free (file);
      return db;
    }
  else
    return NULL;
}


static XrmDatabase
get_user_db (display)
     Display *display;
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
      char *home;
      char *xdefault;

      home = gethomedir ();
      xdefault = (char *) malloc (strlen (home) + sizeof (".Xdefaults"));
      strcpy (xdefault, home);
      strcat (xdefault, ".Xdefaults");
      db = XrmGetFileDatabase (xdefault);
      free (home);
      free (xdefault);
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
get_environ_db ()
{
  XrmDatabase db;
  char *p;
  char *path = 0, *home = 0, *host;

  if ((p = getenv ("XENVIRONMENT")) == NULL)
    {
      home = gethomedir ();
      host = get_system_name ();
      path = (char *) malloc (strlen (home)
			      + sizeof (".Xdefaults-")
			      + strlen (host));
      sprintf (path, "%s%s%s", home, ".Xdefaults-", host);
      p = path;
    }

  db = XrmGetFileDatabase (p);

  if (path) free (path);
  if (home) free (home);

  return db;
}

/* External interface.  */

/* Types of values that we can find in a database */

#define XrmStringType "String"	/* String representation */
XrmRepresentation x_rm_string;	/* Quark representation */

/* Load X resources based on the display and a possible -xrm option. */

XrmDatabase
x_load_resources (display, xrm_string, myname, myclass)
     Display *display;
     char *xrm_string, *myname, *myclass;
{
  char *xdefs;
  XrmDatabase user_database;
  XrmDatabase rdb;
  XrmDatabase db;

  x_rm_string = XrmStringToQuark (XrmStringType);
#ifndef USE_X_TOOLKIT
  /* pmr@osf.org says this shouldn't be done if USE_X_TOOLKIT.
     I suspect it's because the toolkit version does this elsewhere.  */
  XrmInitialize ();
#endif
  rdb = XrmGetStringDatabase ("");

  user_database = get_user_db (display);

  /* Figure out what the "customization string" is, so we can use it
     to decode paths.  */
  if (x_customization_string)
    free (x_customization_string);
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

int
x_get_resource (rdb, name, class, expected_type, ret_value)
     XrmDatabase rdb;
     char *name, *class;
     XrmRepresentation expected_type;
     XrmValue *ret_value;
{
  XrmValue value;
  XrmName namelist[100];
  XrmClass classlist[100];
  XrmRepresentation type;

  XrmStringToNameList(name, namelist);
  XrmStringToClassList(class, classlist);

  if (XrmQGetResource (rdb, namelist, classlist, &type, &value) == True
      && (type == expected_type))
    {
      if (type == x_rm_string)
	ret_value->addr = (char *) value.addr;
      else
	bcopy (value.addr, ret_value->addr, ret_value->size);

      return value.size;
    }

  return 0;
}

/* Retrieve the string resource specified by NAME with CLASS from
   database RDB. */

char *
x_get_string_resource (rdb, name, class)
     XrmDatabase rdb;
     char *name, *class;
{
  XrmValue value;

  if (x_get_resource (rdb, name, class, x_rm_string, &value))
    return (char *) value.addr;

  return (char *) 0;
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
member (elt, list)
     char *elt;
     List list;
{
  List p;

  for (p = list; ! NIL (p); p = cdr (p))
    if (! strcmp (elt, car (p)))
      return p;

  return p;
}

static void
fatal (msg, prog, x1, x2, x3, x4, x5)
    char *msg, *prog;
    int x1, x2, x3, x4, x5;
{
    extern int errno;

    if (errno)
      perror (prog);

    (void) fprintf (stderr, msg, prog, x1, x2, x3, x4, x5);
    exit (1);
}

main (argc, argv)
    int argc;
    char **argv;
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
  if (! NIL (lp))
    resource_string = car (cdr (lp));
  else
    resource_string = (char *) 0;

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

  while (1)
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
}
#endif /* TESTRM */
