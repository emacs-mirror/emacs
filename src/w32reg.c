/* Emulate the X Resource Manager through the registry.
   Copyright (C) 1990, 1993, 1994 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Written by Kevin Gallo */

#include <config.h>
#include "lisp.h"
#include "w32term.h"
#include "blockinput.h"

#include <stdio.h>
#include <string.h>

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

static char *
w32_get_rdb_resource (rdb, resource)
     char *rdb;
     char *resource;
{
  char *value = rdb;
  int len = strlen (resource);

  while (*value)
    {
      /* Comparison is case-insensitive because registry searches are too.  */
      if ((strnicmp (value, resource, len) == 0) && (value[len] == ':'))
        return xstrdup (&value[len + 1]);

      value = strchr (value, '\0') + 1;
    }

  return NULL;
}

LPBYTE
w32_get_string_resource (name, class, dwexptype)
     char *name, *class;
     DWORD dwexptype;
{
  LPBYTE lpvalue = NULL;
  HKEY hrootkey = NULL;
  DWORD dwType;
  DWORD cbData;
  BOOL ok = FALSE;
  HKEY hive = HKEY_CURRENT_USER;

 trykey:

  BLOCK_INPUT;

  /* Check both the current user and the local machine to see if we have
     any resources */

  if (RegOpenKeyEx (hive, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      char *keyname;

      if (RegQueryValueEx (hrootkey, name, NULL, &dwType, NULL, &cbData) == ERROR_SUCCESS
	  && dwType == dwexptype)
	{
	  keyname = name;
	}
      else if (RegQueryValueEx (hrootkey, class, NULL, &dwType, NULL, &cbData) == ERROR_SUCCESS
	       && dwType == dwexptype)
	{
	  keyname = class;
	}
      else
	{
	  keyname = NULL;
	}

      ok = (keyname
	    && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL
	    && RegQueryValueEx (hrootkey, keyname, NULL, NULL, lpvalue, &cbData) == ERROR_SUCCESS);

      RegCloseKey (hrootkey);
    }

  UNBLOCK_INPUT;

  if (!ok)
    {
      if (lpvalue)
	{
	  xfree (lpvalue);
	  lpvalue = NULL;
	}
      if (hive == HKEY_CURRENT_USER)
	{
	  hive = HKEY_LOCAL_MACHINE;
	  goto trykey;
	}
      return (NULL);
    }
  return (lpvalue);
}

/* Retrieve the string resource specified by NAME with CLASS from
   database RDB. */

char *
x_get_string_resource (rdb, name, class)
     XrmDatabase rdb;
     char *name, *class;
{
  if (rdb)
    {
      char *resource;

      if (resource = w32_get_rdb_resource (rdb, name))
        return resource;
      if (resource = w32_get_rdb_resource (rdb, class))
        return resource;
    }

  return (w32_get_string_resource (name, class, REG_SZ));
}
