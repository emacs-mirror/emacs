/* Emulate the X Resource Manager through the registry.
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

/* Written by Kevin Gallo */

#include <config.h>
#include "lisp.h"
#include "w32term.h"
#include "blockinput.h"

#include <stdio.h>
#include <string.h>

#define REG_ROOT "SOFTWARE\\GNU\\Emacs\\"

LPBYTE 
win32_get_string_resource (name, class, dwexptype)
     char *name, *class;
     DWORD dwexptype;
{
  LPBYTE lpvalue = NULL;
  HKEY hrootkey = NULL;
  DWORD dwType;
  DWORD cbData;
  BOOL ok = FALSE;
  
  BLOCK_INPUT;
  
  /* Check both the current user and the local machine to see if we have any resources */
  
  if (RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS
      || RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
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
      if (lpvalue) xfree (lpvalue);
      return (NULL);
    } 
  else 
    {
      return (lpvalue);
    }
}

/* Retrieve the string resource specified by NAME with CLASS from
   database RDB. */

char *
x_get_string_resource (rdb, name, class)
     int rdb;
     char *name, *class;
{
  return (win32_get_string_resource (name, class, REG_SZ));
}
