/* Add entries to the GNU Emacs Program Manager folder.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/****************************************************************************
 *
 * Program: addpm	(adds emacs to the Windows program manager)
 *
 * Usage:
 *   	argv[1] = install path for emacs
 *	argv[2] = full path to icon for emacs (optional)
 */

#include <windows.h>
#include <ddeml.h>
#include <stdlib.h>
#include <stdio.h>

HDDEDATA CALLBACK 
DdeCallback (UINT uType, UINT uFmt, HCONV hconv,
	     HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
	     DWORD dwData1, DWORD dwData2)
{
  return ((HDDEDATA) NULL);
}

#define DdeCommand(str) 	\
	DdeClientTransaction (str, strlen (str)+1, HConversation, (HSZ)NULL, \
		              CF_TEXT, XTYP_EXECUTE, 30000, NULL)

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

static struct entry
{
  char *name;
  char *value;
} 
env_vars[] = 
{
  {"emacs_dir", NULL},
  {"EMACSLOADPATH", "%emacs_dir%/site-lisp;%emacs_dir%/../site-lisp;%emacs_dir%/lisp;%emacs_dir%/leim"},
  {"SHELL", "%emacs_dir%/bin/cmdproxy.exe"},
  {"EMACSDATA", "%emacs_dir%/etc"},
  {"EMACSPATH", "%emacs_dir%/bin"},
  /* We no longer set INFOPATH because Info-default-directory-list
     is then ignored.  */
  /*  {"INFOPATH", "%emacs_dir%/info"},  */
  {"EMACSDOC", "%emacs_dir%/etc"},
  {"TERM", "cmd"}
};

BOOL 
add_registry (path)
     char *path;
{
  HKEY hrootkey = NULL;
  DWORD dwDisp;
  int i;
  BOOL ok = TRUE;
  
  /* Check both the current user and the local machine to see if we 
     have any resources.  */
  
  if (RegCreateKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT,
		      0, "", REG_OPTION_NON_VOLATILE,
		      KEY_WRITE, NULL, &hrootkey, &dwDisp) != ERROR_SUCCESS 
      && RegCreateKeyEx (HKEY_CURRENT_USER, REG_ROOT,
			 0, "", REG_OPTION_NON_VOLATILE,
			 KEY_WRITE, NULL, &hrootkey, &dwDisp) != ERROR_SUCCESS)
    {
      return FALSE;
    }
  
  for (i = 0; i < (sizeof (env_vars) / sizeof (env_vars[0])); i++) 
    {
      char * value = env_vars[i].value ? env_vars[i].value : path;
	
      if (RegSetValueEx (hrootkey, env_vars[i].name,
			 0, REG_EXPAND_SZ,
			 value, lstrlen (value) + 1) != ERROR_SUCCESS)
	ok = FALSE;
    }			      
  
  RegCloseKey (hrootkey);
  
  return (ok);
}

int
main (argc, argv)
     int argc;
     char *argv[];			
{
  DWORD idDde = 0;
  HCONV HConversation;
  HSZ ProgMan;
  char modname[MAX_PATH];
  char additem[MAX_PATH*2 + 100];
  char *prog_name;
  char *emacs_path;
  char *p;
  int quiet = 0;

  /* If no args specified, use our location to set emacs_path.  */
#if 0
  if (argc < 2 || argc > 3)
    {
      fprintf (stderr, "usage: addpm [/q] [emacs_path [icon_path]]\n");
      exit (1);
    }
#endif

  if (argc > 1 && argv[1][0] == '/' && argv[1][1] == 'q')
    {
      quiet = 1;
      --argc;
      ++argv;
    }

  if (argc > 1)
    emacs_path = argv[1];
  else
    {
      if (!GetModuleFileName (NULL, modname, MAX_PATH) ||
	  (p = strrchr (modname, '\\')) == NULL)
	{
	  fprintf (stderr, "fatal error");
	  exit (1);
	}
      *p = 0;

      /* Set emacs_path to emacs_dir if we are in "%emacs_dir%\bin".  */
      if ((p = strrchr (modname, '\\')) && stricmp (p, "\\bin") == 0)
	{
	  *p = 0;
	  emacs_path = modname;
	}
      else
	{
	  fprintf (stderr, "usage: addpm emacs_path [icon_path]\n");
	  exit (1);
	}

      /* Tell user what we are going to do.  */
      if (!quiet)
	{
	  int result;

	  char msg[ MAX_PATH ];
	  sprintf (msg, "Install Emacs at %s?\n", emacs_path);
	  result = MessageBox (NULL, msg, "Install Emacs",
			       MB_OKCANCEL | MB_ICONQUESTION);
	  if (result != IDOK)
	    {
	      fprintf (stderr, "Install cancelled\n");
	      exit (1);
	    }
	}
    }

  add_registry (emacs_path);
  prog_name =  "runemacs.exe";

  DdeInitialize (&idDde, (PFNCALLBACK)DdeCallback, APPCMD_CLIENTONLY, 0);

  ProgMan = DdeCreateStringHandle (idDde, "PROGMAN", CP_WINANSI);

  HConversation = DdeConnect (idDde, ProgMan, ProgMan, NULL);
  if (HConversation != 0)
    {
      DdeCommand ("[CreateGroup (\"Gnu Emacs\")]");
      DdeCommand ("[ReplaceItem (Emacs)]");
      if (argc > 2)
	sprintf (additem, "[AddItem (\"%s\\bin\\%s\", Emacs, \"%s\")]",
		 emacs_path, prog_name, argv[2]);
      else
	sprintf (additem, "[AddItem (\"%s\\bin\\%s\", Emacs)]",
		 emacs_path, prog_name);
      DdeCommand (additem);

      DdeDisconnect (HConversation);
    }

  DdeFreeStringHandle (idDde, ProgMan);

  DdeUninitialize (idDde);

  return (0);
}
