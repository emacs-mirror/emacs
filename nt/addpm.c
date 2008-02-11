/* Add entries to the GNU Emacs Program Manager folder.
   Copyright (C) 1995, 2001, 2002, 2003, 2004, 2005,
      2006, 2007, 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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
#define REG_GTK "SOFTWARE\\GTK\\2.0"
#define REG_APP_PATH \
  "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\emacs.exe"

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
  int i;
  BOOL ok = TRUE;
  DWORD size;

  /* Record the location of Emacs to the App Paths key if we have
     sufficient permissions to do so.  This helps Windows find emacs quickly
     if the user types emacs.exe in the "Run Program" dialog without having
     emacs on their path.  Some applications also use the same registry key
     to discover the installation directory for programs they are looking for.
     Multiple installations cannot be handled by this method, but it does not
     affect the general operation of other installations of Emacs, and we
     are blindly overwriting the Start Menu entries already.
  */
  if (RegCreateKeyEx (HKEY_LOCAL_MACHINE, REG_APP_PATH, 0, "", 
                      REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL,
                      &hrootkey, NULL) == ERROR_SUCCESS)
    {
      int len;
      char *emacs_path;
      HKEY gtk_key = NULL;

      len = strlen (path) + 15; /* \bin\emacs.exe + terminator.  */
      emacs_path = alloca (len);
      sprintf (emacs_path, "%s\\bin\\emacs.exe", path);

      RegSetValueEx (hrootkey, NULL, 0, REG_SZ, emacs_path, len);

      /* Look for a GTK installation. If found, add it to the library search
         path for Emacs so that the image libraries it provides are available
         to Emacs regardless of whether it is in the path or not.  */
      if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_GTK, REG_OPTION_NON_VOLATILE,
                        KEY_READ, &gtk_key) == ERROR_SUCCESS)
        {
          if (RegQueryValueEx (gtk_key, "DllPath", NULL, NULL,
                               NULL, &size) == ERROR_SUCCESS)
            {
              char *gtk_path = (char *) alloca (size);
              if (RegQueryValueEx (gtk_key, "DllPath", NULL, NULL,
                                   gtk_path, &size) == ERROR_SUCCESS)
                {
                  /* Make sure the emacs bin directory continues to be searched
                     first by including it as well.  */
                  char *dll_paths;
                  len = strlen (path) + 5 + size;
                  dll_paths = (char *) alloca (size + strlen (path) + 1);
                  sprintf (dll_paths, "%s\\bin;%s", path, gtk_path);
                  RegSetValueEx (hrootkey, "Path", 0, REG_SZ, dll_paths, len);
                }
            }
          RegCloseKey (gtk_key);
        }
      RegCloseKey (hrootkey);
    }

  /* Previous versions relied on registry settings, but we do not need
     them any more.  If registry settings are installed from a previous
     version, replace them to ensure they are the current settings.
     Otherwise, do nothing.  */

  /* Check both the current user and the local machine to see if we
     have any resources.  */

  if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT,
		      REG_OPTION_NON_VOLATILE,
		      KEY_WRITE, &hrootkey) != ERROR_SUCCESS
      && RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT,
			 REG_OPTION_NON_VOLATILE,
			 KEY_WRITE, &hrootkey) != ERROR_SUCCESS)
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
      fprintf (stderr, "usage: addpm [-q] [emacs_path [icon_path]]\n");
      exit (1);
    }
#endif

  if (argc > 1
      && (argv[1][0] == '/' || argv[1][0] == '-')
      && argv[1][1] == 'q')
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

/* arch-tag: f923609d-b781-4ef4-abce-ca0da29cbbf0
   (do not change this comment) */
