/*
  Simple program to start Emacs with its console window hidden.

  This program is provided purely for convenience, since most users will
  use Emacs in windowing (GUI) mode, and will not want to have an extra
  console window lying around.  */

/*
   You may want to define this if you want to be able to install updated
   emacs binaries even when other users are using the current version.
   The problem with some file servers (notably Novell) is that an open
   file cannot be overwritten, deleted, or even renamed.  So if someone
   is running emacs.exe already, you cannot install a newer version.
   By defining CHOOSE_NEWEST_EXE, you can name your new emacs.exe
   something else which matches "emacs*.exe", and runemacs will
   automatically select the newest emacs executeable in the bin directory.
   (So you'll probably be able to delete the old version some hours/days
   later).
*/

/* #define CHOOSE_NEWEST_EXE */

#define WIN32

#include <windows.h>
#include <string.h>
#include <malloc.h>

int WINAPI
WinMain (HINSTANCE hSelf, HINSTANCE hPrev, LPSTR cmdline, int nShow)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  SECURITY_DESCRIPTOR sec_desc;
  PROCESS_INFORMATION child;
  int wait_for_child = FALSE;
  DWORD ret_code = 0;
  char *new_cmdline;
  char *p;
  char modname[MAX_PATH];

  if (!GetModuleFileName (NULL, modname, MAX_PATH))
    goto error;
  if ((p = strrchr (modname, '\\')) == NULL)
    goto error;
  *p = 0;

  new_cmdline = alloca (MAX_PATH + strlen (cmdline) + 1);
  strcpy (new_cmdline, modname);

#ifdef CHOOSE_NEWEST_EXE
  {
    /* Silly hack to allow new versions to be installed on
       server even when current version is in use. */

    char * best_name = alloca (MAX_PATH + 1);
    FILETIME best_time = {0,0};
    WIN32_FIND_DATA wfd;
    HANDLE fh;
    p = new_cmdline + strlen (new_cmdline);
    strcpy (p, "\\emacs*.exe ");
    fh = FindFirstFile (new_cmdline, &wfd);
    if (fh == INVALID_HANDLE_VALUE)
      goto error;
    do
      {
        if (wfd.ftLastWriteTime.dwHighDateTime > best_time.dwHighDateTime
            || (wfd.ftLastWriteTime.dwHighDateTime == best_time.dwHighDateTime
                && wfd.ftLastWriteTime.dwLowDateTime > best_time.dwLowDateTime))
          {
            best_time = wfd.ftLastWriteTime;
            strcpy (best_name, wfd.cFileName);
          }
      }
    while (FindNextFile (fh, &wfd));
    FindClose (fh);
    *p++ = '\\';
    strcpy (p, best_name);
    strcat (p, " ");
  }
#else
  strcat (new_cmdline, "\\emacs.exe ");
#endif

  /* Append original arguments if any; first look for -wait as first
     argument, and apply that ourselves.  */
  if (strncmp (cmdline, "-wait", 5) == 0)
    {
      wait_for_child = TRUE;
      cmdline += 5;
    }
  strcat (new_cmdline, cmdline);

  /* Set emacs_dir variable if runemacs was in "%emacs_dir%\bin".  */
  if ((p = strrchr (modname, '\\')) && stricmp (p, "\\bin") == 0)
    {
      *p = 0;
      for (p = modname; *p; p++)
	if (*p == '\\') *p = '/';
      SetEnvironmentVariable ("emacs_dir", modname);
    }

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  start.dwFlags = STARTF_USESHOWWINDOW;
  start.wShowWindow = SW_HIDE;

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;

  if (CreateProcess (NULL, new_cmdline, &sec_attrs, NULL, TRUE, 0,
		     NULL, NULL, &start, &child))
    {
      if (wait_for_child)
	{
	  WaitForSingleObject (child.hProcess, INFINITE);
	  GetExitCodeProcess (child.hProcess, &ret_code);
	}
      CloseHandle (child.hThread);
      CloseHandle (child.hProcess);
    }
  else
    goto error;
  return (int) ret_code;

error:
  MessageBox (NULL, "Could not start Emacs.", "Error", MB_ICONSTOP);
  return 1;
}
