/*
  Simple program to start Emacs with its console window hidden.

  This program is provided purely for convenience, since most users will
  use Emacs in windowing (GUI) mode, and will not want to have an extra
  console window lying around.  */

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
  char * new_cmdline;
  char * p;
  char modname[MAX_PATH];

  if (!GetModuleFileName (NULL, modname, MAX_PATH))
    goto error;
  if ((p = strrchr (modname, '\\')) == NULL)
    goto error;
  *p = 0;

  new_cmdline = alloca (MAX_PATH + strlen (cmdline) + 1);
  strcpy (new_cmdline, modname);
  strcat (new_cmdline, "\\emacs.exe ");

  /* append original arguments if any; first look for -wait as first
     argument, and apply that ourselves */
  if (strncmp (cmdline, "-wait", 5) == 0)
  {
    wait_for_child = TRUE;
    cmdline += 5;
  }
  strcat (new_cmdline, cmdline);

  /* set emacs_dir variable if runemacs was in "%emacs_dir%\bin" */
  if ((p = strrchr (modname, '\\')) && stricmp (p, "\\bin") == 0)
  {
    *p = 0;
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
