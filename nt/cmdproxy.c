/* Proxy shell designed for use with Emacs on Windows 95 and NT.
   Copyright (C) 1997 Free Software Foundation, Inc.

   Accepts subset of Unix sh(1) command-line options, for compatability
   with elisp code written for Unix.  When possible, executes external
   programs directly (a common use of /bin/sh by Emacs), otherwise
   invokes the user-specified command processor to handle built-in shell
   commands, batch files and interactive mode.

   The main function is simply to process the "-c string" option in the
   way /bin/sh does, since the standard Windows command shells use the
   convention that everything after "/c" (the Windows equivalent of
   "-c") is the input string.

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

#include <windows.h>

#include <stdarg.h>  /* va_args */
#include <malloc.h>  /* alloca */
#include <stdlib.h>  /* getenv */
#include <string.h>  /* strlen */


/*******  Mock C library routines  *********************************/

/* These routines are used primarily to minimize the executable size.  */

#define stdin  GetStdHandle (STD_INPUT_HANDLE)
#define stdout GetStdHandle (STD_OUTPUT_HANDLE)
#define stderr GetStdHandle (STD_ERROR_HANDLE)

int
vfprintf(HANDLE hnd, char * msg, va_list args)
{
  DWORD bytes_written;
  char buf[1024];

  wvsprintf (buf, msg, args);
  return WriteFile (hnd, buf, strlen (buf), &bytes_written, NULL);
}

int
fprintf(HANDLE hnd, char * msg, ...)
{
  va_list args;
  int rc;

  va_start (args, msg);
  rc = vfprintf (hnd, msg, args);
  va_end (args);

  return rc;
}

int
printf(char * msg, ...)
{
  va_list args;
  int rc;

  va_start (args, msg);
  rc = vfprintf (stdout, msg, args);
  va_end (args);

  return rc;
}

void
fail (char * msg, ...)
{
  va_list args;

  va_start (args, msg);
  vfprintf (stderr, msg, args);
  va_end (args);

  exit (1);
}

void
warn (char * msg, ...)
{
  va_list args;

  va_start (args, msg);
  vfprintf (stderr, msg, args);
  va_end (args);
}

/******************************************************************/

char *
canon_filename (char *fname)
{
  char *p = fname;

  while (*p)
    {
      if (*p == '/')
	*p = '\\';
      p++;
    }

  return fname;
}

char *
skip_space (char *str)
{
  while (isspace (*str)) str++;
  return str;
}

char *
skip_nonspace (char *str)
{
  while (*str && !isspace (*str)) str++;
  return str;
}

int escape_char = '\\';

/* Get next token from input, advancing pointer.  */
int
get_next_token (char * buf, char ** pSrc)
{
  char * p = *pSrc;
  char * o = buf;

  p = skip_space (p);
  if (*p == '"')
    {
      int escape_char_run = 0;

      /* Go through src until an ending quote is found, unescaping
	 quotes along the way.  If the escape char is not quote, then do
	 special handling of multiple escape chars preceding a quote
	 char (ie. the reverse of what Emacs does to escape quotes).  */
      p++;
      while (1)
	{
	  if (p[0] == escape_char && escape_char != '"')
	    {
	      escape_char_run++;
	      continue;
	    }
	  else if (p[0] == '"')
	    {
	      while (escape_char_run > 1)
		{
		  *o++ = escape_char;
		  escape_char_run -= 2;
		}

	      if (escape_char_run > 0)
		{
		  /* escaped quote */
		  *o++ = *p++;
		  escape_char_run = 0;
		}
	      else if (p[1] == escape_char && escape_char == '"')
		{
		  /* quote escaped by doubling */
		  *o++ = *p;
		  p += 2;
		}
	      else
		{
		  /* The ending quote.  */
		  *o = '\0';
		  /* Leave input pointer after token.  */
		  p++;
		  break;
		}
	    }
	  else if (p[0] == '\0')
	    {
	      /* End of string, but no ending quote found.  We might want to
		 flag this as an error, but for now will consider the end as
		 the end of the token.  */
	      *o = '\0';
	      break;
	    }
	  else
	    {
	      *o++ = *p++;
	    }
	}
    }
  else
    {
      /* Next token is delimited by whitespace.  */
      char * p1 = skip_nonspace (p);
      memcpy (o, p, p1 - p);
      o += (p1 - p);
      p = p1;
    }

  *pSrc = p;

  return o - buf;
}

/* Search for EXEC file in DIR.  If EXEC does not have an extension,
   DIR is searched for EXEC with the standard extensions appended.  */
int
search_dir (char *dir, char *exec, int bufsize, char *buffer)
{
  char *exts[] = {".bat", ".cmd", ".exe", ".com"};
  int n_exts = sizeof (exts) / sizeof (char *);
  char *dummy;
  int i, rc;

  /* Search the directory for the program.  */
  for (i = 0; i < n_exts; i++) 
    {
      rc = SearchPath (dir, exec, exts[i], bufsize, buffer, &dummy);
      if (rc > 0)
	return rc;
    }

  return 0;
}

/* Return the absolute name of executable file PROG, including 
   any file extensions.  If an absolute name for PROG cannot be found,
   return NULL.  */
char *
make_absolute (char *prog)
{
  char absname[MAX_PATH];
  char dir[MAX_PATH];
  char curdir[MAX_PATH];
  char *p, *fname;
  char *path;
  int i;

  /* At least partial absolute path specified; search there.  */
  if ((isalpha (prog[0]) && prog[1] == ':') ||
      (prog[0] == '\\'))
    {
      /* Split the directory from the filename.  */
      fname = strrchr (prog, '\\');
      if (!fname)
	/* Only a drive specifier is given.  */
	fname = prog + 2;
      strncpy (dir, prog, fname - prog);
      dir[fname - prog] = '\0';

      /* Search the directory for the program.  */
      if (search_dir (dir, prog, MAX_PATH, absname) > 0)
	return strdup (absname);
      else
	return NULL;
    }

  if (GetCurrentDirectory (MAX_PATH, curdir) <= 0) 
    return NULL;

  /* Relative path; search in current dir. */
  if (strpbrk (prog, "\\")) 
    {
      if (search_dir (curdir, prog, MAX_PATH, absname) > 0)
	return strdup (absname);
      else 
	return NULL;
    }
  
  /* Just filename; search current directory then PATH.  */
  path = alloca (strlen (getenv ("PATH")) + strlen (curdir) + 2);
  strcpy (path, curdir);
  strcat (path, ";");
  strcat (path, getenv ("PATH"));

  while (*path)
    {
      /* Get next directory from path.  */
      p = path;
      while (*p && *p != ';') p++;
      strncpy (dir, path, p - path);
      dir[p - path] = '\0';

      /* Search the directory for the program.  */
      if (search_dir (dir, prog, MAX_PATH, absname) > 0)
	return strdup (absname);

      /* Move to the next directory.  */
      path = p + 1;
    } 

  return NULL;
}

/*****************************************************************/

#if 0
char ** _argv;
int     _argc;

/* Parse commandline into argv array, allowing proper quoting of args.  */
void
setup_argv (void)
{
  char * cmdline = GetCommandLine ();
  int arg_bytes = 0;

  
}
#endif

/* Information about child proc is global, to allow for automatic
   termination when interrupted.  At the moment, only one child process
   can be running at any one time.  */

PROCESS_INFORMATION child;
int interactive = TRUE;

BOOL
console_event_handler (DWORD event)
{
  switch (event)
    {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
      if (!interactive)
	{
	  /* Both command.com and cmd.exe have the annoying behaviour of
	     prompting "Terminate batch job (y/n)?" when interrupted
	     while running a batch file, even if running in
	     non-interactive (-c) mode.  Try to make up for this
	     deficiency by forcibly terminating the subprocess if
	     running non-interactively.  */
	  if (child.hProcess &&
	      WaitForSingleObject (child.hProcess, 500) != WAIT_OBJECT_0)
	    TerminateProcess (child.hProcess, 0);
	  exit (STATUS_CONTROL_C_EXIT);
	}
      break;

#if 0
    default:
      /* CLOSE, LOGOFF and SHUTDOWN events - actually we don't get these
         under Windows 95.  */
      fail ("cmdproxy: received %d event\n", event);
      if (child.hProcess)
	TerminateProcess (child.hProcess, 0);
#endif
    }
  return TRUE;
}

int
spawn (char * progname, char * cmdline)
{
  DWORD rc = 0xff;
  SECURITY_ATTRIBUTES sec_attrs;
  STARTUPINFO start;

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;
  
  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);

  if (CreateProcess (progname, cmdline, &sec_attrs, NULL, TRUE,
		     0, NULL, NULL, &start, &child))
  {
    /* wait for completion and pass on return code */
    WaitForSingleObject (child.hProcess, INFINITE);
    GetExitCodeProcess (child.hProcess, &rc);
    CloseHandle (child.hThread);
    CloseHandle (child.hProcess);
    child.hProcess = NULL;
  }

  return (int) rc;
}

/*******  Main program  ********************************************/

int
main (int argc, char ** argv)
{
  int rc;
  int need_shell;
  char * cmdline;
  char * progname;
  int envsize;
  char modname[MAX_PATH];
  char path[MAX_PATH];


  interactive = TRUE;

  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) console_event_handler, TRUE);

  /* We serve double duty: we can be called either as a proxy for the
     real shell (that is, because we are defined to be the user shell),
     or in our role as a helper application for running DOS programs.
     In the former case, we interpret the command line options as if we
     were a Unix shell, but in the latter case we simply pass our
     command line to CreateProcess.  We know which case we are dealing
     with by whether argv[0] refers to ourself or to some other program.
     (This relies on an arcane feature of CreateProcess, where we can
     specify cmdproxy as the module to run, but specify a different
     program in the command line - the MSVC startup code sets argv[0]
     from the command line.)  */

  if (!GetModuleFileName (NULL, modname, sizeof (modname)))
    fail ("GetModuleFileName failed");

  /* Although Emacs always sets argv[0] to an absolute pathname, we
     might get run in other ways as well, so convert argv[0] to an
     absolute name before comparing to the module name.  */
  if (!SearchPath (NULL, argv[0], ".exe", sizeof (path), path, &progname)
      || stricmp (modname, path) != 0)
    {
      /* We are being used as a helper to run a DOS app; just pass
	 command line to DOS app without change.  */
      /* TODO: fill in progname.  */
      return spawn (NULL, GetCommandLine ());
    }

  /* Process command line.  If running interactively (-c or /c not
     specified) then spawn a real command shell, passing it the command
     line arguments.

     If not running interactively, then attempt to execute the specified
     command directly.  If necessary, spawn a real shell to execute the
     command.

  */

  progname = NULL;
  cmdline = NULL;
  /* If no args, spawn real shell for interactive use.  */
  need_shell = TRUE;
  interactive = TRUE;
  /* Ask for a reasonable size environment for command.com.  */
  envsize = 1024;

  while (--argc > 0)
    {
      ++argv;
      /* Only support single letter switches (except for -e); allow / as
	 switch char for compatability with cmd.exe.  */
      if ( ((*argv)[0] == '-' || (*argv)[0] == '/')
	   && (*argv)[1] != '\0' && (*argv)[2] == '\0' )
	{
	  if ( ((*argv)[1] == 'c') && ((*argv)[2] == '\0')  )
	    {
	      if (--argc == 0)
		fail ("error: expecting arg for %s", *argv);
	      cmdline = *(++argv);
	      interactive = FALSE;
	    }
	  else if ( ((*argv)[1] == 'i') && ((*argv)[2] == '\0')  )
	    {
	      if (cmdline)
		warn ("warning: %s ignored because of -c", *argv);
	    }
	  else if ( ((*argv)[1] == 'e') && ((*argv)[2] == ':')  )
	    {
	      envsize = atoi (*argv + 3);
	      /* Enforce a reasonable minimum size.  */
	      if (envsize < 256)
		envsize = 256;
	    }
	  else
	    {
	      warn ("warning: unknown option %s ignored", *argv);
	    }
	}
      else
	break;
    }

  /* If -c option, determine if we must spawn a real shell, or if we can
     execute the command directly ourself.  */
  if (cmdline)
    {
      /* If no redirection or piping, and if program can be found, then
	 run program directly.  Otherwise invoke a real shell. */

      static char copout_chars[] = "|<>&";

      if (strpbrk (cmdline, copout_chars) == NULL)
	{
 	  char *args;

	  /* The program name is the first token of cmdline.  Since
	     filenames cannot legally contain embedded quotes, the value
	     of escape_char doesn't matter.  */
	  args = cmdline;
	  if (!get_next_token (path, &args))
	    fail ("error: no program name specified.\n");

	  canon_filename (path);
	  progname = make_absolute (path);

	  /* If we found the program, run it directly (if not found it
             might be an internal shell command, so don't fail).  */
	  if (progname != NULL)
	    need_shell = FALSE;
	}
    }

  if (need_shell)
    {
      char * p;

      progname = getenv ("COMSPEC");
      if (!progname)
	fail ("error: COMSPEC is not set");

      canon_filename (progname);
      progname = make_absolute (progname);

      if (progname == NULL || strchr (progname, '\\') == NULL)
	fail ("make_absolute failed");

      if (cmdline)
	{
	  /* Convert to syntax expected by cmd.exe/command.com for
	     running non-interactively.  Always quote program name in
	     case path contains spaces (fortunately it can't contain
	     quotes, since they are illegal in path names).  */
	  wsprintf (p = alloca (strlen (cmdline) + strlen (progname) + 7),
		    "\"%s\" /c %s", progname, cmdline);
	  cmdline = p;
	}
      else
	{
	  /* Provide dir arg expected by command.com when first started
	     interactively (the "command search path").  cmd.exe does
	     not require it, but accepts it silently - presumably other
	     DOS compatible shells do the same.  To avoid potential
	     problems with spaces in command dir (which cannot be quoted
	     - command.com doesn't like it), we always use the 8.3 form.  */
	  GetShortPathName (progname, path, sizeof (path));
	  p = strrchr (path, '\\');
	  /* Trailing slash is acceptable.  */
	  p++;

	  /* Set environment size - again cmd.exe ignores this silently.  */
	  wsprintf (p, " /e:%d", envsize);

	  /* Quote progname in case it contains spaces.  */
	  wsprintf (cmdline = alloca (strlen (progname) + strlen (path) + 4),
		    "\"%s\" %s", progname, path);
	}
    }

  if (!progname)
    fail ("Internal error: program name not defined\n");

  if (!cmdline)
    cmdline = progname;

  rc = spawn (progname, cmdline);

  return rc;
}
