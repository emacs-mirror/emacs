/* Proxy shell designed for use with Emacs on Windows 95 and NT.
   Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
     2008, 2009, 2010  Free Software Foundation, Inc.

   Accepts subset of Unix sh(1) command-line options, for compatibility
   with elisp code written for Unix.  When possible, executes external
   programs directly (a common use of /bin/sh by Emacs), otherwise
   invokes the user-specified command processor to handle built-in shell
   commands, batch files and interactive mode.

   The main function is simply to process the "-c string" option in the
   way /bin/sh does, since the standard Windows command shells use the
   convention that everything after "/c" (the Windows equivalent of
   "-c") is the input string.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <windows.h>

#include <stdarg.h>  /* va_args */
#include <malloc.h>  /* alloca */
#include <stdlib.h>  /* getenv */
#include <string.h>  /* strlen */

/* We don't want to include stdio.h because we are already duplicating
   lots of it here */
extern int _snprintf (char *buffer, size_t count, const char *format, ...);

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

  exit (-1);
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
	      p++;
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
      *o = '\0';
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
	  /* Both command.com and cmd.exe have the annoying behavior of
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

/* Change from normal usage; return value indicates whether spawn
   succeeded or failed - program return code is returned separately.  */
int
spawn (char * progname, char * cmdline, char * dir, int * retcode)
{
  BOOL success = FALSE;
  SECURITY_ATTRIBUTES sec_attrs;
  STARTUPINFO start;
  /* In theory, passing NULL for the environment block to CreateProcess
     is the same as passing the value of GetEnvironmentStrings, but
     doing this explicitly seems to cure problems running DOS programs
     in some cases.  */
  char * envblock = GetEnvironmentStrings ();

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);

  if (CreateProcess (progname, cmdline, &sec_attrs, NULL, TRUE,
		     0, envblock, dir, &start, &child))
  {
    success = TRUE;
    /* wait for completion and pass on return code */
    WaitForSingleObject (child.hProcess, INFINITE);
    if (retcode)
      GetExitCodeProcess (child.hProcess, (DWORD *)retcode);
    CloseHandle (child.hThread);
    CloseHandle (child.hProcess);
    child.hProcess = NULL;
  }

  FreeEnvironmentStrings (envblock);

  return success;
}

/* Return size of current environment block.  */
int
get_env_size ()
{
  char * start = GetEnvironmentStrings ();
  char * tmp = start;

  while (tmp[0] || tmp[1])
    ++tmp;
  FreeEnvironmentStrings (start);
  return  tmp + 2 - start;
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
  char **pass_through_args;
  int num_pass_through_args;
  char modname[MAX_PATH];
  char path[MAX_PATH];
  char dir[MAX_PATH];


  interactive = TRUE;

  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) console_event_handler, TRUE);

  if (!GetCurrentDirectory (sizeof (dir), dir))
    fail ("error: GetCurrentDirectory failed\n");

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
    fail ("error: GetModuleFileName failed\n");

  /* Change directory to location of .exe so startup directory can be
     deleted.  */
  progname = strrchr (modname, '\\');
  *progname = '\0';
  SetCurrentDirectory (modname);
  *progname = '\\';

  /* Due to problems with interaction between API functions that use "OEM"
     codepage vs API functions that use the "ANSI" codepage, we need to
     make things consistent by choosing one and sticking with it.  */
  SetConsoleCP (GetACP());
  SetConsoleOutputCP (GetACP());

  /* Although Emacs always sets argv[0] to an absolute pathname, we
     might get run in other ways as well, so convert argv[0] to an
     absolute name before comparing to the module name.  Don't get
     caught out by mixed short and long names.  */
  GetShortPathName (modname, modname, sizeof (modname));
  path[0] = '\0';
  if (!SearchPath (NULL, argv[0], ".exe", sizeof (path), path, &progname)
      || !GetShortPathName (path, path, sizeof (path))
      || stricmp (modname, path) != 0)
    {
      /* We are being used as a helper to run a DOS app; just pass
	 command line to DOS app without change.  */
      /* TODO: fill in progname.  */
      if (spawn (NULL, GetCommandLine (), dir, &rc))
	return rc;
      fail ("Could not run %s\n", GetCommandLine ());
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
  /* Ask command.com to create an environment block with a reasonable
     amount of free space.  */
  envsize = get_env_size () + 300;
  pass_through_args = (char **) alloca (argc * sizeof(char *));
  num_pass_through_args = 0;

  while (--argc > 0)
    {
      ++argv;
      /* Act on switches we recognize (mostly single letter switches,
	 except for -e); all unrecognized switches and extra args are
	 passed on to real shell if used (only really of benefit for
	 interactive use, but allow for batch use as well).  Accept / as
	 switch char for compatibility with cmd.exe.  */
      if (((*argv)[0] == '-' || (*argv)[0] == '/') && (*argv)[1] != '\0')
	{
	  if (((*argv)[1] == 'c' || (*argv)[1] == 'C') && ((*argv)[2] == '\0'))
	    {
	      if (--argc == 0)
		fail ("error: expecting arg for %s\n", *argv);
	      cmdline = *(++argv);
	      interactive = FALSE;
	    }
	  else if (((*argv)[1] == 'i' || (*argv)[1] == 'I') && ((*argv)[2] == '\0'))
	    {
	      if (cmdline)
		warn ("warning: %s ignored because of -c\n", *argv);
	    }
	  else if (((*argv)[1] == 'e' || (*argv)[1] == 'E') && ((*argv)[2] == ':'))
	    {
	      int requested_envsize = atoi (*argv + 3);
	      /* Enforce a reasonable minimum size, as above.  */
	      if (requested_envsize > envsize)
		envsize = requested_envsize;
	      /* For sanity, enforce a reasonable maximum.  */
	      if (envsize > 32768)
		envsize = 32768;
	    }
	  else
	    {
	      /* warn ("warning: unknown option %s ignored", *argv); */
	      pass_through_args[num_pass_through_args++] = *argv;
	    }
	}
      else
	break;
    }

#if 0
  /* I think this is probably not useful - cmd.exe ignores extra
     (non-switch) args in interactive mode, and they cannot be passed on
     when -c was given.  */

  /* Collect any remaining args after (initial) switches.  */
  while (argc-- > 0)
    {
      pass_through_args[num_pass_through_args++] = *argv++;
    }
#else
  /* Probably a mistake for there to be extra args; not fatal.  */
  if (argc > 0)
    warn ("warning: extra args ignored after '%s'\n", argv[-1]);
#endif

  pass_through_args[num_pass_through_args] = NULL;

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

 pass_to_shell:
  if (need_shell)
    {
      char * p;
      int    extra_arg_space = 0;
      int    maxlen, remlen;
      int    run_command_dot_com;

      progname = getenv ("COMSPEC");
      if (!progname)
	fail ("error: COMSPEC is not set\n");

      canon_filename (progname);
      progname = make_absolute (progname);

      if (progname == NULL || strchr (progname, '\\') == NULL)
	fail ("error: the program %s could not be found.\n", getenv ("COMSPEC"));

      /* Need to set environment size when running command.com.  */
      run_command_dot_com =
	(stricmp (strrchr (progname, '\\'), "command.com") == 0);

      /* Work out how much extra space is required for
         pass_through_args.  */
      for (argv = pass_through_args; *argv != NULL; ++argv)
	/* We don't expect to have to quote switches.  */
	extra_arg_space += strlen (*argv) + 2;

      if (cmdline)
	{
	  char * buf;

	  /* Convert to syntax expected by cmd.exe/command.com for
	     running non-interactively.  Always quote program name in
	     case path contains spaces (fortunately it can't contain
	     quotes, since they are illegal in path names).  */

	  remlen = maxlen =
	    strlen (progname) + extra_arg_space + strlen (cmdline) + 16;
	  buf = p = alloca (maxlen + 1);

	  /* Quote progname in case it contains spaces.  */
	  p += _snprintf (p, remlen, "\"%s\"", progname);
	  remlen = maxlen - (p - buf);

	  /* Include pass_through_args verbatim; these are just switches
             so should not need quoting.  */
	  for (argv = pass_through_args; *argv != NULL; ++argv)
	    {
	      p += _snprintf (p, remlen, " %s", *argv);
	      remlen = maxlen - (p - buf);
	    }

	  if (run_command_dot_com)
	    _snprintf (p, remlen, " /e:%d /c %s", envsize, cmdline);
	  else
	    _snprintf (p, remlen, " /c %s", cmdline);
	  remlen = maxlen - (p - buf);
	  cmdline = buf;
	}
      else
	{
	  if (run_command_dot_com)
	    {
	      /* Provide dir arg expected by command.com when first
		 started interactively (the "command search path").  To
		 avoid potential problems with spaces in command dir
		 (which cannot be quoted - command.com doesn't like it),
		 we always use the 8.3 form.  */
	      GetShortPathName (progname, path, sizeof (path));
	      p = strrchr (path, '\\');
	      /* Trailing slash is acceptable, so always leave it.  */
	      *(++p) = '\0';
	    }
	  else
	    path[0] = '\0';

	  remlen = maxlen =
	    strlen (progname) + extra_arg_space + strlen (path) + 13;
	  cmdline = p = alloca (maxlen + 1);

	  /* Quote progname in case it contains spaces.  */
	  p += _snprintf (p, remlen, "\"%s\" %s", progname, path);
	  remlen = maxlen - (p - cmdline);

	  /* Include pass_through_args verbatim; these are just switches
             so should not need quoting.  */
	  for (argv = pass_through_args; *argv != NULL; ++argv)
	    {
	      p += _snprintf (p, remlen, " %s", *argv);
	      remlen = maxlen - (p - cmdline);
	    }

	  if (run_command_dot_com)
	    {
	      _snprintf (p, remlen, " /e:%d", envsize);
	      remlen = maxlen - (p - cmdline);
	    }
	}
    }

  if (!progname)
    fail ("Internal error: program name not defined\n");

  if (!cmdline)
    cmdline = progname;

  if (spawn (progname, cmdline, dir, &rc))
    return rc;

  if (!need_shell)
    {
      need_shell = TRUE;
      goto pass_to_shell;
    }

  fail ("Could not run %s\n", progname);

  return 0;
}

/* arch-tag: 88678d93-07ac-4e2f-ad63-d4a740ca69ac
   (do not change this comment) */
