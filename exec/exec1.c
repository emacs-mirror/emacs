/* Program execution for Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include <stdlib.h>
#include <stdio.h>
#include <sys/wait.h>

#include "exec.h"

/* exec1 is a program which takes another program and its arguments,
   forks, and executes that program, all while tracing it and its
   children to use the program execution mechanism defined in exec.c.

   This is necessary to bypass security restrictions which prohibit
   Emacs from loading executables from certain directories, by, in
   effect, replacing the executable loader in the Linux kernel.  */



int
main (int argc, char **argv)
{
  pid_t pid, pid1;
  extern char **environ;
  int wstatus;

  /* Provide the file name of the loader.  */
  exec_init (argv[1]);

  pid1 = getpid ();
  pid = fork ();

  if (!pid)
    {
      /* Set the process group used to the parent.  */
      if (setpgid (0, pid1))
	perror ("setpgid");

      tracing_execve (argv[2], argv + 2, environ);

      /* An error occurred.  Exit with failure.  */
      exit (127);
    }
  else
    {
      if (after_fork (pid))
	exit (127);

      /* Start waiting for the process to exit.  */

      while (true)
	{
	  pid1 = exec_waitpid (-1, &wstatus, 0);

	  /* If the child process exits normally, exit with its status
	     code.  If not, raise the signal that caused it to
	     exit.  */

	  if (pid == pid1)
	    {
	      if (WIFEXITED (wstatus))
		exit (WEXITSTATUS (wstatus));
	      else /* if WIFSIGNALED (wstatus) */
		{
		  raise (WTERMSIG (wstatus));

		  /* Just in case the signal raised doesn't cause an
		     exit.  */
		  exit (127);
		}
	    }

	  /* Otherwise, continue looping.  */
	}
    }
}
