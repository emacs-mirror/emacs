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

#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <sys/wait.h>

#include "exec.h"



static void
print_usage (void)
{
  fprintf (stderr, "test loader-name program [args...]\n"
	   "Run the given program using the specified loader.\n");
}



extern char **environ;

/* This program uses libexec to wrap the execution of a child
   process.  */

int
main (int argc, char **argv)
{
  pid_t pid, child;
  int sig;
  sigset_t sigset;

  /* Check that there are a sufficient number of arguments.  */

  if (argc < 3)
    {
      print_usage ();
      return 1;
    }

  exec_init (argv[1]);

  /* Block SIGCHLD to avoid reentrant modification of the child
     process list.  */

  sigemptyset (&sigset);
  sigaddset (&sigset, SIGCHLD);
  sigprocmask (SIG_BLOCK, &sigset, NULL);

  if (!(pid = fork ()))
    {
      tracing_execve (argv[2], argv + 2, environ);
      fprintf (stderr, "tracing_execve: %s\n",
	       strerror (errno));
      exit (1);
    }
  else if (after_fork (pid))
    {
      fprintf (stderr, "after_fork: %s\n",
	       strerror (errno));
      exit (1);
    }

  /* Now start waiting for child processes to exit.  */

  while (true)
    {
      child = exec_waitpid (-1, &sig, 0);

      /* If pid is -1, a system call has been handled.  */

      if (child == -1)
	continue;

      /* If the main process exits, then exit as well.  */

      if (child == pid && !WIFSTOPPED (sig))
	return (WIFEXITED (sig)
		? WEXITSTATUS (sig)
		: WTERMSIG (sig));
    }
}
