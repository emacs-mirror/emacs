/* Android initialization for GNU Emacs.

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
#include <stdio.h>
#include <alloca.h>
#include <string.h>
#include <unistd.h>

/* android-emacs is a wrapper around /system/bin/app_process(64).
   It invokes app_process(64) with the right class path and then
   starts org.gnu.emacs.EmacsNoninteractive.

   The main function in that class tries to load an activity thread
   and obtain a context and asset manager before calling
   android_emacs_init, which is required for Emacs to find required
   preloaded Lisp.  */

int
main (int argc, char **argv)
{
  char **args;
  int i;
  char *bootclasspath, *emacs_class_path;

  /* Allocate enough to hold the arguments to app_process.  */
  args = alloca ((10 + argc) * sizeof *args);

  /* Clear args.  */
  memset (args, 0, (10 + argc) * sizeof *args);

  /* First, figure out what program to start.  */
#if defined __x86_64__ || defined __aarch64__ || defined __mips64
  args[0] = (char *) "/system/bin/app_process64";
#else /* i386 || regular mips || arm */
  args[0] = (char *) "/system/bin/app_process";
#endif /* __x86_64__ || __aarch64__ || __mips64 */

  /* And the Emacs class path.  */
  emacs_class_path = getenv ("EMACS_CLASS_PATH");

  if (!emacs_class_path)
    {
      fprintf (stderr, "EMACS_CLASS_PATH not set."
	       "  Please make sure Emacs is being started"
	       " from within a running copy of Emacs.\n");
      return 1;
    }

  if (asprintf (&bootclasspath, "-Djava.class.path=%s",
		emacs_class_path) < 0)
    {
      perror ("asprintf");
      return 1;
    }

  args[1] = bootclasspath;
  args[2] = (char *) "/system/bin";

#if HAVE_DECL_ANDROID_GET_DEVICE_API_LEVEL
  /* I don't know exactly when --nice-name was introduced; this is
     just a guess.  */
  if (android_get_device_api_level () >= 26)
    {
      args[3] = (char *) "--nice-name=emacs";
      args[4] = (char *) "org.gnu.emacs.EmacsNoninteractive";

      /* Arguments from here on are passed to main in
	 EmacsNoninteractive.java.  */
      args[5] = argv[0];

      /* Now copy the rest of the arguments over.  */
      for (i = 1; i < argc; ++i)
	args[5 + i] = argv[i];
    }
  else
    {
#endif /* HAVE_DECL_ANDROID_GET_DEVICE_API_LEVEL */
      args[3] = (char *) "org.gnu.emacs.EmacsNoninteractive";

      /* Arguments from here on are passed to main in
	 EmacsNoninteractive.java.  */
      args[4] = argv[0];

      /* Now copy the rest of the arguments over.  */
      for (i = 1; i < argc; ++i)
	args[4 + i] = argv[i];
#if HAVE_DECL_ANDROID_GET_DEVICE_API_LEVEL
    }
#endif /* HAVE_DECL_ANDROID_GET_DEVICE_API_LEVEL */

  /* Finally, try to start the app_process.  */
  execvp (args[0], args);

  /* If exit fails, return an error indication.  */
  perror ("exec");
  return 1;
}
