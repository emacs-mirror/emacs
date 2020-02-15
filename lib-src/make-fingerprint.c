/* Hash inputs and generate C file with the digest.

Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2016, 2018-2020 Free
Software Foundation, Inc.

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


/* The arguments given to this program are all the object files that
 go into building GNU Emacs.  There is no special search logic to find
 the files.  */

#include <config.h>

#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <sysstdio.h>

#include <fingerprint.h>
#include <getopt.h>
#include <intprops.h>
#include <min-max.h>
#include <sha256.h>

#ifndef SSIZE_MAX
# define SSIZE_MAX TYPE_MAXIMUM (ssize_t)
#endif

#ifdef WINDOWSNT
/* Defined to be sys_fopen in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.  */
#undef fopen
#include <direct.h>

#ifndef MINGW_W64
# undef fseeko
# define fseeko fseeko64
#endif
#endif /* WINDOWSNT */

/* Static (instead of being local to 'main') to pacify LeakSanitizer.  */
static char *buf;

int
main (int argc, char **argv)
{
  int c;
  bool raw = false;
  while (0 <= (c = getopt (argc, argv, "rh")))
    {
      switch (c)
        {
        case 'r':
          raw = true;
          break;
        case 'h':
          printf ("make-fingerprint [-r] FILE: replace or compute a hash\n");
          return EXIT_SUCCESS;
        default:
          return EXIT_FAILURE;
        }
    }

  struct sha256_ctx ctx;
  sha256_init_ctx (&ctx);

  char *prog = argv[0];
  char *file = argv[optind];
  if (argc - optind != 1)
    {
      fprintf (stderr, "%s: missing or extra file operand\n", prog);
      return EXIT_FAILURE;
    }

  FILE *f = fopen (file, raw ? "r" FOPEN_BINARY : "r+" FOPEN_BINARY);
  struct stat st;
  if (!f || fstat (fileno (f), &st) != 0)
    {
      perror (file);
      return EXIT_FAILURE;
    }

  if (!S_ISREG (st.st_mode))
    {
      fprintf (stderr, "%s: Error: %s is not a regular file\n",
	       prog, file);
      return EXIT_FAILURE;
    }

  ptrdiff_t maxlen = min (min (TYPE_MAXIMUM (off_t), PTRDIFF_MAX),
			  min (SIZE_MAX, SSIZE_MAX));
  if (maxlen <= st.st_size)
    {
      fprintf (stderr, "%s: %s: file too big\n", prog, file);
      return EXIT_FAILURE;
    }

  buf = malloc (st.st_size + 1);
  if (!buf)
    {
      perror ("malloc");
      return EXIT_FAILURE;
    }

  size_t chunksz = fread (buf, 1, st.st_size + 1, f);
  if (ferror (f) || chunksz != st.st_size)
    {
      fprintf (stderr, "%s: Error: could not read %s\n", prog, file);
      return EXIT_FAILURE;
    }

  sha256_process_bytes (buf, chunksz, &ctx);

  unsigned char digest[32];
  sha256_finish_ctx (&ctx, digest);

  if (raw)
    {
      for (int i = 0; i < 32; ++i)
        printf ("%02X", digest[i]);
    }
  else
    {
      bool fingered = false;

      for (char *finger = buf;
	   (finger = memmem (finger, buf + chunksz - finger,
			     (unsigned char *) fingerprint,
			     sizeof fingerprint));
	   finger++)
	{
	  if (! (fseeko (f, finger - buf, SEEK_SET) == 0
		 && fwrite (digest, 1, sizeof digest, f) == sizeof digest))
	    {
	      perror (file);
	      return EXIT_FAILURE;
	    }
	  fingered = true;
	}

      if (!fingered)
	{
	  fprintf (stderr, "%s: %s: missing fingerprint\n", prog, file);
	  return EXIT_FAILURE;
	}
    }

  if (fclose (f) != 0)
    {
      perror (file);
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

/* make-fingerprint.c ends here */
