/* Hash inputs and generate C file with the digest.

Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2016, 2018-2019
Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* The arguments given to this program are all the object files that
 go into building GNU Emacs.  There is no special search logic to find
 the files.  */

#include <config.h>

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysstdio.h>
#include <sha256.h>
#include <getopt.h>

#ifdef WINDOWSNT
/* Defined to be sys_fopen in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.  */
#undef fopen
#include <direct.h>
#endif /* WINDOWSNT */

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
          printf ("make-fingerprint [-r] FILES...: compute a hash\n");
          return 0;
        default:
          return 1;
        }
    }

  struct sha256_ctx ctx;
  sha256_init_ctx (&ctx);

  for (int i = optind; i < argc; ++i)
    {
      FILE *f = fopen (argv[i], "r" FOPEN_BINARY);
      if (!f)
        {
          fprintf (stderr, "%s: Error: could not open %s\n",
                   argv[0], argv[i]);
          return 1;
        }

      char buf[128*1024];
      do
        {
          size_t chunksz = fread (buf, 1, sizeof (buf), f);
          if (ferror (f))
            {
              fprintf (stderr, "%s: Error: could not read %s\n",
                       argv[0], argv[i]);
              return 1;
            }
          sha256_process_bytes (buf, chunksz, &ctx);
        } while (!feof (f));
      fclose (f);
    }

  uint8_t digest[32];
  sha256_finish_ctx (&ctx, digest);

  if (raw)
    {
      for (int i = 0; i < 32; ++i)
        printf ("%02X", digest[i]);
    }
  else
    {
      printf ("#include \"fingerprint.h\"\n");
      printf ("\n");
      printf ("const uint8_t fingerprint[32] = { ");
      for (int i = 0; i < 32; ++i)
        printf ("%s0x%02X", i ? ", " : "", digest[i]);
      printf (" };\n");
    }

  return EXIT_SUCCESS;
}

/* make-fingerprint.c ends here */
