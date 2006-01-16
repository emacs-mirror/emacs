/* machine description file for Sun 68000's OPERATING SYSTEM version 3
   (for either 68000 or 68020 systems).  */

#include "sun2.h"
#undef sun2
#ifndef sun3
#define sun3
#endif

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Sun 1, 2 and 3 (-machine=sun1, -machine=sun2, -machine=sun3;
                -opsystem=bsd4-2 or -opsystem=sunos4)

  Whether you should use sun1, sun2 or sun3 depends on the
		   VERSION OF THE OPERATING SYSTEM
  you have.  There are three machine types for different versions of
  SunOS.  All are derived from Berkeley 4.2, meaning that you should
  use -opsystem=bsd4-2.  Emacs 17 has run on all of them.  You will
  need to use sun3 on Sun 2's running SunOS release 3.

  For SunOS release 4 on a Sun 3, use -machine=sun3 and
  -opsystem=sunos4.  See the file share-lib/SUNBUG for how to solve
  problems caused by bugs in the "export" version of SunOS 4.
NOTE-END  */

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

/* In case we are using floating point, work together with crt0.c.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE -fsoft
#endif

/* This line is needed if you are linking with X windows
   and the library xlib was compiled to use the 68881.
   For maximum cleanliness, don't edit this file;
   instead, insert this line in config.h.  */
/* #define START_FILES crt0.o /usr/lib/Mcrt1.o  */

/* arch-tag: d0559a79-2285-4a78-ad68-9694264d0d8a
   (do not change this comment) */
