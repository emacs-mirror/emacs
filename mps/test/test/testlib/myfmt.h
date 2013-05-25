/* $Id$
myfmt.h
   a format for scannable objects
*/

#ifndef myfmt_h
#define myfmt_h

#include "testlib.h"

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing, copying
 copysurplus      1   copy the surplus space in objects when moving 

*/

extern int formatcomments;
extern int copysurplus;

typedef struct mycell
  {
   mps_word_t    tag;
   mps_word_t    data;
   mps_word_t    size;
   struct mycell *ref[2];
  } mycell;

/* we don't have a separate type for leaf nodes;
   instead the scanning function doesn't fix null refs

   the words after ref[1] are copied by mycopy,
   (so you can use them to store data) as long as copysurplus=1
*/

extern struct mps_fmt_A_s fmtA;

mycell *allocone(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size);

#endif

