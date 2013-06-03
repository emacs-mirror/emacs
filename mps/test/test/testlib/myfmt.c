/* $Id$
myfmt.c
   a format for scannable objects
*/

#include "myfmt.h"
#include <string.h>
#include <stdio.h>

enum {MCpadsingle, MCpadmany, MCheart, MCdata};

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing, copying
 copysurplus      1   copy the surplus space in objects when moving 

*/

int formatcomments=1;
int copysurplus=1;

/* we don't have a separate type for leaf nodes;
   instead the scanning function doesn't fix null refs

   the words after ref[1] are copied by mycopy,
   (so you can use them to store data) as long as copysurplus=1
*/

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t myskip(mps_addr_t object);
static void myfwd(mps_addr_t object, mps_addr_t to);
static mps_addr_t myisfwd(mps_addr_t object);
static void mycopy(mps_addr_t object, mps_addr_t to);
static void mypad(mps_addr_t base, size_t size);

struct mps_fmt_A_s fmtA =
{
 MPS_PF_ALIGN,
 &myscan,
 &myskip,
 &mycopy,
 &myfwd,
 &myisfwd,
 &mypad
};

mycell *allocone(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size)
{
 mps_addr_t p;
 mycell *q;
 size_t align;

 align = MPS_PF_ALIGN; /* makes it long enough for ~ to work */

 if (size < sizeof(mycell))
 {
  error("Tried to allocate too small an object.");
 }

/* twiddle the value of size to make it aligned */
 size = (size+align-1) & ~(align-1);

 do
 {
  die(mps_reserve(&p, ap, size), "Reserve: ");
  q=p;
  q->tag = MCdata;
  q->data = data;
  q->size = size;
  q->ref[0] = ref0;
  q->ref[1] = ref1;
 }
 while (!mps_commit(ap, p, size));
 return q;
}

mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 MPS_SCAN_BEGIN(ss)
 {
  while (base < limit)
  {
   mycell *obj = base;
   mps_res_t res;

   if (formatcomments) printf("Scan %p.\n", (void *)base);
   switch (obj->tag)
   {
    case MCpadsingle:
     base = (mps_addr_t) ((mps_word_t) obj + MPS_PF_ALIGN);
     break;
    case MCpadmany:
     base = (mps_addr_t) (obj->data);
     break;
    case MCdata:
     /* actual scanning is done in here */

     if (obj->ref[0] != NULL)
     {
      if (formatcomments) printf("Fix: %x.\n", (int) &(obj->ref[0]));
      res = MPS_FIX(ss, (mps_addr_t *) &(obj->ref[0])); /* pun! */
      if (res != MPS_RES_OK)
      {
       return res;
      }
     }
     if (obj->ref[1] != NULL)
     {
      if (formatcomments) printf("Fix: %x.\n", (int) &(obj->ref[1]));
      res = MPS_FIX(ss, (mps_addr_t *) &(obj->ref[1])); /* pun! */
      if (res != MPS_RES_OK)
      {
       return res;
      }
     }
     /* \/ fall through \/ */

    case MCheart:
     base = (mps_addr_t) ((char *) obj + (obj->size));
   }
  }
  asserts(base == limit, "base <> limit in scan!");
 }
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

mps_addr_t myskip(mps_addr_t object)
{
 mycell *obj = object;
 
 switch(obj->tag)
 {
  case MCpadsingle:
   return (mps_addr_t) ((mps_word_t) obj+MPS_PF_ALIGN);
  case MCpadmany:
   return (mps_addr_t) (obj->data);
  case MCheart: case MCdata:
   return (mps_addr_t) ((char *) obj + (obj->size));
  default:
   asserts(0, "skip: bizarre obj tag at %p.", obj);
   return 0; /* just to satisfy the compiler! */
 }
}

void mycopy(mps_addr_t object, mps_addr_t to)
{
 mycell *boj = object;
/* mycell *toj = to;
*/

 if (formatcomments) printf("copy! %x -> %x\n", (int) object, (int) to);

/* this line is bad, because the objects might overlap,
   and then C doesn't guarantee to do the right thing!

   *toj = *boj;
*/

 asserts(boj->tag == MCdata, "Bad object tag in copy");

 if (copysurplus)
 {
  memmove(to, object, boj->size);
 }
 else
 {
  memmove(to, object, sizeof(mycell));
 }

/* it's guaranteed that we won't have to copy a pad, so we
   don't have to worry about fiddling the pointer
*/

}

void mypad(mps_addr_t base, size_t size)
{
 mycell *obj = base;

 asserts(size >= MPS_PF_ALIGN, "size too small for pad");
 if (size == MPS_PF_ALIGN)
 {
  obj->tag = MCpadsingle;
 }
 else
 {
  obj->tag = MCpadmany;
  obj->data = ((mps_word_t) base) + size;
 }
}

mps_addr_t myisfwd(mps_addr_t object)
{
 mycell *obj = object;
 
 if (obj->tag != MCheart)
 {
  return NULL;
 }
 else
 {
  return (mps_addr_t) obj->data;
 }
}

void myfwd(mps_addr_t object, mps_addr_t to)
{
 mycell *obj = object;

 obj->tag = MCheart;
 obj->data = (mps_word_t) to;
}

