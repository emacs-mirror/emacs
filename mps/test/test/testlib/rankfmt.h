/* $Id$

   Format like exfmt but with rank-checking built in.

   This format will work without register roots, but to use it, you must
   take precautions.  You must register exfmt_root as a root, and unless
   you register the registers and stack, you must register it as
   ambiguous (or it might change before you can write the address of a
   newly allocated object into another root).  When you call getref or
   setref, you must make sure that the objects concerned are referenced
   ambiguously somewhere (this guarantees that they won't move -- I
   asked richard and drj).
*/

#ifndef rankfmt_h
#define rankfmt_h


#include "testlib.h"


extern int formatcomments;
extern int checkcomments;
extern int countcomments;
extern int alloccomments;
extern int fixcomments;
extern int deathcomments;
extern int skipcomments;
extern int splurgeassoc; /* write to associated objects (but don't change) */


/* the counters are visible so that I can check whether things
   get moved etc
*/

enum {
 SCANCALL_COUNT,
 SCANOBJ_COUNT, /* = read objects scanned */
 SCANPAD_COUNT, /* = pads scanned */
 SCANHEART_COUNT, /* = hearts scanned */
 COPY_COUNT,
 SKIP_COUNT,
 FWD_COUNT,
 ISFWD_COUNT,
 RESERVE_COUNT,
 ALLOC_COUNT,
 DYING_REFERENCE_COUNT,
 PAD_COUNT /* must come last or array sizes will be too small */
};

extern int counters[PAD_COUNT+1];
extern int prevcounters[PAD_COUNT+1];
extern int maxcounters[PAD_COUNT+1];


long int maxcopy;
int freeze;


/* the object format is visible so tests that want to
   can hack around with it
*/

#define MAXSIZE 10000

enum {MCpad=(int) 0x1, MCheart=(int) 0x2, MCdata=(int) 0x0};

enum {MCerrorid=(int) 0xE6606};


/* n.b. MCerrorid < 0x1000000 so it won't clash with id of
   any ordinary object
*/

typedef union mycell mycell;


typedef mps_word_t tag;


struct pad {tag tag;};


struct heart {tag tag; mps_addr_t obj; size_t size;};


struct data
{
 tag tag;
 mycell *assoc;
 size_t size;
 long int id;
 long int copycount;
 long int numrefs;
 int checkedflag;
 mps_rank_t rank;
 struct refitem {mycell *addr; long int id;} ref[MAXSIZE];
};


union mycell
{
 tag tag;
 struct pad       pad;
 struct heart     heart;
 struct data      data;
};


extern struct mps_fmt_A_s fmtA;


mycell *allocone(mps_ap_t ap, int size, mps_rank_t rank);
mycell *allocdumb(mps_ap_t ap, size_t bytes, mps_rank_t rank);

mps_res_t allocrone(mycell **rvar, mps_ap_t ap, int size, mps_rank_t rank);
mps_res_t allocrdumb(mycell **rvar, mps_ap_t ap, size_t bytes, mps_rank_t rank);

mps_addr_t getdata(mycell *obj);
mps_addr_t getassociated(mps_addr_t addr);

void setref(mycell *obj, int n, mycell *to);
mycell *getref(mycell *obj, int n);

long int getid(mycell *obj);
long int getcopycount(mycell *obj);
long int getsize(mycell *obj);


void checkfrom(mycell *obj);


#define RC resetcounters()
#define UC updatecounters()
#define DC displaycounters()
#define DMC displaymaxcounters()

void resetcounters(void);
void updatecounters(void);
void displaycounters(void);
void displaymaxcounters(void);


extern mycell *exfmt_root;


#endif
