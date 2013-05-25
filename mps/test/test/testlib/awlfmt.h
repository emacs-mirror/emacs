/* $Id$
awlfmt.h
   A format for the awl pool
*/

#ifndef awlfmt_h
#define awlfmt_h

#include "testlib.h"

extern int formatcomments;
extern int checkcomments;
extern int countcomments;
extern int alloccomments;
extern int fixcomments;
extern int deathcomments;
extern int skipcomments;
extern int splurgeassoc; /* write to associated objects (but don't change) */

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
 int countflag;
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

mycell *allocone(mps_ap_t ap, int size, int countflag);
mycell *allocdumb(mps_ap_t ap, size_t bytes, int countflag);

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

#endif

