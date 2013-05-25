/* $Id$
newfmt.h
   A nice format with graph checking and statistics
*/

#ifndef newfmt_h
#define newfmt_h

#include "testlib.h"

extern int formatcomments;
extern int checkcomments;
extern int countcomments;
extern int alloccomments;
extern int fixcomments;

/* the object format is visible so tests that want to
   can hack around with it
*/

#define MAXSIZE 10000

enum {MCpadsingle=(int) 0xBAD51497, MCpadmany=(int) 0xBAD3A41,
      MCheart=(int) 0x8EA62, MCdata=(int) 0xDA2A};

enum {MCerrorid=(int) 0xE660E};

/* n.b. MCerrorid < 0x1000000 so it won't clash with id of
   any ordinary object
*/

typedef union mycell mycell;

typedef int tag;

struct padsingle {tag tag;};

struct padmulti {tag tag; mps_addr_t next;};

struct heart {tag tag; size_t size; mps_addr_t obj;};

struct data
{
 tag tag;
 size_t size;
 long int id;
 long int copycount;
 long int numrefs;
 int checkedflag;
 struct refitem {mycell *addr; long int id;} ref[MAXSIZE];
};

union mycell
{
 tag tag;
 struct padsingle padsingle;
 struct padmulti  padmulti;
 struct heart     heart;
 struct data      data;
};

extern struct mps_fmt_A_s fmtA;

mycell *allocone(mps_ap_t ap, int size);
mycell *allocdumb(mps_ap_t ap, size_t bytes);

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

