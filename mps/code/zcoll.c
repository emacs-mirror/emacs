/* zcoll.c: Collection test
 *
 * $Id$
 * Copyright (c) 2008 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * OBJECTIVE
 *
 * Test MPS collections.  In particular, reporting of how collections 
 * progress.
 *
 * Please add tests for other collection behaviour into this file.  
 * (It's easier to maintain a few big tests than myriad small tests).
 * Expand the script language as necessary!  RHSK 2008-12-22.
 *
 *
 * DESIGN OVERVIEW
 *
 * Each script runs in a newly created arena.
 *
 * [preliminary, incomplete, code still being written]
 * The commands are:
 *   Arena -- governs initial arena size, required, must be first
 *   Make -- makes some objects, stores a proportion (chosen at 
 *           random) in the specified myroot array slots, and 
 *           drops the rest (which therefore become garbage)
 *   Katalog -- (will be renamed Catalog) makes a Catalog, which 
 *           is a 40 MB 4-level tree of 10^5 objects; see .catalog;
 *           see also .catalog.broken.
 *   Collect -- request a synchronous full garbage collection
 *
 *
 * CODE OVERVIEW
 *
 * main() has the list of testscripts.  
 *
 * testscriptA() sets up a new arena and trampolines to testscriptB().
 *
 * testscriptB() creates pools and objects for this test script.
 *
 * testscriptC() actually runs the script.
 *
 *
 * DEPENDENCIES
 *
 * This test uses the dylan object format, but the reliance on this
 * particular format is not great and could be removed.
 *
 *
 * BUGS, FUTURE IMPROVEMENTS, ETC
 *
 * HISTORY
 *
 * This code was created by first copying <code/zmess.c>.
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include <stdlib.h>
#include <time.h>  /* clock */


#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: sscanf.  See job001934. */
#pragma warning( disable : 4996 )
#endif


/* testChain -- generation parameters for the test */
#define genCOUNT 2
static mps_gen_param_s testChain[genCOUNT] = {
  { 100, 0.85 }, { 170, 0.45 } };


/* myroot -- array of exact references that are the root
 *
 * (note: static, so pointers are auto-initialised to NULL)
 */
#define myrootCOUNT 30000
static void *myroot[myrootCOUNT];


static unsigned long cols(size_t bytes)
{
  double M;  /* Mebibytes */
  unsigned long cM;  /* hundredths of a Mebibyte */

  M = (double)bytes / (1UL<<20);
  cM = (unsigned long)(M * 100 + 0.5);  /* round to nearest */
  return cM;
}

/* showStatsAscii -- present collection stats, 'graphically'
 *
 */
static void showStatsAscii(size_t notcon, size_t con, size_t live, size_t alimit)
{
  int n = cols(notcon);
  int c = cols(notcon + con);
  int l = cols(notcon + live);  /* a fraction of con */
  int a = cols(alimit);
  int count;
  int i;
  
  /* if we can show alimit within 200 cols, do so */
  count = (a < 200) ? a + 1 : c;
  
  for(i = 0; i < count; i++) {
    printf( (i == a)  ? "A"
            : (i < n) ? "n"
            : (i < l) ? "L"
            : (i < c) ? "_"
            : " "
          );
  }
  printf("\n");
}


/* print_M -- print count of bytes as Mebibytes with decimal fraction 
 *
 * Input:   208896
 * Output:  0m199
 */
static void print_M(size_t bytes)
{
  size_t M;  /* Mebibytes */
  double Mfrac;  /* fraction of a Mebibyte */

  M = bytes / (1UL<<20);
  Mfrac = (double)(bytes % (1UL<<20));
  Mfrac = (Mfrac / (1UL<<20));

  printf("%1lum%03.f", M, Mfrac * 1000);
}


/* showStatsText -- present collection stats
 *
 * prints:
 *   Coll End  0m137[->0m019 14%-live] (0m211-not )
 */
static void showStatsText(size_t notcon, size_t con, size_t live)
{
  double liveFrac = (double)live / (double)con;

  print_M(con);
  printf("[->");
  print_M(live);
  printf("% 3.f%%-live]", liveFrac * 100);
  printf(" (");
  print_M(notcon);
  printf("-not ");
  printf(")\n");
}

/* get -- get messages
 *
 */
static void get(mps_arena_t arena)
{
  mps_message_type_t type;

  while (mps_message_queue_type(&type, arena)) {
    mps_message_t message;
    static mps_clock_t mclockBegin = 0;
    static mps_clock_t mclockEnd = 0;
    mps_word_t *obj;
    mps_word_t objind;
    mps_addr_t objaddr;

    cdie(mps_message_get(&message, arena, type),
         "get");
    
    switch(type) {
      case mps_message_type_gc_start(): {
        mclockBegin = mps_message_clock(arena, message);
        printf("    %5lu: (%5lu)",
               mclockBegin, mclockBegin - mclockEnd);
        printf("    Coll Begin                                     (%s)\n",
               mps_message_gc_start_why(arena, message));
        break;
      }
      case mps_message_type_gc(): {
        size_t con = mps_message_gc_condemned_size(arena, message);
        size_t notcon = mps_message_gc_not_condemned_size(arena, message);
        /* size_t other = 0;  -- cannot determine; new method reqd */
        size_t live = mps_message_gc_live_size(arena, message);
        size_t alimit = mps_arena_reserved(arena);

        mclockEnd = mps_message_clock(arena, message);
        
        printf("    %5lu: (%5lu)",
               mclockEnd, mclockEnd - mclockBegin);
        printf("    Coll End  ");
        showStatsText(notcon, con, live);
        if(rnd()==0) showStatsAscii(notcon, con, live, alimit);
        break;
      }
      case mps_message_type_finalization(): {
        mps_message_finalization_ref(&objaddr, arena, message);
        obj = objaddr;
        objind = DYLAN_INT_INT(DYLAN_VECTOR_SLOT(obj, 0));
        printf("    Finalization for object %lu at %p\n", objind, objaddr);
        break;
      }
      default: {
        cdie(0, "message type");
        break;
      }
    }
    
    mps_message_discard(arena, message);
  }
}


/* .catalog: The Catalog client:
 * 
 * This is an MPS client for testing the MPS.  It simulates 
 * converting a multi-page "Catalog" document from a page-description 
 * into a bitmap.
 *
 * The intention is that this task will cause memory usage that is 
 * fairly realistic (much more so than randomly allocated objects 
 * with random interconnections.  The patterns in common with real 
 * clients are:
 *   - the program input and its task are 'fractal', with a 
 *     self-similar hierarchy;
 *   - object allocation is prompted by each successive element of 
 *     the input/task;
 *   - objects are often used to store a transformed version of the 
 *     program input;
 *   - there may be several stages of transformation;
 *   - at each stage, the old object (holding the untransformed data) 
 *     may become dead;
 *   - sometimes a tree of objects becomes dead once an object at 
 *     some level of the hierarchy has been fully processed;
 *   - there is more than one hierarchy, and objects in different 
 *     hierarchies interact.
 *
 * The entity-relationship diagram is:
 *        Catalog -< Page -< Article -< Polygon
 *                                        v
 *                                        |
 *        Palette --------------------< Colour
 *
 * The first hierarchy is a Catalog, containing Pages, each 
 * containing Articles (bits of artwork etc), each composed of 
 * Polygons.  Each polygon has a single colour.  
 *
 * The second hierarchy is a top-level Palette, containing Colours.  
 * Colours (in this client) are expensive, large objects (perhaps 
 * because of complex colour modelling or colour blending).
 *
 * The things that matter for their effect on MPS behaviour are:
 *   - when objects are allocated, and how big they are;
 *   - how the reference graph mutates over time;
 *   - how the mutator accesses objects (barrier hits).
 */

enum {
  CatalogRootIndex = 0,
  CatalogSig = 0x0000CA2A,  /* CATAlog */
  CatalogFix = 1,
  CatalogVar = 10,
  PageSig =    0x0000BA9E,  /* PAGE */
  PageFix = 1,
  PageVar = 100,
  ArtSig =     0x0000A621,  /* ARTIcle */
  ArtFix = 1,
  ArtVar = 100,
  PolySig =    0x0000B071,  /* POLYgon */
  PolyFix = 1,
  PolyVar = 100
};

static void CatalogCheck(void)
{
  mps_word_t w;
  void *Catalog, *Page, *Art, *Poly;
  unsigned long Catalogs = 0, Pages = 0, Arts = 0, Polys = 0;
  int i, j, k;

  /* retrieve Catalog from root */
  Catalog = myroot[CatalogRootIndex];
  if(!Catalog)
    return;
  Insist(DYLAN_VECTOR_SLOT(Catalog, 0) == DYLAN_INT(CatalogSig));
  Catalogs += 1;

  for(i = 0; i < CatalogVar; i += 1) {
    /* retrieve Page from Catalog */
    w = DYLAN_VECTOR_SLOT(Catalog, CatalogFix + i);
    /* printf("Page = 0x%8x\n", (unsigned int) w); */
    if(w == DYLAN_INT(0))
      break;
    Page = (void *)w;
    Insist(DYLAN_VECTOR_SLOT(Page, 0) == DYLAN_INT(PageSig));
    Pages += 1;
    
    for(j = 0; j < PageVar; j += 1) {
      /* retrieve Art from Page */
      w = DYLAN_VECTOR_SLOT(Page, PageFix + j);
      if(w == DYLAN_INT(0))
        break;
      Art = (void *)w;
      Insist(DYLAN_VECTOR_SLOT(Art, 0) = DYLAN_INT(ArtSig));
      Arts += 1;

      for(k = 0; k < ArtVar; k += 1) {
        /* retrieve Poly from Art */
        w = DYLAN_VECTOR_SLOT(Art, ArtFix + k);
        if(w == DYLAN_INT(0))
          break;
        Poly = (void *)w;
        Insist(DYLAN_VECTOR_SLOT(Poly, 0) = DYLAN_INT(PolySig));
        Polys += 1;
      }
    }
  }
  printf("Catalog ok with: Catalogs: %lu, Pages: %lu, Arts: %lu, Polys: %lu.\n",
         Catalogs, Pages, Arts, Polys);
}


/* CatalogDo -- make a Catalog and its tree of objects
 *
 * .catalog.broken: this code, when compiled with 
 * moderate optimization, may have ambiguous interior pointers but 
 * lack corresponding ambiguous base pointers to MPS objects.  This 
 * means the interior pointers are unmanaged references, and the 
 * code goes wrong.  The hack in poolamc.c#4 cures this, but not very 
 * nicely.  For further discussion, see:
 *    <http://info.ravenbrook.com/mail/2009/02/05/18-05-52/0.txt>
 */
static void CatalogDo(mps_arena_t arena, mps_ap_t ap)
{
  mps_word_t v;
  void *Catalog, *Page, *Art, *Poly;
  int i, j, k;

  die(make_dylan_vector(&v, ap, CatalogFix + CatalogVar), "Catalog");
  DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(CatalogSig);
  Catalog = (void *)v;
  
  /* store Catalog in root */
  myroot[CatalogRootIndex] = Catalog;
  get(arena);

  fflush(stdout);
  CatalogCheck();

  for(i = 0; i < CatalogVar; i += 1) {
    die(make_dylan_vector(&v, ap, PageFix + PageVar), "Page");
    DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(PageSig);
    Page = (void *)v;

    /* store Page in Catalog */
    DYLAN_VECTOR_SLOT(Catalog, CatalogFix + i) = (mps_word_t)Page;
    get(arena);
    
    printf("Page %d: make articles\n", i);
    fflush(stdout);
    
    for(j = 0; j < PageVar; j += 1) {
      die(make_dylan_vector(&v, ap, ArtFix + ArtVar), "Art");
      DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(ArtSig);
      Art = (void *)v;

      /* store Art in Page */
      DYLAN_VECTOR_SLOT(Page, PageFix + j) = (mps_word_t)Art;
      get(arena);

      for(k = 0; k < ArtVar; k += 1) {
        die(make_dylan_vector(&v, ap, PolyFix + PolyVar), "Poly");
        DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(PolySig);
        Poly = (void *)v;

        /* store Poly in Art */
        DYLAN_VECTOR_SLOT(Art, ArtFix + k) = (mps_word_t)Poly;
        /* get(arena); */
      }
    }
  }
  fflush(stdout);
  CatalogCheck();
}


/* checksi -- check count of sscanf items is correct
 */

static void checksi(int si, int si_shouldBe, const char *script, const char *scriptAll)
{
  if(si != si_shouldBe) {
    printf("bad script command %s (full script %s).\n", script, scriptAll);
    cdie(FALSE, "unknown script command");
  }
}

/* testscriptC -- actually runs a test script
 *
 */
static void testscriptC(mps_arena_t arena, mps_ap_t ap, const char *script)
{
  const char *scriptAll = script;
  int si, sb;  /* sscanf items, sscanf bytes */

  while(*script != '\0') {
    switch(*script) {
      case 'C': {
        si = sscanf(script, "Collect%n",
                       &sb);
        checksi(si, 0, script, scriptAll);
        script += sb;
        printf("  Collect\n");
        mps_arena_collect(arena);
        break;
      }
      case 'K': {
        si = sscanf(script, "Katalog()%n",
                       &sb);
        checksi(si, 0, script, scriptAll);
        script += sb;
        printf("  Katalog()\n");
        CatalogDo(arena, ap);
        break;
      }
      case 'M': {
        unsigned keepCount = 0;
        unsigned long objCount = 0;
        unsigned keepTotal = 0;
        unsigned keep1in = 0;
        unsigned keepRootspace = 0;
        si = sscanf(script, "Make(keep-1-in %u, keep %u, rootspace %u)%n",
                    &keep1in, &keepTotal, &keepRootspace, &sb);
        checksi(si, 3, script, scriptAll);
        script += sb;
        printf("  Make(keep-1-in %u, keep %u, rootspace %u).\n",
               keep1in, keepTotal, keepRootspace);
        
        Insist(keepRootspace <= myrootCOUNT);

        objCount = 0;
        while(keepCount < keepTotal) {
          mps_word_t v;
          die(make_dylan_vector(&v, ap, 2), "make_dylan_vector");
          DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(objCount);
          DYLAN_VECTOR_SLOT(v, 1) = (mps_word_t)NULL;
          objCount++;
          if(rnd() % keep1in == 0) {
            /* keep this one */
            myroot[rnd() % keepRootspace] = (void*)v;
            keepCount++;
          }
          get(arena);
        }
        printf("  ...made and kept: %u objects, storing cyclically in "
               "first %u roots "
               "(actually created %lu objects, in accord with "
               "keep-1-in %u).\n",
               keepCount, keepRootspace, objCount, keep1in);

        break;
      }
      case ' ':
      case ',':
      case '.': {
        script++;
        break;
      }
      default: {
        printf("unknown script command %c (script %s).\n",
               *script, scriptAll);
        cdie(FALSE, "unknown script command");
        return;
      }
    }
    get(arena);
  }

}


/* testscriptB -- create pools and objects; call testscriptC
 *
 * Is called via mps_tramp, so matches mps_tramp_t function prototype,
 * and use trampDataStruct to pass parameters.
 */

typedef struct trampDataStruct {
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
} trampDataStruct;

static void *testscriptB(void *arg, size_t s)
{
  trampDataStruct trampData;
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t amc;
  mps_root_t root_table;
  mps_ap_t ap;
  mps_root_t root_stackreg;
  void *stack_starts_here;  /* stack scanning starts here */

  Insist(s == sizeof(trampDataStruct));
  trampData = *(trampDataStruct*)arg;
  arena = trampData.arena;
  thr = trampData.thr;
  script = trampData.script;

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&amc, arena, mps_class_amc(), fmt, chain),
      "pool_create amc");
  die(mps_root_create_table(&root_table, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                            myroot, (size_t)myrootCOUNT),
      "root_create");
  die(mps_ap_create(&ap, amc, MPS_RANK_EXACT), "ap_create");
  
  /* root_stackreg: stack & registers are ambiguous roots = mutator's workspace */
  die(mps_root_create_reg(&root_stackreg, arena,
                          mps_rank_ambig(), (mps_rm_t)0, thr,
                          mps_stack_scan_ambig, &stack_starts_here, 0),
      "root_stackreg");


  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_finalization());

  testscriptC(arena, ap, script);

  mps_root_destroy(root_stackreg);
  mps_ap_destroy(ap);
  mps_root_destroy(root_table);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);

  return NULL;
}


/* testscriptA -- create arena, thr, and tramp; call testscriptB
 */
static void testscriptA(const char *script)
{
  mps_arena_t arena;
  int si, sb;  /* sscanf items, sscanf bytes */
  unsigned long arenasize = 0;
  mps_thr_t thr;
  mps_tramp_t trampFunction;
  trampDataStruct trampData;
  void *trampResult;

  si = sscanf(script, "Arena(size %lu)%n", &arenasize, &sb);
  cdie(si == 1, "bad script command: Arena(size %%lu)");
  script += sb;
  printf("  Create arena, size = %lu.\n", arenasize);

  /* arena */
  die(mps_arena_create(&arena, mps_arena_class_vm(), arenasize),
      "arena_create");

  /* thr: used to stop/restart multiple threads */
  die(mps_thread_reg(&thr, arena), "thread");

  /* tramp: used for protection (barrier hits) */
  /* call testscriptB! */
  trampFunction = testscriptB;
  trampData.arena = arena;
  trampData.thr = thr;
  trampData.script = script;
  mps_tramp(&trampResult, trampFunction, &trampData, sizeof trampData);

  mps_thread_dereg(thr);
  mps_arena_destroy(arena);

  printf("  Destroy arena etc.\n\n");

}


/* main -- runs various test scripts
 *
 */
int main(int argc, char **argv)
{

  randomize(argc, argv);

  /* The most basic scripts */

  /* 1<<19 == 524288 == 1/2 Mebibyte */
  testscriptA("Arena(size 524288), Make(keep-1-in 5, keep 50000, rootspace 30000), Collect.");

  /* 16<<20 == 16777216 == 16 Mebibyte */
  /* See .catalog.broken.
  testscriptA("Arena(size 16777216), Katalog(), Collect.");
  */

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
