/* zcoll.c: Collection test
 *
 * $Id$
 * Copyright (c) 2008-2020 Ravenbrook Limited.  See end of file for license.
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
 * testscriptA() sets up a new arena and calls testscriptB().
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
#include "mpslib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "fmtdytst.h"
#include "mpstd.h"

#include <stdio.h> /* fflush, printf, putchar, puts, stdout */


/* testChain -- generation parameters for the test */
#define genCOUNT 2
static mps_gen_param_s testChain[genCOUNT] = {
  { 100, 0.85 }, { 170, 0.45 } };


/* myroot -- arrays of references that are the root */
#define myrootAmbigCOUNT 30000
static void *myrootAmbig[myrootAmbigCOUNT];
#define myrootExactCOUNT 30000
static void *myrootExact[myrootExactCOUNT];

static mps_root_t root_stackreg;
static void *stack_start;
static mps_thr_t stack_thr;


static ulongest_t cols(size_t bytes)
{
  double M;  /* Mebibytes */
  ulongest_t cM;  /* hundredths of a Mebibyte */

  M = (double)bytes / ((ulongest_t)1<<20);
  cM = (ulongest_t)(M * 100.0 + 0.5);  /* round to nearest */
  return cM;
}

/* showStatsAscii -- present collection stats, 'graphically'
 *
 */
static void showStatsAscii(size_t notcon, size_t con, size_t live, size_t alimit)
{
  ulongest_t n = cols(notcon);
  ulongest_t c = cols(notcon + con);
  ulongest_t l = cols(notcon + live);  /* a fraction of con */
  ulongest_t a = cols(alimit);
  ulongest_t count;
  ulongest_t i;

  /* if we can show alimit within 200 cols, do so */
  count = (a < 200) ? a + 1 : c;

  for(i = 0; i < count; i++) {
    putchar((i == a)  ? 'A'
            : (i < n) ? 'n'
            : (i < l) ? 'L'
            : (i < c) ? '_'
            :           ' ');
  }
  printf("\n");
}


/* print_M -- print count of bytes as Mebibytes or Megabytes
 *
 * Print as a whole number, "m" for the decimal point, and
 * then the decimal fraction.
 *
 * Input:                208896
 * Output:  (Mebibytes)  0m199
 * Output:  (Megabytes)  0m209
 */
#if 0
#define bPerM ((size_t)1 << 20)  /* Mebibytes */
#else
#define bPerM ((size_t)1000000)  /* Megabytes */
#endif
static void print_M(size_t bytes)
{
  size_t M;  /* M thingies */
  double Mfrac;  /* fraction of an M thingy */

  M = bytes / bPerM;
  Mfrac = (double)(bytes % bPerM);
  Mfrac = (Mfrac / bPerM);

  printf("%1"PRIuLONGEST"m%03.f", (ulongest_t)M, Mfrac * 1000);
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
        printf("    %5"PRIuLONGEST": (%5"PRIuLONGEST")",
               (ulongest_t)mclockBegin, (ulongest_t)(mclockBegin - mclockEnd));
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

        printf("    %5"PRIuLONGEST": (%5"PRIuLONGEST")",
               (ulongest_t)mclockEnd, (ulongest_t)(mclockEnd - mclockBegin));
        printf("    Coll End  ");
        showStatsText(notcon, con, live);
        if (rnd()==0)
          showStatsAscii(notcon, con, live, alimit);
        break;
      }
      case mps_message_type_finalization(): {
        mps_message_finalization_ref(&objaddr, arena, message);
        obj = objaddr;
        objind = DYLAN_INT_INT(DYLAN_VECTOR_SLOT(obj, 0));
        printf("    Finalization for object %"PRIuLONGEST" at %p\n",
               (ulongest_t)objind, objaddr);
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


#define CatalogRootIndex 0
#define CatalogSig MPS_WORD_CONST(0x0000CA2A)  /* CATAlog */
#define CatalogFix 1
#define CatalogVar 10
#define PageSig    MPS_WORD_CONST(0x0000BA9E)  /* PAGE */
#define PageFix 1
#define PageVar 100
#define ArtSig     MPS_WORD_CONST(0x0000A621)  /* ARTIcle */
#define ArtFix 1
#define ArtVar 100
#define PolySig    MPS_WORD_CONST(0x0000B071)  /* POLYgon */
#define PolyFix 1
#define PolyVar 100


static void CatalogCheck(void)
{
  mps_word_t w;
  void *Catalog, *Page, *Art, *Poly;
  unsigned long Catalogs = 0, Pages = 0, Arts = 0, Polys = 0;
  size_t i, j, k;

  /* retrieve Catalog from root */
  Catalog = myrootExact[CatalogRootIndex];
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
      Insist(DYLAN_VECTOR_SLOT(Art, 0) == DYLAN_INT(ArtSig));
      Arts += 1;

      for(k = 0; k < ArtVar; k += 1) {
        /* retrieve Poly from Art */
        w = DYLAN_VECTOR_SLOT(Art, ArtFix + k);
        if(w == DYLAN_INT(0))
          break;
        Poly = (void *)w;
        Insist(DYLAN_VECTOR_SLOT(Poly, 0) == DYLAN_INT(PolySig));
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
 *    <https://info.ravenbrook.com/mail/2009/02/05/18-05-52/0.txt>
 */
static void CatalogDo(mps_arena_t arena, mps_ap_t ap)
{
  mps_word_t v;
  void *Catalog, *Page, *Art, *Poly;
  size_t i, j, k;

  die(make_dylan_vector(&v, ap, CatalogFix + CatalogVar), "Catalog");
  DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(CatalogSig);
  Catalog = (void *)v;

  /* store Catalog in root */
  myrootExact[CatalogRootIndex] = Catalog;
  get(arena);

  (void)fflush(stdout);
  CatalogCheck();

  for(i = 0; i < CatalogVar; i += 1) {
    die(make_dylan_vector(&v, ap, PageFix + PageVar), "Page");
    DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(PageSig);
    Page = (void *)v;

    /* store Page in Catalog */
    DYLAN_VECTOR_SLOT(Catalog, CatalogFix + i) = (mps_word_t)Page;
    get(arena);

    printf("Page %"PRIuLONGEST": make articles\n", (ulongest_t)i);
    (void)fflush(stdout);

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
  (void)fflush(stdout);
  CatalogCheck();
}


/* MakeThing -- make an object of the size requested (in bytes)
 *
 * Any size is accepted.  MakeThing may round it up (MakeThing always
 * makes a dylan vector, which has a minimum size of 8 bytes).  Vector
 * slots, if any, are initialized to DYLAN_INT(0).
 *
 * After making the object, calls get(), to retrieve MPS messages.
 *
 * make_dylan_vector [fmtdytst.c] says:
 *   size = (slots + 2) * sizeof(mps_word_t);
 * That is: a dylan vector has two header words before the first slot.
 */
static void* MakeThing(mps_arena_t arena, mps_ap_t ap, size_t size)
{
  mps_word_t v;
  ulongest_t words;
  ulongest_t slots;

  words = (size + (sizeof(mps_word_t) - 1) ) / sizeof(mps_word_t);
  if(words < 2)
    words = 2;

  slots = words - 2;
  die(make_dylan_vector(&v, ap, slots), "make_dylan_vector");
  get(arena);

  return (void *)v;
}

static void BigdropSmall(mps_arena_t arena, mps_ap_t ap, size_t big, char small_ref)
{
  static unsigned keepCount = 0;
  unsigned i;

  mps_arena_park(arena);
  for(i = 0; i < 100; i++) {
    (void) MakeThing(arena, ap, big);
    if(small_ref == 'A') {
      myrootAmbig[keepCount++ % myrootAmbigCOUNT] = MakeThing(arena, ap, 1);
    } else if(small_ref == 'E') {
      myrootExact[keepCount++ % myrootExactCOUNT] = MakeThing(arena, ap, 1);
    } else {
      cdie(0, "BigdropSmall: small must be 'A' or 'E'.\n");
    }
  }
}


/* df -- diversity function
 *
 * Either deterministic based on "number", or 'random' (ie. call rnd).
 */

static unsigned long df(unsigned randm, unsigned number)
{
  if(randm == 0) {
    return number;
  } else {
    return rnd();
  }
}

static void Make(mps_arena_t arena, mps_ap_t ap, unsigned randm, unsigned keep1in, unsigned keepTotal, unsigned keepRootspace, unsigned sizemethod)
{
  unsigned keepCount = 0;
  unsigned objCount = 0;

  Insist(keepRootspace <= myrootExactCOUNT);

  objCount = 0;
  while(keepCount < keepTotal) {
    mps_word_t v;
    unsigned slots = 2;  /* minimum */
    switch(sizemethod) {
      case 0: {
        /* minimum */
        slots = 2;
        break;
      }
      case 1: {
        slots = 2;
        if(df(randm, objCount) % 10000 == 0) {
          printf("*");
          slots = 300000;
        }
        break;
      }
      case 2: {
        slots = 2;
        if(df(randm, objCount) % 6661 == 0) {  /* prime */
          printf("*");
          slots = 300000;
        }
        break;
      }
      default: {
        printf("bad script command: sizemethod %u unknown.\n", sizemethod);
        cdie(FALSE, "bad script command!");
        break;
      }
    }
    die(make_dylan_vector(&v, ap, slots), "make_dylan_vector");
    DYLAN_VECTOR_SLOT(v, 0) = DYLAN_INT(objCount);
    DYLAN_VECTOR_SLOT(v, 1) = (mps_word_t)NULL;
    objCount++;
    if(df(randm, objCount) % keep1in == 0) {
      /* keep this one */
      myrootExact[df(randm, keepCount) % keepRootspace] = (void*)v;
      keepCount++;
    }
    get(arena);
  }
  printf("  ...made and kept: %u objects, storing cyclically in "
         "first %u roots "
         "(actually created %u objects, in accord with "
         "keep-1-in %u).\n",
         keepCount, keepRootspace, objCount, keep1in);
}


static void Rootdrop(char rank_char)
{
  size_t i;

  if(rank_char == 'A') {
    for(i = 0; i < myrootAmbigCOUNT; ++i) {
      myrootAmbig[i] = NULL;
    }
  } else if(rank_char == 'E') {
    for(i = 0; i < myrootExactCOUNT; ++i) {
      myrootExact[i] = NULL;
    }
  } else {
    cdie(0, "Rootdrop: rank must be 'A' or 'E'.\n");
  }
}


#define stackwipedepth 50000
static void stackwipe(void)
{
  size_t iw;
  unsigned long aw[stackwipedepth];

  /* Do some pointless work that the compiler won't optimise away, so that
     this function wipes over the stack by filling stuff into the "aw"
     array. */

  /* https://xkcd.com/710/ */
  /* I don't want my friends to stop calling; I just want the */
  /* compiler to stop optimising away my code. */

  /* Do you ever get two even numbers next to each other?  Hmmmm :-) */
  for(iw = 0; iw < stackwipedepth; iw++) {
    if((iw & 1) == 0) {
      aw[iw] = 1;
    } else {
      aw[iw] = 0;
    }
  }
  for(iw = 1; iw < stackwipedepth; iw++) {
    if(aw[iw - 1] + aw[iw] != 1) {
      printf("Errrr....\n");
      break;
    }
  }
}


static void StackScan(mps_arena_t arena, int on)
{
  if(on) {
    Insist(root_stackreg == NULL);
    die(mps_root_create_thread(&root_stackreg, arena,
                               stack_thr, stack_start),
        "root_stackreg");
    Insist(root_stackreg != NULL);
  } else {
    Insist(root_stackreg != NULL);
    mps_root_destroy(root_stackreg);
    root_stackreg = NULL;
    Insist(root_stackreg == NULL);
  }
}


/* checksi -- check count of sscanf items is correct
 */

static void checksi(int si, int si_shouldBe, const char *script, const char *scriptAll)
{
  if(si != si_shouldBe) {
    printf("bad script command (sscanf found wrong number of params) %s (full script %s).\n", script, scriptAll);
    cdie(FALSE, "bad script command!");
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
        stackwipe();
        die(mps_arena_collect(arena), "mps_arena_collect");
        mps_arena_release(arena);
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
      case 'B': {
        ulongest_t big = 0;
        char small_ref = ' ';
        si = sscanf(script, "BigdropSmall(big %"SCNuLONGEST", small %c)%n",
                    &big, &small_ref, &sb);
        checksi(si, 2, script, scriptAll);
        script += sb;
        printf("  BigdropSmall(big %"PRIuLONGEST", small %c)\n",
               big, small_ref);
        BigdropSmall(arena, ap, big, small_ref);
        break;
      }
      case 'M': {
        unsigned randm = 0;
        unsigned keep1in = 0;
        unsigned keepTotal = 0;
        unsigned keepRootspace = 0;
        unsigned sizemethod = 0;
        si = sscanf(script, "Make(random %u, keep-1-in %u, keep %u, rootspace %u, sizemethod %u)%n",
                    &randm, &keep1in, &keepTotal, &keepRootspace, &sizemethod, &sb);
        checksi(si, 5, script, scriptAll);
        script += sb;
        printf("  Make(random %u, keep-1-in %u, keep %u, rootspace %u, sizemethod %u).\n",
               randm, keep1in, keepTotal, keepRootspace, sizemethod);
        Make(arena, ap, randm, keep1in, keepTotal, keepRootspace, sizemethod);
        break;
      }
      case 'R': {
        char drop_ref = ' ';
        si = sscanf(script, "Rootdrop(rank %c)%n",
                       &drop_ref, &sb);
        checksi(si, 1, script, scriptAll);
        script += sb;
        printf("  Rootdrop(rank %c)\n", drop_ref);
        Rootdrop(drop_ref);
        break;
      }
      case 'S': {
        unsigned on = 0;
        si = sscanf(script, "StackScan(%u)%n",
                       &on, &sb);
        checksi(si, 1, script, scriptAll);
        script += sb;
        printf("  StackScan(%u)\n", on);
        StackScan(arena, on != 0);
        break;
      }
      case 'Z': {
        unsigned long s0;
        si = sscanf(script, "ZRndStateSet(%lu)%n",
                       &s0, &sb);
        checksi(si, 1, script, scriptAll);
        script += sb;
        printf("  ZRndStateSet(%lu)\n", s0);
        rnd_state_set(s0);
        break;
      }
      case ' ':
      case ',':
      case '.': {
        script++;
        break;
      }
      default: {
        printf("unknown script command '%c' (script %s).\n",
               *script, scriptAll);
        cdie(FALSE, "unknown script command!");
        return;
      }
    }
    get(arena);
  }

}


/* testscriptB -- create pools and objects; call testscriptC */

typedef struct testDataStruct {
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
} testDataStruct;

static void testscriptB(testDataStruct *testData)
{
  mps_arena_t arena;
  mps_thr_t thr;
  const char *script;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t amc;
  size_t i;
  mps_root_t root_table_Ambig;
  mps_root_t root_table_Exact;
  mps_ap_t ap;
  void *stack_starts_here;  /* stack scanning starts here */

  arena = testData->arena;
  thr = testData->thr;
  script = testData->script;

  die(mps_fmt_create_A(&fmt, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&amc, arena, mps_class_amc(), fmt, chain),
      "pool_create amc");

  for(i = 0; i < myrootAmbigCOUNT; ++i) {
    myrootAmbig[i] = NULL;
  }
  die(mps_root_create_table(&root_table_Ambig, arena, mps_rank_ambig(), (mps_rm_t)0,
                            myrootAmbig, (size_t)myrootAmbigCOUNT),
      "root_create - ambig");

  for(i = 0; i < myrootExactCOUNT; ++i) {
    myrootExact[i] = NULL;
  }
  die(mps_root_create_table(&root_table_Exact, arena, mps_rank_exact(), (mps_rm_t)0,
                            myrootExact, (size_t)myrootExactCOUNT),
      "root_create - exact");

  die(mps_ap_create(&ap, amc, mps_rank_exact()), "ap_create");

  /* root_stackreg: stack & registers are ambiguous roots = mutator's workspace */
  stack_start = &stack_starts_here;
  stack_thr = thr;
  die(mps_root_create_thread(&root_stackreg, arena,
                             stack_thr, stack_start),
      "root_stackreg");


  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_finalization());

  testscriptC(arena, ap, script);

  printf("  Destroy roots, pools, arena etc.\n\n");
  mps_arena_park(arena);
  mps_root_destroy(root_stackreg);
  mps_ap_destroy(ap);
  mps_root_destroy(root_table_Exact);
  mps_root_destroy(root_table_Ambig);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);
}


/* testscriptA -- create arena and thr; call testscriptB
 */
static void testscriptA(const char *script)
{
  mps_arena_t arena;
  int si, sb;  /* sscanf items, sscanf bytes */
  unsigned long arenasize = 0;
  mps_thr_t thr;
  testDataStruct testData;

  si = sscanf(script, "Arena(size %lu)%n", &arenasize, &sb);
  cdie(si == 1, "bad script command: Arena(size %%lu)");
  script += sb;
  printf("  Create arena, size = %lu.\n", arenasize);

  /* arena */
  MPS_ARGS_BEGIN(args) {
    /* Randomize pause time as a regression test for job004011. */
    MPS_ARGS_ADD(args, MPS_KEY_PAUSE_TIME, rnd_pause_time());
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arenasize);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "arena_create\n");
  } MPS_ARGS_END(args);

  /* thr: used to stop/restart multiple threads */
  die(mps_thread_reg(&thr, arena), "thread");

  /* call testscriptB! */
  testData.arena = arena;
  testData.thr = thr;
  testData.script = script;
  testscriptB(&testData);

  mps_thread_dereg(thr);
  mps_arena_destroy(arena);
}


/* main -- runs various test scripts
 *
 */
int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  /* 1<<19 == 524288 == 1/2 Mebibyte */
  /* 16<<20 == 16777216 == 16 Mebibyte */

  /* 1<<19 == 524288 == 1/2 Mebibyte */
  /* This is bogus!  sizemethod 1 can make a 300,000-slot dylan vector, ie. 1.2MB. */
  /* Try 10MB arena */
  /* testscriptA("Arena(size 10485760), Make(keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect."); */
  if(1) {
    testscriptA("Arena(size 10000000), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), StackScan(0), Collect, Collect, StackScan(1), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), Collect, Collect.");
  }
  if(1) {
    testscriptA("Arena(size 4000000), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), StackScan(0), Collect, Collect, StackScan(1), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), Collect, Collect.");
  }
  if(1) {
    testscriptA("Arena(size 4000000), "
                "Make(random 1, keep-1-in 5, keep 10000, rootspace 30000, sizemethod 1), "
                "Rootdrop(rank E), Collect, "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), "
                "Rootdrop(rank E), Collect, "
                "Make(random 1, keep-1-in 5, keep 100000, rootspace 30000, sizemethod 1), "
                "Rootdrop(rank E), Collect, "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), "
                "Rootdrop(rank E), Collect.");
  }
  if(1) {
    testscriptA("Arena(size 10485760), "
                "Make(random 0, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 2), "
                "Collect, "
                "Rootdrop(rank E), Collect, Collect, "
                "Make(random 0, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 2), "
                "Collect, "
                "Rootdrop(rank E), Collect, Collect.");
  }
  if(1) {
    testscriptA("Arena(size 10485760), "
                "ZRndStateSet(239185672), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), StackScan(0), Collect, Collect, StackScan(1), "
                "ZRndStateSet(239185672), "
                "Make(random 1, keep-1-in 5, keep 50000, rootspace 30000, sizemethod 1), Collect, "
                "Rootdrop(rank E), Collect, Collect.");
  }

  /* LSP -- Large Segment Padding (job001811)
   *
   * BigdropSmall creates a big object & drops ref to it,
   * then a small object but keeps a ref to it.  Do this 100
   * times.  (It also parks the arena, to avoid incremental
   * collections).
   *
   * If big is 28000, it is <= 28672 bytes and therefore fits on a seg
   * of 7 pages.  AMC classes this as a Medium Segment and uses the
   * remainder, placing the subsequent small object there.  If the ref
   * to small is "A" = ambig, the entire 7-page seg is retained.
   *
   * If big is > 28672 bytes (7 pages), it requires a seg of >= 8
   * pages.  AMC classes this as a Large Segment, and does LSP (Large
   * Segment Padding), to prevent the subsequent small object being
   * placed in the remainder.  If the ref to small is "A" = ambig,
   * only its 1-page seg is retained.  This greatly reduces the
   * retention page-count.
   *
   * If the ref to small is "E" = exact, then the small object is
   * preserved-by-copy onto a new seg.  In this case there is no
   * seg/page retention, so LSP does not help.  It has a small cost:
   * total pages increase from 700 to 900.  So in this case (no ambig
   * retention at all, pessimal allocation pattern) LSP would slightly
   * increase the frequency of minor collections.
   */
  /* 7p = 28672b; 8p = 32768b */
  /* 28000 = Medium segment */
  /* 29000 = Large segment */
  testscriptA("Arena(size 16777216), BigdropSmall(big 28000, small A), Collect.");
  testscriptA("Arena(size 16777216), BigdropSmall(big 29000, small A), Collect.");
  testscriptA("Arena(size 16777216), BigdropSmall(big 28000, small E), Collect.");
  testscriptA("Arena(size 16777216), BigdropSmall(big 29000, small E), Collect.");

  /* 16<<20 == 16777216 == 16 Mebibyte */
  /* See .catalog.broken.
  testscriptA("Arena(size 16777216), Katalog(), Collect.");
  */

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2008-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
