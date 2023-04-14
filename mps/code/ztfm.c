/* ztfm.c: Transform test
 *
 * $Id$
 * Copyright (c) 2010 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * OBJECTIVE
 *
 *
 * DESIGN OVERVIEW
 *
 * CODE OVERVIEW
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
 * This code was created by first copying <code/zcoll.c>.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpslib.h"
#include "mpstr.h"
#include "testlib.h"

#include <stdio.h> /* printf */


#define progressf(args) \
    printf args;
#define Xprogressf(args) \
    do{if(0)printf args;}while(FALSE)

/* testChain -- generation parameters for the test */
#define genCOUNT 2
static mps_gen_param_s testChain[genCOUNT] = {
  { 100, 0.85 }, { 170, 0.45 } };


/* myroot -- arrays of references that are the root */
#define myrootAmbigCOUNT 30000
static void *myrootAmbig[myrootAmbigCOUNT];
#define myrootExactCOUNT 1000000
static void *myrootExact[myrootExactCOUNT];

static mps_root_t root_stackreg;
static void *stack_start;
static mps_thr_t stack_thr;


/* ===========  Transform ==============*/
static void get(mps_arena_t arena);

static ulongest_t serial = 0;

/* Tree nodes
 *
 * To make a node:
 *   - use next unique serial;
 *   - choose an id (eg. next in sequence);
 *   - choose a version (eg. 0).
 *
 * To make a New node from (or in connection with) an Old:
 *   - use next unique serial;
 *   - copy id of Old;
 *   - ver = ver(Old) + 1.
 *
 * To invalidate an Old node, when adding an OldNew pair for it:
 *   - ver = -1
 */

struct node_t {
  mps_word_t     word0;
  mps_word_t     word1;
  mps_word_t     serial_dyi;  /* unique across every node ever */
  mps_word_t     id_dyi;  /* .id: replacement nodes copy this */
  mps_word_t     ver_dyi;  /* .version: distinguish new from old */
  struct node_t  *left;
  struct node_t  *right;
  mps_word_t     tour_dyi;  /* latest tour that visited this node */
  mps_word_t     tourIdHash_dyi;  /* hash of node ids, computed last tour */
};

/* Tour -- a particular journey to visit to every node in the world
 *
 * A tour starts with the node at world[0], tours the graph reachable 
 * from it, then any further bits of graph reachable from world[1], 
 * and so on.
 *
 * As it does so, the tour computes a tourReport, characterising the 
 * state of everything reachable (from world) in the form a few numbers.
 *
 * Each tour explores the subgraph rooted at each node exactly once.  
 * That is: if the tour re-encounters a node already visited on that 
 * tour, it uses the values already computed for that node.
 *
 * The tourIdHash deliberately depends on the order in which nodes are 
 * encountered.  Therefore, the tourIdHash of a tour depends on the 
 * entirety of the state of the world and all the world-reachable nodes.  
 *
 * The tourVerSum, being a simple sum, does not depend on the order of 
 * visiting.
 */

enum {
  cVer = 10
}; 
typedef struct tourReportStruct {
  ulongest_t  tour;  /* tour serial */
  ulongest_t  tourIdHash;  /* hash of node ids, computed last tour */
  ulongest_t  acNodesVer[cVer];  /* count of nodes of each Ver */
} *tourReport;

static void tour_subgraph(tourReport tr_o, struct node_t *node);

static ulongest_t tourSerial = 0;

static void tourWorld(tourReport tr_o, mps_addr_t *world, ulongest_t countWorld)
{
  ulongest_t i;
  
  tourSerial += 1;
  tr_o->tour = tourSerial;
  tr_o->tourIdHash = 0;
  for(i = 0; i < cVer; i++) {
    tr_o->acNodesVer[i] = 0;
  }
  Xprogressf(( "[tour %"SCNuLONGEST"] BEGIN, world: %p, countWorld: %"SCNuLONGEST"\n",
    tourSerial, (void *)world, countWorld));

  for(i = 0; i < countWorld; i++) {
    struct node_t *node;
    node = (struct node_t *)world[i];
    tour_subgraph(tr_o, node);
    tr_o->tourIdHash += i * (node ? DYI_INT(node->tourIdHash_dyi) : 0);
  }
}

static void tour_subgraph(tourReport tr_o, struct node_t *node)
{
  ulongest_t tour;
  ulongest_t ver;
  ulongest_t id;
  struct node_t *left;
  struct node_t *right;
  ulongest_t tourIdHashLeft;
  ulongest_t tourIdHashRight;
  ulongest_t tourIdHash;

  Insist(tr_o != NULL);
  
  /* node == NULL is permitted */
  if(node == NULL)
    return;
  
  tour = tr_o->tour;
  if(DYI_INT(node->tour_dyi) == tour)
    return;  /* already visited */
  
  /* this is a newly discovered node */
  Insist(DYI_INT(node->tour_dyi) < tour);

  /* mark as visited */
  node->tour_dyi = INT_DYI(tour);

  /* 'local' idHash = id, used for any re-encounters while computing the computed idHash */
  node->tourIdHash_dyi = node->id_dyi;

  /* record this node in the array of ver counts */
  ver = DYI_INT(node->ver_dyi);
  Insist(ver < cVer);
  tr_o->acNodesVer[ver] += 1;
  
  /* tour the subgraphs (NULL is permitted) */
  left = node->left;
  right = node->right;
  tour_subgraph(tr_o, left);
  tour_subgraph(tr_o, right);
  
  /* computed idHash of subgraph at this node */
  id = DYI_INT(node->id_dyi);
  tourIdHashLeft = left ? DYI_INT(left->tourIdHash_dyi) : 0;
  tourIdHashRight = right ? DYI_INT(right->tourIdHash_dyi) : 0;
  tourIdHash = (13*id + 17*tourIdHashLeft + 19*tourIdHashRight) & DYLAN_UINT_MASK;
  Insist(tourIdHash <= DYLAN_UINT_MAX);
  node->tourIdHash_dyi = INT_DYI(tourIdHash);

  Insist(DYI_INT(node->tour_dyi) == tour);
  Xprogressf(( "[tour %"SCNuLONGEST"] new completed node: %p, ver: %"SCNuLONGEST", tourIdHash: %"SCNuLONGEST"\n",
    tour, (void*)node, ver, tourIdHash ));
}

static struct tourReportStruct trBefore;
static struct tourReportStruct trAfter;

static void before(mps_addr_t *world, ulongest_t countWorld)
{
  tourWorld(&trBefore, world, countWorld);
}

static void after(mps_addr_t *world, ulongest_t countWorld,
  ulongest_t verOld,
  longest_t deltaCVerOld,
  ulongest_t verNew,
  longest_t deltaCVerNew)
{
  longest_t dCVerOld;
  longest_t dCVerNew;
  
  tourWorld(&trAfter, world, countWorld);

  dCVerOld = ((long)trAfter.acNodesVer[verOld] - (long)trBefore.acNodesVer[verOld]);
  dCVerNew = ((long)trAfter.acNodesVer[verNew] - (long)trBefore.acNodesVer[verNew]);
  
  progressf(("tourWorld: (%"PRIuLONGEST"  %"PRIuLONGEST":%"PRIuLONGEST"/%"PRIuLONGEST":%"PRIuLONGEST") -> (%"PRIuLONGEST"  %"PRIuLONGEST":%+"PRIdLONGEST"/%"PRIuLONGEST":%+"PRIdLONGEST"), %s\n",
    trBefore.tourIdHash,
    verOld,
    trBefore.acNodesVer[verOld],
    verNew,
    trBefore.acNodesVer[verNew],

    trAfter.tourIdHash,
    verOld,
    dCVerOld,
    verNew,
    dCVerNew,

    trBefore.tourIdHash == trAfter.tourIdHash ? "same" : "XXXXX DIFFERENT XXXXX"
  ));
  Insist(trBefore.tourIdHash == trAfter.tourIdHash);
  Insist(dCVerOld == deltaCVerOld);
  Insist(dCVerNew == deltaCVerNew);
}


static mps_res_t mps_arena_transform_objects_list(mps_bool_t *transform_done_o,
                                                  mps_arena_t mps_arena,
                                                  mps_addr_t  *old_list,
                                                  size_t      old_list_count,
                                                  mps_addr_t  *new_list,
                                                  size_t      new_list_count)
{
  mps_res_t res;
  mps_transform_t transform;
  mps_bool_t applied = FALSE;
  
  Insist(old_list_count == new_list_count);
  
  res = mps_transform_create(&transform, mps_arena);
  if(res == MPS_RES_OK) {
    /* We have a transform */
    res = mps_transform_add_oldnew(transform, old_list, new_list, old_list_count);
    if(res == MPS_RES_OK) {
      res = mps_transform_apply(&applied, transform);
    }
    if(applied) {
      /* Transform has been destroyed */
      Insist(res == MPS_RES_OK);
    } else {
      mps_transform_destroy(transform);
    }
  }
  
  /* Always set *transform_done_o (even if there is also a non-ResOK */
  /* return code): it is a status report, not a material return. */
  *transform_done_o = applied;
  return res;
}

static void Transform(mps_arena_t arena, mps_ap_t ap)
{
  ulongest_t i;
  ulongest_t keepCount = 0;
  mps_word_t v;
  struct node_t *node;
  mps_res_t res;
  mps_bool_t transform_done;
  ulongest_t old, new;
  ulongest_t perset;

  mps_arena_park(arena);

  {
    /* Test with sets of pre-built nodes, a known distance apart.
     *
     * This gives control over whether new nodes are on the same 
     * segment as the olds or not.
     */
    
    ulongest_t iPerset;
    ulongest_t aPerset[] = {0, 1, 1, 10, 10, 1000, 1000};
    ulongest_t cPerset = NELEMS(aPerset);
    ulongest_t stepPerset;
    ulongest_t countWorld = 0;

    /* randomize the order of set sizes from aPerset */
    stepPerset = 1 + (rnd() % (cPerset - 1));

    progressf(("INT_DYI(1): %"PRIuLONGEST"; DYI_INT(5): %"PRIuLONGEST"\n",
              (ulongest_t)INT_DYI(1), (ulongest_t)DYI_INT(5) ));
    progressf(("Will make and transform sets of old nodes into new nodes.  Set sizes: "));
    for(iPerset = stepPerset;
        aPerset[iPerset] != 0;
        iPerset = (iPerset + stepPerset) % cPerset) {
      countWorld += aPerset[iPerset] * 2;  /* 2: old + new */
      progressf(("%"PRIuLONGEST", ", aPerset[iPerset]));
    }
    progressf(("total: %"PRIuLONGEST".\n", countWorld));
    Insist(countWorld <= myrootExactCOUNT);
    
    keepCount = 0;
    
    for(iPerset = stepPerset;
        aPerset[iPerset] != 0;
        iPerset = (iPerset + stepPerset) % cPerset) {
      ulongest_t j;
      ulongest_t first;
      ulongest_t skip;
      ulongest_t count;
      
      perset = aPerset[iPerset];
      first = keepCount;
      skip = 0;
      count = perset;
      progressf(("perset: %"PRIuLONGEST", first: %"PRIuLONGEST"\n", perset, first));

      /* Make a set of olds, and a set of news */
      for(j = 0; j < 2 * perset; j++) {
        ulongest_t slots = (sizeof(struct node_t) / sizeof(mps_word_t)) - 2;
        /* make_dylan_vector: fills slots with INT_DYI(0) */
        die(make_dylan_vector(&v, ap, slots), "make_dylan_vector");
        node = (struct node_t *)v;
        node->serial_dyi = INT_DYI(serial++);
        node->id_dyi = INT_DYI(j % perset);
        node->ver_dyi = INT_DYI(1 + (j >= perset));
        node->left = NULL;
        node->right = NULL;
        node->tour_dyi = INT_DYI(tourSerial);
        node->tourIdHash_dyi = INT_DYI(0);
        myrootExact[keepCount++ % myrootExactCOUNT] = (void*)v;
        get(arena);
        /*printf("Object %"PRIuLONGEST" at %p.\n", keepCount, (void*)v);*/
      }
      v = 0;
      
      /* >=10? pick subset */
      if(perset >= 10) {
        /* subset of [first..first+perset) */
        
        skip = (rnd() % (2 * perset));
        if(skip > (perset - 1))
          skip = 0;

        count = 1 + rnd() % (2 * (perset - skip));
        if(skip + count > perset)
          count = perset - skip;
        
        Insist(skip < perset);
        Insist(count >= 1);
        Insist(skip + count <= perset);
      }
      
      /* >=10? sometimes build tree */
      if(perset >= 10 && count >= 4 && rnd() % 2 == 0) {
        struct node_t **oldNodes = (struct node_t **)&myrootExact[first + skip];
        struct node_t **newNodes = (struct node_t **)&myrootExact[first + skip + perset];
        progressf(("Building tree in %"PRIuLONGEST" nodes.\n", count));
        for(j = 1; (2 * j) + 1 < count; j++) {
          oldNodes[j]->left = oldNodes[2 * j];
          oldNodes[j]->right = oldNodes[(2 * j) + 1];
          if(1){newNodes[j]->left = newNodes[2 * j];
          newNodes[j]->right = newNodes[(2 * j) + 1];}
        }
      }
      
      /* transform {count} olds into {count} news */
      before(myrootExact, countWorld);
      /* after(myrootExact, countWorld, 1, 0, 2, 0); */
      progressf(("Transform [%"PRIuLONGEST"..%"PRIuLONGEST") to [%"PRIuLONGEST"..%"PRIuLONGEST").\n",
        first + skip, first + skip + count, first + skip + perset, first + skip + count + perset));
      res = mps_arena_transform_objects_list(&transform_done, arena, 
        &myrootExact[first + skip], count,
        &myrootExact[first + skip + perset], count);
      Insist(res == MPS_RES_OK);
      Insist(transform_done);
      /* Olds decrease; news were in world already so don't increase. */
      after(myrootExact, countWorld, 1, -(longest_t)count, 2, 0);
    }
  }
  
  {
    /* Transforming in various situations
     *
     * First, make two sets of 1024 nodes.
     */
    
    perset = 1024;
    Insist(2*perset < myrootExactCOUNT);
    for(keepCount = 0; keepCount < 2*perset; keepCount++) {
      ulongest_t slots = (sizeof(struct node_t) / sizeof(mps_word_t)) - 2;
      /* make_dylan_vector: fills slots with INT_DYI(0) */
      die(make_dylan_vector(&v, ap, slots), "make_dylan_vector");
      node = (struct node_t *)v;
      node->serial_dyi = INT_DYI(serial++);
      node->id_dyi = INT_DYI(keepCount % perset);
      node->ver_dyi = INT_DYI(1 + (keepCount >= perset));
      node->left = NULL;
      node->right = NULL;
      myrootExact[keepCount % myrootExactCOUNT] = (void*)v;
      get(arena);
      /* printf("Object %u at %p.\n", keepCount, (void*)v); */
    }
    v = 0;

    
    /* Functions before() and after() checksum the world, and verify 
     * that the expected transform occurred.
     */
    before(myrootExact, perset);
    after(myrootExact, perset, 1, 0, 2, 0);

    /* Don't transform node 0: its ref coincides with a segbase, so 
     * there are probably ambiguous refs to it on the stack.
     * Don't transform last node either: this test code may leave an 
     * ambiguous reference to it on the stack.
     */

    /* Refs in root */
    /* ============ */
    old = 1;
    new = 1 + perset;
    Insist(myrootExact[old] != myrootExact[new]);
    before(myrootExact, perset);
    res = mps_arena_transform_objects_list(&transform_done, arena, &myrootExact[old], 1, &myrootExact[new], 1);
    Insist(res == MPS_RES_OK);
    Insist(transform_done);
    Insist(myrootExact[old] == myrootExact[new]);
    after(myrootExact, perset, 1, -1, 2, +1);

    /* Refs in root: ambiguous ref causes failure */
    /* ========================================== */
    old = 2;
    new = 2 + perset;
    Insist(myrootExact[old] != myrootExact[new]);
    /* Make an ambiguous reference.  This must make the transform fail. */
    myrootAmbig[1] = myrootExact[old];
    before(myrootExact, perset);
    res = mps_arena_transform_objects_list(&transform_done, arena, &myrootExact[old], 1, &myrootExact[new], 1);
    Insist(res == MPS_RES_OK);
    Insist(!transform_done);
    Insist(myrootExact[old] != myrootExact[new]);
    after(myrootExact, perset, 1, 0, 2, 0);

    /* Ref in an object */
    /* ================ */
    old = 3;
    new = 3 + perset;
    node = myrootExact[4];
    progressf(("node: %p\n", (void *)node));
    node->left = myrootExact[old];
    Insist(myrootExact[old] != myrootExact[new]);
    before(myrootExact, perset);
    res = mps_arena_transform_objects_list(&transform_done, arena, &myrootExact[old], 1, &myrootExact[new], 1);
    Insist(res == MPS_RES_OK);
    Insist(transform_done);
    Insist(myrootExact[old] == myrootExact[new]);
    after(myrootExact, perset, 1, -1, 2, +1);
  }

  {
    /* Tests with mps_transform_t
     *
     * **** USES OBJECTS CREATED IN PREVIOUS TEST GROUP ****
     */
    
    mps_transform_t t1;
    mps_transform_t t2;
    mps_bool_t applied = FALSE;
    ulongest_t k, l;
    mps_addr_t nullref1 = NULL;
    mps_addr_t nullref2 = NULL;

    k = 9;  /* start with this object (in set of 1024) */

    /* Destroy */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    mps_transform_destroy(t1);
    t1 = NULL;

    /* Empty (no add) */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    res = mps_transform_apply(&applied, t1);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t1 = NULL;
    after(myrootExact, perset, 1, 0, 2, 0);

    /* Identity-transform */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    for(l = k + 4; k < l; k++) {
      mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k], 1);
    }
    mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k], 10);
    k += 10;
    res = mps_transform_apply(&applied, t1);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t1 = NULL;
    after(myrootExact, perset, 1, 0, 2, 0);

    /* Mixed non-trivial, NULL- and identity-transforms */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    {
      mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 1);
      k += 1;
      /* NULL */
      mps_transform_add_oldnew(t1, &nullref1, &myrootExact[k + perset], 1);
      k += 1;
      /* identity */
      mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k], 1);
      k += 1;
      /* NULL */
      mps_transform_add_oldnew(t1, &nullref2, &myrootExact[k + perset], 1);
      k += 1;
    }
    mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 10);
    k += 10;
    res = mps_transform_apply(&applied, t1);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t1 = NULL;
    after(myrootExact, perset, 1, -11, 2, +11);

    /* Non-trivial transform */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    for(l = k + 4; k < l; k++) {
      mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 1);
    }
    mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 10);
    k += 10;
    res = mps_transform_apply(&applied, t1);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t1 = NULL;
    after(myrootExact, perset, 1, -14, 2, +14);

    /* Two transforms, first destroyed unused */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 10);
    k += 10;
    l = k;
    res = mps_transform_create(&t2, arena);
    Insist(res == MPS_RES_OK);
    mps_transform_add_oldnew(t2, &myrootExact[l], &myrootExact[l + perset], 10);
    l += 10;
    res = mps_transform_apply(&applied, t2);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t2 = NULL;
    mps_transform_destroy(t1);
    after(myrootExact, perset, 1, -10, 2, +10);

    /* Two transforms, both live [-- not supported yet.  RHSK 2010-12-16] */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    mps_transform_add_oldnew(t1, &myrootExact[k], &myrootExact[k + perset], 10);
    k += 10;
    l = k;
    res = mps_transform_create(&t2, arena);
    Insist(res == MPS_RES_OK);
    mps_transform_add_oldnew(t2, &myrootExact[l], &myrootExact[l + perset], 10);
    l += 10;
    res = mps_transform_apply(&applied, t2);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    t2 = NULL;
    k = l;
    after(myrootExact, perset, 1, -10, 2, +10);

    /* Attempt to destroy after applied. */
    before(myrootExact, perset);
    res = mps_transform_create(&t1, arena);
    Insist(res == MPS_RES_OK);
    res = mps_transform_apply(&applied, t1);
    Insist(res == MPS_RES_OK);
    Insist(applied);
    after(myrootExact, perset, 1, 0, 2, 0);
  }

  /* Large number of objects */
  {
    ulongest_t count;
    mps_transform_t t;
    mps_bool_t applied;

    /* LARGE! */
    perset = myrootExactCOUNT / 2;

    Insist(2*perset <= myrootExactCOUNT);
    for(keepCount = 0; keepCount < 2*perset; keepCount++) {
      ulongest_t slots = (sizeof(struct node_t) / sizeof(mps_word_t)) - 2;
      /* make_dylan_vector: fills slots with INT_DYI(0) */
      die(make_dylan_vector(&v, ap, slots), "make_dylan_vector");
      node = (struct node_t *)v;
      node->serial_dyi = INT_DYI(serial++);
      node->id_dyi = INT_DYI(keepCount % perset);
      node->ver_dyi = INT_DYI(1 + (keepCount >= perset));
      node->left = NULL;
      node->right = NULL;
      myrootExact[keepCount % myrootExactCOUNT] = (void*)v;
      get(arena);
      /* printf("Object %u at %p.\n", keepCount, (void*)v); */
    }
    v = 0;

    /* Refs in root */
    /* ============ */
    /* don't transform 0: its ref coincides with a segbase, so causes ambig refs on stack */
    /* don't transform last: its ambig ref may be left on the stack */
    old = 1;
    new = 1 + perset;
    count = perset - 2;
    Insist(myrootExact[old] != myrootExact[new]);
    before(myrootExact, perset);
    res = mps_transform_create(&t, arena);
    Insist(res == MPS_RES_OK);
    for(i = 0; i < count; i++) {
      res = mps_transform_add_oldnew(t, &myrootExact[old + i], &myrootExact[new + i], 1);
      Insist(res == MPS_RES_OK);
    }
    res = mps_transform_apply(&applied, t);
    Insist(applied);
    Insist(myrootExact[old] == myrootExact[new]);
    after(myrootExact, perset, 1, -(longest_t)count, 2, +(longest_t)count);
  }

  printf("  ...made and kept: %"PRIuLONGEST" objects.\n",
         keepCount);
}


static ulongest_t cols(size_t bytes)
{
  double M;  /* Mebibytes */
  ulongest_t cM;  /* hundredths of a Mebibyte */

  M = (double)bytes / (1UL<<20);
  cM = (ulongest_t)(M * 100 + 0.5);  /* round to nearest */
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
    printf( (i == a)  ? "A"
            : (i < n) ? "n"
            : (i < l) ? "L"
            : (i < c) ? "_"
            : " "
          );
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
#define bPerM (1UL << 20)  /* Mebibytes */
#else
#define bPerM (1000000UL)  /* Megabytes */
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
        
        printf("    %5"PRIuLONGEST": (%5"PRIuLONGEST")",
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
        printf("    Finalization for object %"PRIuLONGEST" at %p\n", (ulongest_t)objind, objaddr);
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
  ulongest_t Catalogs = 0, Pages = 0, Arts = 0, Polys = 0;
  int i, j, k;

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
  printf("Catalog ok with: Catalogs: %"PRIuLONGEST", Pages: %"PRIuLONGEST", Arts: %"PRIuLONGEST", Polys: %"PRIuLONGEST".\n",
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
  myrootExact[CatalogRootIndex] = Catalog;
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
  static ulongest_t keepCount = 0;
  ulongest_t i;
  
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

static ulongest_t df(unsigned randm, ulongest_t number)
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
  ulongest_t objCount = 0;
  
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
         "(actually created %"PRIuLONGEST" objects, in accord with "
         "keep-1-in %u).\n",
         keepCount, keepRootspace, objCount, keep1in);
}


static void Rootdrop(char rank_char)
{
  ulongest_t i;
  
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

#if 0
#define stackwipedepth 50000
static void stackwipe(void)
{
  unsigned iw;
  ulongest_t aw[stackwipedepth];
  
  /* http://xkcd.com/710/ */
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
#endif

static void StackScan(mps_arena_t arena, int on)
{
  if(on) {
    Insist(root_stackreg == NULL);
    die(mps_root_create_reg(&root_stackreg, arena,
                            mps_rank_ambig(), (mps_rm_t)0, stack_thr,
                            mps_stack_scan_ambig, stack_start, 0),
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
        /* stackwipe(); */
        mps_arena_collect(arena);
        mps_arena_release(arena);
        break;
      }
      case 'T': {
        si = sscanf(script, "Transform%n",
                       &sb);
        checksi(si, 0, script, scriptAll);
        script += sb;
        printf("  Transform\n");
        Transform(arena, ap);
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
        printf("  BigdropSmall(big %"PRIuLONGEST", small %c)\n", big, small_ref);
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
        ulongest_t s0;
        si = sscanf(script, "ZRndStateSet(%"SCNuLONGEST")%n",
                       &s0, &sb);
        checksi(si, 1, script, scriptAll);
        script += sb;
        printf("  ZRndStateSet(%"PRIuLONGEST")\n", s0);
        rnd_state_set((unsigned long)s0);
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
  int i;
  mps_root_t root_table_Ambig;
  mps_root_t root_table_Exact;
  mps_ap_t ap;
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
  die(mps_root_create_reg(&root_stackreg, arena,
                          mps_rank_ambig(), (mps_rm_t)0, stack_thr,
                          mps_stack_scan_ambig, stack_start, 0),
      "root_stackreg");


  mps_message_type_enable(arena, mps_message_type_gc_start());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_finalization());

  testscriptC(arena, ap, script);

  printf("  Destroy roots, pools, arena etc.\n\n");
  mps_root_destroy(root_stackreg);
  mps_ap_destroy(ap);
  mps_root_destroy(root_table_Exact);
  mps_root_destroy(root_table_Ambig);
  mps_pool_destroy(amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);

  return NULL;
}


/* testscriptA -- create arena, thr; call testscriptB
 */
static void testscriptA(const char *script)
{
  mps_arena_t arena;
  int si, sb;  /* sscanf items, sscanf bytes */
  ulongest_t arenasize = 0;
  mps_thr_t thr;
  trampDataStruct trampData;

  si = sscanf(script, "Arena(size %"SCNuLONGEST")%n", &arenasize, &sb);
  cdie(si == 1, "bad script command: Arena(size %%"PRIuLONGEST")");
  script += sb;
  printf("  Create arena, size = %"PRIuLONGEST".\n", arenasize);

  /* arena */
  die(mps_arena_create(&arena, mps_arena_class_vm(), arenasize),
      "arena_create");

  /* thr: used to stop/restart multiple threads */
  die(mps_thread_reg(&thr, arena), "thread");

  /* call testscriptB! */
  trampData.arena = arena;
  trampData.thr = thr;
  trampData.script = script;
  testscriptB(&trampData, sizeof trampData);

  mps_thread_dereg(thr);
  mps_arena_destroy(arena);
}


/* main -- runs various test scripts
 *
 */
int main(int argc, char *argv[])
{
  randomize(argc, argv);
  mps_lib_assert_fail_install(assert_die);
  
  /* 1<<19 == 524288 == 1/2 Mebibyte */
  /* 16<<20 == 16777216 == 16 Mebibyte */

  testscriptA("Arena(size 500000000), "
              "Transform"
              /*", Collect, Rootdrop(rank E), Collect, Collect"*/
              ".");

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
