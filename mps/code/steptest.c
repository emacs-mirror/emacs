/* steptest.c: TEST FOR ARENA STEPPING
 *
 * $Id$
 * Copyright (c) 1998-2014 Ravenbrook Limited.  See end of file for license.
 *
 * Loosely based on <code/amcss.c>.
 */

#include "fmtdy.h"
#include "fmtdytst.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpm.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#include "mps.h"

#include <math.h> /* pow */
#include <stdio.h> /* fflush, printf, putchar, stdout */

#define testArenaSIZE     ((size_t)((size_t)64 << 20))
#define avLEN             3
#define exactRootsCOUNT   200
#define ambigRootsCOUNT   50
#define objCOUNT          2000000
#define clockSetFREQ      10000
#define multiStepFREQ     500000
#define multiStepMULT     100

#define genCOUNT          3
#define gen1SIZE          750  /* kB */
#define gen2SIZE          2000 /* kB */
#define gen3SIZE          5000 /* kB */
#define gen1MORTALITY     0.85
#define gen2MORTALITY     0.60
#define gen3MORTALITY     0.40

/* testChain -- generation parameters for the test */

static mps_gen_param_s testChain[genCOUNT] = {
    {gen1SIZE, gen1MORTALITY},
    {gen2SIZE, gen2MORTALITY},
    {gen3SIZE, gen3MORTALITY},
};

/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL           ((mps_addr_t)MPS_WORD_CONST(0xDECEA5ED))

static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];

/* Things we want to measure.  Times are all in microseconds. */

static double alloc_time;       /* Time spent allocating */
static double max_alloc_time;   /* Max time taken to allocate one object */
static double step_time;        /* Time spent in mps_arena_step returning 1 */
static double max_step_time;    /* Max time of mps_arena_step returning 1 */
static double no_step_time;     /* Time spent in mps_arena_step returning 0 */
static double max_no_step_time; /* Max time of mps_arena_step returning 0 */

static double total_clock_time; /* Time spent reading the clock */
static long clock_reads;        /* Number of times clock is read */
static long steps;              /* # of mps_arena_step calls returning 1 */
static long no_steps;           /* # of mps_arena_step calls returning 0 */
static size_t alloc_bytes;      /* # of bytes allocated */
static long commit_failures;    /* # of times mps_commit fails */


/* Operating-system dependent timing.  Defines two functions, void
 * prepare_clock(void) and double my_clock(void).  my_clock() returns
 * the number of microseconds of CPU time used so far by the process.
 * prepare_clock() sets things up so that my_clock() can run
 * efficiently.
 */

#ifdef MPS_OS_W3

#include "mpswin.h"

static HANDLE currentProcess;

static void prepare_clock(void)
{
    currentProcess = GetCurrentProcess();
}

static double my_clock(void)
{
    FILETIME ctime, etime, ktime, utime;
    double dk, du;
    cdie(GetProcessTimes(currentProcess, &ctime, &etime, &ktime, &utime) != 0,
         "GetProcessTimes");
    dk = ktime.dwHighDateTime * 4096.0 * 1024.0 * 1024.0 +
        ktime.dwLowDateTime;
    dk /= 10.0;
    du = utime.dwHighDateTime * 4096.0 * 1024.0 * 1024.0 +
        utime.dwLowDateTime;
    du /= 10.0;
    ++ clock_reads;
    return (du+dk);
}

#else
/* on Posix systems, we can use getrusage. */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

static void prepare_clock(void)
{
    /* do nothing */
}

static double my_clock(void)
{
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    ++ clock_reads;
    return ((ru.ru_utime.tv_sec +
             ru.ru_stime.tv_sec) * 1000000.0 +
            (ru.ru_utime.tv_usec +
             ru.ru_stime.tv_usec));
}
#endif

/* Need to calibrate the clock. */
/* In fact we need to do this repeatedly while the tests run because
 * on some platforms the time taken to read the clock changes
 * significantly during program execution.  Yes, really (e.g. fri4gc
 * on thrush.ravenbrook.com on 2002-06-28, clock_time goes from 5.43
 * us near process start to 7.45 us later). */

static double clock_time;      /* current estimate of time to read the clock */

/* take at least this many microseconds to set the clock */
#define CLOCK_TIME_SET 10000

/* set_clock_timing() sets clock_time. */

static void set_clock_timing(void)
{
    long i;
    double t1, t2, t3;

    t2 = 0.0;
    t3 = my_clock();
    i = 0;
    do {
        t1 = my_clock();
        /* do nothing here */
        t2 += my_clock()-t1;
        ++i;
    } while (t1 < t3 + CLOCK_TIME_SET);
    clock_time = t2/i;
    total_clock_time += my_clock() - t3 + clock_time;
}

/* How much time has elapsed since a recent call to my_clock?
 * Deducts the calibrated clock timing, clamping to zero.
 *
 * The idea is to have code like this:
 *
 *     t = my_clock();
 *     do_something();
 *     t = time_since(t);
 *
 * and the result will be our best estimate of how much CPU time the
 * call to do_something() took.
 */

static double time_since(double t)
{
    t = my_clock() - t;
    total_clock_time += clock_time + clock_time;
    if (t < clock_time)
        return 0.0;
    else
        return (t - clock_time);
}

/* print a number of microseconds in a useful format. */

#define MAXPRINTABLE 100.0
#define MINPRINTABLE (MAXPRINTABLE / 1000.0)

static void print_time(const char *before, double t, const char *after)
{
    char prefixes[] = "\0munpfazy"; /* don't print "ks" etc */
    char *x = prefixes+2; /* start at micro */
    double ot = t;
    if (before)
        printf("%s", before);
    if (t > MAXPRINTABLE) {
        while (x[-1] && t > MAXPRINTABLE) {
            t /= 1000.0;
            -- x;
        }
        if (t < MAXPRINTABLE) {
            printf("%.3f %cs", t, *x);
        } else {
            printf("%.3f s", t/1000.0);
        }
    } else {
        while (x[1] && t < MINPRINTABLE) {
            t *= 1000.0;
            ++ x;
        }
        if (t > MINPRINTABLE)
            printf("%.3f %cs", t, *x);
        else
            printf("%g s", ot/1000000.0);
    }
    if (after)
        printf("%s", after);
}

/* Make a single Dylan object */

static mps_addr_t make(void)
{
    size_t length = rnd() % (avLEN * 2);
    size_t size = (length+2) * sizeof(mps_word_t);
    mps_addr_t p;
    mps_res_t res;

    alloc_bytes += size;

    for(;;) {
        mps_bool_t commit_res;
        double t1, t2;
        t1 = my_clock();
        MPS_RESERVE_BLOCK(res, p, ap, size);
        t1 = time_since(t1); /* reserve time */
        if(res)
            die(res, "MPS_RESERVE_BLOCK");
        res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
        if(res)
            die(res, "dylan_init");
        t2 = my_clock();
        commit_res = mps_commit(ap, p, size);
        t2 = time_since(t2); /* commit time */
        t1 += t2; /* total MPS time for this allocation */
        alloc_time += t1;
        if (t1 > max_alloc_time)
            max_alloc_time = t1;
        if (commit_res)
            break;
        else
            ++ commit_failures;
    }

    return p;
}

/* call mps_arena_step() */

static void test_step(mps_arena_t arena, double multiplier)
{
    mps_bool_t res;
    double t1 = my_clock();
    res = mps_arena_step(arena, 0.1, multiplier);
    cdie(ArenaGlobals(arena)->clamped, "arena was unclamped");
    t1 = time_since(t1);
    if (res) {
        if (t1 > max_step_time)
            max_step_time = t1;
        step_time += t1;
        ++ steps;
    } else {
        if (t1 > max_no_step_time)
            max_no_step_time = t1;
        no_step_time += t1;
        ++ no_steps;
    }
}

/* test -- the body of the test */

static void test(mps_arena_t arena, unsigned long step_period)
{
    mps_fmt_t format;
    mps_chain_t chain;
    mps_root_t exactRoot, ambigRoot;
    unsigned long objs;
    size_t i;
    mps_message_t message;
    size_t live, condemned, not_condemned;
    size_t messages;
    mps_word_t collections, old_collections;
    double total_mps_time, total_time;
    double t1;

    die(dylan_fmt(&format, arena), "fmt_create");
    die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

    die(mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
        "pool_create(amc)");

    die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

    for(i = 0; i < exactRootsCOUNT; ++i)
        exactRoots[i] = objNULL;
    for(i = 0; i < ambigRootsCOUNT; ++i)
        ambigRoots[i] = rnd_addr();

    die(mps_root_create_table_masked(&exactRoot, arena,
                                     mps_rank_exact(), (mps_rm_t)0,
                                     &exactRoots[0], exactRootsCOUNT,
                                     (mps_word_t)1),
        "root_create_table(exact)");
    die(mps_root_create_table(&ambigRoot, arena,
                              mps_rank_ambig(), (mps_rm_t)0,
                              &ambigRoots[0], ambigRootsCOUNT),
        "root_create_table(ambig)");

    printf("Stepping every %lu allocations.\n", step_period);

    mps_message_type_enable(arena, mps_message_type_gc());

    /* zero all our counters and timers. */

    objs = 0;
    clock_reads = 0;
    steps = no_steps = 0;
    alloc_bytes = 0;
    commit_failures = 0;
    alloc_time = step_time = no_step_time = 0.0;
    max_alloc_time = max_step_time = max_no_step_time = 0.0;
    total_clock_time = 0.0;
    collections = old_collections = 0;

    t1 = my_clock();

    while(objs < objCOUNT) {
        size_t r;

        r = (size_t)rnd();
        if(r & 1) {
            i = (r >> 1) % exactRootsCOUNT;
            if(exactRoots[i] != objNULL)
                cdie(dylan_check(exactRoots[i]), "dying root check");
            exactRoots[i] = make();
            if(exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
                dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                            exactRoots, exactRootsCOUNT);
        } else {
            i = (r >> 1) % ambigRootsCOUNT;
            ambigRoots[(ambigRootsCOUNT-1) - i] = make();
            /* Create random interior pointers */
            ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
        }

        ++objs;

        if (objs % step_period == 0)
            test_step(arena, 0.0);

        if (objs % multiStepFREQ == 0)
            test_step(arena, multiStepMULT);

        if (objs % clockSetFREQ == 0)
            set_clock_timing();

        collections = mps_collections(arena);
        if (collections > old_collections) {
            old_collections = collections;
            putchar('.');
            (void)fflush(stdout);
        }
    }

    total_time = time_since(t1) - total_clock_time;

    if (collections > 0)
        printf("\n");

    messages = live = condemned = not_condemned = 0;
    while (mps_message_get(&message, arena, mps_message_type_gc())) {
        ++ messages;
        live += mps_message_gc_live_size(arena, message);
        condemned += mps_message_gc_condemned_size(arena, message);
        not_condemned += mps_message_gc_not_condemned_size(arena,
                                                           message);
        mps_message_discard(arena, message);
    }
    if (collections != messages) {
        printf("%lu collections but %lu messages\n",
               (unsigned long)collections, (unsigned long)messages);
        collections = messages;
    }

    total_mps_time = alloc_time + step_time + no_step_time;
    printf("Collection statistics:\n");
    printf("  %"PRIuLONGEST" collections\n", (ulongest_t)collections);
    printf("  %"PRIuLONGEST" bytes condemned.\n", (ulongest_t)condemned);
    printf("  %"PRIuLONGEST" bytes not condemned.\n",
           (ulongest_t)not_condemned);
    printf("  %"PRIuLONGEST" bytes survived.\n", (ulongest_t)live);
    if (condemned) {
        printf("  Mortality %5.2f%%.\n",
               (1.0 - ((double)live)/condemned) * 100.0);
        printf("  Condemned fraction %5.2f%%.\n",
               ((double)condemned/(condemned + not_condemned)) * 100.0);
    }
    if (collections) {
        printf("  Condemned per collection %"PRIuLONGEST" bytes.\n",
               (ulongest_t)condemned/collections);
        printf("  Reclaimed per collection %"PRIuLONGEST" bytes.\n",
               (ulongest_t)(condemned - live)/collections);
    }

    printf("Allocation statistics:\n");
    printf("  %"PRIuLONGEST" objects (%"PRIuLONGEST" bytes) allocated.\n",
           (ulongest_t)objs, (ulongest_t)alloc_bytes);
    printf("  Commit failed %ld times.\n", commit_failures);

    printf("Timings:\n");
    print_time("  Allocation took ", alloc_time, "");
    print_time(", mean ", alloc_time / objs, "");
    print_time(", max ", max_alloc_time, ".\n");
    if (steps) {
        printf("  %ld steps took ", steps);
        print_time("", step_time, "");
        print_time(", mean ", step_time/steps, "");
        print_time(", max ", max_step_time, ".\n");
    }
    if (no_steps) {
        printf("  %ld non-steps took ", no_steps);
        print_time("", no_step_time, "");
        print_time(", mean ", no_step_time / no_steps, "");
        print_time(", max ", max_no_step_time, ".\n");
    }
    if (alloc_time > 0.0)
        printf("  Allocated %.2f bytes per us.\n",
               (double)alloc_bytes/alloc_time);
    if (step_time > 0.0) {
        printf("  Reclaimed %.2f bytes per us of step.\n",
               (double)(condemned - live)/step_time);
        if (collections > 0) {
            printf("  Took %.2f steps ", (double)steps/collections);
            print_time("(", step_time / collections, ") per collection.\n");
        }
    }
    print_time("  Total time ", total_time, ".\n");
    print_time("  Total MPS time ", total_mps_time, "");
    printf(" (%5.2f%%, ", total_mps_time * 100.0 / total_time);
    print_time("", total_mps_time/alloc_bytes, " per byte, ");
    print_time("", total_mps_time/objs, " per object)\n");
    print_time("  (adjusted for clock timing: ",
               total_clock_time,
               " spent reading the clock;\n");
    printf("   %"PRIuLONGEST" clock reads; ", (ulongest_t)clock_reads);
    print_time("", total_clock_time / clock_reads, " per read;");
    print_time(" recently measured as ", clock_time, ").\n");

    mps_arena_park(arena);
    mps_ap_destroy(ap);
    mps_root_destroy(exactRoot);
    mps_root_destroy(ambigRoot);
    mps_pool_destroy(pool);
    mps_chain_destroy(chain);
    mps_fmt_destroy(format);
}

int main(int argc, char *argv[])
{
    mps_arena_t arena;
    prepare_clock();
    testlib_init(argc, argv);
    set_clock_timing();
    die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
        "arena_create");
    mps_arena_clamp(arena);
    test(arena, (unsigned long)pow(10, rnd() % 10));
    mps_arena_destroy(arena);
    printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
    return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
