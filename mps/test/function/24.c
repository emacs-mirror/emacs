/* 
TEST_HEADER
 id = $HopeName$
 summary = Allocate but never look at objects
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"

void *stackpointer;

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 MPS_SCAN_BEGIN(ss)
 comment("Scan: %p", base);
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

static mps_addr_t myskip(mps_addr_t object)
{
 return (mps_addr_t) ((char *) object + 1);
}

static void mycopy(mps_addr_t object, mps_addr_t to)
{
}

static void mypad(mps_addr_t base, size_t size)
{
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 return NULL;
}

static void myfwd(mps_addr_t object, mps_addr_t to)
{
}

struct mps_fmt_A_s fmtA =
{
 1,
 &myscan,
 &myskip,
 &mycopy,
 &myfwd,
 &myisfwd,
 &mypad
};


static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mps_addr_t p;

 int i;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 for(i=1; i<1000; i++)
 {
  do
  { die(mps_reserve(&p, ap, 10*1024*1024), "Reserve: ");
  }
  while (!mps_commit(ap, p, 10*1024*1024));
  comment("%i at %p", i, p);
  comment("%i objects of 10 megabytes each allocated", i);
 }

 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_space_destroy(space);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

