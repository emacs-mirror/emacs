/* 
TEST_HEADER
 id = $HopeName$
 summary = destroy root twice
 language = c
 link = myfmt.o testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_root_t root;
 mps_addr_t roottable[10];

 cdie(mps_space_create(&space), "create space");

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_AMBIG, 0,
   roottable, sizeof(mps_addr_t[10])), "create root");

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_root_destroy(root);
 comment("Destroyed root again.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

