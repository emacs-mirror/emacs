/* 
TEST_HEADER
 id = $Id$
 summary = alloc in an uncreated pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Pool, pool)
END_HEADER
*/

#include <stdlib.h>

#include "testlib.h"
#include "mpscmvff.h"

static void test(void)
{
 mps_pool_t pool = malloc(4096);
 mps_addr_t obj;

 cdie(mps_alloc(&obj, pool, 152), "allocate");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}
