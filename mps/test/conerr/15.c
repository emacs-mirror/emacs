/* 
TEST_HEADER
 id = $Id$
 summary = destroy an uncreated pool
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

static void test(void)
{
 mps_pool_t pool = malloc(4096);
 mps_pool_destroy(pool);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}
