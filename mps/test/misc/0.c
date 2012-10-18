/* 
TEST_HEADER
 id = $HopeName$
 summary = cause segv and see if has usual effect
 language = c
 link = testlib.o
OUTPUT_SPEC
 memoryerror = true
END_HEADER
*/

#include "mps.h"
#include "testlib.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 int *p;

 cdie(mps_space_create(&space), "create space");

 p = NULL;
 printf("%i", *p);
}

int main(void)
{
 void *m;
 stackpointer=&m;

 easy_tramp(test);
 return 0;
}

