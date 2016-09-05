/* 
TEST_HEADER
 id = $Id$
 summary = access location in last 4 bytes of address space.
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include <stdio.h>
#include "testlib.h"
#include "mpsavm.h"

void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 int *p;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), 64*1024uL*1024uL),
      "create arena");

 p = (int *)-4;
 putchar(*p);
}

int main(void)
{
 void *m;
 stackpointer=&m;

 easy_tramp(test);
 return 0;
}

