/* 
TEST_HEADER
 id = $Id$
 summary = test that the mps header file is accepted by the compiler
 language = c
 link = testlib.o
END_HEADER
*/

#include "mps.h"
#include "testlib.h"

int main(void)
{
 pass();
 return 0;
}

