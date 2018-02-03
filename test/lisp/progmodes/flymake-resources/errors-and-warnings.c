/* Flymake should notice an error on the next line, since
   that file has at least one warning.*/
#include "some-problems.h"
/* But not this one */
#include "no-problems.h"

int main()
{
  char c = 1000; /* a note and a warning */
  int bla;
  char c; if (bla == (void*)3); /* an error, and two warnings */
  return c;
}
