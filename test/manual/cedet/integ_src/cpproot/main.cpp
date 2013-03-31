/*
 * Main program for testing ede/cpp-root project.
 */

#include <cpprootsys.h>
#include "sppmacros.h"
#include "projincmacros.h"
#include "test.h"

int main(int argc, char *argv[])
{

  return 0;
}

#ifdef FEATURE1
int feature1()
{
  return 1;
}
#endif

#ifdef FEATURE2
int feature2()
{
  return 2;
}
#endif

#ifdef FEATURE3
int feature3()
{
  return 3;
}
#endif

#ifdef PROJMACRO_A
char projmacro_a()
{
  return 'a';
}
#endif

int generic_feature()
{
  return 0;
}
