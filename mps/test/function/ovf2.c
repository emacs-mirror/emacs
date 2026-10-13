/*
TEST_HEADER
 id = $Id$
 summary = tests allocating large objects
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

/*
  This test tries to find the largest possible size for a string.  This
  exposed a problem with pointer arithmetic overflow in mps_reserve.
*/

#include "ephfmt.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

enum cmp_result
{
  CMP_EQ,
  CMP_GT,
  CMP_LT
};

typedef enum cmp_result (*cmp_fn)(size_t i, void* closure);

static size_t binary_search(size_t start,
                            size_t end,
                            cmp_fn cmp,
                            void* closure)
{
  while (1) {
    size_t len = end - start;
    size_t mid = start + len / 2;
    if (len == 0)
      return SIZE_MAX;
    else
      switch (cmp(mid, closure)) {
        case CMP_EQ:
          asserts(mid != SIZE_MAX, "not implemented");
          return mid;
        case CMP_LT:
          end = mid;
          continue;
        case CMP_GT:
          start = mid + 1;
          continue;
      }
    asserts(0, "unreachable");
  }
}

static enum cmp_result try_size(size_t size, void* closure)
{
  mmp mm = closure;
  oop o;
  size_t size2 = size + 1;
  comment("trying size %zu (0x%zx)", size, size);
  o = alloc_bytes(mm->amc_ap, size);
  if (o == NULL)
    return CMP_LT;
  comment("trying size %zu (0x%zx)", size2, size2);
  o = alloc_bytes(mm->amc_ap, size2);
  return (o == NULL) ? CMP_EQ : CMP_GT;
}

static void test(mmp mm, void* closure)
{
  binary_search(0, PTRDIFF_MAX, try_size, mm);
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}
