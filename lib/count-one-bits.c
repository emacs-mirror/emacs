#include <config.h>
#define COUNT_ONE_BITS_INLINE _GL_EXTERN_INLINE
#include "count-one-bits.h"

#if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)
int popcount_support = -1;
#endif
