#ifndef _NT_STDALIGN_H_
#define _NT_STDALIGN_H_

/* This header has the necessary stuff from lib/stdalign.in.h, but
   avoids the need to have Sed at build time.  */

#include <stddef.h>
#if defined __cplusplus
   template <class __t> struct __alignof_helper { char __a; __t __b; };
# define _Alignof(type) offsetof (__alignof_helper<type>, __b)
#else
# define _Alignof(type) offsetof (struct { char __a; type __b; }, __b)
#endif
#define alignof _Alignof

#if __GNUC__
# define _Alignas(a) __attribute__ ((__aligned__ (a)))
#elif 1300 <= _MSC_VER
# define _Alignas(a) __declspec (align (a))
#endif
#ifdef _Alignas
# define alignas _Alignas
#endif

#endif	/* _NT_STDALIGN_H_ */
