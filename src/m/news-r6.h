/* news-risc6.h is for the "RISC News", OS version 6.  */
/* This is in the public domain.  */

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / 256.0)

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Alter some of the options used when linking.  */

/*#define C_DEBUG_SWITCH -g*/
#define C_DEBUG_SWITCH -O -Olimit 2000
#ifdef __GNUC__
#define LD_SWITCH_MACHINE -g -Xlinker -D -Xlinker 800000
#else /* !__GNUC__ */
/*#define LD_SWITCH_MACHINE -D 800000 -g*/
#define LD_SWITCH_MACHINE -D 800000
#endif /* !__GNUC__ */
#define LIBS_MACHINE -lmld
#define LIBS_TERMCAP -lcurses

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << (BITS_PER_INT-VALBITS)) >> (BITS_PER_INT-VALBITS))

#define XSET(var, type, ptr)						\
  ((var) =								\
   ((int)(type) << VALBITS)						\
   + (((unsigned) (ptr) << (BITS_PER_INT-VALBITS)) >> (BITS_PER_INT-VALBITS)))

#define XUNMARK(a)							\
  ((a) =								\
   (((unsigned)(a) << (BITS_PER_INT-GCTYPEBITS-VALBITS))			\
    >> (BITS_PER_INT-GCTYPEBITS-VALBITS)))
