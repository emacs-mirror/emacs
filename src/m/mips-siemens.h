#include "pyramid.h"


/* Don't use the ordinary -g for debugging in cc */

#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g

/* This system uses a slightly nonstandard variant of elf format.  */
#undef UNEXEC
#define UNEXEC unexelf.o
#define ELF_BSS_SECTION_NAME ".sbss"

#define BSTRING

#define NO_ARG_ARRAY

#undef BROKEN_FIONREAD

#define HAVE_ALLOCA
