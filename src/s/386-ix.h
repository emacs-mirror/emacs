/* Interactive 386/ix.  */

#include "usg5-3.h"

#define BROKEN_TIOCGETC

/* There are some reports that the following is needed
   with some version of this system.
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -linet
*/

/* This is said to be needed as a result of having _insque rather
   than insque in -loldX.  This may not always be the right thing.  */
#define WRONG_NAME_INSQUE

/* arch-tag: c2fc2d64-46fd-4a87-933b-85eff622829c
   (do not change this comment) */
