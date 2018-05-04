/* Respond to a save- or restore-cwd failure.
   This should never happen with Emacs.  */
#include <config.h>
#include "openat.h"
#include <stdlib.h>
void openat_save_fail (int errnum) { abort (); }
void openat_restore_fail (int errnum) { abort (); }
