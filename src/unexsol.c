/* Trivial unexec for Solaris.  */

#include <config.h>
#include <stdlib.h>
#include <dlfcn.h>

#include "lisp.h"

int
unexec (char *new_name, char *old_name, unsigned int data_start,
        unsigned int bss_start, unsigned int entry_address)
{
  if (dldump (0, new_name, RTLD_MEMORY))
    report_file_error ("Cannot unexec", Fcons (build_string (new_name), Qnil));

  return 0;
}
