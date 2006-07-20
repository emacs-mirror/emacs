/* Trivial unexec for Solaris.  */

#include <config.h>
#include <stdlib.h>
#include <dlfcn.h>

#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"

int
unexec (char *new_name, char *old_name, unsigned int data_start,
        unsigned int bss_start, unsigned int entry_address)
{
  Lisp_Object data;
  Lisp_Object errstring;

  if (! dldump (0, new_name, RTLD_MEMORY))
    return 0;

  data = Fcons (build_string (new_name), Qnil);
  synchronize_system_messages_locale ();
  errstring = code_convert_string_norecord (build_string (dlerror ()),
					    Vlocale_coding_system, 0);

  xsignal (Qfile_error,
	   Fcons (build_string ("Cannot unexec"), Fcons (errstring, data)));
}

/* arch-tag: d8ff72b3-8198-4011-8ef5-011b12027f59
   (do not change this comment) */
