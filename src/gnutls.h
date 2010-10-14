/* GnuTLS glue for GNU Emacs.
   Copyright (C) 2010  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef EMACS_GNUTLS_DEFINED
#define EMACS_GNUTLS_DEFINED

#ifdef HAVE_GNUTLS
#include <gnutls/gnutls.h>

typedef enum
{
  /* Initialization stages.  */
  GNUTLS_STAGE_EMPTY = 0,
  GNUTLS_STAGE_CRED_ALLOC,
  GNUTLS_STAGE_FILES,
  GNUTLS_STAGE_INIT,
  GNUTLS_STAGE_PRIORITY,
  GNUTLS_STAGE_CRED_SET,

  /* Handshake stages.  */
  GNUTLS_STAGE_HANDSHAKE_CANDO = GNUTLS_STAGE_CRED_SET,
  GNUTLS_STAGE_TRANSPORT_POINTERS_SET,
  GNUTLS_STAGE_HANDSHAKE_TRIED,

  GNUTLS_STAGE_READY,
} gnutls_initstage_t;

#define GNUTLS_EMACS_ERROR_INVALID_TYPE GNUTLS_E_APPLICATION_ERROR_MIN

#define GNUTLS_INITSTAGE(proc) (XPROCESS (proc)->gnutls_initstage)

#define GNUTLS_PROCESS_USABLE(proc) (GNUTLS_INITSTAGE(proc) >= GNUTLS_STAGE_READY)

#define GNUTLS_LOG(level, max, string) if (level <= max) { gnutls_log_function (level, "(Emacs) " string); }

#define GNUTLS_LOG2(level, max, string, extra) if (level <= max) { gnutls_log_function2 (level, "(Emacs) " string, extra); }

int
emacs_gnutls_write (int fildes, struct Lisp_Process *proc, char *buf,
                    unsigned int nbyte);
int
emacs_gnutls_read (int fildes, struct Lisp_Process *proc, char *buf,
                   unsigned int nbyte);

extern void syms_of_gnutls (void);

#endif 

#endif
