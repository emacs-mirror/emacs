/* impl.h.ossu: SUNOS ANSI COMPATABILITY HEADER
 *
 *  $Id$
 *
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 * .purpose: This header defines some things which are part of the ANSI
 * standard but missing from the C compiler / environment.
 *
 * .history: This header was imported from the MLWorks runtime system,
 * when it had the following id:
 *   src:OS:SunOS:ansi.h,v 1.2 1994/06/09 14:24:35 nickh
 */

#ifndef ossu_h
#define ossu_h

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <time.h>


/* stddef.h */

#ifndef offsetof        /* true for platform.sus8lc for example */
#define offsetof(ty,mem) ((size_t)((char*)&((ty*)0)->(mem) - (char*)0))
#endif


/* stdio.h things */

extern int fclose (FILE *stream);
extern int fflush (FILE *stream);
extern int fgetc (FILE *stream);
extern int ungetc (int c, FILE *stram);
extern int fputc (int c, FILE *stream);
extern int printf (const char *format, ...);
extern int fprintf (FILE *stream, const char *format, ...);
extern int vfprintf (FILE *stream, const char *format, va_list arg);
extern int vsprintf (char *s, const char *format, va_list arg);
extern int fputs (const char *s, FILE *stream);
extern int puts(const char *);
extern int fscanf (FILE *stream, const char *format, ...);
extern int sscanf (const char *s, const char *format, ...);
extern int fseek (FILE *stream, long int offset, int whence);
extern size_t fread (void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t fwrite (const void *ptr, size_t size, size_t nmemb,
                      FILE *stream);

/* these functions are used in the macro definitions of putc and getc
but not declared in stdio.h */

extern int _filbuf(FILE *stream);

extern int _flsbuf(unsigned char c, FILE *stream);


/* time.h things */

extern size_t strftime (char *s, size_t maxsize, const char *format,
                        const struct tm *timeptr);
extern time_t time (time_t *timer);
extern clock_t clock(void);


/* stdlib.h things */

extern int system(const char *string);
extern long strtol(const char *, char **, int);
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
/* @@@@ This doesn't do quite the right thing, but will get by */
#define strtoul(a,b,c) (unsigned long)strtol((a), (b), (c))


/* string.h things */

extern void *memset(void *, int, size_t);


#ifdef MPS_PF_SUS8LC
/* .hack.malloc: builder.lc (LCC) uses Sun's header files.  Sun's
 * stdlib.h is broken, as it has an incorrect declaration of malloc.
 * We fix that here in a very hacky way.
 */
#define malloc(x) (void *)malloc(x)
#endif /* MPS_PF_SUS8LC */


#endif /* ossu_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
