/* impl.h.ossu: SUNOS ANSI COMPATABILITY HEADER
 *
 *  $HopeName: MMsrc!ossu.h(trunk.10) $
 *
 *  Copyright (C) 1997 Harlequin Limited.  All rights reserved.
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
