/* impl.c.dumper: Simple Event Dumper
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MM developers.
 *
 * .purpose: This is a simple tool to dump events as text.
 *
 * .trans: As a tool, it's allowed to depend on the ANSI C library.
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

typedef unsigned long Word;
typedef struct AddrStruct *Addr;

#include "eventcom.h"


#define RELATION(type, code, always, kind, format) \
  case Event ## type: \
    readEvent(#type, #format, header[0], header[1], header[2]); \
    break;


#define AVER(test) \
  if(test) do {} while(0); else error("AVER: " #test)


static char *prog;
static FILE *progin;


static void error (const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s: Error: ", prog);
  va_start(args, format);
  vfprintf(stderr, format, args);
  fputc('\n', stderr);
  va_end(args);
  exit(EXIT_FAILURE);
  assert(0);
}


#define PROCESS(ch, type, _length, printfFormat, cast) \
  case ch: { \
    type v; \
    size_t n = fread(&v, (_length), 1, progin); \
    if(n < 1) \
      error("Can't read data for format code >%c<", ch); \
    printf(printfFormat " ", (cast)v); \
    length -= (_length) / sizeof(Word); \
  } break;


static void readEvent(char *type, char *format, Word code, Word length,
		      Word cpuTime) {
  AVER(type != NULL);
  AVER(format != NULL);

  printf("%-20s ", type);

  for(; *format != '\0'; format++) {
    switch(*format) {
      PROCESS('A', Addr, sizeof(Addr), "0x%08lX", unsigned long)
      PROCESS('P', void *, sizeof(void *), "0x%08lX", unsigned long)
      PROCESS('U', unsigned, sizeof(unsigned),"%u", unsigned)
      PROCESS('W', Word, sizeof(Word),"%lu", Word)
      PROCESS('D', double, sizeof(double), "%f", double)
      case 'S': {
        size_t n;
        char *v;
        AVER(length > 0);
        v = malloc(length * sizeof(Word));
        if(v == NULL)
          error("Can't allocate string space %u", (unsigned)length);
        n = fread((void *)v, sizeof(Word), length, progin);
        if(n < 1)
          error("Can't read data for string");
        printf("%s  ", v);
        length = 0;
      } break;
      case '0': break;
      default:
        error("Unknown format >%c<", *format);
        break;
    }
  }
  putc('\n', stdout);

  AVER(length == 0);
}

       
int main(int argc, char *argv[]) {
  Word header[3];
  size_t arg = 1;

  prog = (argc >= 1 ? argv[0] : "unknown");

  /* parse options here [there aren't any] */

  do {
    if(argc <= 1) {
      progin = stdin;
    } else {
      char *filename = argv[arg];
      assert(filename != NULL);
      progin = fopen(filename, "rb");
      /* fopen returns NULL in error (ISO C 7.9.5.3) */
      if(progin == NULL) {
        error("Failed to open \"%s\".\n", filename);
      }
      ++arg;
    }
    while(!feof(progin)) {
      size_t n;
      n = fread(header, sizeof(Word), 3, progin);
      if(n < 3) {
	if(feof(progin))
	  continue;
	error("Can't read from input");
      }
     
      switch(header[0]) {
#include "eventdef.h"
      default:
	error("Unknown event code %08lX", header[0]);
      }
    }
  } while(arg < argc);

  return(0);
}
 
     


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
