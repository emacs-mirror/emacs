/* $Id$
some useful functions for testing the MPS */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <time.h>
#include "testlib.h"

#ifdef MMQA_HEADER_mpsacl
#include "mpsacl.h"
#endif
#ifdef MMQA_HEADER_mpsavm
#include "mpsavm.h"
#endif


/* err_text
   mps_res_t -> textual description
   uses case rather than array lookup 'cos we don't want to
   rely on the representation, which might change
*/

constant_string_t err_text(mps_res_t err)
{
 switch (err) {
  case MPS_RES_OK: return "OK";
  case MPS_RES_FAIL: return "FAIL";
  case MPS_RES_RESOURCE: return "RESOURCE";
  case MPS_RES_MEMORY: return "MEMORY";
  case MPS_RES_LIMIT: return "LIMIT";
  case MPS_RES_UNIMPL: return "UNIMPL";
  case MPS_RES_IO: return "IO";
#ifdef MMQA_SYMBOL_MPS_RES_COMMIT_LIMIT
  case MPS_RES_COMMIT_LIMIT: return "COMMIT_LIMIT";
#endif
#ifdef MMQA_SYMBOL_MPS_RES_PARAM
  case MPS_RES_PARAM: return "PARAMETER";
#endif
 }
 asserts(0, "Unknown result code");
 return "*** Unknown result code ***";
}


/* finish, pass 
   report completed=yes and exit(0)
*/

void finish(void)
{
 report("completed", "yes");
 exit(0);
}


void pass(void)
{
 finish();
}


/* fail
   report completed=no and exit(1)
*/   

void fail(void)
{
 report("completed", "no");
 exit(1);
}

/* report
   write a var=value line on stdout
*/


void report_res(const char *str, mps_res_t res)
{
 report(str, err_text(res));
}


void report(const char *str, const char *format, ...)
{
 va_list args;

 va_start(args, format);
 vreport(str, format, args);
 va_end(args);
}


void vreport(const char *str, const char *format, va_list args)
{
 fprintf(stdout, "!%s=", str);
 vfprintf(stdout, format, args);
 fprintf(stdout, "\n");
 fflush(stdout);
}


/* cdie
   report mps result code, and exit if it's not ok
*/

void cdie(mps_res_t err, const char *str)
{
 if (err != MPS_RES_OK) {
  error("%s: %s\n", str, err_text(err));
 }
 else comment("%s: OK", str);
}


/* die
   check mps result code it ok; it not, report and exit 
*/

void die(mps_res_t err, const char *str)
{
 if (err != MPS_RES_OK) {
  error("%s: %s\n", str, err_text(err));
 }
}


/* adie
   report mps result code as error, whatever it is
*/

void adie(mps_res_t err, const char *str)
{
 error("%s: %s\n", str, err_text(err));
}


/* comment
   print comment on stdout
*/

void comment(const char *format, ...)
{
 va_list args;

 va_start(args, format);
 vcomment(format, args);
 va_end(args);
}


void vcomment(const char *format, va_list args)
{
 fprintf(stdout, "%% ");
 vfprintf(stdout, format, args);
 fprintf(stdout, "\n");
 fflush(stdout);
}


/* commentif(boolean, "comment")
*/

void commentif(int cond, const char *format, ...)
{
 va_list args;

 if (cond) {
  va_start(args, format);
  vcomment(format, args);
  va_end(args);
 }
}


/* error
   print error on stdout and exit
*/

void error(const char *format, ...)
{
 va_list args;

 va_start(args, format);
 verror(format, args);
 va_end(args);
}


void myabort(void) {
 exit(EXIT_FAILURE);
}


void verror(const char *format, va_list args)
{
 fprintf(stdout, "%% ERROR \n!error=true\n");
 fprintf(stdout, "!errtext=");
 vfprintf(stdout, format, args);
 fprintf(stdout, "\n");
 fflush(stdout);
 myabort();
}


/* asserts(1<0, "Axiom violation.");
   assert, with textual message instead of expr printed
*/

void asserts(int expr, const char *format, ...)
{
 va_list args;

 if (!expr) {
  va_start(args, format);
  fprintf(stdout, "%% ASSERTION FAILED \n!assert=true\n");
  fprintf(stdout, "!asserttext=");
  vfprintf(stdout, format, args);
  fprintf(stdout, "\n");
  fflush(stdout);
  va_end(args);
  myabort();
 }
}


/* routines for easy use of the MPS */


/* my own assertion handler, insalled by easy_tramp
*/

static void mmqa_assert_handler(const char *cond, const char *id,
                              const char *file, unsigned line)
{
 if (line == 0) {
/* assertion condition contains condition, file, line, separated
   by newline characters
*/
  const char *val;

  comment("MPS ASSERTION FAILURE");
  report("assert", "true");
  report("assertid", "<no id supplied>");

  fprintf(stdout, "!assertcond=");
  val = cond;
  while (*val != '\n') {
   fputc(*val, stdout);
   val++;
  }
  fputc('\n', stdout);
  val++;
  fprintf(stdout, "!assertfile=");
  while (*val != '\n') {
   fputc(*val, stdout);
   val++;
  }
  fputc('\n', stdout);
  val++;
  report("assertline", val);
  fflush(stdout);
 } else {
  comment("MPS ASSERTION FAILURE");
  report("assert", "true");
  report("assertid", id);
  report("assertfile", file);
  report("assertline", "%u", line);
  report("assertcond", cond);
 }
 myabort();
}

/* Installable assertion handler (suitable for installing via
   mps_lib_assert_fail_install). */

static void mmqa_lib_fail(const char *file, unsigned line, const char *message)
{
  mmqa_assert_handler(message, "<none>", file, line);
}

/* easy_tramp
   simplified trampoline, for those who don't want to
   pass anything into or out of it -- it takes
   a function with no arguments returning nothing
*/

static void *call_f(void *p, size_t s)
{
 void (**f)(void) = p;

#ifdef MMQA_DEFINED_mps_lib_assert_fail_install
 mps_lib_assert_fail_install(mmqa_lib_fail);
#endif

 (**f)(); 
 return NULL;
}


#if defined(MMQA_PROD_epcore)

static void easy_tramp2(void (*f)(void))
{
 call_f(&f, (size_t) 0);
}

#else

static void easy_tramp2(void (*f)(void))
{
 void *result;

 mps_tramp(&result, call_f, &f, (size_t)0);
}

#endif


#ifdef MPS_OS_W3

void easy_tramp(void (*f)(void))
{
 __try {
  easy_tramp2(f);
 } __except(mySEHFilter(GetExceptionInformation())) {
  error("Exception handler messed up.");
 }
}

#else

void easy_tramp(void (*f)(void))
{
 easy_tramp2(f);
}

#endif


/* mmqa_pause(n) waits for n seconds
*/

void mmqa_pause(unsigned long sec)
{
 clock_t c;

 c = clock();
 if (c != -1) {
  c = c + sec*CLOCKS_PER_SEC;
  while (clock() < c);
 }
}


/* nabbed from "ML for the Working Programmer"
 * Originally from:
 * Stephen K Park & Keith W Miller (1988). Random number generators:
 * good ones are to find.  Communications of the ACM, 31:1192-1201
 */

static unsigned long rnd(void)
{
  static unsigned long seed = 1;
  double s;
  s = seed;
  s *= 16807.0;
  s = fmod(s, 2147483647.0);  /* 2^31 - 1 */
  seed = (unsigned long)s;
  return seed;
}

unsigned long ranint(unsigned long x)
{
 unsigned long y;
 unsigned long max;

 asserts(x>0, "ranint needs positive parameter");

 if (x==1) return 0;

 max = (2147483647/x)*x;

 do y = rnd();
 while (y>max-1);

 return y%x;
}


unsigned long ranrange(unsigned long min, unsigned long max)
{
 return min+ranint(max-min);
}


/* Event log running

   Event logs contain lines of the form
   A <id> <size>     -- alloc
   F <id> <size>     -- free
*/

int read_event(log_event* event)
{
 int r;
 unsigned long a, b;
 r = scanf("A%lu%lu\n", &a, &b);
 if (r == EOF) {
  return 0;
 } else if (r > 0) {
/*  comment("A %lu %lu", a, b);
*/
  asserts(r == 2, "bad alloc event");
  event->type = EVENT_ALLOC;
  event->alloc.id = a;
  event->alloc.size = b;
  return 1;
 }
 r = scanf("F%lu\n", &a);
 if (r == EOF) {
  return 0;
 } else if (r > 0) {
/*
  comment("F %lu", a);
*/
  asserts(r == 1, "bad free event");
  event->type = EVENT_FREE;
  event->free.id = a;
  return 1;
 } else {
  comment("unknown event: ");
  for (r = 0; r < 5; r++) {
   comment("%d", getchar());
  }
  error("unknown event");
 }
 /* to make compiler happy */
 return 0;
}


/* A useful function the MPS doesn't provide */

size_t arena_committed_and_used(mps_arena_t arena)
{
 return mps_arena_committed(arena)-mps_arena_spare_committed(arena);
}


/* A function to make it easy to parameterise tests by arena
   class 
*/

#ifdef MMQA_SYMBOL_mps_arena_t

mps_res_t mmqa_arena_create(mps_arena_t *arena_p,
                            mps_arena_class_t arena_class,
                            size_t chunk_size, size_t limit_size)
{
 mps_res_t res;
 mps_arena_t arena;
 if ( 0
#ifdef MMQA_DEFINED_mps_arena_class_cl
    || arena_class == mps_arena_class_cl()
#endif
    ) {
  void *block;
  block = malloc(limit_size);
  if(block == NULL) {
   return MPS_RES_MEMORY;
  }
  res = mps_arena_create(&arena, arena_class, limit_size, block);
 } else if ( 0
#ifdef MMQA_DEFINED_mps_arena_class_vm
     || arena_class == mps_arena_class_vm()
#endif
#ifdef MMQA_DEFINED_mps_arena_class_vmnz
     || arena_class == mps_arena_class_vmnz()
#endif
     ) {
  res = mps_arena_create(&arena, arena_class, chunk_size);
  if (res != MPS_RES_OK) {
   return res;
  }
#ifdef MMQA_DEFINED_mps_arena_commit_limit_set
  res = mps_arena_commit_limit_set(arena, limit_size);
  if (res != MPS_RES_OK) {
   mps_arena_destroy(arena);
  }
#endif
 } else {
  error("Unknown arena class.");
  res = MPS_RES_OK;
 }
 if (res == MPS_RES_OK) {
  *arena_p = arena;
 }
 return res;
}
 
#endif


/* TimeQueue
   s are implemented as heaps, stored in arrays. First array element
   used to keep track of size, how much used, &c. Rest of elements are
   entries.
*/

void TQInit(TimeQueue TQ, TQElt* element, long int size)
{
 TQ->size = size;
 TQ->used = 0;
 TQ->element = element;
}

unsigned long TQSize(TimeQueue TQ)
{
 return TQ->size;
}

unsigned long TQCount(TimeQueue TQ)
{
 return TQ->used;
}

int TQEmpty(TimeQueue TQ)
{
 return (TQ->used == 0);
}

int TQFull(TimeQueue TQ)
{
 return (TQ->used == TQ->size);
}

unsigned long TQTime(TimeQueue TQ)
{
 asserts(!TQEmpty(TQ), "TQTime called on empty TimeQueue");
 return (TQ->element[0].time);
}

void *TQElement(TimeQueue TQ)
{
 asserts(!TQEmpty(TQ), "TQElement called on empty TimeQueue");
 return (TQ->element[0].ref);
}


void *TQPop(TimeQueue TQ)
{
 unsigned long i, c, s;

 asserts(!TQEmpty(TQ), "TQPop called on empty TimeQueue");

 TQ->used -= 1;
 s = TQ->used;
 i = 0;
 while (1) {
  c = (2*i)+1;
  if (c >= s-1) break;
  if (TQ->element[c].time > TQ->element[c+1].time) {
   c+=1;
  }
  if (TQ->element[c].time < i) {
   TQ->element[i].time = TQ->element[c].time;
   TQ->element[i].ref  = TQ->element[c].ref;
   i = c;
  } else {
   break;
  }
 }
 return NULL;
}
