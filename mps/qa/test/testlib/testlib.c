/* $HopeName: MMQA_harness!testlib:testlib.c(trunk.9) $
some useful functions for testing the MPS */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <time.h>
#include "testlib.h"

/* err_text
   mps_res_t -> textual description
   uses case rather than array lookup 'cos we don't want to
   rely on the representation, which might change
*/

const char *err_text(mps_res_t err)
{
 switch (err)
 {
  case MPS_RES_OK: return "OK";
  case MPS_RES_FAIL: return "FAIL";
  case MPS_RES_RESOURCE: return "RESOURCE";
  case MPS_RES_MEMORY: return "MEMORY";
  case MPS_RES_LIMIT: return "LIMIT";
  case MPS_RES_UNIMPL: return "UNIMPL";
  case MPS_RES_IO: return "IO";
 }
 asserts(0, "Unknown result code");
 return "*** Unknown result code ***";
}

/* pass 
   report result=pass and exit(0)
*/

void pass(void)
{
 report("result", "pass");
 exit(0);
}

/* fail
   report result=fail and exit(1)
*/   

void fail(void)
{
 report("result", "fail");
 exit(1);
}

/* report
   write a var=value line on stdout
*/

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
 fflush(NULL);
}

/* cdie
   report mps result code, and exit if it's not ok
*/

void cdie(mps_res_t err, const char *str)
{
 if (err != MPS_RES_OK)
 {
  error("%s: %s\n", str, err_text(err));
 }
 else comment("%s: OK", str);
}

/* die
   check mps result code it ok; it not, report and exit 
*/

void die(mps_res_t err, const char *str)
{
 if (err != MPS_RES_OK)
 {
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
 fflush(NULL);
}

/* commentif(boolean, "comment")
*/

void commentif(int cond, const char *format, ...)
{
 va_list args;

 if (cond)
 {
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

static void myabort(void) {
 exit(EXIT_FAILURE);
}

void verror(const char *format, va_list args)
{
 fprintf(stdout, "%% ERROR \n!error=true\n");
 fprintf(stdout, "!errtext=");
 vfprintf(stdout, format, args);
 fprintf(stdout, "\n");
 fflush(NULL);
 myabort();
}

/* asserts(1=0, "Axiom violation.");
   assert, with textual message instead of expr printed
*/

void asserts(int expr, const char *format, ...)
{
 va_list args;

 if (!expr)
 {
  va_start(args, format);
  fprintf(stdout, "%% ASSERTION FAILED \n!assert=true\n");
  fprintf(stdout, "!asserttext=");
  vfprintf(stdout, format, args);
  fprintf(stdout, "\n");
  fflush(NULL);
  va_end(args);
  myabort();
 }
}

/* routines for easy use of the MPS */

/* my own assertion handler, insalled by easy_tramp
*/

static void my_assert_handler(const char *cond, const char *id,
                       const char *file, unsigned line) {
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
  fflush(NULL);
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

/* easy_tramp
   simplified trampoline, for those who don't want to
   pass anything into or out of it -- it takes
   a function with no arguments returning nothing
*/

static void *call_f(void *p, size_t s)
{
 void (**f)(void) = p;

 mps_assert_install(my_assert_handler);

 (**f)(); 
 return NULL;
}

#if defined(MMQA_VERS_SW)

void easy_tramp(void (*f)(void)) {
 call_f(&f, (size_t) 0);
}

#else

void easy_tramp(void (*f)(void)) {
 void *result;

 mps_tramp(&result, call_f, &f, (size_t)0);
}

#endif

/* pause(n) waits for n seconds
*/

void pause(unsigned long sec) {
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

