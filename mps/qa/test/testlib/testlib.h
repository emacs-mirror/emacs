/* $HopeName: MMQA_harness!testlib:testlib.h(trunk.4) $
test_lib.h
   various handy things for reporting errors &c
*/

#ifndef test_lib_h
#define test_lib_h

#include "mps.h"
#include "versind.h"
#include "platform.h"

/* Give textual description of mps error code */

const char *err_text(mps_res_t err);

/* pass, fail: write result=pass/fail to stdout and
   exit (0 or 1 as appropriate)
*/

void pass(void);
void fail(void);

/* report: print variable and value to stdout
*/

void report_res(const char *str, mps_res_t res);
void report(const char *str, const char *format, ...);
void vreport(const char *str, const char *format, va_list args);

/* adie: print text and err code to stdout by calling error
    die: as above, but if err is MPS_RES_OK do nothing
   cdie: print text and err code to stdout as comment, or as error
         if err isn't MPS_RES_OK
*/

void cdie(mps_res_t err, const char *str);
void  die(mps_res_t err, const char *str);
void adie(mps_res_t err, const char *str);

/* Prints text to stdout */

void comment(const char *format, ...);
void vcomment(const char *format, va_list args);

/* Prints text to stdout if cond is true */

void commentif(int cond, const char *format, ...);

/* Prints text to stdout and aborts */

void error(const char *format, ...);
void verror(const char *format, va_list args);

/* If exp is false, prints text to stdout and aborts */

void asserts(int expr, const char *format, ...);

/* Easy way of entering the trampoline, for when you don't
   want to pass any information in or out. If you have a
   function void foo(void), just go easy_tramp(foo).
*/

void easy_tramp(void (*f)(void));

/* Pause for n seconds
*/

void pause(unsigned long);

/* Random number from 0 to x-1
*/

unsigned long ranint(unsigned long limit);
unsigned long ranrange(unsigned long min, unsigned long max);

#endif
