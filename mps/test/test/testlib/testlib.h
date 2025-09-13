/* $Id$
test_lib.h
   various handy things for running tests, reporting errors &c.
*/

#ifndef testlib_h
#define testlib_h

#include <stdlib.h>

#include "mmqasym.h"
#ifndef MMQA_HEADER_mps
#error Header file mps.h not found
#endif
#include "mps.h"
#include "mpslib.h"
#include "mpstd.h"
#include "versind.h"
#include "platform.h"

/* Give textual description of mps error code */

/* Need typedef to avoid bug in MacOS X Public Beta */
typedef const char *constant_string_t;
constant_string_t err_text(mps_res_t err);


/* finish, fail: write completed=yes/no to stdout and
   exit (0 or 1 as appropriate).
   pass is provided for compatibility with old tests, and is
   a synonym for finish.
*/

void pass(void);
void finish(void);
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


/* Abort. Tests should use error rather than this. */

void myabort(void);


/* Run test case with MMQA assertion handler installed. */
typedef void (*mmqa_test_function_t)(void *stack_pointer);
void run_test(mmqa_test_function_t test);


/* Pause for n seconds
*/

void mmqa_pause(unsigned long);


/* Random number from 0 to x-1
*/

unsigned long ranint(unsigned long limit);
unsigned long ranrange(unsigned long min, unsigned long max);


/* stuff for running event logs
*/

typedef union log_event log_event;

typedef int event_type;

enum {EVENT_ALLOC, EVENT_FREE};

struct event_alloc {
 event_type type;
 unsigned int id;
 size_t size;
};

struct event_free {
 event_type type;
 unsigned int id;
};

union log_event {
 event_type type;
 struct event_alloc alloc;
 struct event_free  free;
};

int read_event(log_event*);


/* The MPS doesn't provide this useful function
*/

size_t arena_committed_and_used(mps_arena_t);


#ifdef MMQA_SYMBOL_mps_arena_t

mps_res_t mmqa_arena_create(mps_arena_t *arena_p,
 mps_arena_class_t arena_class, size_t chunk_size, size_t limit_size);

#endif


/* time-based queue
   (use for killing objects at the right time)
*/

typedef struct {
 unsigned long time;
 void *ref;
} TQElt;

typedef struct {
 unsigned long size;
 unsigned long used;
 TQElt *element;
}* TimeQueue;

void TQInit(TimeQueue, TQElt*, long int);

unsigned long TQSize(TimeQueue);
unsigned long TQCount(TimeQueue);
int TQEmpty(TimeQueue);
int TQFull(TimeQueue);

unsigned long TQTime(TimeQueue);
void *TQElement(TimeQueue);

void *TQPop(TimeQueue);
void TQAdd(TimeQueue, void *, unsigned long);

void TQWarp(TimeQueue, unsigned long);


#endif /* testlib_h */
