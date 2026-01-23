
/* $Id$
ephfmt.h
   a format for ephemerons
*/

#ifndef ephfmt_h
#define ephfmt_h

#include "testlib.h"

enum type
{
  TYPE_PAIR,
  TYPE_WEAK_PAIR,
  TYPE_WEAK_OR_PAIR,
  TYPE_WEAK_AND_PAIR,
  TYPE_STRING,
  TYPE_FWD,
  TYPE_PAD
};

union header
{
  enum type type;
  size_t padding;
};

struct pad
{
  union header header;
  size_t size; /* size (including header) of this object */
};

typedef union object* oop;
typedef struct memory_manager* mmp;

struct fwd
{
  union header header;
  size_t size; /* size (including header) of this object */
  oop fwd;
};

struct pair
{
  union header header;
  oop car, cdr;
};

struct weak_pair
{
  union header header;
  oop key, value;
};

struct string
{
  union header header;
  size_t length;
  char data[1];
};

union object
{
  union header header;
  struct pad pad;
  struct fwd fwd;
  struct pair pair;
  struct weak_pair weak_pair;
  struct string string;
};

enum
{
  ROOT_COUNT = 100
};

struct memory_manager
{
  mps_ap_t amc_ap;
  mps_ap_t eph_ap;
  mps_arena_t arena;
  oop roots[ROOT_COUNT];
};

mps_fmt_t eph_fmt(mps_arena_t arena);

oop make_pair(mps_ap_t ap, oop car, oop cdr);
oop make_weak_pair(mps_ap_t ap, oop key, oop value);
oop make_weak_or_pair(mps_ap_t ap, oop key, oop value);
oop make_weak_and_pair(mps_ap_t ap, oop key, oop value);
oop make_string(mps_ap_t ap, size_t length, const char string[]);
oop string_from_cstr(mps_ap_t ap, const char* cstr);
void check_string(oop s, const char* data);

typedef void (*eph_test_fun)(mmp, void* closure);
void run_eph_test(eph_test_fun, void* closure);

#endif
