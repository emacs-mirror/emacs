/* fmtscheme.h: SCHEME OBJECT FORMAT INTERFACE
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2014-01-15/nailboard/code/fmtdy.h#1 $
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#ifndef fmtscheme_h
#define fmtscheme_h

#include <stdio.h>
#include "mps.h"

typedef union obj_u *obj_t;
typedef obj_t (*entry_t)(obj_t env, obj_t op_env, obj_t operator, obj_t rands);
typedef unsigned long (*hash_t)(obj_t obj, mps_ld_t ld);
typedef int (*cmp_t)(obj_t obj1, obj_t obj2);

typedef int type_t;
enum {
  TYPE_PAIR,
  TYPE_INTEGER,
  TYPE_SYMBOL,
  TYPE_SPECIAL,
  TYPE_OPERATOR,
  TYPE_STRING,
  TYPE_PORT,
  TYPE_PROMISE,
  TYPE_CHARACTER,
  TYPE_VECTOR,
  TYPE_TABLE,
  TYPE_BUCKETS,
  TYPE_FWD2,            /* two-word forwarding object */
  TYPE_FWD,             /* three words and up forwarding object */
  TYPE_PAD1,            /* one-word padding object */
  TYPE_PAD              /* two words and up padding object */
};

typedef struct type_s {
  type_t type;
} type_s;

typedef struct pair_s {
  type_t type;			/* TYPE_PAIR */
  obj_t car, cdr;		/* first and second projections */
} pair_s;

typedef struct symbol_s {
  type_t type;			/* TYPE_SYMBOL */
  size_t length;		/* length of symbol string (excl. NUL) */
  char string[1];		/* symbol string, NUL terminated */
} symbol_s;

typedef struct integer_s {
  type_t type;			/* TYPE_INTEGER */
  long integer;			/* the integer */
} integer_s;

typedef struct special_s {
  type_t type;			/* TYPE_SPECIAL */
  char *name;			/* printed representation, NUL terminated */
} special_s;

typedef struct operator_s {
  type_t type;			/* TYPE_OPERATOR */
  char *name;			/* printed name, NUL terminated */
  entry_t entry;		/* entry point -- see eval() */
  obj_t arguments, body;	/* function arguments and code */
  obj_t env, op_env;		/* closure environments */
} operator_s;

typedef struct string_s {
  type_t type;			/* TYPE_STRING */
  size_t length;		/* number of chars in string */
  char string[1];		/* string, NUL terminated */
} string_s;

typedef struct port_s {
  type_t type;			/* TYPE_PORT */
  obj_t name;			/* name of stream */
  FILE *stream;
} port_s;

typedef struct character_s {
  type_t type;			/* TYPE_CHARACTER */
  char c;			/* the character */
} character_s;

typedef struct vector_s {
  type_t type;			/* TYPE_VECTOR */
  size_t length;		/* number of elements */
  obj_t vector[1];		/* vector elements */
} vector_s;

typedef struct table_s {
  type_t type;                  /* TYPE_TABLE */
  hash_t hash;                  /* hash function */
  cmp_t cmp;                    /* comparison function */
  mps_ld_s ld;                  /* location dependency */
  obj_t buckets;                /* hash buckets */
} table_s;

typedef struct buckets_s {
  type_t type;                  /* TYPE_BUCKETS */
  size_t length;                /* number of buckets */
  size_t used;                  /* number of buckets in use */
  size_t deleted;               /* number of deleted buckets */
  struct bucket_s {
    obj_t key, value;
  } bucket[1];                  /* hash buckets */
} buckets_s;


/* fwd2, fwd, pad1, pad -- MPS forwarding and padding objects */

typedef struct fwd2_s {
  type_t type;                  /* TYPE_FWD2 */
  obj_t fwd;                    /* forwarded object */
} fwd2_s;

typedef struct fwd_s {
  type_t type;                  /* TYPE_FWD */
  obj_t fwd;                    /* forwarded object */
  size_t size;                  /* total size of this object */
} fwd_s;

typedef struct pad1_s {
  type_t type;                  /* TYPE_PAD1 */
} pad1_s;

typedef struct pad_s {
  type_t type;                  /* TYPE_PAD */
  size_t size;                  /* total size of this object */
} pad_s;


typedef union obj_u {
  type_s type;			/* one of TYPE_* */
  pair_s pair;
  symbol_s symbol;
  integer_s integer;
  special_s special;
  operator_s operator;
  string_s string;
  port_s port;
  character_s character;
  vector_s vector;
  table_s table;
  buckets_s buckets;
  fwd2_s fwd2;
  fwd_s fwd;
  pad_s pad;
} obj_s;


/* structure macros */

#define TYPE(obj)	((obj)->type.type)
#define CAR(obj)	((obj)->pair.car)
#define CDR(obj)	((obj)->pair.cdr)
#define CAAR(obj)	CAR(CAR(obj))
#define CADR(obj)	CAR(CDR(obj))
#define CDAR(obj)	CDR(CAR(obj))
#define CDDR(obj)	CDR(CDR(obj))
#define CADDR(obj)	CAR(CDDR(obj))
#define CDDDR(obj)	CDR(CDDR(obj))
#define CDDAR(obj)	CDR(CDAR(obj))
#define CADAR(obj)	CAR(CDAR(obj))


extern obj_t scheme_make_bool(int condition);
extern obj_t scheme_make_pair(obj_t car, obj_t cdr);
extern obj_t scheme_make_integer(long integer);
extern obj_t scheme_make_symbol(size_t length, char string[]);
extern obj_t scheme_make_string(size_t length, char string[]);
extern obj_t scheme_make_special(char *string);
extern obj_t scheme_make_operator(char *name, entry_t entry, obj_t arguments,
                                  obj_t body, obj_t env, obj_t op_env);
extern obj_t scheme_make_port(obj_t name, FILE *stream);
extern obj_t scheme_make_character(char c);
extern obj_t scheme_make_vector(size_t length, obj_t fill);
extern obj_t scheme_make_buckets(size_t length);
extern obj_t scheme_make_table(size_t length, hash_t hashf, cmp_t cmpf);
extern void scheme_fmt(mps_fmt_t *fmt);

extern void test_main(void);

#endif /* fmtscheme_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
