/* scheme.c -- SCHEME INTERPRETER EXAMPLE FOR THE MEMORY POOL SYSTEM
 *
 * $Id$
 * Copyright (c) 2001-2012 Ravenbrook Limited.  See end of file for license.
 *
 * This is a toy interpreter for a subset of the Scheme programming
 * language <http://en.wikipedia.org/wiki/Scheme_%28programming_language%29>.
 * It is by no means the best or even the right way to implement Scheme,
 * but it serves the purpose of showing how the Memory Pool System can be
 * used as part of a programming language run-time system.
 *
 * To try it out, "make scheme" then
 *
 * $ ./scheme
 * (define (triangle n) (if (eqv? n 0) 0 (+ n (triangle (- n 1)))))
 * (define (church n f a) (if (eqv? n 0) a (church (- n 1) f (f a))))
 * (church 1000 triangle 0)
 *
 * This won't produce interesting results but it will cause garbage
 * collection cycles.  Note that there's never any waiting for the MPS.
 * THAT'S THE POINT.
 *
 * To find the code that's particularly related to the MPS, search for %%MPS.
 *
 *
 * MPS TO DO LIST
 * - make the symbol table weak to show how to use weak references
 * - add Scheme operators for talking to the MPS, forcing GC etc.
 * - cross-references to documentation
 * - make an mps_perror
 *
 * 
 * SCHEME TO DO LIST
 * - unbounded integers, other number types.
 * - do, named let.
 * - Quasiquote implementation is messy.
 * - Lots of library.
 * - \#foo unsatisfactory in read and print
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"


/* LANGUAGE EXTENSION */

#define unless(c)	if(!(c))
#define LENGTH(array)	(sizeof(array) / sizeof(array[0]))


/* CONFIGURATION PARAMETERS */


#define SYMMAX		((size_t)255)	/* max length of a symbol */
#define MSGMAX		((size_t)255)	/* max length of error message */
#define STRMAX		((size_t)255)	/* max length of a string */


/* DATA TYPES */


/* obj_t -- scheme object type
 *
 * obj_t is a pointer to a union, obj_u, which has members for
 * each scheme representation.
 *
 * The obj_u also has a "type" member.  Each representation
 * structure also has a "type" field first.  ANSI C guarantees
 * that these type fields correspond [section?].
 *
 * Objects are allocated by allocating one of the representation
 * structures and casting the pointer to it to type obj_t.  This
 * allows objects of different sizes to be represented by the
 * same type.
 *
 * To access an object, check its type by reading TYPE(obj), then
 * access the fields of the representation, e.g.
 *   if(TYPE(obj) == TYPE_PAIR) fiddle_with(CAR(obj));
 */

typedef union obj_u *obj_t;

typedef obj_t (*entry_t)(obj_t env, obj_t op_env, obj_t operator, obj_t rands);

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


/* fwd2, fwd, pad1, pad -- MPS forwarding and padding objects        %%MPS
 *
 * These object types are here to satisfy the MPS Format Protocol
 * for format variant "A".  See [type mps_fmt_A_s in the reference
 * manual](../../reference/index.html#mps_fmt_A_s).
 *
 * The MPS needs to be able to replace any object with a forwarding object
 * or [broken heart](http://www.memorymanagement.org/glossary/b.html#broken.heart)
 * and since the smallest normal object defined above is two words long,
 * we have two kinds of forwarding objects: FWD2 is exactly two words
 * long, and FWD stores a size for larger objects.  There are cleverer
 * ways to do this with bit twiddling, of course.
 *
 * The MPS needs to be able to pad out any area of memory that's a
 * multiple of the pool alignment.  We've chosen an single word alignment
 * for this interpreter, so we have to have a special padding object, PAD1,
 * for single words.  For padding multiple words we use PAD objects with a
 * size field.
 *
 * See obj_pad, obj_fwd etc. to see how these are used.
 */

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


/* GLOBAL DATA */


/* total -- total allocated bytes */

static size_t total;


/* symtab -- symbol table                                       %%MPS
 *
 * The symbol table is a hash-table containing objects of TYPE_SYMBOL.
 * When a string is "interned" it is looked up in the table, and added
 * only if it is not there.  This guarantees that all symbols which
 * are equal are actually the same object.
 *
 * The symbol table is simply a malloc'd array of obj_t pointers.  Since
 * it's outside the MPS and refers to objects we want the MPS to keep
 * alive, it must be declared to the MPS as a root.  Search for
 * occurrences of `symtab_root` to see how this is done.
 */

static obj_t *symtab;
static size_t symtab_size;
static mps_root_t symtab_root;


/* special objects                                              %%MPS
 *
 * These global variables are initialized to point to objects of
 * TYPE_SPECIAL by main.  They are used as markers for various
 * special purposes.
 *
 * These static global variable refer to object allocated in the `obj_pool`
 * and so they must also be declared to the MPS as roots.
 * See `globals_scan`.
 */

static obj_t obj_empty;		/* (), the empty list */
static obj_t obj_eof;		/* end of file */
static obj_t obj_error;		/* error indicator */
static obj_t obj_true;		/* #t, boolean true */
static obj_t obj_false;		/* #f, boolean false */
static obj_t obj_undefined;	/* undefined result indicator */
static obj_t obj_tail;          /* tail recursion indicator */


/* predefined symbols
 *
 * These global variables are initialized to point to interned
 * objects of TYPE_SYMBOL.  They have special meaning in the
 * Scheme language, and are used by the evaluator to parse code.
 */

static obj_t obj_quote;		/* "quote" symbol */
static obj_t obj_quasiquote;	/* "quasiquote" symbol */
static obj_t obj_lambda;	/* "lambda" symbol */
static obj_t obj_begin;		/* "begin" symbol */
static obj_t obj_else;		/* "else" symbol */
static obj_t obj_unquote;	/* "unquote" symbol */
static obj_t obj_unquote_splic;	/* "unquote-splicing" symbol */


/* error handler
 *
 * The error_handler variable is initialized to point at a
 * jmp_buf to which the "error" function longjmps if there is
 * any kind of error during evaluation.  It can be set up by
 * any enclosing function that wants to catch errors.  There
 * is a default error handler in `start`, in the read-eval-print
 * loop.  The error function also writes an error message
 * into "error_message" before longjmping, and this can be
 * displayed to the user when catching the error.
 *
 * [An error code should also be passed so that the error can
 *  be decoded by enclosing code.]
 */

static jmp_buf *error_handler = NULL;
static char error_message[MSGMAX+1];


/* MPS globals                                                  %%MPS
 *
 * These are global variables holding MPS values for use by the
 * interpreter.  In a more sophisticated integration some of these might
 * be thread local.  See `main` for where these are set up.
 *
 * `arena` is the global state of the MPS, and there's usually only one
 * per process.
 *
 * `obj_pool` is the memory pool in which the Scheme objects are allocated.
 * It is an instance of the Automatic Mostly Copying (AMC) pool class, which
 * is a general-purpose garbage collector for use when there are formatted
 * objects in the pool, but ambiguous references in thread stacks and
 * registers.
 *
 * `obj_ap` is an Allocation Point that allows fast in-line non-locking
 * allocation in a memory pool.  This would usually be thread-local, but
 * this interpreter is single-threaded.  See `make_pair` etc. for how this
 * is used with the reserve/commit protocol.
 */

static mps_arena_t arena;       /* the arena */
static mps_pool_t obj_pool;     /* pool for ordinary Scheme objects */
static mps_ap_t obj_ap;         /* allocation point used to allocate objects */


/* SUPPORT FUNCTIONS */


/* error -- throw an error condition
 *
 * The "error" function takes a printf-style format string
 * and arguments, writes the message into error_message and
 * longjmps to *error_handler.  There must be a setjmp at
 * the other end to catch the condition and display the
 * message.
 */

static void error(char *format, ...)
{
  va_list args;

  va_start(args, format);
  vsnprintf(error_message, sizeof error_message, format, args);
  va_end(args);

  if (error_handler) {
    longjmp(*error_handler, 1);
  } else {
    fprintf(stderr, "Fatal error during initialization: %s\n",
            error_message);
    abort();
  }
}


/* make_* -- object constructors                                %%MPS
 *
 * Each object type has a function here that allocates an instance of
 * that type.
 *
 * These functions illustrate the two-phase MPS Allocation Point
 * Protocol with `reserve` and `commmit`.  This protocol allows very fast
 * in-line allocation without locking, but there is a very tiny chance that
 * the object must be re-initialized.  In nearly all cases, however, it's
 * just a pointer bump.
 *
 * NOTE: We could reduce duplicated code here using macros, but we want to
 * write these out because this is code to illustrate how to use the
 * protocol.
 */

#define ALIGNMENT sizeof(mps_word_t)

#define ALIGN(size) \
  (((size) + ALIGNMENT - 1) & ~(ALIGNMENT - 1))

static obj_t make_pair(obj_t car, obj_t cdr)
{
  obj_t obj;
  mps_addr_t addr;
  /* When using the allocation point protocol it is up to the client
     code to ensure that all requests are for aligned sizes, because in
     nearly all cases `mps_reserve` is just an increment to a pointer. */
  size_t size = ALIGN(sizeof(pair_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_pair");
    obj = addr;
    obj->pair.type = TYPE_PAIR;
    CAR(obj) = car;
    CDR(obj) = cdr;
    /* `mps_commit` returns false on very rare occasions (when an MPS epoch
       change has happened since reserve) but in those cases the object must
       be re-initialized.  It's therefore important not to do anything you
       don't want to repeat between reserve and commit.  Also, the shorter
       the time between reserve and commit, the less likely commit is to
       return false. */
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(pair_s);
  return obj;
}

static obj_t make_integer(long integer)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(sizeof(integer_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_integer");
    obj = addr;
    obj->integer.type = TYPE_INTEGER;
    obj->integer.integer = integer;
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(integer_s);
  return obj;
}

static obj_t make_symbol(size_t length, char string[])
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(offsetof(symbol_s, string) + length+1);
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_symbol");
    obj = addr;
    obj->symbol.type = TYPE_SYMBOL;
    obj->symbol.length = length;
    memcpy(obj->symbol.string, string, length+1);
  } while(!mps_commit(obj_ap, addr, size));
  total += size;
  return obj;
}

static obj_t make_string(size_t length, char string[])
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(offsetof(string_s, string) + length+1);
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_string");
    obj = addr;
    obj->string.type = TYPE_STRING;
    obj->string.length = length;
    if (string)
      memcpy(obj->string.string, string, length+1);
  } while(!mps_commit(obj_ap, addr, size));
  total += size;
  return obj;
}

static obj_t make_special(char *string)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(sizeof(special_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_special");
    obj = addr;
    obj->special.type = TYPE_SPECIAL;
    obj->special.name = string;
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(special_s);
  return obj;
}

static obj_t make_operator(char *name,
                           entry_t entry, obj_t arguments,
                           obj_t body, obj_t env, obj_t op_env)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(sizeof(operator_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_operator");
    obj = addr;
    obj->operator.type = TYPE_OPERATOR;
    obj->operator.name = name;
    obj->operator.entry = entry;
    obj->operator.arguments = arguments;
    obj->operator.body = body;
    obj->operator.env = env;
    obj->operator.op_env = op_env;
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(operator_s);
  return obj;
}

static obj_t make_port(obj_t name, FILE *stream)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(sizeof(port_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_operator");
    obj = addr;
    obj->port.type = TYPE_PORT;
    obj->port.name = name;
    obj->port.stream = stream;
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(port_s);
  return obj;
}

static obj_t make_character(char c)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(sizeof(character_s));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_character");
    obj = addr;
    obj->character.type = TYPE_CHARACTER;
    obj->character.c = c;
  } while(!mps_commit(obj_ap, addr, size));
  total += sizeof(character_s);
  return obj;
}

static obj_t make_vector(size_t length, obj_t fill)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN(offsetof(vector_s, vector) + length * sizeof(obj_t));
  do {
    mps_res_t res = mps_reserve(&addr, obj_ap, size);
    size_t i;
    if (res != MPS_RES_OK) error("out of memory in make_vector");
    obj = addr;
    obj->vector.type = TYPE_VECTOR;
    obj->vector.length = length;
    for(i = 0; i < length; ++i)
      obj->vector.vector[i] = fill;
  } while(!mps_commit(obj_ap, addr, size));
  total += size;
  return obj;
}


/* getnbc -- get next non-blank char from stream */

static int getnbc(FILE *stream)
{
  int c;
  do
    c = getc(stream);
  while(isspace(c));
  return c;
}


/* isealpha -- test for "extended alphabetic" char
 *
 * Scheme symbols may contain any "extended alphabetic"
 * character (see section 2.1 of R4RS).  This function
 * returns non-zero if a character is in the set of
 * extended characters.
 */

static int isealpha(int c)
{
  return strchr("+-.*/<=>!?:$%_&~^", c) != NULL;
}


/* hash -- hash a string to an unsigned long
 *
 * This hash function was derived (with permission) from
 * Paul Haahr's hash in the most excellent rc 1.4.
 */

static unsigned long hash(const char *s) {
  char c;
  unsigned long h=0;

  do {
    c=*s++; if(c=='\0') break; else h+=(c<<17)^(c<<11)^(c<<5)^(c>>1);
    c=*s++; if(c=='\0') break; else h^=(c<<14)+(c<<7)+(c<<4)+c;
    c=*s++; if(c=='\0') break; else h^=(~c<<11)|((c<<3)^(c>>1));
    c=*s++; if(c=='\0') break; else h-=(c<<16)|(c<<9)|(c<<2)|(c&3);
  } while(c);

  return h;
}


/* find -- find entry for symbol in symbol table
 *
 * Look for a symbol matching the string in the symbol table.
 * If the symbol was found, returns the address of the symbol
 * table entry which points to the symbol.  Otherwise it
 * either returns the address of a NULL entry into which the
 * new symbol should be inserted, or NULL if the symbol table
 * is full.
 */

static obj_t *find(char *string) {
  unsigned long i, h;

  h = hash(string) & (symtab_size-1);
  i = h;
  do {
    if(symtab[i] == NULL ||
       strcmp(string, symtab[i]->symbol.string) == 0)
      return &symtab[i];
    i = (i+h+1) & (symtab_size-1);
  } while(i != h);

  return NULL;
}


/* rehash -- double size of symbol table */

static void rehash(void) {
  obj_t *old_symtab = symtab;
  unsigned old_symtab_size = symtab_size;
  mps_root_t old_symtab_root = symtab_root;
  unsigned i;
  mps_res_t res;

  symtab_size *= 2;
  symtab = malloc(sizeof(obj_t) * symtab_size);
  if(symtab == NULL) error("out of memory");

  /* Initialize the new table to NULL so that "find" will work. */
  for(i = 0; i < symtab_size; ++i)
    symtab[i] = NULL;

  /* Once the symbol table is initialized with scannable references (NULL
     in this case) we must register it as a root before we copy objects
     across from the old symbol table.  The MPS might be moving objects
     in memory at any time, and will arrange that both copies are updated
     atomically to the mutator (this interpreter). */
  res = mps_root_create_table(&symtab_root, arena, mps_rank_exact(), 0,
                              (mps_addr_t *)symtab, symtab_size);
  if(res != MPS_RES_OK) error("Couldn't register new symtab root");

  for(i = 0; i < old_symtab_size; ++i)
    if(old_symtab[i] != NULL) {
      obj_t *where = find(old_symtab[i]->symbol.string);
      assert(where != NULL);	/* new table shouldn't be full */
      assert(*where == NULL);	/* shouldn't be in new table */
      *where = old_symtab[i];
    }

  mps_root_destroy(old_symtab_root);
  free(old_symtab);
}

/* union-find string in symbol table, rehashing if necessary */
static obj_t intern(char *string) {
  obj_t *where;

  where = find(string);

  if(where == NULL) {
    rehash();
    where = find(string);
    assert(where != NULL);	/* shouldn't be full after rehash */
  }
  
  if(*where == NULL)		/* symbol not found in table */
    *where = make_symbol(strlen(string), string);
  
  return *where;
}


static void print(obj_t obj, unsigned depth, FILE *stream)
{
  switch(TYPE(obj)) {
    case TYPE_INTEGER: {
      fprintf(stream, "%ld", obj->integer.integer);
    } break;
    
    case TYPE_SYMBOL: {
      fputs(obj->symbol.string, stream);
    } break;
    
    case TYPE_SPECIAL: {
      fputs(obj->special.name, stream);
    } break;

    case TYPE_PORT: {
      assert(TYPE(obj->port.name) == TYPE_STRING);
      fprintf(stream, "#[port \"%s\"]",
              obj->port.name->string.string);
    } break;

    case TYPE_STRING: {
      size_t i;
      putc('"', stream);
      for(i = 0; i < obj->string.length; ++i) {
        char c = obj->string.string[i];
        switch(c) {
          case '\\': fputs("\\\\", stream); break;
          case '"': fputs("\\\"", stream); break;
          default: putc(c, stream); break;
        }
      }
      putc('"', stream);
    } break;
    
    case TYPE_PROMISE: {
      assert(CAR(obj) == obj_true || CAR(obj) == obj_false);
      fprintf(stream, "#[%sevaluated promise ",
              CAR(obj) == obj_false ? "un" : "");
      print(CDR(obj), depth - 1, stream);
      putc(']', stream);
    } break;

    case TYPE_PAIR: {
      if(TYPE(CAR(obj)) == TYPE_SYMBOL &&
         TYPE(CDR(obj)) == TYPE_PAIR &&
         CDDR(obj) == obj_empty) {
        if(CAR(obj) == obj_quote) {
          putc('\'', stream);
          if(depth == 0)
            fputs("...", stream);
          else
            print(CADR(obj), depth - 1, stream);
          break;
        }
        if(CAR(obj) == obj_quasiquote) {
          putc('`', stream);
          if(depth == 0)
            fputs("...", stream);
          else
            print(CADR(obj), depth - 1, stream);
          break;
        }
        if(CAR(obj) == obj_unquote) {
          putc(',', stream);
          if(depth == 0)
            fputs("...", stream);
          else
            print(CADR(obj), depth - 1, stream);
          break;
        }
        if(CAR(obj) == obj_unquote_splic) {
          fputs(",@", stream);
          if(depth == 0)
            fputs("...", stream);
          else
            print(CADR(obj), depth - 1, stream);
          break;
        }
      }
      putc('(', stream);
      if(depth == 0)
        fputs("...", stream);
      else {
        for(;;) {
          print(CAR(obj), depth - 1, stream);
          obj = CDR(obj);
          if(TYPE(obj) != TYPE_PAIR) break;
          putc(' ', stream);
        }
        if(obj != obj_empty) {
          fputs(" . ", stream);
          print(obj, depth - 1, stream);
        }
      }
      putc(')', stream);
    } break;
    
    case TYPE_VECTOR: {
      fputs("#(", stream);
      if(depth == 0)
        fputs("...", stream);
      else {
        size_t i;
        for(i = 0; i < obj->vector.length; ++i) {
          if(i > 0) putc(' ', stream);
          print(obj->vector.vector[i], depth - 1, stream);
        }
      }
      putc(')', stream);
    } break;
    
    case TYPE_OPERATOR: {
      fprintf(stream, "#[operator \"%s\" %p %p ",
              obj->operator.name,
              (void *)obj,
              (void *)obj->operator.entry);
      if(depth == 0)
        fputs("...", stream);
      else {
        print(obj->operator.arguments, depth - 1, stream);
        putc(' ', stream);
        print(obj->operator.body, depth - 1, stream);
        putc(' ', stream);
        print(obj->operator.env, depth - 1, stream);
        putc(' ', stream);
        print(obj->operator.op_env, depth - 1, stream);
      }
      putc(']', stream);
    } break;
    
    case TYPE_CHARACTER: {
      fprintf(stream, "#\\%c", obj->character.c);
    } break;
    
    default:
      assert(0);
      abort();
  }
}


static obj_t read_integer(FILE *stream, int c)
{
  long integer = 0;

  do {
    integer = integer*10 + c-'0';
    c = getc(stream);
  } while(isdigit(c));
  ungetc(c, stream);

  return make_integer(integer);
}


static obj_t read_symbol(FILE *stream, int c)
{
  int length = 0;
  char string[SYMMAX+1];

  do {
    string[length++] = tolower(c);
    c = getc(stream);
  } while(length < SYMMAX && (isalnum(c) || isealpha(c)));

  if(isalnum(c) || isealpha(c))
    error("read: symbol too long");

  string[length] = '\0';

  ungetc(c, stream);

  return intern(string);
}


static obj_t read_string(FILE *stream, int c)
{
  int length = 0;
  char string[STRMAX+1];

  for(;;) {
    c = getc(stream);
    if(c == EOF)
      error("read: end of file during string");
    if(c == '"') break;
    if(length >= STRMAX)
      error("read: string too long");
    if(c == '\\') {
      c = getc(stream);
      switch(c) {
        case '\\': break;
        case '"': break;
        case 'n': c = '\n'; break;
        case 't': c = '\t'; break;
        case EOF:
          error("read: end of file in escape sequence in string");
        default:
          error("read: unknown escape '%c'", c);
      }
    }
    string[length++] = c;
  }

  string[length] = '\0';

  return make_string(length, string);
}


static obj_t read(FILE *stream);


static obj_t read_quote(FILE *stream, int c)
{
  return make_pair(obj_quote, make_pair(read(stream), obj_empty));
}


static obj_t read_quasiquote(FILE *stream, int c)
{
  return make_pair(obj_quasiquote, make_pair(read(stream), obj_empty));
}


static obj_t read_unquote(FILE *stream, int c)
{
  c = getc(stream);
  if(c == '@')
    return make_pair(obj_unquote_splic, make_pair(read(stream), obj_empty));
  ungetc(c, stream);
  return make_pair(obj_unquote, make_pair(read(stream), obj_empty));
}


static obj_t read_list(FILE *stream, int c)
{
  obj_t list, new, end;

  list = obj_empty;

  for(;;) {
    c = getnbc(stream);
    if(c == ')' || c == '.') break;
    ungetc(c, stream);
    new = make_pair(read(stream), obj_empty);
    if(list == obj_empty) {
      list = new;
      end = new;
    } else {
      CDR(end) = new;
      end = new;
    }
  }

  if(c == '.') {
    if(list == obj_empty)
      error("read: unexpected dot");
    CDR(end) = read(stream);
    c = getnbc(stream);
  }

  if(c != ')')
    error("read: expected close parenthesis");

  return list;
}


static obj_t list_to_vector(obj_t list)
{
  size_t i;
  obj_t l, vector;
  i = 0;
  l = list;
  while(TYPE(l) == TYPE_PAIR) {
    ++i;
    l = CDR(l);
  }
  if(l != obj_empty)
    return obj_error;
  vector = make_vector(i, obj_undefined);
  i = 0;
  l = list;
  while(TYPE(l) == TYPE_PAIR) {
    vector->vector.vector[i] = CAR(l);
    ++i;
    l = CDR(l);
  }
  return vector;
}


static obj_t read_special(FILE *stream, int c)
{
  c = getnbc(stream);
  switch(tolower(c)) {
    case 't': return obj_true;
    case 'f': return obj_false;
    case '\\': {		/* character (R4RS 6.6) */
      c = getc(stream);
      if(c == EOF)
        error("read: end of file reading character literal");
      return make_character(c);
    }
    case '(': {			/* vector (R4RS 6.8) */
      obj_t list = read_list(stream, c);
      obj_t vector = list_to_vector(list);
      if(vector == obj_error)
        error("read: illegal vector syntax");
      return vector;
    }
  }
  error("read: unknown special '%c'", c);
  return obj_error;
}


static obj_t read(FILE *stream)
{
  int c;

  c = getnbc(stream);
  if(c == EOF) return obj_eof;

  if(isdigit(c))
    return read_integer(stream, c);
  
  switch(c) {
    case '\'': return read_quote(stream, c);
    case '`':  return read_quasiquote(stream, c);
    case ',':  return read_unquote(stream, c);
    case '(':  return read_list(stream, c);
    case '#':  return read_special(stream, c);
    case '"':  return read_string(stream, c);
    case '-': case '+': {
      int next = getc(stream);
      if(isdigit(next)) {
        obj_t integer = read_integer(stream, next);
        if(c == '-')
          integer->integer.integer = -integer->integer.integer;
        return integer;
      }
      ungetc(next, stream);
    } break; /* fall through to read as symbol */
  }

  if(isalpha(c) || isealpha(c))
    return read_symbol(stream, c);

  error("read: illegal char '%c'", c);
  return obj_error;
}


/* lookup_in_frame -- look up a symbol in single frame
 *
 * Search a single frame of the environment for a symbol binding.
 */

static obj_t lookup_in_frame(obj_t frame, obj_t symbol)
{
  while(frame != obj_empty) {
    assert(TYPE(frame) == TYPE_PAIR);
    assert(TYPE(CAR(frame)) == TYPE_PAIR);
    assert(TYPE(CAAR(frame)) == TYPE_SYMBOL);
    if(CAAR(frame) == symbol)
      return CAR(frame);
    frame = CDR(frame);
  }
  return obj_undefined;
}


/* lookup -- look up symbol in environment
 *
 * Search an entire environment for a binding of a symbol.
 */

static obj_t lookup(obj_t env, obj_t symbol)
{
  obj_t binding;
  while(env != obj_empty) {
    assert(TYPE(env) == TYPE_PAIR);
    binding = lookup_in_frame(CAR(env), symbol);
    if(binding != obj_undefined)
      return binding;
    env = CDR(env);
  }
  return obj_undefined;
}


/* define -- define symbol in environment
 *
 * In Scheme, define will actually rebind (i.e. set) a symbol in the
 * same frame of the environment, or add a binding if it wasn't already
 * set.  This has the effect of making bindings local to functions
 * (see how entry_interpret adds an empty frame to the environments),
 * allowing recursion, and allowing redefinition at the top level.
 * See R4R2 section 5.2 for details.
 */

static void define(obj_t env, obj_t symbol, obj_t value)
{
  obj_t binding;
  assert(TYPE(env) == TYPE_PAIR);	/* always at least one frame */
  binding = lookup_in_frame(CAR(env), symbol);
  if(binding != obj_undefined)
    CDR(binding) = value;
  else
    CAR(env) = make_pair(make_pair(symbol, value), CAR(env));
}


static obj_t eval(obj_t env, obj_t op_env, obj_t exp);

static obj_t eval(obj_t env, obj_t op_env, obj_t exp)
{
  for(;;) {
    obj_t operator;
    obj_t result;

    /* self-evaluating */
    if(TYPE(exp) == TYPE_INTEGER ||
       (TYPE(exp) == TYPE_SPECIAL && exp != obj_empty) ||
       TYPE(exp) == TYPE_STRING ||
       TYPE(exp) == TYPE_CHARACTER)
      return exp;
  
    /* symbol lookup */
    if(TYPE(exp) == TYPE_SYMBOL) {
      obj_t binding = lookup(env, exp);
      if(binding == obj_undefined)
        error("eval: unbound symbol \"%s\"", exp->symbol.string);
      return CDR(binding);
    }
    
    if(TYPE(exp) != TYPE_PAIR) {
      error("eval: unknown syntax");
      return obj_error;
    }

    /* apply operator or function */
    if(TYPE(CAR(exp)) == TYPE_SYMBOL) {
      obj_t binding = lookup(op_env, CAR(exp));
      if(binding != obj_undefined) {
        operator = CDR(binding);
        assert(TYPE(operator) == TYPE_OPERATOR);
        result = (*operator->operator.entry)(env, op_env, operator, CDR(exp));
        goto found;
      }
    }
    operator = eval(env, op_env, CAR(exp));
    unless(TYPE(operator) == TYPE_OPERATOR)
      error("eval: application of non-function");
    result = (*operator->operator.entry)(env, op_env, operator, CDR(exp));

  found:
    if (!(TYPE(result) == TYPE_PAIR && CAR(result) == obj_tail))
      return result;

    env = CADR(result);
    op_env = CADDR(result);
    exp = CAR(CDDDR(result));
  }
}


/* OPERATOR UTILITIES */


/* eval_list -- evaluate list of expressions giving list of results
 *
 * eval_list evaluates a list of expresions and yields a list of their
 * results, in order.  If the list is badly formed, an error is thrown
 * using the message given.
 */

static obj_t eval_list(obj_t env, obj_t op_env, obj_t list, char *message)
{
  obj_t result, end, pair;
  result = obj_empty;
  while(list != obj_empty) {
    if(TYPE(list) != TYPE_PAIR)
      error(message);
    pair = make_pair(eval(env, op_env, CAR(list)), obj_empty);
    if(result == obj_empty)
      result = pair;
    else
      CDR(end) = pair;
    end = pair;
    list = CDR(list);
  }
  return result;
}


/* eval_args1 -- evaluate some operator arguments
 *
 * See eval_args and eval_args_rest for usage.
 */

static obj_t eval_args1(char *name, obj_t env, obj_t op_env,
                        obj_t operands, unsigned n, va_list args)
{
  unsigned i;
  for(i = 0; i < n; ++i) {
    unless(TYPE(operands) == TYPE_PAIR)
      error("eval: too few arguments to %s", name);
    *va_arg(args, obj_t *) = eval(env, op_env, CAR(operands));
    operands = CDR(operands);
  }
  return operands;
}


/* eval_args -- evaluate operator arguments without rest list
 *
 * eval_args evaluates the first "n" expressions from the list of
 * expressions in "operands", returning the rest of the operands
 * unevaluated.  It puts the results of evaluation in the addresses
 * passed in the vararg list.  If the operands list is badly formed
 * an error is thrown using the operator name passed.  For example:
 *
 *   eval_args("foo", env, op_env, operands, 2, &arg1, &arg2);
 */

static void eval_args(char *name, obj_t env, obj_t op_env,
                      obj_t operands, unsigned n, ...)
{
  va_list args;
  va_start(args, n);
  operands = eval_args1(name, env, op_env, operands, n, args);
  unless(operands == obj_empty)
    error("eval: too many arguments to %s", name);
  va_end(args);
}


/* eval_args_rest -- evaluate operator arguments with rest list
 *
 * eval_args_rest evaluates the first "n" expressions from the list of
 * expressions in "operands", then evaluates the rest of the operands
 * using eval_list and puts the result at *restp.  It puts the results
 * of evaluating the first "n" operands in the addresses
 * passed in the vararg list.  If the operands list is badly formed
 * an error is thrown using the operator name passed.  For example:
 *
 *   eval_args_rest("foo", env, op_env, operands, &rest, 2, &arg1, &arg2);
 */

static void eval_args_rest(char *name, obj_t env, obj_t op_env,
                           obj_t operands, obj_t *restp, unsigned n, ...)
{
  va_list args;
  va_start(args, n);
  operands = eval_args1(name, env, op_env, operands, n, args);
  va_end(args);
  *restp = eval_list(env, op_env, operands, "eval: badly formed argument list");
}


/* eval_tail -- return an object that will cause eval to loop
 *
 * Rather than calling `eval` an operator can return a special object that
 * causes a calling `eval` to loop, avoiding using up a C stack frame.
 * This implements tail recursion (in a simple way).
 */

static obj_t eval_tail(obj_t env, obj_t op_env, obj_t exp)
{
  return make_pair(obj_tail,
                   make_pair(env,
                             make_pair(op_env,
                                       make_pair(exp,
                                                 obj_empty))));
}


/* eval_body -- evaluate a list of expressions, returning last result
 *
 * This is used for the bodies of forms such as let, begin, etc. where
 * a list of expressions is allowed.
 */

static obj_t eval_body(obj_t env, obj_t op_env, obj_t operator, obj_t body)
{
  for (;;) {
    if (TYPE(body) != TYPE_PAIR)
      error("%s: illegal expression list", operator->operator.name);
    if (CDR(body) == obj_empty)
      return eval_tail(env, op_env, CAR(body));
    (void)eval(env, op_env, CAR(body));
    body = CDR(body);
  }
}


/* BUILT-IN OPERATORS */


/* entry_interpret -- interpreted function entry point
 *
 * When a function is made using lambda (see entry_lambda) an operator
 * is created with entry_interpret as its entry point, and the arguments
 * and body of the function.  The entry_interpret function evaluates
 * the operands of the function and binds them to the argument names
 * in a new frame added to the lambda's closure environment.   It then
 * evaluates the body in that environment, executing the function.
 */

static obj_t entry_interpret(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arguments, fun_env, fun_op_env;

  assert(TYPE(operator) == TYPE_OPERATOR);
  
  /* Make a new frame so that bindings are local to the function. */
  /* Arguments will be bound in this new frame. */
  fun_env = make_pair(obj_empty, operator->operator.env);
  fun_op_env = make_pair(obj_empty, operator->operator.op_env);

  arguments = operator->operator.arguments;
  while(operands != obj_empty) {
    if(arguments == obj_empty)
      error("eval: function applied to too many arguments");
    if(TYPE(arguments) == TYPE_SYMBOL) {
      define(fun_env, arguments,
             eval_list(env, op_env, operands, "eval: badly formed argument list"));
      operands = obj_empty;
      arguments = obj_empty;
    } else {
      assert(TYPE(arguments) == TYPE_PAIR &&
             TYPE(CAR(arguments)) == TYPE_SYMBOL);
      define(fun_env,
             CAR(arguments),
             eval(env, op_env, CAR(operands)));
      operands = CDR(operands);
      arguments = CDR(arguments);
    }
  }
  if(arguments != obj_empty)
    error("eval: function applied to too few arguments");

  return eval_tail(fun_env, fun_op_env, operator->operator.body);
}


/* entry_quote -- return operands unevaluated
 *
 * In Scheme, (quote foo) evaluates to foo (i.e. foo is not evaluated).
 * See R4RS 4.1.2.  The reader expands "'x" to "(quote x)".
 */

static obj_t entry_quote(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  unless(TYPE(operands) == TYPE_PAIR &&
         CDR(operands) == obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  return CAR(operands);
}


/* entry_define -- bind a symbol in the top frame of the environment
 *
 * In Scheme, "(define <symbol> <expression>)" evaluates expressions
 * and binds it to symbol in the top frame of the environment (see
 * R4RS 5.2).  This code also allows the non-essential syntax for
 * define, "(define (<symbol> <formals>) <body>)" as a short-hand for
 * "(define <symbol> (lambda (<formals>) <body>))".
 */

static obj_t entry_define(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t symbol, value;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR &&
         CDDR(operands) == obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  if(TYPE(CAR(operands)) == TYPE_SYMBOL) {
    symbol = CAR(operands);
    value = eval(env, op_env, CADR(operands));
  } else if(TYPE(CAR(operands)) == TYPE_PAIR &&
            TYPE(CAAR(operands)) == TYPE_SYMBOL) {
    symbol = CAAR(operands);
    value = eval(env, op_env,
                 make_pair(obj_lambda,
                           make_pair(CDAR(operands), CDR(operands))));
  } else
    error("%s: applied to binder", operator->operator.name);
  define(env, symbol, value);
  return symbol;
}


/* entry_if -- one- or two-armed conditional
 *
 * "(if <test> <consequent> <alternate>)" and "(if <test> <consequent>)".
 * See R4RS 4.1.5.
 */

static obj_t entry_if(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t test;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR &&
         (CDDR(operands) == obj_empty ||
          (TYPE(CDDR(operands)) == TYPE_PAIR &&
           CDDDR(operands) == obj_empty)))
    error("%s: illegal syntax", operator->operator.name);
  test = eval(env, op_env, CAR(operands));
  /* Anything which is not #f counts as true [R4RS 6.1]. */
  if(test != obj_false)
    return eval_tail(env, op_env, CADR(operands));
  if(TYPE(CDDR(operands)) == TYPE_PAIR)
    return eval_tail(env, op_env, CADDR(operands));
  return obj_undefined;
}


/* entry_cond -- general conditional
 *
 * "(cond (<test1> <exp1.1> ...) (<test2> <exp2.1> ...) ... [(else <expe.1> ...)])"
 */

static obj_t entry_cond(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  unless(TYPE(operands) == TYPE_PAIR)
    error("%s: illegal syntax", operator->operator.name);
  while(TYPE(operands) == TYPE_PAIR) {
    obj_t clause = CAR(operands);
    obj_t result;
    unless(TYPE(clause) == TYPE_PAIR &&
           TYPE(CDR(clause)) == TYPE_PAIR)
      error("%s: illegal clause syntax", operator->operator.name);
    if(CAR(clause) == obj_else) {
      unless(CDR(operands) == obj_empty)
        error("%s: else clause must come last", operator->operator.name);
      result = obj_true;
    } else
      result = eval(env, op_env, CAR(clause));
    if(result != obj_false) {
      if (CDR(clause) == obj_empty)
        return result;
      return eval_body(env, op_env, operator, CDR(clause));
    }
    operands = CDR(operands);
  }
  return obj_undefined;
}


/* entry_and -- (and <test1> ...) */

static obj_t entry_and(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t test;
  if (operands == obj_empty)
    return obj_true;
  do {
    if (TYPE(operands) != TYPE_PAIR)
      error("%s: illegal syntax", operator->operator.name);
    if (CDR(operands) == obj_empty)
      return eval_tail(env, op_env, CAR(operands));
    test = eval(env, op_env, CAR(operands));
    operands = CDR(operands);
  } while (test != obj_false);
  return test;
}


/* entry_or -- (or <test1> ...) */

static obj_t entry_or(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t test;
  if (operands == obj_empty)
    return obj_false;
  do {
    if (TYPE(operands) != TYPE_PAIR)
      error("%s: illegal syntax", operator->operator.name);
    if (CDR(operands) == obj_empty)
      return eval_tail(env, op_env, CAR(operands));
    test = eval(env, op_env, CAR(operands));
    operands = CDR(operands);
  } while (test == obj_false);
  return test;
}


/* entry_let -- (let <bindings> <body>) */
/* TODO: Too much common code with let* */

static obj_t entry_let(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t inner_env, bindings;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR)
    error("%s: illegal syntax", operator->operator.name);
  inner_env = make_pair(obj_empty, env);	/* TODO: common with interpret */
  bindings = CAR(operands);
  while(TYPE(bindings) == TYPE_PAIR) {
    obj_t binding = CAR(bindings);
    unless(TYPE(binding) == TYPE_PAIR &&
           TYPE(CAR(binding)) == TYPE_SYMBOL &&
           TYPE(CDR(binding)) == TYPE_PAIR &&
           CDDR(binding) == obj_empty)
      error("%s: illegal binding", operator->operator.name);
    define(inner_env, CAR(binding), eval(env, op_env, CADR(binding)));
    bindings = CDR(bindings);
  }
  if(bindings != obj_empty)
    error("%s: illegal bindings list", operator->operator.name);
  return eval_body(inner_env, op_env, operator, CDR(operands));
}


/* entry_let_star -- (let* <bindings> <body>) */
/* TODO: Too much common code with let */

static obj_t entry_let_star(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t inner_env, bindings;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR)
    error("%s: illegal syntax", operator->operator.name);
  inner_env = make_pair(obj_empty, env);	/* TODO: common with interpret */
  bindings = CAR(operands);
  while(TYPE(bindings) == TYPE_PAIR) {
    obj_t binding = CAR(bindings);
    unless(TYPE(binding) == TYPE_PAIR &&
           TYPE(CAR(binding)) == TYPE_SYMBOL &&
           TYPE(CDR(binding)) == TYPE_PAIR &&
           CDDR(binding) == obj_empty)
      error("%s: illegal binding", operator->operator.name);
    define(inner_env, CAR(binding), eval(inner_env, op_env, CADR(binding)));
    bindings = CDR(bindings);
  }
  if(bindings != obj_empty)
    error("%s: illegal bindings list", operator->operator.name);
  return eval_body(inner_env, op_env, operator, CDR(operands));
}


/* entry_letrec -- (letrec <bindings> <body>) */
/* TODO: Too much common code with let and let* */

static obj_t entry_letrec(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t inner_env, bindings;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR)
    error("%s: illegal syntax", operator->operator.name);
  inner_env = make_pair(obj_empty, env);	/* TODO: common with interpret */
  bindings = CAR(operands);
  while(TYPE(bindings) == TYPE_PAIR) {
    obj_t binding = CAR(bindings);
    unless(TYPE(binding) == TYPE_PAIR &&
           TYPE(CAR(binding)) == TYPE_SYMBOL &&
           TYPE(CDR(binding)) == TYPE_PAIR &&
           CDDR(binding) == obj_empty)
      error("%s: illegal binding", operator->operator.name);
    define(inner_env, CAR(binding), obj_undefined);
    bindings = CDR(bindings);
  }
  if(bindings != obj_empty)
    error("%s: illegal bindings list", operator->operator.name);
  bindings = CAR(operands);
  while(TYPE(bindings) == TYPE_PAIR) {
    obj_t binding = CAR(bindings);
    define(inner_env, CAR(binding), eval(inner_env, op_env, CADR(binding)));
    bindings = CDR(bindings);
  }
  return eval_body(inner_env, op_env, operator, CDR(operands));
}


/* entry_do -- (do ((<var> <init> <step1>) ...) (<test> <exp> ...) <command> ...) */

static obj_t entry_do(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  error("%s: unimplemented", operator->operator.name);
  return obj_error;
}


/* entry_delay -- (delay <exp>) */

static obj_t entry_delay(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t promise;
  unless(TYPE(operands) == TYPE_PAIR &&
         CDR(operands) == obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  promise = make_pair(obj_false,
                      make_operator("anonymous promise",
                                    entry_interpret, obj_empty,
                                    CAR(operands), env, op_env));
  TYPE(promise) = TYPE_PROMISE;
  return promise;
}


/* entry_quasiquote -- (quasiquote <template>) or `<template> */
/* TODO: blech. */

static obj_t entry_quasiquote(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t list, result = obj_empty, pair, end, insert;
  unless(TYPE(operands) == TYPE_PAIR &&
         CDR(operands) == obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  list = CAR(operands);
  while(TYPE(list) == TYPE_PAIR) {
    if(TYPE(CAR(list)) == TYPE_PAIR &&
       TYPE(CAAR(list)) == TYPE_SYMBOL &&
       (CAAR(list) == obj_unquote ||
        CAAR(list) == obj_unquote_splic)) {
      unless(TYPE(CDAR(list)) == TYPE_PAIR &&
             CDDAR(list) == obj_empty)
        error("%s: illegal %s syntax", operator->operator.name, CAAR(list)->symbol.string);
      insert = eval(env, op_env, CADAR(list));
      if(CAAR(list) == obj_unquote) {
        pair = make_pair(insert, obj_empty);
        if(result == obj_empty)
          result = pair;
        else
          CDR(end) = pair;
        end = pair;
      } else if(CAAR(list) == obj_unquote_splic) {
        if(insert != obj_empty) {
          if(TYPE(insert) != TYPE_PAIR)
            error("%s: unquote-splicing expression must return list",
                   operator->operator.name);
          if(result == obj_empty)
            result = insert;
          else
            CDR(end) = insert;
          while(TYPE(CDR(insert)) == TYPE_PAIR)
            insert = CDR(insert);
          if(CDR(insert) != obj_empty)
            error("%s: unquote-splicing expression must return list",
                   operator->operator.name);
          end = insert;
        }
      }
    } else {
      pair = make_pair(CAR(list), obj_empty);
      if(result == obj_empty)
        result = pair;
      else
        CDR(end) = pair;
      end = pair;
    }
    list = CDR(list);
  }
  if(list != obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  return result;
}


/* entry_set -- assignment
 *
 * (set! <variable> <expression>)
 * See R4RS 4.1.6.
 */

static obj_t entry_set(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t symbol, binding, value;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR &&
         CDDR(operands) == obj_empty)
    error("%s: illegal syntax", operator->operator.name);
  unless(TYPE(CAR(operands)) == TYPE_SYMBOL)
    error("%s: applied to non-symbol", operator->operator.name);
  symbol = CAR(operands);
  binding = lookup(env, symbol);
  if(binding == obj_undefined)
    error("%s: applied to unbound symbol \"%s\"",
          operator->operator.name, symbol->symbol.string);
  value = eval(env, op_env, CADR(operands));
  CDR(binding) = value;
  return value;
}


/* entry_lambda -- lambda expressions
 *
 * (lambda <formals> <body>)
 * See R4RS 4.1.4.
 *
 * The entry_lambda function creates a new object of TYPE_OPERATOR
 * which captures the current environments, and contains the lambda
 * formals and body.  This operator has an entry point at
 * entry_interpret, which will evaluate and bind the arguments when
 * the operator is applied.
 *
 * [Capturing the whole environment is bad for GC because it means
 *  that everything defined when the lambda is evaluated will survive
 *  for as long as the operator survives.  It would be better to
 *  examine the lambda body and determine which variables it references,
 *  and either create a new environment or build a new body with just
 *  those variables bound.]
 */

static obj_t entry_lambda(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t list;
  unless(TYPE(operands) == TYPE_PAIR &&
         TYPE(CDR(operands)) == TYPE_PAIR)
    error("%s: illegal syntax", operator->operator.name);
  /* check syntax of argument list to save time in apply */
  list = CAR(operands);
  while(list != obj_empty && TYPE(list) != TYPE_SYMBOL) {
    unless(TYPE(list) == TYPE_PAIR &&
           TYPE(CAR(list)) == TYPE_SYMBOL)
      error("%s: illegal argument list", operator->operator.name);
    list = CDR(list);
  }
  return make_operator("anonymous function",
                       entry_interpret, CAR(operands),
                       make_pair(obj_begin, CDR(operands)),
                       env, op_env);
}


/* entry_begin -- sequencing
 *
 * (begin <expression1> <expression2> ...)
 * See R4RS 4.2.3.
 */

static obj_t entry_begin(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  return eval_body(env, op_env, operator, operands);
}


/* BUILT-IN FUNCTIONS */


/* entry_not -- (not <obj>)
 *
 * Not returns #t if obj is false, and return #f otherwise.  R4RS 6.1.
 */

static obj_t entry_not(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return arg == obj_false ? obj_true : obj_false;
}


/* entry_booleanp -- (boolean? <obj>)
 *
 * Boolean? return #t if obj is either #t or #f, and #f otherwise.  R4RS 6.1.
 */

static obj_t entry_booleanp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return arg == obj_true || arg == obj_false ? obj_true : obj_false;
}


/* entry_eqvp -- (eqv? <obj1> <obj2>) */

static int eqvp(obj_t obj1, obj_t obj2)
{
  return obj1 == obj2 ||
         (TYPE(obj1) == TYPE_INTEGER &&
          TYPE(obj2) == TYPE_INTEGER &&
          obj1->integer.integer == obj2->integer.integer);
}

static obj_t entry_eqvp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg1, arg2;
  eval_args(operator->operator.name, env, op_env, operands, 2, &arg1, &arg2);
  return eqvp(arg1, arg2) ? obj_true : obj_false;
}


/* entry_eqp -- (eq? <obj1> <obj2>) */

static obj_t entry_eqp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg1, arg2;
  eval_args(operator->operator.name, env, op_env, operands, 2, &arg1, &arg2);
  return arg1 == arg2 ? obj_true : obj_false;
}


/* entry_equalp -- (equal? <obj1> <obj2>) */

static int equalp(obj_t obj1, obj_t obj2)
{
  if(TYPE(obj1) != TYPE(obj2))
    return 0;
  if(TYPE(obj1) == TYPE_PAIR)
    return equalp(CAR(obj1), CAR(obj2)) && equalp(CDR(obj1), CDR(obj2));
  /* TODO: Similar recursion for vectors. */
  return eqvp(obj1, obj2);
}

static obj_t entry_equalp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg1, arg2;
  eval_args(operator->operator.name, env, op_env, operands, 2, &arg1, &arg2);
  return equalp(arg1, arg2) ? obj_true : obj_false;
}


/* entry_pairp -- (pair? <obj>) */

static obj_t entry_pairp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_PAIR ? obj_true : obj_false;
}


/* entry_cons -- create pair
 *
 * (cons <obj1> <obj2>)
 * See R4RS 6.3.
 *
 * Returns a newly allocated pair whose car is obj1 and whose cdr is obj2.
 * The pair is guaranteed to be different (in the sense of eqv?) from every
 * existing object.
 */

static obj_t entry_cons(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t car, cdr;
  eval_args(operator->operator.name, env, op_env, operands, 2, &car, &cdr);
  return make_pair(car, cdr);
}


/* entry_car -- R4RS 6.3 */

static obj_t entry_car(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t pair;
  eval_args(operator->operator.name, env, op_env, operands, 1, &pair);
  unless(TYPE(pair) == TYPE_PAIR)
    error("%s: argument must be a pair", operator->operator.name);
  return CAR(pair);
}


static obj_t entry_cdr(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t pair;
  eval_args(operator->operator.name, env, op_env, operands, 1, &pair);
  unless(TYPE(pair) == TYPE_PAIR)
    error("%s: argument must be a pair", operator->operator.name);
  return CDR(pair);
}


static obj_t entry_setcar(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t pair, value;
  eval_args(operator->operator.name, env, op_env, operands, 2, &pair, &value);
  unless(TYPE(pair) == TYPE_PAIR)
    error("%s: first argument must be a pair", operator->operator.name);
  CAR(pair) = value;
  return obj_undefined;
}


static obj_t entry_setcdr(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t pair, value;
  eval_args(operator->operator.name, env, op_env, operands, 2, &pair, &value);
  unless(TYPE(pair) == TYPE_PAIR)
    error("%s: first argument must be a pair", operator->operator.name);
  CDR(pair) = value;
  return obj_undefined;
}


static obj_t entry_nullp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return arg == obj_empty ? obj_true : obj_false;
}


static obj_t entry_listp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  while(TYPE(arg) == TYPE_PAIR)
    arg = CDR(arg);
  return arg == obj_empty ? obj_true : obj_false;
}


static obj_t entry_list(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t rest;
  eval_args_rest(operator->operator.name, env, op_env, operands, &rest, 0);
  return rest;
}


static obj_t entry_length(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  long length;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  length = 0;
  while(TYPE(arg) == TYPE_PAIR) {
    ++length;
    arg = CDR(arg);
  }
  if(arg != obj_empty)
    error("%s: applied to non-list", operator->operator.name);
  return make_integer(length);
}


static obj_t entry_append(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg1, arg2, result, pair, end;
  eval_args(operator->operator.name, env, op_env, operands, 2, &arg1, &arg2);
  result = obj_empty;
  while(TYPE(arg1) == TYPE_PAIR) {
    pair = make_pair(CAR(arg1), obj_empty);
    if(result == obj_empty)
      result = pair;
    else
      CDR(end) = pair;
    end = pair;
    arg1 = CDR(arg1);
  }
  if(arg1 != obj_empty)
    error("%s: applied to non-list", operator->operator.name);
  if(result == obj_empty)
    return arg2;
  CDR(end) = arg2;
  return result;
}


static obj_t entry_integerp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_INTEGER ? obj_true : obj_false;
}


static obj_t entry_zerop(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: argument must be an integer", operator->operator.name);
  return arg->integer.integer == 0 ? obj_true : obj_false;
}


static obj_t entry_positivep(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: argument must be an integer", operator->operator.name);
  return arg->integer.integer > 0 ? obj_true : obj_false;
}


static obj_t entry_negativep(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: argument must be an integer", operator->operator.name);
  return arg->integer.integer < 0 ? obj_true : obj_false;
}


static obj_t entry_symbolp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_SYMBOL ? obj_true : obj_false;
}


/* (procedure? obj)
 * Returns #t if obj is a procedure, otherwise returns #f.
 * R6RS 11.6.
 */
static obj_t entry_procedurep(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_OPERATOR ? obj_true : obj_false;
}


static obj_t entry_add(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t args;
  long result;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 0);
  result = 0;
  while(TYPE(args) == TYPE_PAIR) {
    unless(TYPE(CAR(args)) == TYPE_INTEGER)
      error("%s: arguments must be integers", operator->operator.name);
    result += CAR(args)->integer.integer;
    args = CDR(args);
  }
  assert(args == obj_empty); /* eval_args_rest always returns a list */
  return make_integer(result);
}


static obj_t entry_multiply(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t args;
  long result;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 0);
  result = 1;
  while(TYPE(args) == TYPE_PAIR) {
    unless(TYPE(CAR(args)) == TYPE_INTEGER)
      error("%s: arguments must be integers", operator->operator.name);
    result *= CAR(args)->integer.integer;
    args = CDR(args);
  }
  assert(args == obj_empty); /* eval_args_rest always returns a list */
  return make_integer(result);
}


static obj_t entry_subtract(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, args;
  long result;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  result = arg->integer.integer;
  if(args == obj_empty)
    result = -result;
  else {
    while(TYPE(args) == TYPE_PAIR) {
      unless(TYPE(CAR(args)) == TYPE_INTEGER)
        error("%s: arguments must be integers", operator->operator.name);
      result -= CAR(args)->integer.integer;
      args = CDR(args);
    }
    assert(args == obj_empty); /* eval_args_rest always returns a list */
  }
  return make_integer(result);
}


static obj_t entry_divide(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, args;
  long result;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  result = arg->integer.integer;
  if(args == obj_empty) {
    if(result == 0)
      error("%s: reciprocal of zero", operator->operator.name);
    result = 1/result;	/* TODO: pretty meaningless for integers */
  } else {
    while(TYPE(args) == TYPE_PAIR) {
      unless(TYPE(CAR(args)) == TYPE_INTEGER)
        error("%s: arguments must be integers", operator->operator.name);
      if(CAR(args)->integer.integer == 0)
        error("%s: divide by zero", operator->operator.name);
      result /= CAR(args)->integer.integer;
      args = CDR(args);
    }
    assert(args == obj_empty); /* eval_args_rest always returns a list */
  }
  return make_integer(result);
}


static obj_t entry_lessthan(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, args;
  long last;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  last = arg->integer.integer;
  while(TYPE(args) == TYPE_PAIR) {
    unless(TYPE(CAR(args)) == TYPE_INTEGER)
      error("%s: arguments must be integers", operator->operator.name);
    if (last >= CAR(args)->integer.integer)
      return obj_false;
    last = CAR(args)->integer.integer;
    args = CDR(args);
  }
  assert(args == obj_empty); /* eval_args_rest always returns a list */
  return obj_true;
}


static obj_t entry_greaterthan(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, args;
  long last;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  last = arg->integer.integer;
  while(TYPE(args) == TYPE_PAIR) {
    unless(TYPE(CAR(args)) == TYPE_INTEGER)
      error("%s: arguments must be integers", operator->operator.name);
    if (last <= CAR(args)->integer.integer)
      return obj_false;
    last = CAR(args)->integer.integer;
    args = CDR(args);
  }
  assert(args == obj_empty); /* eval_args_rest always returns a list */
  return obj_true;
}


static obj_t entry_reverse(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, result;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  result = obj_empty;
  while(arg != obj_empty) {
    unless(TYPE(arg) == TYPE_PAIR)
      error("%s: argument must be a list", operator->operator.name);
    result = make_pair(CAR(arg), result);
    arg = CDR(arg);
  }
  return result;
}


static obj_t entry_environment(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  eval_args(operator->operator.name, env, op_env, operands, 0);
  return env;
}


/* (open-input-file filename)
 * Opens filename for input, with empty file options, and returns the
 * obtained port.
 * R6RS Standard Library 8.3.
 */
static obj_t entry_open_input_file(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t filename;
  FILE *stream;
  obj_t port;
  mps_addr_t port_ref;
  eval_args(operator->operator.name, env, op_env, operands, 1, &filename);
  unless(TYPE(filename) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  stream = fopen(filename->string.string, "r");
  if(stream == NULL)
    /* TODO: "raise an exception with condition type &i/o." */
    error("%s: cannot open input file", operator->operator.name);
  port = make_port(filename, stream);

  /* %%MPS: Register the port object for finalization.  When the object is
     no longer referenced elsewhere, a message will be received in `mps_chat`
     so that the file can be closed.  See `mps_chat`. */
  port_ref = port;
  mps_finalize(arena, &port_ref);

  return port;
}


/* TODO: This doesn't work if the promise refers to its own value. */

static obj_t entry_force(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t promise;
  eval_args(operator->operator.name, env, op_env, operands, 1, &promise);
  unless(TYPE(promise) == TYPE_PROMISE)
    error("%s: argument must be a promise", operator->operator.name);
  assert(CAR(promise) == obj_false || CAR(promise) == obj_true);
  /* If the promise is unevaluated then apply the CDR. */
  if(CAR(promise) == obj_false) {
    obj_t closure = CDR(promise);
    assert(TYPE(closure) == TYPE_OPERATOR);
    assert(closure->operator.arguments == obj_empty);
    CDR(promise) = (*closure->operator.entry)(env, op_env, closure, obj_empty);
    CAR(promise) = obj_true;
  }
  return CDR(promise);
}

/* (char? obj)
 * Returns #t if obj is a character, otherwise returns #f.
 * R6RS 11.11.
 */
static obj_t entry_charp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_CHARACTER ? obj_true : obj_false;
}

/* (char->integer char)
 * Given a character, char->integer returns its Unicode scalar value
 * as an exact integer object.
 * R6RS 11.11.
 */
static obj_t entry_char_to_integer(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_CHARACTER)
    error("%s: first argument must be a character", operator->operator.name);
  return make_integer(arg->character.c);
}


/* (integer->char sv)
 * For a Unicode scalar value sv, integer->char returns its associated
 * character.
 * R6RS 11.11.
 */
static obj_t entry_integer_to_char(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  unless(0 <= arg->integer.integer)
    error("%s: first argument is out of range", operator->operator.name);
  return make_character(arg->integer.integer);
}


static obj_t entry_vectorp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_VECTOR ? obj_true : obj_false;
}


static obj_t entry_make_vector(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t length, rest, fill;
  eval_args_rest(operator->operator.name, env, op_env, operands, &rest, 1, &length);
  unless(TYPE(length) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  if(rest == obj_empty)
    fill = obj_undefined;
  else {
    unless(CDR(rest) == obj_empty)
      error("%s: too many arguments", operator->operator.name);
    fill = CAR(rest);
  }
  return make_vector(length->integer.integer, fill);
}


static obj_t entry_vector(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t rest, vector;
  eval_args_rest(operator->operator.name, env, op_env, operands, &rest, 0);
  vector = list_to_vector(rest);
  assert(vector != obj_error);
  return vector;
}


static obj_t entry_vector_length(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t vector;
  eval_args(operator->operator.name, env, op_env, operands, 1, &vector);
  unless(TYPE(vector) == TYPE_VECTOR)
    error("%s: argument must be a vector", operator->operator.name);
  return make_integer(vector->vector.length);
}


static obj_t entry_vector_ref(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t vector, index;
  eval_args(operator->operator.name, env, op_env, operands, 2, &vector, &index);
  unless(TYPE(vector) == TYPE_VECTOR)
    error("%s: first argument must be a vector", operator->operator.name);
  unless(TYPE(index) == TYPE_INTEGER)
    error("%s: second argument must be an integer", operator->operator.name);
  unless(0 <= index->integer.integer && index->integer.integer < vector->vector.length)
    error("%s: index %ld out of bounds of vector length %ld",
          operator->operator.name, index->integer.integer, vector->vector.length);
  return vector->vector.vector[index->integer.integer];
}


static obj_t entry_vector_set(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t vector, index, obj;
  eval_args(operator->operator.name, env, op_env, operands, 3, &vector, &index, &obj);
  unless(TYPE(vector) == TYPE_VECTOR)
    error("%s: first argument must be a vector", operator->operator.name);
  unless(TYPE(index) == TYPE_INTEGER)
    error("%s: second argument must be an integer", operator->operator.name);
  unless(0 <= index->integer.integer && index->integer.integer < vector->vector.length)
    error("%s: index %ld out of bounds of vector length %ld",
          operator->operator.name, index->integer.integer, vector->vector.length);
  vector->vector.vector[index->integer.integer] = obj;
  return obj_undefined;
}


static obj_t entry_vector_to_list(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t vector, list;
  size_t i;
  eval_args(operator->operator.name, env, op_env, operands, 1, &vector);
  unless(TYPE(vector) == TYPE_VECTOR)
    error("%s: argument must be a vector", operator->operator.name);
  list = obj_empty;
  i = vector->vector.length;
  while(i > 0) {
    --i;
    list = make_pair(vector->vector.vector[i], list);
  }
  return list;
}


static obj_t entry_list_to_vector(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t list, vector;
  eval_args(operator->operator.name, env, op_env, operands, 1, &list);
  vector = list_to_vector(list);
  if(vector == obj_error)
    error("%s: argument must be a list", operator->operator.name);
  return vector;
}


static obj_t entry_vector_fill(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t vector, obj;
  size_t i;
  eval_args(operator->operator.name, env, op_env, operands, 2, &vector, &obj);
  unless(TYPE(vector) == TYPE_VECTOR)
    error("%s: first argument must be a vector", operator->operator.name);
  for(i = 0; i < vector->vector.length; ++i)
    vector->vector.vector[i] = obj;
  return obj_undefined;
}


static obj_t entry_eval(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t exp;
  eval_args(operator->operator.name, env, op_env, operands, 1, &exp);
  return eval(env, op_env, exp);
}


static obj_t entry_symbol_to_string(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t symbol;
  eval_args(operator->operator.name, env, op_env, operands, 1, &symbol);
  unless(TYPE(symbol) == TYPE_SYMBOL)
    error("%s: argument must be a symbol", operator->operator.name);
  return make_string(symbol->symbol.length, symbol->symbol.string);
}


static obj_t entry_string_to_symbol(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t string;
  eval_args(operator->operator.name, env, op_env, operands, 1, &string);
  unless(TYPE(string) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  /* TODO: Should pass length to intern to avoid problems with NUL termination. */
  return intern(string->string.string);
}

/* (string? obj)
 * Returns #t if obj is a string, otherwise returns #f.
 * R6RS 11.12
 */
static obj_t entry_stringp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_STRING ? obj_true : obj_false;
}


/* (make-string k)
 * (make-string k char)
 * `make-string' returns a newly allocated string of length k. If char
 * is given, then all elements of the string are initialized to char,
 * otherwise the contents of the string are unspecified.
 * R6RS 11.12
 */
static obj_t entry_make_string(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t obj, k, args;
  char c = '\0';
  int i;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 1, &k);
  unless(TYPE(k) == TYPE_INTEGER)
    error("%s: first argument must be an integer", operator->operator.name);
  unless(k->integer.integer >= 0)
    error("%s: first argument must be non-negative", operator->operator.name);
  if (TYPE(args) == TYPE_PAIR) {
    unless(TYPE(CAR(args)) == TYPE_CHARACTER)
      error("%s: second argument must be a character", operator->operator.name);
    unless(CDR(args) == obj_empty)
      error("%s: too many arguments", operator->operator.name);
    c = CAR(args)->character.c;
  }
  obj = make_string(k->integer.integer, NULL);
  for (i = 0; i < k->integer.integer; ++i) {
    obj->string.string[i] = c;
  }
  return obj;
}


/* (string char ...)
 * Returns a newly allocated string composed of the arguments.
 * R6RS 11.12
 */
static obj_t entry_string(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t args, obj, o;
  size_t length;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 0);
  o = args;
  length = 0;
  while(TYPE(o) == TYPE_PAIR) {
    unless(TYPE(CAR(o)) == TYPE_CHARACTER)
      error("%s: arguments must be strings", operator->operator.name);
    ++ length;
    o = CDR(o);
  }
  obj = make_string(length, NULL);
  o = args;
  length = 0;
  while(TYPE(o) == TYPE_PAIR) {
    assert(TYPE(CAR(o)) == TYPE_CHARACTER);
    obj->string.string[length] = CAR(o)->character.c;
    ++ length;
    o = CDR(o);
  }
  assert(length == obj->string.length);
  return obj;
}


/* (string-length string)
 * Returns the number of characters in the given string as an exact
 * integer object.
 * R6RS 11.12
 */
static obj_t entry_string_length(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  return make_integer(arg->string.length);
}


/* (string-ref string k)
 * k must be a valid index of string. `String-ref' returns character k
 * of string using zero-origin indexing.
 * R6RS 11.12
 */
static obj_t entry_string_ref(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg, k;
  eval_args(operator->operator.name, env, op_env, operands, 2, &arg, &k);
  unless(TYPE(arg) == TYPE_STRING)
    error("%s: first argument must be a string", operator->operator.name);
  unless(TYPE(k) == TYPE_INTEGER)
    error("%s: second argument must be an integer", operator->operator.name);
  unless(0 <= k->integer.integer && k->integer.integer < arg->string.length)
    error("%s: second argument is out of range", operator->operator.name);
  return make_character(arg->string.string[k->integer.integer]);
}


/* (substring string start end)
 * String must be a string, and start and end must be exact integers
 * satisfying 
 *     0 <= start <= end <= (string-length string).
 * `Substring' returns a newly allocated string formed from the
 * characters of string beginning with index start (inclusive) and
 * ending with index end (exclusive).
 * R6RS 11.12
 */
static obj_t entry_substring(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t obj, arg, start, end;
  size_t length;
  eval_args(operator->operator.name, env, op_env, operands, 3, &arg, &start, &end);
  unless(TYPE(arg) == TYPE_STRING)
    error("%s: first argument must be a string", operator->operator.name);
  unless(TYPE(start) == TYPE_INTEGER)
    error("%s: second argument must be an integer", operator->operator.name);
  unless(TYPE(end) == TYPE_INTEGER)
    error("%s: third argument must be an integer", operator->operator.name);
  unless(0 <= start->integer.integer
         && start->integer.integer <= end->integer.integer
         && end->integer.integer <= arg->string.length)
    error("%s: arguments out of range", operator->operator.name);
  length = end->integer.integer - start->integer.integer;
  obj = make_string(length, NULL);
  strncpy(obj->string.string, &arg->string.string[start->integer.integer], length);
  return obj;
}

/* (string-append string ...)
 * Returns a newly allocated string whose characters form the
 * concatenation of the given strings.
 * R6RS 11.12
 */
static obj_t entry_string_append(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t args, obj, o;
  size_t length;
  eval_args_rest(operator->operator.name, env, op_env, operands, &args, 0);
  o = args;
  length = 0;
  while(TYPE(o) == TYPE_PAIR) {
    unless(TYPE(CAR(o)) == TYPE_STRING)
      error("%s: arguments must be strings", operator->operator.name);
    length += CAR(o)->string.length;
    o = CDR(o);
  }
  obj = make_string(length, NULL);
  o = args;
  length = 0;
  while(TYPE(o) == TYPE_PAIR) {
    string_s *s = &CAR(o)->string;
    assert(TYPE(CAR(o)) == TYPE_STRING);
    memcpy(obj->string.string + length, s->string, s->length + 1);
    length += s->length;
    o = CDR(o);
  }
  assert(length == obj->string.length);
  return obj;
}


/* (string->list string)
 * The string->list procedure returns a newly allocated list of the
 * characters that make up the given string.
 * R6RS 11.12.
 */
static obj_t entry_string_to_list(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t string, list;
  size_t i;
  eval_args(operator->operator.name, env, op_env, operands, 1, &string);
  unless(TYPE(string) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  list = obj_empty;
  i = string->string.length;
  while(i > 0) {
    --i;
    list = make_pair(make_character(string->string.string[i]), list);
  }
  return list;
}

/* (list->string list)
 * List must be a list of characters. The list->string procedure
 * returns a newly allocated string formed from the characters in
 * list.
 */
static obj_t entry_list_to_string(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t l, list, string;
  size_t i, length = 0;
  eval_args(operator->operator.name, env, op_env, operands, 1, &list);
  l = list;
  while(l != obj_empty) {
    unless(TYPE(l) == TYPE_PAIR)
      error("%s: argument must be a list", operator->operator.name);
    unless(TYPE(CAR(l)) == TYPE_CHARACTER)
      error("%s: argument must be a list of characters", operator->operator.name);
    ++ length;
    l = CDR(l);
  }
  string = make_string(length, NULL);
  l = list;
  for(i = 0; i < length; ++i) {
    assert(TYPE(l) == TYPE_PAIR);
    assert(TYPE(CAR(l)) == TYPE_CHARACTER);
    string->string.string[i] = CAR(l)->character.c;
    l = CDR(l);
  }
  return string;
}


/* (string-copy string)
 * Returns a newly allocated copy of the given string.
 * R6RS 11.12
 */
static obj_t entry_string_copy(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  return make_string(arg->string.length, arg->string.string);
}


/* entry_gc -- full garbage collection now                      %%MPS
 *
 * This is an example of a direct interface from the language to the MPS.
 * The `gc` function in Scheme will cause the MPS to perform a complete
 * garbage collection of the entire arena right away.
 */

static obj_t entry_gc(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  mps_res_t res = mps_arena_collect(arena);
  if (res != MPS_RES_OK)
    error("Couldn't collect: %d", res);
  mps_arena_release(arena);
  return obj_undefined;
}


/* INITIALIZATION */


/* special table */

static struct {char *name; obj_t *varp;} sptab[] = {
  {"()", &obj_empty},
  {"#[eof]", &obj_eof},
  {"#[error]", &obj_error},
  {"#t", &obj_true},
  {"#f", &obj_false},
  {"#[undefined]", &obj_undefined},
  {"#[tail]", &obj_tail}
};


/* initial symbol table */

static struct {char *name; obj_t *varp;} isymtab[] = {
  {"quote", &obj_quote},
  {"lambda", &obj_lambda},
  {"begin", &obj_begin},
  {"else", &obj_else},
  {"quasiquote", &obj_quasiquote},
  {"unquote", &obj_unquote},
  {"unquote-splicing", &obj_unquote_splic}
};


/* operator table */

static struct {char *name; entry_t entry;} optab[] = {
  {"quote", entry_quote},
  {"define", entry_define},
  {"set!", entry_set},
  {"lambda", entry_lambda},
  {"begin", entry_begin},
  {"cond", entry_cond},
  {"if", entry_if},
  {"and", entry_and},
  {"or", entry_or},
  {"let", entry_let},
  {"let*", entry_let_star},
  {"letrec", entry_letrec},
  {"do", entry_do},
  {"delay", entry_delay},
  {"quasiquote", entry_quasiquote}
};
  

/* function table */

static struct {char *name; entry_t entry;} funtab[] = {
  {"not", entry_not},
  {"boolean?", entry_booleanp},
  {"eqv?", entry_eqvp},
  {"eq?", entry_eqp},
  {"equal?", entry_equalp},
  {"pair?", entry_pairp},
  {"cons", entry_cons},
  {"car", entry_car},
  {"cdr", entry_cdr},
  {"set-car!", entry_setcar},
  {"set-cdr!", entry_setcdr},
  {"null?", entry_nullp},
  {"list?", entry_listp},
  {"list", entry_list},
  {"length", entry_length},
  {"append", entry_append},
  {"integer?", entry_integerp},
  {"zero?", entry_zerop},
  {"positive?", entry_positivep},
  {"negative?", entry_negativep},
  {"symbol?", entry_symbolp},
  {"procedure?", entry_procedurep},
  {"+", entry_add},
  {"-", entry_subtract},
  {"*", entry_multiply},
  {"/", entry_divide},
  {"<", entry_lessthan},
  {">", entry_greaterthan},
  {"reverse", entry_reverse},
  {"the-environment", entry_environment},
  {"open-input-file", entry_open_input_file},
  {"force", entry_force},
  {"char?", entry_charp},
  {"char->integer", entry_char_to_integer},
  {"integer->char", entry_integer_to_char},
  {"vector?", entry_vectorp},
  {"make-vector", entry_make_vector},
  {"vector", entry_vector},
  {"vector-length", entry_vector_length},
  {"vector-ref", entry_vector_ref},
  {"vector-set!", entry_vector_set},
  {"vector->list", entry_vector_to_list},
  {"list->vector", entry_list_to_vector},
  {"vector-fill!", entry_vector_fill},
  {"eval", entry_eval},
  {"symbol->string", entry_symbol_to_string},
  {"string->symbol", entry_string_to_symbol},
  {"string?", entry_stringp},
  {"make-string", entry_make_string},
  {"string", entry_string},
  {"string-length", entry_string_length},
  {"string-ref", entry_string_ref},
  {"substring", entry_substring},
  {"string-append", entry_string_append},
  {"string->list", entry_string_to_list},
  {"list->string", entry_list_to_string},
  {"string-copy", entry_string_copy},
  {"gc", entry_gc}
};


/* MPS Format                                                   %%MPS
 *
 * These functions satisfy the MPS Format Protocol for format
 * variant "A".
 *
 * In general, MPS format methods are performance critical, as they're used
 * on the MPS [critical path](..\..\design\critical-path.txt).
 *
 * Format methods might also be called at any time from the MPS, including
 * in signal handlers, exception handlers, interrupts, or other special
 * contexts.  They must avoid touching any memory except the object they're
 * asked about, and possibly some static volatile data.
 *
 * Because these methods are critical, there are considerable gains in
 * performance if you mix them with the MPS source code and allow the
 * compiler to optimize globally.  See [Building the Memory Pool
 * System](../../manual/build.txt).
 */


/* obj_scan -- object format scanner                            %%MPS
 *
 * The job of the scanner is to identify references in a contiguous
 * group of objects in memory, by passing them to the "fix" operation.
 * This code is highly performance critical.
 */

static mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
#define FIX(ref) \
  do { \
    mps_addr_t _addr = (ref); /* copy to local to avoid type pun */ \
    mps_res_t res = MPS_FIX12(ss, &_addr); \
    if (res != MPS_RES_OK) return res; \
    (ref) = _addr; \
  } while(0)

  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      obj_t obj = base;
      switch (obj->type.type) {
      case TYPE_PAIR:
        FIX(obj->pair.car);
        FIX(obj->pair.cdr);
        base = (char *)base + ALIGN(sizeof(pair_s));
        break;
      case TYPE_INTEGER:
        base = (char *)base + ALIGN(sizeof(integer_s));
        break;
      case TYPE_SYMBOL:
        base = (char *)base +
               ALIGN(offsetof(symbol_s, string) + obj->symbol.length + 1);
        break;
      case TYPE_SPECIAL:
        base = (char *)base + ALIGN(sizeof(special_s));
        break;
      case TYPE_OPERATOR:
        FIX(obj->operator.arguments);
        FIX(obj->operator.body);
        FIX(obj->operator.env);
        FIX(obj->operator.op_env);
        base = (char *)base + ALIGN(sizeof(operator_s));
        break;
      case TYPE_STRING:
        base = (char *)base +
               ALIGN(offsetof(string_s, string) + obj->string.length + 1);
        break;
      case TYPE_PORT:
        FIX(obj->port.name);
        base = (char *)base + ALIGN(sizeof(port_s));
        break;
      case TYPE_CHARACTER:
        base = (char *)base + ALIGN(sizeof(character_s));
        break;
      case TYPE_VECTOR:
        {
          size_t i;
          for (i = 0; i < obj->vector.length; ++i)
            FIX(obj->vector.vector[i]);
        }
        base = (char *)base +
               ALIGN(offsetof(vector_s, vector) +
                     obj->vector.length * sizeof(obj->vector.vector[0]));
        break;
      case TYPE_FWD2:
        base = (char *)base + ALIGN(sizeof(fwd2_s));
        break;
      case TYPE_FWD:
        base = (char *)base + ALIGN(obj->fwd.size);
        break;
      case TYPE_PAD1:
        base = (char *)base + ALIGN(sizeof(pad1_s));
        break;
      case TYPE_PAD:
        base = (char *)base + ALIGN(obj->pad.size);
        break;
      default:
        assert(0);
        fprintf(stderr, "Unexpected object on the heap\n");
        abort();
        return MPS_RES_FAIL;
      }
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}


/* obj_skip -- object format skip method                        %%MPS
 *
 * The job of `obj_skip` is to return the address where the next object would
 * be allocated.  This isn't quite the same as the size of the object,
 * since there may be some rounding according to the memory pool alignment
 * chosen.  This interpreter has chosen to align to single words.
 */

static mps_addr_t obj_skip(mps_addr_t base)
{
  obj_t obj = base;
  switch (obj->type.type) {
  case TYPE_PAIR:
    base = (char *)base + ALIGN(sizeof(pair_s));
    break;
  case TYPE_INTEGER:
    base = (char *)base + ALIGN(sizeof(integer_s));
    break;
  case TYPE_SYMBOL:
    base = (char *)base +
           ALIGN(offsetof(symbol_s, string) + obj->symbol.length + 1);
    break;
  case TYPE_SPECIAL:
    base = (char *)base + ALIGN(sizeof(special_s));
    break;
  case TYPE_OPERATOR:
    base = (char *)base + ALIGN(sizeof(operator_s));
    break;
  case TYPE_STRING:
    base = (char *)base +
           ALIGN(offsetof(string_s, string) + obj->string.length + 1);
    break;
  case TYPE_PORT:
    base = (char *)base + ALIGN(sizeof(port_s));
    break;
  case TYPE_CHARACTER:
    base = (char *)base + ALIGN(sizeof(character_s));
    break;
  case TYPE_VECTOR:
    base = (char *)base +
           ALIGN(offsetof(vector_s, vector) +
                 obj->vector.length * sizeof(obj->vector.vector[0]));
    break;
  case TYPE_FWD2:
    base = (char *)base + ALIGN(sizeof(fwd2_s));
    break;
  case TYPE_FWD:
    base = (char *)base + ALIGN(obj->fwd.size);
    break;
  case TYPE_PAD:
    base = (char *)base + ALIGN(obj->pad.size);
    break;
  case TYPE_PAD1:
    base = (char *)base + ALIGN(sizeof(pad1_s));
    break;
  default:
    assert(0);
    fprintf(stderr, "Unexpected object on the heap\n");
    abort();
    return NULL;
  }
  return base;
}


/* obj_isfwd -- object format forwarded test                    %%MPS
 *
 * The job of `obj_isfwd` is to detect whether an object has been replaced
 * by a forwarding object, and return the address of the new copy if it has,
 * otherwise NULL.  Note that this will return NULL for padding objects
 * because their `fwd` field is set to NULL.
 */

static mps_addr_t obj_isfwd(mps_addr_t addr)
{
  obj_t obj = addr;
  switch (obj->type.type) {
  case TYPE_FWD2:
    return obj->fwd2.fwd;
  case TYPE_FWD:
    return obj->fwd.fwd;
  }
  return NULL;
}


/* obj_fwd -- object format forwarding method                   %%MPS
 *
 * The job of `obj_fwd` is to replace an object by a forwarding object that
 * points at a new copy of the object.  The object must be detected by
 * `obj_isfwd`.  In this case, we have to be careful to replace two-word
 * objects with a `FWD2` object, because the `FWD` object won't fit.
 */

static void obj_fwd(mps_addr_t old, mps_addr_t new)
{
  obj_t obj = old;
  mps_addr_t limit = obj_skip(old);
  size_t size = (char *)limit - (char *)old;
  assert(size >= ALIGN(sizeof(fwd2_s)));
  if (size == ALIGN(sizeof(fwd2_s))) {
    obj->type.type = TYPE_FWD2;
    obj->fwd2.fwd = new;
  } else {
    obj->type.type = TYPE_FWD;
    obj->fwd.fwd = new;
    obj->fwd.size = size;
  }
}


/* obj_pad -- object format padding method                      %%MPS
 *
 * The job of `obj_pad` is to fill in a block of memory with a padding
 * object that will be skipped by `obj_scan` or `obj_skip` but does
 * nothing else.  Because we've chosen to align to single words, we may
 * have to pad a single word, so we have a special single-word padding
 * object, `PAD1` for that purpose.  Otherwise we can use multi-word
 * padding objects, `PAD`.
 */

static void obj_pad(mps_addr_t addr, size_t size)
{
  obj_t obj = addr;
  assert(size >= ALIGN(sizeof(pad1_s)));
  if (size == ALIGN(sizeof(pad1_s))) {
    obj->type.type = TYPE_PAD1;
  } else {
    obj->type.type = TYPE_PAD;
    obj->pad.size = size;
  }
}


/* obj_fmt_s -- object format parameter structure               %%MPS
 *
 * This is simply a gathering of the object format methods and the chosen
 * pool alignment for passing to `mps_fmt_create_A`.
 */

struct mps_fmt_A_s obj_fmt_s = {
  ALIGNMENT,
  obj_scan,
  obj_skip,
  NULL,                         /* Obsolete copy method */
  obj_fwd,
  obj_isfwd,
  obj_pad
};


/* globals_scan -- scan static global variables                 %%MPS
 *
 * The static global variables are all used to hold values that are set
 * up using the `sptab` and `isymtab` tables, and conveniently we have
 * a list of pointers to those variables.  This is a custom root scanning
 * method that uses them to fix those variables.
 */

static mps_res_t globals_scan(mps_ss_t ss, void *p, size_t s)
{
  MPS_SCAN_BEGIN(ss) {
    size_t i;
    for (i = 0; i < LENGTH(sptab); ++i)
      FIX(*sptab[i].varp);
    for (i = 0; i < LENGTH(isymtab); ++i)
      FIX(*isymtab[i].varp);
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}


/* mps_chat -- get and display MPS messages                     %%MPS
 *
 * The MPS message protocol allows the MPS to communicate various things
 * to the client code.  Because the MPS may run asynchronously the client
 * must poll the MPS to pick up messages.  This function shows how this
 * is done.
 */

static void mps_chat(void)
{
  mps_message_type_t type;

  while (mps_message_queue_type(&type, arena)) {
    mps_message_t message;
    mps_bool_t b;
    b = mps_message_get(&message, arena, type);
    assert(b); /* we just checked there was one */
    
    if (type == mps_message_type_gc_start()) {
      printf("Collection started.\n");
      printf("  Why: %s\n", mps_message_gc_start_why(arena, message));
      printf("  Clock: %lu\n", (unsigned long)mps_message_clock(arena, message));

    } else if (type == mps_message_type_gc()) {
      size_t live = mps_message_gc_live_size(arena, message);
      size_t condemned = mps_message_gc_condemned_size(arena, message);
      size_t not_condemned = mps_message_gc_not_condemned_size(arena, message);
      printf("Collection finished.\n");
      printf("    live %lu\n", (unsigned long)live);
      printf("    condemned %lu\n", (unsigned long)condemned);
      printf("    not_condemned %lu\n", (unsigned long)not_condemned);
      printf("    clock: %lu\n", (unsigned long)mps_message_clock(arena, message));

    /* A finalization message is received when an object registered earlier
       with `mps_finalize` would have been recycled if it hadn't been
       registered.  This means there are no other references to the object.
       In this interpreter, we register ports with open files for
       finalization, so that we can close the file (and release operating
       system resources) when a port object gets lost without being
       properly closed first.  Note, however, that finalization isn't
       reliable or prompt.  Treat it as an optimization. */
    } else if (type == mps_message_type_finalization()) {
      mps_addr_t port_ref;
      obj_t port;
      mps_message_finalization_ref(&port_ref, arena, message);
      port = port_ref;
      /* We're only expecting ports to be finalized as they're the only
         objects registered for finalization.  See `entry_open_input_file`. */
      assert(TYPE(port) == TYPE_PORT);
      printf("Port to file \"%s\" is dying. Closing file.\n",
             port->port.name->string.string);
      (void)fclose(port->port.stream);

    } else {
      printf("Unknown message from MPS!\n");
    }

    mps_message_discard(arena, message);
  }
}


/* start -- the main program                                    %%MPS
 *
 * This is the main body of the Scheme interpreter program, invoked by
 * `mps_tramp` so that its stack and exception handling can be managed
 * by the MPS.
 */

static void *start(void *p, size_t s)
{
  size_t i;
  volatile obj_t env, op_env, obj;
  jmp_buf jb;
  mps_res_t res;
  mps_root_t globals_root;
  
  puts("MPS Toy Scheme Example\n"
       "The prompt shows total allocated bytes and number of collections.\n"
       "Try (vector-length (make-vector 100000 1)) to see the MPS in action.\n"
       "You can force a complete garbage collection with (gc).\n"
       "If you recurse too much the interpreter may crash from using too much C stack.");
  
  total = (size_t)0;
  
  symtab_size = 16;
  symtab = malloc(sizeof(obj_t) * symtab_size);
  if(symtab == NULL) error("out of memory");
  for(i = 0; i < symtab_size; ++i)
    symtab[i] = NULL;

  /* Note that since the symbol table is an exact root we must register
     it with the MPS only after it has been initialized with scannable
     pointers -- NULL in this case.  Random values look like false
     references into MPS memory and cause undefined behaviour (most likely
     assertion failures). */
  res = mps_root_create_table(&symtab_root, arena, mps_rank_exact(), 0,
                              (mps_addr_t *)symtab, symtab_size);
  if(res != MPS_RES_OK) error("Couldn't register symtab root");

  error_handler = &jb;

  /* By contrast with the symbol table, we *must* register the globals as
     roots before we start making things to put into them, because making
     stuff might cause a garbage collection and throw away their contents
     if they're not registered.  Since they're static variables they'll
     contain NULL pointers, and are scannable from the start. */
  res = mps_root_create(&globals_root, arena, mps_rank_exact(), 0,
                        globals_scan, NULL, 0);
  if (res != MPS_RES_OK) error("Couldn't register globals root");

  if(!setjmp(*error_handler)) {
    for(i = 0; i < LENGTH(sptab); ++i)
      *sptab[i].varp = make_special(sptab[i].name);
    for(i = 0; i < LENGTH(isymtab); ++i)
      *isymtab[i].varp = intern(isymtab[i].name);
    env = make_pair(obj_empty, obj_empty);
    op_env = make_pair(obj_empty, obj_empty);
    for(i = 0; i < LENGTH(funtab); ++i)
      define(env,
             intern(funtab[i].name),
             make_operator(funtab[i].name, funtab[i].entry,
                           obj_empty, obj_empty, env, op_env));
    for(i = 0; i < LENGTH(optab); ++i)
      define(op_env,
             intern(optab[i].name),
             make_operator(optab[i].name, optab[i].entry,
                           obj_empty, obj_empty, env, op_env));
  } else {
    fprintf(stderr,
            "Fatal error during initialization: %s\n",
            error_message);
    abort();
  }

  /* The read-eval-print loop */
  
  for(;;) {
    if(setjmp(*error_handler) != 0) {
      fprintf(stderr, "%s\n", error_message);
    }
    
    mps_chat();

    printf("%lu, %lu> ", (unsigned long)total,
                         (unsigned long)mps_collections(arena));
    obj = read(stdin);
    if(obj == obj_eof) break;
    obj = eval(env, op_env, obj);
    print(obj, 6, stdout);
    putc('\n', stdout);
  }

  puts("Bye.");

  /* See comment at the end of `main` about cleaning up. */
  mps_root_destroy(symtab_root);
  mps_root_destroy(globals_root);

  return 0;
}


/* obj_gen_params -- initial setup for generational GC          %%MPS
 *
 * Each structure in this array describes one generation of objects. The
 * two members are the capacity of the generation in kilobytes, and the
 * mortality, the proportion of objects in the generation that you expect
 * to survive a collection of that generation.
 * 
 * These numbers are *hints* to the MPS that it may use to make decisions
 * about when and what to collect: nothing will go wrong (other than
 * suboptimal performance) if you make poor choices.
 */

static mps_gen_param_s obj_gen_params[] = {
  { 150, 0.85 },
  { 170, 0.45 }
};


/* main -- program entry point and MPS initialization           %%MPS */

int main(int argc, char *argv[])
{
  mps_res_t res;
  mps_chain_t obj_chain;
  mps_fmt_t obj_fmt;
  mps_thr_t thread;
  mps_root_t reg_root;
  void *r;
  void *marker = &marker;
  
  /* Create an MPS arena.  There is usually only one of these in a process.
     It holds all the MPS "global" state and is where everything happens. */
  res = mps_arena_create(&arena,
                         mps_arena_class_vm(), 
                         (size_t)(1024 * 1024));
  if (res != MPS_RES_OK) error("Couldn't create arena");

  /* Create the object format. */
  res = mps_fmt_create_A(&obj_fmt, arena, &obj_fmt_s);
  if (res != MPS_RES_OK) error("Couldn't create obj format");

  /* Create a chain controlling GC strategy. FIXME: explain! */
  res = mps_chain_create(&obj_chain,
                         arena,
                         LENGTH(obj_gen_params),
                         obj_gen_params);
  if (res != MPS_RES_OK) error("Couldn't create obj chain");

  /* Create an Automatic Mostly-Copying (AMC) pool to manage the Scheme
     objects.  This is a kind of copying garbage collector. */
  res = mps_pool_create(&obj_pool,
                        arena,
                        mps_class_amc(),
                        obj_fmt,
                        obj_chain);
  if (res != MPS_RES_OK) error("Couldn't create obj pool");

  /* Create an allocation point for fast in-line allocation of objects
     from the `obj_pool`.  You'd usually want one of these per thread
     for your primary pools.  This interpreter is single threaded, though,
     so we just have it in a global. */
  res = mps_ap_create(&obj_ap, obj_pool, mps_rank_exact());
  if (res != MPS_RES_OK) error("Couldn't create obj allocation point");

  /* Register the current thread with the MPS.  The MPS must sometimes
     control or examine threads to ensure consistency when it is scanning
     or updating object references, so any threads that access the MPS
     memory need to be registered. */
  res = mps_thread_reg(&thread, arena);
  if (res != MPS_RES_OK) error("Couldn't register thread");

  /* Register the thread as a root.  This thread's stack and registers will
     need to be scanned by the MPS because we are passing references to
     objects around in C parameters, return values, and keeping them in
     automatic local variables. */
  res = mps_root_create_reg(&reg_root,
                            arena,
                            mps_rank_ambig(),
                            0,
                            thread,
                            mps_stack_scan_ambig,
                            marker,
                            0);
  if (res != MPS_RES_OK) error("Couldn't create root");

  /* Ask the MPS to tell us when it's garbage collecting so that we can
     print some messages.  Completely optional. */
  mps_message_type_enable(arena, mps_message_type_finalization());
  mps_message_type_enable(arena, mps_message_type_gc());
  mps_message_type_enable(arena, mps_message_type_gc_start());

  /* Trampoline into the main program.  The MPS trampoline is unfortunately
     required to mark the top of the stack of the main thread, and on some
     platforms it must also catch exceptions in order to implement hardware
     memory barriers. */
  mps_tramp(&r, start, NULL, 0);
  
  /* Cleaning up the MPS object with destroy methods will allow the MPS to
     check final consistency and warn you about bugs.  It also allows the
     MPS to flush buffers for debugging data, etc.  It's good practise
     to destroy MPS objects on exit if possible rather than just quitting. */
  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  mps_ap_destroy(obj_ap);
  mps_pool_destroy(obj_pool);
  mps_chain_destroy(obj_chain);
  mps_fmt_destroy(obj_fmt);
  mps_arena_destroy(arena);

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2012 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
