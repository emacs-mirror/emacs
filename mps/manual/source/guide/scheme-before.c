/* scheme.c -- SCHEME INTERPRETER EXAMPLE FOR THE MEMORY POOL SYSTEM
 *
 * $Id$
 * Copyright (c) 2001-2012 Ravenbrook Limited.  See end of file for license.
 * 
 * TO DO
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
  TYPE_TABLE,
  TYPE_BUCKETS
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
  obj_t buckets;                /* hash buckets */
} table_s;

typedef struct buckets_s {
  type_t type;                  /* TYPE_BUCKETS */
  size_t length;                /* number of buckets */
  size_t used;                  /* number of buckets in use */
  struct bucket_s {
    obj_t key, value;
  } bucket[1];                  /* hash buckets */
} buckets_s;

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


/* symtab -- symbol table
 *
 * The symbol table is a hash-table containing objects of TYPE_SYMBOL.
 * When a string is "interned" it is looked up in the table, and added
 * only if it is not there.  This guarantees that all symbols which
 * are equal are actually the same object.
 */

static obj_t *symtab;
static size_t symtab_size;


/* special objects
 *
 * These global variables are initialized to point to objects of
 * TYPE_SPECIAL by main.  They are used as markers for various
 * special purposes.
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
 * is a default error handler in main, in the read-eval-print
 * loop.  The error function also writes an error message
 * into "error_message" before longjmping, and this can be
 * displayed to the user when catching the error.
 *
 * [An error code should also be passed so that the error can
 *  be decoded by enclosing code.]
 */

static jmp_buf *error_handler = NULL;
static char error_message[MSGMAX+1];


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


/* make_* -- object constructors
 *
 * Each object type has a function here which allocates an
 * instance of that type.
 */

static obj_t make_pair(obj_t car, obj_t cdr)
{
  obj_t obj = (obj_t)malloc(sizeof(pair_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(pair_s);
  obj->pair.type = TYPE_PAIR;
  CAR(obj) = car;
  CDR(obj) = cdr;
  return obj;
}

static obj_t make_integer(long integer)
{
  obj_t obj = (obj_t)malloc(sizeof(integer_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(integer_s);
  obj->integer.type = TYPE_INTEGER;
  obj->integer.integer = integer;
  return obj;
}

static obj_t make_symbol(size_t length, char string[])
{
  size_t size = offsetof(symbol_s, string) + length+1;
  obj_t obj = (obj_t)malloc(size);
  if(obj == NULL) error("out of memory");
  total += size;
  obj->symbol.type = TYPE_SYMBOL;
  obj->symbol.length = length;
  memcpy(obj->symbol.string, string, length+1);
  return obj;
}

static obj_t make_string(size_t length, char string[])
{
  size_t size = offsetof(string_s, string) + length+1;
  obj_t obj = (obj_t)malloc(size);
  if(obj == NULL) error("out of memory");
  total += size;
  obj->string.type = TYPE_STRING;
  obj->string.length = length;
  memcpy(obj->string.string, string, length+1);
  return obj;
}

static obj_t make_special(char *string)
{
  obj_t obj = (obj_t)malloc(sizeof(special_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(special_s);
  obj->special.type = TYPE_SPECIAL;
  obj->special.name = string;
  return obj;
}

static obj_t make_operator(char *name,
                           entry_t entry, obj_t arguments,
                           obj_t body, obj_t env, obj_t op_env)
{
  obj_t obj = (obj_t)malloc(sizeof(operator_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(operator_s);
  obj->operator.type = TYPE_OPERATOR;
  obj->operator.name = name;
  obj->operator.entry = entry;
  obj->operator.arguments = arguments;
  obj->operator.body = body;
  obj->operator.env = env;
  obj->operator.op_env = op_env;
  return obj;
}

static obj_t make_port(obj_t name, FILE *stream)
{
  obj_t obj = (obj_t)malloc(sizeof(port_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(port_s);
  obj->port.type = TYPE_PORT;
  obj->port.name = name;
  obj->port.stream = stream;
  return obj;
}

static obj_t make_character(char c)
{
  obj_t obj = (obj_t)malloc(sizeof(character_s));
  if(obj == NULL) error("out of memory");
  total += sizeof(character_s);
  obj->character.type = TYPE_CHARACTER;
  obj->character.c = c;
  return obj;
}

static obj_t make_vector(size_t length, obj_t fill)
{
  size_t size = offsetof(vector_s, vector) + length * sizeof(obj_t);
  size_t i;
  obj_t obj = (obj_t)malloc(size);
  if(obj == NULL) error("out of memory");
  total += size;
  obj->vector.type = TYPE_VECTOR;
  obj->vector.length = length;
  for(i = 0; i < length; ++i)
    obj->vector.vector[i] = fill;
  return obj;
}

static obj_t make_buckets(size_t length)
{
  size_t i, size = offsetof(buckets_s, bucket) + length * 2 * sizeof(obj_t);
  obj_t obj = (obj_t)malloc(size);
  if(obj == NULL) error("out of memory");
  total += size;
  obj->buckets.type = TYPE_BUCKETS;
  obj->buckets.length = length;
  obj->buckets.used = 0;
  for(i = 0; i < length; ++i) {
    obj->buckets.bucket[i].key = NULL;
    obj->buckets.bucket[i].value = NULL;
  }
  return obj;
}

static obj_t make_table(size_t length)
{
  size_t size = sizeof(table_s);
  obj_t obj = (obj_t)malloc(size);
  if(obj == NULL) error("out of memory");
  total += size;
  obj->table.type = TYPE_TABLE;
  obj->table.buckets = make_buckets(length);
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
  unsigned i;

  symtab_size *= 2;
  symtab = malloc(sizeof(obj_t) * symtab_size);
  if(symtab == NULL) error("out of memory");

  /* Initialize the new table to NULL so that "find" will work. */
  for(i = 0; i < symtab_size; ++i)
    symtab[i] = NULL;

  for(i = 0; i < old_symtab_size; ++i)
    if(old_symtab[i] != NULL) {
      obj_t *where = find(old_symtab[i]->symbol.string);
      assert(where != NULL);	/* new table shouldn't be full */
      assert(*where == NULL);	/* shouldn't be in new table */
      *where = old_symtab[i];
    }

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


/* Hash table implementation
 * Supports eq? hashing (hash-by-identity) only.
 */
static struct bucket_s *buckets_find(obj_t buckets, obj_t key)
{
  union {char s[sizeof(obj_t) + 1]; void *addr; } u = {""};
  unsigned long i, h;
  assert(TYPE(buckets) == TYPE_BUCKETS);
  u.addr = key;
  h = hash(u.s) & (buckets->buckets.length-1);
  i = h;
  do {
    struct bucket_s *b = &buckets->buckets.bucket[i];
    if(b->key == NULL || b->key == key)
      return b;
    i = (i+h+1) & (buckets->buckets.length-1);
  } while(i != h);
  return NULL;
}

static void table_rehash(obj_t tbl)
{
  size_t i, old_length, new_length;
  obj_t new_buckets;

  assert(TYPE(tbl) == TYPE_TABLE);
  old_length = tbl->table.buckets->buckets.length;
  new_length = old_length * 2;
  new_buckets = make_buckets(new_length);

  for (i = 0; i < old_length; ++i) {
    struct bucket_s *old_b = &tbl->table.buckets->buckets.bucket[i];
    if (old_b->key != NULL) {
      struct bucket_s *b = buckets_find(new_buckets, old_b->key);
      assert(b != NULL);	/* new table shouldn't be full */
      assert(b->key == NULL);	/* shouldn't be in new table */
      *b = *old_b;
      ++ new_buckets->buckets.used;
    }
  }

  assert(new_buckets->buckets.used == tbl->table.buckets->buckets.used);
  tbl->table.buckets = new_buckets;
}

static obj_t table_ref(obj_t tbl, obj_t key)
{
  struct bucket_s *b;
  assert(TYPE(tbl) == TYPE_TABLE);
  b = buckets_find(tbl->table.buckets, key);
  if (b && b->key != NULL)
    return b->value;
  return NULL;
}

static int table_full(obj_t tbl)
{
  assert(TYPE(tbl) == TYPE_TABLE);
  return tbl->table.buckets->buckets.used >= tbl->table.buckets->buckets.length / 2;
}

static void table_set(obj_t tbl, obj_t key, obj_t value)
{
  struct bucket_s *b;
  assert(TYPE(tbl) == TYPE_TABLE);
  if (table_full(tbl) || (b = buckets_find(tbl->table.buckets, key)) == NULL) {
    table_rehash(tbl);
    b = buckets_find(tbl->table.buckets, key);
    assert(b != NULL);          /* shouldn't be full after rehash */
  }
  if (b->key == NULL) {
    b->key = key;
    ++ tbl->table.buckets->buckets.used;
  }
  b->value = value;
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

    case TYPE_BUCKETS: {
      size_t i;
      for(i = 0; i < obj->vector.length; ++i) {
        struct bucket_s *b = &obj->buckets.bucket[i];
        if(b->key) {
          fputs(" (", stream);
          print(b->key, depth - 1, stream);
          putc(' ', stream);
          print(b->value, depth - 1, stream);
          putc(')', stream);
        }
      }
    } break;

    case TYPE_TABLE: {
      fputs("#[hashtable", stream);
      print(obj->table.buckets, depth - 1, stream);
      putc(']', stream);
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
 * See R4RS 6.9.
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
 * Takes a string naming an existing file and returns an input port
 * capable of delivering characters from the file. If the file cannot
 * be opened, an error is signalled.
 * See R4RS 6.10.1
 */
static obj_t entry_open_input_file(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t filename;
  FILE *stream;
  eval_args(operator->operator.name, env, op_env, operands, 1, &filename);
  unless(TYPE(filename) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  stream = fopen(filename->string.string, "r");
  if(stream == NULL)
    /* TODO: "an error is signalled" */
    error("%s: cannot open input file", operator->operator.name);
  return make_port(filename, stream);
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
 * See R4RS 6.6.
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
 * See R4RS 6.6.
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
 * See R4RS 6.6.
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
 * See R4RS 6.7.
 */
static obj_t entry_stringp(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  return TYPE(arg) == TYPE_STRING ? obj_true : obj_false;
}


/* (make-string k)
 * (make-string k char)
 * Make-string returns a newly allocated string of length k. If char
 * is given, then all elements of the string are initialized to char,
 * otherwise the contents of the string are unspecified.
 * See R4RS 6.7.
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
 * See R4RS 6.7.
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
 * Returns the number of characters in the given string.
 * See R4RS 6.7.
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
 * k must be a valid index of string. String-ref returns character k
 * of string using zero-origin indexing.
 * See R4RS 6.7.
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
 * Substring returns a newly allocated string formed from the
 * characters of string beginning with index start (inclusive) and
 * ending with index end (exclusive).
 * See R4RS 6.7.
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
 * See R4RS 6.7.
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
 * See R4RS 6.7.
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
 * See R4RS 6.7.
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
 * See R4RS 6.7.
 */
static obj_t entry_string_copy(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t arg;
  eval_args(operator->operator.name, env, op_env, operands, 1, &arg);
  unless(TYPE(arg) == TYPE_STRING)
    error("%s: argument must be a string", operator->operator.name);
  return make_string(arg->string.length, arg->string.string);
}


/* (make-eq-hashtable)
 * (make-eq-hashtable k)
 * Returns a newly allocated mutable hashtable that accepts arbitrary
 * objects as keys, and compares those keys with eq?. If an argument
 * is given, the initial capacity of the hashtable is set to
 * approximately k elements.
 * See R6RS Library 13.1.
 */
static obj_t entry_make_eq_hashtable(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t rest;
  size_t length;
  eval_args_rest(operator->operator.name, env, op_env, operands, &rest, 0);
  if (rest == obj_empty)
    length = 8;
  else unless(CDR(rest) == obj_empty)
    error("%s: too many arguments", operator->operator.name);
  else {
    obj_t arg = CAR(rest);
    unless(TYPE(arg) == TYPE_INTEGER)
      error("%s: first argument must be an integer", operator->operator.name);
    unless(arg->integer.integer > 0)
      error("%s: first argument must be positive", operator->operator.name);
    length = arg->integer.integer;
  }
  return make_table(length);
}


/* (hashtable-ref hashtable key default)
 * Returns the value in hashtable associated with key. If hashtable
 * does not contain an association for key, default is returned.
 * See R6RS Library 13.2.
 */
static obj_t entry_hashtable_ref(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t tbl, key, def, value;
  eval_args(operator->operator.name, env, op_env, operands, 3, &tbl, &key, &def);
  unless(TYPE(tbl) == TYPE_TABLE)
    error("%s: first argument must be a hash table", operator->operator.name);
  value = table_ref(tbl, key);
  if (value) return value;
  return def;
}


/* (hashtable-set! hashtable key value)
 * Changes hashtable to associate key with obj, adding a new
 * association or replacing any existing association for key, and
 * returns unspecified values.
 * See R6RS Library 13.2.
 */
static obj_t entry_hashtable_set(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
{
  obj_t tbl, key, value;
  eval_args(operator->operator.name, env, op_env, operands, 3, &tbl, &key, &value);
  unless(TYPE(tbl) == TYPE_TABLE)
    error("%s: first argument must be a hash table", operator->operator.name);
  table_set(tbl, key, value);
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
  {"make-eq-hashtable", entry_make_eq_hashtable},
  {"hashtable-ref", entry_hashtable_ref},
  {"hashtable-set!", entry_hashtable_set},
};


/* MAIN PROGRAM */


int main(int argc, char *argv[])
{
  size_t i;
  volatile obj_t env, op_env, obj;
  jmp_buf jb;
  
  puts("Scheme Test Harness");
  
  total = (size_t)0;
  
  symtab_size = 16;
  symtab = malloc(sizeof(obj_t) * symtab_size);
  if(symtab == NULL) error("out of memory");
  for(i = 0; i < symtab_size; ++i)
    symtab[i] = NULL;

  error_handler = &jb;

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

    printf("%lu> ", (unsigned long)total);
    obj = read(stdin);
    if(obj == obj_eof) break;
    obj = eval(env, op_env, obj);
    if(obj != obj_undefined) {
      print(obj, 6, stdout);
      putc('\n', stdout);
    }
  }

  puts("Bye.");

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
