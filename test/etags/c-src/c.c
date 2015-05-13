T f(){if(x){}
}T i;

// The next two functions must be kept together
void bar() {while(0) {}}
int foobar() {;}

struct interface *
interface_locate(void)
{ return 0; }

#line 123 "c.c"
// 7.4: string literal in #line directive shall be a character string
//      literal.
//E t_6_062.cpp(21): warning: ill formed #line directive: 123 L"wide"
#line 123			                L"wide.c"
#line 123 L"wide.c"
#line 123L"wide.c"


void (*fa) (void);
void (__attribute__((noreturn)) *fb) (void);

extern int
my_printf (void *my_object, const char *my_format, ...)
     __attribute__ ((format (printf, 2, 3)));

void fatala () __attribute__ ((noreturn));
void fatalb ();

max (int a, int b)
{ if (a > b) return a; else return b; }
struct bar {
  char z;
  struct foo f;
};
__attribute__ ((always_inline)) max (int a, int b)
{ if (a > b) return a; else return b }
extern int old_var __attribute__ ((deprecated));
struct foo
{
  char a;
  int x[2] __attribute__ ((packed));
};
char stack[10000] __attribute__ ((section ("STACK"))) = { 0 };
struct S { short f[3]; } __attribute__ ((aligned (8)));
typedef union
{
  int *__ip;
  union wait *__up;
} wait_status_ptr_t __attribute__ ((__transparent_union__));
Some_Class  A  __attribute__ ((init_priority (2000)));
typedef T1 T3 __attribute__ ((deprecated));
T3 z __attribute__ ((deprecated));
typedef int more_aligned_int __attribute__ ((aligned (8)));
struct S  __attribute__ ((vector_size (16))) foo;
int foo __attribute__ ((vector_size (16)));
char *__attribute__((aligned(8))) *f;
int i __attribute__ ((visibility ("hidden")));
extern void foobar (void) __attribute__ ((section ("bar")));

typedef struct cacheLRUEntry_s
{
  U16 next;
  U16 prev;
}
__attribute__ ((packed)) cacheLRUEntry_t;
struct foo {
  int x;
  char a, b, c, d;
} __attribute__((packed));
void __attribute__ ((visibility ("protected")))
     f1 () { /* Do something. */; }
void f2 () { /* Do something. */; }
__attribute__((noreturn)) void d0 (void),
  __attribute__((format(printf, 1, 2))) d1 (const char *, ...),
  d2 (void);
int x __attribute__ ((aligned (16))) = 0;
struct foo { int x[2] __attribute__ ((aligned (8))); };
short array[3] __attribute__ ((aligned));

asm("	section	10");
int f
	() {}

DEAFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "name.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;
}
XDEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 2, 0, "")
{}
DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 2, 0, "")
{}
/* The next two are not tagged correctly.  To prevent this, the code in
   Emacs should contain the two first args of DEFUN on the same line. */
DEFUN ("x-get-selection-internal",
       Fx_get_selection_internal, Sx_get_selection_internal, 2, 2, 0, "")
{}
DEFUN
     ("y-get-selection-internal",
      Fy_get_selection_internal, Sy_get_selection_internal, 2, 2, 0, "")
{}
defun_func1()
{}
DEFUN_func2()
{}
typedef int bool;
bool funcboo ()
{}
static void (*lang_func) () = NULL;
struct my_struct {
};
typedef struct my_struct my_typedef;
int bla ()
{
  PrkList ExistingOperations =
      ProcedureOperationSelections(PrkNull, CalledFromDomain);
}
a()
 b c;
{}
int func1
  (a,b,c,d) {};
static struct cca_control init_control = { 0 };
static tpcmd rbtp [RB_TPSZ];
static byte ring1 [(RBUFNO + 1) + 8];
static byte ring2 [(RBUFNO + 1) * sizeof (struct le_md) + 8];
request request (a, b)
{
}
int func2 (a,b
	  c,d) {};
int wrongfunc
  aaa;
struct wrongstruct
  bbb;
struct sss1 {};
struct sss2
{
  struct ss3
    {
    };
};
struct a b;
struct aa *b;
struct aaa
  **b;
caccacacca (a,b,c,d,e,f,g)
     struct aa *b;
{
}
a ()
{
  typedef struct aa {} aaa;
}
static void inita () {}
node *lasta = NULL;
b ()
{
  typedef  int bb;
}
static void initb () {}
node *lastb = NULL;
typedef enum { REG_ENOSYS = -1, aa } reg_errcode_t;
