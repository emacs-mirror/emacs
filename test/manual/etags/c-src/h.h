typedef enum
{
   ELEM_I/**< Comment Element i
              Second comment line. */
} Fails_t;
typedef void Lang_function ();
void Asm_labels ();
typedef struct tpcmd
{
#define ggg hhh
  union
  {
  } arg;
}
tpcmd;
typedef struct foobar2_ {
    fu   int (*funcptr) (void *ptr);
    long foo;
    char bar;
} foobar2;
typedef enum
{
    DEVICE_SWP,
    DEVICE_LAST
} bsp_DevId;
typedef union {
  struct constant_args {
    unsigned int burst;
  } constant;
} args;
typedef int *regset;
typedef int INT;
typedef union abc
{
  int def;
} ghi1;
typedef union abc {
  int def;
} ghi2;
typedef struct a {
} b;
#define c() d
typedef struct an_extern_linkage *an_extern_linkage_ptr;
typedef struct an_extern_linkage {
  a_name_linkage_kind
		kind;
			/* The kind of external linkage ("C++" or "C"). */
  a_byte_boolean
		is_explicit;
			/* TRUE if the external linkage requirement is
			   explicitly specified in the source; FALSE for the
			   default set for the translation unit as a whole. */
#ifdef CL_CHANGES
  a_byte_boolean is_curly_brace_form;
#endif
} an_extern_linkage;
typedef struct pollfd   pfdset[FD_SETSIZE];
typedef union rtunion_def
  {
    int rtint;
    char *rtstr;
    struct rtx_def *rtx;
  } womboid ;
typedef union rtunion_def

{

  int rtint;
  char *rtstr;
  struct rtx_def *rtxp;
  struct rtx_def rtxnp;

}

womboid

;


/* Leave the next two lines in that order.  They exercise an old bug. */
enum {dog, cat} animals;
typedef void (_CALLBACK_ *signal_handler)(int);
typedef void (_CALLBACK_ *signal_handler1)(int);
/* comment */ #define ANSIC
 #define ANSIC
 #else
typedef void (proc) ();
typedef void OperatorFun(int opId);
typedef int f(int a,
              int b);
struct my_struct {
};
typedef struct my_struct my_typedef;
typedef RETSIGTYPE (*signal_handler_t) (int);
#if 0
  Date 04 May 87 235311 PDT (Mon)
  Date: 04 May 87 23:53:11 PDT (Mon)
#endif
typedef unsigned char unchar;
typedef int X, Y, Z;
typedef mio mao;
extern void ab();
typedef struct a { } b;
typedef struct b
{
} c;
int	(*oldhup)();
request (*oldhup) ();
int extvar;
#define tag1
#define aaaaaa \
bbbbbb
#define bbbbbb\
cccccc
#define cccccccccc
#define enter_critical_section		do { int pri = spl7();
#define exit_critical_to_previous	splarg (pri); } while (0)
#define UNDEFINED
struct re_pattern_buffer { unsigned char *buffer; };
