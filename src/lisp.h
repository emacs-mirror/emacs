/* Fundamental definitions for GNU Emacs Lisp interpreter.
   Copyright (C) 1985,86,87,93,94,95 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* These are default choices for the types to use.  */
#ifndef EMACS_INT
#define EMACS_INT int
#endif
#ifndef EMACS_UINT
#define EMACS_UINT unsigned int
#endif

/* Define the fundamental Lisp data structures.  */

/* This is the set of Lisp data types.  */

enum Lisp_Type
  {
    /* Integer.  XINT (obj) is the integer value.  */
    Lisp_Int,

    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.  */
    Lisp_Symbol,

    /* Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
       whose first member indicates the subtype.  */
    Lisp_Misc,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein.  */
    Lisp_String,

    /* Vector of Lisp objects, or something resembling it.
       XVECTOR (object) points to a struct Lisp_Vector, which contains
       the size and contents.  The size field also contains the type
       information, if it's not a real vector object.  */
    Lisp_Vectorlike,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons.  */
    Lisp_Cons,

#ifdef LISP_FLOAT_TYPE
    Lisp_Float,
#endif /* LISP_FLOAT_TYPE */

    /* This is not a type code.  It is for range checking.  */
    Lisp_Type_Limit
  };

/* This is the set of datatypes that share a common structure.
   The first member of the structure is a type code from this set.
   The enum values are arbitrary, but we'll use large numbers to make it
   more likely that we'll spot the error if a random word in memory is
   mistakenly interpreted as a Lisp_Misc.  */
enum Lisp_Misc_Type
  {
    Lisp_Misc_Free = 0x5eab,
    Lisp_Misc_Marker,
    Lisp_Misc_Intfwd,
    Lisp_Misc_Boolfwd,
    Lisp_Misc_Objfwd,
    Lisp_Misc_Buffer_Objfwd,
    Lisp_Misc_Buffer_Local_Value,
    Lisp_Misc_Some_Buffer_Local_Value,
    Lisp_Misc_Overlay,
    Lisp_Misc_Display_Objfwd,
    /* Currently floats are not a misc type,
       but let's define this in case we want to change that.  */
    Lisp_Misc_Float,
    /* This is not a type code.  It is for range checking.  */
    Lisp_Misc_Limit
  };

/* These values are overridden by the m- file on some machines.  */
#ifndef VALBITS
#define VALBITS 28
#endif

#ifndef GCTYPEBITS
#define GCTYPEBITS 3
#endif

#ifndef NO_UNION_TYPE

#ifndef WORDS_BIG_ENDIAN

/* Definition of Lisp_Object for little-endian machines.  */

typedef
union Lisp_Object
  {
    /* Used for comparing two Lisp_Objects;
       also, positive integers can be accessed fast this way.  */
    int i;

    struct
      {
	int val: VALBITS;
	int type: GCTYPEBITS+1;
      } s;
    struct
      {
	unsigned int val: VALBITS;
	int type: GCTYPEBITS+1;
      } u;
    struct
      {
	unsigned int val: VALBITS;
	enum Lisp_Type type: GCTYPEBITS;
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned int markbit: 1;
      } gu;
  }
Lisp_Object;

#else /* If WORDS_BIG_ENDIAN */

typedef
union Lisp_Object
  {
    /* Used for comparing two Lisp_Objects;
       also, positive integers can be accessed fast this way.  */
    int i;

    struct
      {
	int type: GCTYPEBITS+1;
	int val: VALBITS;
      } s;
    struct
      {
	int type: GCTYPEBITS+1;
	unsigned int val: VALBITS;
      } u;
    struct
      {
	/* The markbit is not really part of the value of a Lisp_Object,
	   and is always zero except during garbage collection.  */
	unsigned int markbit: 1;
	enum Lisp_Type type: GCTYPEBITS;
	unsigned int val: VALBITS;
      } gu;
  }
Lisp_Object;

#endif /* WORDS_BIG_ENDIAN */

#endif /* NO_UNION_TYPE */


/* If union type is not wanted, define Lisp_Object as just a number
   and define the macros below to extract fields by shifting */

#ifdef NO_UNION_TYPE

#define Lisp_Object EMACS_INT

#ifndef VALMASK
#define VALMASK ((((EMACS_INT) 1)<<VALBITS) - 1)
#endif
#define GCTYPEMASK ((((EMACS_INT) 1)<<GCTYPEBITS) - 1)

/* Two flags that are set during GC.  On some machines, these flags
   are defined differently by the m- file.  */

/* This is set in the car of a cons and in the plist slot of a symbol
   to indicate it is marked.  Likewise in the plist slot of an interval,
   the chain slot of a marker, the type slot of a float, and the name
   slot of a buffer.

   In strings, this bit in the size field indicates that the string
   is a "large" one, one which was separately malloc'd
   rather than being part of a string block.  */

#ifndef MARKBIT
#define MARKBIT (1 << (VALBITS + GCTYPEBITS))
#endif /*MARKBIT */

/* In the size word of a vector, this bit means the vector has been marked.
   In the size word of a large string, likewise.  */

#ifndef ARRAY_MARK_FLAG
#define ARRAY_MARK_FLAG ((MARKBIT >> 1) & ~MARKBIT)
#endif /* no ARRAY_MARK_FLAG */

/* In the size word of a struct Lisp_Vector, this bit means it's really
   some other vector-like object.  */
#ifndef PSEUDOVECTOR_FLAG
#define PSEUDOVECTOR_FLAG ((ARRAY_MARK_FLAG >> 1) & ~ARRAY_MARK_FLAG)
#endif

/* In a pseudovector, the size field actually contains a word with one
   PSEUDOVECTOR_FLAG bit set, and exactly one of the following bits to
   indicate the actual type.  */
enum pvec_type
{
  PVEC_NORMAL_VECTOR = 0,
  PVEC_BUFFER = 0x100,
  PVEC_PROCESS = 0x200,
  PVEC_FRAME = 0x400,
  PVEC_COMPILED = 0x800,
  PVEC_WINDOW = 0x1000,
  PVEC_WINDOW_CONFIGURATION = 0x2000,
  PVEC_SUBR = 0x4000,
  PVEC_TYPE_MASK = 0x7f00,
  PVEC_FLAG = PSEUDOVECTOR_FLAG,
};

/* For convenience, we also store the number of elements in these bits.  */
#define PSEUDOVECTOR_SIZE_MASK 0xff

#if ARRAY_MARK_FLAG == MARKBIT || PSEUDOVECTOR_FLAG == ARRAY_MARK_FLAG || PSEUDOVECTOR_FLAG == MARKBIT
you lose
#endif

#endif /* NO_UNION_TYPE */

/* These macros extract various sorts of values from a Lisp_Object.
 For example, if tem is a Lisp_Object whose type is Lisp_Cons,
 XCONS (tem) is the struct Lisp_Cons * pointing to the memory for that cons.  */

#ifdef NO_UNION_TYPE

/* One need to override this if there must be high bits set in data space
   (doing the result of the below & ((1 << (GCTYPE + 1)) - 1) would work
    on all machines, but would penalise machines which don't need it)
 */
#ifndef XTYPE
#define XTYPE(a) ((enum Lisp_Type) ((a) >> VALBITS))
#endif

#ifndef XSETTYPE
#define XSETTYPE(a, b) ((a)  =  XUINT (a) | ((EMACS_INT)(b) << VALBITS))
#endif

/* For integers known to be positive, XFASTINT provides fast retrieval
   and XSETFASTINT provides fast storage.  This takes advantage of the
   fact that Lisp_Int is 0.  */
#define XFASTINT(a) ((a) + 0)
#define XSETFASTINT(a, b) ((a) = (b))

/* Extract the value of a Lisp_Object as a signed integer.  */

#ifndef XINT   /* Some machines need to do this differently.  */
#define XINT(a) (((a) << (INTBITS-VALBITS)) >> (INTBITS-VALBITS))
#endif

/* Extract the value as an unsigned integer.  This is a basis
   for extracting it as a pointer to a structure in storage.  */

#ifndef XUINT
#define XUINT(a) ((a) & VALMASK)
#endif

#ifndef XPNTR
#ifdef HAVE_SHM
/* In this representation, data is found in two widely separated segments.  */
extern int pure_size;
#define XPNTR(a) \
  (XUINT (a) | (XUINT (a) > pure_size ? DATA_SEG_BITS : PURE_SEG_BITS))
#else /* not HAVE_SHM */
#ifdef DATA_SEG_BITS
/* This case is used for the rt-pc.
   In the diffs I was given, it checked for ptr = 0
   and did not adjust it in that case.
   But I don't think that zero should ever be found
   in a Lisp object whose data type says it points to something.  */
#define XPNTR(a) (XUINT (a) | DATA_SEG_BITS)
#else
#define XPNTR(a) XUINT (a)
#endif
#endif /* not HAVE_SHM */
#endif /* no XPNTR */

#ifndef XSET
#define XSET(var, type, ptr) \
   ((var) = ((EMACS_INT)(type) << VALBITS) + ((EMACS_INT) (ptr) & VALMASK))
#endif

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT accesses the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */

#ifndef XGCTYPE
#define XGCTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#endif

#if VALBITS + GCTYPEBITS == INTBITS - 1
/* Make XMARKBIT faster if mark bit is sign bit.  */
#ifndef XMARKBIT
#define XMARKBIT(a) ((a) < 0)
#endif
#endif /* markbit is sign bit */

#ifndef XMARKBIT
#define XMARKBIT(a) ((a) & MARKBIT)
#endif

#ifndef XSETMARKBIT
#define XSETMARKBIT(a,b) ((a) = ((a) & ~MARKBIT) | ((b) ? MARKBIT : 0))
#endif

#ifndef XMARK
#define XMARK(a) ((a) |= MARKBIT)
#endif

#ifndef XUNMARK
#define XUNMARK(a) ((a) &= ~MARKBIT)
#endif

#endif /* NO_UNION_TYPE */

#ifndef NO_UNION_TYPE

#define XTYPE(a) ((enum Lisp_Type) (a).u.type)
#define XSETTYPE(a, b) ((a).u.type = (char) (b))

/* For integers known to be positive, XFASTINT provides fast retrieval
   and XSETFASTINT provides fast storage.  This takes advantage of the
   fact that Lisp_Int is 0.  */
#define XFASTINT(a) ((a).i + 0)
#define XSETFASTINT(a, b) ((a).i = (b))

#ifdef EXPLICIT_SIGN_EXTEND
/* Make sure we sign-extend; compilers have been known to fail to do so.  */
#define XINT(a) (((a).i << (INTBITS-VALBITS)) >> (INTBITS-VALBITS))
#else
#define XINT(a) ((a).s.val)
#endif /* EXPLICIT_SIGN_EXTEND */

#define XUINT(a) ((a).u.val)
#define XPNTR(a) ((a).u.val)

#define XSET(var, vartype, ptr) \
   (((var).s.type = ((char) (vartype))), ((var).s.val = ((int) (ptr))))

/* During garbage collection, XGCTYPE must be used for extracting types
 so that the mark bit is ignored.  XMARKBIT access the markbit.
 Markbits are used only in particular slots of particular structure types.
 Other markbits are always zero.
 Outside of garbage collection, all mark bits are always zero.  */

#define XGCTYPE(a) ((a).gu.type)
#define XMARKBIT(a) ((a).gu.markbit)
#define XSETMARKBIT(a,b) (XMARKBIT(a) = (b))
#define XMARK(a) (XMARKBIT(a) = 1)
#define XUNMARK(a) (XMARKBIT(a) = 0)

#endif /* NO_UNION_TYPE */

/* Extract a value or address from a Lisp_Object.  */

#define XCONS(a) ((struct Lisp_Cons *) XPNTR(a))
#define XVECTOR(a) ((struct Lisp_Vector *) XPNTR(a))
#define XSTRING(a) ((struct Lisp_String *) XPNTR(a))
#define XSYMBOL(a) ((struct Lisp_Symbol *) XPNTR(a))
#define XFLOAT(a) ((struct Lisp_Float *) XPNTR(a))

/* Misc types.  */
#define XMISC(a)   ((union Lisp_Misc *) XPNTR(a))
#define XMARKER(a) (&(XMISC(a)->u_marker))
#define XINTFWD(a) (&(XMISC(a)->u_intfwd))
#define XBOOLFWD(a) (&(XMISC(a)->u_boolfwd))
#define XOBJFWD(a) (&(XMISC(a)->u_objfwd))
#define XBUFFER_OBJFWD(a) (&(XMISC(a)->u_buffer_objfwd))
#define XBUFFER_LOCAL_VALUE(a) (&(XMISC(a)->u_buffer_local_value))
#define XOVERLAY(a) (&(XMISC(a)->u_overlay))
#define XDISPLAY_OBJFWD(a) (&(XMISC(a)->u_display_objfwd))

/* Pseudovector types.  */
#define XPROCESS(a) ((struct Lisp_Process *) XPNTR(a))
#define XWINDOW(a) ((struct window *) XPNTR(a))
#define XSUBR(a) ((struct Lisp_Subr *) XPNTR(a))
#define XBUFFER(a) ((struct buffer *) XPNTR(a))


/* Construct a Lisp_Object from a value or address.  */

#define XSETINT(a, b) XSET (a, Lisp_Int, b)
#define XSETCONS(a, b) XSET (a, Lisp_Cons, b)
#define XSETVECTOR(a, b) XSET (a, Lisp_Vectorlike, b)
#define XSETSTRING(a, b) XSET (a, Lisp_String, b)
#define XSETSYMBOL(a, b) XSET (a, Lisp_Symbol, b)
#define XSETFLOAT(a, b) XSET (a, Lisp_Float, b)

/* Misc types.  */
#define XSETMISC(a, b) XSET (a, Lisp_Misc, b)
#define XSETMARKER(a, b) (XSETMISC (a, b), XMISC (a)->type = Lisp_Misc_Marker)

/* Pseudovector types.  */
#define XSETPSEUDOVECTOR(a, b, code) \
  (XSETVECTOR (a, b), XVECTOR (a)->size |= PSEUDOVECTOR_FLAG | (code))
#define XSETWINDOW_CONFIGURATION(a, b) \
  (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW_CONFIGURATION))
#define XSETPROCESS(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_PROCESS))
#define XSETWINDOW(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW))
#define XSETSUBR(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_SUBR))
#define XSETCOMPILED(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_COMPILED))
#define XSETBUFFER(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_BUFFER))

#ifdef USE_TEXT_PROPERTIES
/* Basic data type for use of intervals.  See the macros in intervals.h.  */

struct interval
{
  /* The first group of entries deal with the tree structure.  */

  unsigned int total_length;	/* Length of myself and both children.  */
  unsigned int position;	/* Cache of interval's character position.  */
  struct interval *left;	/* Intervals which precede me.  */
  struct interval *right;	/* Intervals which succeed me.  */

  /* Parent in the tree, or the Lisp_Object containing this interval tree.

     The mark bit on the root interval of an interval tree says
     whether we have started (and possibly finished) marking the
     tree.  If GC comes across an interval tree whose root's parent
     field has its markbit set, it leaves the tree alone.

     You'd think we could store this information in the parent object
     somewhere (after all, that should be visited once and then
     ignored too, right?), but strings are GC'd strangely.  */
  struct interval *parent;

  /* The remaining components are `properties' of the interval.
     The first four are duplicates for things which can be on the list,
     for purposes of speed.  */

  unsigned char write_protect;	    /* Non-zero means can't modify.  */
  unsigned char visible;	    /* Zero means don't display.  */
  unsigned char front_sticky;	    /* Non-zero means text inserted just
				       before this interval goes into it.  */
  unsigned char rear_sticky;	    /* Likewise for just after it.  */

  /* Properties of this interval.
     The mark bit on this field says whether this particular interval
     tree node has been visited.  Since intervals should never be
     shared, GC aborts if it seems to have visited an interval twice.  */
  Lisp_Object plist;
};

typedef struct interval *INTERVAL;

/* Complain if object is not string or buffer type */
#define CHECK_STRING_OR_BUFFER(x, i) \
  { if (!STRINGP ((x)) && !BUFFERP ((x))) \
      x = wrong_type_argument (Qbuffer_or_string_p, (x)); }

/* Macro used to conditionally compile intervals into certain data
   structures.  See, e.g., struct Lisp_String below.  */
#define DECLARE_INTERVALS INTERVAL intervals;

/* Macro used to conditionally compile interval initialization into
   certain code.  See, e.g., alloc.c.  */
#define INITIALIZE_INTERVAL(ptr,val) ptr->intervals = val

#else  /* No text properties */

/* If no intervals are used, make the above definitions go away.  */

#define CHECK_STRING_OR_BUFFER(x, i)

#define INTERVAL
#define DECLARE_INTERVALS
#define INITIALIZE_INTERVAL(ptr,val)

#endif /* USE_TEXT_PROPERTIES */

#define ECHOBUFSIZE 300
/* All of the per-display objects, packaged together in a struct.  */
typedef struct PERDISPLAY PERDISPLAY;
struct PERDISPLAY
  {
    PERDISPLAY *next_perdisplay;
    Lisp_Object Vprefix_arg;
    Lisp_Object Vcurrent_prefix_arg;

#ifdef MULTI_FRAME
    /* The frame in which the last input event occurred, or Qmacro if the
       last event came from a macro.  We use this to determine when to
       generate switch-frame events.  This may be cleared by functions
       like Fselect_frame, to make sure that a switch-frame event is
       generated by the next character.  */
    Lisp_Object internal_last_event_frame;
#endif

    /* A user-visible version of the above, intended to allow users to
       figure out where the last event came from, if the event doesn't
       carry that information itself (i.e. if it was a character).  */
    Lisp_Object Vlast_event_frame;

    /* Placeholder for future vars that will be moved here.  */
    Lisp_Object unused[20];

    Lisp_Object this_command_keys;

    /* Vector to GCPRO the frames and windows mentioned in kbd_buffer.

       The interrupt-level event handlers will never enqueue an event on a
       frame which is not in Vframe_list, and once an event is dequeued,
       internal_last_event_frame or the event itself points to the frame.
       So that's all fine.

       But while the event is sitting in the queue, it's completely
       unprotected.  Suppose the user types one command which will run for
       a while and then delete a frame, and then types another event at
       the frame that will be deleted, before the command gets around to
       it.  Suppose there are no references to this frame elsewhere in
       Emacs, and a GC occurs before the second event is dequeued.  Now we
       have an event referring to a freed frame, which will crash Emacs
       when it is dequeued.

       Similar things happen when an event on a scroll bar is enqueued; the
       window may be deleted while the event is in the queue.

       So, we use this vector to protect the frame_or_window field in the
       event queue.  That way, they'll be dequeued as dead frames or
       windows, but still valid lisp objects.

       If perd->kbd_buffer[i].kind != no_event, then
	 (XVECTOR (perd->kbd_buffer_frame_or_window)->contents[i]
	  == perd->kbd_buffer[i].frame_or_window.  */
    Lisp_Object kbd_buffer_frame_or_window;

    /* Circular buffer for pre-read keyboard input.  */
    struct input_event *kbd_buffer;

    /* Pointer to next available character in kbd_buffer.
       If kbd_fetch_ptr == kbd_store_ptr, the buffer is empty.
       This may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the the
       next available char is in kbd_buffer[0].  */
    struct input_event *kbd_fetch_ptr;

    /* Pointer to next place to store character in kbd_buffer.  This
       may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the next
       character should go in kbd_buffer[0].  */
    volatile struct input_event *kbd_store_ptr;

    /* The above pair of variables forms a "queue empty" flag.  When we
       enqueue a non-hook event, we increment kbd_store_ptr.  When we
       dequeue a non-hook event, we increment kbd_fetch_ptr.  We say that
       there is input available iff the two counters are not equal.

       Why not just have a flag set and cleared by the enqueuing and
       dequeuing functions?  Such a flag could be screwed up by interrupts
       at inopportune times.  */

    int this_command_key_count;

    /* Nonzero means echo each character as typed.  */
    int immediate_echo;

    /* If we have echoed a prompt string specified by the user,
       this is its length.  Otherwise this is -1.  */
    int echo_after_prompt;

    /* Where to append more text to echobuf if we want to.  */
    char *echoptr;

    /* The text we're echoing in the modeline - partial key sequences,
       usually.  '\0'-terminated.  This really shouldn't have a fixed size.  */
    char echobuf[ECHOBUFSIZE];
  };

#ifdef MULTI_PERDISPLAY
/* The perdisplay object associated with a particular frame.  */
extern PERDISPLAY *get_perdisplay ();

/* The perdisplay object associated with the currently executing command.  */
extern PERDISPLAY *current_perdisplay;

/* A list of all perdisplay objects, linked through next_perdisplay.  */
extern PERDISPLAY *all_perdisplays;
#else
extern PERDISPLAY the_only_perdisplay;
#define get_perdisplay(f) (&the_only_perdisplay)
#define current_perdisplay (&the_only_perdisplay)
#define all_perdisplays (&the_only_perdisplay)
#endif

/* In a cons, the markbit of the car is the gc mark bit */

struct Lisp_Cons
  {
    Lisp_Object car, cdr;
  };

/* Like a cons, but records info on where the text lives that it was read from */
/* This is not really in use now */

struct Lisp_Buffer_Cons
  {
    Lisp_Object car, cdr;
    struct buffer *buffer;
    int bufpos;
  };

/* In a string or vector, the sign bit of the `size' is the gc mark bit */

struct Lisp_String
  {
    EMACS_INT size;
    DECLARE_INTERVALS		/* `data' field must be last.  */
    unsigned char data[1];
  };

/* If a struct is made to look like a vector, this macro returns the length
   of that vector.  */
#define VECSIZE(type) ((sizeof (type) - (sizeof (struct Lisp_Vector)	\
					 - sizeof (Lisp_Object)))	\
		       / sizeof (Lisp_Object))

struct Lisp_Vector
  {
    EMACS_INT size;
    struct Lisp_Vector *next;
    Lisp_Object contents[1];
  };

/* In a symbol, the markbit of the plist is used as the gc mark bit */

struct Lisp_Symbol
  {
    struct Lisp_String *name;
    Lisp_Object value;
    Lisp_Object function;
    Lisp_Object plist;
    struct Lisp_Symbol *next;	/* -> next symbol in this obarray bucket */
  };

/* This structure describes a built-in function.
   It is generated by the DEFUN macro only.
   defsubr makes it into a Lisp object.

   This type is treated in most respects as a pseudovector,
   but since we never dynamically allocate or free them,
   we don't need a next-vector field.  */
   
struct Lisp_Subr
  {
    EMACS_INT size;
    Lisp_Object (*function) ();
    short min_args, max_args;
    char *symbol_name;
    char *prompt;
    char *doc;
  };

/* These structures are used for various misc types.  */

/* A miscellaneous object, when it's on the free list.  */
struct Lisp_Free
  {
    int type : 16;	/* = Lisp_Misc_Free */
    int spacer : 16;
    union Lisp_Misc *chain;
  };

/* In a marker, the markbit of the chain field is used as the gc mark bit */
struct Lisp_Marker
  {
    int type : 16;	/* = Lisp_Misc_Marker */
    int spacer : 15;
    /* 1 means normal insertion at the marker's position
       leaves the marker after the inserted text.  */
    int insertion_type : 1;
    struct buffer *buffer;
    Lisp_Object chain;
    int bufpos;
  };

/* Forwarding pointer to an int variable.
   This is allowed only in the value cell of a symbol,
   and it means that the symbol's value really lives in the
   specified int variable.  */
struct Lisp_Intfwd
  {
    int type : 16;	/* = Lisp_Misc_Intfwd */
    int spacer : 16;
    int *intvar;
  };

/* Boolean forwarding pointer to an int variable.
   This is like Lisp_Intfwd except that the ostensible
   "value" of the symbol is t if the int variable is nonzero,
   nil if it is zero.  */
struct Lisp_Boolfwd
  {
    int type : 16;	/* = Lisp_Misc_Boolfwd */
    int spacer : 16;
    int *boolvar;
  };

/* Forwarding pointer to a Lisp_Object variable.
   This is allowed only in the value cell of a symbol,
   and it means that the symbol's value really lives in the
   specified variable.  */
struct Lisp_Objfwd
  {
    int type : 16;	/* = Lisp_Misc_Objfwd */
    int spacer : 16;
    Lisp_Object *objvar;
  };

/* Like Lisp_Objfwd except that value lives in a slot in the
   current buffer.  Value is byte index of slot within buffer.  */
struct Lisp_Buffer_Objfwd
  {
    int type : 16;	/* = Lisp_Misc_Buffer_Objfwd */
    int spacer : 16;
    int offset;
  };

/* Used in a symbol value cell when the symbol's value is per-buffer.
   The actual contents resemble a cons cell which starts a list like this:
   (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE).

   The cons-like structure is for historical reasons; it might be better
   to just put these elements into the struct, now.

   BUFFER is the last buffer for which this symbol's value was
   made up to date.

   CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
   local_var_alist, that being the element whose car is this
   variable.  Or it can be a pointer to the
   (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE),
   if BUFFER does not have an element in its alist for this
   variable (that is, if BUFFER sees the default value of this
   variable).

   If we want to examine or set the value and BUFFER is current,
   we just examine or set REALVALUE.  If BUFFER is not current, we
   store the current REALVALUE value into CURRENT-ALIST-ELEMENT,
   then find the appropriate alist element for the buffer now
   current and set up CURRENT-ALIST-ELEMENT.  Then we set
   REALVALUE out of that element, and store into BUFFER.

   If we are setting the variable and the current buffer does not
   have an alist entry for this variable, an alist entry is
   created.

   Note that REALVALUE can be a forwarding pointer.  Each time it
   is examined or set, forwarding must be done.  Each time we
   switch buffers, buffer-local variables which forward into C
   variables are swapped immediately, so the C code can assume
   that they are always up to date.

   Lisp_Misc_Buffer_Local_Value and Lisp_Misc_Some_Buffer_Local_Value
   use the same substructure.  The difference is that with the latter,
   merely setting the variable while some buffer is current
   does not cause that buffer to have its own local value of this variable.
   Only make-local-variable does that.  */
struct Lisp_Buffer_Local_Value
  {
    int type : 16; /* = Lisp_Misc_Buffer_Local_Value
				      or Lisp_Misc_Some_Buffer_Local_Value */
    int spacer : 16;
    Lisp_Object car, cdr;
  };

/* In an overlay object, the mark bit of the plist is used as the GC mark.
   START and END are markers in the overlay's buffer, and
   PLIST is the overlay's property list.  */
struct Lisp_Overlay
  {
    int type : 16;	/* = Lisp_Misc_Overlay */
    int spacer : 16;
    Lisp_Object start, end, plist;
  };

/* Like Lisp_Objfwd except that value lives in a slot in the
   current perdisplay.  */
struct Lisp_Display_Objfwd
  {
    int type : 16;	/* = Lisp_Misc_Display_Objfwd */
    int spacer : 16;
    int offset;
  };


union Lisp_Misc
  {
    int type : 16;
    struct Lisp_Free u_free;
    struct Lisp_Marker u_marker;
    struct Lisp_Intfwd u_intfwd;
    struct Lisp_Boolfwd u_boolfwd;
    struct Lisp_Objfwd u_objfwd;
    struct Lisp_Buffer_Objfwd u_buffer_objfwd;
    struct Lisp_Buffer_Local_Value u_buffer_local_value;
    struct Lisp_Overlay u_overlay;
    struct Lisp_Display_Objfwd u_display_objfwd;
  };

#ifdef LISP_FLOAT_TYPE
/* Optional Lisp floating point type */
struct Lisp_Float
  {
    Lisp_Object type;		/* essentially used for mark-bit 
				   and chaining when on free-list */
    double data;  
  };
#endif /* LISP_FLOAT_TYPE */

/* A character, declared with the following typedef, is a member
   of some character set associated with the current buffer.  */
#ifndef _UCHAR_T  /* Protect against something in ctab.h on AIX.  */
#define _UCHAR_T
typedef unsigned char UCHAR;
#endif

/* Meanings of slots in a Lisp_Compiled:  */

#define COMPILED_ARGLIST 0
#define COMPILED_BYTECODE 1
#define COMPILED_CONSTANTS 2
#define COMPILED_STACK_DEPTH 3
#define COMPILED_DOC_STRING 4
#define COMPILED_INTERACTIVE 5

/* Flag bits in a character.  These also get used in termhooks.h.
   Richard Stallman <rms@gnu.ai.mit.edu> thinks that MULE
   (MUlti-Lingual Emacs) might need 22 bits for the character value
   itself, so we probably shouldn't use any bits lower than 0x0400000.  */
#define CHAR_ALT   (0x0400000)
#define CHAR_SUPER (0x0800000)
#define CHAR_HYPER (0x1000000)
#define CHAR_SHIFT (0x2000000)
#define CHAR_CTL   (0x4000000)
#define CHAR_META  (0x8000000)

#ifdef USE_X_TOOLKIT
#ifdef NO_UNION_TYPE
/* Use this for turning a (void *) into a Lisp_Object, as when the
   Lisp_Object is passed into a toolkit callback function.  */
#define VOID_TO_LISP(larg,varg) \
  do { ((larg) = ((Lisp_Object) (varg))); } while (0)
#define CVOID_TO_LISP VOID_TO_LISP

/* Use this for turning a Lisp_Object into a  (void *), as when the
   Lisp_Object is passed into a toolkit callback function.  */
#define LISP_TO_VOID(larg) ((void *) (larg))
#define LISP_TO_CVOID(varg) ((const void *) (larg))

#else /* not NO_UNION_TYPE */
/* Use this for turning a (void *) into a Lisp_Object, as when the
  Lisp_Object is passed into a toolkit callback function.  */
#define VOID_TO_LISP(larg,varg) \
  do { ((larg).v = (void *) (varg)); } while (0)
#define CVOID_TO_LISP(larg,varg) \
  do { ((larg).cv = (const void *) (varg)); } while (0)

/* Use this for turning a Lisp_Object into a  (void *), as when the
   Lisp_Object is passed into a toolkit callback function.  */
#define LISP_TO_VOID(larg) ((larg).v)
#define LISP_TO_CVOID(larg) ((larg).cv)
#endif /* not NO_UNION_TYPE */
#endif /* USE_X_TOOLKIT */


/* The glyph datatype, used to represent characters on the display.  */

/* The low eight bits are the character code, and the bits above them
   are the numeric face ID.  If FID is the face ID of a glyph on a
   frame F, then F->display.x->faces[FID] contains the description of
   that face.  This is an int instead of a short, so we can support a
   good bunch of face ID's; given that we have no mechanism for
   tossing unused frame face ID's yet, we'll probably run out of 255
   pretty quickly.  */
#define GLYPH unsigned int

#ifdef HAVE_FACES
/* The FAST macros assume that we already know we're in an X window.  */

/* Given a character code and a face ID, return the appropriate glyph.  */
#define FAST_MAKE_GLYPH(char, face) ((char) | ((face) << 8))

/* Return a glyph's character code.  */
#define FAST_GLYPH_CHAR(glyph) ((glyph) & 0xff)

/* Return a glyph's face ID.  */
#define FAST_GLYPH_FACE(glyph) (((glyph) >> 8) & ((1 << 24) - 1))

/* Slower versions that test the frame type first.  */
#define MAKE_GLYPH(f, char, face) (FRAME_TERMCAP_P (f) ? (char) \
				   : FAST_MAKE_GLYPH (char, face))
#define GLYPH_CHAR(f, g) (FRAME_TERMCAP_P (f) ? (g) : FAST_GLYPH_CHAR (g))
#define GLYPH_FACE(f, g) (FRAME_TERMCAP_P (f) ? (0) : FAST_GLYPH_FACE (g))
#else /* not HAVE_FACES */
#define MAKE_GLYPH(f, char, face) (char)
#define GLYPH_CHAR(f, g) (g)
#define GLYPH_FACE(f, g) (g)
#endif /* not HAVE_FACES */

/* The ID of the mode line highlighting face.  */
#define GLYPH_MODE_LINE_FACE 1

/* Data type checking */

#define NILP(x)  (XFASTINT (x) == XFASTINT (Qnil))
#define GC_NILP(x) GC_EQ (x, Qnil)

#ifdef LISP_FLOAT_TYPE
#define NUMBERP(x) (INTEGERP (x) || FLOATP (x))
#define GC_NUMBERP(x) (GC_INTEGERP (x) || GC_FLOATP (x))
#else
#define NUMBERP(x) (INTEGERP (x))
#define GC_NUMBERP(x) (GC_INTEGERP (x))
#endif
#define NATNUMP(x) (INTEGERP (x) && XINT (x) >= 0)
#define GC_NATNUMP(x) (GC_INTEGERP (x) && XINT (x) >= 0)

#define INTEGERP(x) (XTYPE ((x)) == Lisp_Int)
#define GC_INTEGERP(x) (XGCTYPE ((x)) == Lisp_Int)
#define SYMBOLP(x) (XTYPE ((x)) == Lisp_Symbol)
#define GC_SYMBOLP(x) (XGCTYPE ((x)) == Lisp_Symbol)
#define MISCP(x) (XTYPE ((x)) == Lisp_Misc)
#define GC_MISCP(x) (XGCTYPE ((x)) == Lisp_Misc)
#define VECTORLIKEP(x) (XTYPE ((x)) == Lisp_Vectorlike)
#define GC_VECTORLIKEP(x) (XGCTYPE ((x)) == Lisp_Vectorlike)
#define STRINGP(x) (XTYPE ((x)) == Lisp_String)
#define GC_STRINGP(x) (XGCTYPE ((x)) == Lisp_String)
#define CONSP(x) (XTYPE ((x)) == Lisp_Cons)
#define GC_CONSP(x) (XGCTYPE ((x)) == Lisp_Cons)

#ifdef LISP_FLOAT_TYPE
#define FLOATP(x) (XTYPE ((x)) == Lisp_Float)
#define GC_FLOATP(x) (XGCTYPE ((x)) == Lisp_Float)
#else
#define FLOATP(x) (0)
#define GC_FLOATP(x) (0)
#endif
#define VECTORP(x) (VECTORLIKEP (x) && !(XVECTOR (x)->size & PSEUDOVECTOR_FLAG))
#define GC_VECTORP(x) (GC_VECTORLIKEP (x) && !(XVECTOR (x)->size & PSEUDOVECTOR_FLAG))
#define OVERLAYP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Overlay)
#define GC_OVERLAYP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Overlay)
#define MARKERP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Marker)
#define GC_MARKERP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Marker)
#define INTFWDP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Intfwd)
#define GC_INTFWDP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Intfwd)
#define BOOLFWDP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Boolfwd)
#define GC_BOOLFWDP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Boolfwd)
#define OBJFWDP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Objfwd)
#define GC_OBJFWDP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Objfwd)
#define BUFFER_OBJFWDP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Buffer_Objfwd)
#define GC_BUFFER_OBJFWDP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Buffer_Objfwd)
#define BUFFER_LOCAL_VALUEP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Buffer_Local_Value)
#define GC_BUFFER_LOCAL_VALUEP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Buffer_Local_Value)
#define SOME_BUFFER_LOCAL_VALUEP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Some_Buffer_Local_Value)
#define GC_SOME_BUFFER_LOCAL_VALUEP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Some_Buffer_Local_Value)
#define DISPLAY_OBJFWDP(x) (MISCP (x) && XMISC (x)->type == Lisp_Misc_Display_Objfwd)
#define GC_DISPLAY_OBJFWDP(x) (GC_MISCP (x) && XMISC (x)->type == Lisp_Misc_Display_Objfwd)


/* True if object X is a pseudovector whose code is CODE.  */
#define PSEUDOVECTORP(x, code)					\
  (VECTORLIKEP (x)						\
   && (((XVECTOR (x)->size & (PSEUDOVECTOR_FLAG | (code))))	\
       == (PSEUDOVECTOR_FLAG | (code))))

/* True if object X is a pseudovector whose code is CODE.
   This one works during GC.  */
#define GC_PSEUDOVECTORP(x, code)				\
  (GC_VECTORLIKEP (x)						\
   && (((XVECTOR (x)->size & (PSEUDOVECTOR_FLAG | (code))))	\
       == (PSEUDOVECTOR_FLAG | (code))))

/* Test for specific pseudovector types.  */
#define WINDOW_CONFIGURATIONP(x) PSEUDOVECTORP (x, PVEC_WINDOW_CONFIGURATION)
#define GC_WINDOW_CONFIGURATIONP(x) GC_PSEUDOVECTORP (x, PVEC_WINDOW_CONFIGURATION)
#define PROCESSP(x) PSEUDOVECTORP (x, PVEC_PROCESS)
#define GC_PROCESSP(x) GC_PSEUDOVECTORP (x, PVEC_PROCESS)
#define WINDOWP(x) PSEUDOVECTORP (x, PVEC_WINDOW)
#define GC_WINDOWP(x) GC_PSEUDOVECTORP (x, PVEC_WINDOW)
#define SUBRP(x) PSEUDOVECTORP (x, PVEC_SUBR)
#define GC_SUBRP(x) GC_PSEUDOVECTORP (x, PVEC_SUBR)
#define COMPILEDP(x) PSEUDOVECTORP (x, PVEC_COMPILED)
#define GC_COMPILEDP(x) GC_PSEUDOVECTORP (x, PVEC_COMPILED)
#define BUFFERP(x) PSEUDOVECTORP (x, PVEC_BUFFER)
#define GC_BUFFERP(x) GC_PSEUDOVECTORP (x, PVEC_BUFFER)

#ifdef MULTI_FRAME
#define FRAMEP(x) PSEUDOVECTORP (x, PVEC_FRAME)
#define GC_FRAMEP(x) GC_PSEUDOVECTORP (x, PVEC_FRAME)
#else
#ifdef HAVE_MOUSE
/* We could use this in the !HAVE_MOUSE case also, but we prefer a compile-time
   error message in case FRAMEP is used.  */
#define FRAMEP(x) (EQ (x, Fselected_frame ()))
#define GC_FRAMEP(x) (GC_EQ (x, Fselected_frame ()))
#endif
#endif


#define EQ(x, y) (XFASTINT (x) == XFASTINT (y))
#define GC_EQ(x, y) (XGCTYPE (x) == XGCTYPE (y) && XPNTR (x) == XPNTR (y))

#define CHECK_LIST(x, i) \
  do { if (!CONSP ((x)) && !NILP (x)) x = wrong_type_argument (Qlistp, (x)); } while (0)

#define CHECK_STRING(x, i) \
  do { if (!STRINGP ((x))) x = wrong_type_argument (Qstringp, (x)); } while (0)

#define CHECK_CONS(x, i) \
  do { if (!CONSP ((x))) x = wrong_type_argument (Qconsp, (x)); } while (0)

#define CHECK_SYMBOL(x, i) \
  do { if (!SYMBOLP ((x))) x = wrong_type_argument (Qsymbolp, (x)); } while (0)

#define CHECK_VECTOR(x, i) \
  do { if (!VECTORP ((x))) x = wrong_type_argument (Qvectorp, (x)); } while (0)

#define CHECK_BUFFER(x, i) \
  do { if (!BUFFERP ((x))) x = wrong_type_argument (Qbufferp, (x)); } while (0)

#define CHECK_WINDOW(x, i) \
  do { if (!WINDOWP ((x))) x = wrong_type_argument (Qwindowp, (x)); } while (0)

/* This macro rejects windows on the interior of the window tree as
   "dead", which is what we want; this is an argument-checking macro, and 
   the user should never get access to interior windows.

   A window of any sort, leaf or interior, is dead iff the buffer,
   vchild, and hchild members are all nil.  */

#define CHECK_LIVE_WINDOW(x, i)				\
  do {							\
    if (!WINDOWP ((x))					\
	|| NILP (XWINDOW ((x))->buffer))		\
      x = wrong_type_argument (Qwindow_live_p, (x));	\
  } while (0)

#define CHECK_PROCESS(x, i) \
  do { if (!PROCESSP ((x))) x = wrong_type_argument (Qprocessp, (x)); } while (0)

#define CHECK_NUMBER(x, i) \
  do { if (!INTEGERP ((x))) x = wrong_type_argument (Qintegerp, (x)); } while (0)

#define CHECK_NATNUM(x, i) \
  do { if (!NATNUMP (x)) x = wrong_type_argument (Qwholenump, (x)); } while (0)

#define CHECK_MARKER(x, i) \
  do { if (!MARKERP ((x))) x = wrong_type_argument (Qmarkerp, (x)); } while (0)

#define CHECK_NUMBER_COERCE_MARKER(x, i) \
  do { if (MARKERP ((x))) XSETFASTINT (x, marker_position (x)); \
    else if (!INTEGERP ((x))) x = wrong_type_argument (Qinteger_or_marker_p, (x)); } while (0)

#ifdef LISP_FLOAT_TYPE

#ifndef DBL_DIG
#define DBL_DIG 20
#endif

#define XFLOATINT(n) extract_float((n))

#define CHECK_FLOAT(x, i)		\
  do { if (!FLOATP (x))			\
    x = wrong_type_argument (Qfloatp, (x)); } while (0)

#define CHECK_NUMBER_OR_FLOAT(x, i)	\
  do { if (!FLOATP (x) && !INTEGERP (x))	\
    x = wrong_type_argument (Qnumberp, (x)); } while (0)

#define CHECK_NUMBER_OR_FLOAT_COERCE_MARKER(x, i) \
  do { if (MARKERP (x)) XSETFASTINT (x, marker_position (x));	\
  else if (!INTEGERP (x) && !FLOATP (x))		\
    x = wrong_type_argument (Qnumber_or_marker_p, (x)); } while (0)

#else  /* Not LISP_FLOAT_TYPE */

#define CHECK_NUMBER_OR_FLOAT CHECK_NUMBER

#define CHECK_NUMBER_OR_FLOAT_COERCE_MARKER CHECK_NUMBER_COERCE_MARKER

#define XFLOATINT(n) XINT((n))
#endif /* LISP_FLOAT_TYPE */

#define CHECK_OVERLAY(x, i) \
  do { if (!OVERLAYP ((x))) x = wrong_type_argument (Qoverlayp, (x));} while (0)

/* Cast pointers to this type to compare them.  Some machines want int.  */
#ifndef PNTR_COMPARISON_TYPE
#define PNTR_COMPARISON_TYPE unsigned int
#endif

/* Define a built-in function for calling from Lisp.
 `lname' should be the name to give the function in Lisp,
    as a null-terminated C string.
 `fnname' should be the name of the function in C.
    By convention, it starts with F.
 `sname' should be the name for the C constant structure
    that records information on this function for internal use.
    By convention, it should be the same as `fnname' but with S instead of F.
    It's too bad that C macros can't compute this from `fnname'.
 `minargs' should be a number, the minimum number of arguments allowed.
 `maxargs' should be a number, the maximum number of arguments allowed,
    or else MANY or UNEVALLED.
    MANY means pass a vector of evaluated arguments,
	 in the form of an integer number-of-arguments
	 followed by the address of a vector of Lisp_Objects
	 which contains the argument values.
    UNEVALLED means pass the list of unevaluated arguments
 `prompt' says how to read arguments for an interactive call.
    See the doc string for `interactive'.
    A null string means call interactively with no arguments.
 `doc' is documentation for the user.  */

#if !defined (__STDC__) || defined (USE_NONANSI_DEFUN)
#define DEFUN(lname, fnname, sname, minargs, maxargs, prompt, doc)	\
  Lisp_Object fnname ();						\
  struct Lisp_Subr sname =						\
    { PVEC_SUBR | (sizeof (struct Lisp_Subr) / sizeof (EMACS_INT)),	\
      fnname, minargs, maxargs, lname, prompt, 0};			\
  Lisp_Object fnname

#else

/* This version of DEFUN declares a function prototype with the right
   arguments, so we can catch errors with maxargs at compile-time.  */
#define DEFUN(lname, fnname, sname, minargs, maxargs, prompt, doc)	\
  Lisp_Object fnname DEFUN_ARGS_ ## maxargs ;				\
  struct Lisp_Subr sname =						\
    { PVEC_SUBR | (sizeof (struct Lisp_Subr) / sizeof (EMACS_INT)),	\
      fnname, minargs, maxargs, lname, prompt, 0};			\
  Lisp_Object fnname

/* Note that the weird token-substitution semantics of ANSI C makes
   this work for MANY and UNEVALLED.  */
#define DEFUN_ARGS_MANY		(int, Lisp_Object *)
#define DEFUN_ARGS_UNEVALLED	(Lisp_Object)
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(Lisp_Object)
#define DEFUN_ARGS_2	(Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_3	(Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_4	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_5	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object)
#define DEFUN_ARGS_6	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_7	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object, Lisp_Object, Lisp_Object)
#endif

/* defsubr (Sname);
 is how we define the symbol for function `name' at start-up time.  */
extern void defsubr ();

#define MANY -2
#define UNEVALLED -1

extern void defvar_lisp ();
extern void defvar_bool ();
extern void defvar_int ();
extern void defvar_display ();

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.  */

#define DEFVAR_LISP(lname, vname, doc) defvar_lisp (lname, vname)
#define DEFVAR_LISP_NOPRO(lname, vname, doc) defvar_lisp_nopro (lname, vname)
#define DEFVAR_BOOL(lname, vname, doc) defvar_bool (lname, vname)
#define DEFVAR_INT(lname, vname, doc) defvar_int (lname, vname)
#define DEFVAR_PER_BUFFER(lname, vname, type, doc)  \
 defvar_per_buffer (lname, vname, type, 0)
#define DEFVAR_DISPLAY(lname, vname, doc) \
 defvar_display (lname, \
		 (int)((char *)(&current_perdisplay->vname) \
		       - (char *)current_perdisplay))

/* Structure for recording Lisp call stack for backtrace purposes.  */

/* The special binding stack holds the outer values of variables while
   they are bound by a function application or a let form, stores the
   code to be executed for Lisp unwind-protect forms, and stores the C
   functions to be called for record_unwind_protect.

   If func is non-zero, undoing this binding applies func to old_value;
      This implements record_unwind_protect.
   If func is zero and symbol is nil, undoing this binding evaluates
      the list of forms in old_value; this implements Lisp's unwind-protect
      form.
   Otherwise, undoing this binding stores old_value as symbol's value; this
      undoes the bindings made by a let form or function call.  */
struct specbinding
  {
    Lisp_Object symbol, old_value;
    Lisp_Object (*func) ();
    Lisp_Object unused;		/* Dividing by 16 is faster than by 12 */
  };

extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern int specpdl_size;

/* Everything needed to describe an active condition case.  */
struct handler
  {
    /* The handler clauses and variable from the condition-case form.  */
    Lisp_Object handler;
    Lisp_Object var;
    /* Fsignal stores here the condition-case clause that applies,
       and Fcondition_case thus knows which clause to run.  */
    Lisp_Object chosen_clause;

    /* Used to effect the longjump out to the handler.  */
    struct catchtag *tag;

    /* The next enclosing handler.  */
    struct handler *next;
  };

extern struct handler *handlerlist;

extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

extern Lisp_Object memory_signal_data;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
extern char *stack_bottom;

/* Check quit-flag and quit if it is non-nil.  */

#define QUIT \
  if (!NILP (Vquit_flag) && NILP (Vinhibit_quit)) \
    { Vquit_flag = Qnil; Fsignal (Qquit, Qnil); }

/* Nonzero if ought to quit now.  */

#define QUITP (!NILP (Vquit_flag) && NILP (Vinhibit_quit))

/* 1 if CH is upper case.  */

#define UPPERCASEP(CH) \
  (XSTRING (current_buffer->downcase_table)->data[CH] != (CH))

/* 1 if CH is lower case.  */

#define LOWERCASEP(CH) \
  (!UPPERCASEP (CH) \
   && XSTRING (current_buffer->upcase_table)->data[CH] != (CH))

/* 1 if CH is neither upper nor lower case.  */

#define NOCASEP(CH) (XSTRING (current_buffer->upcase_table)->data[CH] == (CH))

/* Upcase a character, or make no change if that cannot be done.  */

#define UPCASE(CH) \
  (XSTRING (current_buffer->downcase_table)->data[CH] == (CH) \
   ? UPCASE1 (CH) : (CH))

/* Upcase a character known to be not upper case.  */

#define UPCASE1(CH) (XSTRING (current_buffer->upcase_table)->data[CH])

/* Downcase a character, or make no change if that cannot be done.  */

#define DOWNCASE(CH) (XSTRING (current_buffer->downcase_table)->data[CH])

/* Current buffer's map from characters to lower-case characters.  */

#define DOWNCASE_TABLE XSTRING (current_buffer->downcase_table)->data

/* Table mapping each char to the next char with the same lowercase version.
   This mapping is a no-op only for characters that don't have case.  */
#define UPCASE_TABLE XSTRING (current_buffer->upcase_table)->data

extern Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
extern Lisp_Object Vascii_canon_table, Vascii_eqv_table;

/* Number of bytes of structure consed since last GC.  */

extern int consing_since_gc;

/* Threshold for doing another gc.  */

extern int gc_cons_threshold;

/* Structure for recording stack slots that need marking.  */

/* This is a chain of structures, each of which points at a Lisp_Object variable
 whose value should be marked in garbage collection.
 Normally every link of the chain is an automatic variable of a function,
 and its `val' points to some argument or local variable of the function.
 On exit to the function, the chain is set back to the value it had on entry.
 This way, no link remains in the chain when the stack frame containing the
 link disappears.

 Every function that can call Feval must protect in this fashion all
 Lisp_Object variables whose contents will be used again.  */

extern struct gcpro *gcprolist;

struct gcpro
  {
    struct gcpro *next;
    Lisp_Object *var;		/* Address of first protected variable */
    int nvars;			/* Number of consecutive protected variables */
  };

#define GCPRO1(varname) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname; gcpro1.nvars = 1; \
  gcprolist = &gcpro1; }

#define GCPRO2(varname1, varname2) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcprolist = &gcpro2; }

#define GCPRO3(varname1, varname2, varname3) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcprolist = &gcpro3; }

#define GCPRO4(varname1, varname2, varname3, varname4) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcprolist = &gcpro4; }

#define GCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4; gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcprolist = &gcpro5; }

/* Call staticpro (&var) to protect static variable `var'.  */

void staticpro();
  
#define UNGCPRO (gcprolist = gcpro1.next)

/* Evaluate expr, UNGCPRO, and then return the value of expr.  */
#define RETURN_UNGCPRO(expr)			\
if (1)						\
    {						\
      Lisp_Object ret_ungc_val;			\
      ret_ungc_val = (expr);			\
      UNGCPRO;					\
      return ret_ungc_val;			\
    }						\
else

/* Defined in data.c */
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
extern Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
extern Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
extern Lisp_Object Qvoid_variable, Qvoid_function;
extern Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
extern Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern Lisp_Object Qend_of_file, Qarith_error;
extern Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
extern Lisp_Object Qmark_inactive;

extern Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
extern Lisp_Object Qoverflow_error, Qunderflow_error;

extern Lisp_Object Qintegerp, Qnumberp, Qnatnump, Qwholenump;
extern Lisp_Object Qsymbolp, Qlistp, Qconsp;
extern Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
extern Lisp_Object Qchar_or_string_p, Qmarkerp, Qvectorp;
extern Lisp_Object Qinteger_or_marker_p, Qnumber_or_marker_p;
extern Lisp_Object Qboundp, Qfboundp;
extern Lisp_Object Qbuffer_or_string_p;
extern Lisp_Object Qcdr;

#ifdef LISP_FLOAT_TYPE
extern Lisp_Object Qfloatp, Qinteger_or_floatp, Qinteger_or_float_or_marker_p;
#endif /* LISP_FLOAT_TYPE */

extern Lisp_Object Qframep;

extern Lisp_Object Feq (), Fnull (), Flistp (), Fconsp (), Fatom (), Fnlistp ();
extern Lisp_Object Fintegerp (), Fnatnump (), Fsymbolp ();
extern Lisp_Object Fvectorp (), Fstringp (), Farrayp (), Fsequencep ();
extern Lisp_Object Fbufferp (), Fmarkerp (), Fsubrp (), Fchar_or_string_p ();
extern Lisp_Object Finteger_or_marker_p ();
#ifdef LISP_FLOAT_TYPE
extern Lisp_Object Ffloatp(), Finteger_or_floatp();
extern Lisp_Object Finteger_or_float_or_marker_p(), Ftruncate();
#endif /* LISP_FLOAT_TYPE */

extern Lisp_Object Fcar (), Fcar_safe(), Fcdr (), Fcdr_safe();
extern Lisp_Object Fsetcar (), Fsetcdr ();
extern Lisp_Object Fboundp (), Ffboundp (), Fmakunbound (), Ffmakunbound ();
extern Lisp_Object Fsymbol_function (), Fsymbol_plist (), Fsymbol_name ();
extern Lisp_Object indirect_function (), Findirect_function ();
extern Lisp_Object Ffset (), Fsetplist ();
extern Lisp_Object Fsymbol_value (), find_symbol_value (), Fset ();
extern Lisp_Object Fdefault_value (), Fset_default (), Fdefault_boundp ();

extern Lisp_Object Faref (), Faset ();

extern Lisp_Object Fstring_to_number (), Fnumber_to_string ();
extern Lisp_Object Feqlsign (), Fgtr (), Flss (), Fgeq (), Fleq ();
extern Lisp_Object Fneq (), Fzerop ();
extern Lisp_Object Fplus (), Fminus (), Ftimes (), Fquo (), Frem ();
extern Lisp_Object Fmax (), Fmin ();
extern Lisp_Object Flogand (), Flogior (), Flogxor (), Flognot ();
extern Lisp_Object Flsh (), Fash ();

extern Lisp_Object Fadd1 (), Fsub1 ();

extern Lisp_Object make_number ();
extern Lisp_Object   long_to_cons ();
extern unsigned long cons_to_long ();
extern void args_out_of_range ();
extern void args_out_of_range_3 ();
extern Lisp_Object wrong_type_argument ();
extern void store_symval_forwarding ();
#ifdef LISP_FLOAT_TYPE
extern Lisp_Object Ffloat_to_int(), Fint_to_float();
extern double extract_float();
extern Lisp_Object make_float ();
extern Lisp_Object Ffloat ();
#endif /* LISP_FLOAT_TYPE */

/* Defined in fns.c */
extern Lisp_Object Qstring_lessp;
extern Lisp_Object Vfeatures;
extern Lisp_Object Fidentity (), Frandom ();
extern Lisp_Object Flength ();
extern Lisp_Object Fappend (), Fconcat (), Fvconcat (), Fcopy_sequence ();
extern Lisp_Object Fsubstring ();
extern Lisp_Object Fnth (), Fnthcdr (), Fmemq (), Fassq (), Fassoc ();
extern Lisp_Object Fmember (), Frassq (), Fdelq (), Fsort ();
extern Lisp_Object Freverse (), Fnreverse (), Fget (), Fput (), Fequal ();
extern Lisp_Object Ffillarray (), Fnconc (), Fmapcar (), Fmapconcat ();
extern Lisp_Object Fy_or_n_p (), do_yes_or_no_p ();
extern Lisp_Object Ffeaturep (), Frequire () , Fprovide ();
extern Lisp_Object concat2 (), nconc2 ();
extern Lisp_Object assq_no_quit ();
extern Lisp_Object Fcopy_alist ();

/* Defined in insdel.c */
extern void move_gap ();
extern void make_gap ();
extern void insert ();
extern void insert_and_inherit ();
extern void insert_1 ();
extern void insert_from_string ();
extern void insert_from_buffer ();
extern void insert_char ();
extern void insert_string ();
extern void insert_before_markers ();
extern void insert_before_markers_and_inherit ();
extern void insert_from_string_before_markers ();
extern void del_range ();
extern void del_range_1 ();
extern void modify_region ();
extern void prepare_to_modify_buffer ();
extern void signal_before_change ();
extern void signal_after_change ();

/* Defined in xdisp.c */
extern void message ();
extern void message1 ();
extern void message1_nolog ();
extern void message2 ();
extern void message2_nolog ();
extern void message_dolog ();
extern void message_log_maybe_newline ();

/* Defined in alloc.c */
extern Lisp_Object Vpurify_flag;
extern Lisp_Object Fcons (), Flist(), Fmake_list (), allocate_misc ();
extern Lisp_Object Fmake_vector (), Fvector (), Fmake_symbol (), Fmake_marker ();
extern Lisp_Object Fmake_string (), build_string (), make_string ();
extern Lisp_Object make_event_array (), make_uninit_string ();
extern Lisp_Object Fpurecopy (), make_pure_string ();
extern Lisp_Object pure_cons (), make_pure_vector ();
extern Lisp_Object Fgarbage_collect ();
extern Lisp_Object Fmake_byte_code ();
extern struct Lisp_Vector *allocate_vectorlike ();
extern int gc_in_progress;

/* Defined in print.c */
extern Lisp_Object Vprin1_to_string_buffer;
extern Lisp_Object Fprin1 (), Fprin1_to_string (), Fprinc ();
extern Lisp_Object Fterpri (), Fprint ();
extern Lisp_Object Vstandard_output, Qstandard_output;
extern Lisp_Object Qexternal_debugging_output;
extern void temp_output_buffer_setup (), temp_output_buffer_show ();
extern int print_level, print_escape_newlines;
extern Lisp_Object Qprint_escape_newlines;

/* Defined in lread.c */
extern Lisp_Object Qvariable_documentation, Qstandard_input;
extern Lisp_Object Vobarray, Vstandard_input;
extern Lisp_Object Fread (), Fread_from_string ();
extern Lisp_Object Fintern (), Fintern_soft (), Fload ();
extern Lisp_Object Fget_file_char (), Fread_char ();
extern Lisp_Object read_filtered_event ();
extern Lisp_Object Feval_current_buffer (), Feval_region ();
extern Lisp_Object intern (), oblookup ();
#define LOADHIST_ATTACH(x) \
 if (initialized) Vcurrent_load_list = Fcons (x, Vcurrent_load_list)
extern Lisp_Object Vcurrent_load_list;
extern Lisp_Object Vload_history;

/* Defined in eval.c */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern Lisp_Object Vinhibit_quit, Qinhibit_quit, Vquit_flag;
extern Lisp_Object Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vdebug_on_error;
/* To run a normal hook, do
   if (!NILP (Vrun_hooks))
     call1 (Vrun_hooks, Qmy_funny_hook);  */
extern Lisp_Object Vrun_hooks;
extern Lisp_Object Fand (), For (), Fif (), Fprogn (), Fprog1 (), Fprog2 ();
extern Lisp_Object Fsetq (), Fquote ();
extern Lisp_Object Fuser_variable_p (), Finteractive_p ();
extern Lisp_Object Fdefun (), Flet (), FletX (), Fwhile ();
extern Lisp_Object Fcatch (), Fthrow (), Funwind_protect ();
extern Lisp_Object Fcondition_case (), Fsignal ();
extern Lisp_Object Ffunction_type (), Fautoload (), Fcommandp ();
extern Lisp_Object Feval (), Fapply (), Ffuncall ();
extern Lisp_Object Fglobal_set (), Fglobal_value (), Fbacktrace ();
extern Lisp_Object apply1 (), call0 (), call1 (), call2 (), call3 ();
extern Lisp_Object call4 (), call5 (), call6 ();
extern Lisp_Object Fkill_emacs (), Fkey_binding (), Fsit_for ();
extern Lisp_Object Fdo_auto_save (), Fset_marker ();
extern Lisp_Object apply_lambda ();
extern Lisp_Object internal_catch ();
extern Lisp_Object internal_condition_case ();
extern Lisp_Object internal_condition_case_1 ();
extern Lisp_Object unbind_to ();
extern void error ();
extern Lisp_Object un_autoload ();

/* Defined in editfns.c */
extern Lisp_Object Fgoto_char ();
extern Lisp_Object Fpoint_min_marker (), Fpoint_max_marker ();
extern Lisp_Object Fpoint_min (), Fpoint_max ();
extern Lisp_Object Fpoint (), Fpoint_marker (), Fmark_marker ();
extern Lisp_Object Ffollowing_char (), Fprevious_char (), Fchar_after ();
extern Lisp_Object Finsert ();
extern Lisp_Object Feolp (), Feobp (), Fbolp (), Fbobp ();
extern Lisp_Object Fformat (), format1 ();
extern Lisp_Object make_buffer_string (), Fbuffer_substring ();
extern Lisp_Object Fbuffer_string ();
extern Lisp_Object Fstring_equal (), Fstring_lessp (), Fbuffer_substring_lessp ();
extern Lisp_Object save_excursion_save (), save_restriction_save ();
extern Lisp_Object save_excursion_restore (), save_restriction_restore ();
extern Lisp_Object Fchar_to_string ();

/* defined in buffer.c */
extern Lisp_Object Vbuffer_alist, Vinhibit_read_only;
extern Lisp_Object Fget_buffer (), Fget_buffer_create (), Fset_buffer ();
extern Lisp_Object Fbarf_if_buffer_read_only ();
extern Lisp_Object Fcurrent_buffer (), Fswitch_to_buffer (), Fpop_to_buffer ();
extern Lisp_Object Fother_buffer ();
extern Lisp_Object Foverlay_get ();
extern Lisp_Object Qoverlayp;
extern struct buffer *all_buffers;

/* defined in marker.c */

extern Lisp_Object Fmarker_position (), Fmarker_buffer ();
extern Lisp_Object Fcopy_marker ();

/* Defined in fileio.c */

extern Lisp_Object Qfile_error;
extern Lisp_Object Ffind_file_name_handler ();
extern Lisp_Object Ffile_name_as_directory ();
extern Lisp_Object Fexpand_file_name (), Ffile_name_nondirectory ();
extern Lisp_Object Fsubstitute_in_file_name ();
extern Lisp_Object Ffile_symlink_p ();
extern Lisp_Object Fverify_visited_file_modtime ();
extern Lisp_Object Ffile_exists_p ();
extern Lisp_Object Fdirectory_file_name ();
extern Lisp_Object Ffile_name_directory ();
extern Lisp_Object expand_and_dir_to_file ();
extern Lisp_Object Ffile_accessible_directory_p ();
extern Lisp_Object Funhandled_file_name_directory ();

/* Defined in abbrev.c */

extern Lisp_Object Vfundamental_mode_abbrev_table;

/* defined in search.c */
extern Lisp_Object Fstring_match ();
extern Lisp_Object Fscan_buffer ();
extern void restore_match_data ();

/* defined in minibuf.c */

extern Lisp_Object last_minibuf_string;
extern Lisp_Object read_minibuf (), Fcompleting_read ();
extern Lisp_Object Fread_from_minibuffer ();
extern Lisp_Object Fread_variable (), Fread_buffer (), Fread_key_sequence ();
extern Lisp_Object Fread_minibuffer (), Feval_minibuffer ();
extern Lisp_Object Fread_string (), Fread_file_name ();
extern Lisp_Object Fread_no_blanks_input ();

/* Defined in callint.c */

extern Lisp_Object Qminus, Qplus;
extern Lisp_Object Vcommand_history;
extern Lisp_Object Qcall_interactively;
extern Lisp_Object Fcall_interactively ();
extern Lisp_Object Fprefix_numeric_value ();

/* defined in casefiddle.c */

extern Lisp_Object Fdowncase (), Fupcase (), Fcapitalize ();

/* defined in keyboard.c */

extern Lisp_Object Qdisabled;
extern Lisp_Object Vhelp_form, Vtop_level;
extern Lisp_Object Fdiscard_input (), Frecursive_edit ();
extern Lisp_Object Fcommand_execute (), Finput_pending_p ();
extern Lisp_Object Qvertical_scroll_bar;

/* defined in keymap.c */

extern Lisp_Object Qkeymap, Qmenu_bar;
extern Lisp_Object current_global_map;
extern Lisp_Object Fkey_description (), Fsingle_key_description ();
extern Lisp_Object Fwhere_is_internal ();
extern Lisp_Object access_keymap (), store_in_keymap ();
extern Lisp_Object get_keyelt (), get_keymap();

/* defined in indent.c */
extern Lisp_Object Fvertical_motion (), Findent_to (), Fcurrent_column ();

/* defined in window.c */
extern Lisp_Object Qwindowp, Qwindow_live_p;
extern Lisp_Object Fget_buffer_window ();
extern Lisp_Object Fsave_window_excursion ();
extern Lisp_Object Fset_window_configuration (), Fcurrent_window_configuration ();
extern Lisp_Object Fcoordinates_in_window_p ();
extern Lisp_Object Fwindow_at ();
extern int window_internal_height (), window_internal_width ();

/* defined in frame.c */
extern Lisp_Object Qvisible;
extern Lisp_Object Fframep ();
extern Lisp_Object Fselect_frame ();
extern Lisp_Object Ffocus_frame ();
extern Lisp_Object Funfocus_frame ();
extern Lisp_Object Fselected_frame ();
extern Lisp_Object Fwindow_frame ();
extern Lisp_Object Fframe_root_window ();
extern Lisp_Object Fframe_selected_window ();
extern Lisp_Object Fframe_list ();
extern Lisp_Object Fnext_frame ();
extern Lisp_Object Fdelete_frame ();
extern Lisp_Object Fread_mouse_position ();
extern Lisp_Object Fset_mouse_position ();
extern Lisp_Object Fmake_frame_visible ();
extern Lisp_Object Fmake_frame_invisible ();
extern Lisp_Object Ficonify_frame ();
extern Lisp_Object Fdeiconify_frame ();
extern Lisp_Object Fframe_visible_p ();
extern Lisp_Object Fvisible_frame_list ();
extern Lisp_Object Fframe_parameters ();
extern Lisp_Object Fmodify_frame_parameters ();
extern Lisp_Object Fframe_pixel_size ();
extern Lisp_Object Fframe_height ();
extern Lisp_Object Fframe_width ();
extern Lisp_Object Fset_frame_height ();
extern Lisp_Object Fset_frame_width ();
extern Lisp_Object Fset_frame_size ();
extern Lisp_Object Fset_frame_position ();
#ifndef HAVE_X11
extern Lisp_Object Frubber_band_rectangle ();
#endif	/* HAVE_X11 */

/* defined in emacs.c */
extern Lisp_Object decode_env_path ();
extern Lisp_Object Vinvocation_name, Vinvocation_directory;
extern Lisp_Object Vinstallation_directory;
void shut_down_emacs ( /* int signal, int no_x, Lisp_Object stuff */ );
/* Nonzero means don't do interactive redisplay and don't change tty modes */
extern int noninteractive;
/* Nonzero means don't do use window-system-specific display code */
extern int inhibit_window_system;
/* Nonzero means that a filter or a sentinel is running.  */
extern int running_asynch_code;

/* defined in process.c */
extern Lisp_Object Fget_process (), Fget_buffer_process (), Fprocessp ();
extern Lisp_Object Fprocess_status (), Fkill_process ();
extern Lisp_Object Fprocess_send_eof ();

/* defined in callproc.c */
extern Lisp_Object Vexec_path, Vexec_directory, Vdata_directory;
extern Lisp_Object Vdoc_directory;

/* defined in doc.c */
extern Lisp_Object Vdoc_file_name;
extern Lisp_Object Fsubstitute_command_keys ();
extern Lisp_Object Fdocumentation (), Fdocumentation_property ();

/* defined in bytecode.c */
extern Lisp_Object Qbytecode;
extern Lisp_Object Fbyte_code ();

/* defined in macros.c */
extern Lisp_Object Qexecute_kbd_macro;
extern Lisp_Object Fexecute_kbd_macro ();

/* defined in undo.c */
extern Lisp_Object Fundo_boundary ();
extern Lisp_Object truncate_undo_list ();

/* defined in textprop.c */
extern Lisp_Object Qmodification_hooks;
extern Lisp_Object Qrear_nonsticky;
extern Lisp_Object Qinsert_in_front_hooks, Qinsert_behind_hooks;
extern Lisp_Object Fnext_property_change ();
extern Lisp_Object Fnext_single_property_change ();

/* Nonzero means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern int initialized;

extern int immediate_quit;	    /* Nonzero means ^G can quit instantly */

extern void debugger ();

extern char *getenv (), *ctime (), *getwd ();
extern long *xmalloc (), *xrealloc ();
extern void xfree ();

extern char *egetenv ();
 
/* Set up the name of the machine we're running on.  */
extern void init_system_name ();

/* Some systems (e.g., NT) use a different path separator than Unix,
   in addition to a device separator.  Default the path separator
   to '/', and don't test for a device separator in IS_ANY_SEP.  */

#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif
#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif
#ifndef IS_DEVICE_SEP
#ifndef DEVICE_SEP
#define IS_DEVICE_SEP(_c_) 0
#else
#define IS_DEVICE_SEP(_c_) ((_c_) == DEVICE_SEP)
#endif
#endif
#ifndef IS_ANY_SEP
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_))
#endif

#ifdef SWITCH_ENUM_BUG
#define SWITCH_ENUM_CAST(x) ((int)(x))
#else
#define SWITCH_ENUM_CAST(x) (x)
#endif
