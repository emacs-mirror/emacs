/* Declarations useful when processing input.
   Copyright (C) 1985, 1986, 1987, 1993 Free Software Foundation, Inc.

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

/* Length of echobuf field in each KBOARD.  */

#define ECHOBUFSIZE 300

/* Each KBOARD represents one logical input stream from which Emacs gets input.
   If we are using an ordinary terminal, it has one KBOARD object.
   Usually each X display screen has its own KBOARD,
   but when two of them are on the same X server,
   we assume they share a keyboard and give them one KBOARD in common.

   Some Lisp variables are per-kboard; they are stored in the KBOARD structure
   and accessed indirectly via a Lisp_Misc_Kboard_Objfwd object.

   So that definition of keyboard macros, and reading of prefix arguments,
   can happen in parallel on various KBOARDs at once,
   the state information for those activities is stored in the KBOARD.

   Emacs has two states for reading input:

   ** Any kboard.  Emacs can accept input from any KBOARD,
   and as soon as any of them provides a complete command, Emacs can run it.

   ** Single kboard.  Then Emacs is running a command for one KBOARD
   and can only read input from that KBOARD.

   All input, from all KBOARDs, goes together in a single event queue
   at interrupt level.  read_char sees the events sequentially,
   but deals with them in accord with the current input state.

   In the any-kboard state, read_key_sequence processes input from any KBOARD
   immediately.  When a new event comes in from a particular KBOARD,
   read_key_sequence switches to that KBOARD.  As a result,
   as soon as a complete key arrives from some KBOARD or other,
   Emacs starts executing that key's binding.  It switches to the
   single-kboard state for the execution of that command,
   so that that command can get input only from its own KBOARD.

   While in the single-kboard state, read_char can consider input only
   from the current KBOARD.  If events come from other KBOARDs, they
   are put aside for later in the KBOARDs' kbd_queue lists.
   The flag kbd_queue_has_data in a KBOARD is 1 if this has happened.
   When Emacs goes back to the any-kboard state, it looks at all the KBOARDS
   to find those; and it tries processing their input right away.  */

typedef struct kboard KBOARD;
struct kboard
  {
    KBOARD *next_kboard;

    /* The state of a prefix arg.
       After pressing C-u COUNT times, prefix_factor is 4^COUNT
       and prefix_value is nil.
       After C-u NUM, prefix_factor is nil and prefix_value is abs(NUM).
       (prefix_factor and prefix_value are never both non-nil.)
       prefix_sign is always either +1 or -1; a value of -1 means that
       the actual numeric argument is the negative of what's in prefix_value,
       or just `-' if prefix_value is nil.
       The boolean prefix_partial means that the user is in the process
       of building a prefix argument, so that a minus or digit key at
       this point is handled specially.  */
    Lisp_Object prefix_factor, prefix_value;
    int prefix_sign, prefix_partial;

    /* Unread events specific to this kboard.  */
    Lisp_Object kbd_queue;

    /* Non-nil while a kbd macro is being defined.  */
    Lisp_Object defining_kbd_macro;

    /* The start of storage for the current keyboard macro.  */
    Lisp_Object *kbd_macro_buffer;

    /* Where to store the next keystroke of the macro.  */
    Lisp_Object *kbd_macro_ptr;

    /* The finalized section of the macro starts at kbd_macro_buffer and
       ends before this.  This is not the same as kbd_macro_ptr, because
       we advance this to kbd_macro_ptr when a key's command is complete.
       This way, the keystrokes for "end-kbd-macro" are not included in the
       macro.  */
    Lisp_Object *kbd_macro_end;

    /* Allocated size of kbd_macro_buffer.  */
    int kbd_macro_bufsize;

    /* Last anonymous kbd macro defined.  */
    Lisp_Object Vlast_kbd_macro;

    /* Alist of system-specific X windows key symbols.  */
    Lisp_Object Vsystem_key_alist;

    /* Minibufferless frames on this display use this frame's minibuffer.  */
    Lisp_Object Vdefault_minibuffer_frame;

    /* Number of displays using this KBOARD.  Normally 1, but can be
       larger when you have multiple screens on a single X display.  */
    int reference_count;

    /* Where to append more text to echobuf if we want to.  */
    char *echoptr;

    /* The text we're echoing in the modeline - partial key sequences,
       usually.  '\0'-terminated.  This really shouldn't have a fixed size.  */
    char echobuf[ECHOBUFSIZE];

    /* This flag indicates that events were put into kbd_queue
       while Emacs was running for some other KBOARD.
       The flag means that, when Emacs goes into the any-kboard state again,
       it should check this KBOARD to see if there is a complete command
       waiting.

       Note that the kbd_queue field can be non-nil even when
       kbd_queue_has_data is 0.  When we push back an incomplete
       command, then this flag is 0, meaning we don't want to try
       reading from this KBOARD again until more input arrives.  */
    char kbd_queue_has_data;

    /* Nonzero means echo each character as typed.  */
    char immediate_echo;

    /* If we have echoed a prompt string specified by the user,
       this is its length.  Otherwise this is -1.  */
    char echo_after_prompt;
  };

#ifdef MULTI_KBOARD
/* Temporarily used before a frame has been opened, and for termcap frames */
extern KBOARD *initial_kboard;

/* In the single-kboard state, this is the kboard
   from which input is accepted.

   In the any-kboard state, this is the kboard from which we are
   right now considering input.  We can consider input from another
   kboard, but doing so requires throwing to wrong_kboard_jmpbuf.  */
extern KBOARD *current_kboard;

/* A list of all kboard objects, linked through next_kboard.  */
extern KBOARD *all_kboards;

/* Nonzero in the single-kboard state, 0 in the any-kboard state.  */
extern int single_kboard;
#else
extern KBOARD the_only_kboard;
#define current_kboard (&the_only_kboard)
#define all_kboards (&the_only_kboard)
#define single_kboard 1
#endif

/* Total number of times read_char has returned.  */
extern int num_input_chars;

/* Total number of times read_char has returned, outside of macros.  */
extern int num_nonmacro_input_chars;

/* Nonzero means polling for input is temporarily suppressed.  */
extern int poll_suppress_count;

/* Nonzero if polling_for_input is actually being used.  */
extern int polling_for_input;

/* Keymap mapping ASCII function key sequences onto their preferred forms.
   Initialized by the terminal-specific lisp files.  */
extern Lisp_Object Vfunction_key_map;

/* Vector holding the key sequence that invoked the current command.
   It is reused for each command, and it may be longer than the current
   sequence; this_command_key_count indicates how many elements
   actually mean something.  */
extern Lisp_Object this_command_keys;
extern int this_command_key_count;

#ifdef MULTI_FRAME
/* The frame in which the last input event occurred, or Qmacro if the
   last event came from a macro.  We use this to determine when to
   generate switch-frame events.  This may be cleared by functions
   like Fselect_frame, to make sure that a switch-frame event is
   generated by the next character.  */
extern Lisp_Object internal_last_event_frame;
#endif


/* Macros for dealing with lispy events.  */

/* True iff EVENT has data fields describing it (i.e. a mouse click).  */
#define EVENT_HAS_PARAMETERS(event) (CONSP (event))

/* Extract the head from an event.
   This works on composite and simple events.  */
#define EVENT_HEAD(event) \
  (EVENT_HAS_PARAMETERS (event) ? XCONS (event)->car : (event))

/* Extract the starting and ending positions from a composite event.  */
#define EVENT_START(event) (XCONS (XCONS (event)->cdr)->car)
#define EVENT_END(event) (XCONS (XCONS (XCONS (event)->cdr)->cdr)->car)

/* Extract the click count from a multi-click event.  */
#define EVENT_CLICK_COUNT(event) (Fnth ((event), make_number (2)))

/* Extract the fields of a position.  */
#define POSN_WINDOW(posn) (XCONS (posn)->car)
#define POSN_BUFFER_POSN(posn) (XCONS (XCONS (posn)->cdr)->car)
#define POSN_WINDOW_POSN(posn) (XCONS (XCONS (XCONS (posn)->cdr)->cdr)->car)
#define POSN_TIMESTAMP(posn) \
  (XCONS (XCONS (XCONS (XCONS (posn)->cdr)->cdr)->cdr)->car)
#define POSN_SCROLLBAR_PART(posn)	(Fnth ((posn), make_number (4)))

/* Some of the event heads.  */
extern Lisp_Object Qswitch_frame;

/* Properties on event heads.  */
extern Lisp_Object Qevent_kind, Qevent_symbol_elements;

/* Getting an unmodified version of an event head.  */
#define EVENT_HEAD_UNMODIFIED(event_head) \
  (Fcar (Fget ((event_head), Qevent_symbol_elements)))

/* The values of Qevent_kind properties.  */
extern Lisp_Object Qfunction_key, Qmouse_click, Qmouse_movement;
extern Lisp_Object Qscroll_bar_movement;

/* Getting the kind of an event head.  */
#define EVENT_HEAD_KIND(event_head) \
  (Fget ((event_head), Qevent_kind))

/* Symbols to use for non-text mouse positions.  */
extern Lisp_Object Qmode_line, Qvertical_line;

extern Lisp_Object get_keymap_1 ();
extern Lisp_Object Fkeymapp ();
extern Lisp_Object reorder_modifiers ();
extern Lisp_Object read_char ();
/* User-supplied string to translate input characters through.  */
extern Lisp_Object Vkeyboard_translate_table;

extern Lisp_Object map_prompt ();
