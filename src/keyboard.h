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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
   When Emacs goes back to the any-kboard state, it looks at all the KBOARDs
   to find those; and it tries processing their input right away.  */

typedef struct kboard KBOARD;
struct kboard
  {
    KBOARD *next_kboard;

    /* If non-nil, a keymap that overrides all others but applies only to
       this KBOARD.  Lisp code that uses this instead of calling read-char
       can effectively wait for input in the any-kboard state, and hence
       avoid blocking out the other KBOARDs.  See universal-argument in
       lisp/simple.el for an example.  */
    Lisp_Object Voverriding_terminal_local_map;

    /* Last command executed by the editor command loop, not counting
       commands that set the prefix argument.  */
    Lisp_Object Vlast_command;

    /* Normally same as last-command, but never modified by
       other commands.  */
    Lisp_Object Vreal_last_command;

    /* The prefix argument for the next command, in raw form.  */
    Lisp_Object Vprefix_arg;

    /* Saved prefix argument for the last command, in raw form.  */
    Lisp_Object Vlast_prefix_arg;

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
       macro.  This also allows us to throw away the events added to the
       macro by the last command: all the events between kbd_macro_end and
       kbd_macro_ptr belong to the last command; see
       cancel-kbd-macro-events.  */
    Lisp_Object *kbd_macro_end;

    /* Allocated size of kbd_macro_buffer.  */
    int kbd_macro_bufsize;

    /* Last anonymous kbd macro defined.  */
    Lisp_Object Vlast_kbd_macro;

    /* Alist of system-specific X windows key symbols.  */
    Lisp_Object Vsystem_key_alist;

    /* Cache for modify_event_symbol.  */
    Lisp_Object system_key_syms;

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

extern Lisp_Object Vlucid_menu_bar_dirty_flag;
extern Lisp_Object Qrecompute_lucid_menubar, Qactivate_menubar_hook;

/* Total number of times read_char has returned.  */
extern int num_input_events;

/* Total number of times read_char has returned, outside of macros.  */
extern EMACS_INT num_nonmacro_input_events;

/* Nonzero means polling for input is temporarily suppressed.  */
extern int poll_suppress_count;

/* Keymap mapping ASCII function key sequences onto their preferred forms.
   Initialized by the terminal-specific lisp files.  */
extern Lisp_Object Vfunction_key_map;

/* Vector holding the key sequence that invoked the current command.
   It is reused for each command, and it may be longer than the current
   sequence; this_command_key_count indicates how many elements
   actually mean something.  */
extern Lisp_Object this_command_keys;
extern int this_command_key_count;

/* The frame in which the last input event occurred, or Qmacro if the
   last event came from a macro.  We use this to determine when to
   generate switch-frame events.  This may be cleared by functions
   like Fselect_frame, to make sure that a switch-frame event is
   generated by the next character.  */
extern Lisp_Object internal_last_event_frame;

/* This holds a Lisp vector that holds the properties of a single
   menu item while decoding it in parse_menu_item.
   Using a Lisp vector to hold this information while we decode it
   takes care of protecting all the data from GC.  */
extern Lisp_Object item_properties;
 
/* This describes the elements of item_properties.
   The first element is not a property, it is a pointer to the item properties
   that is saved for GC protection. */
#define ITEM_PROPERTY_ITEM 0
/* The item string.  */
#define ITEM_PROPERTY_NAME 1
/* Start of initialize to nil */
/* The binding: nil, a command or a keymap.  */
#define ITEM_PROPERTY_DEF 2
/* The keymap if the binding is a keymap, otherwise nil.  */
#define ITEM_PROPERTY_MAP 3
/* Nil, :radio or :toggle.  */
#define ITEM_PROPERTY_TYPE 4
/* Nil or a string describing an equivalent key binding.  */
#define ITEM_PROPERTY_KEYEQ 5
/* Not nil if a selected toggle box or radio button, otherwise nil.  */
#define ITEM_PROPERTY_SELECTED 6
/* Place for a help string. Not yet used.  */
#define ITEM_PROPERTY_HELP 7
/* Start of initialize to t */
/* Last property. */
/* Not nil if item is enabled.  */
#define ITEM_PROPERTY_ENABLE 8

/* Macros for dealing with lispy events.  */

/* True iff EVENT has data fields describing it (i.e. a mouse click).  */
#define EVENT_HAS_PARAMETERS(event) (CONSP (event))

/* Extract the head from an event.
   This works on composite and simple events.  */
#define EVENT_HEAD(event) \
  (EVENT_HAS_PARAMETERS (event) ? XCAR (event) : (event))

/* Extract the starting and ending positions from a composite event.  */
#define EVENT_START(event) (XCAR (XCDR (event)))
#define EVENT_END(event) (XCAR (XCDR (XCDR (event))))

/* Extract the click count from a multi-click event.  */
#define EVENT_CLICK_COUNT(event) (Fnth ((event), make_number (2)))

/* Extract the fields of a position.  */
#define POSN_WINDOW(posn) (XCAR (posn))
#define POSN_BUFFER_POSN(posn) (XCAR (XCDR (posn)))
#define POSN_BUFFER_SET_POSN(posn,x) (XSETCAR (XCDR (posn), (x)))
#define POSN_WINDOW_POSN(posn) (XCAR (XCDR (XCDR (posn))))
#define POSN_TIMESTAMP(posn) \
  (XCAR (XCDR (XCDR (XCDR (posn)))))
#define POSN_SCROLLBAR_PART(posn)	(Fnth ((posn), make_number (4)))

/* A cons (STRING . STRING-CHARPOS), or nil in mouse-click events.
   It's a cons if the click is over a string in the mode line.  */

#define POSN_STRING(POSN) Fnth (make_number (4), (POSN))

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
extern Lisp_Object Qmode_line, Qvertical_line, Qheader_line;

/* Forward declaration for prototypes.  */
struct input_event;

extern Lisp_Object parse_modifiers P_ ((Lisp_Object));
extern Lisp_Object reorder_modifiers P_ ((Lisp_Object));
extern Lisp_Object read_char P_ ((int, int, Lisp_Object *, Lisp_Object, int *));
/* User-supplied string to translate input characters through.  */
extern Lisp_Object Vkeyboard_translate_table;


extern int parse_menu_item P_ ((Lisp_Object, int, int));

extern void echo_now P_ ((void));
extern void init_kboard P_ ((KBOARD *));
extern void delete_kboard P_ ((KBOARD *));
extern void single_kboard_state P_ ((void));
extern void push_frame_kboard P_ ((struct frame *));
extern void pop_frame_kboard P_ ((void));
extern void record_asynch_buffer_change P_ ((void));
extern SIGTYPE input_poll_signal P_ ((int));
extern void start_polling P_ ((void));
extern void stop_polling P_ ((void));
extern void set_poll_suppress_count P_ ((int));
extern void gobble_input P_ ((int));
extern int input_polling_used P_ ((void));
extern void clear_input_pending P_ ((void));
extern int requeued_events_pending_p P_ ((void));
extern void bind_polling_period P_ ((int));
extern void stuff_buffered_input P_ ((Lisp_Object));
extern void clear_waiting_for_input P_ ((void));
extern void swallow_events P_ ((int));
extern int help_char_p P_ ((Lisp_Object));
extern void quit_throw_to_read_char P_ ((void)) NO_RETURN;
extern void cmd_error_internal P_ ((Lisp_Object, char *));
extern void timer_start_idle P_ ((void));
extern void timer_stop_idle P_ ((void));
extern int lucid_event_type_list_p P_ ((Lisp_Object));
extern void kbd_buffer_store_event P_ ((struct input_event *));
#ifdef POLL_FOR_INPUT
extern void poll_for_input_1 P_ ((void));
#endif
extern void show_help_echo P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object, int));
extern int gen_help_event P_ ((struct input_event *, int, Lisp_Object,
			       Lisp_Object, Lisp_Object, Lisp_Object, int));
extern void kbd_buffer_store_help_event P_ ((Lisp_Object, Lisp_Object));
extern Lisp_Object menu_item_eval_property P_ ((Lisp_Object));
extern int  kbd_buffer_events_waiting P_ ((int));
