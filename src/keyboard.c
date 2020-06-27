/* Keyboard and mouse input; editor command loop.

Copyright (C) 1985-1989, 1993-1997, 1999-2020 Free Software Foundation,
Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <sys/stat.h>

#include "lisp.h"
#include "coding.h"
#include "termchar.h"
#include "termopts.h"
#include "frame.h"
#include "termhooks.h"
#include "macros.h"
#include "keyboard.h"
#include "window.h"
#include "commands.h"
#include "character.h"
#include "buffer.h"
#include "dispextern.h"
#include "syntax.h"
#include "intervals.h"
#include "keymap.h"
#include "blockinput.h"
#include "sysstdio.h"
#include "systime.h"
#include "atimer.h"
#include "process.h"
#include "menu.h"
#include <errno.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif
#ifdef MSDOS
#include "msdos.h"
#include <time.h>
#else /* not MSDOS */
#include <sys/ioctl.h>
#endif /* not MSDOS */

#if defined USABLE_FIONREAD && defined USG5_4
# include <sys/filio.h>
#endif

#include "syssignal.h"

#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#include <ignore-value.h>

#include "pdumper.h"

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

/* Work around GCC bug 54561.  */
#if GNUC_PREREQ (4, 3, 0)
# pragma GCC diagnostic ignored "-Wclobbered"
#endif

#ifdef WINDOWSNT
char const DEV_TTY[] = "CONOUT$";
#else
char const DEV_TTY[] = "/dev/tty";
#endif

/* Variables for blockinput.h:  */

/* Positive if interrupt input is blocked right now.  */
volatile int interrupt_input_blocked;

/* True means an input interrupt or alarm signal has arrived.
   The maybe_quit function checks this.  */
volatile bool pending_signals;

enum { KBD_BUFFER_SIZE = 4096 };

KBOARD *initial_kboard;
KBOARD *current_kboard;
static KBOARD *all_kboards;

/* True in the single-kboard state, false in the any-kboard state.  */
static bool single_kboard;

#define NUM_RECENT_KEYS (300)

/* Index for storing next element into recent_keys.  */
static int recent_keys_index;

/* Total number of elements stored into recent_keys.  */
static int total_keys;

/* This vector holds the last NUM_RECENT_KEYS keystrokes.  */
static Lisp_Object recent_keys;

/* Vector holding the key sequence that invoked the current command.
   It is reused for each command, and it may be longer than the current
   sequence; this_command_key_count indicates how many elements
   actually mean something.
   It's easier to staticpro a single Lisp_Object than an array.  */
Lisp_Object this_command_keys;
ptrdiff_t this_command_key_count;

/* This vector is used as a buffer to record the events that were actually read
   by read_key_sequence.  */
static Lisp_Object raw_keybuf;
static int raw_keybuf_count;

#define GROW_RAW_KEYBUF							\
 if (raw_keybuf_count == ASIZE (raw_keybuf))				\
   raw_keybuf = larger_vector (raw_keybuf, 1, -1)

/* Number of elements of this_command_keys
   that precede this key sequence.  */
static ptrdiff_t this_single_command_key_start;

#ifdef HAVE_STACK_OVERFLOW_HANDLING

/* For longjmp to recover from C stack overflow.  */
sigjmp_buf return_to_command_loop;

/* Message displayed by Vtop_level when recovering from C stack overflow.  */
static Lisp_Object recover_top_level_message;

#endif /* HAVE_STACK_OVERFLOW_HANDLING */

/* Message normally displayed by Vtop_level.  */
static Lisp_Object regular_top_level_message;

/* True while displaying for echoing.   Delays C-g throwing.  */

static bool echoing;

/* Non-null means we can start echoing at the next input pause even
   though there is something in the echo area.  */

static struct kboard *ok_to_echo_at_next_pause;

/* The kboard last echoing, or null for none.  Reset to 0 in
   cancel_echoing.  If non-null, and a current echo area message
   exists, and echo_message_buffer is eq to the current message
   buffer, we know that the message comes from echo_kboard.  */

struct kboard *echo_kboard;

/* The buffer used for echoing.  Set in echo_now, reset in
   cancel_echoing.  */

Lisp_Object echo_message_buffer;

/* Character that causes a quit.  Normally C-g.

   If we are running on an ordinary terminal, this must be an ordinary
   ASCII char, since we want to make it our interrupt character.

   If we are not running on an ordinary terminal, it still needs to be
   an ordinary ASCII char.  This character needs to be recognized in
   the input interrupt handler.  At this point, the keystroke is
   represented as a struct input_event, while the desired quit
   character is specified as a lispy event.  The mapping from struct
   input_events to lispy events cannot run in an interrupt handler,
   and the reverse mapping is difficult for anything but ASCII
   keystrokes.

   FOR THESE ELABORATE AND UNSATISFYING REASONS, quit_char must be an
   ASCII character.  */
int quit_char;

/* Current depth in recursive edits.  */
EMACS_INT command_loop_level;

/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any unread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
Lisp_Object unread_switch_frame;

/* Last size recorded for a current buffer which is not a minibuffer.  */
static ptrdiff_t last_non_minibuf_size;

uintmax_t num_input_events;
ptrdiff_t point_before_last_command_or_undo;
struct buffer *buffer_before_last_command_or_undo;

/* Value of num_nonmacro_input_events as of last auto save.  */

static intmax_t last_auto_save;

/* The value of point when the last command was started. */
static ptrdiff_t last_point_position;

/* The frame in which the last input event occurred, or Qmacro if the
   last event came from a macro.  We use this to determine when to
   generate switch-frame events.  This may be cleared by functions
   like Fselect_frame, to make sure that a switch-frame event is
   generated by the next character.

   FIXME: This is modified by a signal handler so it should be volatile.
   It's exported to Lisp, though, so it can't simply be marked
   'volatile' here.  */
Lisp_Object internal_last_event_frame;

/* `read_key_sequence' stores here the command definition of the
   key sequence that it reads.  */
static Lisp_Object read_key_sequence_cmd;
static Lisp_Object read_key_sequence_remapped;

/* File in which we write all commands we read.  */
static FILE *dribble;

/* True if input is available.  */
bool input_pending;

/* True if more input was available last time we read an event.

   Since redisplay can take a significant amount of time and is not
   indispensable to perform the user's commands, when input arrives
   "too fast", Emacs skips redisplay.  More specifically, if the next
   command has already been input when we finish the previous command,
   we skip the intermediate redisplay.

   This is useful to try and make sure Emacs keeps up with fast input
   rates, such as auto-repeating keys.  But in some cases, this proves
   too conservative: we may end up disabling redisplay for the whole
   duration of a key repetition, even though we could afford to
   redisplay every once in a while.

   So we "sample" the input_pending flag before running a command and
   use *that* value after running the command to decide whether to
   skip redisplay or not.  This way, we only skip redisplay if we
   really can't keep up with the repeat rate.

   This only makes a difference if the next input arrives while running the
   command, which is very unlikely if the command is executed quickly.
   IOW this tends to avoid skipping redisplay after a long running command
   (which is a case where skipping redisplay is not very useful since the
   redisplay time is small compared to the time it took to run the command).

   A typical use case is when scrolling.  Scrolling time can be split into:
   - Time to do jit-lock on the newly displayed portion of buffer.
   - Time to run the actual scroll command.
   - Time to perform the redisplay.
   Jit-lock can happen either during the command or during the redisplay.
   In the most painful cases, the jit-lock time is the one that dominates.
   Also jit-lock can be tweaked (via jit-lock-defer) to delay its job, at the
   cost of temporary inaccuracy in display and scrolling.
   So without input_was_pending, what typically happens is the following:
   - when the command starts, there's no pending input (yet).
   - the scroll command triggers jit-lock.
   - during the long jit-lock time the next input arrives.
   - at the end of the command, we check input_pending and hence decide to
     skip redisplay.
   - we read the next input and start over.
   End result: all the hard work of jit-locking is "wasted" since redisplay
   doesn't actually happens (at least not before the input rate slows down).
   With input_was_pending redisplay is still skipped if Emacs can't keep up
   with the input rate, but if it can keep up just enough that there's no
   input_pending when we begin the command, then redisplay is not skipped
   which results in better feedback to the user.  */
static bool input_was_pending;

/* Circular buffer for pre-read keyboard input.  */

static union buffered_input_event kbd_buffer[KBD_BUFFER_SIZE];

/* Pointer to next available character in kbd_buffer.
   If kbd_fetch_ptr == kbd_store_ptr, the buffer is empty.  */
static union buffered_input_event *kbd_fetch_ptr;

/* Pointer to next place to store character in kbd_buffer.  */
static union buffered_input_event *kbd_store_ptr;

/* The above pair of variables forms a "queue empty" flag.  When we
   enqueue a non-hook event, we increment kbd_store_ptr.  When we
   dequeue a non-hook event, we increment kbd_fetch_ptr.  We say that
   there is input available if the two pointers are not equal.

   Why not just have a flag set and cleared by the enqueuing and
   dequeuing functions?  The code is a bit simpler this way.  */

static void recursive_edit_unwind (Lisp_Object buffer);
static Lisp_Object command_loop (void);

static void echo_now (void);
static ptrdiff_t echo_length (void);

/* Incremented whenever a timer is run.  */
unsigned timers_run;

/* Address (if not 0) of struct timespec to zero out if a SIGIO interrupt
   happens.  */
struct timespec *input_available_clear_time;

/* True means use SIGIO interrupts; false means use CBREAK mode.
   Default is true if INTERRUPT_INPUT is defined.  */
bool interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
bool interrupts_deferred;

/* The time when Emacs started being idle.  */

static struct timespec timer_idleness_start_time;

/* After Emacs stops being idle, this saves the last value
   of timer_idleness_start_time from when it was idle.  */

static struct timespec timer_last_idleness_start_time;


/* Global variable declarations.  */

/* Flags for readable_events.  */
#define READABLE_EVENTS_DO_TIMERS_NOW		(1 << 0)
#define READABLE_EVENTS_FILTER_EVENTS		(1 << 1)
#define READABLE_EVENTS_IGNORE_SQUEEZABLES	(1 << 2)

/* Function for init_keyboard to call with no args (if nonzero).  */
static void (*keyboard_init_hook) (void);

static bool get_input_pending (int);
static bool readable_events (int);
static Lisp_Object read_char_x_menu_prompt (Lisp_Object,
                                            Lisp_Object, bool *);
static Lisp_Object read_char_minibuf_menu_prompt (int, Lisp_Object);
static Lisp_Object make_lispy_event (struct input_event *);
static Lisp_Object make_lispy_movement (struct frame *, Lisp_Object,
                                        enum scroll_bar_part,
                                        Lisp_Object, Lisp_Object,
					Time);
static Lisp_Object modify_event_symbol (ptrdiff_t, int, Lisp_Object,
                                        Lisp_Object, const char *const *,
                                        Lisp_Object *, ptrdiff_t);
static Lisp_Object make_lispy_switch_frame (Lisp_Object);
static Lisp_Object make_lispy_focus_in (Lisp_Object);
static Lisp_Object make_lispy_focus_out (Lisp_Object);
static bool help_char_p (Lisp_Object);
static void save_getcjmp (sys_jmp_buf);
static void restore_getcjmp (void *);
static Lisp_Object apply_modifiers (int, Lisp_Object);
static void restore_kboard_configuration (int);
static void handle_interrupt (bool);
static AVOID quit_throw_to_read_char (bool);
static void timer_start_idle (void);
static void timer_stop_idle (void);
static void timer_resume_idle (void);
static void deliver_user_signal (int);
static char *find_user_signal_name (int);
static void store_user_signal_events (void);

/* Advance or retreat a buffered input event pointer.  */

static union buffered_input_event *
next_kbd_event (union buffered_input_event *ptr)
{
  return ptr == kbd_buffer + KBD_BUFFER_SIZE - 1 ? kbd_buffer : ptr + 1;
}

static union buffered_input_event *
prev_kbd_event (union buffered_input_event *ptr)
{
  return ptr == kbd_buffer ? kbd_buffer + KBD_BUFFER_SIZE - 1 : ptr - 1;
}

/* Like EVENT_START, but assume EVENT is an event.
   This pacifies gcc -Wnull-dereference, which might otherwise
   complain about earlier checks that EVENT is indeed an event.  */
static Lisp_Object
xevent_start (Lisp_Object event)
{
  return XCAR (XCDR (event));
}

/* These setters are used only in this file, so they can be private.  */
static void
kset_echo_string (struct kboard *kb, Lisp_Object val)
{
  kb->echo_string_ = val;
}
static void
kset_echo_prompt (struct kboard *kb, Lisp_Object val)
{
  kb->echo_prompt_ = val;
}
static void
kset_kbd_queue (struct kboard *kb, Lisp_Object val)
{
  kb->kbd_queue_ = val;
}
static void
kset_keyboard_translate_table (struct kboard *kb, Lisp_Object val)
{
  kb->Vkeyboard_translate_table_ = val;
}
static void
kset_last_prefix_arg (struct kboard *kb, Lisp_Object val)
{
  kb->Vlast_prefix_arg_ = val;
}
static void
kset_last_repeatable_command (struct kboard *kb, Lisp_Object val)
{
  kb->Vlast_repeatable_command_ = val;
}
static void
kset_local_function_key_map (struct kboard *kb, Lisp_Object val)
{
  kb->Vlocal_function_key_map_ = val;
}
static void
kset_overriding_terminal_local_map (struct kboard *kb, Lisp_Object val)
{
  kb->Voverriding_terminal_local_map_ = val;
}
static void
kset_real_last_command (struct kboard *kb, Lisp_Object val)
{
  kb->Vreal_last_command_ = val;
}
static void
kset_system_key_syms (struct kboard *kb, Lisp_Object val)
{
  kb->system_key_syms_ = val;
}


static bool
echo_keystrokes_p (void)
{
  return (FLOATP (Vecho_keystrokes) ? XFLOAT_DATA (Vecho_keystrokes) > 0.0
	  : FIXNUMP (Vecho_keystrokes) ? XFIXNUM (Vecho_keystrokes) > 0
          : false);
}

/* Add C to the echo string, without echoing it immediately.  C can be
   a character, which is pretty-printed, or a symbol, whose name is
   printed.  */

static void
echo_add_key (Lisp_Object c)
{
  char initbuf[KEY_DESCRIPTION_SIZE + 100];
  ptrdiff_t size = sizeof initbuf;
  char *buffer = initbuf;
  char *ptr = buffer;
  Lisp_Object echo_string = KVAR (current_kboard, echo_string);
  USE_SAFE_ALLOCA;

  if (STRINGP (echo_string) && SCHARS (echo_string) > 0)
    /* Add a space at the end as a separator between keys.  */
    ptr++[0] = ' ';

  /* If someone has passed us a composite event, use its head symbol.  */
  c = EVENT_HEAD (c);

  if (FIXNUMP (c))
    ptr = push_key_description (XFIXNUM (c), ptr);
  else if (SYMBOLP (c))
    {
      Lisp_Object name = SYMBOL_NAME (c);
      ptrdiff_t nbytes = SBYTES (name);

      if (size - (ptr - buffer) < nbytes)
	{
	  ptrdiff_t offset = ptr - buffer;
	  size = max (2 * size, size + nbytes);
	  buffer = SAFE_ALLOCA (size);
	  ptr = buffer + offset;
	}

      ptr += copy_text (SDATA (name), (unsigned char *) ptr, nbytes,
			STRING_MULTIBYTE (name), 1);
    }

  if ((NILP (echo_string) || SCHARS (echo_string) == 0)
      && help_char_p (c))
    {
      static const char text[] = " (Type ? for further options)";
      int len = sizeof text - 1;

      if (size - (ptr - buffer) < len)
	{
	  ptrdiff_t offset = ptr - buffer;
	  size += len;
	  buffer = SAFE_ALLOCA (size);
	  ptr = buffer + offset;
	}

      memcpy (ptr, text, len);
      ptr += len;
    }

  kset_echo_string
    (current_kboard,
     concat2 (echo_string, make_string (buffer, ptr - buffer)));
  SAFE_FREE ();
}

/* Temporarily add a dash to the end of the echo string if it's not
   empty, so that it serves as a mini-prompt for the very next
   character.  */

static void
echo_dash (void)
{
  /* Do nothing if not echoing at all.  */
  if (NILP (KVAR (current_kboard, echo_string)))
    return;

  if (!current_kboard->immediate_echo
      && SCHARS (KVAR (current_kboard, echo_string)) == 0)
    return;

  /* Do nothing if we just printed a prompt.  */
  if (STRINGP (KVAR (current_kboard, echo_prompt))
      && (SCHARS (KVAR (current_kboard, echo_prompt))
	  == SCHARS (KVAR (current_kboard, echo_string))))
    return;

  /* Do nothing if we have already put a dash at the end.  */
  if (SCHARS (KVAR (current_kboard, echo_string)) > 1)
    {
      Lisp_Object last_char, prev_char, idx;

      idx = make_fixnum (SCHARS (KVAR (current_kboard, echo_string)) - 2);
      prev_char = Faref (KVAR (current_kboard, echo_string), idx);

      idx = make_fixnum (SCHARS (KVAR (current_kboard, echo_string)) - 1);
      last_char = Faref (KVAR (current_kboard, echo_string), idx);

      if (XFIXNUM (last_char) == '-' && XFIXNUM (prev_char) != ' ')
	return;
    }

  /* Put a dash at the end of the buffer temporarily,
     but make it go away when the next character is added.  */
  AUTO_STRING (dash, "-");
  kset_echo_string (current_kboard,
		    concat2 (KVAR (current_kboard, echo_string), dash));
  echo_now ();
}

static void
echo_update (void)
{
  if (current_kboard->immediate_echo)
    {
      ptrdiff_t i;
      Lisp_Object prompt = KVAR (current_kboard, echo_prompt);
      Lisp_Object prefix = call0 (Qinternal_echo_keystrokes_prefix);
      kset_echo_string (current_kboard,
			NILP (prompt) ? prefix
			: NILP (prefix) ? prompt
			: concat2 (prompt, prefix));

      for (i = 0; i < this_command_key_count; i++)
	{
	  Lisp_Object c;

	  c = AREF (this_command_keys, i);
	  if (! (EVENT_HAS_PARAMETERS (c)
		 && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_movement)))
	    echo_add_key (c);
	}

      echo_now ();
    }
}

/* Display the current echo string, and begin echoing if not already
   doing so.  */

static void
echo_now (void)
{
  if (!current_kboard->immediate_echo
      /* This test breaks calls that use `echo_now' to display the echo_prompt.
         && echo_keystrokes_p () */)
    {
      current_kboard->immediate_echo = true;
      echo_update ();
      /* Put a dash at the end to invite the user to type more.  */
      echo_dash ();
    }

  echoing = true;
  /* FIXME: Use call (Qmessage) so it can be advised (e.g. emacspeak).  */
  message3_nolog (KVAR (current_kboard, echo_string));
  echoing = false;

  /* Record in what buffer we echoed, and from which kboard.  */
  echo_message_buffer = echo_area_buffer[0];
  echo_kboard = current_kboard;

  if (waiting_for_input && !NILP (Vquit_flag))
    quit_throw_to_read_char (0);
}

/* Turn off echoing, for the start of a new command.  */

void
cancel_echoing (void)
{
  current_kboard->immediate_echo = false;
  kset_echo_prompt (current_kboard, Qnil);
  kset_echo_string (current_kboard, Qnil);
  ok_to_echo_at_next_pause = NULL;
  echo_kboard = NULL;
  echo_message_buffer = Qnil;
}

/* Return the length of the current echo string.  */

static ptrdiff_t
echo_length (void)
{
  return (STRINGP (KVAR (current_kboard, echo_string))
	  ? SCHARS (KVAR (current_kboard, echo_string))
	  : 0);
}

/* Truncate the current echo message to its first LEN chars.
   This and echo_char get used by read_key_sequence when the user
   switches frames while entering a key sequence.  */

static void
echo_truncate (ptrdiff_t nchars)
{
  Lisp_Object es = KVAR (current_kboard, echo_string);
  if (STRINGP (es) && SCHARS (es) > nchars)
    kset_echo_string (current_kboard,
		      Fsubstring (KVAR (current_kboard, echo_string),
				  make_fixnum (0), make_fixnum (nchars)));
  truncate_echo_area (nchars);
}


/* Functions for manipulating this_command_keys.  */
static void
add_command_key (Lisp_Object key)
{
  if (this_command_key_count >= ASIZE (this_command_keys))
    this_command_keys = larger_vector (this_command_keys, 1, -1);

  ASET (this_command_keys, this_command_key_count, key);
  ++this_command_key_count;
}


Lisp_Object
recursive_edit_1 (void)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object val;

  if (command_loop_level > 0)
    {
      specbind (Qstandard_output, Qt);
      specbind (Qstandard_input, Qt);
    }

#ifdef HAVE_WINDOW_SYSTEM
  /* The command loop has started an hourglass timer, so we have to
     cancel it here, otherwise it will fire because the recursive edit
     can take some time.  Do not check for display_hourglass_p here,
     because it could already be nil.  */
    cancel_hourglass ();
#endif

  /* This function may have been called from a debugger called from
     within redisplay, for instance by Edebugging a function called
     from fontification-functions.  We want to allow redisplay in
     the debugging session.

     The recursive edit is left with a `(throw exit ...)'.  The `exit'
     tag is not caught anywhere in redisplay, i.e. when we leave the
     recursive edit, the original redisplay leading to the recursive
     edit will be unwound.  The outcome should therefore be safe.  */
  specbind (Qinhibit_redisplay, Qnil);
  redisplaying_p = 0;

  /* This variable stores buffers that have changed so that an undo
     boundary can be added. specbind this so that changes in the
     recursive edit will not result in undo boundaries in buffers
     changed before we entered there recursive edit.
     See Bug #23632.
  */
  specbind (Qundo_auto__undoably_changed_buffers, Qnil);

  val = command_loop ();
  if (EQ (val, Qt))
    quit ();
  /* Handle throw from read_minibuf when using minibuffer
     while it's active but we're in another window.  */
  if (STRINGP (val))
    xsignal1 (Qerror, val);

  return unbind_to (count, Qnil);
}

/* When an auto-save happens, record the "time", and don't do again soon.  */

void
record_auto_save (void)
{
  last_auto_save = num_nonmacro_input_events;
}

/* Make an auto save happen as soon as possible at command level.  */

#ifdef SIGDANGER
void
force_auto_save_soon (void)
{
  last_auto_save = - auto_save_interval - 1;
  /* FIXME: What's the relationship between forcing auto-save and adding
     a buffer-switch event?  */
  record_asynch_buffer_change ();
}
#endif

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
       doc: /* Invoke the editor command loop recursively.
To get out of the recursive edit, a command can throw to `exit' -- for
instance (throw \\='exit nil).
If you throw a value other than t, `recursive-edit' returns normally
to the function that called it.  Throwing a t value causes
`recursive-edit' to quit, so that control returns to the command loop
one level up.

This function is called by the editor initialization to begin editing.  */)
  (void)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object buffer;

  /* If we enter while input is blocked, don't lock up here.
     This may happen through the debugger during redisplay.  */
  if (input_blocked_p ())
    return Qnil;

  if (command_loop_level >= 0
      && current_buffer != XBUFFER (XWINDOW (selected_window)->contents))
    buffer = Fcurrent_buffer ();
  else
    buffer = Qnil;

  /* Don't do anything interesting between the increment and the
     record_unwind_protect!  Otherwise, we could get distracted and
     never decrement the counter again.  */
  command_loop_level++;
  update_mode_lines = 17;
  record_unwind_protect (recursive_edit_unwind, buffer);

  /* If we leave recursive_edit_1 below with a `throw' for instance,
     like it is done in the splash screen display, we have to
     make sure that we restore single_kboard as command_loop_1
     would have done if it were left normally.  */
  if (command_loop_level > 0)
    temporarily_switch_to_single_kboard (SELECTED_FRAME ());

  recursive_edit_1 ();
  return unbind_to (count, Qnil);
}

void
recursive_edit_unwind (Lisp_Object buffer)
{
  if (BUFFERP (buffer))
    Fset_buffer (buffer);

  command_loop_level--;
  update_mode_lines = 18;
}



/* If we're in single_kboard state for kboard KBOARD,
   get out of it.  */

void
not_single_kboard_state (KBOARD *kboard)
{
  if (kboard == current_kboard)
    single_kboard = false;
}

/* Maintain a stack of kboards, so other parts of Emacs
   can switch temporarily to the kboard of a given frame
   and then revert to the previous status.  */

struct kboard_stack
{
  KBOARD *kboard;
  struct kboard_stack *next;
};

static struct kboard_stack *kboard_stack;

void
push_kboard (struct kboard *k)
{
  struct kboard_stack *p = xmalloc (sizeof *p);

  p->next = kboard_stack;
  p->kboard = current_kboard;
  kboard_stack = p;

  current_kboard = k;
}

void
pop_kboard (void)
{
  struct terminal *t;
  struct kboard_stack *p = kboard_stack;
  bool found = false;
  for (t = terminal_list; t; t = t->next_terminal)
    {
      if (t->kboard == p->kboard)
        {
          current_kboard = p->kboard;
          found = true;
          break;
        }
    }
  if (!found)
    {
      /* The terminal we remembered has been deleted.  */
      current_kboard = FRAME_KBOARD (SELECTED_FRAME ());
      single_kboard = false;
    }
  kboard_stack = p->next;
  xfree (p);
}

/* Switch to single_kboard mode, making current_kboard the only KBOARD
  from which further input is accepted.  If F is non-nil, set its
  KBOARD as the current keyboard.

  This function uses record_unwind_protect_int to return to the previous
  state later.

  If Emacs is already in single_kboard mode, and F's keyboard is
  locked, then this function will throw an error.  */

void
temporarily_switch_to_single_kboard (struct frame *f)
{
  bool was_locked = single_kboard;
  if (was_locked)
    {
      if (f != NULL && FRAME_KBOARD (f) != current_kboard)
        /* We can not switch keyboards while in single_kboard mode.
           In rare cases, Lisp code may call `recursive-edit' (or
           `read-minibuffer' or `y-or-n-p') after it switched to a
           locked frame.  For example, this is likely to happen
           when server.el connects to a new terminal while Emacs is in
           single_kboard mode.  It is best to throw an error instead
           of presenting the user with a frozen screen.  */
        error ("Terminal %d is locked, cannot read from it",
               FRAME_TERMINAL (f)->id);
      else
        /* This call is unnecessary, but helps
           `restore_kboard_configuration' discover if somebody changed
           `current_kboard' behind our back.  */
        push_kboard (current_kboard);
    }
  else if (f != NULL)
    current_kboard = FRAME_KBOARD (f);
  single_kboard = true;
  record_unwind_protect_int (restore_kboard_configuration, was_locked);
}

static void
restore_kboard_configuration (int was_locked)
{
  single_kboard = was_locked;
  if (was_locked)
    {
      struct kboard *prev = current_kboard;
      pop_kboard ();
      /* The pop should not change the kboard.  */
      if (single_kboard && current_kboard != prev)
        emacs_abort ();
    }
}


/* Handle errors that are not handled at inner levels
   by printing an error message and returning to the editor command loop.  */

static Lisp_Object
cmd_error (Lisp_Object data)
{
  Lisp_Object old_level, old_length;
  char macroerror[sizeof "After..kbd macro iterations: "
		  + INT_STRLEN_BOUND (EMACS_INT)];

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  if (!NILP (executing_kbd_macro))
    {
      if (executing_kbd_macro_iterations == 1)
	sprintf (macroerror, "After 1 kbd macro iteration: ");
      else
	sprintf (macroerror, "After %"pI"d kbd macro iterations: ",
		 executing_kbd_macro_iterations);
    }
  else
    *macroerror = 0;

  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_kbd_macro = Qnil;
  executing_kbd_macro = Qnil;
  kset_prefix_arg (current_kboard, Qnil);
  kset_last_prefix_arg (current_kboard, Qnil);
  cancel_echoing ();

  /* Avoid unquittable loop if data contains a circular list.  */
  old_level = Vprint_level;
  old_length = Vprint_length;
  XSETFASTINT (Vprint_level, 10);
  XSETFASTINT (Vprint_length, 10);
  cmd_error_internal (data, macroerror);
  Vprint_level = old_level;
  Vprint_length = old_length;

  Vquit_flag = Qnil;
  Vinhibit_quit = Qnil;

  return make_fixnum (0);
}

/* Take actions on handling an error.  DATA is the data that describes
   the error.

   CONTEXT is a C-string containing ASCII characters only which
   describes the context in which the error happened.  If we need to
   generalize CONTEXT to allow multibyte characters, make it a Lisp
   string.  */

void
cmd_error_internal (Lisp_Object data, const char *context)
{
  /* The immediate context is not interesting for Quits,
     since they are asynchronous.  */
  if (EQ (XCAR (data), Qquit))
    Vsignaling_function = Qnil;

  Vquit_flag = Qnil;
  Vinhibit_quit = Qt;

  /* Use user's specified output function if any.  */
  if (!NILP (Vcommand_error_function))
    call3 (Vcommand_error_function, data,
	   context ? build_string (context) : empty_unibyte_string,
	   Vsignaling_function);

  Vsignaling_function = Qnil;
}

DEFUN ("command-error-default-function", Fcommand_error_default_function,
       Scommand_error_default_function, 3, 3, 0,
       doc: /* Produce default output for unhandled error message.
Default value of `command-error-function'.  */)
  (Lisp_Object data, Lisp_Object context, Lisp_Object signal)
{
  struct frame *sf = SELECTED_FRAME ();

  CHECK_STRING (context);

  /* If the window system or terminal frame hasn't been initialized
     yet, or we're not interactive, write the message to stderr and exit.  */
  if (!sf->glyphs_initialized_p
	   /* The initial frame is a special non-displaying frame. It
	      will be current in daemon mode when there are no frames
	      to display, and in non-daemon mode before the real frame
	      has finished initializing.  If an error is thrown in the
	      latter case while creating the frame, then the frame
	      will never be displayed, so the safest thing to do is
	      write to stderr and quit.  In daemon mode, there are
	      many other potential errors that do not prevent frames
	      from being created, so continuing as normal is better in
	      that case.  */
	   || (!IS_DAEMON && FRAME_INITIAL_P (sf))
	   || noninteractive)
    {
      print_error_message (data, Qexternal_debugging_output,
			   SSDATA (context), signal);
      Fterpri (Qexternal_debugging_output, Qnil);
      Fkill_emacs (make_fixnum (-1));
    }
  else
    {
      clear_message (1, 0);
      Fdiscard_input ();
      message_log_maybe_newline ();
      bitch_at_user ();

      print_error_message (data, Qt, SSDATA (context), signal);
    }
  return Qnil;
}

static Lisp_Object command_loop_2 (Lisp_Object);
static Lisp_Object top_level_1 (Lisp_Object);

/* Entry to editor-command-loop.
   This level has the catches for exiting/returning to editor command loop.
   It returns nil to exit recursive edit, t to abort it.  */

Lisp_Object
command_loop (void)
{
#ifdef HAVE_STACK_OVERFLOW_HANDLING
  /* At least on GNU/Linux, saving signal mask is important here.  */
  if (sigsetjmp (return_to_command_loop, 1) != 0)
    {
      /* Comes here from handle_sigsegv (see sysdep.c) and
	 stack_overflow_handler (see w32fns.c).  */
#ifdef WINDOWSNT
      w32_reset_stack_overflow_guard ();
#endif
      init_eval ();
      Vinternal__top_level_message = recover_top_level_message;
    }
  else
    Vinternal__top_level_message = regular_top_level_message;
#endif /* HAVE_STACK_OVERFLOW_HANDLING */
  if (command_loop_level > 0 || minibuf_level > 0)
    {
      Lisp_Object val;
      val = internal_catch (Qexit, command_loop_2, Qnil);
      executing_kbd_macro = Qnil;
      return val;
    }
  else
    while (1)
      {
	internal_catch (Qtop_level, top_level_1, Qnil);
	internal_catch (Qtop_level, command_loop_2, Qnil);
	executing_kbd_macro = Qnil;

	/* End of file in -batch run causes exit here.  */
	if (noninteractive)
	  Fkill_emacs (Qt);
      }
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and returns a non-nil
   value to us.  A value of nil means that command_loop_1 itself
   returned due to end of file (or end of kbd macro).  */

static Lisp_Object
command_loop_2 (Lisp_Object ignore)
{
  register Lisp_Object val;

  do
    val = internal_condition_case (command_loop_1, Qerror, cmd_error);
  while (!NILP (val));

  return Qnil;
}

static Lisp_Object
top_level_2 (void)
{
  return Feval (Vtop_level, Qnil);
}

static Lisp_Object
top_level_1 (Lisp_Object ignore)
{
  /* On entry to the outer level, run the startup file.  */
  if (!NILP (Vtop_level))
    internal_condition_case (top_level_2, Qerror, cmd_error);
  else if (!NILP (Vpurify_flag))
    message1 ("Bare impure Emacs (standard Lisp code not loaded)");
  else
    message1 ("Bare Emacs (standard Lisp code not loaded)");
  return Qnil;
}

DEFUN ("top-level", Ftop_level, Stop_level, 0, 0, "",
       doc: /* Exit all recursive editing levels.
This also exits all active minibuffers.  */
       attributes: noreturn)
  (void)
{
#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  /* Unblock input if we enter with input blocked.  This may happen if
     redisplay traps e.g. during tool-bar update with input blocked.  */
  totally_unblock_input ();

  Fthrow (Qtop_level, Qnil);
}

static AVOID
user_error (const char *msg)
{
  xsignal1 (Quser_error, build_string (msg));
}

DEFUN ("exit-recursive-edit", Fexit_recursive_edit, Sexit_recursive_edit, 0, 0, "",
       doc: /* Exit from the innermost recursive edit or minibuffer.  */
       attributes: noreturn)
  (void)
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qnil);

  user_error ("No recursive edit is in progress");
}

DEFUN ("abort-recursive-edit", Fabort_recursive_edit, Sabort_recursive_edit, 0, 0, "",
       doc: /* Abort the command that requested this recursive edit or minibuffer input.  */
       attributes: noreturn)
  (void)
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qt);

  user_error ("No recursive edit is in progress");
}

/* Restore mouse tracking enablement.  See Finternal_track_mouse for
   the only use of this function.  */

static void
tracking_off (Lisp_Object old_track_mouse)
{
  track_mouse = old_track_mouse;
  if (NILP (old_track_mouse))
    {
      /* Redisplay may have been preempted because there was input
	 available, and it assumes it will be called again after the
	 input has been processed.  If the only input available was
	 the sort that we have just disabled, then we need to call
	 redisplay.  */
      if (!readable_events (READABLE_EVENTS_DO_TIMERS_NOW))
	{
	  redisplay_preserve_echo_area (6);
	  get_input_pending (READABLE_EVENTS_DO_TIMERS_NOW);
	}
    }
}

DEFUN ("internal--track-mouse", Finternal_track_mouse, Sinternal_track_mouse,
       1, 1, 0,
       doc: /* Call BODYFUN with mouse movement events enabled.  */)
  (Lisp_Object bodyfun)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object val;

  record_unwind_protect (tracking_off, track_mouse);

  track_mouse = Qt;

  val = call0 (bodyfun);
  return unbind_to (count, val);
}

/* If mouse has moved on some frame and we are tracking the mouse,
   return one of those frames.  Return NULL otherwise.

   If ignore_mouse_drag_p is non-zero, ignore (implicit) mouse movement
   after resizing the tool-bar window.  */

bool ignore_mouse_drag_p;

static struct frame *
some_mouse_moved (void)
{
  Lisp_Object tail, frame;

  if (NILP (track_mouse) || ignore_mouse_drag_p)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      if (XFRAME (frame)->mouse_moved)
	return XFRAME (frame);
    }

  return NULL;
}


/* This is the actual command reading loop,
   sans error-handling encapsulation.  */

enum { READ_KEY_ELTS = 30 };
static int read_key_sequence (Lisp_Object *, Lisp_Object,
                              bool, bool, bool, bool);
static void adjust_point_for_property (ptrdiff_t, bool);

Lisp_Object
command_loop_1 (void)
{
  modiff_count prev_modiff = 0;
  struct buffer *prev_buffer = NULL;
  bool already_adjusted = 0;

  kset_prefix_arg (current_kboard, Qnil);
  kset_last_prefix_arg (current_kboard, Qnil);
  Vdeactivate_mark = Qnil;
  waiting_for_input = false;
  cancel_echoing ();

  this_command_key_count = 0;
  this_single_command_key_start = 0;

  if (NILP (Vmemory_full))
    {
      /* Make sure this hook runs after commands that get errors and
	 throw to top level.  */
      /* Note that the value cell will never directly contain nil
	 if the symbol is a local variable.  */
      if (!NILP (Vpost_command_hook) && !NILP (Vrun_hooks))
	safe_run_hooks (Qpost_command_hook);

      /* If displaying a message, resize the echo area window to fit
	 that message's size exactly.  */
      if (!NILP (echo_area_buffer[0]))
	resize_echo_area_exactly ();

      /* If there are warnings waiting, process them.  */
      if (!NILP (Vdelayed_warnings_list))
        safe_run_hooks (Qdelayed_warnings_hook);

      if (!NILP (Vdeferred_action_list))
	safe_run_hooks (Qdeferred_action_function);
    }

  /* Do this after running Vpost_command_hook, for consistency.  */
  kset_last_command (current_kboard, Vthis_command);
  kset_real_last_command (current_kboard, Vreal_this_command);
  if (!CONSP (last_command_event))
    kset_last_repeatable_command (current_kboard, Vreal_this_command);

  while (true)
    {
      Lisp_Object cmd;

      if (! FRAME_LIVE_P (XFRAME (selected_frame)))
	Fkill_emacs (Qnil);

      /* Make sure the current window's buffer is selected.  */
      set_buffer_internal (XBUFFER (XWINDOW (selected_window)->contents));

      /* Display any malloc warning that just came out.  Use while because
	 displaying one warning can cause another.  */

      while (pending_malloc_warning)
	display_malloc_warning ();

      Vdeactivate_mark = Qnil;

      /* Don't ignore mouse movements for more than a single command
	 loop.  (This flag is set in xdisp.c whenever the tool bar is
	 resized, because the resize moves text up or down, and would
	 generate false mouse drag events if we don't ignore them.)  */
      ignore_mouse_drag_p = false;

      /* If minibuffer on and echo area in use,
	 wait a short time and redraw minibuffer.  */

      if (minibuf_level
	  && !NILP (echo_area_buffer[0])
	  && EQ (minibuf_window, echo_area_window)
	  && NUMBERP (Vminibuffer_message_timeout))
	{
	  /* Bind inhibit-quit to t so that C-g gets read in
	     rather than quitting back to the minibuffer.  */
	  ptrdiff_t count = SPECPDL_INDEX ();
	  specbind (Qinhibit_quit, Qt);

	  sit_for (Vminibuffer_message_timeout, 0, 2);

	  /* Clear the echo area.  */
	  message1 (0);
	  safe_run_hooks (Qecho_area_clear_hook);

	  /* We cleared the echo area, and the minibuffer will now
	     show, so resize the mini-window in case the minibuffer
	     needs more or less space than the echo area.  */
	  resize_mini_window (XWINDOW (minibuf_window), false);

	  unbind_to (count, Qnil);

	  /* If a C-g came in before, treat it as input now.  */
	  if (!NILP (Vquit_flag))
	    {
	      Vquit_flag = Qnil;
	      Vunread_command_events = list1i (quit_char);
	    }
	}

      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag)
	  && !NILP (Ffboundp (Qrecompute_lucid_menubar)))
	call0 (Qrecompute_lucid_menubar);

      Vthis_command = Qnil;
      Vreal_this_command = Qnil;
      Vthis_original_command = Qnil;
      Vthis_command_keys_shift_translated = Qnil;

      /* Read next key sequence; i gets its length.  */
      raw_keybuf_count = 0;
      Lisp_Object keybuf[READ_KEY_ELTS];
      int i = read_key_sequence (keybuf, Qnil, false, true, true, false);

      /* A filter may have run while we were reading the input.  */
      if (! FRAME_LIVE_P (XFRAME (selected_frame)))
	Fkill_emacs (Qnil);
      set_buffer_internal (XBUFFER (XWINDOW (selected_window)->contents));

      ++num_input_keys;

      /* Now we have read a key sequence of length I,
	 or else I is 0 and we found end of file.  */

      if (i == 0)		/* End of file -- happens only in */
	return Qnil;		/* a kbd macro, at the end.  */
      /* -1 means read_key_sequence got a menu that was rejected.
	 Just loop around and read another command.  */
      if (i == -1)
	{
	  cancel_echoing ();
	  this_command_key_count = 0;
	  this_single_command_key_start = 0;
	  goto finalize;
	}

      last_command_event = keybuf[i - 1];

      /* If the previous command tried to force a specific window-start,
	 forget about that, in case this command moves point far away
	 from that position.  But also throw away beg_unchanged and
	 end_unchanged information in that case, so that redisplay will
	 update the whole window properly.  */
      if (XWINDOW (selected_window)->force_start)
	{
	  struct buffer *b;
	  XWINDOW (selected_window)->force_start = 0;
	  b = XBUFFER (XWINDOW (selected_window)->contents);
	  BUF_BEG_UNCHANGED (b) = BUF_END_UNCHANGED (b) = 0;
	}

      cmd = read_key_sequence_cmd;
      if (!NILP (Vexecuting_kbd_macro))
	{
	  if (!NILP (Vquit_flag))
	    {
	      Vexecuting_kbd_macro = Qt;
	      maybe_quit ();	/* Make some noise.  */
				/* Will return since macro now empty.  */
	    }
	}

      /* Do redisplay processing after this command except in special
	 cases identified below.  */
      prev_buffer = current_buffer;
      prev_modiff = MODIFF;
      last_point_position = PT;

      /* By default, we adjust point to a boundary of a region that
         has such a property that should be treated intangible
         (e.g. composition, display).  But, some commands will set
         this variable differently.  */
      Vdisable_point_adjustment = Qnil;

      /* Process filters and timers may have messed with deactivate-mark.
	 reset it before we execute the command.  */
      Vdeactivate_mark = Qnil;

      /* Remap command through active keymaps.  */
      Vthis_original_command = cmd;
      if (!NILP (read_key_sequence_remapped))
	cmd = read_key_sequence_remapped;

      /* Execute the command.  */

      {
	total_keys += total_keys < NUM_RECENT_KEYS;
	ASET (recent_keys, recent_keys_index,
	      Fcons (Qnil, cmd));
	if (++recent_keys_index >= NUM_RECENT_KEYS)
	  recent_keys_index = 0;
      }
      Vthis_command = cmd;
      Vreal_this_command = cmd;
      safe_run_hooks (Qpre_command_hook);

      already_adjusted = 0;

      if (NILP (Vthis_command))
	/* nil means key is undefined.  */
	call0 (Qundefined);
      else
	{
	  /* Here for a command that isn't executed directly.  */

#ifdef HAVE_WINDOW_SYSTEM
            ptrdiff_t scount = SPECPDL_INDEX ();

            if (display_hourglass_p
                && NILP (Vexecuting_kbd_macro))
              {
                record_unwind_protect_void (cancel_hourglass);
                start_hourglass ();
              }
#endif

            /* Ensure that we have added appropriate undo-boundaries as a
               result of changes from the last command. */
            call0 (Qundo_auto__add_boundary);

            /* Record point and buffer, so we can put point into the undo
               information if necessary. */
            point_before_last_command_or_undo = PT;
            buffer_before_last_command_or_undo = current_buffer;

            call1 (Qcommand_execute, Vthis_command);

#ifdef HAVE_WINDOW_SYSTEM
	  /* Do not check display_hourglass_p here, because
	     `command-execute' could change it, but we should cancel
	     hourglass cursor anyway.
	     But don't cancel the hourglass within a macro
	     just because a command in the macro finishes.  */
	  if (NILP (Vexecuting_kbd_macro))
            unbind_to (scount, Qnil);
#endif
          }
      kset_last_prefix_arg (current_kboard, Vcurrent_prefix_arg);

      safe_run_hooks (Qpost_command_hook);

      /* If displaying a message, resize the echo area window to fit
	 that message's size exactly.  Do this only if the echo area
	 window is the minibuffer window of the selected frame.  See
	 Bug#34317.  */
      if (!NILP (echo_area_buffer[0])
	  && (EQ (echo_area_window,
		  FRAME_MINIBUF_WINDOW (XFRAME (selected_frame)))))
	resize_echo_area_exactly ();

      /* If there are warnings waiting, process them.  */
      if (!NILP (Vdelayed_warnings_list))
        safe_run_hooks (Qdelayed_warnings_hook);

      safe_run_hooks (Qdeferred_action_function);

      kset_last_command (current_kboard, Vthis_command);
      kset_real_last_command (current_kboard, Vreal_this_command);
      if (!CONSP (last_command_event))
	kset_last_repeatable_command (current_kboard, Vreal_this_command);

      this_command_key_count = 0;
      this_single_command_key_start = 0;

      if (current_kboard->immediate_echo
	  && !NILP (call0 (Qinternal_echo_keystrokes_prefix)))
	{
	  current_kboard->immediate_echo = false;
	  /* Refresh the echo message.  */
	  echo_now ();
	}
      else
	cancel_echoing ();

      if (!NILP (BVAR (current_buffer, mark_active))
	  && !NILP (Vrun_hooks))
	{
	  /* In Emacs 22, setting transient-mark-mode to `only' was a
	     way of turning it on for just one command.  This usage is
	     obsolete, but support it anyway.  */
	  if (EQ (Vtransient_mark_mode, Qidentity))
	    Vtransient_mark_mode = Qnil;
	  else if (EQ (Vtransient_mark_mode, Qonly))
	    Vtransient_mark_mode = Qidentity;

	  if (!NILP (Vdeactivate_mark))
	    /* If `select-active-regions' is non-nil, this call to
	       `deactivate-mark' also sets the PRIMARY selection.  */
	    call0 (Qdeactivate_mark);
	  else
	    {
	      /* Even if not deactivating the mark, set PRIMARY if
		 `select-active-regions' is non-nil.  */
	      if (!NILP (Fwindow_system (Qnil))
		  /* Even if mark_active is non-nil, the actual buffer
		     marker may not have been set yet (Bug#7044).  */
		  && XMARKER (BVAR (current_buffer, mark))->buffer
		  && (EQ (Vselect_active_regions, Qonly)
		      ? EQ (CAR_SAFE (Vtransient_mark_mode), Qonly)
		      : (!NILP (Vselect_active_regions)
			 && !NILP (Vtransient_mark_mode)))
		  && NILP (Fmemq (Vthis_command,
				  Vselection_inhibit_update_commands)))
		{
		  Lisp_Object txt
		    = call1 (Vregion_extract_function, Qnil);
		  if (XFIXNUM (Flength (txt)) > 0)
		    /* Don't set empty selections.  */
		    call2 (Qgui_set_selection, QPRIMARY, txt);
		}

	      if (current_buffer != prev_buffer || MODIFF != prev_modiff)
		run_hook (intern ("activate-mark-hook"));
	    }

	  Vsaved_region_selection = Qnil;
	}

    finalize:

      if (current_buffer == prev_buffer
	  && XBUFFER (XWINDOW (selected_window)->contents) == current_buffer
	  && last_point_position != PT
	  && NILP (Vdisable_point_adjustment)
	  && NILP (Vglobal_disable_point_adjustment))
	{
	  if (last_point_position > BEGV
	      && last_point_position < ZV
	      && (composition_adjust_point (last_point_position,
					    last_point_position)
		  != last_point_position))
	    /* The last point was temporarily set within a grapheme
	       cluster to prevent automatic composition.  To recover
	       the automatic composition, we must update the
	       display.  */
	    windows_or_buffers_changed = 21;
	  if (!already_adjusted)
	    adjust_point_for_property (last_point_position,
				       MODIFF != prev_modiff);
	}

      /* Install chars successfully executed in kbd macro.  */

      if (!NILP (KVAR (current_kboard, defining_kbd_macro))
	  && NILP (KVAR (current_kboard, Vprefix_arg)))
	finalize_kbd_macro_chars ();
    }
}

Lisp_Object
read_menu_command (void)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  /* We don't want to echo the keystrokes while navigating the
     menus.  */
  specbind (Qecho_keystrokes, make_fixnum (0));

  Lisp_Object keybuf[READ_KEY_ELTS];
  int i = read_key_sequence (keybuf, Qnil, false, true, true, true);

  unbind_to (count, Qnil);

  if (! FRAME_LIVE_P (XFRAME (selected_frame)))
    Fkill_emacs (Qnil);
  if (i == 0 || i == -1)
    return Qt;

  return read_key_sequence_cmd;
}

/* Adjust point to a boundary of a region that has such a property
   that should be treated intangible.  For the moment, we check
   `composition', `display' and `invisible' properties.
   LAST_PT is the last position of point.  */

static void
adjust_point_for_property (ptrdiff_t last_pt, bool modified)
{
  ptrdiff_t beg, end;
  Lisp_Object val, overlay, tmp;
  /* When called after buffer modification, we should temporarily
     suppress the point adjustment for automatic composition so that a
     user can keep inserting another character at point or keep
     deleting characters around point.  */
  bool check_composition = ! modified;
  bool check_display = true, check_invisible = true;
  ptrdiff_t orig_pt = PT;

  eassert (XBUFFER (XWINDOW (selected_window)->contents) == current_buffer);

  /* FIXME: cycling is probably not necessary because these properties
     can't be usefully combined anyway.  */
  while (check_composition || check_display || check_invisible)
    {
      /* FIXME: check `intangible'.  */
      if (check_composition
	  && PT > BEGV && PT < ZV
	  && (beg = composition_adjust_point (last_pt, PT)) != PT)
	{
	  SET_PT (beg);
	  check_display = check_invisible = true;
	}
      check_composition = false;
      if (check_display
	  && PT > BEGV && PT < ZV
	  && !NILP (val = get_char_property_and_overlay
		              (make_fixnum (PT), Qdisplay, selected_window,
			       &overlay))
	  && display_prop_intangible_p (val, overlay, PT, PT_BYTE)
	  && (!OVERLAYP (overlay)
	      ? get_property_and_range (PT, Qdisplay, &val, &beg, &end, Qnil)
	      : (beg = OVERLAY_POSITION (OVERLAY_START (overlay)),
		 end = OVERLAY_POSITION (OVERLAY_END (overlay))))
	  && (beg < PT /* && end > PT   <- It's always the case.  */
	      || (beg <= PT && STRINGP (val) && SCHARS (val) == 0)))
	{
	  eassert (end > PT);
	  SET_PT (PT < last_pt
		  ? (STRINGP (val) && SCHARS (val) == 0
		     ? max (beg - 1, BEGV)
		     : beg)
		  : end);
	  check_composition = check_invisible = true;
	}
      check_display = false;
      if (check_invisible && PT > BEGV && PT < ZV)
	{
	  int inv;
	  bool ellipsis = false;
	  beg = end = PT;

	  /* Find boundaries `beg' and `end' of the invisible area, if any.  */
	  while (end < ZV
#if 0
		 /* FIXME: We should stop if we find a spot between
		    two runs of `invisible' where inserted text would
		    be visible.  This is important when we have two
		    invisible boundaries that enclose an area: if the
		    area is empty, we need this test in order to make
		    it possible to place point in the middle rather
		    than skip both boundaries.  However, this code
		    also stops anywhere in a non-sticky text-property,
		    which breaks (e.g.) Org mode.  */
		 && (val = Fget_pos_property (make_fixnum (end),
					      Qinvisible, Qnil),
		     TEXT_PROP_MEANS_INVISIBLE (val))
#endif
		 && !NILP (val = get_char_property_and_overlay
		           (make_fixnum (end), Qinvisible, Qnil, &overlay))
		 && (inv = TEXT_PROP_MEANS_INVISIBLE (val)))
	    {
	      ellipsis = ellipsis || inv > 1
		|| (OVERLAYP (overlay)
		    && (!NILP (Foverlay_get (overlay, Qafter_string))
			|| !NILP (Foverlay_get (overlay, Qbefore_string))));
	      tmp = Fnext_single_char_property_change
		(make_fixnum (end), Qinvisible, Qnil, Qnil);
	      end = FIXNATP (tmp) ? XFIXNAT (tmp) : ZV;
	    }
	  while (beg > BEGV
#if 0
		 && (val = Fget_pos_property (make_fixnum (beg),
					      Qinvisible, Qnil),
		     TEXT_PROP_MEANS_INVISIBLE (val))
#endif
		 && !NILP (val = get_char_property_and_overlay
		           (make_fixnum (beg - 1), Qinvisible, Qnil, &overlay))
		 && (inv = TEXT_PROP_MEANS_INVISIBLE (val)))
	    {
	      ellipsis = ellipsis || inv > 1
		|| (OVERLAYP (overlay)
		    && (!NILP (Foverlay_get (overlay, Qafter_string))
			|| !NILP (Foverlay_get (overlay, Qbefore_string))));
	      tmp = Fprevious_single_char_property_change
		(make_fixnum (beg), Qinvisible, Qnil, Qnil);
	      beg = FIXNATP (tmp) ? XFIXNAT (tmp) : BEGV;
	    }

	  /* Move away from the inside area.  */
	  if (beg < PT && end > PT)
	    {
	      SET_PT ((orig_pt == PT && (last_pt < beg || last_pt > end))
		      /* We haven't moved yet (so we don't need to fear
			 infinite-looping) and we were outside the range
			 before (so either end of the range still corresponds
			 to a move in the right direction): pretend we moved
			 less than we actually did, so that we still have
			 more freedom below in choosing which end of the range
			 to go to.  */
		      ? (orig_pt = -1, PT < last_pt ? end : beg)
		      /* We either have moved already or the last point
			 was already in the range: we don't get to choose
			 which end of the range we have to go to.  */
		      : (PT < last_pt ? beg : end));
	      check_composition = check_display = true;
	    }
#if 0 /* This assertion isn't correct, because SET_PT may end up setting
	 the point to something other than its argument, due to
	 point-motion hooks, intangibility, etc.  */
	  eassert (PT == beg || PT == end);
#endif

	  /* Pretend the area doesn't exist if the buffer is not
	     modified.  */
	  if (!modified && !ellipsis && beg < end)
	    {
	      if (last_pt == beg && PT == end && end < ZV)
		(check_composition = check_display = true, SET_PT (end + 1));
	      else if (last_pt == end && PT == beg && beg > BEGV)
		(check_composition = check_display = true, SET_PT (beg - 1));
	      else if (PT == ((PT < last_pt) ? beg : end))
		/* We've already moved as far as we can.  Trying to go
		   to the other end would mean moving backwards and thus
		   could lead to an infinite loop.  */
		;
	      else if (val = Fget_pos_property (make_fixnum (PT),
						Qinvisible, Qnil),
		       TEXT_PROP_MEANS_INVISIBLE (val)
		       && (val = (Fget_pos_property
				  (make_fixnum (PT == beg ? end : beg),
				   Qinvisible, Qnil)),
			   !TEXT_PROP_MEANS_INVISIBLE (val)))
		(check_composition = check_display = true,
		 SET_PT (PT == beg ? end : beg));
	    }
	}
      check_invisible = false;
    }
}

/* Subroutine for safe_run_hooks: run the hook, which is ARGS[1].  */

static Lisp_Object
safe_run_hooks_1 (ptrdiff_t nargs, Lisp_Object *args)
{
  eassert (nargs == 2);
  return call0 (args[1]);
}

/* Subroutine for safe_run_hooks: handle an error by clearing out the function
   from the hook.  */

static Lisp_Object
safe_run_hooks_error (Lisp_Object error, ptrdiff_t nargs, Lisp_Object *args)
{
  eassert (nargs == 2);
  AUTO_STRING (format, "Error in %s (%S): %S");
  Lisp_Object hook = args[0];
  Lisp_Object fun = args[1];
  CALLN (Fmessage, format, hook, fun, error);

  if (SYMBOLP (hook))
    {
      bool found = false;
      Lisp_Object newval = Qnil;
      Lisp_Object val = find_symbol_value (hook);
      FOR_EACH_TAIL (val)
	if (EQ (fun, XCAR (val)))
	  found = true;
	else
	  newval = Fcons (XCAR (val), newval);
      if (found)
	return Fset (hook, Fnreverse (newval));
      /* Not found in the local part of the hook.  Let's look at the global
	 part.  */
      newval = Qnil;
      val = NILP (Fdefault_boundp (hook)) ? Qnil : Fdefault_value (hook);
      FOR_EACH_TAIL (val)
	if (EQ (fun, XCAR (val)))
	  found = true;
	else
	  newval = Fcons (XCAR (val), newval);
      if (found)
	return Fset_default (hook, Fnreverse (newval));
    }
  return Qnil;
}

static Lisp_Object
safe_run_hook_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  eassert (nargs == 2);
  /* Yes, run_hook_with_args works with args in the other order.  */
  internal_condition_case_n (safe_run_hooks_1,
			     2, ((Lisp_Object []) {args[1], args[0]}),
			     Qt, safe_run_hooks_error);
  return Qnil;
}

/* If we get an error while running the hook, cause the hook variable
   to be nil.  Also inhibit quits, so that C-g won't cause the hook
   to mysteriously evaporate.  */

void
safe_run_hooks (Lisp_Object hook)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  specbind (Qinhibit_quit, Qt);
  run_hook_with_args (2, ((Lisp_Object []) {hook, hook}), safe_run_hook_funcall);
  unbind_to (count, Qnil);
}


/* Nonzero means polling for input is temporarily suppressed.  */

int poll_suppress_count;


#ifdef POLL_FOR_INPUT

/* Asynchronous timer for polling.  */

static struct atimer *poll_timer;

#if defined CYGWIN || defined DOS_NT
/* Poll for input, so that we catch a C-g if it comes in.  */
void
poll_for_input_1 (void)
{
  if (! input_blocked_p ()
      && !waiting_for_input)
    gobble_input ();
}
#endif

/* Timer callback function for poll_timer.  TIMER is equal to
   poll_timer.  */

static void
poll_for_input (struct atimer *timer)
{
  if (poll_suppress_count == 0)
    pending_signals = true;
}

#endif /* POLL_FOR_INPUT */

/* Begin signals to poll for input, if they are appropriate.
   This function is called unconditionally from various places.  */

void
start_polling (void)
{
#ifdef POLL_FOR_INPUT
  /* XXX This condition was (read_socket_hook && !interrupt_input),
     but read_socket_hook is not global anymore.  Let's pretend that
     it's always set.  */
  if (!interrupt_input)
    {
      /* Turn alarm handling on unconditionally.  It might have
	 been turned off in process.c.  */
      turn_on_atimers (1);

      /* If poll timer doesn't exist, or we need one with
	 a different interval, start a new one.  */
      if (poll_timer == NULL
	  || poll_timer->interval.tv_sec != polling_period)
	{
	  time_t period = max (1, min (polling_period, TYPE_MAXIMUM (time_t)));
	  struct timespec interval = make_timespec (period, 0);

	  if (poll_timer)
	    cancel_atimer (poll_timer);

	  poll_timer = start_atimer (ATIMER_CONTINUOUS, interval,
				     poll_for_input, NULL);
	}

      /* Let the timer's callback function poll for input
	 if this becomes zero.  */
      --poll_suppress_count;
    }
#endif
}

#if defined CYGWIN || defined DOS_NT
/* True if we are using polling to handle input asynchronously.  */

bool
input_polling_used (void)
{
# ifdef POLL_FOR_INPUT
  /* XXX This condition was (read_socket_hook && !interrupt_input),
     but read_socket_hook is not global anymore.  Let's pretend that
     it's always set.  */
  return !interrupt_input;
# else
  return false;
# endif
}
#endif

/* Turn off polling.  */

void
stop_polling (void)
{
#ifdef POLL_FOR_INPUT
  /* XXX This condition was (read_socket_hook && !interrupt_input),
     but read_socket_hook is not global anymore.  Let's pretend that
     it's always set.  */
  if (!interrupt_input)
    ++poll_suppress_count;
#endif
}

/* Set the value of poll_suppress_count to COUNT
   and start or stop polling accordingly.  */

void
set_poll_suppress_count (int count)
{
#ifdef POLL_FOR_INPUT
  if (count == 0 && poll_suppress_count != 0)
    {
      poll_suppress_count = 1;
      start_polling ();
    }
  else if (count != 0 && poll_suppress_count == 0)
    {
      stop_polling ();
    }
  poll_suppress_count = count;
#endif
}

/* Bind polling_period to a value at least N.
   But don't decrease it.  */

void
bind_polling_period (int n)
{
#ifdef POLL_FOR_INPUT
  intmax_t new = polling_period;

  if (n > new)
    new = n;

  stop_other_atimers (poll_timer);
  stop_polling ();
  specbind (Qpolling_period, make_int (new));
  /* Start a new alarm with the new period.  */
  start_polling ();
#endif
}

/* Apply the control modifier to CHARACTER.  */

int
make_ctrl_char (int c)
{
  /* Save the upper bits here.  */
  int upper = c & ~0177;

  if (! ASCII_CHAR_P (c))
    return c |= ctrl_modifier;

  c &= 0177;

  /* Everything in the columns containing the upper-case letters
     denotes a control character.  */
  if (c >= 0100 && c < 0140)
    {
      int oc = c;
      c &= ~0140;
      /* Set the shift modifier for a control char
	 made from a shifted letter.  But only for letters!  */
      if (oc >= 'A' && oc <= 'Z')
	c |= shift_modifier;
    }

  /* The lower-case letters denote control characters too.  */
  else if (c >= 'a' && c <= 'z')
    c &= ~0140;

  /* Include the bits for control and shift
     only if the basic ASCII code can't indicate them.  */
  else if (c >= ' ')
    c |= ctrl_modifier;

  /* Replace the high bits.  */
  c |= (upper & ~ctrl_modifier);

  return c;
}

/* Substitute key descriptions and quotes in HELP, unless its first
   character has a non-nil help-echo-inhibit-substitution property.  */

static Lisp_Object
help_echo_substitute_command_keys (Lisp_Object help)
{
  if (STRINGP (help)
      && SCHARS (help) > 0
      && !NILP (Fget_text_property (make_fixnum (0),
                                    Qhelp_echo_inhibit_substitution,
                                    help)))
    return help;

  return Fsubstitute_command_keys (help);
}

/* Display the help-echo property of the character after the mouse pointer.
   Either show it in the echo area, or call show-help-function to display
   it by other means (maybe in a tooltip).

   If HELP is nil, that means clear the previous help echo.

   If HELP is a string, display that string.  If HELP is a function,
   call it with OBJECT and POS as arguments; the function should
   return a help string or nil for none.  For all other types of HELP,
   evaluate it to obtain a string.

   WINDOW is the window in which the help was generated, if any.
   It is nil if not in a window.

   If OBJECT is a buffer, POS is the position in the buffer where the
   `help-echo' text property was found.

   If OBJECT is an overlay, that overlay has a `help-echo' property,
   and POS is the position in the overlay's buffer under the mouse.

   If OBJECT is a string (an overlay string or a string displayed with
   the `display' property).  POS is the position in that string under
   the mouse.

   Note: this function may only be called with HELP nil or a string
   from X code running asynchronously.  */

void
show_help_echo (Lisp_Object help, Lisp_Object window, Lisp_Object object,
		Lisp_Object pos)
{
  if (!NILP (help) && !STRINGP (help))
    {
      if (FUNCTIONP (help))
	help = safe_call (4, help, window, object, pos);
      else
	help = safe_eval (help);

      if (!STRINGP (help))
	return;
    }

  if (!noninteractive && STRINGP (help))
    {
      /* The mouse-fixup-help-message Lisp function can call
	 mouse_position_hook, which resets the mouse_moved flags.
	 This causes trouble if we are trying to read a mouse motion
	 event (i.e., if we are inside a `track-mouse' form), so we
	 restore the mouse_moved flag.  */
      struct frame *f = some_mouse_moved ();

      help = call1 (Qmouse_fixup_help_message, help);
      if (f)
	f->mouse_moved = true;
    }

  if (STRINGP (help) || NILP (help))
    {
      if (!NILP (Vshow_help_function))
	call1 (Vshow_help_function, help_echo_substitute_command_keys (help));
      help_echo_showing_p = STRINGP (help);
    }
}



/* Input of single characters from keyboard.  */

static Lisp_Object kbd_buffer_get_event (KBOARD **kbp, bool *used_mouse_menu,
					 struct timespec *end_time);
static void record_char (Lisp_Object c);

static Lisp_Object help_form_saved_window_configs;
static void
read_char_help_form_unwind (void)
{
  Lisp_Object window_config = XCAR (help_form_saved_window_configs);
  help_form_saved_window_configs = XCDR (help_form_saved_window_configs);
  if (!NILP (window_config))
    Fset_window_configuration (window_config);
}

#define STOP_POLLING					\
do { if (! polling_stopped_here) stop_polling ();	\
       polling_stopped_here = true; } while (0)

#define RESUME_POLLING					\
do { if (polling_stopped_here) start_polling ();	\
       polling_stopped_here = false; } while (0)

static Lisp_Object
read_event_from_main_queue (struct timespec *end_time,
                            sys_jmp_buf local_getcjmp,
                            bool *used_mouse_menu)
{
  Lisp_Object c = Qnil;
  sys_jmp_buf save_jump;
  KBOARD *kb;

 start:

  /* Read from the main queue, and if that gives us something we can't use yet,
     we put it on the appropriate side queue and try again.  */

  if (end_time && timespec_cmp (*end_time, current_timespec ()) <= 0)
    return c;

  /* Actually read a character, waiting if necessary.  */
  ptrdiff_t count = SPECPDL_INDEX ();
  save_getcjmp (save_jump);
  record_unwind_protect_ptr (restore_getcjmp, save_jump);
  restore_getcjmp (local_getcjmp);
  if (!end_time)
    timer_start_idle ();
  c = kbd_buffer_get_event (&kb, used_mouse_menu, end_time);
  unbind_to (count, Qnil);

  if (! NILP (c) && (kb != current_kboard))
    {
      Lisp_Object last = KVAR (kb, kbd_queue);
      if (CONSP (last))
        {
          while (CONSP (XCDR (last)))
	    last = XCDR (last);
          if (!NILP (XCDR (last)))
	    emacs_abort ();
        }
      if (!CONSP (last))
        kset_kbd_queue (kb, list1 (c));
      else
        XSETCDR (last, list1 (c));
      kb->kbd_queue_has_data = true;
      c = Qnil;
      if (single_kboard)
        goto start;
      current_kboard = kb;
      return make_fixnum (-2);
    }

  /* Terminate Emacs in batch mode if at eof.  */
  if (noninteractive && FIXNUMP (c) && XFIXNUM (c) < 0)
    Fkill_emacs (make_fixnum (1));

  if (FIXNUMP (c))
    {
      /* Add in any extra modifiers, where appropriate.  */
      if ((extra_keyboard_modifiers & CHAR_CTL)
	  || ((extra_keyboard_modifiers & 0177) < ' '
	      && (extra_keyboard_modifiers & 0177) != 0))
	XSETINT (c, make_ctrl_char (XFIXNUM (c)));

      /* Transfer any other modifier bits directly from
	 extra_keyboard_modifiers to c.  Ignore the actual character code
	 in the low 16 bits of extra_keyboard_modifiers.  */
      XSETINT (c, XFIXNUM (c) | (extra_keyboard_modifiers & ~0xff7f & ~CHAR_CTL));
    }

  return c;
}



/* Like `read_event_from_main_queue' but applies keyboard-coding-system
   to tty input.  */
static Lisp_Object
read_decoded_event_from_main_queue (struct timespec *end_time,
                                    sys_jmp_buf local_getcjmp,
                                    Lisp_Object prev_event,
                                    bool *used_mouse_menu)
{
#ifndef WINDOWSNT
#define MAX_ENCODED_BYTES 16
  Lisp_Object events[MAX_ENCODED_BYTES];
  int n = 0;
#endif
  while (true)
    {
      Lisp_Object nextevt
        = read_event_from_main_queue (end_time, local_getcjmp,
                                      used_mouse_menu);
#ifdef WINDOWSNT
      /* w32_console already returns decoded events.  It either reads
	 Unicode characters from the Windows keyboard input, or
	 converts characters encoded in the current codepage into
	 Unicode.  See w32inevt.c:key_event, near its end.  */
      return nextevt;
#else
      struct frame *frame = XFRAME (selected_frame);
      struct terminal *terminal = frame->terminal;
      if (!((FRAME_TERMCAP_P (frame) || FRAME_MSDOS_P (frame))
            /* Don't apply decoding if we're just reading a raw event
               (e.g. reading bytes sent by the xterm to specify the position
               of a mouse click).  */
            && (!EQ (prev_event, Qt))
	    && (TERMINAL_KEYBOARD_CODING (terminal)->common_flags
		& CODING_REQUIRE_DECODING_MASK)))
	return nextevt;		/* No decoding needed.  */
      else
	{
	  int meta_key = terminal->display_info.tty->meta_key;
	  eassert (n < MAX_ENCODED_BYTES);
	  events[n++] = nextevt;
	  if (FIXNATP (nextevt)
	      && XFIXNUM (nextevt) < (meta_key == 1 ? 0x80 : 0x100))
	    { /* An encoded byte sequence, let's try to decode it.  */
	      struct coding_system *coding
		= TERMINAL_KEYBOARD_CODING (terminal);

	      if (raw_text_coding_system_p (coding))
		{
		  int i;
		  if (meta_key != 2)
		    for (i = 0; i < n; i++)
		      events[i] = make_fixnum (XFIXNUM (events[i]) & ~0x80);
		}
	      else
		{
		  unsigned char src[MAX_ENCODED_BYTES];
		  unsigned char dest[MAX_ENCODED_BYTES * MAX_MULTIBYTE_LENGTH];
		  int i;
		  for (i = 0; i < n; i++)
		    src[i] = XFIXNUM (events[i]);
		  if (meta_key != 2)
		    for (i = 0; i < n; i++)
		      src[i] &= ~0x80;
		  coding->destination = dest;
		  coding->dst_bytes = sizeof dest;
		  decode_coding_c_string (coding, src, n, Qnil);
		  eassert (coding->produced_char <= n);
		  if (coding->produced_char == 0)
		    { /* The encoded sequence is incomplete.  */
		      if (n < MAX_ENCODED_BYTES) /* Avoid buffer overflow.  */
			continue;		     /* Read on!  */
		    }
		  else
		    {
		      const unsigned char *p = coding->destination;
		      eassert (coding->carryover_bytes == 0);
		      n = 0;
		      while (n < coding->produced_char)
			events[n++] = make_fixnum (STRING_CHAR_ADVANCE (p));
		    }
		}
	    }
	  /* Now `events' should hold decoded events.
	     Normally, n should be equal to 1, but better not rely on it.
	     We can only return one event here, so return the first we
	     had and keep the others (if any) for later.  */
	  while (n > 1)
	    Vunread_command_events
	      = Fcons (events[--n], Vunread_command_events);
	  return events[0];
	}
#endif
    }
}

/* Read a character from the keyboard; call the redisplay if needed.  */
/* commandflag 0 means do not autosave, but do redisplay.
   -1 means do not redisplay, but do autosave.
   -2 means do neither.
   1 means do both.

   The argument MAP is a keymap for menu prompting.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence (or not reading a key sequence).
   If PREV_EVENT is t, that is a "magic" value that says
   not to run input methods, but in other respects to act as if
   not reading a key sequence.

   If USED_MOUSE_MENU is non-null, then set *USED_MOUSE_MENU to true
   if we used a mouse menu to read the input, or false otherwise.  If
   USED_MOUSE_MENU is null, don't dereference it.

   Value is -2 when we find input on another keyboard.  A second call
   to read_char will read it.

   If END_TIME is non-null, it is a pointer to a struct timespec
   specifying the maximum time to wait until.  If no input arrives by
   that time, stop waiting and return nil.

   Value is t if we showed a menu and the user rejected it.  */

Lisp_Object
read_char (int commandflag, Lisp_Object map,
	   Lisp_Object prev_event,
	   bool *used_mouse_menu, struct timespec *end_time)
{
  Lisp_Object c;
  ptrdiff_t jmpcount;
  sys_jmp_buf local_getcjmp;
  sys_jmp_buf save_jump;
  Lisp_Object tem, save;
  volatile Lisp_Object previous_echo_area_message;
  volatile Lisp_Object also_record;
  volatile bool reread, recorded;
  bool volatile polling_stopped_here = false;
  struct kboard *orig_kboard = current_kboard;

  also_record = Qnil;

  c = Qnil;
  previous_echo_area_message = Qnil;

 retry:

  recorded = false;

  if (CONSP (Vunread_post_input_method_events))
    {
      c = XCAR (Vunread_post_input_method_events);
      Vunread_post_input_method_events
	= XCDR (Vunread_post_input_method_events);

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && (SYMBOLP (XCAR (c)) || FIXNUMP (XCAR (c)))
	  && NILP (XCDR (c)))
	c = XCAR (c);

      reread = true;
      goto reread_first;
    }
  else
    reread = false;


  if (CONSP (Vunread_command_events))
    {
      bool was_disabled = false;

      c = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);

      /* Undo what sit-for did when it unread additional keys
	 inside universal-argument.  */

      if (CONSP (c) && EQ (XCAR (c), Qt))
	c = XCDR (c);
      else
	{
	  if (CONSP (c) && EQ (XCAR (c), Qno_record))
	    {
	      c = XCDR (c);
	      recorded = true;
	    }
	  reread = true;
	}

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && EQ (XCDR (c), Qdisabled)
	  && (SYMBOLP (XCAR (c)) || FIXNUMP (XCAR (c))))
	{
	  was_disabled = true;
	  c = XCAR (c);
	}

      /* If the queued event is something that used the mouse,
         set used_mouse_menu accordingly.  */
      if (used_mouse_menu
	  /* Also check was_disabled so last-nonmenu-event won't return
	     a bad value when submenus are involved.  (Bug#447)  */
	  && (EQ (c, Qtool_bar) || EQ (c, Qtab_bar) || EQ (c, Qmenu_bar)
	      || was_disabled))
	*used_mouse_menu = true;

      goto reread_for_input_method;
    }

  if (CONSP (Vunread_input_method_events))
    {
      c = XCAR (Vunread_input_method_events);
      Vunread_input_method_events = XCDR (Vunread_input_method_events);

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && (SYMBOLP (XCAR (c)) || FIXNUMP (XCAR (c)))
	  && NILP (XCDR (c)))
	c = XCAR (c);
      reread = true;
      goto reread_for_input_method;
    }

  if (!NILP (Vexecuting_kbd_macro))
    {
      /* We set this to Qmacro; since that's not a frame, nobody will
	 try to switch frames on us, and the selected window will
	 remain unchanged.

         Since this event came from a macro, it would be misleading to
	 leave internal_last_event_frame set to wherever the last
	 real event came from.  Normally, a switch-frame event selects
	 internal_last_event_frame after each command is read, but
	 events read from a macro should never cause a new frame to be
	 selected.  */
      Vlast_event_frame = internal_last_event_frame = Qmacro;

      /* Exit the macro if we are at the end.
	 Also, some things replace the macro with t
	 to force an early exit.  */
      if (EQ (Vexecuting_kbd_macro, Qt)
	  || executing_kbd_macro_index >= XFIXNAT (Flength (Vexecuting_kbd_macro)))
	{
	  XSETINT (c, -1);
	  goto exit;
	}

      c = Faref (Vexecuting_kbd_macro, make_int (executing_kbd_macro_index));
      if (STRINGP (Vexecuting_kbd_macro)
	  && (XFIXNAT (c) & 0x80) && (XFIXNAT (c) <= 0xff))
	XSETFASTINT (c, CHAR_META | (XFIXNAT (c) & ~0x80));

      executing_kbd_macro_index++;

      goto from_macro;
    }

  if (!NILP (unread_switch_frame))
    {
      c = unread_switch_frame;
      unread_switch_frame = Qnil;

      /* This event should make it into this_command_keys, and get echoed
	 again, so we do not set `reread'.  */
      goto reread_first;
    }

  /* If redisplay was requested.  */
  if (commandflag >= 0)
    {
      bool echo_current = EQ (echo_message_buffer, echo_area_buffer[0]);

	/* If there is pending input, process any events which are not
	   user-visible, such as X selection_request events.  */
      if (input_pending
	  || detect_input_pending_run_timers (0))
	swallow_events (false);		/* May clear input_pending.  */

      /* Redisplay if no pending input.  */
      while (!(input_pending
	       && (input_was_pending || !redisplay_dont_pause)))
	{
	  input_was_pending = input_pending;
	  if (help_echo_showing_p && !EQ (selected_window, minibuf_window))
	    redisplay_preserve_echo_area (5);
	  else
	    redisplay ();

	  if (!input_pending)
	    /* Normal case: no input arrived during redisplay.  */
	    break;

	  /* Input arrived and pre-empted redisplay.
	     Process any events which are not user-visible.  */
	  swallow_events (false);
	  /* If that cleared input_pending, try again to redisplay.  */
	}

      /* Prevent the redisplay we just did
	 from messing up echoing of the input after the prompt.  */
      if (commandflag == 0 && echo_current)
	echo_message_buffer = echo_area_buffer[0];

    }

  /* Message turns off echoing unless more keystrokes turn it on again.

     The code in 20.x for the condition was

     1. echo_area_glyphs && *echo_area_glyphs
     2. && echo_area_glyphs != current_kboard->echobuf
     3. && ok_to_echo_at_next_pause != echo_area_glyphs

     (1) means there's a current message displayed

     (2) means it's not the message from echoing from the current
     kboard.

     (3) There's only one place in 20.x where ok_to_echo_at_next_pause
     is set to a non-null value.  This is done in read_char and it is
     set to echo_area_glyphs.  That means
     ok_to_echo_at_next_pause is either null or
     current_kboard->echobuf with the appropriate current_kboard at
     that time.

     So, condition (3) means in clear text ok_to_echo_at_next_pause
     must be either null, or the current message isn't from echoing at
     all, or it's from echoing from a different kboard than the
     current one.  */

  if (/* There currently is something in the echo area.  */
      !NILP (echo_area_buffer[0])
      && (/* It's an echo from a different kboard.  */
	  echo_kboard != current_kboard
	  /* Or we explicitly allow overwriting whatever there is.  */
	  || ok_to_echo_at_next_pause == NULL))
    cancel_echoing ();
  else
    echo_dash ();

  /* Try reading a character via menu prompting in the minibuf.
     Try this before the sit-for, because the sit-for
     would do the wrong thing if we are supposed to do
     menu prompting. If EVENT_HAS_PARAMETERS then we are reading
     after a mouse event so don't try a minibuf menu.  */
  c = Qnil;
  if (KEYMAPP (map) && INTERACTIVE
      && !NILP (prev_event) && ! EVENT_HAS_PARAMETERS (prev_event)
      /* Don't bring up a menu if we already have another event.  */
      && !CONSP (Vunread_command_events)
      && !detect_input_pending_run_timers (0))
    {
      c = read_char_minibuf_menu_prompt (commandflag, map);

      if (FIXNUMP (c) && XFIXNUM (c) == -2)
        return c;               /* wrong_kboard_jmpbuf */

      if (! NILP (c))
	goto exit;
    }

  /* Make a longjmp point for quits to use, but don't alter getcjmp just yet.
     We will do that below, temporarily for short sections of code,
     when appropriate.  local_getcjmp must be in effect
     around any call to sit_for or kbd_buffer_get_event;
     it *must not* be in effect when we call redisplay.  */

  jmpcount = SPECPDL_INDEX ();
  if (sys_setjmp (local_getcjmp))
    {
      /* Handle quits while reading the keyboard.  */
      /* We must have saved the outer value of getcjmp here,
	 so restore it now.  */
      restore_getcjmp (save_jump);
      pthread_sigmask (SIG_SETMASK, &empty_mask, 0);
      unbind_to (jmpcount, Qnil);
      /* If we are in while-no-input, don't trigger C-g, as that will
	 quit instead of letting while-no-input do its thing.  */
      if (!EQ (Vquit_flag, Vthrow_on_input))
	XSETINT (c, quit_char);
      internal_last_event_frame = selected_frame;
      Vlast_event_frame = internal_last_event_frame;
      /* If we report the quit char as an event,
	 don't do so more than once.  */
      if (!NILP (Vinhibit_quit))
	Vquit_flag = Qnil;

      {
	KBOARD *kb = FRAME_KBOARD (XFRAME (selected_frame));
	if (kb != current_kboard)
	  {
	    Lisp_Object last = KVAR (kb, kbd_queue);
	    /* We shouldn't get here if we were in single-kboard mode!  */
	    if (single_kboard)
	      emacs_abort ();
	    if (CONSP (last))
	      {
		while (CONSP (XCDR (last)))
		  last = XCDR (last);
		if (!NILP (XCDR (last)))
		  emacs_abort ();
	      }
	    if (!CONSP (last))
	      kset_kbd_queue (kb, list1 (c));
	    else
	      XSETCDR (last, list1 (c));
	    kb->kbd_queue_has_data = true;
	    current_kboard = kb;
            return make_fixnum (-2); /* wrong_kboard_jmpbuf */
	  }
      }
      goto non_reread;
    }

  /* Start idle timers if no time limit is supplied.  We don't do it
     if a time limit is supplied to avoid an infinite recursion in the
     situation where an idle timer calls `sit-for'.  */

  if (!end_time)
    timer_start_idle ();

  /* If in middle of key sequence and minibuffer not active,
     start echoing if enough time elapses.  */

  if (minibuf_level == 0
      && !end_time
      && !current_kboard->immediate_echo
      && (this_command_key_count > 0
	  || !NILP (call0 (Qinternal_echo_keystrokes_prefix)))
      && ! noninteractive
      && echo_keystrokes_p ()
      && (/* No message.  */
	  NILP (echo_area_buffer[0])
	  /* Or empty message.  */
	  || (BUF_BEG (XBUFFER (echo_area_buffer[0]))
	      == BUF_Z (XBUFFER (echo_area_buffer[0])))
	  /* Or already echoing from same kboard.  */
	  || (echo_kboard && ok_to_echo_at_next_pause == echo_kboard)
	  /* Or not echoing before and echoing allowed.  */
	  || (!echo_kboard && ok_to_echo_at_next_pause)))
    {
      /* After a mouse event, start echoing right away.
	 This is because we are probably about to display a menu,
	 and we don't want to delay before doing so.  */
      if (EVENT_HAS_PARAMETERS (prev_event))
	echo_now ();
      else
	{
	  Lisp_Object tem0;

	  ptrdiff_t count = SPECPDL_INDEX ();
	  save_getcjmp (save_jump);
	  record_unwind_protect_ptr (restore_getcjmp, save_jump);
	  restore_getcjmp (local_getcjmp);
	  tem0 = sit_for (Vecho_keystrokes, 1, 1);
	  unbind_to (count, Qnil);
	  if (EQ (tem0, Qt)
	      && ! CONSP (Vunread_command_events))
	    echo_now ();
	}
    }

  /* Maybe auto save due to number of keystrokes.  */

  if (commandflag != 0 && commandflag != -2
      && auto_save_interval > 0
      && num_nonmacro_input_events - last_auto_save > max (auto_save_interval, 20)
      && !detect_input_pending_run_timers (0))
    {
      Fdo_auto_save (auto_save_no_message ? Qt : Qnil, Qnil);
      /* Hooks can actually change some buffers in auto save.  */
      redisplay ();
    }

  /* Try reading using an X menu.
     This is never confused with reading using the minibuf
     because the recursive call of read_char in read_char_minibuf_menu_prompt
     does not pass on any keymaps.  */

  if (KEYMAPP (map) && INTERACTIVE
      && !NILP (prev_event)
      && EVENT_HAS_PARAMETERS (prev_event)
      && !EQ (XCAR (prev_event), Qmenu_bar)
      && !EQ (XCAR (prev_event), Qtab_bar)
      && !EQ (XCAR (prev_event), Qtool_bar)
      /* Don't bring up a menu if we already have another event.  */
      && !CONSP (Vunread_command_events))
    {
      c = read_char_x_menu_prompt (map, prev_event, used_mouse_menu);

      /* Now that we have read an event, Emacs is not idle.  */
      if (!end_time)
	timer_stop_idle ();

      goto exit;
    }

  /* Maybe autosave and/or garbage collect due to idleness.  */

  if (INTERACTIVE && NILP (c))
    {
      int delay_level;
      ptrdiff_t buffer_size;

      /* Slow down auto saves logarithmically in size of current buffer,
	 and garbage collect while we're at it.  */
      if (! MINI_WINDOW_P (XWINDOW (selected_window)))
	last_non_minibuf_size = Z - BEG;
      buffer_size = (last_non_minibuf_size >> 8) + 1;
      delay_level = 0;
      while (buffer_size > 64)
	delay_level++, buffer_size -= buffer_size >> 2;
      if (delay_level < 4) delay_level = 4;
      /* delay_level is 4 for files under around 50k, 7 at 100k,
	 9 at 200k, 11 at 300k, and 12 at 500k.  It is 15 at 1 meg.  */

      /* Auto save if enough time goes by without input.  */
      if (commandflag != 0 && commandflag != -2
	  && num_nonmacro_input_events > last_auto_save
	  && FIXNUMP (Vauto_save_timeout)
	  && XFIXNUM (Vauto_save_timeout) > 0)
	{
	  Lisp_Object tem0;
	  EMACS_INT timeout = XFIXNAT (Vauto_save_timeout);

	  timeout = min (timeout, MOST_POSITIVE_FIXNUM / delay_level * 4);
	  timeout = delay_level * timeout / 4;
	  ptrdiff_t count1 = SPECPDL_INDEX ();
	  save_getcjmp (save_jump);
	  record_unwind_protect_ptr (restore_getcjmp, save_jump);
	  restore_getcjmp (local_getcjmp);
	  tem0 = sit_for (make_fixnum (timeout), 1, 1);
	  unbind_to (count1, Qnil);

	  if (EQ (tem0, Qt)
	      && ! CONSP (Vunread_command_events))
	    {
	      Fdo_auto_save (auto_save_no_message ? Qt : Qnil, Qnil);
	      redisplay ();
	    }
	}

      /* If there is still no input available, ask for GC.  */
      if (!detect_input_pending_run_timers (0))
	maybe_gc ();
    }

  /* Notify the caller if an autosave hook, or a timer, sentinel or
     filter in the sit_for calls above have changed the current
     kboard.  This could happen if they use the minibuffer or start a
     recursive edit, like the fancy splash screen in server.el's
     filter.  If this longjmp wasn't here, read_key_sequence would
     interpret the next key sequence using the wrong translation
     tables and function keymaps.  */
  if (NILP (c) && current_kboard != orig_kboard)
    return make_fixnum (-2);  /* wrong_kboard_jmpbuf */

  /* If this has become non-nil here, it has been set by a timer
     or sentinel or filter.  */
  if (CONSP (Vunread_command_events))
    {
      c = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);

      if (CONSP (c) && EQ (XCAR (c), Qt))
	c = XCDR (c);
      else
	{
	  if (CONSP (c) && EQ (XCAR (c), Qno_record))
	    {
	      c = XCDR (c);
	      recorded = true;
	    }
	  reread = true;
	}
    }

  /* Read something from current KBOARD's side queue, if possible.  */

  if (NILP (c))
    {
      if (current_kboard->kbd_queue_has_data)
	{
	  if (!CONSP (KVAR (current_kboard, kbd_queue)))
	    emacs_abort ();
	  c = XCAR (KVAR (current_kboard, kbd_queue));
	  kset_kbd_queue (current_kboard,
			  XCDR (KVAR (current_kboard, kbd_queue)));
	  if (NILP (KVAR (current_kboard, kbd_queue)))
	    current_kboard->kbd_queue_has_data = false;
	  input_pending = readable_events (0);
	  if (EVENT_HAS_PARAMETERS (c)
	      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qswitch_frame))
	    internal_last_event_frame = XCAR (XCDR (c));
	  Vlast_event_frame = internal_last_event_frame;
	}
    }

  /* If current_kboard's side queue is empty check the other kboards.
     If one of them has data that we have not yet seen here,
     switch to it and process the data waiting for it.

     Note: if the events queued up for another kboard
     have already been seen here, and therefore are not a complete command,
     the kbd_queue_has_data field is 0, so we skip that kboard here.
     That's to avoid an infinite loop switching between kboards here.  */
  if (NILP (c) && !single_kboard)
    {
      KBOARD *kb;
      for (kb = all_kboards; kb; kb = kb->next_kboard)
	if (kb->kbd_queue_has_data)
	  {
	    current_kboard = kb;
            return make_fixnum (-2); /* wrong_kboard_jmpbuf */
	  }
    }

 wrong_kboard:

  STOP_POLLING;

  if (NILP (c))
    {
      c = read_decoded_event_from_main_queue (end_time, local_getcjmp,
                                              prev_event, used_mouse_menu);
      if (NILP (c) && end_time
	  && timespec_cmp (*end_time, current_timespec ()) <= 0)
        {
          goto exit;
        }

      if (EQ (c, make_fixnum (-2)))
	return c;

      if (CONSP (c) && EQ (XCAR (c), Qt))
	c = XCDR (c);
      else if (CONSP (c) && EQ (XCAR (c), Qno_record))
	{
	  c = XCDR (c);
	  recorded = true;
	}
  }

 non_reread:

  if (!end_time)
    timer_stop_idle ();
  RESUME_POLLING;

  if (NILP (c))
    {
      if (commandflag >= 0
	  && !input_pending && !detect_input_pending_run_timers (0))
	redisplay ();

      goto wrong_kboard;
    }

  /* Buffer switch events are only for internal wakeups
     so don't show them to the user.
     Also, don't record a key if we already did.  */
  if (BUFFERP (c))
    goto exit;

  /* Process special events within read_char
     and loop around to read another event.  */
  save = Vquit_flag;
  Vquit_flag = Qnil;
  tem = access_keymap (get_keymap (Vspecial_event_map, 0, 1), c, 0, 0, 1);
  Vquit_flag = save;

  if (!NILP (tem))
    {
      struct buffer *prev_buffer = current_buffer;
      last_input_event = c;
      call4 (Qcommand_execute, tem, Qnil, Fvector (1, &last_input_event), Qt);

      if (CONSP (c)
          && (EQ (XCAR (c), Qselect_window)
              || EQ (XCAR (c), Qfocus_out)
#ifdef HAVE_DBUS
	      || EQ (XCAR (c), Qdbus_event)
#endif
#ifdef USE_FILE_NOTIFY
	      || EQ (XCAR (c), Qfile_notify)
#endif
#ifdef THREADS_ENABLED
	      || EQ (XCAR (c), Qthread_event)
#endif
	      || EQ (XCAR (c), Qconfig_changed_event))
          && !end_time)
	/* We stopped being idle for this event; undo that.  This
	   prevents automatic window selection (under
	   mouse-autoselect-window) from acting as a real input event, for
	   example banishing the mouse under mouse-avoidance-mode.  */
	timer_resume_idle ();

      if (current_buffer != prev_buffer)
	{
	  /* The command may have changed the keymaps.  Pretend there
	     is input in another keyboard and return.  This will
	     recalculate keymaps.  */
	  c = make_fixnum (-2);
	  goto exit;
	}
      else
	goto retry;
    }

  /* Handle things that only apply to characters.  */
  if (FIXNUMP (c))
    {
      /* If kbd_buffer_get_event gave us an EOF, return that.  */
      if (XFIXNUM (c) == -1)
	goto exit;

      if ((STRINGP (KVAR (current_kboard, Vkeyboard_translate_table))
	   && UNSIGNED_CMP (XFIXNAT (c), <,
			    SCHARS (KVAR (current_kboard,
					  Vkeyboard_translate_table))))
	  || (VECTORP (KVAR (current_kboard, Vkeyboard_translate_table))
	      && UNSIGNED_CMP (XFIXNAT (c), <,
			       ASIZE (KVAR (current_kboard,
					    Vkeyboard_translate_table))))
	  || (CHAR_TABLE_P (KVAR (current_kboard, Vkeyboard_translate_table))
	      && CHARACTERP (c)))
	{
	  Lisp_Object d;
	  d = Faref (KVAR (current_kboard, Vkeyboard_translate_table), c);
	  /* nil in keyboard-translate-table means no translation.  */
	  if (!NILP (d))
	    c = d;
	}
    }

  /* If this event is a mouse click in the menu bar,
     return just menu-bar for now.  Modify the mouse click event
     so we won't do this twice, then queue it up.  */
  if (EVENT_HAS_PARAMETERS (c)
      && CONSP (XCDR (c))
      && CONSP (xevent_start (c))
      && CONSP (XCDR (xevent_start (c))))
    {
      Lisp_Object posn;

      posn = POSN_POSN (xevent_start (c));
      /* Handle menu-bar events:
	 insert the dummy prefix event `menu-bar'.  */
      if (EQ (posn, Qmenu_bar) || EQ (posn, Qtab_bar) || EQ (posn, Qtool_bar))
	{
	  /* Change menu-bar to (menu-bar) as the event "position".  */
	  POSN_SET_POSN (xevent_start (c), list1 (posn));

	  also_record = c;
	  Vunread_command_events = Fcons (c, Vunread_command_events);
	  c = posn;
	}
    }

  /* Store these characters into recent_keys, the dribble file if any,
     and the keyboard macro being defined, if any.  */
  record_char (c);
  recorded = true;
  if (! NILP (also_record))
    record_char (also_record);

  /* Wipe the echo area.
     But first, if we are about to use an input method,
     save the echo area contents for it to refer to.  */
  if (FIXNUMP (c)
      && ! NILP (Vinput_method_function)
      && ' ' <= XFIXNUM (c) && XFIXNUM (c) < 256 && XFIXNUM (c) != 127)
    {
      previous_echo_area_message = Fcurrent_message ();
      Vinput_method_previous_message = previous_echo_area_message;
    }

  /* Now wipe the echo area, except for help events which do their
     own stuff with the echo area.  */
  if (!CONSP (c)
      || (!(EQ (Qhelp_echo, XCAR (c)))
	  && !(EQ (Qswitch_frame, XCAR (c)))
	  /* Don't wipe echo area for select window events: These might
	     get delayed via `mouse-autoselect-window' (Bug#11304).  */
	  && !(EQ (Qselect_window, XCAR (c)))))
    {
      if (!NILP (echo_area_buffer[0]))
	{
	  safe_run_hooks (Qecho_area_clear_hook);
	  clear_message (1, 0);
	  /* If we were showing the echo-area message on top of an
	     active minibuffer, resize the mini-window, since the
	     minibuffer may need more or less space than the echo area
	     we've just wiped.  */
	  if (minibuf_level
	      && EQ (minibuf_window, echo_area_window)
	      /* The case where minibuffer-message-timeout is a number
		 was already handled near the beginning of command_loop_1.  */
	      && !NUMBERP (Vminibuffer_message_timeout))
	    resize_mini_window (XWINDOW (minibuf_window), false);
	}
      else if (FUNCTIONP (Vclear_message_function))
        clear_message (1, 0);
    }

 reread_for_input_method:
 from_macro:
  /* Pass this to the input method, if appropriate.  */
  if (FIXNUMP (c)
      && ! NILP (Vinput_method_function)
      /* Don't run the input method within a key sequence,
	 after the first event of the key sequence.  */
      && NILP (prev_event)
      && ' ' <= XFIXNUM (c) && XFIXNUM (c) < 256 && XFIXNUM (c) != 127)
    {
      Lisp_Object keys;
      ptrdiff_t key_count;
      ptrdiff_t command_key_start;
      ptrdiff_t count = SPECPDL_INDEX ();

      /* Save the echo status.  */
      bool saved_immediate_echo = current_kboard->immediate_echo;
      struct kboard *saved_ok_to_echo = ok_to_echo_at_next_pause;
      Lisp_Object saved_echo_string = KVAR (current_kboard, echo_string);
      Lisp_Object saved_echo_prompt = KVAR (current_kboard, echo_prompt);

      /* Save the this_command_keys status.  */
      key_count = this_command_key_count;
      command_key_start = this_single_command_key_start;

      if (key_count > 0)
	keys = Fcopy_sequence (this_command_keys);
      else
	keys = Qnil;

      /* Clear out this_command_keys.  */
      this_command_key_count = 0;
      this_single_command_key_start = 0;

      /* Now wipe the echo area.  */
      if (!NILP (echo_area_buffer[0]))
	safe_run_hooks (Qecho_area_clear_hook);
      clear_message (1, 0);
      echo_truncate (0);

      /* If we are not reading a key sequence,
	 never use the echo area.  */
      if (!KEYMAPP (map))
	{
	  specbind (Qinput_method_use_echo_area, Qt);
	}

      /* Call the input method.  */
      tem = call1 (Vinput_method_function, c);

      tem = unbind_to (count, tem);

      /* Restore the saved echoing state
	 and this_command_keys state.  */
      this_command_key_count = key_count;
      this_single_command_key_start = command_key_start;
      if (key_count > 0)
	this_command_keys = keys;

      cancel_echoing ();
      ok_to_echo_at_next_pause = saved_ok_to_echo;
      kset_echo_string (current_kboard, saved_echo_string);
      kset_echo_prompt (current_kboard, saved_echo_prompt);
      if (saved_immediate_echo)
	echo_now ();

      /* The input method can return no events.  */
      if (! CONSP (tem))
	{
	  /* Bring back the previous message, if any.  */
	  if (! NILP (previous_echo_area_message))
	    message_with_string ("%s", previous_echo_area_message, 0);
	  goto retry;
	}
      /* It returned one event or more.  */
      c = XCAR (tem);
      Vunread_post_input_method_events
	= nconc2 (XCDR (tem), Vunread_post_input_method_events);
    }
  /* When we consume events from the various unread-*-events lists, we
     bypass the code that records input, so record these events now if
     they were not recorded already.  */
  if (!recorded)
    {
      record_char (c);
      recorded = true;
    }

 reread_first:

  /* Display help if not echoing.  */
  if (CONSP (c) && EQ (XCAR (c), Qhelp_echo))
    {
      /* (help-echo FRAME HELP WINDOW OBJECT POS).  */
      Lisp_Object help, object, position, window, htem;

      htem = Fcdr (XCDR (c));
      help = Fcar (htem);
      htem = Fcdr (htem);
      window = Fcar (htem);
      htem = Fcdr (htem);
      object = Fcar (htem);
      htem = Fcdr (htem);
      position = Fcar (htem);

      show_help_echo (help, window, object, position);

      /* We stopped being idle for this event; undo that.  */
      if (!end_time)
	timer_resume_idle ();
      goto retry;
    }

  if ((! reread || this_command_key_count == 0)
      && !end_time)
    {

      /* Don't echo mouse motion events.  */
      if (! (EVENT_HAS_PARAMETERS (c)
	     && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_movement)))
	/* Once we reread a character, echoing can happen
	   the next time we pause to read a new one.  */
	ok_to_echo_at_next_pause = current_kboard;

      /* Record this character as part of the current key.  */
      add_command_key (c);
      if (! NILP (also_record))
	add_command_key (also_record);

      echo_update ();
    }

  last_input_event = c;
  num_input_events++;

  /* Process the help character specially if enabled.  */
  if (!NILP (Vhelp_form) && help_char_p (c))
    {
      ptrdiff_t count = SPECPDL_INDEX ();

      help_form_saved_window_configs
	= Fcons (Fcurrent_window_configuration (Qnil),
		 help_form_saved_window_configs);
      record_unwind_protect_void (read_char_help_form_unwind);
      call0 (Qhelp_form_show);

      cancel_echoing ();
      do
	{
	  c = read_char (0, Qnil, Qnil, 0, NULL);
	  if (EVENT_HAS_PARAMETERS (c)
	      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_click))
	    XSETCAR (help_form_saved_window_configs, Qnil);
	}
      while (BUFFERP (c));
      /* Remove the help from the frame.  */
      unbind_to (count, Qnil);

      redisplay ();
      if (EQ (c, make_fixnum (040)))
	{
	  cancel_echoing ();
	  do
	    c = read_char (0, Qnil, Qnil, 0, NULL);
	  while (BUFFERP (c));
	}
    }

 exit:
  RESUME_POLLING;
  input_was_pending = input_pending;
  return c;
}

/* Record a key that came from a mouse menu.
   Record it for echoing, for this-command-keys, and so on.  */

static void
record_menu_key (Lisp_Object c)
{
  /* Wipe the echo area.  */
  clear_message (1, 0);

  record_char (c);

  /* Once we reread a character, echoing can happen
     the next time we pause to read a new one.  */
  ok_to_echo_at_next_pause = NULL;

  /* Record this character as part of the current key.  */
  add_command_key (c);
  echo_update ();

  /* Re-reading in the middle of a command.  */
  last_input_event = c;
  num_input_events++;
}

/* Return true if should recognize C as "the help character".  */

static bool
help_char_p (Lisp_Object c)
{
  if (EQ (c, Vhelp_char))
    return true;
  Lisp_Object tail = Vhelp_event_list;
  FOR_EACH_TAIL_SAFE (tail)
    if (EQ (c, XCAR (tail)))
      return true;
  return false;
}

/* Record the input event C in various ways.  */

static void
record_char (Lisp_Object c)
{
  /* quail.el binds this to avoid recording keys twice.  */
  if (inhibit_record_char)
    return;

  int recorded = 0;

  if (CONSP (c) && (EQ (XCAR (c), Qhelp_echo) || EQ (XCAR (c), Qmouse_movement)))
    {
      /* To avoid filling recent_keys with help-echo and mouse-movement
	 events, we filter out repeated help-echo events, only store the
	 first and last in a series of mouse-movement events, and don't
	 store repeated help-echo events which are only separated by
	 mouse-movement events.  */

      Lisp_Object ev1, ev2, ev3;
      int ix1, ix2, ix3;

      if ((ix1 = recent_keys_index - 1) < 0)
	ix1 = NUM_RECENT_KEYS - 1;
      ev1 = AREF (recent_keys, ix1);

      if ((ix2 = ix1 - 1) < 0)
	ix2 = NUM_RECENT_KEYS - 1;
      ev2 = AREF (recent_keys, ix2);

      if ((ix3 = ix2 - 1) < 0)
	ix3 = NUM_RECENT_KEYS - 1;
      ev3 = AREF (recent_keys, ix3);

      if (EQ (XCAR (c), Qhelp_echo))
	{
	  /* Don't record `help-echo' in recent_keys unless it shows some help
	     message, and a different help than the previously recorded
	     event.  */
	  Lisp_Object help, last_help;

	  help = Fcar_safe (Fcdr_safe (XCDR (c)));
	  if (!STRINGP (help))
	    recorded = 1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev1))), EQ (last_help, help)))
	    recorded = 1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
		   && CONSP (ev2) && EQ (XCAR (ev2), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev2))), EQ (last_help, help)))
	    recorded = -1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
		   && CONSP (ev2) && EQ (XCAR (ev2), Qmouse_movement)
		   && CONSP (ev3) && EQ (XCAR (ev3), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev3))), EQ (last_help, help)))
	    recorded = -2;
	}
      else if (EQ (XCAR (c), Qmouse_movement))
	{
	  /* Only record one pair of `mouse-movement' on a window in recent_keys.
	     So additional mouse movement events replace the last element.  */
	  Lisp_Object last_window, window;

	  window = Fcar_safe (Fcar_safe (XCDR (c)));
	  if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
	      && (last_window = Fcar_safe (Fcar_safe (XCDR (ev1))), EQ (last_window, window))
	      && CONSP (ev2) && EQ (XCAR (ev2), Qmouse_movement)
	      && (last_window = Fcar_safe (Fcar_safe (XCDR (ev2))), EQ (last_window, window)))
	    {
	      ASET (recent_keys, ix1, c);
	      recorded = 1;
	    }
	}
    }
  else if (NILP (Vexecuting_kbd_macro))
    store_kbd_macro_char (c);

  /* recent_keys should not include events from keyboard macros.  */
  if (NILP (Vexecuting_kbd_macro))
    {
      if (!recorded)
	{
	  total_keys += total_keys < NUM_RECENT_KEYS;
	  ASET (recent_keys, recent_keys_index,
                /* Copy the event, in case it gets modified by side-effect
                   by some remapping function (bug#30955).  */
                CONSP (c) ? Fcopy_sequence (c) : c);
	  if (++recent_keys_index >= NUM_RECENT_KEYS)
	    recent_keys_index = 0;
	}
      else if (recorded < 0)
	{
	  /* We need to remove one or two events from recent_keys.
	     To do this, we simply put nil at those events and move the
	     recent_keys_index backwards over those events.  Usually,
	     users will never see those nil events, as they will be
	     overwritten by the command keys entered to see recent_keys
	     (e.g. C-h l).  */

	  while (recorded++ < 0 && total_keys > 0)
	    {
	      if (total_keys < NUM_RECENT_KEYS)
		total_keys--;
	      if (--recent_keys_index < 0)
		recent_keys_index = NUM_RECENT_KEYS - 1;
	      ASET (recent_keys, recent_keys_index, Qnil);
	    }
	}

      num_nonmacro_input_events++;
    }

  /* Write c to the dribble file.  If c is a lispy event, write
     the event's symbol to the dribble file, in <brackets>.  Bleaugh.
     If you, dear reader, have a better idea, you've got the source.  :-) */
  if (dribble && NILP (Vexecuting_kbd_macro))
    {
      block_input ();
      if (FIXNUMP (c))
	{
	  if (XUFIXNUM (c) < 0x100)
	    putc (XUFIXNUM (c), dribble);
	  else
	    fprintf (dribble, " 0x%"pI"x", XUFIXNUM (c));
	}
      else
	{
	  Lisp_Object dribblee;

	  /* If it's a structured event, take the event header.  */
	  dribblee = EVENT_HEAD (c);

	  if (SYMBOLP (dribblee))
	    {
	      putc ('<', dribble);
	      fwrite (SDATA (SYMBOL_NAME (dribblee)), sizeof (char),
		      SBYTES (SYMBOL_NAME (dribblee)), dribble);
	      putc ('>', dribble);
	    }
	}

      fflush (dribble);
      unblock_input ();
    }
}

/* Copy out or in the info on where C-g should throw to.
   This is used when running Lisp code from within get_char,
   in case get_char is called recursively.
   See read_process_output.  */

static void
save_getcjmp (sys_jmp_buf temp)
{
  memcpy (temp, getcjmp, sizeof getcjmp);
}

static void
restore_getcjmp (void *temp)
{
  memcpy (getcjmp, temp, sizeof getcjmp);
}

/* Low level keyboard/mouse input.
   kbd_buffer_store_event places events in kbd_buffer, and
   kbd_buffer_get_event retrieves them.  */

/* Return true if there are any events in the queue that read-char
   would return.  If this returns false, a read-char would block.  */
static bool
readable_events (int flags)
{
  if (flags & READABLE_EVENTS_DO_TIMERS_NOW)
    timer_check ();

  /* If the buffer contains only FOCUS_IN/OUT_EVENT events, and
     READABLE_EVENTS_FILTER_EVENTS is set, report it as empty.  */
  if (kbd_fetch_ptr != kbd_store_ptr)
    {
      if (flags & (READABLE_EVENTS_FILTER_EVENTS
#ifdef USE_TOOLKIT_SCROLL_BARS
		   | READABLE_EVENTS_IGNORE_SQUEEZABLES
#endif
		   ))
        {
          union buffered_input_event *event = kbd_fetch_ptr;

	  do
	    {
	      if (!(
#ifdef USE_TOOLKIT_SCROLL_BARS
		    (flags & READABLE_EVENTS_FILTER_EVENTS) &&
#endif
		    (event->kind == FOCUS_IN_EVENT
                     || event->kind == FOCUS_OUT_EVENT))
#ifdef USE_TOOLKIT_SCROLL_BARS
		  && !((flags & READABLE_EVENTS_IGNORE_SQUEEZABLES)
		       && (event->kind == SCROLL_BAR_CLICK_EVENT
			   || event->kind == HORIZONTAL_SCROLL_BAR_CLICK_EVENT)
		       && event->ie.part == scroll_bar_handle
		       && event->ie.modifiers == 0)
#endif
		  && !((flags & READABLE_EVENTS_FILTER_EVENTS)
		       && event->kind == BUFFER_SWITCH_EVENT))
		return 1;
	      event = next_kbd_event (event);
	    }
	  while (event != kbd_store_ptr);
        }
      else
	return 1;
    }

  if (!(flags & READABLE_EVENTS_IGNORE_SQUEEZABLES) && some_mouse_moved ())
    return 1;
  if (single_kboard)
    {
      if (current_kboard->kbd_queue_has_data)
	return 1;
    }
  else
    {
      KBOARD *kb;
      for (kb = all_kboards; kb; kb = kb->next_kboard)
	if (kb->kbd_queue_has_data)
	  return 1;
    }
  return 0;
}

/* Set this for debugging, to have a way to get out */
int stop_character EXTERNALLY_VISIBLE;

static KBOARD *
event_to_kboard (struct input_event *event)
{
  /* Not applicable for these special events.  */
  if (event->kind == SELECTION_REQUEST_EVENT
      || event->kind == SELECTION_CLEAR_EVENT)
    return NULL;
  else
    {
      Lisp_Object obj = event->frame_or_window;
      /* There are some events that set this field to nil or string.  */
      if (WINDOWP (obj))
	obj = WINDOW_FRAME (XWINDOW (obj));
      /* Also ignore dead frames here.  */
      return ((FRAMEP (obj) && FRAME_LIVE_P (XFRAME (obj)))
	      ? FRAME_KBOARD (XFRAME (obj)) : NULL);
    }
}

#ifdef subprocesses
/* Return the number of slots occupied in kbd_buffer.  */

static int
kbd_buffer_nr_stored (void)
{
  int n = kbd_store_ptr - kbd_fetch_ptr;
  return n + (n < 0 ? KBD_BUFFER_SIZE : 0);
}
#endif	/* Store an event obtained at interrupt level into kbd_buffer, fifo */

void
kbd_buffer_store_event (register struct input_event *event)
{
  kbd_buffer_store_event_hold (event, 0);
}

/* Store EVENT obtained at interrupt level into kbd_buffer, fifo.

   If HOLD_QUIT is 0, just stuff EVENT into the fifo.
   Else, if HOLD_QUIT.kind != NO_EVENT, discard EVENT.
   Else, if EVENT is a quit event, store the quit event
   in HOLD_QUIT, and return (thus ignoring further events).

   This is used to postpone the processing of the quit event until all
   subsequent input events have been parsed (and discarded).  */

void
kbd_buffer_store_buffered_event (union buffered_input_event *event,
				 struct input_event *hold_quit)
{
  if (event->kind == NO_EVENT)
    emacs_abort ();

  if (hold_quit && hold_quit->kind != NO_EVENT)
    return;

  if (event->kind == ASCII_KEYSTROKE_EVENT)
    {
      int c = event->ie.code & 0377;

      if (event->ie.modifiers & ctrl_modifier)
	c = make_ctrl_char (c);

      c |= (event->ie.modifiers
	    & (meta_modifier | alt_modifier
	       | hyper_modifier | super_modifier));

      if (c == quit_char)
	{
	  KBOARD *kb = FRAME_KBOARD (XFRAME (event->ie.frame_or_window));

	  if (single_kboard && kb != current_kboard)
	    {
	      kset_kbd_queue
		(kb, list2 (make_lispy_switch_frame (event->ie.frame_or_window),
			    make_fixnum (c)));
	      kb->kbd_queue_has_data = true;

	      for (union buffered_input_event *sp = kbd_fetch_ptr;
		   sp != kbd_store_ptr; sp = next_kbd_event (sp))
		{
		  if (event_to_kboard (&sp->ie) == kb)
		    {
		      sp->ie.kind = NO_EVENT;
		      sp->ie.frame_or_window = Qnil;
		      sp->ie.arg = Qnil;
		    }
		}
	      return;
	    }

	  if (hold_quit)
	    {
	      *hold_quit = event->ie;
	      return;
	    }

	  /* If this results in a quit_char being returned to Emacs as
	     input, set Vlast_event_frame properly.  If this doesn't
	     get returned to Emacs as an event, the next event read
	     will set Vlast_event_frame again, so this is safe to do.  */
	  {
	    Lisp_Object focus;

	    focus = FRAME_FOCUS_FRAME (XFRAME (event->ie.frame_or_window));
	    if (NILP (focus))
	      focus = event->ie.frame_or_window;
	    internal_last_event_frame = focus;
	    Vlast_event_frame = focus;
	  }

	  handle_interrupt (0);
	  return;
	}

      if (c && c == stop_character)
	{
	  sys_suspend ();
	  return;
	}
    }
  /* Don't insert two BUFFER_SWITCH_EVENT's in a row.
     Just ignore the second one.  */
  else if (event->kind == BUFFER_SWITCH_EVENT
	   && kbd_fetch_ptr != kbd_store_ptr
	   && prev_kbd_event (kbd_store_ptr)->kind == BUFFER_SWITCH_EVENT)
    return;

  /* Don't let the very last slot in the buffer become full,
     since that would make the two pointers equal,
     and that is indistinguishable from an empty buffer.
     Discard the event if it would fill the last slot.  */
  union buffered_input_event *next_slot = next_kbd_event (kbd_store_ptr);
  if (kbd_fetch_ptr != next_slot)
    {
      *kbd_store_ptr = *event;
      kbd_store_ptr = next_slot;
#ifdef subprocesses
      if (kbd_buffer_nr_stored () > KBD_BUFFER_SIZE / 2
	  && ! kbd_on_hold_p ())
        {
          /* Don't read keyboard input until we have processed kbd_buffer.
             This happens when pasting text longer than KBD_BUFFER_SIZE/2.  */
          hold_keyboard_input ();
          unrequest_sigio ();
          stop_polling ();
        }
#endif	/* subprocesses */
    }

  Lisp_Object ignore_event;

  switch (event->kind)
    {
    case FOCUS_IN_EVENT: ignore_event = Qfocus_in; break;
    case FOCUS_OUT_EVENT: ignore_event = Qfocus_out; break;
    case HELP_EVENT: ignore_event = Qhelp_echo; break;
    case ICONIFY_EVENT: ignore_event = Qiconify_frame; break;
    case DEICONIFY_EVENT: ignore_event = Qmake_frame_visible; break;
    case SELECTION_REQUEST_EVENT: ignore_event = Qselection_request; break;
    case BUFFER_SWITCH_EVENT: ignore_event = Qbuffer_switch; break;
    default: ignore_event = Qnil; break;
    }

  /* If we're inside while-no-input, and this event qualifies
     as input, set quit-flag to cause an interrupt.  */
  if (!NILP (Vthrow_on_input)
      && NILP (Fmemq (ignore_event, Vwhile_no_input_ignore_events)))
    Vquit_flag = Vthrow_on_input;
}


#ifdef HAVE_X11

/* Put a selection input event back in the head of the event queue.  */

void
kbd_buffer_unget_event (struct selection_input_event *event)
{
  /* Don't let the very last slot in the buffer become full,  */
  union buffered_input_event *kp = prev_kbd_event (kbd_fetch_ptr);
  if (kp != kbd_store_ptr)
    {
      kp->sie = *event;
      kbd_fetch_ptr = kp;
    }
}

#endif

/* Limit help event positions to this range, to avoid overflow problems.  */
#define INPUT_EVENT_POS_MAX \
  ((ptrdiff_t) min (PTRDIFF_MAX, min (TYPE_MAXIMUM (Time) / 2, \
				      MOST_POSITIVE_FIXNUM)))
#define INPUT_EVENT_POS_MIN (-1 - INPUT_EVENT_POS_MAX)

/* Return a Time that encodes position POS.  POS must be in range.  */

static Time
position_to_Time (ptrdiff_t pos)
{
  eassert (INPUT_EVENT_POS_MIN <= pos && pos <= INPUT_EVENT_POS_MAX);
  return pos;
}

/* Return the position that ENCODED_POS encodes.
   Avoid signed integer overflow.  */

static ptrdiff_t
Time_to_position (Time encoded_pos)
{
  if (encoded_pos <= INPUT_EVENT_POS_MAX)
    return encoded_pos;
  Time encoded_pos_min = INPUT_EVENT_POS_MIN;
  eassert (encoded_pos_min <= encoded_pos);
  ptrdiff_t notpos = -1 - encoded_pos;
  return -1 - notpos;
}

/* Generate a HELP_EVENT input_event and store it in the keyboard
   buffer.

   HELP is the help form.

   FRAME and WINDOW are the frame and window where the help is
   generated.  OBJECT is the Lisp object where the help was found (a
   buffer, a string, an overlay, or nil if neither from a string nor
   from a buffer).  POS is the position within OBJECT where the help
   was found.  */

void
gen_help_event (Lisp_Object help, Lisp_Object frame, Lisp_Object window,
		Lisp_Object object, ptrdiff_t pos)
{
  struct input_event event;

  event.kind = HELP_EVENT;
  event.frame_or_window = frame;
  event.arg = object;
  event.x = WINDOWP (window) ? window : frame;
  event.y = help;
  event.timestamp = position_to_Time (pos);
  kbd_buffer_store_event (&event);
}


/* Store HELP_EVENTs for HELP on FRAME in the input queue.  */

void
kbd_buffer_store_help_event (Lisp_Object frame, Lisp_Object help)
{
  struct input_event event;

  event.kind = HELP_EVENT;
  event.frame_or_window = frame;
  event.arg = Qnil;
  event.x = Qnil;
  event.y = help;
  event.timestamp = 0;
  kbd_buffer_store_event (&event);
}


/* Discard any mouse events in the event buffer by setting them to
   NO_EVENT.  */
void
discard_mouse_events (void)
{
  for (union buffered_input_event *sp = kbd_fetch_ptr;
       sp != kbd_store_ptr; sp = next_kbd_event (sp))
    {
      if (sp->kind == MOUSE_CLICK_EVENT
	  || sp->kind == WHEEL_EVENT
          || sp->kind == HORIZ_WHEEL_EVENT
#ifdef HAVE_GPM
	  || sp->kind == GPM_CLICK_EVENT
#endif
	  || sp->kind == SCROLL_BAR_CLICK_EVENT
	  || sp->kind == HORIZONTAL_SCROLL_BAR_CLICK_EVENT)
	{
	  sp->kind = NO_EVENT;
	}
    }
}


/* Return true if there are any real events waiting in the event
   buffer, not counting `NO_EVENT's.

   Discard NO_EVENT events at the front of the input queue, possibly
   leaving the input queue empty if there are no real input events.  */

bool
kbd_buffer_events_waiting (void)
{
  for (union buffered_input_event *sp = kbd_fetch_ptr;
       ; sp = next_kbd_event (sp))
    if (sp == kbd_store_ptr || sp->kind != NO_EVENT)
      {
	kbd_fetch_ptr = sp;
	return sp != kbd_store_ptr && sp->kind != NO_EVENT;
      }
}


/* Clear input event EVENT.  */

static void
clear_event (struct input_event *event)
{
  event->kind = NO_EVENT;
}


/* Read one event from the event buffer, waiting if necessary.
   The value is a Lisp object representing the event.
   The value is nil for an event that should be ignored,
   or that was handled here.
   We always read and discard one event.  */

static Lisp_Object
kbd_buffer_get_event (KBOARD **kbp,
                      bool *used_mouse_menu,
                      struct timespec *end_time)
{
  Lisp_Object obj;

#ifdef subprocesses
  if (kbd_on_hold_p () && kbd_buffer_nr_stored () < KBD_BUFFER_SIZE / 4)
    {
      /* Start reading input again because we have processed enough to
         be able to accept new events again.  */
      unhold_keyboard_input ();
      request_sigio ();
      start_polling ();
    }
#endif	/* subprocesses */

#if !defined HAVE_DBUS && !defined USE_FILE_NOTIFY && !defined THREADS_ENABLED
  if (noninteractive
      /* In case we are running as a daemon, only do this before
	 detaching from the terminal.  */
      || (IS_DAEMON && DAEMON_RUNNING))
    {
      int c = getchar ();
      XSETINT (obj, c);
      *kbp = current_kboard;
      return obj;
    }
#endif	/* !defined HAVE_DBUS && !defined USE_FILE_NOTIFY && !defined THREADS_ENABLED  */

  /* Wait until there is input available.  */
  for (;;)
    {
      /* Break loop if there's an unread command event.  Needed in
	 moused window autoselection which uses a timer to insert such
	 events.  */
      if (CONSP (Vunread_command_events))
	break;

      if (kbd_fetch_ptr != kbd_store_ptr)
	break;
      if (some_mouse_moved ())
	break;

      /* If the quit flag is set, then read_char will return
	 quit_char, so that counts as "available input."  */
      if (!NILP (Vquit_flag))
	quit_throw_to_read_char (0);

      /* One way or another, wait until input is available; then, if
	 interrupt handlers have not read it, read it now.  */

#ifdef USABLE_SIGIO
      gobble_input ();
#endif
      if (kbd_fetch_ptr != kbd_store_ptr)
	break;
      if (some_mouse_moved ())
	break;
      if (end_time)
	{
	  struct timespec now = current_timespec ();
	  if (timespec_cmp (*end_time, now) <= 0)
	    return Qnil;	/* Finished waiting.  */
	  else
	    {
	      struct timespec duration = timespec_sub (*end_time, now);
	      wait_reading_process_output (min (duration.tv_sec,
						WAIT_READING_MAX),
					   duration.tv_nsec,
					   -1, 1, Qnil, NULL, 0);
	    }
	}
      else
	{
	  bool do_display = true;

	  if (FRAME_TERMCAP_P (SELECTED_FRAME ()))
	    {
	      struct tty_display_info *tty = CURTTY ();

	      /* When this TTY is displaying a menu, we must prevent
		 any redisplay, because we modify the frame's glyph
		 matrix behind the back of the display engine.  */
	      if (tty->showing_menu)
		do_display = false;
	    }

	  wait_reading_process_output (0, 0, -1, do_display, Qnil, NULL, 0);
	}

      if (!interrupt_input && kbd_fetch_ptr == kbd_store_ptr)
	gobble_input ();
    }

  if (CONSP (Vunread_command_events))
    {
      Lisp_Object first;
      first = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);
      *kbp = current_kboard;
      return first;
    }

  /* At this point, we know that there is a readable event available
     somewhere.  If the event queue is empty, then there must be a
     mouse movement enabled and available.  */
  if (kbd_fetch_ptr != kbd_store_ptr)
    {
      union buffered_input_event *event = kbd_fetch_ptr;

      *kbp = event_to_kboard (&event->ie);
      if (*kbp == 0)
	*kbp = current_kboard;  /* Better than returning null ptr?  */

      obj = Qnil;

      /* These two kinds of events get special handling
	 and don't actually appear to the command loop.
	 We return nil for them.  */
      switch (event->kind)
      {
      case SELECTION_REQUEST_EVENT:
      case SELECTION_CLEAR_EVENT:
	{
#ifdef HAVE_X11
	  /* Remove it from the buffer before processing it,
	     since otherwise swallow_events will see it
	     and process it again.  */
	  struct selection_input_event copy = event->sie;
	  kbd_fetch_ptr = next_kbd_event (event);
	  input_pending = readable_events (0);
	  x_handle_selection_event (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  emacs_abort ();
#endif
	}
        break;

#ifdef HAVE_EXT_MENU_BAR
      case MENU_BAR_ACTIVATE_EVENT:
	{
          struct frame *f;
	  kbd_fetch_ptr = next_kbd_event (event);
	  input_pending = readable_events (0);
          f = (XFRAME (event->ie.frame_or_window));
	  if (FRAME_LIVE_P (f) && FRAME_TERMINAL (f)->activate_menubar_hook)
	    FRAME_TERMINAL (f)->activate_menubar_hook (f);
	}
        break;
#endif
#if defined (HAVE_NS)
      case NS_TEXT_EVENT:
	if (used_mouse_menu)
	  *used_mouse_menu = true;
	FALLTHROUGH;
#endif
#ifdef HAVE_NTGUI
      case END_SESSION_EVENT:
      case LANGUAGE_CHANGE_EVENT:
#endif
#ifdef HAVE_WINDOW_SYSTEM
      case DELETE_WINDOW_EVENT:
      case ICONIFY_EVENT:
      case DEICONIFY_EVENT:
      case MOVE_FRAME_EVENT:
#endif
#ifdef USE_FILE_NOTIFY
      case FILE_NOTIFY_EVENT:
#endif
#ifdef HAVE_DBUS
      case DBUS_EVENT:
#endif
#ifdef THREADS_ENABLED
      case THREAD_EVENT:
#endif
#ifdef HAVE_XWIDGETS
      case XWIDGET_EVENT:
#endif
      case BUFFER_SWITCH_EVENT:
      case SAVE_SESSION_EVENT:
      case NO_EVENT:
      case HELP_EVENT:
      case FOCUS_IN_EVENT:
      case CONFIG_CHANGED_EVENT:
      case FOCUS_OUT_EVENT:
      case SELECT_WINDOW_EVENT:
        {
          obj = make_lispy_event (&event->ie);
          kbd_fetch_ptr = next_kbd_event (event);
        }
        break;
      default:
	{
	  /* If this event is on a different frame, return a
	     switch-frame this time, and leave the event in the queue
	     for next time.  */
	  Lisp_Object frame;
	  Lisp_Object focus;

	  frame = event->ie.frame_or_window;
	  if (CONSP (frame))
	    frame = XCAR (frame);
	  else if (WINDOWP (frame))
	    frame = WINDOW_FRAME (XWINDOW (frame));

	  focus = FRAME_FOCUS_FRAME (XFRAME (frame));
	  if (! NILP (focus))
	    frame = focus;

	  if (!EQ (frame, internal_last_event_frame)
	      && !EQ (frame, selected_frame))
	    obj = make_lispy_switch_frame (frame);
	  internal_last_event_frame = frame;

	  /* If we didn't decide to make a switch-frame event, go ahead
	     and build a real event from the queue entry.  */
	  if (NILP (obj))
	    {
	      obj = make_lispy_event (&event->ie);

#ifdef HAVE_EXT_MENU_BAR
	      /* If this was a menu selection, then set the flag to inhibit
		 writing to last_nonmenu_event.  Don't do this if the event
		 we're returning is (menu-bar), though; that indicates the
		 beginning of the menu sequence, and we might as well leave
		 that as the `event with parameters' for this selection.  */
	      if (used_mouse_menu
		  && !EQ (event->ie.frame_or_window, event->ie.arg)
		  && (event->kind == MENU_BAR_EVENT
		      || event->kind == TAB_BAR_EVENT
		      || event->kind == TOOL_BAR_EVENT))
		*used_mouse_menu = true;
#endif
#ifdef HAVE_NS
	      /* Certain system events are non-key events.  */
	      if (used_mouse_menu
                  && event->kind == NS_NONKEY_EVENT)
		*used_mouse_menu = true;
#endif

	      /* Wipe out this event, to catch bugs.  */
	      clear_event (&event->ie);
	      kbd_fetch_ptr = next_kbd_event (event);
	    }
	}
      }
    }
  /* Try generating a mouse motion event.  */
  else if (some_mouse_moved ())
    {
      struct frame *f = some_mouse_moved ();
      Lisp_Object bar_window;
      enum scroll_bar_part part;
      Lisp_Object x, y;
      Time t;

      *kbp = current_kboard;
      /* Note that this uses F to determine which terminal to look at.
	 If there is no valid info, it does not store anything
	 so x remains nil.  */
      x = Qnil;

      /* XXX Can f or mouse_position_hook be NULL here?  */
      if (f && FRAME_TERMINAL (f)->mouse_position_hook)
        (*FRAME_TERMINAL (f)->mouse_position_hook) (&f, 0, &bar_window,
                                                    &part, &x, &y, &t);

      obj = Qnil;

      /* Decide if we should generate a switch-frame event.  Don't
	 generate switch-frame events for motion outside of all Emacs
	 frames.  */
      if (!NILP (x) && f)
	{
	  Lisp_Object frame;

	  frame = FRAME_FOCUS_FRAME (f);
	  if (NILP (frame))
	    XSETFRAME (frame, f);

	  if (!EQ (frame, internal_last_event_frame)
	      && !EQ (frame, selected_frame))
	    obj = make_lispy_switch_frame (frame);
	  internal_last_event_frame = frame;
	}

      /* If we didn't decide to make a switch-frame event, go ahead and
	 return a mouse-motion event.  */
      if (!NILP (x) && NILP (obj))
	obj = make_lispy_movement (f, bar_window, part, x, y, t);
    }
  else
    /* We were promised by the above while loop that there was
       something for us to read!  */
    emacs_abort ();

  input_pending = readable_events (0);

  Vlast_event_frame = internal_last_event_frame;

  return (obj);
}

/* Process any non-user-visible events (currently X selection events),
   without reading any user-visible events.  */

static void
process_special_events (void)
{
  for (union buffered_input_event *event = kbd_fetch_ptr;
       event != kbd_store_ptr; event = next_kbd_event (event))
    {
      /* If we find a stored X selection request, handle it now.  */
      if (event->kind == SELECTION_REQUEST_EVENT
	  || event->kind == SELECTION_CLEAR_EVENT)
	{
#ifdef HAVE_X11

	  /* Remove the event from the fifo buffer before processing;
	     otherwise swallow_events called recursively could see it
	     and process it again.  To do this, we move the events
	     between kbd_fetch_ptr and EVENT one slot to the right,
	     cyclically.  */

	  struct selection_input_event copy = event->sie;
	  int moved_events;

	  if (event < kbd_fetch_ptr)
	    {
	      memmove (kbd_buffer + 1, kbd_buffer,
		       (event - kbd_buffer) * sizeof *kbd_buffer);
	      kbd_buffer[0] = kbd_buffer[KBD_BUFFER_SIZE - 1];
	      moved_events = kbd_buffer + KBD_BUFFER_SIZE - 1 - kbd_fetch_ptr;
	    }
	  else
	    moved_events = event - kbd_fetch_ptr;

	  memmove (kbd_fetch_ptr + 1, kbd_fetch_ptr,
		   moved_events * sizeof *kbd_fetch_ptr);
	  kbd_fetch_ptr = next_kbd_event (kbd_fetch_ptr);
	  input_pending = readable_events (0);
	  x_handle_selection_event (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  emacs_abort ();
#endif
	}
    }
}

/* Process any events that are not user-visible, run timer events that
   are ripe, and return, without reading any user-visible events.  */

void
swallow_events (bool do_display)
{
  unsigned old_timers_run;

  process_special_events ();

  old_timers_run = timers_run;
  get_input_pending (READABLE_EVENTS_DO_TIMERS_NOW);

  if (!input_pending && timers_run != old_timers_run && do_display)
    redisplay_preserve_echo_area (7);
}

/* Record the start of when Emacs is idle,
   for the sake of running idle-time timers.  */

static void
timer_start_idle (void)
{
  /* If we are already in the idle state, do nothing.  */
  if (timespec_valid_p (timer_idleness_start_time))
    return;

  timer_idleness_start_time = current_timespec ();
  timer_last_idleness_start_time = timer_idleness_start_time;

  /* Mark all idle-time timers as once again candidates for running.  */
  call0 (intern ("internal-timer-start-idle"));
}

/* Record that Emacs is no longer idle, so stop running idle-time timers.  */

static void
timer_stop_idle (void)
{
  timer_idleness_start_time = invalid_timespec ();
}

/* Resume idle timer from last idle start time.  */

static void
timer_resume_idle (void)
{
  if (timespec_valid_p (timer_idleness_start_time))
    return;

  timer_idleness_start_time = timer_last_idleness_start_time;
}

/* List of elisp functions to call, delayed because they were generated in
   a context where Elisp could not be safely run (e.g. redisplay, signal,
   ...).  Each element has the form (FUN . ARGS).  */
Lisp_Object pending_funcalls;

/* Return true if TIMER is a valid timer, placing its value into *RESULT.  */
static bool
decode_timer (Lisp_Object timer, struct timespec *result)
{
  Lisp_Object *vec;

  if (! (VECTORP (timer) && ASIZE (timer) == 9))
    return false;
  vec = XVECTOR (timer)->contents;
  if (! NILP (vec[0]))
    return false;
  if (! FIXNUMP (vec[2]))
    return false;
  return list4_to_timespec (vec[1], vec[2], vec[3], vec[8], result);
}


/* Check whether a timer has fired.  To prevent larger problems we simply
   disregard elements that are not proper timers.  Do not make a circular
   timer list for the time being.

   Returns the time to wait until the next timer fires.  If a
   timer is triggering now, return zero.
   If no timer is active, return -1.

   If a timer is ripe, we run it, with quitting turned off.
   In that case we return 0 to indicate that a new timer_check_2 call
   should be done.  */

static struct timespec
timer_check_2 (Lisp_Object timers, Lisp_Object idle_timers)
{
  struct timespec nexttime;
  struct timespec now;
  struct timespec idleness_now;
  Lisp_Object chosen_timer;

  nexttime = invalid_timespec ();

  chosen_timer = Qnil;

  /* First run the code that was delayed.  */
  while (CONSP (pending_funcalls))
    {
      Lisp_Object funcall = XCAR (pending_funcalls);
      pending_funcalls = XCDR (pending_funcalls);
      safe_call2 (Qapply, XCAR (funcall), XCDR (funcall));
    }

  if (CONSP (timers) || CONSP (idle_timers))
    {
      now = current_timespec ();
      idleness_now = (timespec_valid_p (timer_idleness_start_time)
		      ? timespec_sub (now, timer_idleness_start_time)
		      : make_timespec (0, 0));
    }

  while (CONSP (timers) || CONSP (idle_timers))
    {
      Lisp_Object timer = Qnil, idle_timer = Qnil;
      struct timespec timer_time, idle_timer_time;
      struct timespec difference;
      struct timespec timer_difference = invalid_timespec ();
      struct timespec idle_timer_difference = invalid_timespec ();
      bool ripe, timer_ripe = 0, idle_timer_ripe = 0;

      /* Set TIMER and TIMER_DIFFERENCE
	 based on the next ordinary timer.
	 TIMER_DIFFERENCE is the distance in time from NOW to when
	 this timer becomes ripe.
         Skip past invalid timers and timers already handled.  */
      if (CONSP (timers))
	{
	  timer = XCAR (timers);
	  if (! decode_timer (timer, &timer_time))
	    {
	      timers = XCDR (timers);
	      continue;
	    }

	  timer_ripe = timespec_cmp (timer_time, now) <= 0;
	  timer_difference = (timer_ripe
			      ? timespec_sub (now, timer_time)
			      : timespec_sub (timer_time, now));
	}

      /* Likewise for IDLE_TIMER and IDLE_TIMER_DIFFERENCE
	 based on the next idle timer.  */
      if (CONSP (idle_timers))
	{
	  idle_timer = XCAR (idle_timers);
	  if (! decode_timer (idle_timer, &idle_timer_time))
	    {
	      idle_timers = XCDR (idle_timers);
	      continue;
	    }

	  idle_timer_ripe = timespec_cmp (idle_timer_time, idleness_now) <= 0;
	  idle_timer_difference
	    = (idle_timer_ripe
	       ? timespec_sub (idleness_now, idle_timer_time)
	       : timespec_sub (idle_timer_time, idleness_now));
	}

      /* Decide which timer is the next timer,
	 and set CHOSEN_TIMER, DIFFERENCE, and RIPE accordingly.
	 Also step down the list where we found that timer.  */

      if (timespec_valid_p (timer_difference)
	  && (! timespec_valid_p (idle_timer_difference)
	      || idle_timer_ripe < timer_ripe
	      || (idle_timer_ripe == timer_ripe
		  && ((timer_ripe
		       ? timespec_cmp (idle_timer_difference,
				       timer_difference)
		       : timespec_cmp (timer_difference,
				       idle_timer_difference))
		      < 0))))
	{
	  chosen_timer = timer;
	  timers = XCDR (timers);
	  difference = timer_difference;
	  ripe = timer_ripe;
	}
      else
	{
	  chosen_timer = idle_timer;
	  idle_timers = XCDR (idle_timers);
	  difference = idle_timer_difference;
	  ripe = idle_timer_ripe;
	}

      /* If timer is ripe, run it if it hasn't been run.  */
      if (ripe)
	{
	  if (NILP (AREF (chosen_timer, 0)))
	    {
	      ptrdiff_t count = SPECPDL_INDEX ();
	      Lisp_Object old_deactivate_mark = Vdeactivate_mark;

	      /* Mark the timer as triggered to prevent problems if the lisp
		 code fails to reschedule it right.  */
	      ASET (chosen_timer, 0, Qt);

	      specbind (Qinhibit_quit, Qt);

	      call1 (Qtimer_event_handler, chosen_timer);
	      Vdeactivate_mark = old_deactivate_mark;
	      timers_run++;
	      unbind_to (count, Qnil);

	      /* Since we have handled the event,
		 we don't need to tell the caller to wake up and do it.  */
              /* But the caller must still wait for the next timer, so
                 return 0 to indicate that.  */
	    }

	  nexttime = make_timespec (0, 0);
          break;
	}
      else
	/* When we encounter a timer that is still waiting,
	   return the amount of time to wait before it is ripe.  */
	{
	  return difference;
	}
    }

  /* No timers are pending in the future.  */
  /* Return 0 if we generated an event, and -1 if not.  */
  return nexttime;
}


/* Check whether a timer has fired.  To prevent larger problems we simply
   disregard elements that are not proper timers.  Do not make a circular
   timer list for the time being.

   Returns the time to wait until the next timer fires.
   If no timer is active, return an invalid value.

   As long as any timer is ripe, we run it.  */

struct timespec
timer_check (void)
{
  struct timespec nexttime;
  Lisp_Object timers, idle_timers;

  Lisp_Object tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  /* We use copies of the timers' lists to allow a timer to add itself
     again, without locking up Emacs if the newly added timer is
     already ripe when added.  */

  /* Always consider the ordinary timers.  */
  timers = Fcopy_sequence (Vtimer_list);
  /* Consider the idle timers only if Emacs is idle.  */
  if (timespec_valid_p (timer_idleness_start_time))
    idle_timers = Fcopy_sequence (Vtimer_idle_list);
  else
    idle_timers = Qnil;

  Vinhibit_quit = tem;

  do
    {
      nexttime = timer_check_2 (timers, idle_timers);
    }
  while (nexttime.tv_sec == 0 && nexttime.tv_nsec == 0);

  return nexttime;
}

DEFUN ("current-idle-time", Fcurrent_idle_time, Scurrent_idle_time, 0, 0, 0,
       doc: /* Return the current length of Emacs idleness, or nil.
The value when Emacs is idle is a Lisp timestamp in the style of
`current-time'.

The value when Emacs is not idle is nil.

PSEC is a multiple of the system clock resolution.  */)
  (void)
{
  if (timespec_valid_p (timer_idleness_start_time))
    return make_lisp_time (timespec_sub (current_timespec (),
					 timer_idleness_start_time));

  return Qnil;
}

/* Caches for modify_event_symbol.  */
static Lisp_Object accent_key_syms;
static Lisp_Object func_key_syms;
static Lisp_Object mouse_syms;
static Lisp_Object wheel_syms;
static Lisp_Object drag_n_drop_syms;

/* This is a list of keysym codes for special "accent" characters.
   It parallels lispy_accent_keys.  */

static const int lispy_accent_codes[] =
{
#ifdef XK_dead_circumflex
  XK_dead_circumflex,
#else
  0,
#endif
#ifdef XK_dead_grave
  XK_dead_grave,
#else
  0,
#endif
#ifdef XK_dead_tilde
  XK_dead_tilde,
#else
  0,
#endif
#ifdef XK_dead_diaeresis
  XK_dead_diaeresis,
#else
  0,
#endif
#ifdef XK_dead_macron
  XK_dead_macron,
#else
  0,
#endif
#ifdef XK_dead_degree
  XK_dead_degree,
#else
  0,
#endif
#ifdef XK_dead_acute
  XK_dead_acute,
#else
  0,
#endif
#ifdef XK_dead_cedilla
  XK_dead_cedilla,
#else
  0,
#endif
#ifdef XK_dead_breve
  XK_dead_breve,
#else
  0,
#endif
#ifdef XK_dead_ogonek
  XK_dead_ogonek,
#else
  0,
#endif
#ifdef XK_dead_caron
  XK_dead_caron,
#else
  0,
#endif
#ifdef XK_dead_doubleacute
  XK_dead_doubleacute,
#else
  0,
#endif
#ifdef XK_dead_abovedot
  XK_dead_abovedot,
#else
  0,
#endif
#ifdef XK_dead_abovering
  XK_dead_abovering,
#else
  0,
#endif
#ifdef XK_dead_iota
  XK_dead_iota,
#else
  0,
#endif
#ifdef XK_dead_belowdot
  XK_dead_belowdot,
#else
  0,
#endif
#ifdef XK_dead_voiced_sound
  XK_dead_voiced_sound,
#else
  0,
#endif
#ifdef XK_dead_semivoiced_sound
  XK_dead_semivoiced_sound,
#else
  0,
#endif
#ifdef XK_dead_hook
  XK_dead_hook,
#else
  0,
#endif
#ifdef XK_dead_horn
  XK_dead_horn,
#else
  0,
#endif
};

/* This is a list of Lisp names for special "accent" characters.
   It parallels lispy_accent_codes.  */

static const char *const lispy_accent_keys[] =
{
  "dead-circumflex",
  "dead-grave",
  "dead-tilde",
  "dead-diaeresis",
  "dead-macron",
  "dead-degree",
  "dead-acute",
  "dead-cedilla",
  "dead-breve",
  "dead-ogonek",
  "dead-caron",
  "dead-doubleacute",
  "dead-abovedot",
  "dead-abovering",
  "dead-iota",
  "dead-belowdot",
  "dead-voiced-sound",
  "dead-semivoiced-sound",
  "dead-hook",
  "dead-horn",
};

#ifdef HAVE_NTGUI
#define FUNCTION_KEY_OFFSET 0x0

const char *const lispy_function_keys[] =
  {
    0,                /* 0                      */

    0,                /* VK_LBUTTON        0x01 */
    0,                /* VK_RBUTTON        0x02 */
    "cancel",         /* VK_CANCEL         0x03 */
    0,                /* VK_MBUTTON        0x04 */

    0, 0, 0,          /*    0x05 .. 0x07        */

    "backspace",      /* VK_BACK           0x08 */
    "tab",            /* VK_TAB            0x09 */

    0, 0,             /*    0x0A .. 0x0B        */

    "clear",          /* VK_CLEAR          0x0C */
    "return",         /* VK_RETURN         0x0D */

    0, 0,             /*    0x0E .. 0x0F        */

    0,                /* VK_SHIFT          0x10 */
    0,                /* VK_CONTROL        0x11 */
    0,                /* VK_MENU           0x12 */
    "pause",          /* VK_PAUSE          0x13 */
    "capslock",       /* VK_CAPITAL        0x14 */
    "kana",           /* VK_KANA/VK_HANGUL 0x15 */
    0,                /*    0x16                */
    "junja",          /* VK_JUNJA          0x17 */
    "final",          /* VK_FINAL          0x18 */
    "kanji",          /* VK_KANJI/VK_HANJA 0x19 */
    0,                /*    0x1A                */
    "escape",         /* VK_ESCAPE         0x1B */
    "convert",        /* VK_CONVERT        0x1C */
    "non-convert",    /* VK_NONCONVERT     0x1D */
    "accept",         /* VK_ACCEPT         0x1E */
    "mode-change",    /* VK_MODECHANGE     0x1F */
    0,                /* VK_SPACE          0x20 */
    "prior",          /* VK_PRIOR          0x21 */
    "next",           /* VK_NEXT           0x22 */
    "end",            /* VK_END            0x23 */
    "home",           /* VK_HOME           0x24 */
    "left",           /* VK_LEFT           0x25 */
    "up",             /* VK_UP             0x26 */
    "right",          /* VK_RIGHT          0x27 */
    "down",           /* VK_DOWN           0x28 */
    "select",         /* VK_SELECT         0x29 */
    "print",          /* VK_PRINT          0x2A */
    "execute",        /* VK_EXECUTE        0x2B */
    "snapshot",       /* VK_SNAPSHOT       0x2C */
    "insert",         /* VK_INSERT         0x2D */
    "delete",         /* VK_DELETE         0x2E */
    "help",           /* VK_HELP           0x2F */

    /* VK_0 thru VK_9 are the same as ASCII '0' thru '9' (0x30 - 0x39) */

    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

    0, 0, 0, 0, 0, 0, 0, /* 0x3A .. 0x40       */

    /* VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (0x41 - 0x5A) */

    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,

    "lwindow",       /* VK_LWIN           0x5B */
    "rwindow",       /* VK_RWIN           0x5C */
    "apps",          /* VK_APPS           0x5D */
    0,               /*    0x5E                */
    "sleep",
    "kp-0",          /* VK_NUMPAD0        0x60 */
    "kp-1",          /* VK_NUMPAD1        0x61 */
    "kp-2",          /* VK_NUMPAD2        0x62 */
    "kp-3",          /* VK_NUMPAD3        0x63 */
    "kp-4",          /* VK_NUMPAD4        0x64 */
    "kp-5",          /* VK_NUMPAD5        0x65 */
    "kp-6",          /* VK_NUMPAD6        0x66 */
    "kp-7",          /* VK_NUMPAD7        0x67 */
    "kp-8",          /* VK_NUMPAD8        0x68 */
    "kp-9",          /* VK_NUMPAD9        0x69 */
    "kp-multiply",   /* VK_MULTIPLY       0x6A */
    "kp-add",        /* VK_ADD            0x6B */
    "kp-separator",  /* VK_SEPARATOR      0x6C */
    "kp-subtract",   /* VK_SUBTRACT       0x6D */
    "kp-decimal",    /* VK_DECIMAL        0x6E */
    "kp-divide",     /* VK_DIVIDE         0x6F */
    "f1",            /* VK_F1             0x70 */
    "f2",            /* VK_F2             0x71 */
    "f3",            /* VK_F3             0x72 */
    "f4",            /* VK_F4             0x73 */
    "f5",            /* VK_F5             0x74 */
    "f6",            /* VK_F6             0x75 */
    "f7",            /* VK_F7             0x76 */
    "f8",            /* VK_F8             0x77 */
    "f9",            /* VK_F9             0x78 */
    "f10",           /* VK_F10            0x79 */
    "f11",           /* VK_F11            0x7A */
    "f12",           /* VK_F12            0x7B */
    "f13",           /* VK_F13            0x7C */
    "f14",           /* VK_F14            0x7D */
    "f15",           /* VK_F15            0x7E */
    "f16",           /* VK_F16            0x7F */
    "f17",           /* VK_F17            0x80 */
    "f18",           /* VK_F18            0x81 */
    "f19",           /* VK_F19            0x82 */
    "f20",           /* VK_F20            0x83 */
    "f21",           /* VK_F21            0x84 */
    "f22",           /* VK_F22            0x85 */
    "f23",           /* VK_F23            0x86 */
    "f24",           /* VK_F24            0x87 */

    0, 0, 0, 0,      /*    0x88 .. 0x8B        */
    0, 0, 0, 0,      /*    0x8C .. 0x8F        */

    "kp-numlock",    /* VK_NUMLOCK        0x90 */
    "scroll",        /* VK_SCROLL         0x91 */
    /* Not sure where the following block comes from.
       Windows headers have NEC and Fujitsu specific keys in
       this block, but nothing generic.  */
    "kp-space",	     /* VK_NUMPAD_CLEAR   0x92 */
    "kp-enter",	     /* VK_NUMPAD_ENTER   0x93 */
    "kp-prior",	     /* VK_NUMPAD_PRIOR   0x94 */
    "kp-next",	     /* VK_NUMPAD_NEXT    0x95 */
    "kp-end",	     /* VK_NUMPAD_END     0x96 */
    "kp-home",	     /* VK_NUMPAD_HOME    0x97 */
    "kp-left",	     /* VK_NUMPAD_LEFT    0x98 */
    "kp-up",	     /* VK_NUMPAD_UP      0x99 */
    "kp-right",	     /* VK_NUMPAD_RIGHT   0x9A */
    "kp-down",	     /* VK_NUMPAD_DOWN    0x9B */
    "kp-insert",     /* VK_NUMPAD_INSERT  0x9C */
    "kp-delete",     /* VK_NUMPAD_DELETE  0x9D */

    0, 0,	     /*    0x9E .. 0x9F        */

    /*
     * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
     * Used only as parameters to GetAsyncKeyState and GetKeyState.
     * No other API or message will distinguish left and right keys this way.
     * 0xA0 .. 0xA5
     */
    0, 0, 0, 0, 0, 0,

    /* Multimedia keys. These are handled as WM_APPCOMMAND, which allows us
       to enable them selectively, and gives access to a few more functions.
       See lispy_multimedia_keys below.  */
    0, 0, 0, 0, 0, 0, 0, /* 0xA6 .. 0xAC        Browser */
    0, 0, 0,             /* 0xAD .. 0xAF         Volume */
    0, 0, 0, 0,          /* 0xB0 .. 0xB3          Media */
    0, 0, 0, 0,          /* 0xB4 .. 0xB7           Apps */

    /* 0xB8 .. 0xC0 "OEM" keys - all seem to be punctuation.  */
    0, 0, 0, 0, 0, 0, 0, 0, 0,

    /* 0xC1 - 0xDA unallocated, 0xDB-0xDF more OEM keys */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

    0,               /* 0xE0                   */
    "ax",            /* VK_OEM_AX         0xE1 */
    0,               /* VK_OEM_102        0xE2 */
    "ico-help",      /* VK_ICO_HELP       0xE3 */
    "ico-00",        /* VK_ICO_00         0xE4 */
    0,               /* VK_PROCESSKEY     0xE5 - used by IME */
    "ico-clear",     /* VK_ICO_CLEAR      0xE6 */
    0,               /* VK_PACKET         0xE7  - used to pass Unicode chars */
    0,               /*                   0xE8 */
    "reset",         /* VK_OEM_RESET      0xE9 */
    "jump",          /* VK_OEM_JUMP       0xEA */
    "oem-pa1",       /* VK_OEM_PA1        0xEB */
    "oem-pa2",       /* VK_OEM_PA2        0xEC */
    "oem-pa3",       /* VK_OEM_PA3        0xED */
    "wsctrl",        /* VK_OEM_WSCTRL     0xEE */
    "cusel",         /* VK_OEM_CUSEL      0xEF */
    "oem-attn",      /* VK_OEM_ATTN       0xF0 */
    "finish",        /* VK_OEM_FINISH     0xF1 */
    "copy",          /* VK_OEM_COPY       0xF2 */
    "auto",          /* VK_OEM_AUTO       0xF3 */
    "enlw",          /* VK_OEM_ENLW       0xF4 */
    "backtab",       /* VK_OEM_BACKTAB    0xF5 */
    "attn",          /* VK_ATTN           0xF6 */
    "crsel",         /* VK_CRSEL          0xF7 */
    "exsel",         /* VK_EXSEL          0xF8 */
    "ereof",         /* VK_EREOF          0xF9 */
    "play",          /* VK_PLAY           0xFA */
    "zoom",          /* VK_ZOOM           0xFB */
    "noname",        /* VK_NONAME         0xFC */
    "pa1",           /* VK_PA1            0xFD */
    "oem_clear",     /* VK_OEM_CLEAR      0xFE */
    0 /* 0xFF */
  };

/* Some of these duplicate the "Media keys" on newer keyboards,
   but they are delivered to the application in a different way.  */
static const char *const lispy_multimedia_keys[] =
  {
    0,
    "browser-back",
    "browser-forward",
    "browser-refresh",
    "browser-stop",
    "browser-search",
    "browser-favorites",
    "browser-home",
    "volume-mute",
    "volume-down",
    "volume-up",
    "media-next",
    "media-previous",
    "media-stop",
    "media-play-pause",
    "mail",
    "media-select",
    "app-1",
    "app-2",
    "bass-down",
    "bass-boost",
    "bass-up",
    "treble-down",
    "treble-up",
    "mic-volume-mute",
    "mic-volume-down",
    "mic-volume-up",
    "help",
    "find",
    "new",
    "open",
    "close",
    "save",
    "print",
    "undo",
    "redo",
    "copy",
    "cut",
    "paste",
    "mail-reply",
    "mail-forward",
    "mail-send",
    "spell-check",
    "toggle-dictate-command",
    "mic-toggle",
    "correction-list",
    "media-play",
    "media-pause",
    "media-record",
    "media-fast-forward",
    "media-rewind",
    "media-channel-up",
    "media-channel-down"
  };

#else /* not HAVE_NTGUI */

/* This should be dealt with in XTread_socket now, and that doesn't
   depend on the client system having the Kana syms defined.  See also
   the XK_kana_A case below.  */
#if 0
#ifdef XK_kana_A
static const char *const lispy_kana_keys[] =
  {
    /* X Keysym value */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x400 .. 0x40f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x410 .. 0x41f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x420 .. 0x42f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x430 .. 0x43f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x440 .. 0x44f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x450 .. 0x45f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x460 .. 0x46f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,"overline",0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x480 .. 0x48f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x490 .. 0x49f */
    0, "kana-fullstop", "kana-openingbracket", "kana-closingbracket",
    "kana-comma", "kana-conjunctive", "kana-WO", "kana-a",
    "kana-i", "kana-u", "kana-e", "kana-o",
    "kana-ya", "kana-yu", "kana-yo", "kana-tsu",
    "prolongedsound", "kana-A", "kana-I", "kana-U",
    "kana-E", "kana-O", "kana-KA", "kana-KI",
    "kana-KU", "kana-KE", "kana-KO", "kana-SA",
    "kana-SHI", "kana-SU", "kana-SE", "kana-SO",
    "kana-TA", "kana-CHI", "kana-TSU", "kana-TE",
    "kana-TO", "kana-NA", "kana-NI", "kana-NU",
    "kana-NE", "kana-NO", "kana-HA", "kana-HI",
    "kana-FU", "kana-HE", "kana-HO", "kana-MA",
    "kana-MI", "kana-MU", "kana-ME", "kana-MO",
    "kana-YA", "kana-YU", "kana-YO", "kana-RA",
    "kana-RI", "kana-RU", "kana-RE", "kana-RO",
    "kana-WA", "kana-N", "voicedsound", "semivoicedsound",
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x4e0 .. 0x4ef */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x4f0 .. 0x4ff */
  };
#endif /* XK_kana_A */
#endif /* 0 */

#define FUNCTION_KEY_OFFSET 0xff00

/* You'll notice that this table is arranged to be conveniently
   indexed by X Windows keysym values.  */
static const char *const lispy_function_keys[] =
  {
    /* X Keysym value */

    0, 0, 0, 0, 0, 0, 0, 0,			      /* 0xff00...0f */
    "backspace", "tab", "linefeed", "clear",
    0, "return", 0, 0,
    0, 0, 0, "pause",				      /* 0xff10...1f */
    0, 0, 0, 0, 0, 0, 0, "escape",
    0, 0, 0, 0,
    0, "kanji", "muhenkan", "henkan",		      /* 0xff20...2f */
    "romaji", "hiragana", "katakana", "hiragana-katakana",
    "zenkaku", "hankaku", "zenkaku-hankaku", "touroku",
    "massyo", "kana-lock", "kana-shift", "eisu-shift",
    "eisu-toggle",				      /* 0xff30...3f */
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   /* 0xff40...4f */

    "home", "left", "up", "right", /* 0xff50 */	/* IsCursorKey */
    "down", "prior", "next", "end",
    "begin", 0, 0, 0, 0, 0, 0, 0,
    "select",			/* 0xff60 */	/* IsMiscFunctionKey */
    "print",
    "execute",
    "insert",
    0,		/* 0xff64 */
    "undo",
    "redo",
    "menu",
    "find",
    "cancel",
    "help",
    "break",			/* 0xff6b */

    0, 0, 0, 0,
    0, 0, 0, 0, "backtab", 0, 0, 0,		/* 0xff70...  */
    0, 0, 0, 0, 0, 0, 0, "kp-numlock",		/* 0xff78...  */
    "kp-space",			/* 0xff80 */	/* IsKeypadKey */
    0, 0, 0, 0, 0, 0, 0, 0,
    "kp-tab",			/* 0xff89 */
    0, 0, 0,
    "kp-enter",			/* 0xff8d */
    0, 0, 0,
    "kp-f1",			/* 0xff91 */
    "kp-f2",
    "kp-f3",
    "kp-f4",
    "kp-home",			/* 0xff95 */
    "kp-left",
    "kp-up",
    "kp-right",
    "kp-down",
    "kp-prior",			/* kp-page-up */
    "kp-next",			/* kp-page-down */
    "kp-end",
    "kp-begin",
    "kp-insert",
    "kp-delete",
    0,				/* 0xffa0 */
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    "kp-multiply",		/* 0xffaa */
    "kp-add",
    "kp-separator",
    "kp-subtract",
    "kp-decimal",
    "kp-divide",		/* 0xffaf */
    "kp-0",			/* 0xffb0 */
    "kp-1",	"kp-2",	"kp-3",	"kp-4",	"kp-5",	"kp-6",	"kp-7",	"kp-8",	"kp-9",
    0,		/* 0xffba */
    0, 0,
    "kp-equal",			/* 0xffbd */
    "f1",			/* 0xffbe */	/* IsFunctionKey */
    "f2",
    "f3", "f4", "f5", "f6", "f7", "f8",	"f9", "f10", /* 0xffc0 */
    "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18",
    "f19", "f20", "f21", "f22", "f23", "f24", "f25", "f26", /* 0xffd0 */
    "f27", "f28", "f29", "f30", "f31", "f32", "f33", "f34",
    "f35", 0, 0, 0, 0, 0, 0, 0,	/* 0xffe0 */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,     /* 0xfff0 */
    0, 0, 0, 0, 0, 0, 0, "delete"
  };

/* ISO 9995 Function and Modifier Keys; the first byte is 0xFE.  */
#define ISO_FUNCTION_KEY_OFFSET 0xfe00

static const char *const iso_lispy_function_keys[] =
  {
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe00 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe08 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe10 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe18 */
    "iso-lefttab",		/* 0xfe20 */
    "iso-move-line-up", "iso-move-line-down",
    "iso-partial-line-up", "iso-partial-line-down",
    "iso-partial-space-left", "iso-partial-space-right",
    "iso-set-margin-left", "iso-set-margin-right", /* 0xffe27, 28 */
    "iso-release-margin-left", "iso-release-margin-right",
    "iso-release-both-margins",
    "iso-fast-cursor-left", "iso-fast-cursor-right",
    "iso-fast-cursor-up", "iso-fast-cursor-down",
    "iso-continuous-underline", "iso-discontinuous-underline", /* 0xfe30, 31 */
    "iso-emphasize", "iso-center-object", "iso-enter", /* ... 0xfe34 */
  };

#endif /* not HAVE_NTGUI */

static Lisp_Object Vlispy_mouse_stem;

static const char *const lispy_wheel_names[] =
{
  "wheel-up", "wheel-down", "wheel-left", "wheel-right"
};

/* drag-n-drop events are generated when a set of selected files are
   dragged from another application and dropped onto an Emacs window.  */
static const char *const lispy_drag_n_drop_names[] =
{
  "drag-n-drop"
};

/* An array of symbol indexes of scroll bar parts, indexed by an enum
   scroll_bar_part value.  Note that Qnil corresponds to
   scroll_bar_nowhere and should not appear in Lisp events.  */
static short const scroll_bar_parts[] = {
  SYMBOL_INDEX (Qnil), SYMBOL_INDEX (Qabove_handle), SYMBOL_INDEX (Qhandle),
  SYMBOL_INDEX (Qbelow_handle), SYMBOL_INDEX (Qup), SYMBOL_INDEX (Qdown),
  SYMBOL_INDEX (Qtop), SYMBOL_INDEX (Qbottom), SYMBOL_INDEX (Qend_scroll),
  SYMBOL_INDEX (Qratio), SYMBOL_INDEX (Qbefore_handle),
  SYMBOL_INDEX (Qhorizontal_handle), SYMBOL_INDEX (Qafter_handle),
  SYMBOL_INDEX (Qleft), SYMBOL_INDEX (Qright), SYMBOL_INDEX (Qleftmost),
  SYMBOL_INDEX (Qrightmost), SYMBOL_INDEX (Qend_scroll), SYMBOL_INDEX (Qratio)
};

#ifdef HAVE_WINDOW_SYSTEM
/* An array of symbol indexes of internal border parts, indexed by an enum
   internal_border_part value.  Note that Qnil corresponds to
   internal_border_part_none and should not appear in Lisp events.  */
static short const internal_border_parts[] = {
  SYMBOL_INDEX (Qnil), SYMBOL_INDEX (Qleft_edge),
  SYMBOL_INDEX (Qtop_left_corner), SYMBOL_INDEX (Qtop_edge),
  SYMBOL_INDEX (Qtop_right_corner), SYMBOL_INDEX (Qright_edge),
  SYMBOL_INDEX (Qbottom_right_corner), SYMBOL_INDEX (Qbottom_edge),
  SYMBOL_INDEX (Qbottom_left_corner)
};
#endif

/* A vector, indexed by button number, giving the down-going location
   of currently depressed buttons, both scroll bar and non-scroll bar.

   The elements have the form
     (BUTTON-NUMBER MODIFIER-MASK . REST)
   where REST is the cdr of a position as it would be reported in the event.

   The make_lispy_event function stores positions here to tell the
   difference between click and drag events, and to store the starting
   location to be included in drag events.  */

static Lisp_Object button_down_location;

/* Information about the most recent up-going button event:  Which
   button, what location, and what time.  */

static int last_mouse_button;
static int last_mouse_x;
static int last_mouse_y;
static Time button_down_time;

/* The number of clicks in this multiple-click.  */

static int double_click_count;

/* X and Y are frame-relative coordinates for a click or wheel event.
   Return a Lisp-style event list.  */

static Lisp_Object
make_lispy_position (struct frame *f, Lisp_Object x, Lisp_Object y,
		     Time t)
{
  enum window_part part;
  Lisp_Object posn = Qnil;
  Lisp_Object extra_info = Qnil;
  /* Coordinate pixel positions to return.  */
  int xret = 0, yret = 0;
  /* The window or frame under frame pixel coordinates (x,y)  */
  Lisp_Object window_or_frame = f
    ? window_from_coordinates (f, XFIXNUM (x), XFIXNUM (y), &part, 0, 0)
    : Qnil;

  if (WINDOWP (window_or_frame))
    {
      /* It's a click in window WINDOW at frame coordinates (X,Y)  */
      struct window *w = XWINDOW (window_or_frame);
      Lisp_Object string_info = Qnil;
      ptrdiff_t textpos = 0;
      int col = -1, row = -1;
      int dx  = -1, dy  = -1;
      int width = -1, height = -1;
      Lisp_Object object = Qnil;

      /* Pixel coordinates relative to the window corner.  */
      int wx = XFIXNUM (x) - WINDOW_LEFT_EDGE_X (w);
      int wy = XFIXNUM (y) - WINDOW_TOP_EDGE_Y (w);

      /* For text area clicks, return X, Y relative to the corner of
	 this text area.  Note that dX, dY etc are set below, by
	 buffer_posn_from_coords.  */
      if (part == ON_TEXT)
	{
	  xret = XFIXNUM (x) - window_box_left (w, TEXT_AREA);
	  yret = wy - WINDOW_TAB_LINE_HEIGHT (w) - WINDOW_HEADER_LINE_HEIGHT (w);
	}
      /* For mode line and header line clicks, return X, Y relative to
	 the left window edge.  Use mode_line_string to look for a
	 string on the click position.  */
      else if (part == ON_MODE_LINE || part == ON_TAB_LINE
	       || part == ON_HEADER_LINE)
	{
	  Lisp_Object string;
	  ptrdiff_t charpos;

	  posn = (part == ON_MODE_LINE ? Qmode_line
		  : (part == ON_TAB_LINE ? Qtab_line
		     : Qheader_line));

	  /* Note that mode_line_string takes COL, ROW as pixels and
	     converts them to characters.  */
	  col = wx;
	  row = wy;
	  string = mode_line_string (w, part, &col, &row, &charpos,
				     &object, &dx, &dy, &width, &height);
	  if (STRINGP (string))
	    string_info = Fcons (string, make_fixnum (charpos));
	  textpos = -1;

	  xret = wx;
	  yret = wy;
	}
      /* For fringes and margins, Y is relative to the area's (and the
	 window's) top edge, while X is meaningless.  */
      else if (part == ON_LEFT_MARGIN || part == ON_RIGHT_MARGIN)
	{
	  Lisp_Object string;
	  ptrdiff_t charpos;

	  posn = (part == ON_LEFT_MARGIN) ? Qleft_margin : Qright_margin;
	  col = wx;
	  row = wy;
	  string = marginal_area_string (w, part, &col, &row, &charpos,
					 &object, &dx, &dy, &width, &height);
	  if (STRINGP (string))
	    string_info = Fcons (string, make_fixnum (charpos));
	  xret = wx;
	  yret = wy - WINDOW_TAB_LINE_HEIGHT (w) - WINDOW_HEADER_LINE_HEIGHT (w);
	}
      else if (part == ON_LEFT_FRINGE)
	{
	  posn = Qleft_fringe;
	  col = 0;
	  xret = wx;
	  dx = wx
	    - (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	       ? 0 : window_box_width (w, LEFT_MARGIN_AREA));
	  dy = yret = wy - WINDOW_TAB_LINE_HEIGHT (w) - WINDOW_HEADER_LINE_HEIGHT (w);
	}
      else if (part == ON_RIGHT_FRINGE)
	{
	  posn = Qright_fringe;
	  col = 0;
	  xret = wx;
	  dx = wx
	    - window_box_width (w, LEFT_MARGIN_AREA)
	    - window_box_width (w, TEXT_AREA)
	    - (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
	       ? window_box_width (w, RIGHT_MARGIN_AREA)
	       : 0);
	  dy = yret = wy - WINDOW_TAB_LINE_HEIGHT (w) - WINDOW_HEADER_LINE_HEIGHT (w);
	}
      else if (part == ON_VERTICAL_BORDER)
	{
	  posn = Qvertical_line;
	  width = 1;
	  dx = 0;
	  xret = wx;
	  dy = yret = wy;
	}
      else if (part == ON_VERTICAL_SCROLL_BAR)
	{
	  posn = Qvertical_scroll_bar;
	  width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);
	  dx = xret = wx;
	  dy = yret = wy;
	}
      else if (part == ON_HORIZONTAL_SCROLL_BAR)
	{
	  posn = Qhorizontal_scroll_bar;
	  width = WINDOW_SCROLL_BAR_AREA_HEIGHT (w);
	  dx = xret = wx;
	  dy = yret = wy;
	}
      else if (part == ON_RIGHT_DIVIDER)
	{
	  posn = Qright_divider;
	  width = WINDOW_RIGHT_DIVIDER_WIDTH (w);
	  dx = xret = wx;
	  dy = yret = wy;
	}
      else if (part == ON_BOTTOM_DIVIDER)
	{
	  posn = Qbottom_divider;
	  width = WINDOW_BOTTOM_DIVIDER_WIDTH (w);
	  dx = xret = wx;
	  dy = yret = wy;
	}

      /* For clicks in the text area, fringes, margins, or vertical
	 scroll bar, call buffer_posn_from_coords to extract TEXTPOS,
	 the buffer position nearest to the click.  */
      if (!textpos)
	{
	  Lisp_Object string2, object2 = Qnil;
	  struct display_pos p;
	  int dx2, dy2;
	  int width2, height2;
	  /* The pixel X coordinate passed to buffer_posn_from_coords
	     is the X coordinate relative to the text area for clicks
	     in text-area, right-margin/fringe and right-side vertical
	     scroll bar, zero otherwise.  */
	  int x2
	    = (part == ON_TEXT) ? xret
	    : (part == ON_RIGHT_FRINGE || part == ON_RIGHT_MARGIN
	       || (part == ON_VERTICAL_SCROLL_BAR
		   && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w)))
	    ? (XFIXNUM (x) - window_box_left (w, TEXT_AREA))
	    : 0;
	  int y2 = wy;

	  string2 = buffer_posn_from_coords (w, &x2, &y2, &p,
					     &object2, &dx2, &dy2,
					     &width2, &height2);
	  textpos = CHARPOS (p.pos);
	  if (col < 0) col = x2;
	  if (row < 0) row = y2;
	  if (dx < 0) dx = dx2;
	  if (dy < 0) dy = dy2;
	  if (width < 0) width = width2;
	  if (height < 0) height = height2;

	  if (NILP (posn))
	    {
	      posn = make_fixnum (textpos);
	      if (STRINGP (string2))
		string_info = Fcons (string2,
				     make_fixnum (CHARPOS (p.string_pos)));
	    }
	  if (NILP (object))
	    object = object2;
	}

#ifdef HAVE_WINDOW_SYSTEM
      if (IMAGEP (object))
	{
	  Lisp_Object image_map, hotspot;
	  if ((image_map = Fplist_get (XCDR (object), QCmap),
	       !NILP (image_map))
	      && (hotspot = find_hot_spot (image_map, dx, dy),
		  CONSP (hotspot))
	      && (hotspot = XCDR (hotspot), CONSP (hotspot)))
	    posn = XCAR (hotspot);
	}
#endif

      /* Object info.  */
      extra_info
	= list3 (object,
		 Fcons (make_fixnum (dx), make_fixnum (dy)),
		 Fcons (make_fixnum (width), make_fixnum (height)));

      /* String info.  */
      extra_info = Fcons (string_info,
			  Fcons (textpos < 0 ? Qnil : make_fixnum (textpos),
				 Fcons (Fcons (make_fixnum (col),
					       make_fixnum (row)),
					extra_info)));
    }

#ifdef HAVE_WINDOW_SYSTEM
  else if (f)
    {
      /* Return mouse pixel coordinates here.  */
      XSETFRAME (window_or_frame, f);
      xret = XFIXNUM (x);
      yret = XFIXNUM (y);

      if (FRAME_LIVE_P (f)
	  && FRAME_INTERNAL_BORDER_WIDTH (f) > 0
	  && !NILP (get_frame_param (f, Qdrag_internal_border)))
	{
	  enum internal_border_part part
	    = frame_internal_border_part (f, xret, yret);

	  posn = builtin_lisp_symbol (internal_border_parts[part]);
	}
    }
#endif

  else
    window_or_frame = Qnil;

  return Fcons (window_or_frame,
		Fcons (posn,
		       Fcons (Fcons (make_fixnum (xret),
				     make_fixnum (yret)),
			      Fcons (INT_TO_INTEGER (t),
				     extra_info))));
}

/* Return non-zero if F is a GUI frame that uses some toolkit-managed
   menu bar.  This really means that Emacs draws and manages the menu
   bar as part of its normal display, and therefore can compute its
   geometry.  */
static bool
toolkit_menubar_in_use (struct frame *f)
{
#ifdef HAVE_EXT_MENU_BAR
  return !(!FRAME_WINDOW_P (f));
#else
  return false;
#endif
}

/* Build the part of Lisp event which represents scroll bar state from
   EV.  TYPE is one of Qvertical_scroll_bar or Qhorizontal_scroll_bar.  */

static Lisp_Object
make_scroll_bar_position (struct input_event *ev, Lisp_Object type)
{
  return list5 (ev->frame_or_window, type, Fcons (ev->x, ev->y),
		INT_TO_INTEGER (ev->timestamp),
		builtin_lisp_symbol (scroll_bar_parts[ev->part]));
}

/* Given a struct input_event, build the lisp event which represents
   it.  If EVENT is 0, build a mouse movement event from the mouse
   movement buffer, which should have a movement event in it.

   Note that events must be passed to this function in the order they
   are received; this function stores the location of button presses
   in order to build drag events when the button is released.  */

static Lisp_Object
make_lispy_event (struct input_event *event)
{
  int i;

  switch (event->kind)
    {
#ifdef HAVE_WINDOW_SYSTEM
    case DELETE_WINDOW_EVENT:
      /* Make an event (delete-frame (FRAME)).  */
      return list2 (Qdelete_frame, list1 (event->frame_or_window));

    case ICONIFY_EVENT:
      /* Make an event (iconify-frame (FRAME)).  */
      return list2 (Qiconify_frame, list1 (event->frame_or_window));

    case DEICONIFY_EVENT:
      /* Make an event (make-frame-visible (FRAME)).  */
      return list2 (Qmake_frame_visible, list1 (event->frame_or_window));

    case MOVE_FRAME_EVENT:
      /* Make an event (move-frame (FRAME)).  */
      return list2 (Qmove_frame, list1 (event->frame_or_window));
#endif

    case BUFFER_SWITCH_EVENT:
      {
	/* The value doesn't matter here; only the type is tested.  */
	Lisp_Object obj;
        XSETBUFFER (obj, current_buffer);
        return obj;
      }

    /* Just discard these, by returning nil.
       With MULTI_KBOARD, these events are used as placeholders
       when we need to randomly delete events from the queue.
       (They shouldn't otherwise be found in the buffer,
       but on some machines it appears they do show up
       even without MULTI_KBOARD.)  */
    /* On Windows NT/9X, NO_EVENT is used to delete extraneous
       mouse events during a popup-menu call.  */
    case NO_EVENT:
      return Qnil;

    case HELP_EVENT:
      {
	Lisp_Object frame = event->frame_or_window;
	Lisp_Object object = event->arg;
	Lisp_Object position
          = make_fixnum (Time_to_position (event->timestamp));
	Lisp_Object window = event->x;
	Lisp_Object help = event->y;
	clear_event (event);

	if (!WINDOWP (window))
	  window = Qnil;
	return Fcons (Qhelp_echo,
		      list5 (frame, help, window, object, position));
      }

    case FOCUS_IN_EVENT:
        return make_lispy_focus_in (event->frame_or_window);

    case FOCUS_OUT_EVENT:
        return make_lispy_focus_out (event->frame_or_window);

    /* A simple keystroke.  */
    case ASCII_KEYSTROKE_EVENT:
    case MULTIBYTE_CHAR_KEYSTROKE_EVENT:
      {
	Lisp_Object lispy_c;
	EMACS_INT c = event->code;
	if (event->kind == ASCII_KEYSTROKE_EVENT)
	  {
	    c &= 0377;
	    eassert (c == event->code);
          }

        /* Caps-lock shouldn't affect interpretation of key chords:
           Control+s should produce C-s whether caps-lock is on or
           not.  And Control+Shift+s should produce C-S-s whether
           caps-lock is on or not.  */
        if (event->modifiers & ~shift_modifier)
	  {
            /* This is a key chord: some non-shift modifier is
               depressed.  */

            if (uppercasep (c) &&
                !(event->modifiers & shift_modifier))
	      {
                /* Got a capital letter without a shift.  The caps
                   lock is on.   Un-capitalize the letter.  */
                c = downcase (c);
	      }
            else if (lowercasep (c) &&
                     (event->modifiers & shift_modifier))
	      {
                /* Got a lower-case letter even though shift is
                   depressed.  The caps lock is on.  Capitalize the
                   letter.  */
                c = upcase (c);
	      }
	  }

	if (event->kind == ASCII_KEYSTROKE_EVENT)
	  {
	    /* Turn ASCII characters into control characters
	       when proper.  */
	    if (event->modifiers & ctrl_modifier)
	      {
		c = make_ctrl_char (c);
		event->modifiers &= ~ctrl_modifier;
	      }
	  }

	/* Add in the other modifier bits.  The shift key was taken care
	   of by the X code.  */
	c |= (event->modifiers
	      & (meta_modifier | alt_modifier
		 | hyper_modifier | super_modifier | ctrl_modifier));
	/* Distinguish Shift-SPC from SPC.  */
	if ((event->code) == 040
	    && event->modifiers & shift_modifier)
	  c |= shift_modifier;
	button_down_time = 0;
	XSETFASTINT (lispy_c, c);
	return lispy_c;
      }

#ifdef HAVE_NS
    case NS_TEXT_EVENT:
      return list1 (intern (event->code == KEY_NS_PUT_WORKING_TEXT
                            ? "ns-put-working-text"
                            : "ns-unput-working-text"));

      /* NS_NONKEY_EVENTs are just like NON_ASCII_KEYSTROKE_EVENTs,
	 except that they are non-key events (last-nonmenu-event is nil).  */
    case NS_NONKEY_EVENT:
#endif

      /* A function key.  The symbol may need to have modifier prefixes
	 tacked onto it.  */
    case NON_ASCII_KEYSTROKE_EVENT:
      button_down_time = 0;

      for (i = 0; i < ARRAYELTS (lispy_accent_codes); i++)
	if (event->code == lispy_accent_codes[i])
	  return modify_event_symbol (i,
				      event->modifiers,
				      Qfunction_key, Qnil,
				      lispy_accent_keys, &accent_key_syms,
                                      ARRAYELTS (lispy_accent_keys));

#if 0
#ifdef XK_kana_A
      if (event->code >= 0x400 && event->code < 0x500)
	return modify_event_symbol (event->code - 0x400,
				    event->modifiers & ~shift_modifier,
				    Qfunction_key, Qnil,
				    lispy_kana_keys, &func_key_syms,
                                    ARRAYELTS (lispy_kana_keys));
#endif /* XK_kana_A */
#endif /* 0 */

#ifdef ISO_FUNCTION_KEY_OFFSET
      if (event->code < FUNCTION_KEY_OFFSET
	  && event->code >= ISO_FUNCTION_KEY_OFFSET)
	return modify_event_symbol (event->code - ISO_FUNCTION_KEY_OFFSET,
				    event->modifiers,
				    Qfunction_key, Qnil,
				    iso_lispy_function_keys, &func_key_syms,
                                    ARRAYELTS (iso_lispy_function_keys));
#endif

      if ((FUNCTION_KEY_OFFSET <= event->code
	   && (event->code
	       < FUNCTION_KEY_OFFSET + ARRAYELTS (lispy_function_keys)))
	  && lispy_function_keys[event->code - FUNCTION_KEY_OFFSET])
	return modify_event_symbol (event->code - FUNCTION_KEY_OFFSET,
				    event->modifiers,
				    Qfunction_key, Qnil,
				    lispy_function_keys, &func_key_syms,
				    ARRAYELTS (lispy_function_keys));

      /* Handle system-specific or unknown keysyms.
	 We need to use an alist rather than a vector as the cache
	 since we can't make a vector long enough.  */
      if (NILP (KVAR (current_kboard, system_key_syms)))
	kset_system_key_syms (current_kboard, Fcons (Qnil, Qnil));
      return modify_event_symbol (event->code,
				  event->modifiers,
				  Qfunction_key,
				  KVAR (current_kboard, Vsystem_key_alist),
				  0, &KVAR (current_kboard, system_key_syms),
				  PTRDIFF_MAX);

#ifdef HAVE_NTGUI
    case END_SESSION_EVENT:
      /* Make an event (end-session).  */
      return list1 (Qend_session);

    case LANGUAGE_CHANGE_EVENT:
      /* Make an event (language-change FRAME CODEPAGE LANGUAGE-ID).  */
      return list4 (Qlanguage_change,
		    event->frame_or_window,
		    make_fixnum (event->code),
		    make_fixnum (event->modifiers));

    case MULTIMEDIA_KEY_EVENT:
      if (event->code < ARRAYELTS (lispy_multimedia_keys)
          && event->code > 0 && lispy_multimedia_keys[event->code])
        {
          return modify_event_symbol (event->code, event->modifiers,
                                      Qfunction_key, Qnil,
                                      lispy_multimedia_keys, &func_key_syms,
                                      ARRAYELTS (lispy_multimedia_keys));
        }
      return Qnil;
#endif

      /* A mouse click.  Figure out where it is, decide whether it's
         a press, click or drag, and build the appropriate structure.  */
    case MOUSE_CLICK_EVENT:
#ifdef HAVE_GPM
    case GPM_CLICK_EVENT:
#endif
#ifndef USE_TOOLKIT_SCROLL_BARS
    case SCROLL_BAR_CLICK_EVENT:
    case HORIZONTAL_SCROLL_BAR_CLICK_EVENT:
#endif
      {
	int button = event->code;
	bool is_double;
	Lisp_Object position;
	Lisp_Object *start_pos_ptr;
	Lisp_Object start_pos;

	position = Qnil;

	/* Build the position as appropriate for this mouse click.  */
	if (event->kind == MOUSE_CLICK_EVENT
#ifdef HAVE_GPM
	    || event->kind == GPM_CLICK_EVENT
#endif
	    )
	  {
	    struct frame *f = XFRAME (event->frame_or_window);
	    int row, column;

	    /* Ignore mouse events that were made on frame that
	       have been deleted.  */
	    if (! FRAME_LIVE_P (f))
	      return Qnil;

	    /* EVENT->x and EVENT->y are frame-relative pixel
	       coordinates at this place.  Under old redisplay, COLUMN
	       and ROW are set to frame relative glyph coordinates
	       which are then used to determine whether this click is
	       in a menu (non-toolkit version).  */
	    if (!toolkit_menubar_in_use (f))
	      {
		pixel_to_glyph_coords (f, XFIXNUM (event->x), XFIXNUM (event->y),
				       &column, &row, NULL, 1);

		/* In the non-toolkit version, clicks on the menu bar
		   are ordinary button events in the event buffer.
		   Distinguish them, and invoke the menu.

		   (In the toolkit version, the toolkit handles the
		   menu bar and Emacs doesn't know about it until
		   after the user makes a selection.)  */
		if (row >= 0 && row < FRAME_MENU_BAR_LINES (f)
		  && (event->modifiers & down_modifier))
		  {
		    Lisp_Object items, item;

		    /* Find the menu bar item under `column'.  */
		    item = Qnil;
		    items = FRAME_MENU_BAR_ITEMS (f);
		    for (i = 0; i < ASIZE (items); i += 4)
		      {
			Lisp_Object pos, string;
			string = AREF (items, i + 1);
			pos = AREF (items, i + 3);
			if (NILP (string))
			  break;
			if (column >= XFIXNUM (pos)
			    && column < XFIXNUM (pos) + SCHARS (string))
			  {
			    item = AREF (items, i);
			    break;
			  }
		      }

		    /* ELisp manual 2.4b says (x y) are window
		       relative but code says they are
		       frame-relative.  */
		    position = list4 (event->frame_or_window,
				      Qmenu_bar,
				      Fcons (event->x, event->y),
				      INT_TO_INTEGER (event->timestamp));

		    return list2 (item, position);
		  }
	      }

	    position = make_lispy_position (f, event->x, event->y,
					    event->timestamp);
	  }
#ifndef USE_TOOLKIT_SCROLL_BARS
	else
	  /* It's a scrollbar click.  */
	  position = make_scroll_bar_position (event, Qvertical_scroll_bar);
#endif /* not USE_TOOLKIT_SCROLL_BARS */

	if (button >= ASIZE (button_down_location))
	  {
	    ptrdiff_t incr = button - ASIZE (button_down_location) + 1;
	    button_down_location = larger_vector (button_down_location,
						  incr, -1);
	    mouse_syms = larger_vector (mouse_syms, incr, -1);
	  }

	start_pos_ptr = aref_addr (button_down_location, button);
	start_pos = *start_pos_ptr;
	*start_pos_ptr = Qnil;

	{
	  /* On window-system frames, use the value of
	     double-click-fuzz as is.  On other frames, interpret it
	     as a multiple of 1/8 characters.  */
	  struct frame *f;
	  intmax_t fuzz;

	  if (WINDOWP (event->frame_or_window))
	    f = XFRAME (XWINDOW (event->frame_or_window)->frame);
	  else if (FRAMEP (event->frame_or_window))
	    f = XFRAME (event->frame_or_window);
	  else
	    emacs_abort ();

	  if (FRAME_WINDOW_P (f))
	    fuzz = double_click_fuzz;
	  else
	    fuzz = double_click_fuzz / 8;

	  is_double = (button == last_mouse_button
		       && (eabs (XFIXNUM (event->x) - last_mouse_x) <= fuzz)
		       && (eabs (XFIXNUM (event->y) - last_mouse_y) <= fuzz)
		       && button_down_time != 0
		       && (EQ (Vdouble_click_time, Qt)
			   || (FIXNATP (Vdouble_click_time)
			       && (event->timestamp - button_down_time
				   < XFIXNAT (Vdouble_click_time)))));
	}

	last_mouse_button = button;
	last_mouse_x = XFIXNUM (event->x);
	last_mouse_y = XFIXNUM (event->y);

	/* If this is a button press, squirrel away the location, so
           we can decide later whether it was a click or a drag.  */
	if (event->modifiers & down_modifier)
	  {
	    if (is_double)
	      {
		double_click_count++;
		event->modifiers |= ((double_click_count > 2)
				     ? triple_modifier
				     : double_modifier);
	      }
	    else
	      double_click_count = 1;
	    button_down_time = event->timestamp;
	    *start_pos_ptr = Fcopy_alist (position);
	    ignore_mouse_drag_p = false;
	  }

	/* Now we're releasing a button - check the co-ordinates to
           see if this was a click or a drag.  */
	else if (event->modifiers & up_modifier)
	  {
	    /* If we did not see a down before this up, ignore the up.
	       Probably this happened because the down event chose a
	       menu item.  It would be an annoyance to treat the
	       release of the button that chose the menu item as a
	       separate event.  */

	    if (!CONSP (start_pos))
	      return Qnil;

	    unsigned click_or_drag_modifier = click_modifier;

	    if (ignore_mouse_drag_p)
	      ignore_mouse_drag_p = false;
	    else
	      {
		Lisp_Object new_down, down;
		intmax_t xdiff = double_click_fuzz, ydiff = double_click_fuzz;

		/* The third element of every position
		   should be the (x,y) pair.  */
		down = Fcar (Fcdr (Fcdr (start_pos)));
		new_down = Fcar (Fcdr (Fcdr (position)));

		if (CONSP (down)
		    && FIXNUMP (XCAR (down)) && FIXNUMP (XCDR (down)))
		  {
		    xdiff = XFIXNUM (XCAR (new_down)) - XFIXNUM (XCAR (down));
		    ydiff = XFIXNUM (XCDR (new_down)) - XFIXNUM (XCDR (down));
		  }

		if (! (0 < double_click_fuzz
		       && - double_click_fuzz < xdiff
		       && xdiff < double_click_fuzz
		       && - double_click_fuzz < ydiff
		       && ydiff < double_click_fuzz
		       /* Maybe the mouse has moved a lot, caused scrolling, and
			  eventually ended up at the same screen position (but
			  not buffer position) in which case it is a drag, not
			  a click.  */
		       /* FIXME: OTOH if the buffer position has changed
			  because of a timer or process filter rather than
			  because of mouse movement, it should be considered as
			  a click.  But mouse-drag-region completely ignores
			  this case and it hasn't caused any real problem, so
			  it's probably OK to ignore it as well.  */
		       && EQ (Fcar (Fcdr (start_pos)), Fcar (Fcdr (position)))))
		  {
		    /* Mouse has moved enough.  */
		    button_down_time = 0;
		    click_or_drag_modifier = drag_modifier;
		  }
	      }

	    /* Don't check is_double; treat this as multiple if the
	       down-event was multiple.  */
	    event->modifiers
	      = ((event->modifiers & ~up_modifier)
		 | click_or_drag_modifier
		 | (double_click_count < 2 ? 0
		    : double_click_count == 2 ? double_modifier
		    : triple_modifier));
	  }
	else
	  /* Every mouse event should either have the down_modifier or
             the up_modifier set.  */
	  emacs_abort ();

	{
	  /* Get the symbol we should use for the mouse click.  */
	  Lisp_Object head;

	  head = modify_event_symbol (button,
				      event->modifiers,
				      Qmouse_click, Vlispy_mouse_stem,
				      NULL,
				      &mouse_syms,
				      ASIZE (mouse_syms));
	  if (event->modifiers & drag_modifier)
	    return list3 (head, start_pos, position);
	  else if (event->modifiers & (double_modifier | triple_modifier))
	    return list3 (head, position, make_fixnum (double_click_count));
	  else
	    return list2 (head, position);
	}
      }

    case WHEEL_EVENT:
    case HORIZ_WHEEL_EVENT:
      {
	Lisp_Object position;
	Lisp_Object head;

	/* Build the position as appropriate for this mouse click.  */
	struct frame *f = XFRAME (event->frame_or_window);

	/* Ignore wheel events that were made on frame that have been
	   deleted.  */
	if (! FRAME_LIVE_P (f))
	  return Qnil;

	position = make_lispy_position (f, event->x, event->y,
					event->timestamp);

	/* Set double or triple modifiers to indicate the wheel speed.  */
	{
	  /* On window-system frames, use the value of
	     double-click-fuzz as is.  On other frames, interpret it
	     as a multiple of 1/8 characters.  */
	  struct frame *fr;
	  intmax_t fuzz;
	  int symbol_num;
	  bool is_double;

	  if (WINDOWP (event->frame_or_window))
	    fr = XFRAME (XWINDOW (event->frame_or_window)->frame);
	  else if (FRAMEP (event->frame_or_window))
	    fr = XFRAME (event->frame_or_window);
	  else
	    emacs_abort ();

	  fuzz = FRAME_WINDOW_P (fr)
	    ? double_click_fuzz : double_click_fuzz / 8;

	  if (event->modifiers & up_modifier)
	    {
	      /* Emit a wheel-up event.  */
	      event->modifiers &= ~up_modifier;
	      symbol_num = 0;
	    }
	  else if (event->modifiers & down_modifier)
	    {
	      /* Emit a wheel-down event.  */
	      event->modifiers &= ~down_modifier;
	      symbol_num = 1;
	    }
	  else
	    /* Every wheel event should either have the down_modifier or
	       the up_modifier set.  */
	    emacs_abort ();

          if (event->kind == HORIZ_WHEEL_EVENT)
            symbol_num += 2;

	  is_double = (last_mouse_button == - (1 + symbol_num)
		       && (eabs (XFIXNUM (event->x) - last_mouse_x) <= fuzz)
		       && (eabs (XFIXNUM (event->y) - last_mouse_y) <= fuzz)
		       && button_down_time != 0
		       && (EQ (Vdouble_click_time, Qt)
			   || (FIXNATP (Vdouble_click_time)
			       && (event->timestamp - button_down_time
				   < XFIXNAT (Vdouble_click_time)))));
	  if (is_double)
	    {
	      double_click_count++;
	      event->modifiers |= ((double_click_count > 2)
				   ? triple_modifier
				   : double_modifier);
	    }
	  else
	    {
	      double_click_count = 1;
	      event->modifiers |= click_modifier;
	    }

	  button_down_time = event->timestamp;
	  /* Use a negative value to distinguish wheel from mouse button.  */
	  last_mouse_button = - (1 + symbol_num);
	  last_mouse_x = XFIXNUM (event->x);
	  last_mouse_y = XFIXNUM (event->y);

	  /* Get the symbol we should use for the wheel event.  */
	  head = modify_event_symbol (symbol_num,
				      event->modifiers,
				      Qmouse_click,
				      Qnil,
				      lispy_wheel_names,
				      &wheel_syms,
				      ASIZE (wheel_syms));
	}

        if (NUMBERP (event->arg))
          return list4 (head, position, make_fixnum (double_click_count),
                        event->arg);
	else if (event->modifiers & (double_modifier | triple_modifier))
	  return list3 (head, position, make_fixnum (double_click_count));
	else
	  return list2 (head, position);
      }


#ifdef USE_TOOLKIT_SCROLL_BARS

      /* We don't have down and up events if using toolkit scroll bars,
	 so make this always a click event.  Store in the `part' of
	 the Lisp event a symbol which maps to the following actions:

	 `above_handle'		page up
	 `below_handle'		page down
	 `up'			line up
	 `down'			line down
	 `top'			top of buffer
	 `bottom'		bottom of buffer
	 `handle'		thumb has been dragged.
	 `end-scroll'		end of interaction with scroll bar

	 The incoming input_event contains in its `part' member an
	 index of type `enum scroll_bar_part' which we can use as an
	 index in scroll_bar_parts to get the appropriate symbol.  */

    case SCROLL_BAR_CLICK_EVENT:
      {
	Lisp_Object position, head;

	position = make_scroll_bar_position (event, Qvertical_scroll_bar);

	/* Always treat scroll bar events as clicks.  */
	event->modifiers |= click_modifier;
	event->modifiers &= ~up_modifier;

	if (event->code >= ASIZE (mouse_syms))
          mouse_syms = larger_vector (mouse_syms,
				      event->code - ASIZE (mouse_syms) + 1,
				      -1);

	/* Get the symbol we should use for the mouse click.  */
	head = modify_event_symbol (event->code,
				    event->modifiers,
				    Qmouse_click,
				    Vlispy_mouse_stem,
				    NULL, &mouse_syms,
				    ASIZE (mouse_syms));
	return list2 (head, position);
      }

    case HORIZONTAL_SCROLL_BAR_CLICK_EVENT:
      {
	Lisp_Object position, head;

	position = make_scroll_bar_position (event, Qhorizontal_scroll_bar);

	/* Always treat scroll bar events as clicks.  */
	event->modifiers |= click_modifier;
	event->modifiers &= ~up_modifier;

	if (event->code >= ASIZE (mouse_syms))
          mouse_syms = larger_vector (mouse_syms,
				      event->code - ASIZE (mouse_syms) + 1,
				      -1);

	/* Get the symbol we should use for the mouse click.  */
	head = modify_event_symbol (event->code,
				    event->modifiers,
				    Qmouse_click,
				    Vlispy_mouse_stem,
				    NULL, &mouse_syms,
				    ASIZE (mouse_syms));
	return list2 (head, position);
      }

#endif /* USE_TOOLKIT_SCROLL_BARS */

    case DRAG_N_DROP_EVENT:
      {
	struct frame *f;
	Lisp_Object head, position;
	Lisp_Object files;

	f = XFRAME (event->frame_or_window);
	files = event->arg;

	/* Ignore mouse events that were made on frames that
	   have been deleted.  */
	if (! FRAME_LIVE_P (f))
	  return Qnil;

	position = make_lispy_position (f, event->x, event->y,
					event->timestamp);

	head = modify_event_symbol (0, event->modifiers,
				    Qdrag_n_drop, Qnil,
				    lispy_drag_n_drop_names,
				    &drag_n_drop_syms, 1);
	return list3 (head, position, files);
      }

#ifdef HAVE_EXT_MENU_BAR
    case MENU_BAR_EVENT:
      if (EQ (event->arg, event->frame_or_window))
	/* This is the prefix key.  We translate this to
	   `(menu_bar)' because the code in keyboard.c for menu
	   events, which we use, relies on this.  */
	return list1 (Qmenu_bar);
      return event->arg;
#endif

    case SELECT_WINDOW_EVENT:
      /* Make an event (select-window (WINDOW)).  */
      return list2 (Qselect_window, list1 (event->frame_or_window));

    case TAB_BAR_EVENT:
      if (EQ (event->arg, event->frame_or_window))
	/* This is the prefix key.  We translate this to
	   `(tab_bar)' because the code in keyboard.c for tab bar
	   events, which we use, relies on this.  */
	return list1 (Qtab_bar);
      else if (SYMBOLP (event->arg))
	return apply_modifiers (event->modifiers, event->arg);
      return event->arg;

    case TOOL_BAR_EVENT:
      if (EQ (event->arg, event->frame_or_window))
	/* This is the prefix key.  We translate this to
	   `(tool_bar)' because the code in keyboard.c for tool bar
	   events, which we use, relies on this.  */
	return list1 (Qtool_bar);
      else if (SYMBOLP (event->arg))
	return apply_modifiers (event->modifiers, event->arg);
      return event->arg;

    case USER_SIGNAL_EVENT:
      /* A user signal.  */
      {
	char *name = find_user_signal_name (event->code);
	if (!name)
	  emacs_abort ();
	return intern (name);
      }

    case SAVE_SESSION_EVENT:
      return list2 (Qsave_session, event->arg);

#ifdef HAVE_DBUS
    case DBUS_EVENT:
      {
	return Fcons (Qdbus_event, event->arg);
      }
#endif /* HAVE_DBUS */

#ifdef THREADS_ENABLED
    case THREAD_EVENT:
      {
	return Fcons (Qthread_event, event->arg);
      }
#endif /* THREADS_ENABLED */

#ifdef HAVE_XWIDGETS
    case XWIDGET_EVENT:
      {
        return Fcons (Qxwidget_event, event->arg);
      }
#endif

#ifdef USE_FILE_NOTIFY
    case FILE_NOTIFY_EVENT:
#ifdef HAVE_W32NOTIFY
      /* Make an event (file-notify (DESCRIPTOR ACTION FILE) CALLBACK).  */
      return list3 (Qfile_notify, event->arg, event->frame_or_window);
#else
      return Fcons (Qfile_notify, event->arg);
#endif
#endif /* USE_FILE_NOTIFY */

    case CONFIG_CHANGED_EVENT:
	return list3 (Qconfig_changed_event,
		      event->arg, event->frame_or_window);

      /* The 'kind' field of the event is something we don't recognize.  */
    default:
      emacs_abort ();
    }
}

static Lisp_Object
make_lispy_movement (struct frame *frame, Lisp_Object bar_window, enum scroll_bar_part part,
		     Lisp_Object x, Lisp_Object y, Time t)
{
  /* Is it a scroll bar movement?  */
  if (frame && ! NILP (bar_window))
    {
      Lisp_Object part_sym;

      part_sym = builtin_lisp_symbol (scroll_bar_parts[part]);
      return list2 (Qscroll_bar_movement,
		    list5 (bar_window,
			   Qvertical_scroll_bar,
			   Fcons (x, y),
			   make_fixnum (t),
			   part_sym));
    }
  /* Or is it an ordinary mouse movement?  */
  else
    {
      Lisp_Object position;
      position = make_lispy_position (frame, x, y, t);
      return list2 (Qmouse_movement, position);
    }
}

/* Construct a switch frame event.  */
static Lisp_Object
make_lispy_switch_frame (Lisp_Object frame)
{
  return list2 (Qswitch_frame, frame);
}

static Lisp_Object
make_lispy_focus_in (Lisp_Object frame)
{
  return list2 (Qfocus_in, frame);
}

static Lisp_Object
make_lispy_focus_out (Lisp_Object frame)
{
  return list2 (Qfocus_out, frame);
}

/* Manipulating modifiers.  */

/* Parse the name of SYMBOL, and return the set of modifiers it contains.

   If MODIFIER_END is non-zero, set *MODIFIER_END to the position in
   SYMBOL's name of the end of the modifiers; the string from this
   position is the unmodified symbol name.

   This doesn't use any caches.  */

static int
parse_modifiers_uncached (Lisp_Object symbol, ptrdiff_t *modifier_end)
{
  Lisp_Object name;
  ptrdiff_t i;
  int modifiers;

  CHECK_SYMBOL (symbol);

  modifiers = 0;
  name = SYMBOL_NAME (symbol);

  for (i = 0; i < SBYTES (name) - 1; )
    {
      ptrdiff_t this_mod_end = 0;
      int this_mod = 0;

      /* See if the name continues with a modifier word.
	 Check that the word appears, but don't check what follows it.
	 Set this_mod and this_mod_end to record what we find.  */

      switch (SREF (name, i))
	{
#define SINGLE_LETTER_MOD(BIT)				\
	  (this_mod_end = i + 1, this_mod = BIT)

	case 'A':
	  SINGLE_LETTER_MOD (alt_modifier);
	  break;

	case 'C':
	  SINGLE_LETTER_MOD (ctrl_modifier);
	  break;

	case 'H':
	  SINGLE_LETTER_MOD (hyper_modifier);
	  break;

	case 'M':
	  SINGLE_LETTER_MOD (meta_modifier);
	  break;

	case 'S':
	  SINGLE_LETTER_MOD (shift_modifier);
	  break;

	case 's':
	  SINGLE_LETTER_MOD (super_modifier);
	  break;

#undef SINGLE_LETTER_MOD

#define MULTI_LETTER_MOD(BIT, NAME, LEN)			\
	  if (i + LEN + 1 <= SBYTES (name)			\
	      && ! memcmp (SDATA (name) + i, NAME, LEN))	\
	    {							\
	      this_mod_end = i + LEN;				\
	      this_mod = BIT;					\
	    }

	case 'd':
	  MULTI_LETTER_MOD (drag_modifier, "drag", 4);
	  MULTI_LETTER_MOD (down_modifier, "down", 4);
	  MULTI_LETTER_MOD (double_modifier, "double", 6);
	  break;

	case 't':
	  MULTI_LETTER_MOD (triple_modifier, "triple", 6);
	  break;

	case 'u':
	  MULTI_LETTER_MOD (up_modifier, "up", 2);
	  break;
#undef MULTI_LETTER_MOD

	}

      /* If we found no modifier, stop looking for them.  */
      if (this_mod_end == 0)
	break;

      /* Check there is a dash after the modifier, so that it
	 really is a modifier.  */
      if (this_mod_end >= SBYTES (name)
	  || SREF (name, this_mod_end) != '-')
	break;

      /* This modifier is real; look for another.  */
      modifiers |= this_mod;
      i = this_mod_end + 1;
    }

  /* Should we include the `click' modifier?  */
  if (! (modifiers & (down_modifier | drag_modifier
		      | double_modifier | triple_modifier))
      && i + 7 == SBYTES (name)
      && memcmp (SDATA (name) + i, "mouse-", 6) == 0
      && ('0' <= SREF (name, i + 6) && SREF (name, i + 6) <= '9'))
    modifiers |= click_modifier;

  if (! (modifiers & (double_modifier | triple_modifier))
      && i + 6 < SBYTES (name)
      && memcmp (SDATA (name) + i, "wheel-", 6) == 0)
    modifiers |= click_modifier;

  if (modifier_end)
    *modifier_end = i;

  return modifiers;
}

/* Return a symbol whose name is the modifier prefixes for MODIFIERS
   prepended to the string BASE[0..BASE_LEN-1].
   This doesn't use any caches.  */
static Lisp_Object
apply_modifiers_uncached (int modifiers, char *base, int base_len, int base_len_byte)
{
  /* Since BASE could contain NULs, we can't use intern here; we have
     to use Fintern, which expects a genuine Lisp_String, and keeps a
     reference to it.  */
  char new_mods[sizeof "A-C-H-M-S-s-up-down-drag-double-triple-"];
  int mod_len;

  {
    char *p = new_mods;

    /* Mouse events should not exhibit the `up' modifier once they
       leave the event queue only accessible to C code; `up' will
       always be turned into a click or drag event before being
       presented to lisp code.  But since lisp events can be
       synthesized bypassing the event queue and pushed into
       `unread-command-events' or its companions, it's better to just
       deal with unexpected modifier combinations. */

    if (modifiers & alt_modifier)   { *p++ = 'A'; *p++ = '-'; }
    if (modifiers & ctrl_modifier)  { *p++ = 'C'; *p++ = '-'; }
    if (modifiers & hyper_modifier) { *p++ = 'H'; *p++ = '-'; }
    if (modifiers & meta_modifier)  { *p++ = 'M'; *p++ = '-'; }
    if (modifiers & shift_modifier) { *p++ = 'S'; *p++ = '-'; }
    if (modifiers & super_modifier) { *p++ = 's'; *p++ = '-'; }
    if (modifiers & double_modifier) p = stpcpy (p, "double-");
    if (modifiers & triple_modifier) p = stpcpy (p, "triple-");
    if (modifiers & up_modifier) p = stpcpy (p, "up-");
    if (modifiers & down_modifier) p = stpcpy (p, "down-");
    if (modifiers & drag_modifier) p = stpcpy (p, "drag-");
    /* The click modifier is denoted by the absence of other modifiers.  */

    *p = '\0';

    mod_len = p - new_mods;
  }

  {
    Lisp_Object new_name;

    new_name = make_uninit_multibyte_string (mod_len + base_len,
					     mod_len + base_len_byte);
    memcpy (SDATA (new_name), new_mods, mod_len);
    memcpy (SDATA (new_name) + mod_len, base, base_len_byte);

    return Fintern (new_name, Qnil);
  }
}


static const char *const modifier_names[] =
{
  "up", "down", "drag", "click", "double", "triple", 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, "alt", "super", "hyper", "shift", "control", "meta"
};
#define NUM_MOD_NAMES ARRAYELTS (modifier_names)

static Lisp_Object modifier_symbols;

/* Return the list of modifier symbols corresponding to the mask MODIFIERS.  */
static Lisp_Object
lispy_modifier_list (int modifiers)
{
  Lisp_Object modifier_list;
  int i;

  modifier_list = Qnil;
  for (i = 0; (1<<i) <= modifiers && i < NUM_MOD_NAMES; i++)
    if (modifiers & (1<<i))
      modifier_list = Fcons (AREF (modifier_symbols, i),
			     modifier_list);

  return modifier_list;
}


/* Parse the modifiers on SYMBOL, and return a list like (UNMODIFIED MASK),
   where UNMODIFIED is the unmodified form of SYMBOL,
   MASK is the set of modifiers present in SYMBOL's name.
   This is similar to parse_modifiers_uncached, but uses the cache in
   SYMBOL's Qevent_symbol_element_mask property, and maintains the
   Qevent_symbol_elements property.  */

#define KEY_TO_CHAR(k) (XFIXNUM (k) & ((1 << CHARACTERBITS) - 1))

Lisp_Object
parse_modifiers (Lisp_Object symbol)
{
  Lisp_Object elements;

  if (FIXNUMP (symbol))
    return list2i (KEY_TO_CHAR (symbol), XFIXNUM (symbol) & CHAR_MODIFIER_MASK);
  else if (!SYMBOLP (symbol))
    return Qnil;

  elements = Fget (symbol, Qevent_symbol_element_mask);
  if (CONSP (elements))
    return elements;
  else
    {
      ptrdiff_t end;
      int modifiers = parse_modifiers_uncached (symbol, &end);
      Lisp_Object unmodified;
      Lisp_Object mask;

      unmodified = Fintern (make_string (SSDATA (SYMBOL_NAME (symbol)) + end,
					 SBYTES (SYMBOL_NAME (symbol)) - end),
			    Qnil);

      if (modifiers & ~INTMASK)
	emacs_abort ();
      XSETFASTINT (mask, modifiers);
      elements = list2 (unmodified, mask);

      /* Cache the parsing results on SYMBOL.  */
      Fput (symbol, Qevent_symbol_element_mask,
	    elements);
      Fput (symbol, Qevent_symbol_elements,
	    Fcons (unmodified, lispy_modifier_list (modifiers)));

      /* Since we know that SYMBOL is modifiers applied to unmodified,
	 it would be nice to put that in unmodified's cache.
	 But we can't, since we're not sure that parse_modifiers is
	 canonical.  */

      return elements;
    }
}

DEFUN ("internal-event-symbol-parse-modifiers", Fevent_symbol_parse_modifiers,
       Sevent_symbol_parse_modifiers, 1, 1, 0,
       doc: /* Parse the event symbol.  For internal use.  */)
  (Lisp_Object symbol)
{
  /* Fill the cache if needed.  */
  parse_modifiers (symbol);
  /* Ignore the result (which is stored on Qevent_symbol_element_mask)
     and use the Lispier representation stored on Qevent_symbol_elements
     instead.  */
  return Fget (symbol, Qevent_symbol_elements);
}

/* Apply the modifiers MODIFIERS to the symbol BASE.
   BASE must be unmodified.

   This is like apply_modifiers_uncached, but uses BASE's
   Qmodifier_cache property, if present.

   apply_modifiers copies the value of BASE's Qevent_kind property to
   the modified symbol.  */
static Lisp_Object
apply_modifiers (int modifiers, Lisp_Object base)
{
  Lisp_Object cache, idx, entry, new_symbol;

  /* Mask out upper bits.  We don't know where this value's been.  */
  modifiers &= INTMASK;

  if (FIXNUMP (base))
    return make_fixnum (XFIXNUM (base) | modifiers);

  /* The click modifier never figures into cache indices.  */
  cache = Fget (base, Qmodifier_cache);
  XSETFASTINT (idx, (modifiers & ~click_modifier));
  entry = assq_no_quit (idx, cache);

  if (CONSP (entry))
    new_symbol = XCDR (entry);
  else
    {
      /* We have to create the symbol ourselves.  */
      new_symbol = apply_modifiers_uncached (modifiers,
					     SSDATA (SYMBOL_NAME (base)),
					     SCHARS (SYMBOL_NAME (base)),
					     SBYTES (SYMBOL_NAME (base)));

      /* Add the new symbol to the base's cache.  */
      entry = Fcons (idx, new_symbol);
      Fput (base, Qmodifier_cache, Fcons (entry, cache));

      /* We have the parsing info now for free, so we could add it to
	 the caches:
         XSETFASTINT (idx, modifiers);
         Fput (new_symbol, Qevent_symbol_element_mask,
               list2 (base, idx));
         Fput (new_symbol, Qevent_symbol_elements,
               Fcons (base, lispy_modifier_list (modifiers)));
	 Sadly, this is only correct if `base' is indeed a base event,
	 which is not necessarily the case.  -stef  */
    }

  /* Make sure this symbol is of the same kind as BASE.

     You'd think we could just set this once and for all when we
     intern the symbol above, but reorder_modifiers may call us when
     BASE's property isn't set right; we can't assume that just
     because it has a Qmodifier_cache property it must have its
     Qevent_kind set right as well.  */
  if (NILP (Fget (new_symbol, Qevent_kind)))
    {
      Lisp_Object kind;

      kind = Fget (base, Qevent_kind);
      if (! NILP (kind))
	Fput (new_symbol, Qevent_kind, kind);
    }

  return new_symbol;
}


/* Given a symbol whose name begins with modifiers ("C-", "M-", etc),
   return a symbol with the modifiers placed in the canonical order.
   Canonical order is alphabetical, except for down and drag, which
   always come last.  The 'click' modifier is never written out.

   Fdefine_key calls this to make sure that (for example) C-M-foo
   and M-C-foo end up being equivalent in the keymap.  */

Lisp_Object
reorder_modifiers (Lisp_Object symbol)
{
  /* It's hopefully okay to write the code this way, since everything
     will soon be in caches, and no consing will be done at all.  */
  Lisp_Object parsed;

  parsed = parse_modifiers (symbol);
  return apply_modifiers (XFIXNAT (XCAR (XCDR (parsed))),
			  XCAR (parsed));
}


/* For handling events, we often want to produce a symbol whose name
   is a series of modifier key prefixes ("M-", "C-", etcetera) attached
   to some base, like the name of a function key or mouse button.
   modify_event_symbol produces symbols of this sort.

   NAME_TABLE should point to an array of strings, such that NAME_TABLE[i]
   is the name of the i'th symbol.  TABLE_SIZE is the number of elements
   in the table.

   Alternatively, NAME_ALIST_OR_STEM is either an alist mapping codes
   into symbol names, or a string specifying a name stem used to
   construct a symbol name or the form `STEM-N', where N is the decimal
   representation of SYMBOL_NUM.  NAME_ALIST_OR_STEM is used if it is
   non-nil; otherwise NAME_TABLE is used.

   SYMBOL_TABLE should be a pointer to a Lisp_Object whose value will
   persist between calls to modify_event_symbol that it can use to
   store a cache of the symbols it's generated for this NAME_TABLE
   before.  The object stored there may be a vector or an alist.

   SYMBOL_NUM is the number of the base name we want from NAME_TABLE.

   MODIFIERS is a set of modifier bits (as given in struct input_events)
   whose prefixes should be applied to the symbol name.

   SYMBOL_KIND is the value to be placed in the event_kind property of
   the returned symbol.

   The symbols we create are supposed to have an
   `event-symbol-elements' property, which lists the modifiers present
   in the symbol's name.  */

static Lisp_Object
modify_event_symbol (ptrdiff_t symbol_num, int modifiers, Lisp_Object symbol_kind,
		     Lisp_Object name_alist_or_stem, const char *const *name_table,
		     Lisp_Object *symbol_table, ptrdiff_t table_size)
{
  Lisp_Object value;
  Lisp_Object symbol_int;

  /* Get rid of the "vendor-specific" bit here.  */
  XSETINT (symbol_int, symbol_num & 0xffffff);

  /* Is this a request for a valid symbol?  */
  if (symbol_num < 0 || symbol_num >= table_size)
    return Qnil;

  if (CONSP (*symbol_table))
    value = Fcdr (assq_no_quit (symbol_int, *symbol_table));

  /* If *symbol_table doesn't seem to be initialized properly, fix that.
     *symbol_table should be a lisp vector TABLE_SIZE elements long,
     where the Nth element is the symbol for NAME_TABLE[N], or nil if
     we've never used that symbol before.  */
  else
    {
      if (! VECTORP (*symbol_table)
	  || ASIZE (*symbol_table) != table_size)
	*symbol_table = make_nil_vector (table_size);

      value = AREF (*symbol_table, symbol_num);
    }

  /* Have we already used this symbol before?  */
  if (NILP (value))
    {
      /* No; let's create it.  */
      if (CONSP (name_alist_or_stem))
	value = Fcdr_safe (Fassq (symbol_int, name_alist_or_stem));
      else if (STRINGP (name_alist_or_stem))
	{
	  char *buf;
	  ptrdiff_t len = (SBYTES (name_alist_or_stem)
			   + sizeof "-" + INT_STRLEN_BOUND (EMACS_INT));
	  USE_SAFE_ALLOCA;
	  buf = SAFE_ALLOCA (len);
	  esprintf (buf, "%s-%"pI"d", SDATA (name_alist_or_stem),
		    XFIXNUM (symbol_int) + 1);
	  value = intern (buf);
	  SAFE_FREE ();
	}
      else if (name_table != 0 && name_table[symbol_num])
	value = intern (name_table[symbol_num]);

#ifdef HAVE_WINDOW_SYSTEM
      if (NILP (value))
	{
	  char *name = get_keysym_name (symbol_num);
	  if (name)
	    value = intern (name);
	}
#endif

      if (NILP (value))
	{
	  char buf[sizeof "key-" + INT_STRLEN_BOUND (EMACS_INT)];
	  sprintf (buf, "key-%"pD"d", symbol_num);
	  value = intern (buf);
	}

      if (CONSP (*symbol_table))
        *symbol_table = Fcons (Fcons (symbol_int, value), *symbol_table);
      else
	ASET (*symbol_table, symbol_num, value);

      /* Fill in the cache entries for this symbol; this also
	 builds the Qevent_symbol_elements property, which the user
	 cares about.  */
      apply_modifiers (modifiers & click_modifier, value);
      Fput (value, Qevent_kind, symbol_kind);
    }

  /* Apply modifiers to that symbol.  */
  return apply_modifiers (modifiers, value);
}

/* Convert a list that represents an event type,
   such as (ctrl meta backspace), into the usual representation of that
   event type as a number or a symbol.  */

DEFUN ("event-convert-list", Fevent_convert_list, Sevent_convert_list, 1, 1, 0,
       doc: /* Convert the event description list EVENT-DESC to an event type.
EVENT-DESC should contain one base event type (a character or symbol)
and zero or more modifier names (control, meta, hyper, super, shift, alt,
drag, down, double or triple).  The base must be last.
The return value is an event type (a character or symbol) which
has the same base event type and all the specified modifiers.  */)
  (Lisp_Object event_desc)
{
  Lisp_Object base = Qnil;
  int modifiers = 0;

  FOR_EACH_TAIL_SAFE (event_desc)
    {
      Lisp_Object elt = XCAR (event_desc);
      int this = 0;

      /* Given a symbol, see if it is a modifier name.  */
      if (SYMBOLP (elt) && CONSP (XCDR (event_desc)))
	this = parse_solitary_modifier (elt);

      if (this != 0)
	modifiers |= this;
      else if (!NILP (base))
	error ("Two bases given in one event");
      else
	base = elt;
    }

  /* Let the symbol A refer to the character A.  */
  if (SYMBOLP (base) && SCHARS (SYMBOL_NAME (base)) == 1)
    XSETINT (base, SREF (SYMBOL_NAME (base), 0));

  if (FIXNUMP (base))
    {
      /* Turn (shift a) into A.  */
      if ((modifiers & shift_modifier) != 0
	  && (XFIXNUM (base) >= 'a' && XFIXNUM (base) <= 'z'))
	{
	  XSETINT (base, XFIXNUM (base) - ('a' - 'A'));
	  modifiers &= ~shift_modifier;
	}

      /* Turn (control a) into C-a.  */
      if (modifiers & ctrl_modifier)
	return make_fixnum ((modifiers & ~ctrl_modifier)
			    | make_ctrl_char (XFIXNUM (base)));
      else
	return make_fixnum (modifiers | XFIXNUM (base));
    }
  else if (SYMBOLP (base))
    return apply_modifiers (modifiers, base);
  else
    error ("Invalid base event");
}

DEFUN ("internal-handle-focus-in", Finternal_handle_focus_in,
       Sinternal_handle_focus_in, 1, 1, 0,
       doc: /* Internally handle focus-in events.
This function potentially generates an artifical switch-frame event.  */)
     (Lisp_Object event)
{
  Lisp_Object frame;
  if (!EQ (CAR_SAFE (event), Qfocus_in) ||
      !CONSP (XCDR (event)) ||
      !FRAMEP ((frame = XCAR (XCDR (event)))))
    error ("invalid focus-in event");

  /* Conceptually, the concept of window manager focus on a particular
     frame and the Emacs selected frame shouldn't be related, but for
     a long time, we automatically switched the selected frame in
     response to focus events, so let's keep doing that.  */
  bool switching = (!EQ (frame, internal_last_event_frame)
                    && !EQ (frame, selected_frame));
  internal_last_event_frame = frame;
  if (switching || !NILP (unread_switch_frame))
    unread_switch_frame = make_lispy_switch_frame (frame);

  return Qnil;
}

/* Try to recognize SYMBOL as a modifier name.
   Return the modifier flag bit, or 0 if not recognized.  */

int
parse_solitary_modifier (Lisp_Object symbol)
{
  Lisp_Object name;

  if (!SYMBOLP (symbol))
    return 0;

  name = SYMBOL_NAME (symbol);

  switch (SREF (name, 0))
    {
#define SINGLE_LETTER_MOD(BIT)				\
      if (SBYTES (name) == 1)				\
	return BIT;

#define MULTI_LETTER_MOD(BIT, NAME, LEN)		\
      if (LEN == SBYTES (name)				\
	  && ! memcmp (SDATA (name), NAME, LEN))	\
	return BIT;

    case 'A':
      SINGLE_LETTER_MOD (alt_modifier);
      break;

    case 'a':
      MULTI_LETTER_MOD (alt_modifier, "alt", 3);
      break;

    case 'C':
      SINGLE_LETTER_MOD (ctrl_modifier);
      break;

    case 'c':
      MULTI_LETTER_MOD (ctrl_modifier, "ctrl", 4);
      MULTI_LETTER_MOD (ctrl_modifier, "control", 7);
      break;

    case 'H':
      SINGLE_LETTER_MOD (hyper_modifier);
      break;

    case 'h':
      MULTI_LETTER_MOD (hyper_modifier, "hyper", 5);
      break;

    case 'M':
      SINGLE_LETTER_MOD (meta_modifier);
      break;

    case 'm':
      MULTI_LETTER_MOD (meta_modifier, "meta", 4);
      break;

    case 'S':
      SINGLE_LETTER_MOD (shift_modifier);
      break;

    case 's':
      MULTI_LETTER_MOD (shift_modifier, "shift", 5);
      MULTI_LETTER_MOD (super_modifier, "super", 5);
      SINGLE_LETTER_MOD (super_modifier);
      break;

    case 'd':
      MULTI_LETTER_MOD (drag_modifier, "drag", 4);
      MULTI_LETTER_MOD (down_modifier, "down", 4);
      MULTI_LETTER_MOD (double_modifier, "double", 6);
      break;

    case 't':
      MULTI_LETTER_MOD (triple_modifier, "triple", 6);
      break;

    case 'u':
      MULTI_LETTER_MOD (up_modifier, "up", 2);
      break;

#undef SINGLE_LETTER_MOD
#undef MULTI_LETTER_MOD
    }

  return 0;
}

/* Return true if EVENT is a list whose elements are all integers or symbols.
   Such a list is not valid as an event,
   but it can be a Lucid-style event type list.  */

bool
lucid_event_type_list_p (Lisp_Object object)
{
  if (! CONSP (object))
    return false;

  if (EQ (XCAR (object), Qhelp_echo)
      || EQ (XCAR (object), Qvertical_line)
      || EQ (XCAR (object), Qmode_line)
      || EQ (XCAR (object), Qtab_line)
      || EQ (XCAR (object), Qheader_line))
    return false;

  Lisp_Object tail = object;
  FOR_EACH_TAIL_SAFE (object)
    {
      Lisp_Object elt = XCAR (object);
      if (! (FIXNUMP (elt) || SYMBOLP (elt)))
	return false;
      tail = XCDR (object);
    }

  return NILP (tail);
}

/* Return true if terminal input chars are available.
   Also, store the return value into INPUT_PENDING.

   Serves the purpose of ioctl (0, FIONREAD, ...)
   but works even if FIONREAD does not exist.
   (In fact, this may actually read some input.)

   If READABLE_EVENTS_DO_TIMERS_NOW is set in FLAGS, actually run
   timer events that are ripe.
   If READABLE_EVENTS_FILTER_EVENTS is set in FLAGS, ignore internal
   events (FOCUS_IN_EVENT).
   If READABLE_EVENTS_IGNORE_SQUEEZABLES is set in FLAGS, ignore mouse
   movements and toolkit scroll bar thumb drags.  */

static bool
get_input_pending (int flags)
{
  /* First of all, have we already counted some input?  */
  input_pending = (!NILP (Vquit_flag) || readable_events (flags));

  /* If input is being read as it arrives, and we have none, there is none.  */
  if (!input_pending && (!interrupt_input || interrupts_deferred))
    {
      /* Try to read some input and see how much we get.  */
      gobble_input ();
      input_pending = (!NILP (Vquit_flag) || readable_events (flags));
    }

  return input_pending;
}

/* Put a BUFFER_SWITCH_EVENT in the buffer
   so that read_key_sequence will notice the new current buffer.  */

void
record_asynch_buffer_change (void)
{
  /* We don't need a buffer-switch event unless Emacs is waiting for input.
     The purpose of the event is to make read_key_sequence look up the
     keymaps again.  If we aren't in read_key_sequence, we don't need one,
     and the event could cause trouble by messing up (input-pending-p).
     Note: Fwaiting_for_user_input_p always returns nil when async
     subprocesses aren't supported.  */
  if (!NILP (Fwaiting_for_user_input_p ()))
    {
      struct input_event event;

      EVENT_INIT (event);
      event.kind = BUFFER_SWITCH_EVENT;
      event.frame_or_window = Qnil;
      event.arg = Qnil;

      /* Make sure no interrupt happens while storing the event.  */
#ifdef USABLE_SIGIO
      if (interrupt_input)
	kbd_buffer_store_event (&event);
      else
#endif
	{
	  stop_polling ();
	  kbd_buffer_store_event (&event);
	  start_polling ();
	}
    }
}

/* Read any terminal input already buffered up by the system
   into the kbd_buffer, but do not wait.

   Return the number of keyboard chars read, or -1 meaning
   this is a bad time to try to read input.  */

int
gobble_input (void)
{
  int nread = 0;
  bool err = false;
  struct terminal *t;

  /* Store pending user signal events, if any.  */
  store_user_signal_events ();

  /* Loop through the available terminals, and call their input hooks.  */
  t = terminal_list;
  while (t)
    {
      struct terminal *next = t->next_terminal;

      if (t->read_socket_hook)
        {
          int nr;
          struct input_event hold_quit;

	  if (input_blocked_p ())
	    {
	      pending_signals = true;
	      break;
	    }

          EVENT_INIT (hold_quit);
          hold_quit.kind = NO_EVENT;

          /* No need for FIONREAD or fcntl; just say don't wait.  */
	  while ((nr = (*t->read_socket_hook) (t, &hold_quit)) > 0)
	    nread += nr;

          if (nr == -1)          /* Not OK to read input now.  */
            {
              err = true;
            }
          else if (nr == -2)          /* Non-transient error.  */
            {
              /* The terminal device terminated; it should be closed.  */

              /* Kill Emacs if this was our last terminal.  */
              if (!terminal_list->next_terminal)
                /* Formerly simply reported no input, but that
                   sometimes led to a failure of Emacs to terminate.
                   SIGHUP seems appropriate if we can't reach the
                   terminal.  */
                /* ??? Is it really right to send the signal just to
                   this process rather than to the whole process
                   group?  Perhaps on systems with FIONREAD Emacs is
                   alone in its group.  */
		terminate_due_to_signal (SIGHUP, 10);

              /* XXX Is calling delete_terminal safe here?  It calls delete_frame.  */
	      {
		Lisp_Object tmp;
		XSETTERMINAL (tmp, t);
		Fdelete_terminal (tmp, Qnoelisp);
	      }
            }

	  /* If there was no error, make sure the pointer
	     is visible for all frames on this terminal.  */
	  if (nr >= 0)
	    {
	      Lisp_Object tail, frame;

	      FOR_EACH_FRAME (tail, frame)
		{
		  struct frame *f = XFRAME (frame);
		  if (FRAME_TERMINAL (f) == t)
		    frame_make_pointer_visible (f);
		}
	    }

          if (hold_quit.kind != NO_EVENT)
            kbd_buffer_store_event (&hold_quit);
        }

      t = next;
    }

  if (err && !nread)
    nread = -1;

  return nread;
}

/* This is the tty way of reading available input.

   Note that each terminal device has its own `struct terminal' object,
   and so this function is called once for each individual termcap
   terminal.  The first parameter indicates which terminal to read from.  */

int
tty_read_avail_input (struct terminal *terminal,
                      struct input_event *hold_quit)
{
  /* Using KBD_BUFFER_SIZE - 1 here avoids reading more than
     the kbd_buffer can really hold.  That may prevent loss
     of characters on some systems when input is stuffed at us.  */
  unsigned char cbuf[KBD_BUFFER_SIZE - 1];
#ifndef WINDOWSNT
  int n_to_read;
#endif
  int i;
  struct tty_display_info *tty = terminal->display_info.tty;
  int nread = 0;
#ifdef subprocesses
  int buffer_free = KBD_BUFFER_SIZE - kbd_buffer_nr_stored () - 1;

  if (kbd_on_hold_p () || buffer_free <= 0)
    return 0;
#endif	/* subprocesses */

  if (!terminal->name)		/* Don't read from a dead terminal.  */
    return 0;

  if (terminal->type != output_termcap
      && terminal->type != output_msdos_raw)
    emacs_abort ();

  /* XXX I think the following code should be moved to separate hook
     functions in system-dependent files.  */
#ifdef WINDOWSNT
  /* FIXME: AFAIK, tty_read_avail_input is not used under w32 since the non-GUI
     code sets read_socket_hook to w32_console_read_socket instead!  */
  return 0;
#else /* not WINDOWSNT */
  if (! tty->term_initted)      /* In case we get called during bootstrap.  */
    return 0;

  if (! tty->input)
    return 0;                   /* The terminal is suspended.  */

#ifdef MSDOS
  n_to_read = dos_keysns ();
  if (n_to_read == 0)
    return 0;

  cbuf[0] = dos_keyread ();
  nread = 1;

#else /* not MSDOS */
#ifdef HAVE_GPM
  if (gpm_tty == tty)
  {
      Gpm_Event event;
      struct input_event gpm_hold_quit;
      int gpm, fd = gpm_fd;

      EVENT_INIT (gpm_hold_quit);
      gpm_hold_quit.kind = NO_EVENT;

      /* gpm==1 if event received.
         gpm==0 if the GPM daemon has closed the connection, in which case
                Gpm_GetEvent closes gpm_fd and clears it to -1, which is why
		we save it in `fd' so close_gpm can remove it from the
		select masks.
         gpm==-1 if a protocol error or EWOULDBLOCK; the latter is normal.  */
      while (gpm = Gpm_GetEvent (&event), gpm == 1) {
	  nread += handle_one_term_event (tty, &event, &gpm_hold_quit);
      }
      if (gpm == 0)
	/* Presumably the GPM daemon has closed the connection.  */
	close_gpm (fd);
      if (gpm_hold_quit.kind != NO_EVENT)
	  kbd_buffer_store_event (&gpm_hold_quit);
      if (nread)
	  return nread;
  }
#endif /* HAVE_GPM */

/* Determine how many characters we should *try* to read.  */
#ifdef USABLE_FIONREAD
  /* Find out how much input is available.  */
  if (ioctl (fileno (tty->input), FIONREAD, &n_to_read) < 0)
    {
      if (! noninteractive)
        return -2;          /* Close this terminal.  */
      else
        n_to_read = 0;
    }
  if (n_to_read == 0)
    return 0;
  if (n_to_read > sizeof cbuf)
    n_to_read = sizeof cbuf;
#elif defined USG || defined CYGWIN
  /* Read some input if available, but don't wait.  */
  n_to_read = sizeof cbuf;
  fcntl (fileno (tty->input), F_SETFL, O_NONBLOCK);
#else
# error "Cannot read without possibly delaying"
#endif

#ifdef subprocesses
  /* Don't read more than we can store.  */
  if (n_to_read > buffer_free)
    n_to_read = buffer_free;
#endif	/* subprocesses */

  /* Now read; for one reason or another, this will not block.
     NREAD is set to the number of chars read.  */
  nread = emacs_read (fileno (tty->input), (char *) cbuf, n_to_read);
  /* POSIX infers that processes which are not in the session leader's
     process group won't get SIGHUPs at logout time.  BSDI adheres to
     this part standard and returns -1 from read (0) with errno==EIO
     when the control tty is taken away.
     Jeffrey Honig <jch@bsdi.com> says this is generally safe.  */
  if (nread == -1 && errno == EIO)
    return -2;          /* Close this terminal.  */
#if defined AIX && defined _BSD
  /* The kernel sometimes fails to deliver SIGHUP for ptys.
     This looks incorrect, but it isn't, because _BSD causes
     O_NDELAY to be defined in fcntl.h as O_NONBLOCK,
     and that causes a value other than 0 when there is no input.  */
  if (nread == 0)
    return -2;          /* Close this terminal.  */
#endif

#ifndef USABLE_FIONREAD
#if defined (USG) || defined (CYGWIN)
  fcntl (fileno (tty->input), F_SETFL, 0);
#endif /* USG or CYGWIN */
#endif /* no FIONREAD */

  if (nread <= 0)
    return nread;

#endif /* not MSDOS */
#endif /* not WINDOWSNT */

  for (i = 0; i < nread; i++)
    {
      struct input_event buf;
      EVENT_INIT (buf);
      buf.kind = ASCII_KEYSTROKE_EVENT;
      buf.modifiers = 0;
      if (tty->meta_key == 1 && (cbuf[i] & 0x80))
        buf.modifiers = meta_modifier;
      if (tty->meta_key != 2)
        cbuf[i] &= ~0x80;

      buf.code = cbuf[i];
      /* Set the frame corresponding to the active tty.  Note that the
         value of selected_frame is not reliable here, redisplay tends
         to temporarily change it.  */
      buf.frame_or_window = tty->top_frame;
      buf.arg = Qnil;

      kbd_buffer_store_event (&buf);
      /* Don't look at input that follows a C-g too closely.
         This reduces lossage due to autorepeat on C-g.  */
      if (buf.kind == ASCII_KEYSTROKE_EVENT
          && buf.code == quit_char)
        break;
    }

  return nread;
}

static void
handle_async_input (void)
{
#ifdef USABLE_SIGIO
  while (1)
    {
      int nread = gobble_input ();
      /* -1 means it's not ok to read the input now.
	 UNBLOCK_INPUT will read it later; now, avoid infinite loop.
	 0 means there was no keyboard input available.  */
      if (nread <= 0)
	break;
    }
#endif
}

void
process_pending_signals (void)
{
  pending_signals = false;
  handle_async_input ();
  do_pending_atimers ();
}

/* Undo any number of BLOCK_INPUT calls down to level LEVEL,
   and reinvoke any pending signal if the level is now 0 and
   a fatal error is not already in progress.  */

void
unblock_input_to (int level)
{
  interrupt_input_blocked = level;
  if (level == 0)
    {
      if (pending_signals && !fatal_error_in_progress)
	process_pending_signals ();
    }
  else if (level < 0)
    emacs_abort ();
}

/* End critical section.

   If doing signal-driven input, and a signal came in when input was
   blocked, reinvoke the signal handler now to deal with it.

   It will also process queued input, if it was not read before.
   When a longer code sequence does not use block/unblock input
   at all, the whole input gathered up to the next call to
   unblock_input will be processed inside that call. */

void
unblock_input (void)
{
  unblock_input_to (interrupt_input_blocked - 1);
}

/* Undo any number of BLOCK_INPUT calls,
   and also reinvoke any pending signal.  */

void
totally_unblock_input (void)
{
  unblock_input_to (0);
}

#ifdef USABLE_SIGIO

void
handle_input_available_signal (int sig)
{
  pending_signals = true;

  if (input_available_clear_time)
    *input_available_clear_time = make_timespec (0, 0);
}

static void
deliver_input_available_signal (int sig)
{
  deliver_process_signal (sig, handle_input_available_signal);
}
#endif /* USABLE_SIGIO */


/* User signal events.  */

struct user_signal_info
{
  /* Signal number.  */
  int sig;

  /* Name of the signal.  */
  char *name;

  /* Number of pending signals.  */
  int npending;

  struct user_signal_info *next;
};

/* List of user signals.  */
static struct user_signal_info *user_signals = NULL;

void
add_user_signal (int sig, const char *name)
{
  struct sigaction action;
  struct user_signal_info *p;

  for (p = user_signals; p; p = p->next)
    if (p->sig == sig)
      /* Already added.  */
      return;

  p = xmalloc (sizeof *p);
  p->sig = sig;
  p->name = xstrdup (name);
  p->npending = 0;
  p->next = user_signals;
  user_signals = p;

  emacs_sigaction_init (&action, deliver_user_signal);
  sigaction (sig, &action, 0);
}

static void
handle_user_signal (int sig)
{
  struct user_signal_info *p;
  const char *special_event_name = NULL;

  if (SYMBOLP (Vdebug_on_event))
    special_event_name = SSDATA (SYMBOL_NAME (Vdebug_on_event));

  for (p = user_signals; p; p = p->next)
    if (p->sig == sig)
      {
        if (special_event_name
	    && strcmp (special_event_name, p->name) == 0)
          {
            /* Enter the debugger in many ways.  */
            debug_on_next_call = true;
            debug_on_quit = true;
            Vquit_flag = Qt;
            Vinhibit_quit = Qnil;

            /* Eat the event.  */
            break;
          }

	p->npending++;
#ifdef USABLE_SIGIO
	if (interrupt_input)
	  handle_input_available_signal (sig);
	else
#endif
	  {
	    /* Tell wait_reading_process_output that it needs to wake
	       up and look around.  */
	    if (input_available_clear_time)
	      *input_available_clear_time = make_timespec (0, 0);
	  }
	break;
      }
}

static void
deliver_user_signal (int sig)
{
  deliver_process_signal (sig, handle_user_signal);
}

static char *
find_user_signal_name (int sig)
{
  struct user_signal_info *p;

  for (p = user_signals; p; p = p->next)
    if (p->sig == sig)
      return p->name;

  return NULL;
}

static void
store_user_signal_events (void)
{
  struct user_signal_info *p;
  struct input_event buf;
  bool buf_initialized = false;

  for (p = user_signals; p; p = p->next)
    if (p->npending > 0)
      {
	if (! buf_initialized)
	  {
	    memset (&buf, 0, sizeof buf);
	    buf.kind = USER_SIGNAL_EVENT;
	    buf.frame_or_window = selected_frame;
	    buf_initialized = true;
	  }

	do
	  {
	    buf.code = p->sig;
	    kbd_buffer_store_event (&buf);
	    p->npending--;
	  }
	while (p->npending > 0);
      }
}


static void menu_bar_item (Lisp_Object, Lisp_Object, Lisp_Object, void *);
static Lisp_Object menu_bar_one_keymap_changed_items;

/* These variables hold the vector under construction within
   menu_bar_items and its subroutines, and the current index
   for storing into that vector.  */
static Lisp_Object menu_bar_items_vector;
static int menu_bar_items_index;


static const char *separator_names[] = {
  "space",
  "no-line",
  "single-line",
  "double-line",
  "single-dashed-line",
  "double-dashed-line",
  "shadow-etched-in",
  "shadow-etched-out",
  "shadow-etched-in-dash",
  "shadow-etched-out-dash",
  "shadow-double-etched-in",
  "shadow-double-etched-out",
  "shadow-double-etched-in-dash",
  "shadow-double-etched-out-dash",
  0,
};

/* Return true if LABEL specifies a separator.  */

bool
menu_separator_name_p (const char *label)
{
  if (!label)
    return 0;
  else if (strnlen (label, 4) == 4
	   && memcmp (label, "--", 2) == 0
	   && label[2] != '-')
    {
      int i;
      label += 2;
      for (i = 0; separator_names[i]; ++i)
	if (strcmp (label, separator_names[i]) == 0)
          return 1;
    }
  else
    {
      /* It's a separator if it contains only dashes.  */
      while (*label == '-')
	++label;
      return (*label == 0);
    }

  return 0;
}


/* Return a vector of menu items for a menu bar, appropriate
   to the current buffer.  Each item has three elements in the vector:
   KEY STRING MAPLIST.

   OLD is an old vector we can optionally reuse, or nil.  */

Lisp_Object
menu_bar_items (Lisp_Object old)
{
  /* The number of keymaps we're scanning right now, and the number of
     keymaps we have allocated space for.  */
  ptrdiff_t nmaps;

  /* maps[0..nmaps-1] are the prefix definitions of KEYBUF[0..t-1]
     in the current keymaps, or nil where it is not a prefix.  */
  Lisp_Object *maps;

  Lisp_Object mapsbuf[3];
  Lisp_Object def;

  ptrdiff_t mapno;
  Lisp_Object oquit;

  USE_SAFE_ALLOCA;

  /* In order to build the menus, we need to call the keymap
     accessors.  They all call maybe_quit.  But this function is called
     during redisplay, during which a quit is fatal.  So inhibit
     quitting while building the menus.
     We do this instead of specbind because (1) errors will clear it anyway
     and (2) this avoids risk of specpdl overflow.  */
  oquit = Vinhibit_quit;
  Vinhibit_quit = Qt;

  if (!NILP (old))
    menu_bar_items_vector = old;
  else
    menu_bar_items_vector = make_nil_vector (24);
  menu_bar_items_index = 0;

  /* Build our list of keymaps.
     If we recognize a function key and replace its escape sequence in
     keybuf with its symbol, or if the sequence starts with a mouse
     click and we need to switch buffers, we jump back here to rebuild
     the initial keymaps from the current buffer.  */
  {
    Lisp_Object *tmaps;

    /* Should overriding-terminal-local-map and overriding-local-map apply?  */
    if (!NILP (Voverriding_local_map_menu_flag)
	&& !NILP (Voverriding_local_map))
      {
	/* Yes, use them (if non-nil) as well as the global map.  */
	maps = mapsbuf;
	nmaps = 0;
	if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
	  maps[nmaps++] = KVAR (current_kboard, Voverriding_terminal_local_map);
	if (!NILP (Voverriding_local_map))
	  maps[nmaps++] = Voverriding_local_map;
      }
    else
      {
	/* No, so use major and minor mode keymaps and keymap property.
	   Note that menu-bar bindings in the local-map and keymap
	   properties may not work reliable, as they are only
	   recognized when the menu-bar (or mode-line) is updated,
	   which does not normally happen after every command.  */
	ptrdiff_t nminor = current_minor_maps (NULL, &tmaps);
	SAFE_NALLOCA (maps, 1, nminor + 4);
	nmaps = 0;
	Lisp_Object tem = KVAR (current_kboard, Voverriding_terminal_local_map);
	if (!NILP (tem) && !NILP (Voverriding_local_map_menu_flag))
	  maps[nmaps++] = tem;
	if (tem = get_local_map (PT, current_buffer, Qkeymap), !NILP (tem))
	  maps[nmaps++] = tem;
	if (nminor != 0)
	  {
	    memcpy (maps + nmaps, tmaps, nminor * sizeof (maps[0]));
	    nmaps += nminor;
	  }
	maps[nmaps++] = get_local_map (PT, current_buffer, Qlocal_map);
      }
    maps[nmaps++] = current_global_map;
  }

  /* Look up in each map the dummy prefix key `menu-bar'.  */

  for (mapno = nmaps - 1; mapno >= 0; mapno--)
    if (!NILP (maps[mapno]))
      {
	def = get_keymap (access_keymap (maps[mapno], Qmenu_bar, 1, 0, 1),
			  0, 1);
	if (CONSP (def))
	  {
	    menu_bar_one_keymap_changed_items = Qnil;
	    map_keymap_canonical (def, menu_bar_item, Qnil, NULL);
	  }
      }

  /* Move to the end those items that should be at the end.  */

  Lisp_Object tail = Vmenu_bar_final_items;
  FOR_EACH_TAIL (tail)
    {
      int end = menu_bar_items_index;

      for (int i = 0; i < end; i += 4)
	if (EQ (XCAR (tail), AREF (menu_bar_items_vector, i)))
	  {
	    Lisp_Object tem0, tem1, tem2, tem3;
	    /* Move the item at index I to the end,
	       shifting all the others forward.  */
	    tem0 = AREF (menu_bar_items_vector, i + 0);
	    tem1 = AREF (menu_bar_items_vector, i + 1);
	    tem2 = AREF (menu_bar_items_vector, i + 2);
	    tem3 = AREF (menu_bar_items_vector, i + 3);
	    if (end > i + 4)
	      memmove (aref_addr (menu_bar_items_vector, i),
		       aref_addr (menu_bar_items_vector, i + 4),
		       (end - i - 4) * word_size);
	    ASET (menu_bar_items_vector, end - 4, tem0);
	    ASET (menu_bar_items_vector, end - 3, tem1);
	    ASET (menu_bar_items_vector, end - 2, tem2);
	    ASET (menu_bar_items_vector, end - 1, tem3);
	    break;
	  }
    }

  /* Add nil, nil, nil, nil at the end.  */
  {
    int i = menu_bar_items_index;
    if (i + 4 > ASIZE (menu_bar_items_vector))
      menu_bar_items_vector
	= larger_vector (menu_bar_items_vector, 4, -1);
    /* Add this item.  */
    ASET (menu_bar_items_vector, i, Qnil); i++;
    ASET (menu_bar_items_vector, i, Qnil); i++;
    ASET (menu_bar_items_vector, i, Qnil); i++;
    ASET (menu_bar_items_vector, i, Qnil); i++;
    menu_bar_items_index = i;
  }

  Vinhibit_quit = oquit;
  SAFE_FREE ();
  return menu_bar_items_vector;
}

/* Add one item to menu_bar_items_vector, for KEY, ITEM_STRING and DEF.
   If there's already an item for KEY, add this DEF to it.  */

Lisp_Object item_properties;

static void
menu_bar_item (Lisp_Object key, Lisp_Object item, Lisp_Object dummy1, void *dummy2)
{
  int i;
  bool parsed;
  Lisp_Object tem;

  if (EQ (item, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made menu bar item.  */

      for (i = 0; i < menu_bar_items_index; i += 4)
	if (EQ (key, AREF (menu_bar_items_vector, i)))
	  {
	    if (menu_bar_items_index > i + 4)
	      memmove (aref_addr (menu_bar_items_vector, i),
		       aref_addr (menu_bar_items_vector, i + 4),
		       (menu_bar_items_index - i - 4) * word_size);
	    menu_bar_items_index -= 4;
	  }
    }

  /* If this keymap has already contributed to this KEY,
     don't contribute to it a second time.  */
  tem = Fmemq (key, menu_bar_one_keymap_changed_items);
  if (!NILP (tem) || NILP (item))
    return;

  menu_bar_one_keymap_changed_items
    = Fcons (key, menu_bar_one_keymap_changed_items);

  /* We add to menu_bar_one_keymap_changed_items before doing the
     parse_menu_item, so that if it turns out it wasn't a menu item,
     it still correctly hides any further menu item.  */
  parsed = parse_menu_item (item, 1);
  if (!parsed)
    return;

  item = AREF (item_properties, ITEM_PROPERTY_DEF);

  /* Find any existing item for this KEY.  */
  for (i = 0; i < menu_bar_items_index; i += 4)
    if (EQ (key, AREF (menu_bar_items_vector, i)))
      break;

  /* If we did not find this KEY, add it at the end.  */
  if (i == menu_bar_items_index)
    {
      /* If vector is too small, get a bigger one.  */
      if (i + 4 > ASIZE (menu_bar_items_vector))
	menu_bar_items_vector = larger_vector (menu_bar_items_vector, 4, -1);
      /* Add this item.  */
      ASET (menu_bar_items_vector, i, key); i++;
      ASET (menu_bar_items_vector, i,
	    AREF (item_properties, ITEM_PROPERTY_NAME)); i++;
      ASET (menu_bar_items_vector, i, list1 (item)); i++;
      ASET (menu_bar_items_vector, i, make_fixnum (0)); i++;
      menu_bar_items_index = i;
    }
  /* We did find an item for this KEY.  Add ITEM to its list of maps.  */
  else
    {
      Lisp_Object old;
      old = AREF (menu_bar_items_vector, i + 2);
      /* If the new and the old items are not both keymaps,
	 the lookup will only find `item'.  */
      item = Fcons (item, KEYMAPP (item) && KEYMAPP (XCAR (old)) ? old : Qnil);
      ASET (menu_bar_items_vector, i + 2, item);
    }
}

 /* This is used as the handler when calling menu_item_eval_property.  */
static Lisp_Object
menu_item_eval_property_1 (Lisp_Object arg)
{
  /* If we got a quit from within the menu computation,
     quit all the way out of it.  This takes care of C-] in the debugger.  */
  if (CONSP (arg) && EQ (XCAR (arg), Qquit))
    quit ();

  return Qnil;
}

static Lisp_Object
eval_dyn (Lisp_Object form)
{
  return Feval (form, Qnil);
}

/* Evaluate an expression and return the result (or nil if something
   went wrong).  Used to evaluate dynamic parts of menu items.  */
Lisp_Object
menu_item_eval_property (Lisp_Object sexpr)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object val;
  specbind (Qinhibit_redisplay, Qt);
  val = internal_condition_case_1 (eval_dyn, sexpr, Qerror,
				   menu_item_eval_property_1);
  return unbind_to (count, val);
}

/* This function parses a menu item and leaves the result in the
   vector item_properties.
   ITEM is a key binding, a possible menu item.
   INMENUBAR is > 0 when this is considered for an entry in a menu bar
   top level.
   INMENUBAR is < 0 when this is considered for an entry in a keyboard menu.
   parse_menu_item returns true if the item is a menu item and false
   otherwise.  */

bool
parse_menu_item (Lisp_Object item, int inmenubar)
{
  Lisp_Object def, tem, item_string, start;
  Lisp_Object filter;
  Lisp_Object keyhint;
  int i;

  filter = Qnil;
  keyhint = Qnil;

  if (!CONSP (item))
    return 0;

  /* Create item_properties vector if necessary.  */
  if (NILP (item_properties))
    item_properties = make_nil_vector (ITEM_PROPERTY_ENABLE + 1);

  /* Initialize optional entries.  */
  for (i = ITEM_PROPERTY_DEF; i < ITEM_PROPERTY_ENABLE; i++)
    ASET (item_properties, i, Qnil);
  ASET (item_properties, ITEM_PROPERTY_ENABLE, Qt);

  /* Save the item here to protect it from GC.  */
  ASET (item_properties, ITEM_PROPERTY_ITEM, item);

  item_string = XCAR (item);

  start = item;
  item = XCDR (item);
  if (STRINGP (item_string))
    {
      /* Old format menu item.  */
      ASET (item_properties, ITEM_PROPERTY_NAME, item_string);

      /* Maybe help string.  */
      if (CONSP (item) && STRINGP (XCAR (item)))
	{
	  ASET (item_properties, ITEM_PROPERTY_HELP,
		help_echo_substitute_command_keys (XCAR (item)));
	  start = item;
	  item = XCDR (item);
	}

      /* Maybe an obsolete key binding cache.  */
      if (CONSP (item) && CONSP (XCAR (item))
	  && (NILP (XCAR (XCAR (item)))
	      || VECTORP (XCAR (XCAR (item)))))
	item = XCDR (item);

      /* This is the real definition--the function to run.  */
      ASET (item_properties, ITEM_PROPERTY_DEF, item);

      /* Get enable property, if any.  */
      if (SYMBOLP (item))
	{
	  tem = Fget (item, Qmenu_enable);
	  if (!NILP (Venable_disabled_menus_and_buttons))
	    ASET (item_properties, ITEM_PROPERTY_ENABLE, Qt);
	  else if (!NILP (tem))
	    ASET (item_properties, ITEM_PROPERTY_ENABLE, tem);
	}
    }
  else if (EQ (item_string, Qmenu_item) && CONSP (item))
    {
      /* New format menu item.  */
      ASET (item_properties, ITEM_PROPERTY_NAME, XCAR (item));
      start = XCDR (item);
      if (CONSP (start))
	{
	  /* We have a real binding.  */
	  ASET (item_properties, ITEM_PROPERTY_DEF, XCAR (start));

	  item = XCDR (start);
	  /* Is there an obsolete cache list with key equivalences.  */
	  if (CONSP (item) && CONSP (XCAR (item)))
	    item = XCDR (item);

	  /* Parse properties.  */
	  FOR_EACH_TAIL (item)
	    {
	      tem = XCAR (item);
	      item = XCDR (item);
	      if (!CONSP (item))
		break;

	      if (EQ (tem, QCenable))
		{
		  if (!NILP (Venable_disabled_menus_and_buttons))
		    ASET (item_properties, ITEM_PROPERTY_ENABLE, Qt);
		  else
		    ASET (item_properties, ITEM_PROPERTY_ENABLE, XCAR (item));
		}
	      else if (EQ (tem, QCvisible))
		{
		  /* If got a visible property and that evaluates to nil
		     then ignore this item.  */
		  tem = menu_item_eval_property (XCAR (item));
		  if (NILP (tem))
		    return 0;
	 	}
	      else if (EQ (tem, QChelp))
		{
		  Lisp_Object help = XCAR (item);
		  if (STRINGP (help))
		    help = help_echo_substitute_command_keys (help);
		  ASET (item_properties, ITEM_PROPERTY_HELP, help);
		}
	      else if (EQ (tem, QCfilter))
		filter = item;
	      else if (EQ (tem, QCkey_sequence))
		{
		  tem = XCAR (item);
		  if (SYMBOLP (tem) || STRINGP (tem) || VECTORP (tem))
		    /* Be GC protected. Set keyhint to item instead of tem.  */
		    keyhint = item;
		}
	      else if (EQ (tem, QCkeys))
		{
		  tem = XCAR (item);
		  if (CONSP (tem) || STRINGP (tem))
		    ASET (item_properties, ITEM_PROPERTY_KEYEQ, tem);
		}
	      else if (EQ (tem, QCbutton) && CONSP (XCAR (item)))
		{
		  Lisp_Object type;
		  tem = XCAR (item);
		  type = XCAR (tem);
		  if (EQ (type, QCtoggle) || EQ (type, QCradio))
		    {
		      ASET (item_properties, ITEM_PROPERTY_SELECTED,
			    XCDR (tem));
		      ASET (item_properties, ITEM_PROPERTY_TYPE, type);
		    }
		}
	    }
	}
      else if (inmenubar || !NILP (start))
	return 0;
    }
  else
    return 0;			/* not a menu item */

  /* If item string is not a string, evaluate it to get string.
     If we don't get a string, skip this item.  */
  item_string = AREF (item_properties, ITEM_PROPERTY_NAME);
  if (!(STRINGP (item_string)))
    {
      item_string = menu_item_eval_property (item_string);
      if (!STRINGP (item_string))
	return 0;
      ASET (item_properties, ITEM_PROPERTY_NAME, item_string);
    }

  /* If got a filter apply it on definition.  */
  def = AREF (item_properties, ITEM_PROPERTY_DEF);
  if (!NILP (filter))
    {
      def = menu_item_eval_property (list2 (XCAR (filter),
					    list2 (Qquote, def)));

      ASET (item_properties, ITEM_PROPERTY_DEF, def);
    }

  /* Enable or disable selection of item.  */
  tem = AREF (item_properties, ITEM_PROPERTY_ENABLE);
  if (!EQ (tem, Qt))
    {
      tem = menu_item_eval_property (tem);
      if (inmenubar && NILP (tem))
	return 0;		/* Ignore disabled items in menu bar.  */
      ASET (item_properties, ITEM_PROPERTY_ENABLE, tem);
    }

  /* If we got no definition, this item is just unselectable text which
     is OK in a submenu but not in the menubar.  */
  if (NILP (def))
    return (!inmenubar);

  /* See if this is a separate pane or a submenu.  */
  def = AREF (item_properties, ITEM_PROPERTY_DEF);
  tem = get_keymap (def, 0, 1);
  /* For a subkeymap, just record its details and exit.  */
  if (CONSP (tem))
    {
      ASET (item_properties, ITEM_PROPERTY_MAP, tem);
      ASET (item_properties, ITEM_PROPERTY_DEF, tem);
      return 1;
    }

  /* At the top level in the menu bar, do likewise for commands also.
     The menu bar does not display equivalent key bindings anyway.
     ITEM_PROPERTY_DEF is already set up properly.  */
  if (inmenubar > 0)
    return 1;

  { /* This is a command.  See if there is an equivalent key binding.  */
    Lisp_Object keyeq = AREF (item_properties, ITEM_PROPERTY_KEYEQ);
    AUTO_STRING (space_space, "  ");

    /* The previous code preferred :key-sequence to :keys, so we
       preserve this behavior.  */
    if (STRINGP (keyeq) && !CONSP (keyhint))
      keyeq = concat2 (space_space, Fsubstitute_command_keys (keyeq));
    else
      {
	Lisp_Object prefix = keyeq;
	Lisp_Object keys = Qnil;

	if (CONSP (prefix))
	  {
	    def = XCAR (prefix);
	    prefix = XCDR (prefix);
	  }
	else
	  def = AREF (item_properties, ITEM_PROPERTY_DEF);

	if (CONSP (keyhint) && !NILP (XCAR (keyhint)))
	  {
	    keys = XCAR (keyhint);
	    tem = Fkey_binding (keys, Qnil, Qnil, Qnil);

	    /* We have a suggested key.  Is it bound to the command?  */
	    if (NILP (tem)
		|| (!EQ (tem, def)
		    /* If the command is an alias for another
		       (such as lmenu.el set it up), check if the
		       original command matches the cached command.  */
		    && !(SYMBOLP (def)
			 && EQ (tem, XSYMBOL (def)->u.s.function))))
	      keys = Qnil;
	  }

	if (NILP (keys))
	  keys = Fwhere_is_internal (def, Qnil, Qt, Qnil, Qnil);

	if (!NILP (keys))
	  {
	    tem = Fkey_description (keys, Qnil);
	    if (CONSP (prefix))
	      {
		if (STRINGP (XCAR (prefix)))
		  tem = concat2 (XCAR (prefix), tem);
		if (STRINGP (XCDR (prefix)))
		  tem = concat2 (tem, XCDR (prefix));
	      }
	    keyeq = concat2 (space_space, tem);
	  }
	else
	  keyeq = Qnil;
      }

    /* If we have an equivalent key binding, use that.  */
    ASET (item_properties, ITEM_PROPERTY_KEYEQ, keyeq);
  }

  /* Include this when menu help is implemented.
  tem = XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP];
  if (!(NILP (tem) || STRINGP (tem)))
    {
      tem = menu_item_eval_property (tem);
      if (!STRINGP (tem))
	tem = Qnil;
      XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP] = tem;
    }
  */

  /* Handle radio buttons or toggle boxes.  */
  tem = AREF (item_properties, ITEM_PROPERTY_SELECTED);
  if (!NILP (tem))
    ASET (item_properties, ITEM_PROPERTY_SELECTED,
	  menu_item_eval_property (tem));

  return 1;
}



/***********************************************************************
			       Tab-bars
 ***********************************************************************/

/* A vector holding tab bar items while they are parsed in function
   tab_bar_items. Each item occupies TAB_BAR_ITEM_NSCLOTS elements
   in the vector.  */

static Lisp_Object tab_bar_items_vector;

/* A vector holding the result of parse_tab_bar_item.  Layout is like
   the one for a single item in tab_bar_items_vector.  */

static Lisp_Object tab_bar_item_properties;

/* Next free index in tab_bar_items_vector.  */

static int ntab_bar_items;

/* Function prototypes.  */

static void init_tab_bar_items (Lisp_Object);
static void process_tab_bar_item (Lisp_Object, Lisp_Object, Lisp_Object,
				   void *);
static bool parse_tab_bar_item (Lisp_Object, Lisp_Object);
static void append_tab_bar_item (void);


/* Return a vector of tab bar items for keymaps currently in effect.
   Reuse vector REUSE if non-nil.  Return in *NITEMS the number of
   tab bar items found.  */

Lisp_Object
tab_bar_items (Lisp_Object reuse, int *nitems)
{
  Lisp_Object *maps;
  Lisp_Object mapsbuf[3];
  ptrdiff_t nmaps, i;
  Lisp_Object oquit;
  Lisp_Object *tmaps;
  USE_SAFE_ALLOCA;

  *nitems = 0;

  /* In order to build the menus, we need to call the keymap
     accessors.  They all call maybe_quit.  But this function is called
     during redisplay, during which a quit is fatal.  So inhibit
     quitting while building the menus.  We do this instead of
     specbind because (1) errors will clear it anyway and (2) this
     avoids risk of specpdl overflow.  */
  oquit = Vinhibit_quit;
  Vinhibit_quit = Qt;

  /* Initialize tab_bar_items_vector and protect it from GC.  */
  init_tab_bar_items (reuse);

  /* Build list of keymaps in maps.  Set nmaps to the number of maps
     to process.  */

  /* Should overriding-terminal-local-map and overriding-local-map apply?  */
  if (!NILP (Voverriding_local_map_menu_flag)
      && !NILP (Voverriding_local_map))
    {
      /* Yes, use them (if non-nil) as well as the global map.  */
      maps = mapsbuf;
      nmaps = 0;
      if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
	maps[nmaps++] = KVAR (current_kboard, Voverriding_terminal_local_map);
      if (!NILP (Voverriding_local_map))
	maps[nmaps++] = Voverriding_local_map;
    }
  else
    {
      /* No, so use major and minor mode keymaps and keymap property.
	 Note that tab-bar bindings in the local-map and keymap
	 properties may not work reliably, as they are only
	 recognized when the tab-bar (or mode-line) is updated,
	 which does not normally happen after every command.  */
      ptrdiff_t nminor = current_minor_maps (NULL, &tmaps);
      SAFE_NALLOCA (maps, 1, nminor + 4);
      nmaps = 0;
      Lisp_Object tem = KVAR (current_kboard, Voverriding_terminal_local_map);
      if (!NILP (tem) && !NILP (Voverriding_local_map_menu_flag))
	maps[nmaps++] = tem;
      if (tem = get_local_map (PT, current_buffer, Qkeymap), !NILP (tem))
	maps[nmaps++] = tem;
      if (nminor != 0)
	{
	  memcpy (maps + nmaps, tmaps, nminor * sizeof (maps[0]));
	  nmaps += nminor;
	}
      maps[nmaps++] = get_local_map (PT, current_buffer, Qlocal_map);
    }

  /* Add global keymap at the end.  */
  maps[nmaps++] = current_global_map;

  /* Process maps in reverse order and look up in each map the prefix
     key `tab-bar'.  */
  for (i = nmaps - 1; i >= 0; --i)
    if (!NILP (maps[i]))
      {
	Lisp_Object keymap;

	keymap = get_keymap (access_keymap (maps[i], Qtab_bar, 1, 0, 1), 0, 1);
	if (CONSP (keymap))
	  map_keymap (keymap, process_tab_bar_item, Qnil, NULL, 1);
      }

  Vinhibit_quit = oquit;
  *nitems = ntab_bar_items / TAB_BAR_ITEM_NSLOTS;
  SAFE_FREE ();
  return tab_bar_items_vector;
}


/* Process the definition of KEY which is DEF.  */

static void
process_tab_bar_item (Lisp_Object key, Lisp_Object def, Lisp_Object data, void *args)
{
  int i;

  if (EQ (def, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made item.  */
      for (i = 0; i < ntab_bar_items; i += TAB_BAR_ITEM_NSLOTS)
	{
	  Lisp_Object *v = XVECTOR (tab_bar_items_vector)->contents + i;

	  if (EQ (key, v[TAB_BAR_ITEM_KEY]))
	    {
	      if (ntab_bar_items > i + TAB_BAR_ITEM_NSLOTS)
		memmove (v, v + TAB_BAR_ITEM_NSLOTS,
			 ((ntab_bar_items - i - TAB_BAR_ITEM_NSLOTS)
			  * word_size));
	      ntab_bar_items -= TAB_BAR_ITEM_NSLOTS;
	      break;
	    }
	}
    }
  else if (parse_tab_bar_item (key, def))
    /* Append a new tab bar item to tab_bar_items_vector.  Accept
       more than one definition for the same key.  */
    append_tab_bar_item ();
}

/* Access slot with index IDX of vector tab_bar_item_properties.  */
#define PROP(IDX) AREF (tab_bar_item_properties, (IDX))
static void
set_prop_tab_bar (ptrdiff_t idx, Lisp_Object val)
{
  ASET (tab_bar_item_properties, idx, val);
}


/* Parse a tab bar item specification ITEM for key KEY and return the
   result in tab_bar_item_properties.  Value is false if ITEM is
   invalid.

   ITEM is a list `(menu-item CAPTION BINDING PROPS...)'.

   CAPTION is the caption of the item,  If it's not a string, it is
   evaluated to get a string.

   BINDING is the tab bar item's binding.  Tab-bar items with keymaps
   as binding are currently ignored.

   The following properties are recognized:

   - `:enable FORM'.

   FORM is evaluated and specifies whether the tab bar item is
   enabled or disabled.

   - `:visible FORM'

   FORM is evaluated and specifies whether the tab bar item is visible.

   - `:filter FUNCTION'

   FUNCTION is invoked with one parameter `(quote BINDING)'.  Its
   result is stored as the new binding.

   - `:button (TYPE SELECTED)'

   TYPE must be one of `:radio' or `:toggle'.  SELECTED is evaluated
   and specifies whether the button is selected (pressed) or not.

   - `:image IMAGES'

   IMAGES is either a single image specification or a vector of four
   image specifications.  See enum tab_bar_item_images.

   - `:help HELP-STRING'.

   Gives a help string to display for the tab bar item.

   - `:label LABEL-STRING'.

   A text label to show with the tab bar button if labels are enabled.  */

static bool
parse_tab_bar_item (Lisp_Object key, Lisp_Object item)
{
  Lisp_Object filter = Qnil;
  Lisp_Object caption;
  int i;

  /* Definition looks like `(menu-item CAPTION BINDING PROPS...)'.
     Rule out items that aren't lists, don't start with
     `menu-item' or whose rest following `tab-bar-item' is not a
     list.  */
  if (!CONSP (item))
    return 0;

  /* As an exception, allow old-style menu separators.  */
  if (STRINGP (XCAR (item)))
    item = list1 (XCAR (item));
  else if (!EQ (XCAR (item), Qmenu_item)
	   || (item = XCDR (item), !CONSP (item)))
    return 0;

  /* Create tab_bar_item_properties vector if necessary.  Reset it to
     defaults.  */
  if (VECTORP (tab_bar_item_properties))
    {
      for (i = 0; i < TAB_BAR_ITEM_NSLOTS; ++i)
	set_prop_tab_bar (i, Qnil);
    }
  else
    tab_bar_item_properties = make_nil_vector (TAB_BAR_ITEM_NSLOTS);

  /* Set defaults.  */
  set_prop_tab_bar (TAB_BAR_ITEM_KEY, key);
  set_prop_tab_bar (TAB_BAR_ITEM_ENABLED_P, Qt);

  /* Get the caption of the item.  If the caption is not a string,
     evaluate it to get a string.  If we don't get a string, skip this
     item.  */
  caption = XCAR (item);
  if (!STRINGP (caption))
    {
      caption = menu_item_eval_property (caption);
      if (!STRINGP (caption))
	return 0;
    }
  set_prop_tab_bar (TAB_BAR_ITEM_CAPTION, caption);

  /* If the rest following the caption is not a list, the menu item is
     either a separator, or invalid.  */
  item = XCDR (item);
  if (!CONSP (item))
    {
      if (menu_separator_name_p (SSDATA (caption)))
	{
	  set_prop_tab_bar (TAB_BAR_ITEM_ENABLED_P, Qnil);
	  set_prop_tab_bar (TAB_BAR_ITEM_SELECTED_P, Qnil);
	  set_prop_tab_bar (TAB_BAR_ITEM_CAPTION, Qnil);
	  return 1;
	}
      return 0;
    }

  /* Store the binding.  */
  set_prop_tab_bar (TAB_BAR_ITEM_BINDING, XCAR (item));
  item = XCDR (item);

  /* Ignore cached key binding, if any.  */
  if (CONSP (item) && CONSP (XCAR (item)))
    item = XCDR (item);

  /* Process the rest of the properties.  */
  FOR_EACH_TAIL (item)
    {
      Lisp_Object ikey = XCAR (item);
      item = XCDR (item);
      if (!CONSP (item))
	break;
      Lisp_Object value = XCAR (item);

      if (EQ (ikey, QCenable))
	{
	  /* `:enable FORM'.  */
	  if (!NILP (Venable_disabled_menus_and_buttons))
	    set_prop_tab_bar (TAB_BAR_ITEM_ENABLED_P, Qt);
	  else
	    set_prop_tab_bar (TAB_BAR_ITEM_ENABLED_P, value);
	}
      else if (EQ (ikey, QCvisible))
	{
	  /* `:visible FORM'.  If got a visible property and that
	     evaluates to nil then ignore this item.  */
	  if (NILP (menu_item_eval_property (value)))
	    return 0;
	}
      else if (EQ (ikey, QChelp))
        /* `:help HELP-STRING'.  */
        set_prop_tab_bar (TAB_BAR_ITEM_HELP, value);
      else if (EQ (ikey, QCfilter))
	/* ':filter FORM'.  */
	filter = value;
      else if (EQ (ikey, QCbutton) && CONSP (value))
	{
	  /* `:button (TYPE . SELECTED)'.  */
	  Lisp_Object type, selected;

	  type = XCAR (value);
	  selected = XCDR (value);
	  if (EQ (type, QCtoggle) || EQ (type, QCradio))
	    {
	      set_prop_tab_bar (TAB_BAR_ITEM_SELECTED_P, selected);
	    }
	}
    }

  /* If got a filter apply it on binding.  */
  if (!NILP (filter))
    set_prop_tab_bar (TAB_BAR_ITEM_BINDING,
	      (menu_item_eval_property
	       (list2 (filter,
		       list2 (Qquote,
			      PROP (TAB_BAR_ITEM_BINDING))))));

  /* See if the binding is a keymap.  Give up if it is.  */
  if (CONSP (get_keymap (PROP (TAB_BAR_ITEM_BINDING), 0, 1)))
    return 0;

  /* Enable or disable selection of item.  */
  if (!EQ (PROP (TAB_BAR_ITEM_ENABLED_P), Qt))
    set_prop_tab_bar (TAB_BAR_ITEM_ENABLED_P,
	      menu_item_eval_property (PROP (TAB_BAR_ITEM_ENABLED_P)));

  /* Handle radio buttons or toggle boxes.  */
  if (!NILP (PROP (TAB_BAR_ITEM_SELECTED_P)))
    set_prop_tab_bar (TAB_BAR_ITEM_SELECTED_P,
	      menu_item_eval_property (PROP (TAB_BAR_ITEM_SELECTED_P)));

  return 1;

#undef PROP
}


/* Initialize tab_bar_items_vector.  REUSE, if non-nil, is a vector
   that can be reused.  */

static void
init_tab_bar_items (Lisp_Object reuse)
{
  if (VECTORP (reuse))
    tab_bar_items_vector = reuse;
  else
    tab_bar_items_vector = make_nil_vector (64);
  ntab_bar_items = 0;
}


/* Append parsed tab bar item properties from
   tab_bar_item_properties */

static void
append_tab_bar_item (void)
{
  ptrdiff_t incr
    = (ntab_bar_items
       - (ASIZE (tab_bar_items_vector) - TAB_BAR_ITEM_NSLOTS));

  /* Enlarge tab_bar_items_vector if necessary.  */
  if (incr > 0)
    tab_bar_items_vector = larger_vector (tab_bar_items_vector, incr, -1);

  /* Append entries from tab_bar_item_properties to the end of
     tab_bar_items_vector.  */
  vcopy (tab_bar_items_vector, ntab_bar_items,
	 XVECTOR (tab_bar_item_properties)->contents, TAB_BAR_ITEM_NSLOTS);
  ntab_bar_items += TAB_BAR_ITEM_NSLOTS;
}





/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* A vector holding tool bar items while they are parsed in function
   tool_bar_items. Each item occupies TOOL_BAR_ITEM_NSCLOTS elements
   in the vector.  */

static Lisp_Object tool_bar_items_vector;

/* A vector holding the result of parse_tool_bar_item.  Layout is like
   the one for a single item in tool_bar_items_vector.  */

static Lisp_Object tool_bar_item_properties;

/* Next free index in tool_bar_items_vector.  */

static int ntool_bar_items;

/* Function prototypes.  */

static void init_tool_bar_items (Lisp_Object);
static void process_tool_bar_item (Lisp_Object, Lisp_Object, Lisp_Object,
				   void *);
static bool parse_tool_bar_item (Lisp_Object, Lisp_Object);
static void append_tool_bar_item (void);


/* Return a vector of tool bar items for keymaps currently in effect.
   Reuse vector REUSE if non-nil.  Return in *NITEMS the number of
   tool bar items found.  */

Lisp_Object
tool_bar_items (Lisp_Object reuse, int *nitems)
{
  Lisp_Object *maps;
  Lisp_Object mapsbuf[3];
  ptrdiff_t nmaps, i;
  Lisp_Object oquit;
  Lisp_Object *tmaps;
  USE_SAFE_ALLOCA;

  *nitems = 0;

  /* In order to build the menus, we need to call the keymap
     accessors.  They all call maybe_quit.  But this function is called
     during redisplay, during which a quit is fatal.  So inhibit
     quitting while building the menus.  We do this instead of
     specbind because (1) errors will clear it anyway and (2) this
     avoids risk of specpdl overflow.  */
  oquit = Vinhibit_quit;
  Vinhibit_quit = Qt;

  /* Initialize tool_bar_items_vector and protect it from GC.  */
  init_tool_bar_items (reuse);

  /* Build list of keymaps in maps.  Set nmaps to the number of maps
     to process.  */

  /* Should overriding-terminal-local-map and overriding-local-map apply?  */
  if (!NILP (Voverriding_local_map_menu_flag)
      && !NILP (Voverriding_local_map))
    {
      /* Yes, use them (if non-nil) as well as the global map.  */
      maps = mapsbuf;
      nmaps = 0;
      if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
	maps[nmaps++] = KVAR (current_kboard, Voverriding_terminal_local_map);
      if (!NILP (Voverriding_local_map))
	maps[nmaps++] = Voverriding_local_map;
    }
  else
    {
      /* No, so use major and minor mode keymaps and keymap property.
	 Note that tool-bar bindings in the local-map and keymap
	 properties may not work reliably, as they are only
	 recognized when the tool-bar (or mode-line) is updated,
	 which does not normally happen after every command.  */
      ptrdiff_t nminor = current_minor_maps (NULL, &tmaps);
      SAFE_NALLOCA (maps, 1, nminor + 4);
      nmaps = 0;
      Lisp_Object tem = KVAR (current_kboard, Voverriding_terminal_local_map);
      if (!NILP (tem) && !NILP (Voverriding_local_map_menu_flag))
	maps[nmaps++] = tem;
      if (tem = get_local_map (PT, current_buffer, Qkeymap), !NILP (tem))
	maps[nmaps++] = tem;
      if (nminor != 0)
	{
	  memcpy (maps + nmaps, tmaps, nminor * sizeof (maps[0]));
	  nmaps += nminor;
	}
      maps[nmaps++] = get_local_map (PT, current_buffer, Qlocal_map);
    }

  /* Add global keymap at the end.  */
  maps[nmaps++] = current_global_map;

  /* Process maps in reverse order and look up in each map the prefix
     key `tool-bar'.  */
  for (i = nmaps - 1; i >= 0; --i)
    if (!NILP (maps[i]))
      {
	Lisp_Object keymap;

	keymap = get_keymap (access_keymap (maps[i], Qtool_bar, 1, 0, 1), 0, 1);
	if (CONSP (keymap))
	  map_keymap (keymap, process_tool_bar_item, Qnil, NULL, 1);
      }

  Vinhibit_quit = oquit;
  *nitems = ntool_bar_items / TOOL_BAR_ITEM_NSLOTS;
  SAFE_FREE ();
  return tool_bar_items_vector;
}


/* Process the definition of KEY which is DEF.  */

static void
process_tool_bar_item (Lisp_Object key, Lisp_Object def, Lisp_Object data, void *args)
{
  int i;

  if (EQ (def, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made item.  */
      for (i = 0; i < ntool_bar_items; i += TOOL_BAR_ITEM_NSLOTS)
	{
	  Lisp_Object *v = XVECTOR (tool_bar_items_vector)->contents + i;

	  if (EQ (key, v[TOOL_BAR_ITEM_KEY]))
	    {
	      if (ntool_bar_items > i + TOOL_BAR_ITEM_NSLOTS)
		memmove (v, v + TOOL_BAR_ITEM_NSLOTS,
			 ((ntool_bar_items - i - TOOL_BAR_ITEM_NSLOTS)
			  * word_size));
	      ntool_bar_items -= TOOL_BAR_ITEM_NSLOTS;
	      break;
	    }
	}
    }
  else if (parse_tool_bar_item (key, def))
    /* Append a new tool bar item to tool_bar_items_vector.  Accept
       more than one definition for the same key.  */
    append_tool_bar_item ();
}

/* Access slot with index IDX of vector tool_bar_item_properties.  */
#define PROP(IDX) AREF (tool_bar_item_properties, (IDX))
static void
set_prop (ptrdiff_t idx, Lisp_Object val)
{
  ASET (tool_bar_item_properties, idx, val);
}


/* Parse a tool bar item specification ITEM for key KEY and return the
   result in tool_bar_item_properties.  Value is false if ITEM is
   invalid.

   ITEM is a list `(menu-item CAPTION BINDING PROPS...)'.

   CAPTION is the caption of the item,  If it's not a string, it is
   evaluated to get a string.

   BINDING is the tool bar item's binding.  Tool-bar items with keymaps
   as binding are currently ignored.

   The following properties are recognized:

   - `:enable FORM'.

   FORM is evaluated and specifies whether the tool bar item is
   enabled or disabled.

   - `:visible FORM'

   FORM is evaluated and specifies whether the tool bar item is visible.

   - `:filter FUNCTION'

   FUNCTION is invoked with one parameter `(quote BINDING)'.  Its
   result is stored as the new binding.

   - `:button (TYPE SELECTED)'

   TYPE must be one of `:radio' or `:toggle'.  SELECTED is evaluated
   and specifies whether the button is selected (pressed) or not.

   - `:image IMAGES'

   IMAGES is either a single image specification or a vector of four
   image specifications.  See enum tool_bar_item_images.

   - `:help HELP-STRING'.

   Gives a help string to display for the tool bar item.

   - `:label LABEL-STRING'.

   A text label to show with the tool bar button if labels are enabled.  */

static bool
parse_tool_bar_item (Lisp_Object key, Lisp_Object item)
{
  Lisp_Object filter = Qnil;
  Lisp_Object caption;
  int i;
  bool have_label = false;

  /* Definition looks like `(menu-item CAPTION BINDING PROPS...)'.
     Rule out items that aren't lists, don't start with
     `menu-item' or whose rest following `tool-bar-item' is not a
     list.  */
  if (!CONSP (item))
    return 0;

  /* As an exception, allow old-style menu separators.  */
  if (STRINGP (XCAR (item)))
    item = list1 (XCAR (item));
  else if (!EQ (XCAR (item), Qmenu_item)
	   || (item = XCDR (item), !CONSP (item)))
    return 0;

  /* Create tool_bar_item_properties vector if necessary.  Reset it to
     defaults.  */
  if (VECTORP (tool_bar_item_properties))
    {
      for (i = 0; i < TOOL_BAR_ITEM_NSLOTS; ++i)
	set_prop (i, Qnil);
    }
  else
    tool_bar_item_properties = make_nil_vector (TOOL_BAR_ITEM_NSLOTS);

  /* Set defaults.  */
  set_prop (TOOL_BAR_ITEM_KEY, key);
  set_prop (TOOL_BAR_ITEM_ENABLED_P, Qt);

  /* Get the caption of the item.  If the caption is not a string,
     evaluate it to get a string.  If we don't get a string, skip this
     item.  */
  caption = XCAR (item);
  if (!STRINGP (caption))
    {
      caption = menu_item_eval_property (caption);
      if (!STRINGP (caption))
	return 0;
    }
  set_prop (TOOL_BAR_ITEM_CAPTION, caption);

  /* If the rest following the caption is not a list, the menu item is
     either a separator, or invalid.  */
  item = XCDR (item);
  if (!CONSP (item))
    {
      if (menu_separator_name_p (SSDATA (caption)))
	{
	  set_prop (TOOL_BAR_ITEM_TYPE, Qt);
#ifndef HAVE_EXT_TOOL_BAR
	  /* If we use build_desired_tool_bar_string to render the
	     tool bar, the separator is rendered as an image.  */
	  set_prop (TOOL_BAR_ITEM_IMAGES,
		    (menu_item_eval_property
		     (Vtool_bar_separator_image_expression)));
	  set_prop (TOOL_BAR_ITEM_ENABLED_P, Qnil);
	  set_prop (TOOL_BAR_ITEM_SELECTED_P, Qnil);
	  set_prop (TOOL_BAR_ITEM_CAPTION, Qnil);
#endif
	  return 1;
	}
      return 0;
    }

  /* Store the binding.  */
  set_prop (TOOL_BAR_ITEM_BINDING, XCAR (item));
  item = XCDR (item);

  /* Ignore cached key binding, if any.  */
  if (CONSP (item) && CONSP (XCAR (item)))
    item = XCDR (item);

  /* Process the rest of the properties.  */
  FOR_EACH_TAIL (item)
    {
      Lisp_Object ikey = XCAR (item);
      item = XCDR (item);
      if (!CONSP (item))
	break;
      Lisp_Object value = XCAR (item);

      if (EQ (ikey, QCenable))
	{
	  /* `:enable FORM'.  */
	  if (!NILP (Venable_disabled_menus_and_buttons))
	    set_prop (TOOL_BAR_ITEM_ENABLED_P, Qt);
	  else
	    set_prop (TOOL_BAR_ITEM_ENABLED_P, value);
	}
      else if (EQ (ikey, QCvisible))
	{
	  /* `:visible FORM'.  If got a visible property and that
	     evaluates to nil then ignore this item.  */
	  if (NILP (menu_item_eval_property (value)))
	    return 0;
	}
      else if (EQ (ikey, QChelp))
        /* `:help HELP-STRING'.  */
        set_prop (TOOL_BAR_ITEM_HELP, value);
      else if (EQ (ikey, QCvert_only))
        /* `:vert-only t/nil'.  */
        set_prop (TOOL_BAR_ITEM_VERT_ONLY, value);
      else if (EQ (ikey, QClabel))
        {
          const char *bad_label = "!!?GARBLED ITEM?!!";
          /* `:label LABEL-STRING'.  */
          set_prop (TOOL_BAR_ITEM_LABEL,
		    STRINGP (value) ? value : build_string (bad_label));
          have_label = true;
        }
      else if (EQ (ikey, QCfilter))
	/* ':filter FORM'.  */
	filter = value;
      else if (EQ (ikey, QCbutton) && CONSP (value))
	{
	  /* `:button (TYPE . SELECTED)'.  */
	  Lisp_Object type, selected;

	  type = XCAR (value);
	  selected = XCDR (value);
	  if (EQ (type, QCtoggle) || EQ (type, QCradio))
	    {
	      set_prop (TOOL_BAR_ITEM_SELECTED_P, selected);
	      set_prop (TOOL_BAR_ITEM_TYPE, type);
	    }
	}
      else if (EQ (ikey, QCimage)
	       && (CONSP (value)
		   || (VECTORP (value) && ASIZE (value) == 4)))
	/* Value is either a single image specification or a vector
	   of 4 such specifications for the different button states.  */
	set_prop (TOOL_BAR_ITEM_IMAGES, value);
      else if (EQ (ikey, QCrtl))
        /* ':rtl STRING' */
	set_prop (TOOL_BAR_ITEM_RTL_IMAGE, value);
    }


  if (!have_label)
    {
      /* Try to make one from caption and key.  */
      Lisp_Object tkey = PROP (TOOL_BAR_ITEM_KEY);
      Lisp_Object tcapt = PROP (TOOL_BAR_ITEM_CAPTION);
      const char *label = SYMBOLP (tkey) ? SSDATA (SYMBOL_NAME (tkey)) : "";
      const char *capt = STRINGP (tcapt) ? SSDATA (tcapt) : "";
      ptrdiff_t max_lbl_size =
	2 * max (0, min (tool_bar_max_label_size, STRING_BYTES_BOUND / 2)) + 1;
      char *buf = xmalloc (max_lbl_size);
      Lisp_Object new_lbl;
      ptrdiff_t caption_len = strnlen (capt, max_lbl_size);

      if (0 < caption_len && caption_len < max_lbl_size)
        {
          strcpy (buf, capt);
          while (caption_len > 0 && buf[caption_len - 1] == '.')
            caption_len--;
	  buf[caption_len] = '\0';
	  label = capt = buf;
        }

      ptrdiff_t label_len = strnlen (label, max_lbl_size);
      if (0 < label_len && label_len < max_lbl_size)
        {
          ptrdiff_t j;
          if (label != buf)
	    strcpy (buf, label);

          for (j = 0; buf[j] != '\0'; ++j)
	    if (buf[j] == '-')
	      buf[j] = ' ';
          label = buf;
        }
      else
	label = "";

      new_lbl = Fupcase_initials (build_string (label));
      if (SCHARS (new_lbl) <= tool_bar_max_label_size)
        set_prop (TOOL_BAR_ITEM_LABEL, new_lbl);
      else
        set_prop (TOOL_BAR_ITEM_LABEL, empty_unibyte_string);
      xfree (buf);
    }

  /* If got a filter apply it on binding.  */
  if (!NILP (filter))
    set_prop (TOOL_BAR_ITEM_BINDING,
	      (menu_item_eval_property
	       (list2 (filter,
		       list2 (Qquote,
			      PROP (TOOL_BAR_ITEM_BINDING))))));

  /* See if the binding is a keymap.  Give up if it is.  */
  if (CONSP (get_keymap (PROP (TOOL_BAR_ITEM_BINDING), 0, 1)))
    return 0;

  /* If there is a key binding, add it to the help, which will be
     displayed as a tooltip for this entry. */
  Lisp_Object binding = PROP (TOOL_BAR_ITEM_BINDING);
  Lisp_Object keys = Fwhere_is_internal (binding, Qnil, Qt, Qnil, Qnil);
  if (!NILP (keys))
    {
      AUTO_STRING (beg, "  (");
      AUTO_STRING (end, ")");
      Lisp_Object orig = PROP (TOOL_BAR_ITEM_HELP);
      Lisp_Object desc = Fkey_description (keys, Qnil);

      if (NILP (orig))
        orig = PROP (TOOL_BAR_ITEM_CAPTION);

      set_prop (TOOL_BAR_ITEM_HELP, CALLN (Fconcat, orig, beg, desc, end));
    }

  /* Enable or disable selection of item.  */
  if (!EQ (PROP (TOOL_BAR_ITEM_ENABLED_P), Qt))
    set_prop (TOOL_BAR_ITEM_ENABLED_P,
	      menu_item_eval_property (PROP (TOOL_BAR_ITEM_ENABLED_P)));

  /* Handle radio buttons or toggle boxes.  */
  if (!NILP (PROP (TOOL_BAR_ITEM_SELECTED_P)))
    set_prop (TOOL_BAR_ITEM_SELECTED_P,
	      menu_item_eval_property (PROP (TOOL_BAR_ITEM_SELECTED_P)));

  return 1;

#undef PROP
}


/* Initialize tool_bar_items_vector.  REUSE, if non-nil, is a vector
   that can be reused.  */

static void
init_tool_bar_items (Lisp_Object reuse)
{
  if (VECTORP (reuse))
    tool_bar_items_vector = reuse;
  else
    tool_bar_items_vector = make_nil_vector (64);
  ntool_bar_items = 0;
}


/* Append parsed tool bar item properties from
   tool_bar_item_properties */

static void
append_tool_bar_item (void)
{
  ptrdiff_t incr
    = (ntool_bar_items
       - (ASIZE (tool_bar_items_vector) - TOOL_BAR_ITEM_NSLOTS));

  /* Enlarge tool_bar_items_vector if necessary.  */
  if (incr > 0)
    tool_bar_items_vector = larger_vector (tool_bar_items_vector, incr, -1);

  /* Append entries from tool_bar_item_properties to the end of
     tool_bar_items_vector.  */
  vcopy (tool_bar_items_vector, ntool_bar_items,
	 XVECTOR (tool_bar_item_properties)->contents, TOOL_BAR_ITEM_NSLOTS);
  ntool_bar_items += TOOL_BAR_ITEM_NSLOTS;
}





/* Read a character using menus based on the keymap MAP.
   Return nil if there are no menus in the maps.
   Return t if we displayed a menu but the user rejected it.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence.

   If USED_MOUSE_MENU is non-null, set *USED_MOUSE_MENU to true
   if we used a mouse menu to read the input, or false otherwise.  If
   USED_MOUSE_MENU is null, don't dereference it.

   The prompting is done based on the prompt-string of the map
   and the strings associated with various map elements.

   This can be done with X menus or with menus put in the minibuf.
   These are done in different ways, depending on how the input will be read.
   Menus using X are done after auto-saving in read-char, getting the input
   event from Fx_popup_menu; menus using the minibuf use read_char recursively
   and do auto-saving in the inner call of read_char.  */

static Lisp_Object
read_char_x_menu_prompt (Lisp_Object map,
			 Lisp_Object prev_event, bool *used_mouse_menu)
{
  if (used_mouse_menu)
    *used_mouse_menu = false;

  /* Use local over global Menu maps.  */

  if (! menu_prompting)
    return Qnil;

  /* If we got to this point via a mouse click,
     use a real menu for mouse selection.  */
  if (EVENT_HAS_PARAMETERS (prev_event)
      && !EQ (XCAR (prev_event), Qmenu_bar)
      && !EQ (XCAR (prev_event), Qtab_bar)
      && !EQ (XCAR (prev_event), Qtool_bar))
    {
      /* Display the menu and get the selection.  */
      Lisp_Object value;

      value = x_popup_menu_1 (prev_event, get_keymap (map, 0, 1));
      if (CONSP (value))
	{
	  Lisp_Object tem;

	  record_menu_key (XCAR (value));

	  /* If we got multiple events, unread all but
	     the first.
	     There is no way to prevent those unread events
	     from showing up later in last_nonmenu_event.
	     So turn symbol and integer events into lists,
	     to indicate that they came from a mouse menu,
	     so that when present in last_nonmenu_event
	     they won't confuse things.  */
	  for (tem = XCDR (value); CONSP (tem); tem = XCDR (tem))
	    {
	      record_menu_key (XCAR (tem));
	      if (SYMBOLP (XCAR (tem))
		  || FIXNUMP (XCAR (tem)))
		XSETCAR (tem, Fcons (XCAR (tem), Qdisabled));
	    }

	  /* If we got more than one event, put all but the first
	     onto this list to be read later.
	     Return just the first event now.  */
	  Vunread_command_events
	    = nconc2 (XCDR (value), Vunread_command_events);
	  value = XCAR (value);
	}
      else if (NILP (value))
	value = Qt;
      if (used_mouse_menu)
	*used_mouse_menu = true;
      return value;
    }
  return Qnil ;
}

static Lisp_Object
read_char_minibuf_menu_prompt (int commandflag,
			       Lisp_Object map)
{
  Lisp_Object name;
  ptrdiff_t nlength;
  /* FIXME: Use the minibuffer's frame width.  */
  ptrdiff_t width = FRAME_COLS (SELECTED_FRAME ()) - 4;
  ptrdiff_t idx = -1;
  bool nobindings = true;
  Lisp_Object rest, vector;
  Lisp_Object prompt_strings = Qnil;

  vector = Qnil;

  if (! menu_prompting)
    return Qnil;

  map = get_keymap (map, 0, 1);
  name = Fkeymap_prompt (map);

  /* If we don't have any menus, just read a character normally.  */
  if (!STRINGP (name))
    return Qnil;

#define PUSH_C_STR(str, listvar) \
  listvar = Fcons (build_unibyte_string (str), listvar)

  /* Prompt string always starts with map's prompt, and a space.  */
  prompt_strings = Fcons (name, prompt_strings);
  PUSH_C_STR (": ", prompt_strings);
  nlength = SCHARS (name) + 2;

  rest = map;

  /* Present the documented bindings, a line at a time.  */
  while (1)
    {
      bool notfirst = false;
      Lisp_Object menu_strings = prompt_strings;
      ptrdiff_t i = nlength;
      Lisp_Object obj;
      Lisp_Object orig_defn_macro;

      /* Loop over elements of map.  */
      while (i < width)
	{
	  Lisp_Object elt;

	  /* FIXME: Use map_keymap to handle new keymap formats.  */

	  /* At end of map, wrap around if just starting,
	     or end this line if already have something on it.  */
	  if (NILP (rest))
	    {
	      if (notfirst || nobindings)
		break;
	      else
		rest = map;
	    }

	  /* Look at the next element of the map.  */
	  if (idx >= 0)
	    elt = AREF (vector, idx);
	  else
	    elt = Fcar_safe (rest);

	  if (idx < 0 && VECTORP (elt))
	    {
	      /* If we found a dense table in the keymap,
		 advanced past it, but start scanning its contents.  */
	      rest = Fcdr_safe (rest);
	      vector = elt;
	      idx = 0;
	    }
	  else
	    {
	      /* An ordinary element.  */
	      Lisp_Object event, tem;

	      if (idx < 0)
		{
		  event = Fcar_safe (elt); /* alist */
		  elt = Fcdr_safe (elt);
		}
	      else
		{
		  XSETINT (event, idx); /* vector */
		}

	      /* Ignore the element if it has no prompt string.  */
	      if (FIXNUMP (event) && parse_menu_item (elt, -1))
		{
		  /* True if the char to type matches the string.  */
		  bool char_matches;
		  Lisp_Object upcased_event, downcased_event;
		  Lisp_Object desc = Qnil;
		  Lisp_Object s
		    = AREF (item_properties, ITEM_PROPERTY_NAME);

		  upcased_event = Fupcase (event);
		  downcased_event = Fdowncase (event);
		  char_matches = (XFIXNUM (upcased_event) == SREF (s, 0)
				  || XFIXNUM (downcased_event) == SREF (s, 0));
		  if (! char_matches)
		    desc = Fsingle_key_description (event, Qnil);

#if 0  /* It is redundant to list the equivalent key bindings because
	  the prefix is what the user has already typed.  */
		  tem
		    = XVECTOR (item_properties)->contents[ITEM_PROPERTY_KEYEQ];
		  if (!NILP (tem))
		    /* Insert equivalent keybinding.  */
		    s = concat2 (s, tem);
#endif
		  tem
		    = AREF (item_properties, ITEM_PROPERTY_TYPE);
		  if (EQ (tem, QCradio) || EQ (tem, QCtoggle))
		    {
		      /* Insert button prefix.  */
		      Lisp_Object selected
			= AREF (item_properties, ITEM_PROPERTY_SELECTED);
		      AUTO_STRING (radio_yes, "(*) ");
		      AUTO_STRING (radio_no , "( ) ");
		      AUTO_STRING (check_yes, "[X] ");
		      AUTO_STRING (check_no , "[ ] ");
		      if (EQ (tem, QCradio))
			tem = NILP (selected) ? radio_yes : radio_no;
		      else
			tem = NILP (selected) ? check_yes : check_no;
		      s = concat2 (tem, s);
		    }


		  /* If we have room for the prompt string, add it to this line.
		     If this is the first on the line, always add it.  */
		  if ((SCHARS (s) + i + 2
		       + (char_matches ? 0 : SCHARS (desc) + 3))
		      < width
		      || !notfirst)
		    {
		      ptrdiff_t thiswidth;

		      /* Punctuate between strings.  */
		      if (notfirst)
			{
			  PUSH_C_STR (", ", menu_strings);
			  i += 2;
			}
		      notfirst = true;
		      nobindings = false;

		      /* If the char to type doesn't match the string's
			 first char, explicitly show what char to type.  */
		      if (! char_matches)
			{
			  /* Add as much of string as fits.  */
			  thiswidth = min (SCHARS (desc), width - i);
			  menu_strings
			    = Fcons (Fsubstring (desc, make_fixnum (0),
						 make_fixnum (thiswidth)),
				     menu_strings);
			  i += thiswidth;
			  PUSH_C_STR (" = ", menu_strings);
			  i += 3;
			}

		      /* Add as much of string as fits.  */
		      thiswidth = min (SCHARS (s), width - i);
		      menu_strings
			= Fcons (Fsubstring (s, make_fixnum (0),
					     make_fixnum (thiswidth)),
				 menu_strings);
		      i += thiswidth;
		    }
		  else
		    {
		      /* If this element does not fit, end the line now,
			 and save the element for the next line.  */
		      PUSH_C_STR ("...", menu_strings);
		      break;
		    }
		}

	      /* Move past this element.  */
	      if (idx >= 0 && idx + 1 >= ASIZE (vector))
		/* Handle reaching end of dense table.  */
		idx = -1;
	      if (idx >= 0)
		idx++;
	      else
		rest = Fcdr_safe (rest);
	    }
	}

      /* Prompt with that and read response.  */
      message3_nolog (apply1 (intern ("concat"), Fnreverse (menu_strings)));

      /* Make believe it's not a keyboard macro in case the help char
	 is pressed.  Help characters are not recorded because menu prompting
	 is not used on replay.  */
      orig_defn_macro = KVAR (current_kboard, defining_kbd_macro);
      kset_defining_kbd_macro (current_kboard, Qnil);
      do
	obj = read_char (commandflag, Qnil, Qt, 0, NULL);
      while (BUFFERP (obj));
      kset_defining_kbd_macro (current_kboard, orig_defn_macro);

      if (!FIXNUMP (obj) || XFIXNUM (obj) == -2
	  || (! EQ (obj, menu_prompt_more_char)
	      && (!FIXNUMP (menu_prompt_more_char)
		  || ! EQ (obj, make_fixnum (Ctl (XFIXNUM (menu_prompt_more_char)))))))
	{
	  if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
	    store_kbd_macro_char (obj);
	  return obj;
	}
      /* Help char - go round again.  */
    }
}

/* Reading key sequences.  */

static Lisp_Object
follow_key (Lisp_Object keymap, Lisp_Object key)
{
  return access_keymap (get_keymap (keymap, 0, 1),
			key, 1, 0, 1);
}

static Lisp_Object
active_maps (Lisp_Object first_event, Lisp_Object second_event)
{
  Lisp_Object position
    = EVENT_HAS_PARAMETERS (first_event) ? EVENT_START (first_event) : Qnil;
  /* The position of a click can be in the second event if the first event
     is a fake_prefixed_key like `header-line` or `mode-line`.  */
  if (SYMBOLP (first_event)
      && EVENT_HAS_PARAMETERS (second_event)
      && EQ (first_event, POSN_POSN (EVENT_START (second_event))))
    {
      eassert (NILP (position));
      position = EVENT_START (second_event);
    }
  return Fcons (Qkeymap, Fcurrent_active_maps (Qt, position));
}

/* Structure used to keep track of partial application of key remapping
   such as Vfunction_key_map and Vkey_translation_map.  */
typedef struct keyremap
{
  /* This is the map originally specified for this use.  */
  Lisp_Object parent;
  /* This is a submap reached by looking up, in PARENT,
     the events from START to END.  */
  Lisp_Object map;
  /* Positions [START, END) in the key sequence buffer
     are the key that we have scanned so far.
     Those events are the ones that we will replace
     if PARENT maps them into a key sequence.  */
  int start, end;
} keyremap;

/* Lookup KEY in MAP.
   MAP is a keymap mapping keys to key vectors or functions.
   If the mapping is a function and DO_FUNCALL is true,
   the function is called with PROMPT as parameter and its return
   value is used as the return value of this function (after checking
   that it is indeed a vector).  */

static Lisp_Object
access_keymap_keyremap (Lisp_Object map, Lisp_Object key, Lisp_Object prompt,
			bool do_funcall)
{
  Lisp_Object next;

  next = access_keymap (map, key, 1, 0, 1);

  /* Handle a symbol whose function definition is a keymap
     or an array.  */
  if (SYMBOLP (next) && !NILP (Ffboundp (next))
      && (ARRAYP (XSYMBOL (next)->u.s.function)
	  || KEYMAPP (XSYMBOL (next)->u.s.function)))
    next = Fautoload_do_load (XSYMBOL (next)->u.s.function, next, Qnil);

  /* If the keymap gives a function, not an
     array, then call the function with one arg and use
     its value instead.  */
  if (do_funcall && FUNCTIONP (next))
    {
      Lisp_Object tem;
      tem = next;

      next = call1 (next, prompt);
      /* If the function returned something invalid,
	 barf--don't ignore it.  */
      if (! (NILP (next) || VECTORP (next) || STRINGP (next)))
	error ("Function %s returns invalid key sequence",
	       SSDATA (SYMBOL_NAME (tem)));
    }
  return next;
}

/* Do one step of the key remapping used for function-key-map and
   key-translation-map:
   KEYBUF is the READ_KEY_ELTS-size buffer holding the input events.
   FKEY is a pointer to the keyremap structure to use.
   INPUT is the index of the last element in KEYBUF.
   DOIT if true says that the remapping can actually take place.
   DIFF is used to return the number of keys added/removed by the remapping.
   PARENT is the root of the keymap.
   PROMPT is the prompt to use if the remapping happens through a function.
   Return true if the remapping actually took place.  */

static bool
keyremap_step (Lisp_Object *keybuf, volatile keyremap *fkey,
	       int input, bool doit, int *diff, Lisp_Object prompt)
{
  Lisp_Object next, key;

  key = keybuf[fkey->end++];

  if (KEYMAPP (fkey->parent))
    next = access_keymap_keyremap (fkey->map, key, prompt, doit);
  else
    next = Qnil;

  /* If keybuf[fkey->start..fkey->end] is bound in the
     map and we're in a position to do the key remapping, replace it with
     the binding and restart with fkey->start at the end.  */
  if ((VECTORP (next) || STRINGP (next)) && doit)
    {
      int len = XFIXNAT (Flength (next));
      int i;

      *diff = len - (fkey->end - fkey->start);

      if (READ_KEY_ELTS - input <= *diff)
	error ("Key sequence too long");

      /* Shift the keys that follow fkey->end.  */
      if (*diff < 0)
	for (i = fkey->end; i < input; i++)
	  keybuf[i + *diff] = keybuf[i];
      else if (*diff > 0)
	for (i = input - 1; i >= fkey->end; i--)
	  keybuf[i + *diff] = keybuf[i];
      /* Overwrite the old keys with the new ones.  */
      for (i = 0; i < len; i++)
	keybuf[fkey->start + i]
	  = Faref (next, make_fixnum (i));

      fkey->start = fkey->end += *diff;
      fkey->map = fkey->parent;

      return 1;
    }

  fkey->map = get_keymap (next, 0, 1);

  /* If we no longer have a bound suffix, try a new position for
     fkey->start.  */
  if (!CONSP (fkey->map))
    {
      fkey->end = ++fkey->start;
      fkey->map = fkey->parent;
    }
  return 0;
}

static bool
test_undefined (Lisp_Object binding)
{
  return (NILP (binding)
	  || EQ (binding, Qundefined)
	  || (SYMBOLP (binding)
	      && EQ (Fcommand_remapping (binding, Qnil, Qnil), Qundefined)));
}

void init_raw_keybuf_count (void)
{
  raw_keybuf_count = 0;
}

/* Read a sequence of keys that ends with a non prefix character,
   storing it in KEYBUF, a buffer of size READ_KEY_ELTS.
   Prompt with PROMPT.
   Return the length of the key sequence stored.
   Return -1 if the user rejected a command menu.

   Echo starting immediately unless `prompt' is 0.

   If PREVENT_REDISPLAY is non-zero, avoid redisplay by calling
   read_char with a suitable COMMANDFLAG argument.

   Where a key sequence ends depends on the currently active keymaps.
   These include any minor mode keymaps active in the current buffer,
   the current buffer's local map, and the global map.

   If a key sequence has no other bindings, we check Vfunction_key_map
   to see if some trailing subsequence might be the beginning of a
   function key's sequence.  If so, we try to read the whole function
   key, and substitute its symbolic name into the key sequence.

   We ignore unbound `down-' mouse clicks.  We turn unbound `drag-' and
   `double-' events into similar click events, if that would make them
   bound.  We try to turn `triple-' events first into `double-' events,
   then into clicks.

   If we get a mouse click in a mode line, vertical divider, or other
   non-text area, we treat the click as if it were prefixed by the
   symbol denoting that area - `mode-line', `vertical-line', or
   whatever.

   If the sequence starts with a mouse click, we read the key sequence
   with respect to the buffer clicked on, not the current buffer.

   If the user switches frames in the midst of a key sequence, we put
   off the switch-frame event until later; the next call to
   read_char will return it.

   If FIX_CURRENT_BUFFER, we restore current_buffer
   from the selected window's buffer.  */

static int
read_key_sequence (Lisp_Object *keybuf, Lisp_Object prompt,
		   bool dont_downcase_last, bool can_return_switch_frame,
		   bool fix_current_buffer, bool prevent_redisplay)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  /* How many keys there are in the current key sequence.  */
  int t;

  /* The length of the echo buffer when we started reading, and
     the length of this_command_keys when we started reading.  */
  ptrdiff_t echo_start UNINIT;
  ptrdiff_t keys_start;

  Lisp_Object current_binding = Qnil;

  /* Index of the first key that has no binding.
     It is useless to try fkey.start larger than that.  */
  int first_unbound;

  /* If t < mock_input, then KEYBUF[t] should be read as the next
     input key.

     We use this to recover after recognizing a function key.  Once we
     realize that a suffix of the current key sequence is actually a
     function key's escape sequence, we replace the suffix with the
     function key's binding from Vfunction_key_map.  Now keybuf
     contains a new and different key sequence, so the echo area,
     this_command_keys, and the submaps and defs arrays are wrong.  In
     this situation, we set mock_input to t, set t to 0, and jump to
     restart_sequence; the loop will read keys from keybuf up until
     mock_input, thus rebuilding the state; and then it will resume
     reading characters from the keyboard.  */
  int mock_input = 0;

  /* Whether each event in the mocked input came from a mouse menu.  */
  bool used_mouse_menu_history[READ_KEY_ELTS] = {0};

  /* If the sequence is unbound in submaps[], then
     keybuf[fkey.start..fkey.end-1] is a prefix in Vfunction_key_map,
     and fkey.map is its binding.

     These might be > t, indicating that all function key scanning
     should hold off until t reaches them.  We do this when we've just
     recognized a function key, to avoid searching for the function
     key's again in Vfunction_key_map.  */
  keyremap fkey;

  /* Likewise, for key_translation_map and input-decode-map.  */
  keyremap keytran, indec;

  /* True if we are trying to map a key by changing an upper-case
     letter to lower case, or a shifted function key to an unshifted
     one.  */
  bool shift_translated = false;

  /* If we receive a `switch-frame' or `select-window' event in the middle of
     a key sequence, we put it off for later.
     While we're reading, we keep the event here.  */
  Lisp_Object delayed_switch_frame;

  Lisp_Object original_uppercase UNINIT;
  int original_uppercase_position = -1;

  /* Gets around Microsoft compiler limitations.  */
  bool dummyflag = false;

  struct buffer *starting_buffer;

  /* List of events for which a fake prefix key has been generated.  */
  Lisp_Object fake_prefixed_keys = Qnil;

  /* raw_keybuf_count is now initialized in (most of) the callers of
     read_key_sequence.  This is so that in a recursive call (for
     mouse menus) a spurious initialization doesn't erase the contents
     of raw_keybuf created by the outer call.  */
  /* raw_keybuf_count = 0; */

  delayed_switch_frame = Qnil;

  if (INTERACTIVE)
    {
      if (!NILP (prompt))
	{
	  /* Install the string PROMPT as the beginning of the string
	     of echoing, so that it serves as a prompt for the next
	     character.  */
	  kset_echo_prompt (current_kboard, prompt);
          /* FIXME: This use of echo_now doesn't look quite right and is ugly
             since it forces us to fiddle with current_kboard->immediate_echo
             before and after.  */
	  current_kboard->immediate_echo = false;
	  echo_now ();
          if (!echo_keystrokes_p ())
	    current_kboard->immediate_echo = false;
	}
      else if (cursor_in_echo_area /* FIXME: Not sure why we test this here,
                                      maybe we should just drop this test.  */
	       && echo_keystrokes_p ())
	/* This doesn't put in a dash if the echo buffer is empty, so
	   you don't always see a dash hanging out in the minibuffer.  */
	echo_dash ();
    }

  /* Record the initial state of the echo area and this_command_keys;
     we will need to restore them if we replay a key sequence.  */
  if (INTERACTIVE)
    echo_start = echo_length ();
  keys_start = this_command_key_count;
  this_single_command_key_start = keys_start;

  /* We jump here when we need to reinitialize fkey and keytran; this
     happens if we switch keyboards between rescans.  */
 replay_entire_sequence:

  indec.map = indec.parent = KVAR (current_kboard, Vinput_decode_map);
  fkey.map = fkey.parent = KVAR (current_kboard, Vlocal_function_key_map);
  keytran.map = keytran.parent = Vkey_translation_map;
  indec.start = indec.end = 0;
  fkey.start = fkey.end = 0;
  keytran.start = keytran.end = 0;

  /* We jump here when the key sequence has been thoroughly changed, and
     we need to rescan it starting from the beginning.  When we jump here,
     keybuf[0..mock_input] holds the sequence we should reread.  */
 replay_sequence:

  starting_buffer = current_buffer;
  first_unbound = READ_KEY_ELTS + 1;
  Lisp_Object first_event = mock_input > 0 ? keybuf[0] : Qnil;
  Lisp_Object second_event = mock_input > 1 ? keybuf[1] : Qnil;

  /* Build our list of keymaps.
     If we recognize a function key and replace its escape sequence in
     keybuf with its symbol, or if the sequence starts with a mouse
     click and we need to switch buffers, we jump back here to rebuild
     the initial keymaps from the current buffer.  */
  current_binding = active_maps (first_event, second_event);

  /* Start from the beginning in keybuf.  */
  t = 0;
  last_nonmenu_event = Qnil;

  /* These are no-ops the first time through, but if we restart, they
     revert the echo area and this_command_keys to their original state.  */
  this_command_key_count = keys_start;
  if (INTERACTIVE && t < mock_input)
    echo_truncate (echo_start);

  /* If the best binding for the current key sequence is a keymap, or
     we may be looking at a function key's escape sequence, keep on
     reading.  */
  while (!NILP (current_binding)
	 /* Keep reading as long as there's a prefix binding.  */
	 ? KEYMAPP (current_binding)
	 /* Don't return in the middle of a possible function key sequence,
	    if the only bindings we found were via case conversion.
	    Thus, if ESC O a has a function-key-map translation
	    and ESC o has a binding, don't return after ESC O,
	    so that we can translate ESC O plus the next character.  */
	 : (/* indec.start < t || fkey.start < t || */ keytran.start < t))
    {
      Lisp_Object key;
      bool used_mouse_menu = false;

      /* Where the last real key started.  If we need to throw away a
         key that has expanded into more than one element of keybuf
         (say, a mouse click on the mode line which is being treated
         as [mode-line (mouse-...)], then we backtrack to this point
         of keybuf.  */
      int last_real_key_start;

      /* These variables are analogous to echo_start and keys_start;
	 while those allow us to restart the entire key sequence,
	 echo_local_start and keys_local_start allow us to throw away
	 just one key.  */
      ptrdiff_t echo_local_start UNINIT;
      int keys_local_start;
      Lisp_Object new_binding;

      eassert (indec.end == t || (indec.end > t && indec.end <= mock_input));
      eassert (indec.start <= indec.end);
      eassert (fkey.start <= fkey.end);
      eassert (keytran.start <= keytran.end);
      /* key-translation-map is applied *after* function-key-map
	 which is itself applied *after* input-decode-map.  */
      eassert (fkey.end <= indec.start);
      eassert (keytran.end <= fkey.start);

      if (/* first_unbound < indec.start && first_unbound < fkey.start && */
	  first_unbound < keytran.start)
	{ /* The prefix upto first_unbound has no binding and has
	     no translation left to do either, so we know it's unbound.
	     If we don't stop now, we risk staying here indefinitely
	     (if the user keeps entering fkey or keytran prefixes
	     like C-c ESC ESC ESC ESC ...)  */
	  int i;
	  for (i = first_unbound + 1; i < t; i++)
	    keybuf[i - first_unbound - 1] = keybuf[i];
	  mock_input = t - first_unbound - 1;
	  indec.end = indec.start -= first_unbound + 1;
	  indec.map = indec.parent;
	  fkey.end = fkey.start -= first_unbound + 1;
	  fkey.map = fkey.parent;
	  keytran.end = keytran.start -= first_unbound + 1;
	  keytran.map = keytran.parent;
	  goto replay_sequence;
	}

      if (t >= READ_KEY_ELTS)
	error ("Key sequence too long");

      if (INTERACTIVE)
	echo_local_start = echo_length ();
      keys_local_start = this_command_key_count;

    replay_key:
      /* These are no-ops, unless we throw away a keystroke below and
	 jumped back up to replay_key; in that case, these restore the
	 variables to their original state, allowing us to replay the
	 loop.  */
      if (INTERACTIVE && t < mock_input)
	echo_truncate (echo_local_start);
      this_command_key_count = keys_local_start;

      /* By default, assume each event is "real".  */
      last_real_key_start = t;

      /* Does mock_input indicate that we are re-reading a key sequence?  */
      if (t < mock_input)
	{
	  key = keybuf[t];
	  add_command_key (key);
	  if (current_kboard->immediate_echo)
	    {
	      /* Set immediate_echo to false so as to force echo_now to
		 redisplay (it will set immediate_echo right back to true).  */
	      current_kboard->immediate_echo = false;
	      echo_now ();
	    }
	  used_mouse_menu = used_mouse_menu_history[t];
	}

      /* If not, we should actually read a character.  */
      else
	{
	  {
	    KBOARD *interrupted_kboard = current_kboard;
	    struct frame *interrupted_frame = SELECTED_FRAME ();
	    /* Calling read_char with COMMANDFLAG = -2 avoids
	       redisplay in read_char and its subroutines.  */
	    key = read_char (prevent_redisplay ? -2 : NILP (prompt),
		             current_binding, last_nonmenu_event,
                             &used_mouse_menu, NULL);
	    used_mouse_menu_history[t] = used_mouse_menu;
	    if ((FIXNUMP (key) && XFIXNUM (key) == -2) /* wrong_kboard_jmpbuf */
		/* When switching to a new tty (with a new keyboard),
		   read_char returns the new buffer, rather than -2
		   (Bug#5095).  This is because `terminal-init-xterm'
		   calls read-char, which eats the wrong_kboard_jmpbuf
		   return.  Any better way to fix this? -- cyd  */
		|| (interrupted_kboard != current_kboard))
	      {
		bool found = false;
		struct kboard *k;

		for (k = all_kboards; k; k = k->next_kboard)
		  if (k == interrupted_kboard)
		    found = true;

		if (!found)
		  {
		    /* Don't touch interrupted_kboard when it's been
		       deleted.  */
		    delayed_switch_frame = Qnil;
		    goto replay_entire_sequence;
		  }

		if (!NILP (delayed_switch_frame))
		  {
		    kset_kbd_queue
		      (interrupted_kboard,
		       Fcons (delayed_switch_frame,
			      KVAR (interrupted_kboard, kbd_queue)));
		    delayed_switch_frame = Qnil;
		  }

		while (t > 0)
		  kset_kbd_queue
		    (interrupted_kboard,
		     Fcons (keybuf[--t], KVAR (interrupted_kboard, kbd_queue)));

		/* If the side queue is non-empty, ensure it begins with a
		   switch-frame, so we'll replay it in the right context.  */
		if (CONSP (KVAR (interrupted_kboard, kbd_queue))
		    && (key = XCAR (KVAR (interrupted_kboard, kbd_queue)),
			!(EVENT_HAS_PARAMETERS (key)
			  && EQ (EVENT_HEAD_KIND (EVENT_HEAD (key)),
				 Qswitch_frame))))
		  {
		    Lisp_Object frame;
		    XSETFRAME (frame, interrupted_frame);
		    kset_kbd_queue
		      (interrupted_kboard,
		       Fcons (make_lispy_switch_frame (frame),
			      KVAR (interrupted_kboard, kbd_queue)));
		  }
                if (FIXNUMP (key) && XFIXNUM (key) != -2)
                  {
                    /* If interrupted while initializing terminal, we
                       need to replay the interrupting key.  See
                       Bug#5095 and Bug#37782.  */
                    mock_input = 1;
                    keybuf[0] = key;
                  }
                else
                  mock_input = 0;
		goto replay_entire_sequence;
	      }
	  }

	  /* read_char returns t when it shows a menu and the user rejects it.
	     Just return -1.  */
	  if (EQ (key, Qt))
	    {
	      unbind_to (count, Qnil);
	      return -1;
	    }

	  /* read_char returns -1 at the end of a macro.
	     Emacs 18 handles this by returning immediately with a
	     zero, so that's what we'll do.  */
	  if (FIXNUMP (key) && XFIXNUM (key) == -1)
	    {
	      t = 0;
	      /* The Microsoft C compiler can't handle the goto that
		 would go here.  */
	      dummyflag = true;
	      break;
	    }

	  /* If the current buffer has been changed from under us, the
	     keymap may have changed, so replay the sequence.  */
	  if (BUFFERP (key))
	    {
	      timer_resume_idle ();

	      mock_input = t;
	      /* Reset the current buffer from the selected window
		 in case something changed the former and not the latter.
		 This is to be more consistent with the behavior
		 of the command_loop_1.  */
	      if (fix_current_buffer)
		{
		  if (! FRAME_LIVE_P (XFRAME (selected_frame)))
		    Fkill_emacs (Qnil);
		  if (XBUFFER (XWINDOW (selected_window)->contents)
		      != current_buffer)
		    Fset_buffer (XWINDOW (selected_window)->contents);
		}

	      goto replay_sequence;
	    }

	  /* If we have a quit that was typed in another frame, and
	     quit_throw_to_read_char switched buffers,
	     replay to get the right keymap.  */
	  if (FIXNUMP (key)
	      && XFIXNUM (key) == quit_char
	      && current_buffer != starting_buffer)
	    {
	      GROW_RAW_KEYBUF;
	      ASET (raw_keybuf, raw_keybuf_count, key);
	      raw_keybuf_count++;
	      keybuf[t++] = key;
	      mock_input = t;
	      Vquit_flag = Qnil;
	      goto replay_sequence;
	    }

	  Vquit_flag = Qnil;

	  if (EVENT_HAS_PARAMETERS (key)
	      /* Either a `switch-frame' or a `select-window' event.  */
	      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (key)), Qswitch_frame))
	    {
	      /* If we're at the beginning of a key sequence, and the caller
		 says it's okay, go ahead and return this event.  If we're
		 in the midst of a key sequence, delay it until the end.  */
	      if (t > 0 || !can_return_switch_frame)
		{
		  delayed_switch_frame = key;
		  goto replay_key;
		}
	    }

	  if (NILP (first_event))
	    {
	      first_event = key;
	      /* Even if first_event does not specify a particular
		 window/position, it's important to recompute the maps here
		 since a long time might have passed since we entered
		 read_key_sequence, and a timer (or process-filter or
		 special-event-map, ...) might have switched the current buffer
		 or the selected window from under us in the mean time.  */
	      if (fix_current_buffer
		  && (XBUFFER (XWINDOW (selected_window)->contents)
		      != current_buffer))
		Fset_buffer (XWINDOW (selected_window)->contents);
	      current_binding = active_maps (first_event, Qnil);
	    }

	  GROW_RAW_KEYBUF;
	  ASET (raw_keybuf, raw_keybuf_count,
                /* Copy the event, in case it gets modified by side-effect
                   by some remapping function (bug#30955).  */
                CONSP (key) ? Fcopy_sequence (key) : key);
	  raw_keybuf_count++;
	}

      /* Clicks in non-text areas get prefixed by the symbol
	 in their CHAR-ADDRESS field.  For example, a click on
	 the mode line is prefixed by the symbol `mode-line'.

	 Furthermore, key sequences beginning with mouse clicks
	 are read using the keymaps of the buffer clicked on, not
	 the current buffer.  So we may have to switch the buffer
	 here.

	 When we turn one event into two events, we must make sure
	 that neither of the two looks like the original--so that,
	 if we replay the events, they won't be expanded again.
	 If not for this, such reexpansion could happen either here
	 or when user programs play with this-command-keys.  */
      if (EVENT_HAS_PARAMETERS (key))
	{
	  Lisp_Object kind = EVENT_HEAD_KIND (EVENT_HEAD (key));
	  if (EQ (kind, Qmouse_click))
	    {
	      Lisp_Object window = POSN_WINDOW (EVENT_START (key));
	      Lisp_Object posn = POSN_POSN (EVENT_START (key));

	      if (CONSP (posn)
		  || (!NILP (fake_prefixed_keys)
		      && !NILP (Fmemq (key, fake_prefixed_keys))))
		{
		  /* We're looking a second time at an event for which
		     we generated a fake prefix key.  Set
		     last_real_key_start appropriately.  */
		  if (t > 0)
		    last_real_key_start = t - 1;
		}

	      if (last_real_key_start == 0)
		{
		  /* Key sequences beginning with mouse clicks are
		     read using the keymaps in the buffer clicked on,
		     not the current buffer.  If we're at the
		     beginning of a key sequence, switch buffers.  */
		  if (WINDOWP (window)
		      && BUFFERP (XWINDOW (window)->contents)
		      && XBUFFER (XWINDOW (window)->contents) != current_buffer)
		    {
		      keybuf[t] = key;
		      mock_input = t + 1;

		      /* Arrange to go back to the original buffer once we're
			 done reading the key sequence.  Note that we can't
			 use save_excursion_{save,restore} here, because they
			 save point as well as the current buffer; we don't
			 want to save point, because redisplay may change it,
			 to accommodate a Fset_window_start or something.  We
			 don't want to do this at the top of the function,
			 because we may get input from a subprocess which
			 wants to change the selected window and stuff (say,
			 emacsclient).  */
		      record_unwind_current_buffer ();

		      if (! FRAME_LIVE_P (XFRAME (selected_frame)))
			Fkill_emacs (Qnil);
		      set_buffer_internal (XBUFFER (XWINDOW (window)->contents));
		      goto replay_sequence;
		    }
		}

	      /* Expand mode-line and scroll-bar events into two events:
		 use posn as a fake prefix key.  */
	      if (SYMBOLP (posn)
		  && (NILP (fake_prefixed_keys)
		      || NILP (Fmemq (key, fake_prefixed_keys))))
		{
		  if (READ_KEY_ELTS - t <= 1)
		    error ("Key sequence too long");

		  keybuf[t]     = posn;
		  keybuf[t + 1] = key;
		  mock_input    = t + 2;

		  /* Record that a fake prefix key has been generated
		     for KEY.  Don't modify the event; this would
		     prevent proper action when the event is pushed
		     back into unread-command-events.  */
		  fake_prefixed_keys = Fcons (key, fake_prefixed_keys);
		  goto replay_key;
		}
	    }
	  else if (CONSP (XCDR (key))
		   && CONSP (xevent_start (key))
		   && CONSP (XCDR (xevent_start (key))))
	    {
	      Lisp_Object posn;

	      posn = POSN_POSN (xevent_start (key));
	      /* Handle menu-bar events:
		 insert the dummy prefix event `menu-bar'.  */
	      if (EQ (posn, Qmenu_bar) || EQ (posn, Qtab_bar) || EQ (posn, Qtool_bar))
		{
		  if (READ_KEY_ELTS - t <= 1)
		    error ("Key sequence too long");
		  keybuf[t] = posn;
		  keybuf[t + 1] = key;

		  /* Zap the position in key, so we know that we've
		     expanded it, and don't try to do so again.  */
		  POSN_SET_POSN (xevent_start (key), list1 (posn));

		  mock_input = t + 2;
		  goto replay_sequence;
		}
	      else if (CONSP (posn))
		{
		  /* We're looking at the second event of a
		     sequence which we expanded before.  Set
		     last_real_key_start appropriately.  */
		  if (last_real_key_start == t && t > 0)
		    last_real_key_start = t - 1;
		}
	    }
	}

      /* We have finally decided that KEY is something we might want
	 to look up.  */
      new_binding = follow_key (current_binding, key);

      /* If KEY wasn't bound, we'll try some fallbacks.  */
      if (!NILP (new_binding))
	/* This is needed for the following scenario:
	   event 0: a down-event that gets dropped by calling replay_key.
	   event 1: some normal prefix like C-h.
	   After event 0, first_unbound is 0, after event 1 indec.start,
	   fkey.start, and keytran.start are all 1, so when we see that
	   C-h is bound, we need to update first_unbound.  */
	first_unbound = max (t + 1, first_unbound);
      else
	{
	  Lisp_Object head;

	  /* Remember the position to put an upper bound on indec.start.  */
	  first_unbound = min (t, first_unbound);

	  head = EVENT_HEAD (key);

	  if (SYMBOLP (head))
	    {
	      Lisp_Object breakdown;
	      int modifiers;

	      breakdown = parse_modifiers (head);
	      modifiers = XFIXNUM (XCAR (XCDR (breakdown)));
	      /* Attempt to reduce an unbound mouse event to a simpler
		 event that is bound:
		   Drags reduce to clicks.
		   Double-clicks reduce to clicks.
		   Triple-clicks reduce to double-clicks, then to clicks.
		   Up/Down-clicks are eliminated.
		   Double-downs reduce to downs, then are eliminated.
		   Triple-downs reduce to double-downs, then to downs,
		     then are eliminated.  */
	      if (modifiers & (up_modifier | down_modifier
			       | drag_modifier
			       | double_modifier | triple_modifier))
		{
		  while (modifiers & (up_modifier | down_modifier
				      | drag_modifier
				      | double_modifier | triple_modifier))
		    {
		      Lisp_Object new_head, new_click;
		      if (modifiers & triple_modifier)
			modifiers ^= (double_modifier | triple_modifier);
		      else if (modifiers & double_modifier)
			modifiers &= ~double_modifier;
		      else if (modifiers & drag_modifier)
			modifiers &= ~drag_modifier;
		      else
			{
			  /* Dispose of this `up/down' event by simply jumping
			     back to replay_key, to get another event.

			     Note that if this event came from mock input,
			     then just jumping back to replay_key will just
			     hand it to us again.  So we have to wipe out any
			     mock input.

			     We could delete keybuf[t] and shift everything
			     after that to the left by one spot, but we'd also
			     have to fix up any variable that points into
			     keybuf, and shifting isn't really necessary
			     anyway.

			     Adding prefixes for non-textual mouse clicks
			     creates two characters of mock input, and both
			     must be thrown away.  If we're only looking at
			     the prefix now, we can just jump back to
			     replay_key.  On the other hand, if we've already
			     processed the prefix, and now the actual click
			     itself is giving us trouble, then we've lost the
			     state of the keymaps we want to backtrack to, and
			     we need to replay the whole sequence to rebuild
			     it.

			     Beyond that, only function key expansion could
			     create more than two keys, but that should never
			     generate mouse events, so it's okay to zero
			     mock_input in that case too.

			     FIXME: The above paragraph seems just plain
			     wrong, if you consider things like
			     xterm-mouse-mode.  -stef

			     Isn't this just the most wonderful code ever?  */

			  /* If mock_input > t + 1, the above simplification
			     will actually end up dropping keys on the floor.
			     This is probably OK for now, but even
			     if mock_input <= t + 1, we need to adjust indec,
			     fkey, and keytran.
			     Typical case [header-line down-mouse-N]:
			     mock_input = 2, t = 1, fkey.end = 1,
			     last_real_key_start = 0.  */
			  if (indec.end > last_real_key_start)
			    {
			      indec.end = indec.start
				= min (last_real_key_start, indec.start);
			      indec.map = indec.parent;
			      if (fkey.end > last_real_key_start)
				{
				  fkey.end = fkey.start
				    = min (last_real_key_start, fkey.start);
				  fkey.map = fkey.parent;
				  if (keytran.end > last_real_key_start)
				    {
				      keytran.end = keytran.start
					= min (last_real_key_start, keytran.start);
				      keytran.map = keytran.parent;
				    }
				}
			    }
			  if (t == last_real_key_start)
			    {
			      mock_input = 0;
			      goto replay_key;
			    }
			  else
			    {
			      mock_input = last_real_key_start;
			      goto replay_sequence;
			    }
			}

		      new_head
			= apply_modifiers (modifiers, XCAR (breakdown));
		      new_click = list2 (new_head, EVENT_START (key));

		      /* Look for a binding for this new key.  */
		      new_binding = follow_key (current_binding, new_click);

		      /* If that click is bound, go for it.  */
		      if (!NILP (new_binding))
			{
			  current_binding = new_binding;
			  key = new_click;
			  break;
			}
		      /* Otherwise, we'll leave key set to the drag event.  */
		    }
		}
	    }
	}
      current_binding = new_binding;

      keybuf[t++] = key;
      /* Normally, last_nonmenu_event gets the previous key we read.
	 But when a mouse popup menu is being used,
	 we don't update last_nonmenu_event; it continues to hold the mouse
	 event that preceded the first level of menu.  */
      if (!used_mouse_menu)
	last_nonmenu_event = key;

      /* Record what part of this_command_keys is the current key sequence.  */
      this_single_command_key_start = this_command_key_count - t;
      /* When 'input-method-function' called above causes events to be
	 put on 'unread-post-input-method-events', and as result
	 'reread' is set to 'true', the value of 't' can become larger
	 than 'this_command_key_count', because 'add_command_key' is
	 not called to update 'this_command_key_count'.  If this
	 happens, 'this_single_command_key_start' will become negative
	 above, and any call to 'this-single-command-keys' will return
	 a garbled vector.  See bug #20223 for one such situation.
	 Here we force 'this_single_command_key_start' to never become
	 negative, to avoid that.  */
      if (this_single_command_key_start < 0)
	this_single_command_key_start = 0;

      /* Look for this sequence in input-decode-map.
	 Scan from indec.end until we find a bound suffix.  */
      while (indec.end < t)
	{
	  bool done;
	  int diff;

	  done = keyremap_step (keybuf, &indec, max (t, mock_input),
				true, &diff, prompt);
	  if (done)
	    {
	      mock_input = diff + max (t, mock_input);
	      goto replay_sequence;
	    }
	}

      if (!KEYMAPP (current_binding)
	  && !test_undefined (current_binding)
	  && indec.start >= t)
	/* There is a binding and it's not a prefix.
	   (and it doesn't have any input-decode-map translation pending).
	   There is thus no function-key in this sequence.
	   Moving fkey.start is important in this case to allow keytran.start
	   to go over the sequence before we return (since we keep the
	   invariant that keytran.end <= fkey.start).  */
	{
	  if (fkey.start < t)
	    (fkey.start = fkey.end = t, fkey.map = fkey.parent);
	}
      else
	/* If the sequence is unbound, see if we can hang a function key
	   off the end of it.  */
	/* Continue scan from fkey.end until we find a bound suffix.  */
	while (fkey.end < indec.start)
	  {
	    bool done;
	    int diff;

	    done = keyremap_step (keybuf, &fkey,
				  max (t, mock_input),
				  /* If there's a binding (i.e.
				     first_binding >= nmaps) we don't want
				     to apply this function-key-mapping.  */
				  (fkey.end + 1 == t
				   && test_undefined (current_binding)),
				  &diff, prompt);
	    if (done)
	      {
		mock_input = diff + max (t, mock_input);
		/* Adjust the input-decode-map counters.  */
		indec.end += diff;
		indec.start += diff;

		goto replay_sequence;
	      }
	  }

      /* Look for this sequence in key-translation-map.
	 Scan from keytran.end until we find a bound suffix.  */
      while (keytran.end < fkey.start)
	{
	  bool done;
	  int diff;

	  done = keyremap_step (keybuf, &keytran, max (t, mock_input),
				true, &diff, prompt);
	  if (done)
	    {
	      mock_input = diff + max (t, mock_input);
	      /* Adjust the function-key-map and input-decode-map counters.  */
	      indec.end += diff;
	      indec.start += diff;
	      fkey.end += diff;
	      fkey.start += diff;

	      goto replay_sequence;
	    }
	}

      /* If KEY is not defined in any of the keymaps,
	 and cannot be part of a function key or translation,
	 and is an upper case letter
	 use the corresponding lower-case letter instead.  */
      if (NILP (current_binding)
	  && /* indec.start >= t && fkey.start >= t && */ keytran.start >= t
	  && FIXNUMP (key))
	{
	  Lisp_Object new_key;
	  EMACS_INT k = XFIXNUM (key);

	  if (k & shift_modifier)
	    XSETINT (new_key, k & ~shift_modifier);
	  else if (CHARACTERP (make_fixnum (k & ~CHAR_MODIFIER_MASK)))
	    {
	      int dc = downcase (k & ~CHAR_MODIFIER_MASK);
	      if (dc == (k & ~CHAR_MODIFIER_MASK))
		goto not_upcase;
	      XSETINT (new_key, dc | (k & CHAR_MODIFIER_MASK));
	    }
	  else
	    goto not_upcase;

	  original_uppercase = key;
	  original_uppercase_position = t - 1;

	  /* We have to do this unconditionally, regardless of whether
	     the lower-case char is defined in the keymaps, because they
	     might get translated through function-key-map.  */
	  keybuf[t - 1] = new_key;
	  mock_input = max (t, mock_input);
	  shift_translated = true;

	  goto replay_sequence;
	}

    not_upcase:
      if (NILP (current_binding)
	  && help_char_p (EVENT_HEAD (key)) && t > 1)
	    {
	      read_key_sequence_cmd = Vprefix_help_command;
	      /* The Microsoft C compiler can't handle the goto that
		 would go here.  */
	      dummyflag = true;
	      break;
	    }

      /* If KEY is not defined in any of the keymaps,
	 and cannot be part of a function key or translation,
	 and is a shifted function key,
	 use the corresponding unshifted function key instead.  */
      if (NILP (current_binding)
	  && /* indec.start >= t && fkey.start >= t && */ keytran.start >= t)
	{
	  Lisp_Object breakdown = parse_modifiers (key);
	  int modifiers
	    = CONSP (breakdown) ? (XFIXNUM (XCAR (XCDR (breakdown)))) : 0;

	  if (modifiers & shift_modifier
	      /* Treat uppercase keys as shifted.  */
	      || (FIXNUMP (key)
		  && (KEY_TO_CHAR (key)
		      < XCHAR_TABLE (BVAR (current_buffer, downcase_table))->header.size)
		  && uppercasep (KEY_TO_CHAR (key))))
	    {
	      Lisp_Object new_key
		= (modifiers & shift_modifier
		   ? apply_modifiers (modifiers & ~shift_modifier,
				      XCAR (breakdown))
		   : make_fixnum (downcase (KEY_TO_CHAR (key)) | modifiers));

	      original_uppercase = key;
	      original_uppercase_position = t - 1;

	      /* We have to do this unconditionally, regardless of whether
		 the lower-case char is defined in the keymaps, because they
		 might get translated through function-key-map.  */
	      keybuf[t - 1] = new_key;
	      mock_input = max (t, mock_input);
	      /* Reset fkey (and consequently keytran) to apply
		 function-key-map on the result, so that S-backspace is
		 correctly mapped to DEL (via backspace).  OTOH,
		 input-decode-map doesn't need to go through it again.  */
	      fkey.start = fkey.end = 0;
	      keytran.start = keytran.end = 0;
	      shift_translated = true;

	      goto replay_sequence;
	    }
	}
    }
  if (!dummyflag)
    read_key_sequence_cmd = current_binding;
  read_key_sequence_remapped
    /* Remap command through active keymaps.
       Do the remapping here, before the unbind_to so it uses the keymaps
       of the appropriate buffer.  */
    = SYMBOLP (read_key_sequence_cmd)
    ? Fcommand_remapping (read_key_sequence_cmd, Qnil, Qnil)
    : Qnil;

  unread_switch_frame = delayed_switch_frame;
  unbind_to (count, Qnil);

  /* Don't downcase the last character if the caller says don't.
     Don't downcase it if the result is undefined, either.  */
  if ((dont_downcase_last || NILP (current_binding))
      && t > 0
      && t - 1 == original_uppercase_position)
    {
      keybuf[t - 1] = original_uppercase;
      shift_translated = false;
    }

  if (shift_translated)
    Vthis_command_keys_shift_translated = Qt;

  /* Occasionally we fabricate events, perhaps by expanding something
     according to function-key-map, or by adding a prefix symbol to a
     mouse click in the scroll bar or modeline.  In this cases, return
     the entire generated key sequence, even if we hit an unbound
     prefix or a definition before the end.  This means that you will
     be able to push back the event properly, and also means that
     read-key-sequence will always return a logical unit.

     Better ideas?  */
  for (; t < mock_input; t++)
    add_command_key (keybuf[t]);
  echo_update ();

  return t;
}

static Lisp_Object
read_key_sequence_vs (Lisp_Object prompt, Lisp_Object continue_echo,
		      Lisp_Object dont_downcase_last,
		      Lisp_Object can_return_switch_frame,
		      Lisp_Object cmd_loop, bool allow_string)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  if (!NILP (prompt))
    CHECK_STRING (prompt);
  maybe_quit ();

  specbind (Qinput_method_exit_on_first_char,
	    (NILP (cmd_loop) ? Qt : Qnil));
  specbind (Qinput_method_use_echo_area,
	    (NILP (cmd_loop) ? Qt : Qnil));

  if (NILP (continue_echo))
    {
      this_command_key_count = 0;
      this_single_command_key_start = 0;
    }

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  raw_keybuf_count = 0;
  Lisp_Object keybuf[READ_KEY_ELTS];
  int i = read_key_sequence (keybuf, prompt, ! NILP (dont_downcase_last),
			     ! NILP (can_return_switch_frame), false, false);

#if 0  /* The following is fine for code reading a key sequence and
	  then proceeding with a lengthy computation, but it's not good
	  for code reading keys in a loop, like an input method.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    start_hourglass ();
#endif
#endif

  if (i == -1)
    {
      Vquit_flag = Qt;
      maybe_quit ();
    }

  return unbind_to (count,
		    ((allow_string ? make_event_array : Fvector)
		     (i, keybuf)));
}

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 5, 0,
       doc: /* Read a sequence of keystrokes and return as a string or vector.
The sequence is sufficient to specify a non-prefix command in the
current local and global maps.

First arg PROMPT is a prompt string.  If nil, do not prompt specially.
Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos
as a continuation of the previous key.

The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not
convert the last event to lower case.  (Normally any upper case event
is converted to lower case if the original event is undefined and the lower
case equivalent is defined.)  A non-nil value is appropriate for reading
a key sequence to be defined.

A C-g typed while in this function is treated like any other character,
and `quit-flag' is not set.

If the key sequence starts with a mouse click, then the sequence is read
using the keymaps of the buffer of the window clicked in, not the buffer
of the selected window as normal.

`read-key-sequence' drops unbound button-down events, since you normally
only care about the click or drag events which follow them.  If a drag
or multi-click event is unbound, but the corresponding click event would
be bound, `read-key-sequence' turns the event into a click event at the
drag's starting position.  This means that you don't have to distinguish
between click and drag, double, or triple events unless you want to.

`read-key-sequence' prefixes mouse events on mode lines, the vertical
lines separating windows, and scroll bars with imaginary keys
`mode-line', `vertical-line', and `vertical-scroll-bar'.

Optional fourth argument CAN-RETURN-SWITCH-FRAME non-nil means that this
function will process a switch-frame event if the user switches frames
before typing anything.  If the user switches frames in the middle of a
key sequence, or at the start of the sequence but CAN-RETURN-SWITCH-FRAME
is nil, then the event will be put off until after the current key sequence.

`read-key-sequence' checks `function-key-map' for function key
sequences, where they wouldn't conflict with ordinary bindings.  See
`function-key-map' for more details.

The optional fifth argument CMD-LOOP, if non-nil, means
that this key sequence is being read by something that will
read commands one after another.  It should be nil if the caller
will read just one key sequence.  */)
  (Lisp_Object prompt, Lisp_Object continue_echo, Lisp_Object dont_downcase_last, Lisp_Object can_return_switch_frame, Lisp_Object cmd_loop)
{
  return read_key_sequence_vs (prompt, continue_echo, dont_downcase_last,
			       can_return_switch_frame, cmd_loop, true);
}

DEFUN ("read-key-sequence-vector", Fread_key_sequence_vector,
       Sread_key_sequence_vector, 1, 5, 0,
       doc: /* Like `read-key-sequence' but always return a vector.  */)
  (Lisp_Object prompt, Lisp_Object continue_echo, Lisp_Object dont_downcase_last, Lisp_Object can_return_switch_frame, Lisp_Object cmd_loop)
{
  return read_key_sequence_vs (prompt, continue_echo, dont_downcase_last,
			       can_return_switch_frame, cmd_loop, false);
}

/* Return true if input events are pending.  */

bool
detect_input_pending (void)
{
  return input_pending || get_input_pending (0);
}

/* Return true if input events other than mouse movements are
   pending.  */

bool
detect_input_pending_ignore_squeezables (void)
{
  return input_pending || get_input_pending (READABLE_EVENTS_IGNORE_SQUEEZABLES);
}

/* Return true if input events are pending, and run any pending timers.  */

bool
detect_input_pending_run_timers (bool do_display)
{
  unsigned old_timers_run = timers_run;

  if (!input_pending)
    get_input_pending (READABLE_EVENTS_DO_TIMERS_NOW);

  if (old_timers_run != timers_run && do_display)
    redisplay_preserve_echo_area (8);

  return input_pending;
}

/* This is called in some cases before a possible quit.
   It cases the next call to detect_input_pending to recompute input_pending.
   So calling this function unnecessarily can't do any harm.  */

void
clear_input_pending (void)
{
  input_pending = false;
}

/* Return true if there are pending requeued events.
   This isn't used yet.  The hope is to make wait_reading_process_output
   call it, and return if it runs Lisp code that unreads something.
   The problem is, kbd_buffer_get_event needs to be fixed to know what
   to do in that case.  It isn't trivial.  */

bool
requeued_events_pending_p (void)
{
  return (CONSP (Vunread_command_events));
}

DEFUN ("input-pending-p", Finput_pending_p, Sinput_pending_p, 0, 1, 0,
       doc: /* Return t if command input is currently available with no wait.
Actually, the value is nil only if we can be sure that no input is available;
if there is a doubt, the value is t.

If CHECK-TIMERS is non-nil, timers that are ready to run will do so.  */)
  (Lisp_Object check_timers)
{
  if (CONSP (Vunread_command_events)
      || !NILP (Vunread_post_input_method_events)
      || !NILP (Vunread_input_method_events))
    return (Qt);

  /* Process non-user-visible events (Bug#10195).  */
  process_special_events ();

  return (get_input_pending ((NILP (check_timers)
                              ? 0 : READABLE_EVENTS_DO_TIMERS_NOW)
			     | READABLE_EVENTS_FILTER_EVENTS)
	  ? Qt : Qnil);
}

DEFUN ("recent-keys", Frecent_keys, Srecent_keys, 0, 1, 0,
       doc: /* Return vector of last few events, not counting those from keyboard macros.
If INCLUDE-CMDS is non-nil, include the commands that were run,
represented as pseudo-events of the form (nil . COMMAND).  */)
  (Lisp_Object include_cmds)
{
  bool cmds = !NILP (include_cmds);

  if (!total_keys
      || (cmds && total_keys < NUM_RECENT_KEYS))
    return Fvector (total_keys,
		    XVECTOR (recent_keys)->contents);
  else
    {
      Lisp_Object es = Qnil;
      int i = (total_keys < NUM_RECENT_KEYS
	       ? 0 : recent_keys_index);
      eassert (recent_keys_index < NUM_RECENT_KEYS);
      do
	{
	  Lisp_Object e = AREF (recent_keys, i);
	  if (cmds || !CONSP (e) || !NILP (XCAR (e)))
	    es = Fcons (e, es);
	  if (++i >= NUM_RECENT_KEYS)
	    i = 0;
	} while (i != recent_keys_index);
      es = Fnreverse (es);
      return Fvconcat (1, &es);
    }
}

DEFUN ("this-command-keys", Fthis_command_keys, Sthis_command_keys, 0, 0, 0,
       doc: /* Return the key sequence that invoked this command.
However, if the command has called `read-key-sequence', it returns
the last key sequence that has been read.
The value is a string or a vector.

See also `this-command-keys-vector'.  */)
  (void)
{
  return make_event_array (this_command_key_count,
			   XVECTOR (this_command_keys)->contents);
}

DEFUN ("set--this-command-keys", Fset__this_command_keys,
       Sset__this_command_keys, 1, 1, 0,
       doc: /* Set the vector to be returned by `this-command-keys'.
The argument KEYS must be a string.
Internal use only.  */)
  (Lisp_Object keys)
{
  CHECK_STRING (keys);

  this_command_key_count = 0;
  this_single_command_key_start = 0;

  int charidx = 0, byteidx = 0;
  int key0;
  FETCH_STRING_CHAR_ADVANCE (key0, keys, charidx, byteidx);
  if (CHAR_BYTE8_P (key0))
    key0 = CHAR_TO_BYTE8 (key0);

  /* Kludge alert: this makes M-x be in the form expected by
     novice.el.  (248 is \370, a.k.a. "Meta-x".)  Any better ideas?  */
  if (key0 == 248)
    add_command_key (make_fixnum ('x' | meta_modifier));
  else
    add_command_key (make_fixnum (key0));
  for (ptrdiff_t i = 1; i < SCHARS (keys); i++)
    {
      int key_i;
      FETCH_STRING_CHAR_ADVANCE (key_i, keys, charidx, byteidx);
      if (CHAR_BYTE8_P (key_i))
	key_i = CHAR_TO_BYTE8 (key_i);
      add_command_key (make_fixnum (key_i));
    }
  return Qnil;
}

DEFUN ("this-command-keys-vector", Fthis_command_keys_vector, Sthis_command_keys_vector, 0, 0, 0,
       doc: /* Return the key sequence that invoked this command, as a vector.
However, if the command has called `read-key-sequence', it returns
the last key sequence that has been read.

See also `this-command-keys'.  */)
  (void)
{
  return Fvector (this_command_key_count,
		  XVECTOR (this_command_keys)->contents);
}

DEFUN ("this-single-command-keys", Fthis_single_command_keys,
       Sthis_single_command_keys, 0, 0, 0,
       doc: /* Return the key sequence that invoked this command.
More generally, it returns the last key sequence read, either by
the command loop or by `read-key-sequence'.
Unlike `this-command-keys', this function's value
does not include prefix arguments.
The value is always a vector.  */)
  (void)
{
  return Fvector (this_command_key_count
		  - this_single_command_key_start,
		  (XVECTOR (this_command_keys)->contents
		   + this_single_command_key_start));
}

DEFUN ("this-single-command-raw-keys", Fthis_single_command_raw_keys,
       Sthis_single_command_raw_keys, 0, 0, 0,
       doc: /* Return the raw events that were read for this command.
More generally, it returns the last key sequence read, either by
the command loop or by `read-key-sequence'.
Unlike `this-single-command-keys', this function's value
shows the events before all translations (except for input methods).
The value is always a vector.  */)
  (void)
{
  return Fvector (raw_keybuf_count, XVECTOR (raw_keybuf)->contents);
}

DEFUN ("clear-this-command-keys", Fclear_this_command_keys,
       Sclear_this_command_keys, 0, 1, 0,
       doc: /* Clear out the vector that `this-command-keys' returns.
Also clear the record of the last 300 input events, unless optional arg
KEEP-RECORD is non-nil.  */)
  (Lisp_Object keep_record)
{
  int i;

  this_command_key_count = 0;

  if (NILP (keep_record))
    {
      for (i = 0; i < ASIZE (recent_keys); ++i)
	ASET (recent_keys, i, Qnil);
      total_keys = 0;
      recent_keys_index = 0;
    }
  return Qnil;
}

DEFUN ("recursion-depth", Frecursion_depth, Srecursion_depth, 0, 0, 0,
       doc: /* Return the current depth in recursive edits.  */)
  (void)
{
  EMACS_INT sum;
  INT_ADD_WRAPV (command_loop_level, minibuf_level, &sum);
  return make_fixnum (sum);
}

DEFUN ("open-dribble-file", Fopen_dribble_file, Sopen_dribble_file, 1, 1,
       "FOpen dribble file: ",
       doc: /* Start writing input events to a dribble file called FILE.
Any previously open dribble file will be closed first.  If FILE is
nil, just close the dribble file, if any.

If the file is still open when Emacs exits, it will be closed then.

The events written to the file include keyboard and mouse input
events, but not events from executing keyboard macros.  The events are
written to the dribble file immediately without line buffering.

Be aware that this records ALL characters you type!
This may include sensitive information such as passwords.  */)
  (Lisp_Object file)
{
  if (dribble)
    {
      block_input ();
      fclose (dribble);
      unblock_input ();
      dribble = 0;
    }
  if (!NILP (file))
    {
      int fd;
      Lisp_Object encfile;

      file = Fexpand_file_name (file, Qnil);
      encfile = ENCODE_FILE (file);
      fd = emacs_open (SSDATA (encfile), O_WRONLY | O_CREAT | O_EXCL, 0600);
      if (fd < 0 && errno == EEXIST
	  && (unlink (SSDATA (encfile)) == 0 || errno == ENOENT))
	fd = emacs_open (SSDATA (encfile), O_WRONLY | O_CREAT | O_EXCL, 0600);
      dribble = fd < 0 ? 0 : fdopen (fd, "w");
      if (dribble == 0)
	report_file_error ("Opening dribble", file);
    }
  return Qnil;
}

DEFUN ("discard-input", Fdiscard_input, Sdiscard_input, 0, 0, 0,
       doc: /* Discard the contents of the terminal input buffer.
Also end any kbd macro being defined.  */)
  (void)
{
  if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
    {
      /* Discard the last command from the macro.  */
      Fcancel_kbd_macro_events ();
      end_kbd_macro ();
    }

  Vunread_command_events = Qnil;

  discard_tty_input ();

  kbd_fetch_ptr =  kbd_store_ptr;
  input_pending = false;

  return Qnil;
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
       doc: /* Stop Emacs and return to superior process.  You can resume later.
If `cannot-suspend' is non-nil, or if the system doesn't support job
control, run a subshell instead.

If optional arg STUFFSTRING is non-nil, its characters are stuffed
to be read as terminal input by Emacs's parent, after suspension.

Before suspending, run the normal hook `suspend-hook'.
After resumption run the normal hook `suspend-resume-hook'.

Some operating systems cannot stop the Emacs process and resume it later.
On such systems, Emacs starts a subshell instead of suspending.  */)
  (Lisp_Object stuffstring)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  int old_height, old_width;
  int width, height;

  if (tty_list && tty_list->next)
    error ("There are other tty frames open; close them before suspending Emacs");

  if (!NILP (stuffstring))
    CHECK_STRING (stuffstring);

  run_hook (intern ("suspend-hook"));

  get_tty_size (fileno (CURTTY ()->input), &old_width, &old_height);
  reset_all_sys_modes ();
  /* sys_suspend can get an error if it tries to fork a subshell
     and the system resources aren't available for that.  */
  record_unwind_protect_void (init_all_sys_modes);
  stuff_buffered_input (stuffstring);
  if (cannot_suspend)
    sys_subshell ();
  else
    sys_suspend ();
  unbind_to (count, Qnil);

  /* Check if terminal/window size has changed.
     Note that this is not useful when we are running directly
     with a window system; but suspend should be disabled in that case.  */
  get_tty_size (fileno (CURTTY ()->input), &width, &height);
  if (width != old_width || height != old_height)
    change_frame_size (SELECTED_FRAME (), width,
		       height - FRAME_MENU_BAR_LINES (SELECTED_FRAME ())
		       - FRAME_TAB_BAR_LINES (SELECTED_FRAME ()),
		       0, 0, 0, 0);

  run_hook (intern ("suspend-resume-hook"));

  return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anything Emacs has read ahead and not used.  */

void
stuff_buffered_input (Lisp_Object stuffstring)
{
#ifdef SIGTSTP  /* stuff_char is defined if SIGTSTP.  */
  register unsigned char *p;

  if (STRINGP (stuffstring))
    {
      register ptrdiff_t count;

      p = SDATA (stuffstring);
      count = SBYTES (stuffstring);
      while (count-- > 0)
	stuff_char (*p++);
      stuff_char ('\n');
    }

  /* Anything we have read ahead, put back for the shell to read.  */
  /* ?? What should this do when we have multiple keyboards??
     Should we ignore anything that was typed in at the "wrong" kboard?

     rms: we should stuff everything back into the kboard
     it came from.  */
  for (; kbd_fetch_ptr != kbd_store_ptr;
       kbd_fetch_ptr = next_kbd_event (kbd_fetch_ptr))
    {

      if (kbd_fetch_ptr->kind == ASCII_KEYSTROKE_EVENT)
	stuff_char (kbd_fetch_ptr->ie.code);

      clear_event (&kbd_fetch_ptr->ie);
    }

  input_pending = false;
#endif /* SIGTSTP */
}

void
set_waiting_for_input (struct timespec *time_to_clear)
{
  input_available_clear_time = time_to_clear;

  /* Tell handle_interrupt to throw back to read_char,  */
  waiting_for_input = true;

  /* If handle_interrupt was called before and buffered a C-g,
     make it run again now, to avoid timing error.  */
  if (!NILP (Vquit_flag))
    quit_throw_to_read_char (0);
}

void
clear_waiting_for_input (void)
{
  /* Tell handle_interrupt not to throw back to read_char,  */
  waiting_for_input = false;
  input_available_clear_time = 0;
}

/* The SIGINT handler.

   If we have a frame on the controlling tty, we assume that the
   SIGINT was generated by C-g, so we call handle_interrupt.
   Otherwise, tell maybe_quit to kill Emacs.  */

static void
handle_interrupt_signal (int sig)
{
  /* See if we have an active terminal on our controlling tty.  */
  struct terminal *terminal = get_named_terminal (DEV_TTY);
  if (!terminal)
    {
      /* If there are no frames there, let's pretend that we are a
         well-behaving UN*X program and quit.  We must not call Lisp
         in a signal handler, so tell maybe_quit to exit when it is
         safe.  */
      Vquit_flag = Qkill_emacs;
    }
  else
    {
      /* Otherwise, the SIGINT was probably generated by C-g.  */

      /* Set internal_last_event_frame to the top frame of the
         controlling tty, if we have a frame there.  We disable the
         interrupt key on secondary ttys, so the SIGINT must have come
         from the controlling tty.  */
      internal_last_event_frame = terminal->display_info.tty->top_frame;

      handle_interrupt (1);
    }
}

static void
deliver_interrupt_signal (int sig)
{
  deliver_process_signal (sig, handle_interrupt_signal);
}

/* Output MSG directly to standard output, without buffering.  Ignore
   failures.  This is safe in a signal handler.  */
static void
write_stdout (char const *msg)
{
  ignore_value (write (STDOUT_FILENO, msg, strlen (msg)));
}

/* Read a byte from stdin, without buffering.  Safe in signal handlers.  */
static int
read_stdin (void)
{
  char c;
  return read (STDIN_FILENO, &c, 1) == 1 ? c : EOF;
}

/* If Emacs is stuck because `inhibit-quit' is true, then keep track
   of the number of times C-g has been requested.  If C-g is pressed
   enough times, then quit anyway.  See bug#6585.  */
static int volatile force_quit_count;

/* This routine is called at interrupt level in response to C-g.

   It is called from the SIGINT handler or kbd_buffer_store_event.

   If `waiting_for_input' is non zero, then unless `echoing' is
   nonzero, immediately throw back to read_char.

   Otherwise it sets the Lisp variable quit-flag not-nil.  This causes
   eval to throw, when it gets a chance.  If quit-flag is already
   non-nil, it stops the job right away.  */

static void
handle_interrupt (bool in_signal_handler)
{
  char c;

  cancel_echoing ();

  /* XXX This code needs to be revised for multi-tty support.  */
  if (!NILP (Vquit_flag) && get_named_terminal (DEV_TTY))
    {
      if (! in_signal_handler)
	{
	  /* If SIGINT isn't blocked, don't let us be interrupted by
	     a SIGINT.  It might be harmful due to non-reentrancy
	     in I/O functions.  */
	  sigset_t blocked;
	  sigemptyset (&blocked);
	  sigaddset (&blocked, SIGINT);
	  pthread_sigmask (SIG_BLOCK, &blocked, 0);
	  fflush (stdout);
	}

      reset_all_sys_modes ();

#ifdef SIGTSTP
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
      sys_suspend ();
#else
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      write_stdout ("No support for stopping a process"
		    " on this operating system;\n"
		    "you can continue or abort.\n");
#endif /* not SIGTSTP */
#ifdef MSDOS
      /* We must remain inside the screen area when the internal terminal
	 is used.  Note that [Enter] is not echoed by dos.  */
      cursor_to (SELECTED_FRAME (), 0, 0);
#endif

      write_stdout ("Emacs is resuming after an emergency escape.\n");

      /* It doesn't work to autosave while GC is in progress;
	 the code used for auto-saving doesn't cope with the mark bit.  */
      if (!gc_in_progress)
	{
	  write_stdout ("Auto-save? (y or n) ");
	  c = read_stdin ();
	  if (c == 'y' || c == 'Y')
	    {
	      Fdo_auto_save (Qt, Qnil);
#ifdef MSDOS
	      write_stdout ("\r\nAuto-save done");
#else
	      write_stdout ("Auto-save done\n");
#endif
	    }
	  while (c != '\n')
	    c = read_stdin ();
	}
      else
	{
	  /* During GC, it must be safe to reenable quitting again.  */
	  Vinhibit_quit = Qnil;
	  write_stdout
	    (
#ifdef MSDOS
	     "\r\n"
#endif
	     "Garbage collection in progress; cannot auto-save now\r\n"
	     "but will instead do a real quit"
	     " after garbage collection ends\r\n");
	}

#ifdef MSDOS
      write_stdout ("\r\nAbort?  (y or n) ");
#else
      write_stdout ("Abort (and dump core)? (y or n) ");
#endif
      c = read_stdin ();
      if (c == 'y' || c == 'Y')
	emacs_abort ();
      while (c != '\n')
	c = read_stdin ();
#ifdef MSDOS
      write_stdout ("\r\nContinuing...\r\n");
#else /* not MSDOS */
      write_stdout ("Continuing...\n");
#endif /* not MSDOS */
      init_all_sys_modes ();
    }
  else
    {
      /* Request quit when it's safe.  */
      int count = NILP (Vquit_flag) ? 1 : force_quit_count + 1;
      force_quit_count = count;
      if (count == 3)
	Vinhibit_quit = Qnil;
      Vquit_flag = Qt;
    }

  pthread_sigmask (SIG_SETMASK, &empty_mask, 0);

/* TODO: The longjmp in this call throws the NS event loop integration off,
         and it seems to do fine without this.  Probably some attention
	 needs to be paid to the setting of waiting_for_input in
         wait_reading_process_output() under HAVE_NS because of the call
         to ns_select there (needed because otherwise events aren't picked up
         outside of polling since we don't get SIGIO like X and we don't have a
         separate event loop thread like W32.  */
#ifndef HAVE_NS
#ifdef THREADS_ENABLED
  /* If we were called from a signal handler, we must be in the main
     thread, see deliver_process_signal.  So we must make sure the
     main thread holds the global lock.  */
  if (in_signal_handler)
    maybe_reacquire_global_lock ();
#endif
  if (waiting_for_input && !echoing)
    quit_throw_to_read_char (in_signal_handler);
#endif
}

/* Handle a C-g by making read_char return C-g.  */

static void
quit_throw_to_read_char (bool from_signal)
{
  /* When not called from a signal handler it is safe to call
     Lisp.  */
  if (!from_signal && EQ (Vquit_flag, Qkill_emacs))
    Fkill_emacs (Qnil);

  /* Prevent another signal from doing this before we finish.  */
  clear_waiting_for_input ();
  input_pending = false;

  Vunread_command_events = Qnil;

  if (FRAMEP (internal_last_event_frame)
      && !EQ (internal_last_event_frame, selected_frame))
    do_switch_frame (make_lispy_switch_frame (internal_last_event_frame),
		     0, 0, Qnil);

  sys_longjmp (getcjmp, 1);
}

DEFUN ("set-input-interrupt-mode", Fset_input_interrupt_mode,
       Sset_input_interrupt_mode, 1, 1, 0,
       doc: /* Set interrupt mode of reading keyboard input.
If INTERRUPT is non-nil, Emacs will use input interrupts;
otherwise Emacs uses CBREAK mode.

See also `current-input-mode'.  */)
  (Lisp_Object interrupt)
{
  bool new_interrupt_input;
#ifdef USABLE_SIGIO
#ifdef HAVE_X_WINDOWS
  if (x_display_list != NULL)
    {
      /* When using X, don't give the user a real choice,
	 because we haven't implemented the mechanisms to support it.  */
      new_interrupt_input = true;
    }
  else
#endif /* HAVE_X_WINDOWS */
    new_interrupt_input = !NILP (interrupt);
#else /* not USABLE_SIGIO */
  new_interrupt_input = false;
#endif /* not USABLE_SIGIO */

  if (new_interrupt_input != interrupt_input)
    {
#ifdef POLL_FOR_INPUT
      stop_polling ();
#endif
#ifndef DOS_NT
      /* this causes startup screen to be restored and messes with the mouse */
      reset_all_sys_modes ();
      interrupt_input = new_interrupt_input;
      init_all_sys_modes ();
#else
      interrupt_input = new_interrupt_input;
#endif

#ifdef POLL_FOR_INPUT
      poll_suppress_count = 1;
      start_polling ();
#endif
    }
  return Qnil;
}

DEFUN ("set-output-flow-control", Fset_output_flow_control, Sset_output_flow_control, 1, 2, 0,
       doc: /* Enable or disable ^S/^Q flow control for output to TERMINAL.
If FLOW is non-nil, flow control is enabled and you cannot use C-s or
C-q in key sequences.

This setting only has an effect on tty terminals and only when
Emacs reads input in CBREAK mode; see `set-input-interrupt-mode'.

See also `current-input-mode'.  */)
  (Lisp_Object flow, Lisp_Object terminal)
{
  struct terminal *t = decode_tty_terminal (terminal);
  struct tty_display_info *tty;

  if (!t)
    return Qnil;
  tty = t->display_info.tty;

  if (tty->flow_control != !NILP (flow))
    {
#ifndef DOS_NT
      /* This causes startup screen to be restored and messes with the mouse.  */
      reset_sys_modes (tty);
#endif

      tty->flow_control = !NILP (flow);

#ifndef DOS_NT
      init_sys_modes (tty);
#endif
    }
  return Qnil;
}

DEFUN ("set-input-meta-mode", Fset_input_meta_mode, Sset_input_meta_mode, 1, 2, 0,
       doc: /* Enable or disable 8-bit input on TERMINAL.
If META is t, Emacs will accept 8-bit input, and interpret the 8th
bit as the Meta modifier.

If META is nil, Emacs will ignore the top bit, on the assumption it is
parity.

Otherwise, Emacs will accept and pass through 8-bit input without
specially interpreting the top bit.

This setting only has an effect on tty terminal devices.

Optional parameter TERMINAL specifies the tty terminal device to use.
It may be a terminal object, a frame, or nil for the terminal used by
the currently selected frame.

See also `current-input-mode'.  */)
  (Lisp_Object meta, Lisp_Object terminal)
{
  struct terminal *t = decode_tty_terminal (terminal);
  struct tty_display_info *tty;
  int new_meta;

  if (!t)
    return Qnil;
  tty = t->display_info.tty;

  if (NILP (meta))
    new_meta = 0;
  else if (EQ (meta, Qt))
    new_meta = 1;
  else
    new_meta = 2;

  if (tty->meta_key != new_meta)
    {
#ifndef DOS_NT
      /* this causes startup screen to be restored and messes with the mouse */
      reset_sys_modes (tty);
#endif

      tty->meta_key = new_meta;

#ifndef DOS_NT
      init_sys_modes (tty);
#endif
    }
  return Qnil;
}

DEFUN ("set-quit-char", Fset_quit_char, Sset_quit_char, 1, 1, 0,
       doc: /* Specify character used for quitting.
QUIT must be an ASCII character.

This function only has an effect on the controlling tty of the Emacs
process.

See also `current-input-mode'.  */)
  (Lisp_Object quit)
{
  struct terminal *t = get_named_terminal (DEV_TTY);
  struct tty_display_info *tty;

  if (!t)
    return Qnil;
  tty = t->display_info.tty;

  if (NILP (quit) || !FIXNUMP (quit) || XFIXNUM (quit) < 0 || XFIXNUM (quit) > 0400)
    error ("QUIT must be an ASCII character");

#ifndef DOS_NT
  /* this causes startup screen to be restored and messes with the mouse */
  reset_sys_modes (tty);
#endif

  /* Don't let this value be out of range.  */
  quit_char = XFIXNUM (quit) & (tty->meta_key == 0 ? 0177 : 0377);

#ifndef DOS_NT
  init_sys_modes (tty);
#endif

  return Qnil;
}

DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 3, 4, 0,
       doc: /* Set mode of reading keyboard input.
First arg INTERRUPT non-nil means use input interrupts;
 nil means use CBREAK mode.
Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal
 (no effect except in CBREAK mode).
Third arg META t means accept 8-bit input (for a Meta key).
 META nil means ignore the top bit, on the assumption it is parity.
 Otherwise, accept 8-bit input and don't use the top bit for Meta.
Optional fourth arg QUIT if non-nil specifies character to use for quitting.
See also `current-input-mode'.  */)
  (Lisp_Object interrupt, Lisp_Object flow, Lisp_Object meta, Lisp_Object quit)
{
  Fset_input_interrupt_mode (interrupt);
  Fset_output_flow_control (flow, Qnil);
  Fset_input_meta_mode (meta, Qnil);
  if (!NILP (quit))
    Fset_quit_char (quit);
  return Qnil;
}

DEFUN ("current-input-mode", Fcurrent_input_mode, Scurrent_input_mode, 0, 0, 0,
       doc: /* Return information about the way Emacs currently reads keyboard input.
The value is a list of the form (INTERRUPT FLOW META QUIT), where
  INTERRUPT is non-nil if Emacs is using interrupt-driven input; if
    nil, Emacs is using CBREAK mode.
  FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the
    terminal; this does not apply if Emacs uses interrupt-driven input.
  META is t if accepting 8-bit input with 8th bit as Meta flag.
    META nil means ignoring the top bit, on the assumption it is parity.
    META is neither t nor nil if accepting 8-bit input and using
    all 8 bits as the character code.
  QUIT is the character Emacs currently uses to quit.
The elements of this list correspond to the arguments of
`set-input-mode'.  */)
  (void)
{
  struct frame *sf = XFRAME (selected_frame);

  Lisp_Object interrupt = interrupt_input ? Qt : Qnil;
  Lisp_Object flow, meta;
  if (FRAME_TERMCAP_P (sf) || FRAME_MSDOS_P (sf))
    {
      flow = FRAME_TTY (sf)->flow_control ? Qt : Qnil;
      meta = (FRAME_TTY (sf)->meta_key == 2
	      ? make_fixnum (0)
	      : (CURTTY ()->meta_key == 1 ? Qt : Qnil));
    }
  else
    {
      flow = Qnil;
      meta = Qt;
    }
  Lisp_Object quit = make_fixnum (quit_char);

  return list4 (interrupt, flow, meta, quit);
}

DEFUN ("posn-at-x-y", Fposn_at_x_y, Sposn_at_x_y, 2, 4, 0,
       doc: /* Return position information for pixel coordinates X and Y.
By default, X and Y are relative to text area of the selected window.
Optional third arg FRAME-OR-WINDOW non-nil specifies frame or window.
If optional fourth arg WHOLE is non-nil, X is relative to the left
edge of the window.

The return value is similar to a mouse click position:
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists.  */)
  (Lisp_Object x, Lisp_Object y, Lisp_Object frame_or_window, Lisp_Object whole)
{
  CHECK_FIXNUM (x);
  /* We allow X of -1, for the newline in a R2L line that overflowed
     into the left fringe.  */
  if (XFIXNUM (x) != -1)
    CHECK_FIXNAT (x);
  CHECK_FIXNAT (y);

  if (NILP (frame_or_window))
    frame_or_window = selected_window;

  if (WINDOWP (frame_or_window))
    {
      struct window *w = decode_live_window (frame_or_window);

      XSETINT (x, (XFIXNUM (x)
		   + WINDOW_LEFT_EDGE_X (w)
		   + (NILP (whole)
		      ? window_box_left_offset (w, TEXT_AREA)
		      : 0)));
      XSETINT (y, WINDOW_TO_FRAME_PIXEL_Y (w, XFIXNUM (y)));
      frame_or_window = w->frame;
    }

  CHECK_LIVE_FRAME (frame_or_window);

  return make_lispy_position (XFRAME (frame_or_window), x, y, 0);
}

DEFUN ("posn-at-point", Fposn_at_point, Sposn_at_point, 0, 2, 0,
       doc: /* Return position information for buffer position POS in WINDOW.
POS defaults to point in WINDOW; WINDOW defaults to the selected window.

Return nil if POS is not visible in WINDOW.  Otherwise,
the return value is similar to that returned by `event-start' for
a mouse click at the upper left corner of the glyph corresponding
to POS:
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists.  */)
  (Lisp_Object pos, Lisp_Object window)
{
  Lisp_Object tem;

  if (NILP (window))
    window = selected_window;

  tem = Fpos_visible_in_window_p (pos, window, Qt);
  if (!NILP (tem))
    {
      Lisp_Object x = XCAR (tem);
      Lisp_Object y = XCAR (XCDR (tem));
      Lisp_Object aux_info = XCDR (XCDR (tem));
      int y_coord = XFIXNUM (y);

      /* Point invisible due to hscrolling?  X can be -1 when a
	 newline in a R2L line overflows into the left fringe.  */
      if (XFIXNUM (x) < -1)
	return Qnil;
      if (!NILP (aux_info) && y_coord < 0)
	{
	  int rtop = XFIXNUM (XCAR (aux_info));

	  y = make_fixnum (y_coord + rtop);
	}
      tem = Fposn_at_x_y (x, y, window, Qnil);
    }

  return tem;
}

/* Set up a new kboard object with reasonable initial values.
   TYPE is a window system for which this keyboard is used.  */

static void
init_kboard (KBOARD *kb, Lisp_Object type)
{
  kset_overriding_terminal_local_map (kb, Qnil);
  kset_last_command (kb, Qnil);
  kset_real_last_command (kb, Qnil);
  kset_keyboard_translate_table (kb, Qnil);
  kset_last_repeatable_command (kb, Qnil);
  kset_prefix_arg (kb, Qnil);
  kset_last_prefix_arg (kb, Qnil);
  kset_kbd_queue (kb, Qnil);
  kb->kbd_queue_has_data = false;
  kb->immediate_echo = false;
  kset_echo_string (kb, Qnil);
  kset_echo_prompt (kb, Qnil);
  kb->kbd_macro_buffer = 0;
  kb->kbd_macro_bufsize = 0;
  kset_defining_kbd_macro (kb, Qnil);
  kset_last_kbd_macro (kb, Qnil);
  kb->reference_count = 0;
  kset_system_key_alist (kb, Qnil);
  kset_system_key_syms (kb, Qnil);
  kset_window_system (kb, type);
  kset_input_decode_map (kb, Fmake_sparse_keymap (Qnil));
  kset_local_function_key_map (kb, Fmake_sparse_keymap (Qnil));
  Fset_keymap_parent (KVAR (kb, Vlocal_function_key_map), Vfunction_key_map);
  kset_default_minibuffer_frame (kb, Qnil);
}

/* Allocate and basically initialize keyboard
   object to use with window system TYPE.  */

KBOARD *
allocate_kboard (Lisp_Object type)
{
  KBOARD *kb = xmalloc (sizeof *kb);

  init_kboard (kb, type);
  kb->next_kboard = all_kboards;
  all_kboards = kb;
  return kb;
}

/*
 * Destroy the contents of a kboard object, but not the object itself.
 * We use this just before deleting it, or if we're going to initialize
 * it a second time.
 */
static void
wipe_kboard (KBOARD *kb)
{
  xfree (kb->kbd_macro_buffer);
}

/* Free KB and memory referenced from it.  */

void
delete_kboard (KBOARD *kb)
{
  KBOARD **kbp;

  for (kbp = &all_kboards; *kbp != kb; kbp = &(*kbp)->next_kboard)
    if (*kbp == NULL)
      emacs_abort ();
  *kbp = kb->next_kboard;

  /* Prevent a dangling reference to KB.  */
  if (kb == current_kboard
      && FRAMEP (selected_frame)
      && FRAME_LIVE_P (XFRAME (selected_frame)))
    {
      current_kboard = FRAME_KBOARD (XFRAME (selected_frame));
      single_kboard = false;
      if (current_kboard == kb)
	emacs_abort ();
    }

  wipe_kboard (kb);
  xfree (kb);
}

void
init_keyboard (void)
{
  /* This is correct before outermost invocation of the editor loop.  */
  command_loop_level = -1;
  quit_char = Ctl ('g');
  Vunread_command_events = Qnil;
  timer_idleness_start_time = invalid_timespec ();
  total_keys = 0;
  recent_keys_index = 0;
  kbd_fetch_ptr = kbd_buffer;
  kbd_store_ptr = kbd_buffer;
  track_mouse = Qnil;
  input_pending = false;
  interrupt_input_blocked = 0;
  pending_signals = false;

  /* This means that command_loop_1 won't try to select anything the first
     time through.  */
  internal_last_event_frame = Qnil;
  Vlast_event_frame = internal_last_event_frame;

  current_kboard = initial_kboard;
  /* Re-initialize the keyboard again.  */
  wipe_kboard (current_kboard);
  /* A value of nil for Vwindow_system normally means a tty, but we also use
     it for the initial terminal since there is no window system there.  */
  init_kboard (current_kboard, Qnil);

  if (!noninteractive)
    {
      /* Before multi-tty support, these handlers used to be installed
         only if the current session was a tty session.  Now an Emacs
         session may have multiple display types, so we always handle
         SIGINT.  There is special code in handle_interrupt_signal to exit
         Emacs on SIGINT when there are no termcap frames on the
         controlling terminal.  */
      struct sigaction action;
      emacs_sigaction_init (&action, deliver_interrupt_signal);
      sigaction (SIGINT, &action, 0);
#ifndef DOS_NT
      /* For systems with SysV TERMIO, C-g is set up for both SIGINT and
	 SIGQUIT and we can't tell which one it will give us.  */
      sigaction (SIGQUIT, &action, 0);
#endif /* not DOS_NT */
    }
#ifdef USABLE_SIGIO
  if (!noninteractive)
    {
      struct sigaction action;
      emacs_sigaction_init (&action, deliver_input_available_signal);
      sigaction (SIGIO, &action, 0);
    }
#endif

/* Use interrupt input by default, if it works and noninterrupt input
   has deficiencies.  */

#ifdef INTERRUPT_INPUT
  interrupt_input = 1;
#else
  interrupt_input = 0;
#endif

  pthread_sigmask (SIG_SETMASK, &empty_mask, 0);
  dribble = 0;

  if (keyboard_init_hook)
    (*keyboard_init_hook) ();

#ifdef POLL_FOR_INPUT
  poll_timer = NULL;
  poll_suppress_count = 1;
  start_polling ();
#endif
}

/* This type's only use is in syms_of_keyboard, to put properties on the
   event header symbols.  */
struct event_head
{
  short var;
  short kind;
};

static const struct event_head head_table[] = {
  {SYMBOL_INDEX (Qmouse_movement),      SYMBOL_INDEX (Qmouse_movement)},
  {SYMBOL_INDEX (Qscroll_bar_movement), SYMBOL_INDEX (Qmouse_movement)},

  /* Some of the event heads.  */
  {SYMBOL_INDEX (Qswitch_frame),        SYMBOL_INDEX (Qswitch_frame)},

  {SYMBOL_INDEX (Qfocus_in),            SYMBOL_INDEX (Qfocus_in)},
  {SYMBOL_INDEX (Qfocus_out),           SYMBOL_INDEX (Qfocus_out)},
  {SYMBOL_INDEX (Qmove_frame),          SYMBOL_INDEX (Qmove_frame)},
  {SYMBOL_INDEX (Qdelete_frame),        SYMBOL_INDEX (Qdelete_frame)},
  {SYMBOL_INDEX (Qiconify_frame),       SYMBOL_INDEX (Qiconify_frame)},
  {SYMBOL_INDEX (Qmake_frame_visible),  SYMBOL_INDEX (Qmake_frame_visible)},
  /* `select-window' should be handled just like `switch-frame'
     in read_key_sequence.  */
  {SYMBOL_INDEX (Qselect_window),       SYMBOL_INDEX (Qswitch_frame)}
};

static void syms_of_keyboard_for_pdumper (void);

void
syms_of_keyboard (void)
{
  pending_funcalls = Qnil;
  staticpro (&pending_funcalls);

  Vlispy_mouse_stem = build_pure_c_string ("mouse");
  staticpro (&Vlispy_mouse_stem);

  regular_top_level_message = build_pure_c_string ("Back to top level");
  staticpro (&regular_top_level_message);
#ifdef HAVE_STACK_OVERFLOW_HANDLING
  recover_top_level_message
    = build_pure_c_string ("Re-entering top level after C stack overflow");
  staticpro (&recover_top_level_message);
#endif
  DEFVAR_LISP ("internal--top-level-message", Vinternal__top_level_message,
	       doc: /* Message displayed by `normal-top-level'.  */);
  Vinternal__top_level_message = regular_top_level_message;

  /* Tool-bars.  */
  DEFSYM (QCimage, ":image");
  DEFSYM (Qhelp_echo, "help-echo");
  DEFSYM (Qhelp_echo_inhibit_substitution, "help-echo-inhibit-substitution");
  DEFSYM (QCrtl, ":rtl");

  staticpro (&item_properties);
  item_properties = Qnil;

  staticpro (&tab_bar_item_properties);
  tab_bar_item_properties = Qnil;
  staticpro (&tab_bar_items_vector);
  tab_bar_items_vector = Qnil;

  staticpro (&tool_bar_item_properties);
  tool_bar_item_properties = Qnil;
  staticpro (&tool_bar_items_vector);
  tool_bar_items_vector = Qnil;

  DEFSYM (Qtimer_event_handler, "timer-event-handler");

  /* Non-nil disable property on a command means do not execute it;
     call disabled-command-function's value instead.  */
  DEFSYM (Qdisabled, "disabled");

  DEFSYM (Qundefined, "undefined");

  /* Hooks to run before and after each command.  */
  DEFSYM (Qpre_command_hook, "pre-command-hook");
  DEFSYM (Qpost_command_hook, "post-command-hook");

  DEFSYM (Qundo_auto__add_boundary, "undo-auto--add-boundary");
  DEFSYM (Qundo_auto__undoably_changed_buffers,
          "undo-auto--undoably-changed-buffers");

  DEFSYM (Qdeferred_action_function, "deferred-action-function");
  DEFSYM (Qdelayed_warnings_hook, "delayed-warnings-hook");
  DEFSYM (Qfunction_key, "function-key");

  /* The values of Qevent_kind properties.  */
  DEFSYM (Qmouse_click, "mouse-click");

  DEFSYM (Qdrag_n_drop, "drag-n-drop");
  DEFSYM (Qsave_session, "save-session");
  DEFSYM (Qconfig_changed_event, "config-changed-event");

  /* Menu and tool bar item parts.  */
  DEFSYM (Qmenu_enable, "menu-enable");

  DEFSYM (Qbuffer_switch, "buffer-switch");

#ifdef HAVE_NTGUI
  DEFSYM (Qlanguage_change, "language-change");
  DEFSYM (Qend_session, "end-session");
#endif

#ifdef HAVE_DBUS
  DEFSYM (Qdbus_event, "dbus-event");
#endif

#ifdef THREADS_ENABLED
  DEFSYM (Qthread_event, "thread-event");
#endif

#ifdef HAVE_XWIDGETS
  DEFSYM (Qxwidget_event, "xwidget-event");
#endif

#ifdef USE_FILE_NOTIFY
  DEFSYM (Qfile_notify, "file-notify");
#endif /* USE_FILE_NOTIFY */

  /* Menu and tool bar item parts.  */
  DEFSYM (QCenable, ":enable");
  DEFSYM (QCvisible, ":visible");
  DEFSYM (QChelp, ":help");
  DEFSYM (QCfilter, ":filter");
  DEFSYM (QCbutton, ":button");
  DEFSYM (QCkeys, ":keys");
  DEFSYM (QCkey_sequence, ":key-sequence");

  /* Non-nil disable property on a command means
     do not execute it; call disabled-command-function's value instead.  */
  DEFSYM (QCtoggle, ":toggle");
  DEFSYM (QCradio, ":radio");
  DEFSYM (QClabel, ":label");
  DEFSYM (QCvert_only, ":vert-only");

  /* Symbols to use for parts of windows.  */
  DEFSYM (Qvertical_line, "vertical-line");
  DEFSYM (Qright_divider, "right-divider");
  DEFSYM (Qbottom_divider, "bottom-divider");

  DEFSYM (Qmouse_fixup_help_message, "mouse-fixup-help-message");

  DEFSYM (Qabove_handle, "above-handle");
  DEFSYM (Qhandle, "handle");
  DEFSYM (Qbelow_handle, "below-handle");
  DEFSYM (Qup, "up");
  DEFSYM (Qdown, "down");
  DEFSYM (Qtop, "top");
  DEFSYM (Qbottom, "bottom");
  DEFSYM (Qend_scroll, "end-scroll");
  DEFSYM (Qratio, "ratio");
  DEFSYM (Qbefore_handle, "before-handle");
  DEFSYM (Qhorizontal_handle, "horizontal-handle");
  DEFSYM (Qafter_handle, "after-handle");
  DEFSYM (Qleft, "left");
  DEFSYM (Qright, "right");
  DEFSYM (Qleftmost, "leftmost");
  DEFSYM (Qrightmost, "rightmost");

  /* Properties of event headers.  */
  DEFSYM (Qevent_kind, "event-kind");
  DEFSYM (Qevent_symbol_elements, "event-symbol-elements");

  /* An event header symbol HEAD may have a property named
     Qevent_symbol_element_mask, which is of the form (BASE MODIFIERS);
     BASE is the base, unmodified version of HEAD, and MODIFIERS is the
     mask of modifiers applied to it.  If present, this is used to help
     speed up parse_modifiers.  */
  DEFSYM (Qevent_symbol_element_mask, "event-symbol-element-mask");

  /* An unmodified event header BASE may have a property named
     Qmodifier_cache, which is an alist mapping modifier masks onto
     modified versions of BASE.  If present, this helps speed up
     apply_modifiers.  */
  DEFSYM (Qmodifier_cache, "modifier-cache");

  DEFSYM (Qrecompute_lucid_menubar, "recompute-lucid-menubar");
  DEFSYM (Qactivate_menubar_hook, "activate-menubar-hook");

  DEFSYM (Qpolling_period, "polling-period");

  DEFSYM (Qgui_set_selection, "gui-set-selection");

  /* The primary selection.  */
  DEFSYM (QPRIMARY, "PRIMARY");

  DEFSYM (Qhandle_switch_frame, "handle-switch-frame");
  DEFSYM (Qhandle_select_window, "handle-select-window");

  DEFSYM (Qinput_method_exit_on_first_char, "input-method-exit-on-first-char");
  DEFSYM (Qinput_method_use_echo_area, "input-method-use-echo-area");

  DEFSYM (Qhelp_form_show, "help-form-show");

  DEFSYM (Qecho_keystrokes, "echo-keystrokes");

  Fset (Qinput_method_exit_on_first_char, Qnil);
  Fset (Qinput_method_use_echo_area, Qnil);

  /* Symbols for dragging internal borders.  */
  DEFSYM (Qdrag_internal_border, "drag-internal-border");
  DEFSYM (Qleft_edge, "left-edge");
  DEFSYM (Qtop_left_corner, "top-left-corner");
  DEFSYM (Qtop_edge, "top-edge");
  DEFSYM (Qtop_right_corner, "top-right-corner");
  DEFSYM (Qright_edge, "right-edge");
  DEFSYM (Qbottom_right_corner, "bottom-right-corner");
  DEFSYM (Qbottom_edge, "bottom-edge");
  DEFSYM (Qbottom_left_corner, "bottom-left-corner");

  /* Symbols to head events.  */
  DEFSYM (Qmouse_movement, "mouse-movement");
  DEFSYM (Qscroll_bar_movement, "scroll-bar-movement");
  DEFSYM (Qswitch_frame, "switch-frame");
  DEFSYM (Qfocus_in, "focus-in");
  DEFSYM (Qfocus_out, "focus-out");
  DEFSYM (Qmove_frame, "move-frame");
  DEFSYM (Qdelete_frame, "delete-frame");
  DEFSYM (Qiconify_frame, "iconify-frame");
  DEFSYM (Qmake_frame_visible, "make-frame-visible");
  DEFSYM (Qselect_window, "select-window");
  DEFSYM (Qselection_request, "selection-request");
  {
    int i;

    for (i = 0; i < ARRAYELTS (head_table); i++)
      {
	const struct event_head *p = &head_table[i];
	Lisp_Object var = builtin_lisp_symbol (p->var);
	Lisp_Object kind = builtin_lisp_symbol (p->kind);
	Fput (var, Qevent_kind, kind);
	Fput (var, Qevent_symbol_elements, list1 (var));
      }
  }
  DEFSYM (Qno_record, "no-record");

  button_down_location = make_nil_vector (5);
  staticpro (&button_down_location);
  mouse_syms = make_nil_vector (5);
  staticpro (&mouse_syms);
  wheel_syms = make_nil_vector (ARRAYELTS (lispy_wheel_names));
  staticpro (&wheel_syms);

  {
    int i;
    int len = ARRAYELTS (modifier_names);

    modifier_symbols = make_nil_vector (len);
    for (i = 0; i < len; i++)
      if (modifier_names[i])
	ASET (modifier_symbols, i, intern_c_string (modifier_names[i]));
    staticpro (&modifier_symbols);
  }

  recent_keys = make_nil_vector (NUM_RECENT_KEYS);
  staticpro (&recent_keys);

  this_command_keys = make_nil_vector (40);
  staticpro (&this_command_keys);

  raw_keybuf = make_nil_vector (30);
  staticpro (&raw_keybuf);

  DEFSYM (Qcommand_execute, "command-execute");
  DEFSYM (Qinternal_echo_keystrokes_prefix, "internal-echo-keystrokes-prefix");

  accent_key_syms = Qnil;
  staticpro (&accent_key_syms);

  func_key_syms = Qnil;
  staticpro (&func_key_syms);

  drag_n_drop_syms = Qnil;
  staticpro (&drag_n_drop_syms);

  unread_switch_frame = Qnil;
  staticpro (&unread_switch_frame);

  internal_last_event_frame = Qnil;
  staticpro (&internal_last_event_frame);

  read_key_sequence_cmd = Qnil;
  staticpro (&read_key_sequence_cmd);
  read_key_sequence_remapped = Qnil;
  staticpro (&read_key_sequence_remapped);

  menu_bar_one_keymap_changed_items = Qnil;
  staticpro (&menu_bar_one_keymap_changed_items);

  menu_bar_items_vector = Qnil;
  staticpro (&menu_bar_items_vector);

  help_form_saved_window_configs = Qnil;
  staticpro (&help_form_saved_window_configs);

  defsubr (&Scurrent_idle_time);
  defsubr (&Sevent_symbol_parse_modifiers);
  defsubr (&Sevent_convert_list);
  defsubr (&Sinternal_handle_focus_in);
  defsubr (&Sread_key_sequence);
  defsubr (&Sread_key_sequence_vector);
  defsubr (&Srecursive_edit);
  defsubr (&Sinternal_track_mouse);
  defsubr (&Sinput_pending_p);
  defsubr (&Srecent_keys);
  defsubr (&Sthis_command_keys);
  defsubr (&Sthis_command_keys_vector);
  defsubr (&Sthis_single_command_keys);
  defsubr (&Sthis_single_command_raw_keys);
  defsubr (&Sset__this_command_keys);
  defsubr (&Sclear_this_command_keys);
  defsubr (&Ssuspend_emacs);
  defsubr (&Sabort_recursive_edit);
  defsubr (&Sexit_recursive_edit);
  defsubr (&Srecursion_depth);
  defsubr (&Scommand_error_default_function);
  defsubr (&Stop_level);
  defsubr (&Sdiscard_input);
  defsubr (&Sopen_dribble_file);
  defsubr (&Sset_input_interrupt_mode);
  defsubr (&Sset_output_flow_control);
  defsubr (&Sset_input_meta_mode);
  defsubr (&Sset_quit_char);
  defsubr (&Sset_input_mode);
  defsubr (&Scurrent_input_mode);
  defsubr (&Sposn_at_point);
  defsubr (&Sposn_at_x_y);

  DEFVAR_LISP ("last-command-event", last_command_event,
		     doc: /* Last input event of a key sequence that called a command.
See Info node `(elisp)Command Loop Info'.*/);

  DEFVAR_LISP ("last-nonmenu-event", last_nonmenu_event,
	       doc: /* Last input event in a command, except for mouse menu events.
Mouse menus give back keys that don't look like mouse events;
this variable holds the actual mouse event that led to the menu,
so that you can determine whether the command was run by mouse or not.  */);

  DEFVAR_LISP ("last-input-event", last_input_event,
	       doc: /* Last input event.  */);

  DEFVAR_LISP ("unread-command-events", Vunread_command_events,
	       doc: /* List of events to be read as the command input.
These events are processed first, before actual keyboard input.
Events read from this list are not normally added to `this-command-keys',
as they will already have been added once as they were read for the first time.
An element of the form (t . EVENT) forces EVENT to be added to that list.
An element of the form (no-record . EVENT) means process EVENT, but do not
record it in the keyboard macros, recent-keys, and the dribble file.  */);
  Vunread_command_events = Qnil;

  DEFVAR_LISP ("unread-post-input-method-events", Vunread_post_input_method_events,
	       doc: /* List of events to be processed as input by input methods.
These events are processed before `unread-command-events'
and actual keyboard input, but are not given to `input-method-function'.  */);
  Vunread_post_input_method_events = Qnil;

  DEFVAR_LISP ("unread-input-method-events", Vunread_input_method_events,
	       doc: /* List of events to be processed as input by input methods.
These events are processed after `unread-command-events', but
before actual keyboard input.
If there's an active input method, the events are given to
`input-method-function'.  */);
  Vunread_input_method_events = Qnil;

  DEFVAR_LISP ("meta-prefix-char", meta_prefix_char,
	       doc: /* Meta-prefix character code.
Meta-foo as command input turns into this character followed by foo.  */);
  XSETINT (meta_prefix_char, 033);

  DEFVAR_KBOARD ("last-command", Vlast_command,
		 doc: /* The last command executed.
Normally a symbol with a function definition, but can be whatever was found
in the keymap, or whatever the variable `this-command' was set to by that
command.

The value `mode-exit' is special; it means that the previous command
read an event that told it to exit, and it did so and unread that event.
In other words, the present command is the event that made the previous
command exit.

The value `kill-region' is special; it means that the previous command
was a kill command.

`last-command' has a separate binding for each terminal device.
See Info node `(elisp)Multiple Terminals'.  */);

  DEFVAR_KBOARD ("real-last-command", Vreal_last_command,
		 doc: /* Same as `last-command', but never altered by Lisp code.
Taken from the previous value of `real-this-command'.  */);

  DEFVAR_KBOARD ("last-repeatable-command", Vlast_repeatable_command,
		 doc: /* Last command that may be repeated.
The last command executed that was not bound to an input event.
This is the command `repeat' will try to repeat.
Taken from a previous value of `real-this-command'.  */);

  DEFVAR_LISP ("this-command", Vthis_command,
	       doc: /* The command now being executed.
The command can set this variable; whatever is put here
will be in `last-command' during the following command.  */);
  Vthis_command = Qnil;

  DEFVAR_LISP ("real-this-command", Vreal_this_command,
	       doc: /* This is like `this-command', except that commands should never modify it.  */);
  Vreal_this_command = Qnil;

  DEFVAR_LISP ("this-command-keys-shift-translated",
	       Vthis_command_keys_shift_translated,
	       doc: /* Non-nil if the key sequence activating this command was shift-translated.
Shift-translation occurs when there is no binding for the key sequence
as entered, but a binding was found by changing an upper-case letter
to lower-case, or a shifted function key to an unshifted one.  */);
  Vthis_command_keys_shift_translated = Qnil;

  DEFVAR_LISP ("this-original-command", Vthis_original_command,
	       doc: /* The command bound to the current key sequence before remapping.
It equals `this-command' if the original command was not remapped through
any of the active keymaps.  Otherwise, the value of `this-command' is the
result of looking up the original command in the active keymaps.  */);
  Vthis_original_command = Qnil;

  DEFVAR_INT ("auto-save-interval", auto_save_interval,
	      doc: /* Number of input events between auto-saves.
Zero means disable autosaving due to number of characters typed.  */);
  auto_save_interval = 300;

  DEFVAR_BOOL ("auto-save-no-message", auto_save_no_message,
	       doc: /* Non-nil means do not print any message when auto-saving. */);
  auto_save_no_message = false;

  DEFVAR_LISP ("auto-save-timeout", Vauto_save_timeout,
	       doc: /* Number of seconds idle time before auto-save.
Zero or nil means disable auto-saving due to idleness.
After auto-saving due to this many seconds of idle time,
Emacs also does a garbage collection if that seems to be warranted.  */);
  XSETFASTINT (Vauto_save_timeout, 30);

  DEFVAR_LISP ("echo-keystrokes", Vecho_keystrokes,
	       doc: /* Nonzero means echo unfinished commands after this many seconds of pause.
The value may be integer or floating point.
If the value is zero, don't echo at all.  */);
  Vecho_keystrokes = make_fixnum (1);

  DEFVAR_INT ("polling-period", polling_period,
	      doc: /* Interval between polling for input during Lisp execution.
The reason for polling is to make C-g work to stop a running program.
Polling is needed only when using X windows and SIGIO does not work.
Polling is automatically disabled in all other cases.  */);
  polling_period = 2;

  DEFVAR_LISP ("double-click-time", Vdouble_click_time,
	       doc: /* Maximum time between mouse clicks to make a double-click.
Measured in milliseconds.  The value nil means disable double-click
recognition; t means double-clicks have no time limit and are detected
by position only.  */);
  Vdouble_click_time = make_fixnum (500);

  DEFVAR_INT ("double-click-fuzz", double_click_fuzz,
	      doc: /* Maximum mouse movement between clicks to make a double-click.
On window-system frames, value is the number of pixels the mouse may have
moved horizontally or vertically between two clicks to make a double-click.
On non window-system frames, value is interpreted in units of 1/8 characters
instead of pixels.

This variable is also the threshold for motion of the mouse
to count as a drag.  */);
  double_click_fuzz = 3;

  DEFVAR_INT ("num-input-keys", num_input_keys,
	      doc: /* Number of complete key sequences read as input so far.
This includes key sequences read from keyboard macros.
The number is effectively the number of interactive command invocations.  */);
  num_input_keys = 0;

  DEFVAR_INT ("num-nonmacro-input-events", num_nonmacro_input_events,
	      doc: /* Number of input events read from the keyboard so far.
This does not include events generated by keyboard macros.  */);
  num_nonmacro_input_events = 0;

  DEFVAR_LISP ("last-event-frame", Vlast_event_frame,
	       doc: /* The frame in which the most recently read event occurred.
If the last event came from a keyboard macro, this is set to `macro'.  */);
  Vlast_event_frame = Qnil;

  /* This variable is set up in sysdep.c.  */
  DEFVAR_LISP ("tty-erase-char", Vtty_erase_char,
	       doc: /* The ERASE character as set by the user with stty.  */);

  DEFVAR_LISP ("help-char", Vhelp_char,
	       doc: /* Character to recognize as meaning Help.
When it is read, do `(eval help-form)', and display result if it's a string.
If the value of `help-form' is nil, this char can be read normally.  */);
  XSETINT (Vhelp_char, Ctl ('H'));

  DEFVAR_LISP ("help-event-list", Vhelp_event_list,
	       doc: /* List of input events to recognize as meaning Help.
These work just like the value of `help-char' (see that).  */);
  Vhelp_event_list = Qnil;

  DEFVAR_LISP ("help-form", Vhelp_form,
	       doc: /* Form to execute when character `help-char' is read.
If the form returns a string, that string is displayed.
If `help-form' is nil, the help char is not recognized.  */);
  Vhelp_form = Qnil;

  DEFVAR_LISP ("prefix-help-command", Vprefix_help_command,
	       doc: /* Command to run when `help-char' character follows a prefix key.
This command is used only when there is no actual binding
for that character after that prefix key.  */);
  Vprefix_help_command = Qnil;

  DEFVAR_LISP ("top-level", Vtop_level,
	       doc: /* Form to evaluate when Emacs starts up.
Useful to set before you dump a modified Emacs.  */);
  Vtop_level = Qnil;
  XSYMBOL (Qtop_level)->u.s.declared_special = false;

  DEFVAR_KBOARD ("keyboard-translate-table", Vkeyboard_translate_table,
                 doc: /* Translate table for local keyboard input, or nil.
If non-nil, the value should be a char-table.  Each character read
from the keyboard is looked up in this char-table.  If the value found
there is non-nil, then it is used instead of the actual input character.

The value can also be a string or vector, but this is considered obsolete.
If it is a string or vector of length N, character codes N and up are left
untranslated.  In a vector, an element which is nil means "no translation".

This is applied to the characters supplied to input methods, not their
output.  See also `translation-table-for-input'.

This variable has a separate binding for each terminal.
See Info node `(elisp)Multiple Terminals'.  */);

  DEFVAR_BOOL ("cannot-suspend", cannot_suspend,
	       doc: /* Non-nil means to always spawn a subshell instead of suspending.
\(Even if the operating system has support for stopping a process.)  */);
  cannot_suspend = false;

  DEFVAR_BOOL ("menu-prompting", menu_prompting,
	       doc: /* Non-nil means prompt with menus when appropriate.
This is done when reading from a keymap that has a prompt string,
for elements that have prompt strings.
The menu is displayed on the screen
if X menus were enabled at configuration
time and the previous event was a mouse click prefix key.
Otherwise, menu prompting uses the echo area.  */);
  menu_prompting = true;

  DEFVAR_LISP ("menu-prompt-more-char", menu_prompt_more_char,
	       doc: /* Character to see next line of menu prompt.
Type this character while in a menu prompt to rotate around the lines of it.  */);
  XSETINT (menu_prompt_more_char, ' ');

  DEFVAR_INT ("extra-keyboard-modifiers", extra_keyboard_modifiers,
	      doc: /* A mask of additional modifier keys to use with every keyboard character.
Emacs applies the modifiers of the character stored here to each keyboard
character it reads.  For example, after evaluating the expression
    (setq extra-keyboard-modifiers ?\\C-x)
all input characters will have the control modifier applied to them.

Note that the character ?\\C-@, equivalent to the integer zero, does
not count as a control character; rather, it counts as a character
with no modifiers; thus, setting `extra-keyboard-modifiers' to zero
cancels any modification.  */);
  extra_keyboard_modifiers = 0;

  DEFSYM (Qdeactivate_mark, "deactivate-mark");
  DEFVAR_LISP ("deactivate-mark", Vdeactivate_mark,
	       doc: /* If an editing command sets this to t, deactivate the mark afterward.
The command loop sets this to nil before each command,
and tests the value when the command returns.
Buffer modification stores t in this variable.  */);
  Vdeactivate_mark = Qnil;
  Fmake_variable_buffer_local (Qdeactivate_mark);

  DEFVAR_LISP ("pre-command-hook", Vpre_command_hook,
	       doc: /* Normal hook run before each command is executed.
If an unhandled error happens in running this hook,
the function in which the error occurred is unconditionally removed, since
otherwise the error might happen repeatedly and make Emacs nonfunctional.

See also `post-command-hook'.  */);
  Vpre_command_hook = Qnil;

  DEFVAR_LISP ("post-command-hook", Vpost_command_hook,
	       doc: /* Normal hook run after each command is executed.
If an unhandled error happens in running this hook,
the function in which the error occurred is unconditionally removed, since
otherwise the error might happen repeatedly and make Emacs nonfunctional.

It is a bad idea to use this hook for expensive processing.  If
unavoidable, wrap your code in `(while-no-input (redisplay) CODE)' to
avoid making Emacs unresponsive while the user types.

See also `pre-command-hook'.  */);
  Vpost_command_hook = Qnil;

#if 0
  DEFVAR_LISP ("echo-area-clear-hook", ...,
	       doc: /* Normal hook run when clearing the echo area.  */);
#endif
  DEFSYM (Qecho_area_clear_hook, "echo-area-clear-hook");
  Fset (Qecho_area_clear_hook, Qnil);

  DEFVAR_LISP ("lucid-menu-bar-dirty-flag", Vlucid_menu_bar_dirty_flag,
	       doc: /* Non-nil means menu bar, specified Lucid style, needs to be recomputed.  */);
  Vlucid_menu_bar_dirty_flag = Qnil;

  DEFVAR_LISP ("menu-bar-final-items", Vmenu_bar_final_items,
	       doc: /* List of menu bar items to move to the end of the menu bar.
The elements of the list are event types that may have menu bar bindings.  */);
  Vmenu_bar_final_items = Qnil;

  DEFVAR_LISP ("tab-bar-separator-image-expression", Vtab_bar_separator_image_expression,
    doc: /* Expression evaluating to the image spec for a tab-bar separator.
This is used internally by graphical displays that do not render
tab-bar separators natively.  Otherwise it is unused (e.g. on GTK).  */);
  Vtab_bar_separator_image_expression = Qnil;

  DEFVAR_LISP ("tool-bar-separator-image-expression", Vtool_bar_separator_image_expression,
    doc: /* Expression evaluating to the image spec for a tool-bar separator.
This is used internally by graphical displays that do not render
tool-bar separators natively.  Otherwise it is unused (e.g. on GTK).  */);
  Vtool_bar_separator_image_expression = Qnil;

  DEFVAR_KBOARD ("overriding-terminal-local-map",
		 Voverriding_terminal_local_map,
		 doc: /* Per-terminal keymap that takes precedence over all other keymaps.
This variable is intended to let commands such as `universal-argument'
set up a different keymap for reading the next command.

`overriding-terminal-local-map' has a separate binding for each
terminal device.  See Info node `(elisp)Multiple Terminals'.  */);

  DEFVAR_LISP ("overriding-local-map", Voverriding_local_map,
	       doc: /* Keymap that replaces (overrides) local keymaps.
If this variable is non-nil, Emacs looks up key bindings in this
keymap INSTEAD OF the keymap char property, minor mode maps, and the
buffer's local map.  Hence, the only active keymaps would be
`overriding-terminal-local-map', this keymap, and `global-keymap', in
order of precedence.  */);
  Voverriding_local_map = Qnil;

  DEFVAR_LISP ("overriding-local-map-menu-flag", Voverriding_local_map_menu_flag,
	       doc: /* Non-nil means `overriding-local-map' applies to the menu bar.
Otherwise, the menu bar continues to reflect the buffer's local map
and the minor mode maps regardless of `overriding-local-map'.  */);
  Voverriding_local_map_menu_flag = Qnil;

  DEFVAR_LISP ("special-event-map", Vspecial_event_map,
	       doc: /* Keymap defining bindings for special events to execute at low level.  */);
  Vspecial_event_map = list1 (Qkeymap);

  DEFVAR_LISP ("track-mouse", track_mouse,
	       doc: /* Non-nil means generate motion events for mouse motion.
The special values `dragging' and `dropping' assert that the mouse
cursor retains its appearance during mouse motion.  Any non-nil value
but `dropping' asserts that motion events always relate to the frame
where the mouse movement started.  The value `dropping' asserts
that motion events relate to the frame where the mouse cursor is seen
when generating the event.  If there's no such frame, such motion
events relate to the frame where the mouse movement started.  */);

  DEFVAR_KBOARD ("system-key-alist", Vsystem_key_alist,
		 doc: /* Alist of system-specific X windows key symbols.
Each element should have the form (N . SYMBOL) where N is the
numeric keysym code (sans the \"system-specific\" bit 1<<28)
and SYMBOL is its name.

`system-key-alist' has a separate binding for each terminal device.
See Info node `(elisp)Multiple Terminals'.  */);

  DEFVAR_KBOARD ("local-function-key-map", Vlocal_function_key_map,
                 doc: /* Keymap that translates key sequences to key sequences during input.
This is used mainly for mapping key sequences into some preferred
key events (symbols).

The `read-key-sequence' function replaces any subsequence bound by
`local-function-key-map' with its binding.  More precisely, when the
active keymaps have no binding for the current key sequence but
`local-function-key-map' binds a suffix of the sequence to a vector or
string, `read-key-sequence' replaces the matching suffix with its
binding, and continues with the new sequence.

If the binding is a function, it is called with one argument (the prompt)
and its return value (a key sequence) is used.

The events that come from bindings in `local-function-key-map' are not
themselves looked up in `local-function-key-map'.

For example, suppose `local-function-key-map' binds `ESC O P' to [f1].
Typing `ESC O P' to `read-key-sequence' would return [f1].  Typing
`C-x ESC O P' would return [?\\C-x f1].  If [f1] were a prefix key,
typing `ESC O P x' would return [f1 x].

`local-function-key-map' has a separate binding for each terminal
device.  See Info node `(elisp)Multiple Terminals'.  If you need to
define a binding on all terminals, change `function-key-map'
instead.  Initially, `local-function-key-map' is an empty keymap that
has `function-key-map' as its parent on all terminal devices.  */);

  DEFVAR_KBOARD ("input-decode-map", Vinput_decode_map,
		 doc: /* Keymap that decodes input escape sequences.
This is used mainly for mapping ASCII function key sequences into
real Emacs function key events (symbols).

The `read-key-sequence' function replaces any subsequence bound by
`input-decode-map' with its binding.  Contrary to `function-key-map',
this map applies its rebinding regardless of the presence of an ordinary
binding.  So it is more like `key-translation-map' except that it applies
before `function-key-map' rather than after.

If the binding is a function, it is called with one argument (the prompt)
and its return value (a key sequence) is used.

The events that come from bindings in `input-decode-map' are not
themselves looked up in `input-decode-map'.  */);

  DEFVAR_LISP ("function-key-map", Vfunction_key_map,
               doc: /* The parent keymap of all `local-function-key-map' instances.
Function key definitions that apply to all terminal devices should go
here.  If a mapping is defined in both the current
`local-function-key-map' binding and this variable, then the local
definition will take precedence.  */);
  Vfunction_key_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("key-translation-map", Vkey_translation_map,
               doc: /* Keymap of key translations that can override keymaps.
This keymap works like `input-decode-map', but comes after `function-key-map'.
Another difference is that it is global rather than terminal-local.  */);
  Vkey_translation_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("deferred-action-list", Vdeferred_action_list,
	       doc: /* List of deferred actions to be performed at a later time.
The precise format isn't relevant here; we just check whether it is nil.  */);
  Vdeferred_action_list = Qnil;

  DEFVAR_LISP ("deferred-action-function", Vdeferred_action_function,
	       doc: /* Function to call to handle deferred actions, after each command.
This function is called with no arguments after each command
whenever `deferred-action-list' is non-nil.  */);
  Vdeferred_action_function = Qnil;

  DEFVAR_LISP ("delayed-warnings-list", Vdelayed_warnings_list,
               doc: /* List of warnings to be displayed after this command.
Each element must be a list (TYPE MESSAGE [LEVEL [BUFFER-NAME]]),
as per the args of `display-warning' (which see).
If this variable is non-nil, `delayed-warnings-hook' will be run
immediately after running `post-command-hook'.  */);
  Vdelayed_warnings_list = Qnil;

  DEFVAR_LISP ("timer-list", Vtimer_list,
	       doc: /* List of active absolute time timers in order of increasing time.  */);
  Vtimer_list = Qnil;

  DEFVAR_LISP ("timer-idle-list", Vtimer_idle_list,
	       doc: /* List of active idle-time timers in order of increasing time.  */);
  Vtimer_idle_list = Qnil;

  DEFVAR_LISP ("input-method-function", Vinput_method_function,
	       doc: /* If non-nil, the function that implements the current input method.
It's called with one argument, which must be a single-byte
character that was just read.  Any single-byte character is
acceptable, except the DEL character, codepoint 127 decimal, 177 octal.
Typically this function uses `read-event' to read additional events.
When it does so, it should first bind `input-method-function' to nil
so it will not be called recursively.

The function should return a list of zero or more events
to be used as input.  If it wants to put back some events
to be reconsidered, separately, by the input method,
it can add them to the beginning of `unread-command-events'.

The input method function can find in `input-method-previous-message'
the previous echo area message.

The input method function should refer to the variables
`input-method-use-echo-area' and `input-method-exit-on-first-char'
for guidance on what to do.  */);
  Vinput_method_function = Qlist;

  DEFVAR_LISP ("input-method-previous-message",
	       Vinput_method_previous_message,
	       doc: /* When `input-method-function' is called, hold the previous echo area message.
This variable exists because `read-event' clears the echo area
before running the input method.  It is nil if there was no message.  */);
  Vinput_method_previous_message = Qnil;

  DEFVAR_LISP ("show-help-function", Vshow_help_function,
	       doc: /* If non-nil, the function that implements the display of help.
It's called with one argument, the help string to display.  */);
  Vshow_help_function = Qnil;

  DEFVAR_LISP ("disable-point-adjustment", Vdisable_point_adjustment,
	       doc: /* If non-nil, suppress point adjustment after executing a command.

After a command is executed, if point moved into a region that has
special properties (e.g. composition, display), Emacs adjusts point to
the boundary of the region.  But when a command leaves this variable at
a non-nil value (e.g., with a setq), this point adjustment is suppressed.

This variable is set to nil before reading a command, and is checked
just after executing the command.  */);
  Vdisable_point_adjustment = Qnil;

  DEFVAR_LISP ("global-disable-point-adjustment",
	       Vglobal_disable_point_adjustment,
	       doc: /* If non-nil, always suppress point adjustments.

The default value is nil, in which case point adjustments are
suppressed only after special commands that leave
`disable-point-adjustment' (which see) at a non-nil value.  */);
  Vglobal_disable_point_adjustment = Qnil;

  DEFVAR_LISP ("minibuffer-message-timeout", Vminibuffer_message_timeout,
	       doc: /* How long to display an echo-area message when the minibuffer is active.
If the value is a number, it should be specified in seconds.
If the value is not a number, such messages never time out.  */);
  Vminibuffer_message_timeout = make_fixnum (2);

  DEFVAR_LISP ("throw-on-input", Vthrow_on_input,
	       doc: /* If non-nil, any keyboard input throws to this symbol.
The value of that variable is passed to `quit-flag' and later causes a
peculiar kind of quitting.  */);
  Vthrow_on_input = Qnil;

  DEFVAR_LISP ("command-error-function", Vcommand_error_function,
	       doc: /* Function to output error messages.
Called with three arguments:
- the error data, a list of the form (SIGNALED-CONDITION . SIGNAL-DATA)
  such as what `condition-case' would bind its variable to,
- the context (a string which normally goes at the start of the message),
- the Lisp function within which the error was signaled.  */);
  Vcommand_error_function = intern ("command-error-default-function");

  DEFVAR_LISP ("enable-disabled-menus-and-buttons",
	       Venable_disabled_menus_and_buttons,
	       doc: /* If non-nil, don't ignore events produced by disabled menu items and tool-bar.

Help functions bind this to allow help on disabled menu items
and tool-bar buttons.  */);
  Venable_disabled_menus_and_buttons = Qnil;

  DEFVAR_LISP ("select-active-regions",
	       Vselect_active_regions,
	       doc: /* If non-nil, an active region automatically sets the primary selection.
If the value is `only', only temporarily active regions (usually made
by mouse-dragging or shift-selection) set the window selection.

This takes effect only when Transient Mark mode is enabled.  */);
  Vselect_active_regions = Qt;

  DEFVAR_LISP ("saved-region-selection",
	       Vsaved_region_selection,
	       doc: /* Contents of active region prior to buffer modification.
If `select-active-regions' is non-nil, Emacs sets this to the
text in the region before modifying the buffer.  The next call to
the function `deactivate-mark' uses this to set the window selection.  */);
  Vsaved_region_selection = Qnil;

  DEFVAR_LISP ("selection-inhibit-update-commands",
	       Vselection_inhibit_update_commands,
	       doc: /* List of commands which should not update the selection.
Normally, if `select-active-regions' is non-nil and the mark remains
active after a command (i.e. the mark was not deactivated), the Emacs
command loop sets the selection to the text in the region.  However,
if the command is in this list, the selection is not updated.  */);
  Vselection_inhibit_update_commands
    = list2 (Qhandle_switch_frame, Qhandle_select_window);

  DEFVAR_LISP ("debug-on-event",
               Vdebug_on_event,
               doc: /* Enter debugger on this event.
When Emacs receives the special event specified by this variable,
it will try to break into the debugger as soon as possible instead
of processing the event normally through `special-event-map'.

Currently, the only supported values for this
variable are `sigusr1' and `sigusr2'.  */);
  Vdebug_on_event = intern_c_string ("sigusr2");

  DEFVAR_BOOL ("attempt-stack-overflow-recovery",
               attempt_stack_overflow_recovery,
               doc: /* If non-nil, attempt to recover from C stack overflows.
This recovery is potentially unsafe and may lead to deadlocks or data
corruption, but it usually works and may preserve modified buffers
that would otherwise be lost.  If nil, treat stack overflow like any
other kind of crash or fatal error.  */);
  attempt_stack_overflow_recovery = true;

  DEFVAR_BOOL ("attempt-orderly-shutdown-on-fatal-signal",
               attempt_orderly_shutdown_on_fatal_signal,
               doc: /* If non-nil, attempt orderly shutdown on fatal signals.
By default this variable is non-nil, and Emacs attempts to perform
an orderly shutdown when it catches a fatal signal (e.g., a crash).
The orderly shutdown includes an attempt to auto-save your unsaved edits
and other useful cleanups.  These cleanups are potentially unsafe and may
lead to deadlocks or data corruption, but it usually works and may
preserve data in modified buffers that would otherwise be lost.
If nil, Emacs crashes immediately in response to fatal signals.  */);
  attempt_orderly_shutdown_on_fatal_signal = true;

  DEFVAR_LISP ("while-no-input-ignore-events",
               Vwhile_no_input_ignore_events,
               doc: /* Ignored events from while-no-input.  */);

  DEFVAR_BOOL ("inhibit--record-char",
	       inhibit_record_char,
	       doc: /* If non-nil, don't record input events.
This inhibits recording input events for the purposes of keyboard
macros, dribble file, and `recent-keys'.
Internal use only.  */);

  pdumper_do_now_and_after_load (syms_of_keyboard_for_pdumper);
}

static void
syms_of_keyboard_for_pdumper (void)
{
  /* Make sure input state is pristine when restoring from a dump.
     init_keyboard() also resets some of these, but the duplication
     doesn't hurt and makes sure that allocate_kboard and subsequent
     early init functions see the environment they expect.  */

  PDUMPER_RESET_LV (pending_funcalls, Qnil);
  PDUMPER_RESET_LV (unread_switch_frame, Qnil);
  PDUMPER_RESET_LV (internal_last_event_frame, Qnil);
  PDUMPER_RESET_LV (last_command_event, Qnil);
  PDUMPER_RESET_LV (last_nonmenu_event, Qnil);
  PDUMPER_RESET_LV (last_input_event, Qnil);
  PDUMPER_RESET_LV (Vunread_command_events, Qnil);
  PDUMPER_RESET_LV (Vunread_post_input_method_events, Qnil);
  PDUMPER_RESET_LV (Vunread_input_method_events, Qnil);
  PDUMPER_RESET_LV (Vthis_command, Qnil);
  PDUMPER_RESET_LV (Vreal_this_command, Qnil);
  PDUMPER_RESET_LV (Vthis_command_keys_shift_translated, Qnil);
  PDUMPER_RESET_LV (Vthis_original_command, Qnil);
  PDUMPER_RESET (num_input_keys, 0);
  PDUMPER_RESET (num_nonmacro_input_events, 0);
  PDUMPER_RESET_LV (Vlast_event_frame, Qnil);
  PDUMPER_RESET_LV (Vdeferred_action_list, Qnil);
  PDUMPER_RESET_LV (Vdelayed_warnings_list, Qnil);

  /* Create the initial keyboard.  Qt means 'unset'.  */
  eassert (initial_kboard == NULL);
  initial_kboard = allocate_kboard (Qt);

  Vwhile_no_input_ignore_events = Qnil;

  inhibit_record_char = false;
}

void
keys_of_keyboard (void)
{
  initial_define_key (global_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (control_x_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (meta_map, Ctl ('C'), "exit-recursive-edit");
  initial_define_key (global_map, Ctl (']'), "abort-recursive-edit");
  initial_define_key (meta_map, 'x', "execute-extended-command");

  initial_define_lispy_key (Vspecial_event_map, "delete-frame",
			    "handle-delete-frame");
#ifdef HAVE_NTGUI
  initial_define_lispy_key (Vspecial_event_map, "end-session",
			    "kill-emacs");
#endif
  initial_define_lispy_key (Vspecial_event_map, "ns-put-working-text",
			    "ns-put-working-text");
  initial_define_lispy_key (Vspecial_event_map, "ns-unput-working-text",
			    "ns-unput-working-text");
  /* Here we used to use `ignore-event' which would simple set prefix-arg to
     current-prefix-arg, as is done in `handle-switch-frame'.
     But `handle-switch-frame is not run from the special-map.
     Commands from that map are run in a special way that automatically
     preserves the prefix-arg.  Restoring the prefix arg here is not just
     redundant but harmful:
     - C-u C-x v =
     - current-prefix-arg is set to non-nil, prefix-arg is set to nil.
     - after the first prompt, the exit-minibuffer-hook is run which may
       iconify a frame and thus push a `iconify-frame' event.
     - after running exit-minibuffer-hook, current-prefix-arg is
       restored to the non-nil value it had before the prompt.
     - we enter the second prompt.
       current-prefix-arg is non-nil, prefix-arg is nil.
     - before running the first real event, we run the special iconify-frame
       event, but we pass the `special' arg to command-execute so
       current-prefix-arg and prefix-arg are left untouched.
     - here we foolishly copy the non-nil current-prefix-arg to prefix-arg.
     - the next key event will have a spuriously non-nil current-prefix-arg.  */
  initial_define_lispy_key (Vspecial_event_map, "iconify-frame",
			    "ignore");
  initial_define_lispy_key (Vspecial_event_map, "make-frame-visible",
			    "ignore");
  /* Handling it at such a low-level causes read_key_sequence to get
   * confused because it doesn't realize that the current_buffer was
   * changed by read_char.
   *
   * initial_define_lispy_key (Vspecial_event_map, "select-window",
   * 			    "handle-select-window"); */
  initial_define_lispy_key (Vspecial_event_map, "save-session",
			    "handle-save-session");

#ifdef HAVE_DBUS
  /* Define a special event which is raised for dbus callback
     functions.  */
  initial_define_lispy_key (Vspecial_event_map, "dbus-event",
			    "dbus-handle-event");
#endif

#ifdef THREADS_ENABLED
  /* Define a special event which is raised for thread signals.  */
  initial_define_lispy_key (Vspecial_event_map, "thread-event",
			    "thread-handle-event");
#endif

#ifdef USE_FILE_NOTIFY
  /* Define a special event which is raised for notification callback
     functions.  */
  initial_define_lispy_key (Vspecial_event_map, "file-notify",
                            "file-notify-handle-event");
#endif /* USE_FILE_NOTIFY */

  initial_define_lispy_key (Vspecial_event_map, "config-changed-event",
			    "ignore");
#if defined (WINDOWSNT)
  initial_define_lispy_key (Vspecial_event_map, "language-change",
			    "ignore");
#endif
  initial_define_lispy_key (Vspecial_event_map, "focus-in",
			    "handle-focus-in");
  initial_define_lispy_key (Vspecial_event_map, "focus-out",
			    "handle-focus-out");
  initial_define_lispy_key (Vspecial_event_map, "move-frame",
			    "handle-move-frame");
}

/* Mark the pointers in the kboard objects.
   Called by Fgarbage_collect.  */
void
mark_kboards (void)
{
  KBOARD *kb;
  Lisp_Object *p;
  for (kb = all_kboards; kb; kb = kb->next_kboard)
    {
      if (kb->kbd_macro_buffer)
        for (p = kb->kbd_macro_buffer; p < kb->kbd_macro_ptr; p++)
          mark_object (*p);
      mark_object (KVAR (kb, Voverriding_terminal_local_map));
      mark_object (KVAR (kb, Vlast_command));
      mark_object (KVAR (kb, Vreal_last_command));
      mark_object (KVAR (kb, Vkeyboard_translate_table));
      mark_object (KVAR (kb, Vlast_repeatable_command));
      mark_object (KVAR (kb, Vprefix_arg));
      mark_object (KVAR (kb, Vlast_prefix_arg));
      mark_object (KVAR (kb, kbd_queue));
      mark_object (KVAR (kb, defining_kbd_macro));
      mark_object (KVAR (kb, Vlast_kbd_macro));
      mark_object (KVAR (kb, Vsystem_key_alist));
      mark_object (KVAR (kb, system_key_syms));
      mark_object (KVAR (kb, Vwindow_system));
      mark_object (KVAR (kb, Vinput_decode_map));
      mark_object (KVAR (kb, Vlocal_function_key_map));
      mark_object (KVAR (kb, Vdefault_minibuffer_frame));
      mark_object (KVAR (kb, echo_string));
      mark_object (KVAR (kb, echo_prompt));
    }

  for (union buffered_input_event *event = kbd_fetch_ptr;
       event != kbd_store_ptr; event = next_kbd_event (event))
    {
      /* These two special event types have no Lisp_Objects to mark.  */
      if (event->kind != SELECTION_REQUEST_EVENT
	  && event->kind != SELECTION_CLEAR_EVENT)
	{
	  mark_object (event->ie.x);
	  mark_object (event->ie.y);
	  mark_object (event->ie.frame_or_window);
	  mark_object (event->ie.arg);
	}
    }
}
