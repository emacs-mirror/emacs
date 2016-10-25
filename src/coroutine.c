/* Coroutine support for GNU Emacs Lisp.

Copyright (C) 2016 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>

#include <limits.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "lisp.h"

#include "verify.h"
#include "sysselect.h"
#include "libtask/task.h"

#if __has_attribute (cleanup)
enum { coroutine_has_cleanup = true };
#else
enum { coroutine_has_cleanup = false };
#endif

#define COROUTINE_HANDLE_NONLOCAL_EXIT(retval)                        \
  COROUTINE_SETJMP (CONDITION_CASE, coroutine_handle_signal, retval); \
  COROUTINE_SETJMP (CATCHER_ALL, coroutine_handle_throw, retval)

#define COROUTINE_SETJMP(handlertype, handlerfunc, retval)              \
  COROUTINE_SETJMP_1 (handlertype, handlerfunc, retval,                 \
                      internal_handler_##handlertype,                   \
                      internal_cleanup_##handlertype)

#define COROUTINE_SETJMP_1(handlertype, handlerfunc, retval, c, dummy)	\
  struct handler *c = push_handler_nosignal (Qt, handlertype);		\
  if (!c)								\
    {									\
      coroutine_out_of_memory ();                                       \
      return retval;							\
    }									\
  verify (coroutine_has_cleanup);                                       \
  int dummy __attribute__ ((cleanup (coroutine_reset_handlerlist)));	\
  if (sys_setjmp (c->jmp))						\
    {									\
      (handlerfunc) (c->val);                                           \
      return retval;							\
    }									\
  do { } while (false)

/* Must be called after setting up a handler immediately before
   returning from the function.  See the comments in lisp.h and the
   code in eval.c for details.  The macros below arrange for this
   function to be called automatically.  DUMMY is ignored.  */
static void
coroutine_reset_handlerlist (const int *dummy)
{
  handlerlist = handlerlist->next;
}

static bool pending_error;

/* Called on `signal'.  ERR is a pair (SYMBOL . DATA).  */
static void
coroutine_handle_signal (Lisp_Object err)
{
  eassert (! pending_error);
  print_error_message (err, Qnil, "signal in coroutine caught: ", Qnil);
  pending_error = true;
  Vpending_coroutine_error_symbol = XCAR (err);
  Vpending_coroutine_error_data = XCDR (err);
  taskexit (1);
}

/* Called on `throw'.  TAG_VAL is a pair (TAG . VALUE).  */
static void
coroutine_handle_throw (Lisp_Object tag_val)
{
  eassert (! pending_error);
  print_error_message (tag_val, Qnil, "throw in coroutine caught: ", Qnil);
  pending_error = true;
  Vpending_coroutine_error_symbol = Qno_catch;
  Vpending_coroutine_error_data = list2 (XCAR (tag_val), XCDR (tag_val));
  taskexit (1);
}

static void
coroutine_out_of_memory (void)
{
  eassert (! pending_error);
  fputs ("OOM in coroutine\n", stderr);
  pending_error = true;
  Vpending_coroutine_error_symbol = Qnil;
  Vpending_coroutine_error_data = Vmemory_signal_data;
  taskexit (1);
}

struct task_arguments
{
  Lisp_Object function;
};

static void
task_function (void *arg)
{
  struct task_arguments args = *(struct task_arguments *) arg;
  xfree (arg);
  COROUTINE_HANDLE_NONLOCAL_EXIT ();
  ptrdiff_t count = SPECPDL_INDEX ();
  specbind (Qinternal_interpreter_environment, Vinternal_interpreter_environment);
  call0 (args.function);
  unbind_to (count, Qnil);
}

DEFUN ("start-coroutine", Fstart_coroutine, Sstart_coroutine, 1, 1, 0,
       doc: /* Start FUNCTION in a background coroutine.  */)
  (Lisp_Object function)
{
  // args will be freed in task_function.
  struct task_arguments *args = xmalloc (sizeof *args);
  args->function = function;
  taskcreate (task_function, args, 0x400000);
  return Qnil;
}

static unsigned int main_task_id;

static bool
in_main_task (void)
{
  return taskid () == main_task_id;
}

static void
check_coroutine_signal (void)
{
  if (! pending_error)
    return;
  if (in_main_task ())
    {
      pending_error = false;
      Lisp_Object symbol = Vpending_coroutine_error_symbol;
      Lisp_Object data = Vpending_coroutine_error_data;
      eassert (SYMBOLP (symbol) || CONSP (data));
      Vpending_coroutine_error_symbol = Qnil;
      Vpending_coroutine_error_data = Qnil;
      xsignal (symbol, data);
    }
  else
    {
      int woken = taskyield ();
      eassert (woken > 0);
      eassert (! pending_error);
    }
}

static void
CHECK_CHANNEL (Lisp_Object x)
{
  CHECK_TYPE (CHANNELP (x), Qchannelp, x);
}

DEFUN ("channelp", Fchannelp, Schannelp, 1, 1, 0,
       doc: /* Return t if OBJECT is a communication channel, nil otherwise.  */)
  (Lisp_Object object)
{
  return CHANNELP (object) ? Qt : Qnil;
}

DEFUN ("receive-from-channel", Freceive_from_channel, Sreceive_from_channel,
       1, 1, 0,
       doc: /* Receive an object from CHANNEL.
Block the current coroutine until an object is available on CHANNEL,
then return that object and remove it from CHANNEL.
CHANNEL must be a communication channel created by `make-channel'.  */)
  (Lisp_Object channel)
{
  CHECK_CHANNEL (channel);
  Lisp_Object result;
  int status = chanrecv (XCHANNEL (channel)->channel, &result);
  eassert (status == 1);
  check_coroutine_signal ();
  return result;
}

DEFUN ("try-receive-from-channel", Ftry_receive_from_channel, Stry_receive_from_channel,
       1, 1, 0,
       doc: /* Attempt to receive an object from CHANNEL.
If an object can be read from CHANNEL, return a singleton list containing
the object and remove it from CHANNEL.  Otherwise, return nil.
CHANNEL must be a communication channel created by `make-channel'.   */)
  (Lisp_Object channel)
{
  CHECK_CHANNEL (channel);
  Lisp_Object result;
  int status = channbrecv (XCHANNEL (channel)->channel, &result);
  return (status == 1) ? list1 (result) : Qnil;
}

DEFUN ("send-to-channel", Fsend_to_channel, Ssend_to_channel,
       2, 2, 0,
       doc: /* Send an object to CHANNEL.
Block the current coroutine until an object can be put into CHANNEL,
then put VALUE into CHANNEL.  CHANNEL must be a communication channel
created by `make-channel'.  */)
  (Lisp_Object channel, Lisp_Object value)
{
  CHECK_CHANNEL (channel);
  int status = chansend (XCHANNEL (channel)->channel, &value);
  eassert (status == 1);
  check_coroutine_signal ();
  return Qnil;
}

DEFUN ("try-send-to-channel", Ftry_send_to_channel, Stry_send_to_channel,
       2, 2, 0,
       doc: /* Attempt to send an object to CHANNEL.
If an object can be sent to channel CHANNEL, put VALUE into CHANNEL
and return t, otherwise return nil.  CHANNEL must be a communication
channel created by `make-channel'. */)
  (Lisp_Object channel, Lisp_Object value)
{
  CHECK_CHANNEL (channel);
  int status = channbsend (XCHANNEL (channel)->channel, &value);
  return (status == 1) ? Qt : Qnil;
}

DEFUN ("select", Fselect, Sselect, 0, UNEVALLED, 0,
       doc: /* Synchronously multiplex between ALTERNATIVES.
ALTERNATIVES must be a list of communication alternatives.
Each alternative must be of one of the four following forms:
1. ((receive CHANNEL) BODY...)
2. ((receive CHANNEL SYMBOL) BODY...)
3. ((send CHANNEL VALUE) BODY...)
4. (default BODY...)
At most one `default' form may be present.

`select' checks for each of the alternative whether the respective
communication operation can progress.  If there is at least one such
operation, one of the available ones is randomly selected and the
respective BODY forms are evaluated.  If there is no such operation
(all operations would block), `select' either blocks until at least
one operation can make progress (if no `default' alternative is
given), or evaluates the BODY of the `default' alternative.

The first two forms attempt to receive a value from CHANNEL (which is
evaluated), as if by `receive-from-channel'.  If SYMBOL is given and
not nil, it is interpreted as an unevaluated symbol and bound to the
value that has been received from CHANNEL within BODY, otherwise the
value from CHANNEL is ignored.

The third form attempts to send VALUE to CHANNEL, as if by
`send-to-channel'.  Both VALUE and CHANNEL are evaluated.

The value of the last form of the BODY of the alternative being chosen
is returned.  */)
  (Lisp_Object alternatives)
{
  CHECK_LIST (alternatives);
  Lisp_Object length = Flength (alternatives);

  // libtask uses signed integers for counts, therefore we restrict
  // the number of alternatives to INT_MAX.  Because we allocate one
  // more Alt object for the delimiter, actually use INT_MAX − 1.
  CHECK_RANGED_INTEGER (length, 0, INT_MAX - 1);
  int num_alts = XINT (length);

  // Allocate array of alternatives given to chanalt below.
  // alts[num_alts] is the delimiter (CHANEND or CHANNOBLK, depending
  // on whether a default alternative is present).
  Alt *alts = xmalloc ((num_alts + 1) * sizeof *alts);

  // The objects in this structure correspond to VALUE, SYMBOL, and
  // BODY in the docstring, possibly evaluated.  The body for a
  // default alternative, if present, is stored elsewhere.
  struct {
    Lisp_Object value;
    Lisp_Object symbol;
    Lisp_Object body;
  } *comms = xmalloc (num_alts * sizeof *comms);

  bool has_default = false;
  Lisp_Object default_body;

  // Fill in the contents of alts and comms.
  int i = 0;
  for (Lisp_Object rest = alternatives; ! NILP (rest); rest = CDR (rest))
    {
      eassert (i < num_alts);
      CHECK_CONS (rest);
      Lisp_Object alternative = XCAR (rest);
      CHECK_CONS (alternative);
      Lisp_Object comm = XCAR (alternative);
      Lisp_Object body = XCDR (alternative);
      CHECK_LIST (body);
      if (EQ (comm, Qdefault))
        {
          if (has_default)
            signal_error ("Cannot have two default alternatives", alternatives);
          has_default = true;
          default_body = body;
          // As a default alternative is allowed anywhere, simply skip
          // it here.
          alts[i].op = CHANNOP;
        }
      else
        {
          CHECK_CONS (comm);
          Lisp_Object operation = XCAR (comm);
          Lisp_Object channel_arg = XCDR (comm);
          CHECK_CONS (channel_arg);
          Lisp_Object channel = eval_sub (XCAR (channel_arg));
          CHECK_CHANNEL (channel);
          alts[i].c = XCHANNEL (channel)->channel;
          Lisp_Object rest = XCDR (channel_arg);
          CHECK_LIST (rest);
          if (CONSP (rest) && ! NILP (XCDR (rest)))
            signal_error ("Communication specification must have at most three elements", comm);
          if (EQ (operation, Qreceive))
            {
              alts[i].op = CHANRCV;
              Lisp_Object var = NILP (rest) ? Qnil : XCAR (rest);
              CHECK_SYMBOL (var);
              // We don’t care whether VAR refers to a constant here,
              // `let' below will do it.
              comms[i].value = Qnil;
              comms[i].symbol = var;
              // If alts[i].v is NULL, chanalt will throw away the
              // received value.
              alts[i].v = NILP (var) ? NULL : &comms[i].value;
            }
          else if (EQ (operation, Qsend))
            {
              alts[i].op = CHANSND;
              CHECK_CONS (rest);
              comms[i].value = eval_sub (XCAR (rest));
              alts[i].v = &comms[i].value;
            }
          else
            wrong_choice (list2 (Qreceive, Qsend), operation);
          comms[i].body = body;
        }
      i++;
    }
  eassert (i == num_alts);
  alts[num_alts].op = has_default ? CHANNOBLK : CHANEND;

  // Actually perform the select operation.
  int choice = chanalt (alts);
  check_coroutine_signal ();

  if (choice == -1)
    {
      // Default alternative has been chosen.
      eassert (has_default);
      eassert (alts[num_alts].op == CHANNOBLK);
      return Fprogn (default_body);
    }
  else
    {
      // Non-default alternative has been chosen.
      eassert (choice >= 0 && choice < num_alts);
      Lisp_Object body = comms[choice].body;
      if (alts[choice].op == CHANRCV && ! NILP (comms[choice].symbol))
        {
          Lisp_Object symbol = comms[choice].symbol;
          Lisp_Object value = comms[choice].value;
          return Flet (Fcons (list1 (list2 (symbol, value)), body));
        }
      else
        return Fprogn (body);
    }
}

struct pselect_args {
  int nfds;
  fd_set *readfds;
  fd_set *writefds;
  fd_set *errorfds;
  const struct timespec *timeout;
  const sigset_t *sigmask;
  Rendez rendez;
  int result;
};

static void
do_pselect (void *arg)
{
  struct pselect_args *args = arg;
  args->result = pselect (args->nfds,
                          args->readfds, args->writefds, args->errorfds,
                          args->timeout, args->sigmask);
  int woken = taskwakeup (&args->rendez);
  eassert (woken == 1);
}

// Non-blocking variant of pselect.  Uses a task to run in the
// background.
int
pselect_noblock (int nfds,
                 fd_set *restrict readfds,
                 fd_set *restrict writefds,
                 fd_set *restrict errorfds,
                 const struct timespec *restrict timeout,
                 const sigset_t *restrict sigmask)
{
  struct pselect_args args = {
    .nfds = nfds,
    .readfds = readfds,
    .writefds = writefds,
    .errorfds = errorfds,
    .timeout = timeout,
    .sigmask = sigmask,
  };
  taskcreate (do_pselect, &args, 0x1000);
  tasksleep (&args.rendez);
  check_coroutine_signal ();
  return args.result;
}

void
coroutine_init (void)
{
  main_task_id = taskid ();
  pending_error = false;
}

void
syms_of_coroutine (void)
{
  DEFSYM (Qchannelp, "channelp");
  DEFSYM (Qreceive, "receive");
  DEFSYM (Qsend, "send");

  DEFVAR_LISP ("pending-coroutine-error-symbol", Vpending_coroutine_error_symbol,
               doc: /* Pending error symbol. */);
  DEFSYM (Qpending_coroutine_error_symbol, "pending-coroutine-error-symbol");
  Funintern (Qpending_coroutine_error_symbol, Qnil);

  DEFVAR_LISP ("pending-coroutine-error-data", Vpending_coroutine_error_data,
               doc: /* Pending error data. */);
  DEFSYM (Qpending_coroutine_error_data, "pending-coroutine-error-data");
  Funintern (Qpending_coroutine_error_data, Qnil);

  defsubr (&Sstart_coroutine);
  defsubr (&Schannelp);
  defsubr (&Sreceive_from_channel);
  defsubr (&Stry_receive_from_channel);
  defsubr (&Ssend_to_channel);
  defsubr (&Stry_send_to_channel);
  defsubr (&Sselect);
}
