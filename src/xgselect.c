/* Function for handling the GLib event loop.

Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

#include "xgselect.h"

#ifdef HAVE_GLIB

#include <glib.h>
#include <errno.h>
#include "lisp.h"
#include "blockinput.h"
#include "systime.h"

/* `xg_select' is a `pselect' replacement.  Why do we need a separate function?
   1. Timeouts.  Glib and Gtk rely on timer events.  If we did pselect
      with a greater timeout then the one scheduled by Glib, we would
      not allow Glib to process its timer events.  We want Glib to
      work smoothly, so we need to reduce our timeout to match Glib.
   2. Descriptors.  Glib may listen to more file descriptors than we do.
      So we add Glib descriptors to our pselect pool, but we don't change
      the value returned by the function.  The return value  matches only
      the descriptors passed as arguments, making it compatible with
      plain pselect.  */

int
xg_select (int fds_lim, fd_set *rfds, fd_set *wfds, fd_set *efds,
	   struct timespec *timeout, sigset_t *sigmask)
{
  fd_set all_rfds, all_wfds;
  struct timespec tmo;
  struct timespec *tmop = timeout;

  GMainContext *context;
  bool have_wfds = wfds != NULL;
  GPollFD gfds_buf[128];
  GPollFD *gfds = gfds_buf;
  int gfds_size = ARRAYELTS (gfds_buf);
  int n_gfds, retval = 0, our_fds = 0, max_fds = fds_lim - 1;
  bool context_acquired = false;
  int i, nfds, tmo_in_millisec, must_free = 0;
  bool need_to_dispatch;

  context = g_main_context_default ();
  context_acquired = g_main_context_acquire (context);
  /* FIXME: If we couldn't acquire the context, we just silently proceed
     because this function handles more than just glib file descriptors.
     Note that, as implemented, this failure is completely silent: there is
     no feedback to the caller.  */

  if (rfds) all_rfds = *rfds;
  else FD_ZERO (&all_rfds);
  if (wfds) all_wfds = *wfds;
  else FD_ZERO (&all_wfds);

  n_gfds = (context_acquired
	    ? g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				    gfds, gfds_size)
	    : -1);

  if (gfds_size < n_gfds)
    {
      /* Avoid using SAFE_NALLOCA, as that implicitly refers to the
	 current thread.  Using xnmalloc avoids thread-switching
	 problems here.  */
      gfds = xnmalloc (n_gfds, sizeof *gfds);
      must_free = 1;
      gfds_size = n_gfds;
      n_gfds = g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				     gfds, gfds_size);
    }

  for (i = 0; i < n_gfds; ++i)
    {
      if (gfds[i].events & G_IO_IN)
        {
          FD_SET (gfds[i].fd, &all_rfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
        }
      if (gfds[i].events & G_IO_OUT)
        {
          FD_SET (gfds[i].fd, &all_wfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
          have_wfds = true;
        }
    }

  if (must_free)
    xfree (gfds);

  if (n_gfds >= 0 && tmo_in_millisec >= 0)
    {
      tmo = make_timespec (tmo_in_millisec / 1000,
			   1000 * 1000 * (tmo_in_millisec % 1000));
      if (!timeout || timespec_cmp (tmo, *timeout) < 0)
	tmop = &tmo;
    }

  fds_lim = max_fds + 1;
  nfds = thread_select (pselect, fds_lim,
			&all_rfds, have_wfds ? &all_wfds : NULL, efds,
			tmop, sigmask);
  if (nfds < 0)
    retval = nfds;
  else if (nfds > 0)
    {
      for (i = 0; i < fds_lim; ++i)
        {
          if (FD_ISSET (i, &all_rfds))
            {
              if (rfds && FD_ISSET (i, rfds)) ++retval;
              else ++our_fds;
            }
          else if (rfds)
            FD_CLR (i, rfds);

          if (have_wfds && FD_ISSET (i, &all_wfds))
            {
              if (wfds && FD_ISSET (i, wfds)) ++retval;
              else ++our_fds;
            }
          else if (wfds)
            FD_CLR (i, wfds);

          if (efds && FD_ISSET (i, efds))
            ++retval;
        }
    }

  /* If Gtk+ is in use eventually gtk_main_iteration will be called,
     unless retval is zero.  */
#ifdef USE_GTK
  need_to_dispatch = retval == 0;
#else
  need_to_dispatch = true;
#endif
  if (need_to_dispatch && context_acquired)
    {
      int pselect_errno = errno;
      /* Prevent g_main_dispatch recursion, that would occur without
         block_input wrapper, because event handlers call
         unblock_input.  Event loop recursion was causing Bug#15801.  */
      block_input ();
      while (g_main_context_pending (context))
        g_main_context_dispatch (context);
      unblock_input ();
      errno = pselect_errno;
    }

  if (context_acquired)
    g_main_context_release (context);

  /* To not have to recalculate timeout, return like this.  */
  if ((our_fds > 0 || (nfds == 0 && tmop == &tmo)) && (retval == 0))
    {
      retval = -1;
      errno = EINTR;
    }

  return retval;
}
#endif /* HAVE_GLIB */
