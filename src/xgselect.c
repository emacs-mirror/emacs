/* Function for handling the GLib event loop.

Copyright (C) 2009-2013 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include "xgselect.h"

#ifdef HAVE_GLIB

#include <glib.h>
#include <errno.h>
#include <timespec.h>
#include "frame.h"

int
xg_select (int fds_lim, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	   struct timespec const *timeout, sigset_t const *sigmask)
{
  SELECT_TYPE all_rfds, all_wfds;
  struct timespec tmo;
  struct timespec const *tmop = timeout;

  GMainContext *context;
  int have_wfds = wfds != NULL;
  GPollFD gfds_buf[128];
  GPollFD *gfds = gfds_buf;
  int gfds_size = sizeof gfds_buf / sizeof *gfds_buf;
  int n_gfds, retval = 0, our_fds = 0, max_fds = fds_lim - 1;
  int i, nfds, tmo_in_millisec;
  USE_SAFE_ALLOCA;

  /* Do not try to optimize with an initial check with g_main_context_pending
     and a call to pselect if it returns false.  If Gdk has a timeout for 0.01
     second, and Emacs has a timeout for 1 second, g_main_context_pending will
     return false, but the timeout will be 1 second, thus missing the gdk
     timeout with a lot.  */

  context = g_main_context_default ();

  if (rfds) all_rfds = *rfds;
  else FD_ZERO (&all_rfds);
  if (wfds) all_wfds = *wfds;
  else FD_ZERO (&all_wfds);

  n_gfds = g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				 gfds, gfds_size);
  if (gfds_size < n_gfds)
    {
      SAFE_NALLOCA (gfds, sizeof *gfds, n_gfds);
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
          have_wfds = 1;
        }
    }

  SAFE_FREE ();

  if (tmo_in_millisec >= 0)
    {
      tmo = make_timespec (tmo_in_millisec / 1000,
			   1000 * 1000 * (tmo_in_millisec % 1000));
      if (!timeout || timespec_cmp (tmo, *timeout) < 0)
	tmop = &tmo;
    }

  fds_lim = max_fds + 1;
  nfds = pselect (fds_lim, &all_rfds, have_wfds ? &all_wfds : NULL,
		  efds, tmop, sigmask);

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

  if (our_fds > 0 || (nfds == 0 && tmop == &tmo))
    {

      /* If Gtk+ is in use eventually gtk_main_iteration will be called,
         unless retval is zero.  */
#ifdef USE_GTK
      if (retval == 0)
#endif
        while (g_main_context_pending (context))
          g_main_context_dispatch (context);

      /* To not have to recalculate timeout, return like this.  */
      if (retval == 0)
        {
          retval = -1;
          errno = EINTR;
        }
    }

  return retval;
}
#endif /* HAVE_GLIB */
