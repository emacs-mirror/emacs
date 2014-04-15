/* Function for handling the GLib event loop.

Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
#include <stdbool.h>
#include <timespec.h>
#include "frame.h"

int
xg_select (int fds_lim, fd_set *rfds, fd_set *wfds, fd_set *efds,
	   struct timespec const *timeout, sigset_t const *sigmask)
{
  fd_set all_rfds, all_wfds, all_efds;
  struct timespec tmo;
  struct timespec const *tmop = timeout;

  GMainContext *context;
  GPollFD gfds_buf[128];
  GPollFD *gfds = gfds_buf;
  int gfds_size = sizeof gfds_buf / sizeof *gfds_buf;
  int n_gfds, retval = 0, all_lim = fds_lim;
  int i, nfds, tmo_in_millisec;
  bool need_to_dispatch;
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
  if (efds) all_efds = *efds;
  else FD_ZERO (&all_efds);

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
    if (gfds[i].events & (G_IO_IN | G_IO_OUT | G_IO_PRI))
      {
	int fd = gfds[i].fd;
	for (; all_lim <= fd; all_lim++)
	  {
	    FD_CLR (all_lim, &all_rfds);
	    FD_CLR (all_lim, &all_wfds);
	    FD_CLR (all_lim, &all_efds);
	  }
	if (gfds[i].events & G_IO_IN)
	  FD_SET (fd, &all_rfds);
	if (gfds[i].events & G_IO_OUT)
	  FD_SET (fd, &all_wfds);
	if (gfds[i].events & G_IO_PRI)
	  FD_SET (fd, &all_efds);
      }

  SAFE_FREE ();

  if (tmo_in_millisec >= 0)
    {
      tmo = make_timespec (tmo_in_millisec / 1000,
			   1000 * 1000 * (tmo_in_millisec % 1000));
      if (!timeout || timespec_cmp (tmo, *timeout) < 0)
	tmop = &tmo;
    }

  nfds = pselect (all_lim, &all_rfds, &all_wfds, &all_efds, tmop, sigmask);

  if (nfds < 0)
    retval = nfds;
  else
    {
      for (i = 0; i < fds_lim; ++i)
        {
	  if (rfds && FD_ISSET (i, rfds))
	    {
	      if (FD_ISSET (i, &all_rfds))
		retval++;
	      else
		FD_CLR (i, rfds);
	    }
	  if (wfds && FD_ISSET (i, wfds))
	    {
	      if (FD_ISSET (i, &all_wfds))
		retval++;
	      else
		FD_CLR (i, wfds);
	    }
	  if (efds && FD_ISSET (i, efds))
	    {
	      if (FD_ISSET (i, &all_efds))
		retval++;
	      else
		FD_CLR (i, efds);
	    }
        }
    }

  /* If Gtk+ is in use eventually gtk_main_iteration will be called,
     unless retval is zero.  */
#ifdef USE_GTK
  need_to_dispatch = retval == 0;
#else
  need_to_dispatch = true;
#endif
  if (need_to_dispatch)
    {
      int pselect_errno = errno;
      while (g_main_context_pending (context))
	g_main_context_dispatch (context);
      errno = pselect_errno;
    }

  /* To not have to recalculate timeout, return like this.  */
  if (retval == 0 && (0 < nfds || tmop == &tmo))
    {
      retval = -1;
      errno = EINTR;
    }

  return retval;
}
#endif /* HAVE_GLIB */
