/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
#include <signal.h>
#include <errno.h>

int
sigaction(int _sig, const struct sigaction *_act, struct sigaction *_oact)
{
  int retval = 0;

  if (_oact)
  {
    void (*installed_sig)(int) = signal (_sig, SIG_IGN);

    /* FIXME */
    if (installed_sig == SIG_ERR)
    {
      retval = -1;
      errno = EINVAL;
    }
    else
      signal (_sig, installed_sig);
    _oact->sa_handler = installed_sig;
    retval = sigemptyset (&_oact->sa_mask);
    _oact->sa_flags = 0;
  }
  if (_act)
  {
    if (signal (_sig, _act->sa_handler) == SIG_ERR)
    {
      retval = -1;
      errno = EINVAL;
    }
  }
  return 0;
}



