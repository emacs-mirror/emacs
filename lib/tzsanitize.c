/* Sanitizing the TZ environment variable on native Windows.
   Copyright (C) 2017-2026 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Bruno Haible.  */

#include <config.h>

/* Specification.  */
#include "tzsanitize.h"

#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#if !defined __MINGW32__ || defined _UCRT
# define WIN32_LEAN_AND_MEAN  /* avoid including junk */
# include <windows.h>
#endif

void
tzsanitize (void)
{
  const char *tz = getenv ("TZ");
  if (tz != NULL && strchr (tz, '/') != NULL)
    {
      /* Neutralize it, in a way that is thread-safe.
         (If we were to use _putenv ("TZ="), it would free the memory allocated
         for the environment variable "TZ", and thus other threads that are
         using the previously fetched value of getenv ("TZ") could crash.)  */
      char **env = _environ;
      wchar_t **wenv = _wenviron;
      if (env != NULL)
        for (char **ep = env; *ep != NULL; ep++)
          {
            char *s = *ep;
            if (s[0] == 'T' && s[1] == 'Z' && s[2] == '=')
              s[0] = '$';
          }
      if (wenv != NULL)
        for (wchar_t **wep = wenv; *wep != NULL; wep++)
          {
            wchar_t *ws = *wep;
            if (ws[0] == L'T' && ws[1] == L'Z' && ws[2] == L'=')
              ws[0] = L'$';
          }
#if !defined __MINGW32__ || defined _UCRT
      SetEnvironmentVariable ("TZ", NULL);
#endif
    }
#if !defined __MINGW32__ || defined _UCRT
  /* On MSVC, we also need to sanitize the environment variables stored in the
     process's environment block.
     We don't lose much by assuming that the environment variable's value
     is not too large.  */
  else
    {
      /* Documentation:
         <https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getenvironmentvariable>  */
      char tz_buf[256];
      DWORD tz_len = GetEnvironmentVariable ("TZ", tz_buf, sizeof (tz_buf));
      if (tz_len > 0 && tz_len < sizeof (tz_buf)
          && strchr (tz_buf, '/') != NULL)
        {
          /* Documentation:
             <https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setenvironmentvariable>  */
          SetEnvironmentVariable ("TZ", NULL);
        }
    }
#endif
}
