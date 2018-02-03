/* Common functions for the Microsoft Windows and Cygwin builds.

Copyright (C) 2018 Free Software Foundation, Inc.

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

#include <stdio.h>

#include "lisp.h"
#include "w32common.h"

DEFUN ("w32-battery-status", Fw32_battery_status, Sw32_battery_status, 0, 0, 0,
       doc: /* Get power status information from Windows system.

The following %-sequences are provided:
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%s Remaining time (to charge or discharge) in seconds
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'  */)
  (void)
{
  Lisp_Object status = Qnil;

  SYSTEM_POWER_STATUS system_status;
  if (GetSystemPowerStatus (&system_status))
    {
      Lisp_Object line_status, battery_status, battery_status_symbol;
      Lisp_Object load_percentage, seconds, minutes, hours, remain;

      long seconds_left = (long) system_status.BatteryLifeTime;

      if (system_status.ACLineStatus == 0)
	line_status = build_string ("off-line");
      else if (system_status.ACLineStatus == 1)
	line_status = build_string ("on-line");
      else
	line_status = build_string ("N/A");

      if (system_status.BatteryFlag & 128)
	{
	  battery_status = build_string ("N/A");
	  battery_status_symbol = empty_unibyte_string;
	}
      else if (system_status.BatteryFlag & 8)
	{
	  battery_status = build_string ("charging");
	  battery_status_symbol = build_string ("+");
	  if (system_status.BatteryFullLifeTime != -1L)
	    seconds_left = system_status.BatteryFullLifeTime - seconds_left;
	}
      else if (system_status.BatteryFlag & 4)
	{
	  battery_status = build_string ("critical");
	  battery_status_symbol = build_string ("!");
	}
      else if (system_status.BatteryFlag & 2)
	{
	  battery_status = build_string ("low");
	  battery_status_symbol = build_string ("-");
	}
      else if (system_status.BatteryFlag & 1)
	{
	  battery_status = build_string ("high");
	  battery_status_symbol = empty_unibyte_string;
	}
      else
	{
	  battery_status = build_string ("medium");
	  battery_status_symbol = empty_unibyte_string;
	}

      if (system_status.BatteryLifePercent > 100)
	load_percentage = build_string ("N/A");
      else
	{
	  char buffer[16];
	  snprintf (buffer, 16, "%d", system_status.BatteryLifePercent);
	  load_percentage = build_string (buffer);
	}

      if (seconds_left < 0)
	seconds = minutes = hours = remain = build_string ("N/A");
      else
	{
	  long m;
	  double h;
	  char buffer[16];
	  snprintf (buffer, 16, "%ld", seconds_left);
	  seconds = build_string (buffer);

	  m = seconds_left / 60;
	  snprintf (buffer, 16, "%ld", m);
	  minutes = build_string (buffer);

	  h = seconds_left / 3600.0;
	  snprintf (buffer, 16, "%3.1f", h);
	  hours = build_string (buffer);

	  snprintf (buffer, 16, "%ld:%02ld", m / 60, m % 60);
	  remain = build_string (buffer);
	}

      status = listn (CONSTYPE_HEAP, 8,
		      Fcons (make_number ('L'), line_status),
		      Fcons (make_number ('B'), battery_status),
		      Fcons (make_number ('b'), battery_status_symbol),
		      Fcons (make_number ('p'), load_percentage),
		      Fcons (make_number ('s'), seconds),
		      Fcons (make_number ('m'), minutes),
		      Fcons (make_number ('h'), hours),
		      Fcons (make_number ('t'), remain));
    }
  return status;
}

void
syms_of_w32cygwinx (void)
{
  defsubr (&Sw32_battery_status);
}
