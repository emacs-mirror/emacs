/* Time support for VMS.
   Copyright (C) 1993 Free Software Foundation.

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

#include <config.h>
#include "vmstime.h"

long timezone=0;
int daylight=0;

static char tzname_default[20]="";
static char tzname_dst[20]="";

char *tzname[2] = { tzname_default, tzname_dst };

static long internal_daylight=0;
static char daylight_set=0;

static long read_time(const char *nptr, const char **endptr,
		      int sign_allowed_p)
{
  int t;

  *endptr = nptr;

  /* This routine trusts the user very much, and does no checks!
      The only exception is this: */
  if (!sign_allowed_p && (*nptr == '-' || *nptr == '+'))
    return 0;

  t = strtol(*endptr, endptr, 10) * 3600;
  if (**endptr != ':' || **endptr == '+' || **endptr == '-')
    return t;
  (*endptr)++;

  t = t + strtol(*endptr, endptr, 10) * 60;
  if (**endptr != ':' || **endptr == '+' || **endptr == '-')
    return t;
  (*endptr)++;

  return t + strtol(*endptr, endptr, 10);
}

static void read_dst_time(const char *nptr, const char **endptr,
			  int *m, int *n, int *d,
			  int *leap_p)
{
  time_t bintim = time(0);
  struct tm *lc = localtime(&bintim);

  *leap_p = 1;
  *m = 0;			/* When m and n are 0, a Julian */
  *n = 0;			/* date has been inserted in d */

  switch(*nptr)
    {
    case 'M':
      {
	/* This routine counts on the user to have specified "Mm.n.d",
	   where 1 <= n <= 5, 1 <= m <= 12, 0 <= d <= 6 */

	*m = strtol(++nptr, endptr, 10);
	(*endptr)++;		/* Skip the dot */
	*n = strtol(*endptr, endptr, 10);
	(*endptr)++;		/* Skip the dot */
	*d = strtol(*endptr, endptr, 10);

	return;
      }
    case 'J':
      *leap_p = 0;		/* Never count with leap years */
    default: /* trust the user to have inserted a number! */
      *d = strtol(++nptr, endptr, 10);
      return;
    }
}

struct vms_vectim
{
  short year, month, day, hour, minute, second, centi_second;
};
static void find_dst_time(int m, int n, long d,
			  int hour, int minute, int second,
			  int leap_p,
			  long vms_internal_time[2])
{
  long status = SYS$GETTIM(vms_internal_time);
  struct vms_vectim vms_vectime;
  status = SYS$NUMTIM(&vms_vectime, vms_internal_time);

  if (m == 0 && n == 0)
    {
      long tmp_vms_internal_time[2][2];
      long day_of_year;
      long tmp_operation = LIB$K_DAY_OF_YEAR;

      status = LIB$CVT_FROM_INTERNAL_TIME(&tmp_operation, &day_of_year,
					  vms_internal_time);
      
      vms_vectime.month = 2;
      vms_vectime.day = 29;
      status = LIB$CVT_VECTIM(&vms_vectime, tmp_vms_internal_time[0]);
      if (status & 1) /* This is a leap year */
	{
	  if (!leap_p && d > 59)
	    d ++;		/* If we don't count with 29th Feb,
				   and this is a leap year, count up,
				   to make day 60 really become the
				   1st March. */
	}
      /* 1st January, at midnight */
      vms_vectime.month = 1;
      vms_vectime.day = 1;
      vms_vectime.hour = hour;
      vms_vectime.minute = minute;
      vms_vectime.second = second;
      vms_vectime.centi_second = 0;
      status = LIB$CVT_VECTIM(&vms_vectime, tmp_vms_internal_time[0]);
      tmp_operation = LIB$K_DELTA_DAYS;
      status = LIB$CVT_TO_INTERNAL_TIME(&tmp_operation, &d,
					tmp_vms_internal_time[1]);
      /* now, tmp_vms_interval_time[0] contains 1st Jan, 00:00:00,
	 and  tmp_vms_interval_time[1] contains delta time +d days.
	 Let's just add them together */
      status = LIB$ADD_TIMES(tmp_vms_internal_time[0],
			     tmp_vms_internal_time[1],
			     vms_internal_time);
    }
  else
    {
      long tmp_vms_internal_time[2];
      long day_of_week;
      long tmp_operation = LIB$K_DAY_OF_YEAR;

      if (d == 0) /* 0 is Sunday, which isn't compatible with VMS,
		     where day_of_week is 1 -- 7, and 1 is Monday */
	{
	  d = 7; /* So a simple conversion is required */
	}
      vms_vectime.month = m;
      vms_vectime.day = 1;
      vms_vectime.hour = hour;
      vms_vectime.minute = minute;
      vms_vectime.second = second;
      vms_vectime.centi_second = 0;
      status = LIB$CVT_VECTIM(&vms_vectime, tmp_vms_internal_time);
      tmp_operation = LIB$K_DAY_OF_WEEK;
      status = LIB$CVT_FROM_INTERNAL_TIME(&tmp_operation, &day_of_week,
					  tmp_vms_internal_time);
      d -= day_of_week;
      if (d < 0)
	{
	  d += 7;
	}
      vms_vectime.day += (n-1)*7 + d;
      status = LIB$CVT_VECTIM(&vms_vectime, vms_internal_time);
      if (!(status & 1))
	{
	  vms_vectime.day -= 7;	/* n was probably 5 */
	  status = LIB$CVT_VECTIM(&vms_vectime, vms_internal_time);
	}
    }
}

static cmp_vms_internal_times(long vms_internal_time1[2],
			      long vms_internal_time2[2])
{
  if (vms_internal_time1[1] < vms_internal_time2[1])
    return -1;
  else
    if (vms_internal_time1[1] > vms_internal_time2[1])
      return 1;

  if (vms_internal_time1[0] < vms_internal_time2[0])
    return -1;
  else
    if (vms_internal_time1[0] > vms_internal_time2[0])
      return 1;

  return 0;
}

/* -------------------------- Global routines ------------------------------ */

#ifdef tzset
#undef tzset
#endif
void sys_tzset()
{
  char *TZ;
  char *p, *q;

  if (daylight_set)
    return;

  daylight = 0;

  if ((TZ = getenv("TZ")) == 0)
    return;

  p = TZ;
  q = tzname[0];

  while(*p != '\0'
	&& (*p <'0' || *p > '9') && *p != '-' && *p != '+' && *p != ',')
    *q++ = *p++;
  *q = '\0';

  /* This is special for VMS, so I don't care if it doesn't exist anywhere
     else */

  timezone = read_time(p, &p, 1);

  q = tzname[1];

  while(*p != '\0'
	&& (*p <'0' || *p > '9') && *p != '-' && *p != '+' && *p != ',')
    *q++ = *p++;
  *q = '\0';

  if (*p != '-' && *p != '+' && !(*p >='0' && *p <= '9'))
    internal_daylight = timezone - 3600;
  else
    internal_daylight = read_time(p, &p, 1);

  if (*p == ',')
    {
      int start_m;
      int start_n;
      int start_d;
      int start_leap_p;
      int start_hour=2, start_minute=0, start_second=0;

      p++;
      read_dst_time(p, &p, &start_m, &start_n, &start_d, &start_leap_p);
      if (*p == '/')
	{
	  long tmp = read_time (++p, &p, 0);
	  start_hour = tmp / 3600;
	  start_minute = (tmp % 3600) / 60;
	  start_second = tmp % 60;
	}
      if (*p == ',')
	{
	  int end_m;
	  int end_n;
	  int end_d;
	  int end_leap_p;
	  int end_hour=2, end_minute=0, end_second=0;

	  p++;
	  read_dst_time(p, &p, &end_m, &end_n, &end_d, &end_leap_p);
	  if (*p == '/')
	    {
	      long tmp = read_time (++p, &p, 0);
	      end_hour = tmp / 3600;
	      end_minute = (tmp % 3600) / 60;
	      end_second = tmp % 60;
	    }
	  {
	    long vms_internal_time[3][2];
	    find_dst_time(start_m, start_n, start_d,
			  start_hour, start_minute, start_second,
			  start_leap_p,
			  vms_internal_time[0]);
	    SYS$GETTIM(&vms_internal_time[1]);
	    find_dst_time(end_m, end_n, end_d,
			  end_hour, end_minute, end_second,
			  end_leap_p,
			  vms_internal_time[2]);
	    if (cmp_vms_internal_times(vms_internal_time[0],
				      vms_internal_time[1]) < 0
		&& cmp_vms_internal_times(vms_internal_time[1],
					 vms_internal_time[2]) < 0)
	      daylight = 1;
	  }
	}
    }
}  

#ifdef localtime
#undef localtime
#endif
struct tm *sys_localtime(time_t *clock)
{
  struct tm *tmp = localtime(clock);

  sys_tzset();
  tmp->tm_isdst = daylight;

  return tmp;
}

#ifdef gmtime
#undef gmtime
#endif
struct tm *sys_gmtime(time_t *clock)
{
  static struct tm gmt;
  struct vms_vectim tmp_vectime;
  long vms_internal_time[3][2];
  long tmp_operation = LIB$K_DELTA_SECONDS;
  long status;
  long tmp_offset;
  char tmp_o_sign;

  sys_tzset();
  
  if (daylight)
    tmp_offset = internal_daylight;
  else
    tmp_offset = timezone;

  if (tmp_offset < 0)
    {
      tmp_o_sign = -1;
      tmp_offset = -tmp_offset;
    }
  else
    tmp_o_sign = 1;

  status = LIB$CVT_TO_INTERNAL_TIME(&tmp_operation, &tmp_offset,
				    vms_internal_time[1]);
  status = SYS$GETTIM(vms_internal_time[0]);
  if (tmp_o_sign < 0)
    {
      status = LIB$SUB_TIMES(vms_internal_time[0],
			     vms_internal_time[1],
			     vms_internal_time[2]);
    }
  else
    {
      status = LIB$ADD_TIMES(vms_internal_time[0],
			     vms_internal_time[1],
			     vms_internal_time[2]);
    }

  status = SYS$NUMTIM(&tmp_vectime, vms_internal_time[2]);
  gmt.tm_sec = tmp_vectime.second;
  gmt.tm_min = tmp_vectime.minute;
  gmt.tm_hour = tmp_vectime.hour;
  gmt.tm_mday = tmp_vectime.day;
  gmt.tm_mon = tmp_vectime.month - 1;
  gmt.tm_year = tmp_vectime.year % 100;

  tmp_operation = LIB$K_DAY_OF_WEEK;
  status = LIB$CVT_FROM_INTERNAL_TIME(&tmp_operation,
				      &gmt.tm_wday,
				      vms_internal_time[2]);
  if (gmt.tm_wday == 7) gmt.tm_wday = 0;

  tmp_operation = LIB$K_DAY_OF_YEAR;
  status = LIB$CVT_FROM_INTERNAL_TIME(&tmp_operation,
				      &gmt.tm_yday,
				      vms_internal_time[2]);
  gmt.tm_yday--;
  gmt.tm_isdst = daylight;

  return &gmt;
}

