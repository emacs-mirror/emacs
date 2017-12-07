/* A limited emulation of sys/resource.h.

Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

#ifndef INC_SYS_RESOURCE_H_
#define INC_SYS_RESOURCE_H_

/* We only support RLIMIT_STACK and RLIMIT_NOFILE for now.  */
enum rlimit_resource {
  RLIMIT_STACK = 0,
#define RLIMIT_STACK RLIMIT_STACK

  RLIMIT_NOFILE = 1,
#define RLIMIT_NOFILE RLIMIT_NOFILE

  RLIMIT_NLIMITS
#define RLIMIT_NLIMITS RLIMIT_NLIMITS
};

typedef enum rlimit_resource rlimit_resource_t;

/* We use a 64-bit data type because some values could potentially be
   64-bit wide even in 32-bit builds.  */
typedef long long rlim_t;

#define RLIMIT_INFINITY ((rlim_t) -1)

struct rlimit {
  rlim_t rlim_cur;	/* current soft limit */
  rlim_t rlim_max;	/* hard limit */
};

extern int getrlimit (rlimit_resource_t, struct rlimit *);
extern int setrlimit (rlimit_resource_t, const struct rlimit *);

#endif	/* INC_SYS_RESOURCE_H_ */
