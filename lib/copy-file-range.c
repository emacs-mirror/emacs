/* Stub for copy_file_range
   Copyright 2019-2025 Free Software Foundation, Inc.

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

#include <config.h>

#include <unistd.h>

#include <errno.h>

#if defined __linux__ && HAVE_COPY_FILE_RANGE
# include <linux/version.h>
# include <sys/utsname.h>
/* Although it can be dicey to use static checks for Linux kernel versions,
   due to the dubious practice of building on newer kernels for older ones,
   do it here anyway as the buggy kernels are rare (they are all EOLed)
   and builders for them are unlikely to use the dubious practice.
   Circa 2029 we should remove the old-kernel workarounds entirely.  */
# if LINUX_VERSION_CODE < KERNEL_VERSION (5, 3, 0)
#  define CHECK_LINUX_KERNEL_VERSION true
# else
#  define CHECK_LINUX_KERNEL_VERSION false
# endif
#endif

#include "sys-limits.h"

ssize_t
copy_file_range (int infd, off_t *pinoff,
                 int outfd, off_t *poutoff,
                 size_t length, unsigned int flags)
{
#undef copy_file_range

#if HAVE_COPY_FILE_RANGE
  bool ok = true;

# if CHECK_LINUX_KERNEL_VERSION
  /* The implementation of copy_file_range (which first appeared in
     Linux kernel release 4.5) had many issues before release 5.3
     <https://lwn.net/Articles/789527/>, so fail with ENOSYS for Linux
     kernels 5.2 and earlier.

     This workaround can be removed when such kernels (released March
     2016 through September 2019) are no longer a consideration.
     Although all such kernels have reached EOL, some distros use
     older kernels.  For example, RHEL 8 uses kernel 4.18 and has an
     EOL of 2029.  */

  static signed char kernel_ok;
  if (! kernel_ok)
    {
      struct utsname name;
      uname (&name);
      char *p = name.release;
      kernel_ok = ((p[1] != '.' || '5' < p[0]
                    || (p[0] == '5' && (p[3] != '.' || '2' < p[2])))
                   ? 1 : -1);
    }

  if (kernel_ok < 0)
    ok = false;
# endif

  if (ok)
    {
# if defined __GLIBC__ && ! (2 < __GLIBC__ + (43 <= __GLIBC_MINOR__))
      /* Work around glibc bug 33245
         <https://sourceware.org/PR33245>.
         This bug is present in glibc 2.42 (2025) and fixed in 2.43,
         so this workaround, and the configure-time check for glibc,
         can be removed once glibc 2.42 and earlier is no longer a
         consideration.  Perhaps in 2040.  */
      if (SYS_BUFSIZE_MAX < length)
        length = SYS_BUFSIZE_MAX;
# endif

      return copy_file_range (infd, pinoff, outfd, poutoff, length, flags);
    }
#endif

  /* There is little need to emulate copy_file_range with read+write,
     since programs that use copy_file_range must fall back on
     read+write anyway.  */
  errno = ENOSYS;
  return -1;
}
