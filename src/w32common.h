/* Common functions for Microsoft Windows builds of Emacs
   Copyright (C) 2012-2020 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

*/

#ifndef W32COMMON_H
#define W32COMMON_H

#include <windows.h>

#define ROUND_UP(p, align)   (((DWORD_PTR)(p) + (align)-1) & ~((DWORD_PTR)(align)-1))
#define ROUND_DOWN(p, align) ((DWORD_PTR)(p) & ~((DWORD_PTR)(align)-1))

#define get_page_size()			sysinfo_cache.dwPageSize
#define get_allocation_unit()		sysinfo_cache.dwAllocationGranularity
#define get_processor_type()		sysinfo_cache.dwProcessorType
#define get_w32_major_version()  	w32_major_version
#define get_w32_minor_version()  	w32_minor_version

extern SYSTEM_INFO    sysinfo_cache;
extern OSVERSIONINFO  osinfo_cache;
extern DWORD_PTR      syspage_mask;

extern int    	      w32_major_version;
extern int    	      w32_minor_version;
extern int    	      w32_build_number;

enum {
  OS_9X = 1,
  OS_NT
};

extern int os_subtype;

/* Cache system info, e.g., the NT page size.  */
extern void cache_system_info (void);

typedef void (* VOIDFNPTR) (void);

/* Load a function address from a DLL.  Cast the result via VOIDFNPTR
   to pacify -Wcast-function-type in GCC 8.1.  The return value must
   be cast to the correct function pointer type.  */
INLINE VOIDFNPTR get_proc_addr (HINSTANCE, LPCSTR);
INLINE VOIDFNPTR
get_proc_addr (HINSTANCE handle, LPCSTR fname)
{
  return (VOIDFNPTR) GetProcAddress (handle, fname);
}

/* Define a function that will be loaded from a DLL.  The variable
   arguments should contain the argument list for the function, and
   optionally be followed by function attributes.  For example:
   DEF_DLL_FN (void, png_longjmp, (png_structp, int) PNG_NORETURN);
  */
#define DEF_DLL_FN(type, func, ...)                     \
  typedef type (CDECL *W32_PFN_##func) __VA_ARGS__;     \
  static W32_PFN_##func fn_##func

/* Load a function from the DLL.  */
#define LOAD_DLL_FN(lib, func)						\
  do									\
    {									\
      fn_##func = (W32_PFN_##func) get_proc_addr (lib, #func);		\
      if (!fn_##func)							\
	return false;							\
    }									\
  while (false)

#ifdef HAVE_HARFBUZZ
extern bool hbfont_init_w32_funcs (HMODULE);
#endif

#endif /* W32COMMON_H */
