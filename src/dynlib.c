/* Portable API for dynamic loading.

Copyright 2015-2017 Free Software Foundation, Inc.

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


/* Assume modules are enabled on modern systems...  *Yes*, the
   preprocessor macro checks could be more precise.  I don't care.

   If you think the abstraction is too leaky use libltdl (libtool),
   don't reinvent the wheel by fixing this one.  */

#include <config.h>

#include "dynlib.h"

#include <stddef.h>

#ifdef WINDOWSNT

/* MS-Windows systems.  */

#include <errno.h>
#include "lisp.h"
#include "w32common.h"	/* for os_subtype */
#include "w32.h"

static BOOL g_b_init_get_module_handle_ex;
static DWORD dynlib_last_err;

/* Some versions of w32api headers only expose the following when
   _WIN32_WINNT is set to higher values that we use.  */
typedef BOOL (WINAPI *GetModuleHandleExA_Proc) (DWORD,LPCSTR,HMODULE*);
#ifndef GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
# define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS 4
#endif
#ifndef GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT
# define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT 2
#endif

/* This needs to be called at startup to countermand any non-zero
   values recorded by temacs.  */
void dynlib_reset_last_error (void);
void
dynlib_reset_last_error (void)
{
  g_b_init_get_module_handle_ex = 0;
  dynlib_last_err = 0;
}

dynlib_handle_ptr
dynlib_open (const char *dll_fname)
{
  HMODULE hdll;
  char dll_fname_local[MAX_UTF8_PATH];

  if (!dll_fname)
    {
      errno = ENOTSUP;
      return NULL;
    }

  if (!dll_fname)
    hdll = GetModuleHandle (NULL);
  else
    {
      /* LoadLibrary wants backslashes.  */
      strcpy (dll_fname_local, dll_fname);
      unixtodos_filename (dll_fname_local);

      if (w32_unicode_filenames)
	{
	  wchar_t dll_fname_w[MAX_PATH];

	  filename_to_utf16 (dll_fname_local, dll_fname_w);
	  hdll = LoadLibraryW (dll_fname_w);
	}
      else
	{
	  char dll_fname_a[MAX_PATH];

	  filename_to_ansi (dll_fname_local, dll_fname_a);
	  hdll = LoadLibraryA (dll_fname_a);
	}
    }

  if (!hdll)
    dynlib_last_err = GetLastError ();

  return (dynlib_handle_ptr) hdll;
}

void *
dynlib_sym (dynlib_handle_ptr h, const char *sym)
{
  FARPROC sym_addr = NULL;

  if (!h || h == INVALID_HANDLE_VALUE || !sym)
    {
      dynlib_last_err = ERROR_INVALID_PARAMETER;
      return NULL;
    }

  sym_addr = GetProcAddress ((HMODULE) h, sym);
  if (!sym_addr)
    dynlib_last_err = GetLastError ();

  return (void *)sym_addr;
}

void
dynlib_addr (void *addr, const char **fname, const char **symname)
{
  static char dll_filename[MAX_UTF8_PATH];
  static GetModuleHandleExA_Proc s_pfn_Get_Module_HandleExA = NULL;
  char *dll_fn = NULL;
  HMODULE hm_kernel32 = NULL;
  HMODULE hm_dll = NULL;
  wchar_t mfn_w[MAX_PATH];
  char mfn_a[MAX_PATH];

  /* Step 1: Find the handle of the module where ADDR lives.  */
  if (os_subtype == OS_9X
      /* Windows NT family version before XP (v5.1).  */
      || ((w32_major_version + (w32_minor_version > 0)) < 6))
    {
      MEMORY_BASIC_INFORMATION mbi;

      /* According to Matt Pietrek, the module handle is just the base
	 address where it's loaded in memory.  */
      if (VirtualQuery (addr, &mbi, sizeof(mbi)))
	hm_dll = (HMODULE)mbi.AllocationBase;
    }
  else
    {
      /* Use the documented API when available (XP and later).  */
      if (g_b_init_get_module_handle_ex == 0)
	{
	  g_b_init_get_module_handle_ex = 1;
	  hm_kernel32 = LoadLibrary ("kernel32.dll");
	  /* We load the ANSI version of the function because the
	     address we pass to it is not an address of a string, but
	     an address of a function.  So we don't care about the
	     Unicode version.  */
	  s_pfn_Get_Module_HandleExA =
	    (GetModuleHandleExA_Proc) GetProcAddress (hm_kernel32,
						      "GetModuleHandleExA");
	}
      if (s_pfn_Get_Module_HandleExA)
	{
	  DWORD flags = (GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
			 /* We don't want to call FreeLibrary at the
			    end, because then we'd need to remember
			    whether we obtained the handle by this
			    method or the above one.  */
			 | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT);

	  if (!s_pfn_Get_Module_HandleExA (flags, addr, &hm_dll))
	    {
	      dynlib_last_err = GetLastError ();
	      hm_dll = NULL;
	    }
	}
    }

  /* Step 2: Find the absolute file name of the module corresponding
     to the hm_dll handle.  */
  if (hm_dll)
    {
      DWORD retval;

      if (w32_unicode_filenames)
	{
	  retval = GetModuleFileNameW (hm_dll, mfn_w, MAX_PATH);
	  if (retval > 0 && retval < MAX_PATH
	      && filename_from_utf16 (mfn_w, dll_filename) == 0)
	    dll_fn = dll_filename;
	  else if (retval == MAX_PATH)
	    dynlib_last_err = ERROR_INSUFFICIENT_BUFFER;
	  else
	    dynlib_last_err = GetLastError ();
	}
      else
	{
	  retval = GetModuleFileNameA (hm_dll, mfn_a, MAX_PATH);
	  if (retval > 0 && retval < MAX_PATH
	      && filename_from_ansi (mfn_a, dll_filename) == 0)
	    dll_fn = dll_filename;
	  else if (retval == MAX_PATH)
	    dynlib_last_err = ERROR_INSUFFICIENT_BUFFER;
	  else
	    dynlib_last_err = GetLastError ();
	}
      if (dll_fn)
        dostounix_filename (dll_fn);
    }

  *fname = dll_fn;

  /* We cannot easily produce the function name, since typically all
     of the module functions will be unexported, and probably even
     static, which means the symbols can be obtained only if we link
     against libbfd (and the DLL can be stripped anyway).  So we just
     show the address and the file name (see print_vectorlike in
     print.c); they can use that with addr2line or GDB to recover the
     symbolic name.  */
  *symname = NULL;
}

const char *
dynlib_error (void)
{
  char *error_string = NULL;

  if (dynlib_last_err)
    {
      error_string = w32_strerror (dynlib_last_err);
      dynlib_last_err = 0;
    }

  return error_string;
}

int
dynlib_close (dynlib_handle_ptr h)
{
  if (!h || h == INVALID_HANDLE_VALUE)
    {
      dynlib_last_err = ERROR_INVALID_PARAMETER;
      return -1;
    }
  /* If the handle is for the main module (the .exe file), it
     shouldn't be passed to FreeLibrary, because GetModuleHandle
     doesn't increment the refcount, but FreeLibrary does decrement
     it.  I don't think this should matter for the main module, but
     just in case, we avoid the call here, relying on another call to
     GetModuleHandle to return the same value.  */
  if (h == GetModuleHandle (NULL))
    return 0;

  if (!FreeLibrary ((HMODULE) h))
    {
      dynlib_last_err = GetLastError ();
      return -1;
    }

  return 0;
}

#elif defined HAVE_UNISTD_H

/* POSIX systems.  */

#include <dlfcn.h>

dynlib_handle_ptr
dynlib_open (const char *path)
{
  return dlopen (path, RTLD_LAZY);
}

void *
dynlib_sym (dynlib_handle_ptr h, const char *sym)
{
  return dlsym (h, sym);
}

void
dynlib_addr (void *ptr, const char **path, const char **sym)
{
  *path = NULL;
  *sym = NULL;
#ifdef HAVE_DLADDR
  Dl_info info;
  if (dladdr (ptr, &info) && info.dli_fname && info.dli_sname)
    {
      *path = info.dli_fname;
      *sym = info.dli_sname;
    }
#endif
}

const char *
dynlib_error (void)
{
  return dlerror ();
}

/* FIXME: Currently there is no way to unload a module, so this
   function is never used.  */
#if false
int
dynlib_close (dynlib_handle_ptr h)
{
  return dlclose (h) == 0;
}
#endif

#else

#error "No dynamic loading for this system"

#endif

#if !HAVE_DLFUNC
# define dlfunc dynlib_sym
#endif

dynlib_function_ptr
dynlib_func (dynlib_handle_ptr h, const char *sym)
{
  return (dynlib_function_ptr) dlfunc (h, sym);
}
