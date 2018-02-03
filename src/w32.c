/* Utility and Unix shadow routines for GNU Emacs on the Microsoft Windows API.

Copyright (C) 1994-1995, 2000-2018 Free Software Foundation, Inc.

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

/*
   Geoff Voelker (voelker@cs.washington.edu)                         7-29-94
*/

#define DEFER_MS_W32_H
#include <config.h>

#include <mingw_time.h>
#include <stddef.h> /* for offsetof */
#include <stdlib.h>
#include <stdio.h>
#include <float.h>	/* for DBL_EPSILON */
#include <io.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <sys/file.h>
#include <time.h>	/* must be before nt/inc/sys/time.h, for MinGW64 */
#include <sys/time.h>
#include <sys/utime.h>
#include <math.h>

/* Include (most) CRT headers *before* ms-w32.h.  */
#include <ms-w32.h>

#include <string.h>	/* for strerror, needed by sys_strerror */
#include <mbstring.h>	/* for _mbspbrk, _mbslwr, _mbsrchr, ... */

#undef access
#undef chdir
#undef chmod
#undef creat
#undef ctime
#undef fopen
#undef link
#undef mkdir
#undef open
#undef rename
#undef rmdir
#undef unlink

#undef close
#undef dup
#undef dup2
#undef pipe
#undef read
#undef write

#undef strerror

#undef localtime

char *sys_ctime (const time_t *);
int sys_chdir (const char *);
int sys_creat (const char *, int);
FILE *sys_fopen (const char *, const char *);
int sys_open (const char *, int, int);
int sys_rename (char const *, char const *);
int sys_rmdir (const char *);
int sys_close (int);
int sys_dup2 (int, int);
int sys_read (int, char *, unsigned int);
int sys_write (int, const void *, unsigned int);
struct tm *sys_localtime (const time_t *);
/* MinGW64 system headers include string.h too early, causing the
   compiler to emit a warning about sys_strerror having no
   prototype.  */
char *sys_strerror (int);

#ifdef HAVE_MODULES
extern void dynlib_reset_last_error (void);
#endif

#include "lisp.h"
#include "epaths.h"	/* for PATH_EXEC */

#include <pwd.h>
#include <grp.h>

#include <windows.h>
/* Some versions of compiler define MEMORYSTATUSEX, some don't, so we
   use a different name to avoid compilation problems.  */
typedef struct _MEMORY_STATUS_EX {
  DWORD dwLength;
  DWORD dwMemoryLoad;
  DWORDLONG ullTotalPhys;
  DWORDLONG ullAvailPhys;
  DWORDLONG ullTotalPageFile;
  DWORDLONG ullAvailPageFile;
  DWORDLONG ullTotalVirtual;
  DWORDLONG ullAvailVirtual;
  DWORDLONG ullAvailExtendedVirtual;
} MEMORY_STATUS_EX,*LPMEMORY_STATUS_EX;

/* These are here so that GDB would know about these data types.  This
   allows attaching GDB to Emacs when a fatal exception is triggered
   and Windows pops up the "application needs to be closed" dialog.
   At that point, _gnu_exception_handler, the top-level exception
   handler installed by the MinGW startup code, is somewhere on the
   call-stack of the main thread, so going to that call frame and
   looking at the argument to _gnu_exception_handler, which is a
   PEXCEPTION_POINTERS pointer, can reveal the exception code
   (excptr->ExceptionRecord->ExceptionCode) and the address where the
   exception happened (excptr->ExceptionRecord->ExceptionAddress), as
   well as some additional information specific to the exception.  */
PEXCEPTION_POINTERS excptr;
PEXCEPTION_RECORD excprec;
PCONTEXT ctxrec;

#include <lmcons.h>
#include <shlobj.h>

#include <tlhelp32.h>
#include <psapi.h>
#ifndef _MSC_VER
#include <w32api.h>
#endif
#if _WIN32_WINNT < 0x0500
#if !defined (__MINGW32__) || __W32API_MAJOR_VERSION < 3 || (__W32API_MAJOR_VERSION == 3 && __W32API_MINOR_VERSION < 15)
/* This either is not in psapi.h or guarded by higher value of
   _WIN32_WINNT than what we use.  w32api supplied with MinGW 3.15
   defines it in psapi.h  */
typedef struct _PROCESS_MEMORY_COUNTERS_EX {
  DWORD  cb;
  DWORD  PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
  SIZE_T PrivateUsage;
} PROCESS_MEMORY_COUNTERS_EX,*PPROCESS_MEMORY_COUNTERS_EX;
#endif
#endif

#include <winioctl.h>
#include <aclapi.h>
#include <sddl.h>

#include <sys/acl.h>
#include <acl.h>

/* This is not in MinGW's sddl.h (but they are in MSVC headers), so we
   define them by hand if not already defined.  */
#ifndef SDDL_REVISION_1
#define SDDL_REVISION_1	1
#endif	/* SDDL_REVISION_1 */

#if defined(_MSC_VER) || defined(MINGW_W64)
/* MSVC and MinGW64 don't provide the definition of
   REPARSE_DATA_BUFFER and the associated macros, except on ntifs.h,
   which cannot be included because it triggers conflicts with other
   Windows API headers.  So we define it here by hand.  */

typedef struct _REPARSE_DATA_BUFFER {
    ULONG  ReparseTag;
    USHORT ReparseDataLength;
    USHORT Reserved;
    union {
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            ULONG Flags;
            WCHAR PathBuffer[1];
        } SymbolicLinkReparseBuffer;
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            WCHAR PathBuffer[1];
        } MountPointReparseBuffer;
        struct {
            UCHAR  DataBuffer[1];
        } GenericReparseBuffer;
    } DUMMYUNIONNAME;
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

#ifndef FILE_DEVICE_FILE_SYSTEM
#define FILE_DEVICE_FILE_SYSTEM	9
#endif
#ifndef METHOD_BUFFERED
#define METHOD_BUFFERED	        0
#endif
#ifndef FILE_ANY_ACCESS
#define FILE_ANY_ACCESS	        0x00000000
#endif
#ifndef CTL_CODE
#define CTL_CODE(t,f,m,a)       (((t)<<16)|((a)<<14)|((f)<<2)|(m))
#endif
/* MinGW64 defines FSCTL_GET_REPARSE_POINT on winioctl.h.  */
#ifndef FSCTL_GET_REPARSE_POINT
#define FSCTL_GET_REPARSE_POINT \
  CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 42, METHOD_BUFFERED, FILE_ANY_ACCESS)
#endif
#endif

/* TCP connection support.  */
#include <sys/socket.h>
#undef socket
#undef bind
#undef connect
#undef htons
#undef ntohs
#undef inet_addr
#undef gethostname
#undef gethostbyname
#undef getservbyname
#undef getpeername
#undef shutdown
#undef setsockopt
#undef listen
#undef getsockname
#undef accept
#undef recvfrom
#undef sendto

#include <iphlpapi.h>	/* should be after winsock2.h */

#include <wincrypt.h>

#include <c-strcase.h>
#include <utimens.h>	/* for fdutimens */

#include "w32.h"
#include <dirent.h>
#include "w32common.h"
#include "w32select.h"
#include "systime.h"		/* for current_timespec, struct timespec */
#include "dispextern.h"		/* for xstrcasecmp */
#include "coding.h"		/* for Vlocale_coding_system */

#include "careadlinkat.h"
#include "allocator.h"

/* For Lisp_Process, serial_configure and serial_open.  */
#include "process.h"
#include "systty.h"

typedef HRESULT (WINAPI * ShGetFolderPath_fn)
  (IN HWND, IN int, IN HANDLE, IN DWORD, OUT char *);

static DWORD get_rid (PSID);
static int is_symlink (const char *);
static char * chase_symlinks (const char *);
static int enable_privilege (LPCTSTR, BOOL, TOKEN_PRIVILEGES *);
static int restore_privilege (TOKEN_PRIVILEGES *);
static BOOL WINAPI revert_to_self (void);

static int sys_access (const char *, int);
extern void *e_malloc (size_t);
extern int sys_select (int, SELECT_TYPE *, SELECT_TYPE *, SELECT_TYPE *,
		       const struct timespec *, const sigset_t *);
extern int sys_dup (int);


/* Initialization states.

   WARNING: If you add any more such variables for additional APIs,
            you MUST add initialization for them to globals_of_w32
            below.  This is because these variables might get set
            to non-NULL values during dumping, but the dumped Emacs
            cannot reuse those values, because it could be run on a
            different version of the OS, where API addresses are
            different.  */
static BOOL g_b_init_is_windows_9x;
static BOOL g_b_init_open_process_token;
static BOOL g_b_init_get_token_information;
static BOOL g_b_init_lookup_account_sid;
static BOOL g_b_init_get_sid_sub_authority;
static BOOL g_b_init_get_sid_sub_authority_count;
static BOOL g_b_init_get_security_info;
static BOOL g_b_init_get_file_security_w;
static BOOL g_b_init_get_file_security_a;
static BOOL g_b_init_get_security_descriptor_owner;
static BOOL g_b_init_get_security_descriptor_group;
static BOOL g_b_init_is_valid_sid;
static BOOL g_b_init_create_toolhelp32_snapshot;
static BOOL g_b_init_process32_first;
static BOOL g_b_init_process32_next;
static BOOL g_b_init_open_thread_token;
static BOOL g_b_init_impersonate_self;
static BOOL g_b_init_revert_to_self;
static BOOL g_b_init_get_process_memory_info;
static BOOL g_b_init_get_process_working_set_size;
static BOOL g_b_init_global_memory_status;
static BOOL g_b_init_global_memory_status_ex;
static BOOL g_b_init_get_length_sid;
static BOOL g_b_init_equal_sid;
static BOOL g_b_init_copy_sid;
static BOOL g_b_init_get_native_system_info;
static BOOL g_b_init_get_system_times;
static BOOL g_b_init_create_symbolic_link_w;
static BOOL g_b_init_create_symbolic_link_a;
static BOOL g_b_init_get_security_descriptor_dacl;
static BOOL g_b_init_convert_sd_to_sddl;
static BOOL g_b_init_convert_sddl_to_sd;
static BOOL g_b_init_is_valid_security_descriptor;
static BOOL g_b_init_set_file_security_w;
static BOOL g_b_init_set_file_security_a;
static BOOL g_b_init_set_named_security_info_w;
static BOOL g_b_init_set_named_security_info_a;
static BOOL g_b_init_get_adapters_info;

BOOL g_b_init_compare_string_w;
BOOL g_b_init_debug_break_process;

/*
  BEGIN: Wrapper functions around OpenProcessToken
  and other functions in advapi32.dll that are only
  supported in Windows NT / 2k / XP
*/
  /* ** Function pointer typedefs ** */
typedef BOOL (WINAPI * OpenProcessToken_Proc) (
    HANDLE ProcessHandle,
    DWORD DesiredAccess,
    PHANDLE TokenHandle);
typedef BOOL (WINAPI * GetTokenInformation_Proc) (
    HANDLE TokenHandle,
    TOKEN_INFORMATION_CLASS TokenInformationClass,
    LPVOID TokenInformation,
    DWORD TokenInformationLength,
    PDWORD ReturnLength);
typedef BOOL (WINAPI * GetProcessTimes_Proc) (
    HANDLE process_handle,
    LPFILETIME creation_time,
    LPFILETIME exit_time,
    LPFILETIME kernel_time,
    LPFILETIME user_time);

GetProcessTimes_Proc get_process_times_fn = NULL;

#ifdef _UNICODE
const char * const LookupAccountSid_Name = "LookupAccountSidW";
#else
const char * const LookupAccountSid_Name = "LookupAccountSidA";
#endif
typedef BOOL (WINAPI * LookupAccountSid_Proc) (
    LPCTSTR lpSystemName,
    PSID Sid,
    LPTSTR Name,
    LPDWORD cbName,
    LPTSTR DomainName,
    LPDWORD cbDomainName,
    PSID_NAME_USE peUse);
typedef PDWORD (WINAPI * GetSidSubAuthority_Proc) (
    PSID pSid,
    DWORD n);
typedef PUCHAR (WINAPI * GetSidSubAuthorityCount_Proc) (
    PSID pSid);
typedef DWORD (WINAPI * GetSecurityInfo_Proc) (
    HANDLE handle,
    SE_OBJECT_TYPE ObjectType,
    SECURITY_INFORMATION SecurityInfo,
    PSID *ppsidOwner,
    PSID *ppsidGroup,
    PACL *ppDacl,
    PACL *ppSacl,
    PSECURITY_DESCRIPTOR *ppSecurityDescriptor);
typedef BOOL (WINAPI * GetFileSecurityW_Proc) (
    LPCWSTR lpFileName,
    SECURITY_INFORMATION RequestedInformation,
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    DWORD nLength,
    LPDWORD lpnLengthNeeded);
typedef BOOL (WINAPI * GetFileSecurityA_Proc) (
    LPCSTR lpFileName,
    SECURITY_INFORMATION RequestedInformation,
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    DWORD nLength,
    LPDWORD lpnLengthNeeded);
typedef BOOL (WINAPI *SetFileSecurityW_Proc) (
    LPCWSTR lpFileName,
    SECURITY_INFORMATION SecurityInformation,
    PSECURITY_DESCRIPTOR pSecurityDescriptor);
typedef BOOL (WINAPI *SetFileSecurityA_Proc) (
    LPCSTR lpFileName,
    SECURITY_INFORMATION SecurityInformation,
    PSECURITY_DESCRIPTOR pSecurityDescriptor);
typedef DWORD (WINAPI *SetNamedSecurityInfoW_Proc) (
    LPCWSTR lpObjectName,
    SE_OBJECT_TYPE ObjectType,
    SECURITY_INFORMATION SecurityInformation,
    PSID psidOwner,
    PSID psidGroup,
    PACL pDacl,
    PACL pSacl);
typedef DWORD (WINAPI *SetNamedSecurityInfoA_Proc) (
    LPCSTR lpObjectName,
    SE_OBJECT_TYPE ObjectType,
    SECURITY_INFORMATION SecurityInformation,
    PSID psidOwner,
    PSID psidGroup,
    PACL pDacl,
    PACL pSacl);
typedef BOOL (WINAPI * GetSecurityDescriptorOwner_Proc) (
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    PSID *pOwner,
    LPBOOL lpbOwnerDefaulted);
typedef BOOL (WINAPI * GetSecurityDescriptorGroup_Proc) (
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    PSID *pGroup,
    LPBOOL lpbGroupDefaulted);
typedef BOOL (WINAPI *GetSecurityDescriptorDacl_Proc) (
    PSECURITY_DESCRIPTOR pSecurityDescriptor,
    LPBOOL lpbDaclPresent,
    PACL *pDacl,
    LPBOOL lpbDaclDefaulted);
typedef BOOL (WINAPI * IsValidSid_Proc) (
    PSID sid);
typedef HANDLE (WINAPI * CreateToolhelp32Snapshot_Proc) (
    DWORD dwFlags,
    DWORD th32ProcessID);
typedef BOOL (WINAPI * Process32First_Proc) (
    HANDLE hSnapshot,
    LPPROCESSENTRY32 lppe);
typedef BOOL (WINAPI * Process32Next_Proc) (
    HANDLE hSnapshot,
    LPPROCESSENTRY32 lppe);
typedef BOOL (WINAPI * OpenThreadToken_Proc) (
    HANDLE ThreadHandle,
    DWORD DesiredAccess,
    BOOL OpenAsSelf,
    PHANDLE TokenHandle);
typedef BOOL (WINAPI * ImpersonateSelf_Proc) (
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel);
typedef BOOL (WINAPI * RevertToSelf_Proc) (void);
typedef BOOL (WINAPI * GetProcessMemoryInfo_Proc) (
    HANDLE Process,
    PPROCESS_MEMORY_COUNTERS ppsmemCounters,
    DWORD cb);
typedef BOOL (WINAPI * GetProcessWorkingSetSize_Proc) (
    HANDLE hProcess,
    PSIZE_T lpMinimumWorkingSetSize,
    PSIZE_T lpMaximumWorkingSetSize);
typedef BOOL (WINAPI * GlobalMemoryStatus_Proc) (
    LPMEMORYSTATUS lpBuffer);
typedef BOOL (WINAPI * GlobalMemoryStatusEx_Proc) (
    LPMEMORY_STATUS_EX lpBuffer);
typedef BOOL (WINAPI * CopySid_Proc) (
    DWORD nDestinationSidLength,
    PSID pDestinationSid,
    PSID pSourceSid);
typedef BOOL (WINAPI * EqualSid_Proc) (
    PSID pSid1,
    PSID pSid2);
typedef DWORD (WINAPI * GetLengthSid_Proc) (
    PSID pSid);
typedef void (WINAPI * GetNativeSystemInfo_Proc) (
    LPSYSTEM_INFO lpSystemInfo);
typedef BOOL (WINAPI * GetSystemTimes_Proc) (
    LPFILETIME lpIdleTime,
    LPFILETIME lpKernelTime,
    LPFILETIME lpUserTime);
typedef BOOLEAN (WINAPI *CreateSymbolicLinkW_Proc) (
    LPCWSTR lpSymlinkFileName,
    LPCWSTR lpTargetFileName,
    DWORD  dwFlags);
typedef BOOLEAN (WINAPI *CreateSymbolicLinkA_Proc) (
    LPCSTR lpSymlinkFileName,
    LPCSTR lpTargetFileName,
    DWORD  dwFlags);
typedef BOOL (WINAPI *ConvertStringSecurityDescriptorToSecurityDescriptor_Proc) (
    LPCTSTR StringSecurityDescriptor,
    DWORD StringSDRevision,
    PSECURITY_DESCRIPTOR  *SecurityDescriptor,
    PULONG  SecurityDescriptorSize);
typedef BOOL (WINAPI *ConvertSecurityDescriptorToStringSecurityDescriptor_Proc) (
    PSECURITY_DESCRIPTOR  SecurityDescriptor,
    DWORD RequestedStringSDRevision,
    SECURITY_INFORMATION SecurityInformation,
    LPTSTR  *StringSecurityDescriptor,
    PULONG StringSecurityDescriptorLen);
typedef BOOL (WINAPI *IsValidSecurityDescriptor_Proc) (PSECURITY_DESCRIPTOR);
typedef DWORD (WINAPI *GetAdaptersInfo_Proc) (
    PIP_ADAPTER_INFO pAdapterInfo,
    PULONG pOutBufLen);

int (WINAPI *pMultiByteToWideChar)(UINT,DWORD,LPCSTR,int,LPWSTR,int);
int (WINAPI *pWideCharToMultiByte)(UINT,DWORD,LPCWSTR,int,LPSTR,int,LPCSTR,LPBOOL);
DWORD multiByteToWideCharFlags;

  /* ** A utility function ** */
static BOOL
is_windows_9x (void)
{
  static BOOL s_b_ret = 0;
  OSVERSIONINFO os_ver;
  if (g_b_init_is_windows_9x == 0)
    {
      g_b_init_is_windows_9x = 1;
      ZeroMemory (&os_ver, sizeof (OSVERSIONINFO));
      os_ver.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
      if (GetVersionEx (&os_ver))
        {
          s_b_ret = (os_ver.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS);
        }
    }
  return s_b_ret;
}

static Lisp_Object ltime (ULONGLONG);

/* Get total user and system times for get-internal-run-time.
   Returns a list of integers if the times are provided by the OS
   (NT derivatives), otherwise it returns the result of current-time. */
Lisp_Object w32_get_internal_run_time (void);

Lisp_Object
w32_get_internal_run_time (void)
{
  if (get_process_times_fn)
    {
      FILETIME create, exit, kernel, user;
      HANDLE proc = GetCurrentProcess ();
      if ((*get_process_times_fn) (proc, &create, &exit, &kernel, &user))
        {
          LARGE_INTEGER user_int, kernel_int, total;
          user_int.LowPart = user.dwLowDateTime;
          user_int.HighPart = user.dwHighDateTime;
          kernel_int.LowPart = kernel.dwLowDateTime;
          kernel_int.HighPart = kernel.dwHighDateTime;
          total.QuadPart = user_int.QuadPart + kernel_int.QuadPart;
	  return ltime (total.QuadPart);
        }
    }

  return Fcurrent_time ();
}

  /* ** The wrapper functions ** */

static BOOL WINAPI
open_process_token (HANDLE ProcessHandle,
		    DWORD DesiredAccess,
		    PHANDLE TokenHandle)
{
  static OpenProcessToken_Proc s_pfn_Open_Process_Token = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_open_process_token == 0)
    {
      g_b_init_open_process_token = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Open_Process_Token =
        (OpenProcessToken_Proc) GetProcAddress (hm_advapi32, "OpenProcessToken");
    }
  if (s_pfn_Open_Process_Token == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Open_Process_Token (
          ProcessHandle,
          DesiredAccess,
          TokenHandle)
      );
}

static BOOL WINAPI
get_token_information (HANDLE TokenHandle,
		       TOKEN_INFORMATION_CLASS TokenInformationClass,
		       LPVOID TokenInformation,
		       DWORD TokenInformationLength,
		       PDWORD ReturnLength)
{
  static GetTokenInformation_Proc s_pfn_Get_Token_Information = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_token_information == 0)
    {
      g_b_init_get_token_information = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Token_Information =
        (GetTokenInformation_Proc) GetProcAddress (hm_advapi32, "GetTokenInformation");
    }
  if (s_pfn_Get_Token_Information == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Get_Token_Information (
          TokenHandle,
          TokenInformationClass,
          TokenInformation,
          TokenInformationLength,
          ReturnLength)
      );
}

static BOOL WINAPI
lookup_account_sid (LPCTSTR lpSystemName,
		    PSID Sid,
		    LPTSTR Name,
		    LPDWORD cbName,
		    LPTSTR DomainName,
		    LPDWORD cbDomainName,
		    PSID_NAME_USE peUse)
{
  static LookupAccountSid_Proc s_pfn_Lookup_Account_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_lookup_account_sid == 0)
    {
      g_b_init_lookup_account_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Lookup_Account_Sid =
        (LookupAccountSid_Proc) GetProcAddress (hm_advapi32, LookupAccountSid_Name);
    }
  if (s_pfn_Lookup_Account_Sid == NULL)
    {
      return FALSE;
    }
  return (
      s_pfn_Lookup_Account_Sid (
          lpSystemName,
          Sid,
          Name,
          cbName,
          DomainName,
          cbDomainName,
          peUse)
      );
}

static PDWORD WINAPI
get_sid_sub_authority (PSID pSid, DWORD n)
{
  static GetSidSubAuthority_Proc s_pfn_Get_Sid_Sub_Authority = NULL;
  static DWORD zero = 0U;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return &zero;
    }
  if (g_b_init_get_sid_sub_authority == 0)
    {
      g_b_init_get_sid_sub_authority = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Sid_Sub_Authority =
        (GetSidSubAuthority_Proc) GetProcAddress (
            hm_advapi32, "GetSidSubAuthority");
    }
  if (s_pfn_Get_Sid_Sub_Authority == NULL)
    {
      return &zero;
    }
  return (s_pfn_Get_Sid_Sub_Authority (pSid, n));
}

static PUCHAR WINAPI
get_sid_sub_authority_count (PSID pSid)
{
  static GetSidSubAuthorityCount_Proc s_pfn_Get_Sid_Sub_Authority_Count = NULL;
  static UCHAR zero = 0U;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return &zero;
    }
  if (g_b_init_get_sid_sub_authority_count == 0)
    {
      g_b_init_get_sid_sub_authority_count = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Sid_Sub_Authority_Count =
        (GetSidSubAuthorityCount_Proc) GetProcAddress (
            hm_advapi32, "GetSidSubAuthorityCount");
    }
  if (s_pfn_Get_Sid_Sub_Authority_Count == NULL)
    {
      return &zero;
    }
  return (s_pfn_Get_Sid_Sub_Authority_Count (pSid));
}

static DWORD WINAPI
get_security_info (HANDLE handle,
		   SE_OBJECT_TYPE ObjectType,
		   SECURITY_INFORMATION SecurityInfo,
		   PSID *ppsidOwner,
		   PSID *ppsidGroup,
		   PACL *ppDacl,
		   PACL *ppSacl,
		   PSECURITY_DESCRIPTOR *ppSecurityDescriptor)
{
  static GetSecurityInfo_Proc s_pfn_Get_Security_Info = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_security_info == 0)
    {
      g_b_init_get_security_info = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Info =
        (GetSecurityInfo_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityInfo");
    }
  if (s_pfn_Get_Security_Info == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Get_Security_Info (handle, ObjectType, SecurityInfo,
				   ppsidOwner, ppsidGroup, ppDacl, ppSacl,
				   ppSecurityDescriptor));
}

static BOOL WINAPI
get_file_security (const char *lpFileName,
		   SECURITY_INFORMATION RequestedInformation,
		   PSECURITY_DESCRIPTOR pSecurityDescriptor,
		   DWORD nLength,
		   LPDWORD lpnLengthNeeded)
{
  static GetFileSecurityA_Proc s_pfn_Get_File_SecurityA = NULL;
  static GetFileSecurityW_Proc s_pfn_Get_File_SecurityW = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  if (w32_unicode_filenames)
    {
      wchar_t filename_w[MAX_PATH];

      if (g_b_init_get_file_security_w == 0)
	{
	  g_b_init_get_file_security_w = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Get_File_SecurityW =
	    (GetFileSecurityW_Proc) GetProcAddress (hm_advapi32,
						   "GetFileSecurityW");
	}
      if (s_pfn_Get_File_SecurityW == NULL)
	{
	  errno = ENOTSUP;
	  return FALSE;
	}
      filename_to_utf16 (lpFileName, filename_w);
      return (s_pfn_Get_File_SecurityW (filename_w, RequestedInformation,
					pSecurityDescriptor, nLength,
					lpnLengthNeeded));
    }
  else
    {
      char filename_a[MAX_PATH];

      if (g_b_init_get_file_security_a == 0)
	{
	  g_b_init_get_file_security_a = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Get_File_SecurityA =
	    (GetFileSecurityA_Proc) GetProcAddress (hm_advapi32,
						   "GetFileSecurityA");
	}
      if (s_pfn_Get_File_SecurityA == NULL)
	{
	  errno = ENOTSUP;
	  return FALSE;
	}
      filename_to_ansi (lpFileName, filename_a);
      return (s_pfn_Get_File_SecurityA (filename_a, RequestedInformation,
					pSecurityDescriptor, nLength,
					lpnLengthNeeded));
    }
}

static BOOL WINAPI
set_file_security (const char *lpFileName,
		   SECURITY_INFORMATION SecurityInformation,
		   PSECURITY_DESCRIPTOR pSecurityDescriptor)
{
  static SetFileSecurityW_Proc s_pfn_Set_File_SecurityW = NULL;
  static SetFileSecurityA_Proc s_pfn_Set_File_SecurityA = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  if (w32_unicode_filenames)
    {
      wchar_t filename_w[MAX_PATH];

      if (g_b_init_set_file_security_w == 0)
	{
	  g_b_init_set_file_security_w = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Set_File_SecurityW =
	    (SetFileSecurityW_Proc) GetProcAddress (hm_advapi32,
						    "SetFileSecurityW");
	}
      if (s_pfn_Set_File_SecurityW == NULL)
	{
	  errno = ENOTSUP;
	  return FALSE;
	}
      filename_to_utf16 (lpFileName, filename_w);
      return (s_pfn_Set_File_SecurityW (filename_w, SecurityInformation,
					pSecurityDescriptor));
    }
  else
    {
      char filename_a[MAX_PATH];

      if (g_b_init_set_file_security_a == 0)
	{
	  g_b_init_set_file_security_a = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Set_File_SecurityA =
	    (SetFileSecurityA_Proc) GetProcAddress (hm_advapi32,
						    "SetFileSecurityA");
	}
      if (s_pfn_Set_File_SecurityA == NULL)
	{
	  errno = ENOTSUP;
	  return FALSE;
	}
      filename_to_ansi (lpFileName, filename_a);
      return (s_pfn_Set_File_SecurityA (filename_a, SecurityInformation,
					pSecurityDescriptor));
    }
}

static DWORD WINAPI
set_named_security_info (LPCTSTR lpObjectName,
			 SE_OBJECT_TYPE ObjectType,
			 SECURITY_INFORMATION SecurityInformation,
			 PSID psidOwner,
			 PSID psidGroup,
			 PACL pDacl,
			 PACL pSacl)
{
  static SetNamedSecurityInfoW_Proc s_pfn_Set_Named_Security_InfoW = NULL;
  static SetNamedSecurityInfoA_Proc s_pfn_Set_Named_Security_InfoA = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return ENOTSUP;
    }
  if (w32_unicode_filenames)
    {
      wchar_t filename_w[MAX_PATH];

      if (g_b_init_set_named_security_info_w == 0)
	{
	  g_b_init_set_named_security_info_w = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Set_Named_Security_InfoW =
	    (SetNamedSecurityInfoW_Proc) GetProcAddress (hm_advapi32,
							 "SetNamedSecurityInfoW");
	}
      if (s_pfn_Set_Named_Security_InfoW == NULL)
	{
	  errno = ENOTSUP;
	  return ENOTSUP;
	}
      filename_to_utf16 (lpObjectName, filename_w);
      return (s_pfn_Set_Named_Security_InfoW (filename_w, ObjectType,
					      SecurityInformation, psidOwner,
					      psidGroup, pDacl, pSacl));
    }
  else
    {
      char filename_a[MAX_PATH];

      if (g_b_init_set_named_security_info_a == 0)
	{
	  g_b_init_set_named_security_info_a = 1;
	  hm_advapi32 = LoadLibrary ("Advapi32.dll");
	  s_pfn_Set_Named_Security_InfoA =
	    (SetNamedSecurityInfoA_Proc) GetProcAddress (hm_advapi32,
							 "SetNamedSecurityInfoA");
	}
      if (s_pfn_Set_Named_Security_InfoA == NULL)
	{
	  errno = ENOTSUP;
	  return ENOTSUP;
	}
      filename_to_ansi (lpObjectName, filename_a);
      return (s_pfn_Set_Named_Security_InfoA (filename_a, ObjectType,
					      SecurityInformation, psidOwner,
					      psidGroup, pDacl, pSacl));
    }
}

static BOOL WINAPI
get_security_descriptor_owner (PSECURITY_DESCRIPTOR pSecurityDescriptor,
			       PSID *pOwner,
			       LPBOOL lpbOwnerDefaulted)
{
  static GetSecurityDescriptorOwner_Proc s_pfn_Get_Security_Descriptor_Owner = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  if (g_b_init_get_security_descriptor_owner == 0)
    {
      g_b_init_get_security_descriptor_owner = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Descriptor_Owner =
        (GetSecurityDescriptorOwner_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityDescriptorOwner");
    }
  if (s_pfn_Get_Security_Descriptor_Owner == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  return (s_pfn_Get_Security_Descriptor_Owner (pSecurityDescriptor, pOwner,
					       lpbOwnerDefaulted));
}

static BOOL WINAPI
get_security_descriptor_group (PSECURITY_DESCRIPTOR pSecurityDescriptor,
			       PSID *pGroup,
			       LPBOOL lpbGroupDefaulted)
{
  static GetSecurityDescriptorGroup_Proc s_pfn_Get_Security_Descriptor_Group = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  if (g_b_init_get_security_descriptor_group == 0)
    {
      g_b_init_get_security_descriptor_group = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Descriptor_Group =
        (GetSecurityDescriptorGroup_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityDescriptorGroup");
    }
  if (s_pfn_Get_Security_Descriptor_Group == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  return (s_pfn_Get_Security_Descriptor_Group (pSecurityDescriptor, pGroup,
					       lpbGroupDefaulted));
}

static BOOL WINAPI
get_security_descriptor_dacl (PSECURITY_DESCRIPTOR pSecurityDescriptor,
			      LPBOOL lpbDaclPresent,
			      PACL *pDacl,
			      LPBOOL lpbDaclDefaulted)
{
  static GetSecurityDescriptorDacl_Proc s_pfn_Get_Security_Descriptor_Dacl = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  if (g_b_init_get_security_descriptor_dacl == 0)
    {
      g_b_init_get_security_descriptor_dacl = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Security_Descriptor_Dacl =
        (GetSecurityDescriptorDacl_Proc) GetProcAddress (
            hm_advapi32, "GetSecurityDescriptorDacl");
    }
  if (s_pfn_Get_Security_Descriptor_Dacl == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }
  return (s_pfn_Get_Security_Descriptor_Dacl (pSecurityDescriptor,
					      lpbDaclPresent, pDacl,
					      lpbDaclDefaulted));
}

static BOOL WINAPI
is_valid_sid (PSID sid)
{
  static IsValidSid_Proc s_pfn_Is_Valid_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_is_valid_sid == 0)
    {
      g_b_init_is_valid_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Is_Valid_Sid =
        (IsValidSid_Proc) GetProcAddress (
            hm_advapi32, "IsValidSid");
    }
  if (s_pfn_Is_Valid_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Is_Valid_Sid (sid));
}

static BOOL WINAPI
equal_sid (PSID sid1, PSID sid2)
{
  static EqualSid_Proc s_pfn_Equal_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_equal_sid == 0)
    {
      g_b_init_equal_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Equal_Sid =
        (EqualSid_Proc) GetProcAddress (
            hm_advapi32, "EqualSid");
    }
  if (s_pfn_Equal_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Equal_Sid (sid1, sid2));
}

static DWORD WINAPI
get_length_sid (PSID sid)
{
  static GetLengthSid_Proc s_pfn_Get_Length_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return 0;
    }
  if (g_b_init_get_length_sid == 0)
    {
      g_b_init_get_length_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Get_Length_Sid =
        (GetLengthSid_Proc) GetProcAddress (
            hm_advapi32, "GetLengthSid");
    }
  if (s_pfn_Get_Length_Sid == NULL)
    {
      return 0;
    }
  return (s_pfn_Get_Length_Sid (sid));
}

static BOOL WINAPI
copy_sid (DWORD destlen, PSID dest, PSID src)
{
  static CopySid_Proc s_pfn_Copy_Sid = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_copy_sid == 0)
    {
      g_b_init_copy_sid = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Copy_Sid =
        (CopySid_Proc) GetProcAddress (
            hm_advapi32, "CopySid");
    }
  if (s_pfn_Copy_Sid == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Copy_Sid (destlen, dest, src));
}

/*
  END: Wrapper functions around OpenProcessToken
  and other functions in advapi32.dll that are only
  supported in Windows NT / 2k / XP
*/

static void WINAPI
get_native_system_info (LPSYSTEM_INFO lpSystemInfo)
{
  static GetNativeSystemInfo_Proc s_pfn_Get_Native_System_Info = NULL;
  if (is_windows_9x () != TRUE)
    {
      if (g_b_init_get_native_system_info == 0)
	{
	  g_b_init_get_native_system_info = 1;
	  s_pfn_Get_Native_System_Info =
	    (GetNativeSystemInfo_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
						      "GetNativeSystemInfo");
	}
      if (s_pfn_Get_Native_System_Info != NULL)
	s_pfn_Get_Native_System_Info (lpSystemInfo);
    }
  else
    lpSystemInfo->dwNumberOfProcessors = -1;
}

static BOOL WINAPI
get_system_times (LPFILETIME lpIdleTime,
		  LPFILETIME lpKernelTime,
		  LPFILETIME lpUserTime)
{
  static GetSystemTimes_Proc s_pfn_Get_System_times = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_system_times == 0)
    {
      g_b_init_get_system_times = 1;
      s_pfn_Get_System_times =
	(GetSystemTimes_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
					     "GetSystemTimes");
    }
  if (s_pfn_Get_System_times == NULL)
    return FALSE;
  return (s_pfn_Get_System_times (lpIdleTime, lpKernelTime, lpUserTime));
}

static BOOLEAN WINAPI
create_symbolic_link (LPCSTR lpSymlinkFilename,
		      LPCSTR lpTargetFileName,
		      DWORD dwFlags)
{
  static CreateSymbolicLinkW_Proc s_pfn_Create_Symbolic_LinkW = NULL;
  static CreateSymbolicLinkA_Proc s_pfn_Create_Symbolic_LinkA = NULL;
  BOOLEAN retval;

  if (is_windows_9x () == TRUE)
    {
      errno = ENOSYS;
      return 0;
    }
  if (w32_unicode_filenames)
    {
      wchar_t symfn_w[MAX_PATH], tgtfn_w[MAX_PATH];

      if (g_b_init_create_symbolic_link_w == 0)
	{
	  g_b_init_create_symbolic_link_w = 1;
	  s_pfn_Create_Symbolic_LinkW =
	    (CreateSymbolicLinkW_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
						     "CreateSymbolicLinkW");
	}
      if (s_pfn_Create_Symbolic_LinkW == NULL)
	{
	  errno = ENOSYS;
	  return 0;
	}

      filename_to_utf16 (lpSymlinkFilename, symfn_w);
      filename_to_utf16 (lpTargetFileName, tgtfn_w);
      retval = s_pfn_Create_Symbolic_LinkW (symfn_w, tgtfn_w, dwFlags);
      /* If we were denied creation of the symlink, try again after
	 enabling the SeCreateSymbolicLinkPrivilege for our process.  */
      if (!retval)
	{
	  TOKEN_PRIVILEGES priv_current;

	  if (enable_privilege (SE_CREATE_SYMBOLIC_LINK_NAME, TRUE,
				&priv_current))
	    {
	      retval = s_pfn_Create_Symbolic_LinkW (symfn_w, tgtfn_w, dwFlags);
	      restore_privilege (&priv_current);
	      revert_to_self ();
	    }
	}
    }
  else
    {
      char symfn_a[MAX_PATH], tgtfn_a[MAX_PATH];

      if (g_b_init_create_symbolic_link_a == 0)
	{
	  g_b_init_create_symbolic_link_a = 1;
	  s_pfn_Create_Symbolic_LinkA =
	    (CreateSymbolicLinkA_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
						     "CreateSymbolicLinkA");
	}
      if (s_pfn_Create_Symbolic_LinkA == NULL)
	{
	  errno = ENOSYS;
	  return 0;
	}

      filename_to_ansi (lpSymlinkFilename, symfn_a);
      filename_to_ansi (lpTargetFileName, tgtfn_a);
      retval = s_pfn_Create_Symbolic_LinkA (symfn_a, tgtfn_a, dwFlags);
      /* If we were denied creation of the symlink, try again after
	 enabling the SeCreateSymbolicLinkPrivilege for our process.  */
      if (!retval)
	{
	  TOKEN_PRIVILEGES priv_current;

	  if (enable_privilege (SE_CREATE_SYMBOLIC_LINK_NAME, TRUE,
				&priv_current))
	    {
	      retval = s_pfn_Create_Symbolic_LinkA (symfn_a, tgtfn_a, dwFlags);
	      restore_privilege (&priv_current);
	      revert_to_self ();
	    }
	}
    }
  return retval;
}

static BOOL WINAPI
is_valid_security_descriptor (PSECURITY_DESCRIPTOR pSecurityDescriptor)
{
  static IsValidSecurityDescriptor_Proc s_pfn_Is_Valid_Security_Descriptor_Proc = NULL;

  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  if (g_b_init_is_valid_security_descriptor == 0)
    {
      g_b_init_is_valid_security_descriptor = 1;
      s_pfn_Is_Valid_Security_Descriptor_Proc =
	(IsValidSecurityDescriptor_Proc)GetProcAddress (GetModuleHandle ("Advapi32.dll"),
							"IsValidSecurityDescriptor");
    }
  if (s_pfn_Is_Valid_Security_Descriptor_Proc == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  return s_pfn_Is_Valid_Security_Descriptor_Proc (pSecurityDescriptor);
}

static BOOL WINAPI
convert_sd_to_sddl (PSECURITY_DESCRIPTOR SecurityDescriptor,
		    DWORD RequestedStringSDRevision,
		    SECURITY_INFORMATION SecurityInformation,
		    LPTSTR  *StringSecurityDescriptor,
		    PULONG StringSecurityDescriptorLen)
{
  static ConvertSecurityDescriptorToStringSecurityDescriptor_Proc s_pfn_Convert_SD_To_SDDL = NULL;
  BOOL retval;

  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  if (g_b_init_convert_sd_to_sddl == 0)
    {
      g_b_init_convert_sd_to_sddl = 1;
#ifdef _UNICODE
      s_pfn_Convert_SD_To_SDDL =
	(ConvertSecurityDescriptorToStringSecurityDescriptor_Proc)GetProcAddress (GetModuleHandle ("Advapi32.dll"),
										  "ConvertSecurityDescriptorToStringSecurityDescriptorW");
#else
      s_pfn_Convert_SD_To_SDDL =
	(ConvertSecurityDescriptorToStringSecurityDescriptor_Proc)GetProcAddress (GetModuleHandle ("Advapi32.dll"),
										  "ConvertSecurityDescriptorToStringSecurityDescriptorA");
#endif
    }
  if (s_pfn_Convert_SD_To_SDDL == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  retval = s_pfn_Convert_SD_To_SDDL (SecurityDescriptor,
				     RequestedStringSDRevision,
				     SecurityInformation,
				     StringSecurityDescriptor,
				     StringSecurityDescriptorLen);

  return retval;
}

static BOOL WINAPI
convert_sddl_to_sd (LPCTSTR StringSecurityDescriptor,
		    DWORD StringSDRevision,
		    PSECURITY_DESCRIPTOR  *SecurityDescriptor,
		    PULONG  SecurityDescriptorSize)
{
  static ConvertStringSecurityDescriptorToSecurityDescriptor_Proc s_pfn_Convert_SDDL_To_SD = NULL;
  BOOL retval;

  if (is_windows_9x () == TRUE)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  if (g_b_init_convert_sddl_to_sd == 0)
    {
      g_b_init_convert_sddl_to_sd = 1;
#ifdef _UNICODE
      s_pfn_Convert_SDDL_To_SD =
	(ConvertStringSecurityDescriptorToSecurityDescriptor_Proc)GetProcAddress (GetModuleHandle ("Advapi32.dll"),
										  "ConvertStringSecurityDescriptorToSecurityDescriptorW");
#else
      s_pfn_Convert_SDDL_To_SD =
	(ConvertStringSecurityDescriptorToSecurityDescriptor_Proc)GetProcAddress (GetModuleHandle ("Advapi32.dll"),
										  "ConvertStringSecurityDescriptorToSecurityDescriptorA");
#endif
    }
  if (s_pfn_Convert_SDDL_To_SD == NULL)
    {
      errno = ENOTSUP;
      return FALSE;
    }

  retval = s_pfn_Convert_SDDL_To_SD (StringSecurityDescriptor,
				     StringSDRevision,
				     SecurityDescriptor,
				     SecurityDescriptorSize);

  return retval;
}

static DWORD WINAPI
get_adapters_info (PIP_ADAPTER_INFO pAdapterInfo, PULONG pOutBufLen)
{
  static GetAdaptersInfo_Proc s_pfn_Get_Adapters_Info = NULL;
  HMODULE hm_iphlpapi = NULL;

  if (is_windows_9x () == TRUE)
    return ERROR_NOT_SUPPORTED;

  if (g_b_init_get_adapters_info == 0)
    {
      g_b_init_get_adapters_info = 1;
      hm_iphlpapi = LoadLibrary ("Iphlpapi.dll");
      if (hm_iphlpapi)
	s_pfn_Get_Adapters_Info = (GetAdaptersInfo_Proc)
	  GetProcAddress (hm_iphlpapi, "GetAdaptersInfo");
    }
  if (s_pfn_Get_Adapters_Info == NULL)
    return ERROR_NOT_SUPPORTED;
  return s_pfn_Get_Adapters_Info (pAdapterInfo, pOutBufLen);
}



/* Return 1 if P is a valid pointer to an object of size SIZE.  Return
   0 if P is NOT a valid pointer.  Return -1 if we cannot validate P.

   This is called from alloc.c:valid_pointer_p.  */
int
w32_valid_pointer_p (void *p, int size)
{
  SIZE_T done;
  HANDLE h = OpenProcess (PROCESS_VM_READ, FALSE, GetCurrentProcessId ());

  if (h)
    {
      unsigned char *buf = alloca (size);
      int retval = ReadProcessMemory (h, p, buf, size, &done);

      CloseHandle (h);
      return retval;
    }
  else
    return -1;
}



/* Here's an overview of how the Windows build supports file names
   that cannot be encoded by the current system codepage.

   From the POV of Lisp and layers of C code above the functions here,
   Emacs on Windows pretends that its file names are encoded in UTF-8;
   see encode_file and decode_file on coding.c.  Any file name that is
   passed as a unibyte string to C functions defined here is assumed
   to be in UTF-8 encoding.  Any file name returned by functions
   defined here must be in UTF-8 encoding, with only a few exceptions
   reserved for a couple of special cases.  (Be sure to use
   MAX_UTF8_PATH for char arrays that store UTF-8 encoded file names,
   as they can be much longer than MAX_PATH!)

   The UTF-8 encoded file names cannot be passed to system APIs, as
   Windows does not support that.  Therefore, they are converted
   either to UTF-16 or to the ANSI codepage, depending on the value of
   w32-unicode-filenames, before calling any system APIs or CRT library
   functions.  The default value of that variable is determined by the
   OS on which Emacs runs: nil on Windows 9X and t otherwise, but the
   user can change that default (although I don't see why would she
   want to).

   The 4 functions defined below, filename_to_utf16, filename_to_ansi,
   filename_from_utf16, and filename_from_ansi, are the workhorses of
   these conversions.  They rely on Windows native APIs
   MultiByteToWideChar and WideCharToMultiByte; we cannot use
   functions from coding.c here, because they allocate memory, which
   is a bad idea on the level of libc, which is what the functions
   here emulate.  (If you worry about performance due to constant
   conversion back and forth from UTF-8 to UTF-16, then don't: first,
   it was measured to take only a few microseconds on a not-so-fast
   machine, and second, that's exactly what the ANSI APIs we used
   before did anyway, because they are just thin wrappers around the
   Unicode APIs.)

   The variables file-name-coding-system and default-file-name-coding-system
   still exist, but are actually used only when a file name needs to
   be converted to the ANSI codepage.  This happens all the time when
   w32-unicode-filenames is nil, but can also happen from time to time
   when it is t.  Otherwise, these variables have no effect on file-name
   encoding when w32-unicode-filenames is t; this is similar to
   selection-coding-system.

   This arrangement works very well, but it has a few gotchas and
   limitations:

   . Lisp code that encodes or decodes file names manually should
     normally use 'utf-8' as the coding-system on Windows,
     disregarding file-name-coding-system.  This is a somewhat
     unpleasant consequence, but it cannot be avoided.  Fortunately,
     very few Lisp packages need to do that.

     More generally, passing to library functions (e.g., fopen or
     opendir) file names already encoded in the ANSI codepage is
     explicitly *verboten*, as all those functions, as shadowed and
     emulated here, assume they will receive UTF-8 encoded file names.

     For the same reasons, no CRT function or Win32 API can be called
     directly in Emacs sources, without either converting the file
     names from UTF-8 to UTF-16 or ANSI codepage, or going through
     some shadowing function defined here.

   . Environment variables stored in Vprocess_environment are encoded
     in the ANSI codepage, so if getenv/egetenv is used for a variable
     whose value is a file name or a list of directories, it needs to
     be converted to UTF-8, before it is used as argument to functions
     or decoded into a Lisp string.

   . File names passed to external libraries, like the image libraries
     and GnuTLS, need special handling.  These libraries generally
     don't support UTF-16 or UTF-8 file names, so they must get file
     names encoded in the ANSI codepage.  To facilitate using these
     libraries with file names that are not encodable in the ANSI
     codepage, use the function ansi_encode_filename, which will try
     to use the short 8+3 alias of a file name if that file name is
     not encodable in the ANSI codepage.  See image.c and gnutls.c for
     examples of how this should be done.

   . Running subprocesses in non-ASCII directories and with non-ASCII
     file arguments is limited to the current codepage (even though
     Emacs is perfectly capable of finding an executable program file
     in a directory whose name cannot be encoded in the current
     codepage).  This is because the command-line arguments are
     encoded _before_ they get to the w32-specific level, and the
     encoding is not known in advance (it doesn't have to be the
     current ANSI codepage), so w32proc.c functions cannot re-encode
     them in UTF-16.  This should be fixed, but will also require
     changes in cmdproxy.  The current limitation is not terribly bad
     anyway, since very few, if any, Windows console programs that are
     likely to be invoked by Emacs support UTF-16 encoded command
     lines.

   . For similar reasons, server.el and emacsclient are also limited
     to the current ANSI codepage for now.

   . Emacs itself can only handle command-line arguments encoded in
     the current codepage.

   . Turning on w32-unicode-filename on Windows 9X (if it at all
     works) requires UNICOWS.DLL, which is thus a requirement even in
     non-GUI sessions, something that we previously avoided.  */



/* Converting file names from UTF-8 to either UTF-16 or the ANSI
   codepage defined by file-name-coding-system.  */

/* Current codepage for encoding file names.  */
static int file_name_codepage;

/* Initialize the codepage used for decoding file names.  This is
   needed to undo the value recorded during dumping, which might not
   be correct when we run the dumped Emacs.  */
void
w32_init_file_name_codepage (void)
{
  file_name_codepage = CP_ACP;
  w32_ansi_code_page = CP_ACP;
}

/* Produce a Windows ANSI codepage suitable for encoding file names.
   Return the information about that codepage in CP_INFO.  */
int
codepage_for_filenames (CPINFO *cp_info)
{
  /* A simple cache to avoid calling GetCPInfo every time we need to
     encode/decode a file name.  The file-name encoding is not
     supposed to be changed too frequently, if ever.  */
  static Lisp_Object last_file_name_encoding;
  static CPINFO cp;
  Lisp_Object current_encoding;

  current_encoding = Vfile_name_coding_system;
  if (NILP (current_encoding))
    current_encoding = Vdefault_file_name_coding_system;

  if (!EQ (last_file_name_encoding, current_encoding)
      || NILP (last_file_name_encoding))
    {
      /* Default to the current ANSI codepage.  */
      file_name_codepage = w32_ansi_code_page;

      if (!NILP (current_encoding))
	{
	  char *cpname = SSDATA (SYMBOL_NAME (current_encoding));
	  char *cp = NULL, *end;
	  int cpnum;

	  if (strncmp (cpname, "cp", 2) == 0)
	    cp = cpname + 2;
	  else if (strncmp (cpname, "windows-", 8) == 0)
	    cp = cpname + 8;

	  if (cp)
	    {
	      end = cp;
	      cpnum = strtol (cp, &end, 10);
	      if (cpnum && *end == '\0' && end - cp >= 2)
		file_name_codepage = cpnum;
	    }
	}

      if (!file_name_codepage)
	file_name_codepage = CP_ACP; /* CP_ACP = 0, but let's not assume that */

      if (!GetCPInfo (file_name_codepage, &cp))
	{
	  file_name_codepage = CP_ACP;
	  if (!GetCPInfo (file_name_codepage, &cp))
	    emacs_abort ();
	}

      /* Cache the new value.  */
      last_file_name_encoding = current_encoding;
    }
  if (cp_info)
    *cp_info = cp;

  return file_name_codepage;
}

int
filename_to_utf16 (const char *fn_in, wchar_t *fn_out)
{
  int result = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags, fn_in,
				     -1, fn_out, MAX_PATH);

  if (!result)
    {
      DWORD err = GetLastError ();

      switch (err)
	{
	case ERROR_INVALID_FLAGS:
	case ERROR_INVALID_PARAMETER:
	  errno = EINVAL;
	  break;
	case ERROR_INSUFFICIENT_BUFFER:
	case ERROR_NO_UNICODE_TRANSLATION:
	default:
	  errno = ENOENT;
	  break;
	}
      return -1;
    }
  return 0;
}

int
filename_from_utf16 (const wchar_t *fn_in, char *fn_out)
{
  int result = pWideCharToMultiByte (CP_UTF8, 0, fn_in, -1,
				     fn_out, MAX_UTF8_PATH, NULL, NULL);

  if (!result)
    {
      DWORD err = GetLastError ();

      switch (err)
	{
	case ERROR_INVALID_FLAGS:
	case ERROR_INVALID_PARAMETER:
	  errno = EINVAL;
	  break;
	case ERROR_INSUFFICIENT_BUFFER:
	case ERROR_NO_UNICODE_TRANSLATION:
	default:
	  errno = ENOENT;
	  break;
	}
      return -1;
    }
  return 0;
}

int
filename_to_ansi (const char *fn_in, char *fn_out)
{
  wchar_t fn_utf16[MAX_PATH];

  if (filename_to_utf16 (fn_in, fn_utf16) == 0)
    {
      int result;
      int codepage = codepage_for_filenames (NULL);

      result  = pWideCharToMultiByte (codepage, 0, fn_utf16, -1,
				      fn_out, MAX_PATH, NULL, NULL);
      if (!result)
	{
	  DWORD err = GetLastError ();

	  switch (err)
	    {
	    case ERROR_INVALID_FLAGS:
	    case ERROR_INVALID_PARAMETER:
	      errno = EINVAL;
	      break;
	    case ERROR_INSUFFICIENT_BUFFER:
	    case ERROR_NO_UNICODE_TRANSLATION:
	    default:
	      errno = ENOENT;
	      break;
	    }
	  return -1;
	}
      return 0;
    }
  return -1;
}

int
filename_from_ansi (const char *fn_in, char *fn_out)
{
  wchar_t fn_utf16[MAX_PATH];
  int codepage = codepage_for_filenames (NULL);
  int result = pMultiByteToWideChar (codepage, multiByteToWideCharFlags, fn_in,
				     -1, fn_utf16, MAX_PATH);

  if (!result)
    {
      DWORD err = GetLastError ();

      switch (err)
	{
	case ERROR_INVALID_FLAGS:
	case ERROR_INVALID_PARAMETER:
	  errno = EINVAL;
	  break;
	case ERROR_INSUFFICIENT_BUFFER:
	case ERROR_NO_UNICODE_TRANSLATION:
	default:
	  errno = ENOENT;
	  break;
	}
      return -1;
    }
  return filename_from_utf16 (fn_utf16, fn_out);
}



/* The directory where we started, in UTF-8. */
static char startup_dir[MAX_UTF8_PATH];

/* Get the current working directory.  */
char *
getcwd (char *dir, int dirsize)
{
  if (!dirsize)
    {
      errno = EINVAL;
      return NULL;
    }
  if (dirsize <= strlen (startup_dir))
    {
      errno = ERANGE;
      return NULL;
    }
#if 0
  if (GetCurrentDirectory (MAXPATHLEN, dir) > 0)
    return dir;
  return NULL;
#else
  /* Emacs doesn't actually change directory itself, it stays in the
     same directory where it was started.  */
  strcpy (dir, startup_dir);
  return dir;
#endif
}

/* Emulate getloadavg.  */

struct load_sample {
  time_t sample_time;
  ULONGLONG idle;
  ULONGLONG kernel;
  ULONGLONG user;
};

/* Number of processors on this machine.  */
static unsigned num_of_processors;

/* We maintain 1-sec samples for the last 16 minutes in a circular buffer.  */
static struct load_sample samples[16*60];
static int first_idx = -1, last_idx = -1;
static int max_idx = ARRAYELTS (samples);

static int
buf_next (int from)
{
  int next_idx = from + 1;

  if (next_idx >= max_idx)
    next_idx = 0;

  return next_idx;
}

static int
buf_prev (int from)
{
  int prev_idx = from - 1;

  if (prev_idx < 0)
    prev_idx = max_idx - 1;

  return prev_idx;
}

static void
sample_system_load (ULONGLONG *idle, ULONGLONG *kernel, ULONGLONG *user)
{
  SYSTEM_INFO sysinfo;
  FILETIME ft_idle, ft_user, ft_kernel;

  /* Initialize the number of processors on this machine.  */
  if (num_of_processors <= 0)
    {
      get_native_system_info (&sysinfo);
      num_of_processors = sysinfo.dwNumberOfProcessors;
      if (num_of_processors <= 0)
	{
	  GetSystemInfo (&sysinfo);
	  num_of_processors = sysinfo.dwNumberOfProcessors;
	}
      if (num_of_processors <= 0)
	num_of_processors = 1;
    }

  /* TODO: Take into account threads that are ready to run, by
     sampling the "\System\Processor Queue Length" performance
     counter.  The code below accounts only for threads that are
     actually running.  */

  if (get_system_times (&ft_idle, &ft_kernel, &ft_user))
    {
      ULARGE_INTEGER uidle, ukernel, uuser;

      memcpy (&uidle, &ft_idle, sizeof (ft_idle));
      memcpy (&ukernel, &ft_kernel, sizeof (ft_kernel));
      memcpy (&uuser, &ft_user, sizeof (ft_user));
      *idle = uidle.QuadPart;
      *kernel = ukernel.QuadPart;
      *user = uuser.QuadPart;
    }
  else
    {
      *idle = 0;
      *kernel = 0;
      *user = 0;
    }
}

/* Produce the load average for a given time interval, using the
   samples in the samples[] array.  WHICH can be 0, 1, or 2, meaning
   1-minute, 5-minute, or 15-minute average, respectively. */
static double
getavg (int which)
{
  double retval = -1.0;
  double tdiff;
  int idx;
  double span = (which == 0 ? 1.0 : (which == 1 ? 5.0 : 15.0)) * 60;
  time_t now = samples[last_idx].sample_time;

  if (first_idx != last_idx)
    {
      for (idx = buf_prev (last_idx); ; idx = buf_prev (idx))
	{
	  tdiff = difftime (now, samples[idx].sample_time);
	  if (tdiff >= span - 2*DBL_EPSILON*now)
	    {
	      long double sys =
		samples[last_idx].kernel + samples[last_idx].user
		- (samples[idx].kernel + samples[idx].user);
	      long double idl = samples[last_idx].idle - samples[idx].idle;

	      retval = (1.0 - idl / sys) * num_of_processors;
	      break;
	    }
	  if (idx == first_idx)
	    break;
	}
    }

  return retval;
}

int
getloadavg (double loadavg[], int nelem)
{
  int elem;
  ULONGLONG idle, kernel, user;
  time_t now = time (NULL);

  /* If system time jumped back for some reason, delete all samples
     whose time is later than the current wall-clock time.  This
     prevents load average figures from becoming frozen for prolonged
     periods of time, when system time is reset backwards.  */
  if (last_idx >= 0)
    {
      while (difftime (now, samples[last_idx].sample_time) < -1.0)
	{
	  if (last_idx == first_idx)
	    {
	      first_idx = last_idx = -1;
	      break;
	    }
	  last_idx = buf_prev (last_idx);
	}
    }

  /* Store another sample.  We ignore samples that are less than 1 sec
     apart.  */
  if (last_idx < 0
      || (difftime (now, samples[last_idx].sample_time)
	  >= 1.0 - 2*DBL_EPSILON*now))
    {
      sample_system_load (&idle, &kernel, &user);
      last_idx = buf_next (last_idx);
      samples[last_idx].sample_time = now;
      samples[last_idx].idle = idle;
      samples[last_idx].kernel = kernel;
      samples[last_idx].user = user;
      /* If the buffer has more that 15 min worth of samples, discard
	 the old ones.  */
      if (first_idx == -1)
	first_idx = last_idx;
      while (first_idx != last_idx
	     && (difftime (now, samples[first_idx].sample_time)
	         >= 15.0*60 + 2*DBL_EPSILON*now))
	first_idx = buf_next (first_idx);
    }

  for (elem = 0; elem < nelem; elem++)
    {
      double avg = getavg (elem);

      if (avg < 0)
	break;
      loadavg[elem] = avg;
    }

  return elem;
}

/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char dflt_passwd_name[PASSWD_FIELD_SIZE];
static char dflt_passwd_passwd[PASSWD_FIELD_SIZE];
static char dflt_passwd_gecos[PASSWD_FIELD_SIZE];
static char dflt_passwd_dir[MAX_UTF8_PATH];
static char dflt_passwd_shell[MAX_UTF8_PATH];

static struct passwd dflt_passwd =
{
  dflt_passwd_name,
  dflt_passwd_passwd,
  0,
  0,
  0,
  dflt_passwd_gecos,
  dflt_passwd_dir,
  dflt_passwd_shell,
};

static char dflt_group_name[GNLEN+1];

static struct group dflt_group =
{
  /* When group information is not available, we return this as the
     group for all files.  */
  dflt_group_name,
  0,
};

unsigned
getuid (void)
{
  return dflt_passwd.pw_uid;
}

unsigned
geteuid (void)
{
  /* I could imagine arguing for checking to see whether the user is
     in the Administrators group and returning a UID of 0 for that
     case, but I don't know how wise that would be in the long run.  */
  return getuid ();
}

unsigned
getgid (void)
{
  return dflt_passwd.pw_gid;
}

unsigned
getegid (void)
{
  return getgid ();
}

struct passwd *
getpwuid (unsigned uid)
{
  if (uid == dflt_passwd.pw_uid)
    return &dflt_passwd;
  return NULL;
}

struct group *
getgrgid (gid_t gid)
{
  return &dflt_group;
}

struct passwd *
getpwnam (char *name)
{
  struct passwd *pw;

  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (xstrcasecmp (name, pw->pw_name))
    return NULL;

  return pw;
}

static void
init_user_info (void)
{
  /* Find the user's real name by opening the process token and
     looking up the name associated with the user-sid in that token.

     Use the relative portion of the identifier authority value from
     the user-sid as the user id value (same for group id using the
     primary group sid from the process token). */

  char         uname[UNLEN+1], gname[GNLEN+1], domain[1025];
  DWORD        ulength = sizeof (uname), dlength = sizeof (domain), needed;
  DWORD	       glength = sizeof (gname);
  HANDLE       token = NULL;
  SID_NAME_USE user_type;
  unsigned char *buf = NULL;
  DWORD        blen = 0;
  TOKEN_USER   user_token;
  TOKEN_PRIMARY_GROUP group_token;
  BOOL         result;

  result = open_process_token (GetCurrentProcess (), TOKEN_QUERY, &token);
  if (result)
    {
      result = get_token_information (token, TokenUser, NULL, 0, &blen);
      if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	{
	  buf = xmalloc (blen);
	  result = get_token_information (token, TokenUser,
					  (LPVOID)buf, blen, &needed);
	  if (result)
	    {
	      memcpy (&user_token, buf, sizeof (user_token));
	      result = lookup_account_sid (NULL, user_token.User.Sid,
					   uname, &ulength,
					   domain, &dlength, &user_type);
	    }
	}
      else
	result = FALSE;
    }
  if (result)
    {
      strcpy (dflt_passwd.pw_name, uname);
      /* Determine a reasonable uid value.  */
      if (xstrcasecmp ("administrator", uname) == 0)
	{
	  dflt_passwd.pw_uid = 500; /* well-known Administrator uid */
	  dflt_passwd.pw_gid = 513; /* well-known None gid */
	}
      else
	{
	  /* Use the last sub-authority value of the RID, the relative
	     portion of the SID, as user/group ID. */
	  dflt_passwd.pw_uid = get_rid (user_token.User.Sid);

	  /* Get group id and name.  */
	  result = get_token_information (token, TokenPrimaryGroup,
					  (LPVOID)buf, blen, &needed);
	  if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	    {
	      buf = xrealloc (buf, blen = needed);
	      result = get_token_information (token, TokenPrimaryGroup,
					      (LPVOID)buf, blen, &needed);
	    }
	  if (result)
	    {
	      memcpy (&group_token, buf, sizeof (group_token));
	      dflt_passwd.pw_gid = get_rid (group_token.PrimaryGroup);
	      dlength = sizeof (domain);
	      /* If we can get at the real Primary Group name, use that.
		 Otherwise, the default group name was already set to
		 "None" in globals_of_w32.  */
	      if (lookup_account_sid (NULL, group_token.PrimaryGroup,
				      gname, &glength, NULL, &dlength,
				      &user_type))
		strcpy (dflt_group_name, gname);
	    }
	  else
	    dflt_passwd.pw_gid = dflt_passwd.pw_uid;
	}
    }
  /* If security calls are not supported (presumably because we
     are running under Windows 9X), fallback to this: */
  else if (GetUserName (uname, &ulength))
    {
      strcpy (dflt_passwd.pw_name, uname);
      if (xstrcasecmp ("administrator", uname) == 0)
	dflt_passwd.pw_uid = 0;
      else
	dflt_passwd.pw_uid = 123;
      dflt_passwd.pw_gid = dflt_passwd.pw_uid;
    }
  else
    {
      strcpy (dflt_passwd.pw_name, "unknown");
      dflt_passwd.pw_uid = 123;
      dflt_passwd.pw_gid = 123;
    }
  dflt_group.gr_gid = dflt_passwd.pw_gid;

  /* Set dir and shell from environment variables. */
  if (w32_unicode_filenames)
    {
      wchar_t *home = _wgetenv (L"HOME");
      wchar_t *shell = _wgetenv (L"SHELL");

      /* Ensure HOME and SHELL are defined. */
      if (home == NULL)
	emacs_abort ();
      if (shell == NULL)
	emacs_abort ();
      filename_from_utf16 (home, dflt_passwd.pw_dir);
      filename_from_utf16 (shell, dflt_passwd.pw_shell);
    }
  else
    {
      char *home = getenv ("HOME");
      char *shell = getenv ("SHELL");

      if (home == NULL)
	emacs_abort ();
      if (shell == NULL)
	emacs_abort ();
      filename_from_ansi (home, dflt_passwd.pw_dir);
      filename_from_ansi (shell, dflt_passwd.pw_shell);
    }

  xfree (buf);
  if (token)
    CloseHandle (token);
}

static HCRYPTPROV w32_crypto_hprov;
static int
w32_init_crypt_random (void)
{
  if (!CryptAcquireContext (&w32_crypto_hprov, NULL, NULL, PROV_RSA_FULL,
			    CRYPT_VERIFYCONTEXT | CRYPT_SILENT))
    {
      DebPrint (("CryptAcquireContext failed with error %x\n",
		 GetLastError ()));
      w32_crypto_hprov = 0;
      return -1;
    }
  return 0;
}

int
w32_init_random (void *buf, ptrdiff_t buflen)
{
  if (!w32_crypto_hprov)
    w32_init_crypt_random ();
  if (w32_crypto_hprov)
    {
      if (CryptGenRandom (w32_crypto_hprov, buflen, (BYTE *)buf))
	return 0;
    }
  return -1;
}

/* MS-Windows 'rand' produces separate identical series for each
   thread, so we replace it with our version.  */

/* Algorithm AS183: An Efficient and Portable Pseudo-random Number
   Generator, by B.A. Wichmann, I.D. Hill.  AS, v31, No. 2 (1982).  */
static int ix = 3172, iy = 9814, iz = 20125;
#define RAND_MAX_X  30269
#define RAND_MAX_Y  30307
#define RAND_MAX_Z  30323

static int
rand_as183 (void)
{
  ix = (171 * ix) % RAND_MAX_X;
  iy = (172 * iy) % RAND_MAX_Y;
  iz = (170 * iz) % RAND_MAX_Z;

  return (ix + iy + iz) & 0x7fff;
}

int
random (void)
{
  /* rand_as183 () gives us 15 random bits...hack together 30 bits.  */
  return ((rand_as183 () << 15) | rand_as183 ());
}

void
srandom (int seed)
{
  srand (seed);
  ix = rand () % RAND_MAX_X;
  iy = rand () % RAND_MAX_Y;
  iz = rand () % RAND_MAX_Z;
}

/* Return the maximum length in bytes of a multibyte character
   sequence encoded in the current ANSI codepage.  This is required to
   correctly walk the encoded file names one character at a time.  */
static int
max_filename_mbslen (void)
{
  CPINFO cp_info;

  codepage_for_filenames (&cp_info);
  return cp_info.MaxCharSize;
}

/* Normalize filename by converting in-place all of its path
   separators to the separator specified by PATH_SEP.  */

static void
normalize_filename (register char *fp, char path_sep)
{
  char *p2;

  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so filenames can be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  p2 = fp + 1;

  if (*p2 == ':' && *fp >= 'A' && *fp <= 'Z')
    {
      *fp += 'a' - 'A';
      fp += 2;
    }

  while (*fp)
    {
      if ((*fp == '/' || *fp == '\\') && *fp != path_sep)
	*fp = path_sep;
      fp++;
    }
}

/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (register char *p)
{
  normalize_filename (p, '/');
}

/* Destructively turn slashes into backslashes.  */
void
unixtodos_filename (register char *p)
{
  normalize_filename (p, '\\');
}

/* Remove all CR's that are followed by a LF.
   (From msdos.c...probably should figure out a way to share it,
   although this code isn't going to ever change.)  */
static int
crlf_to_lf (register int n, register char *buf)
{
  unsigned char *np = (unsigned char *)buf;
  unsigned char *startp = np;
  char *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

/* Parse the root part of file name, if present.  Return length and
    optionally store pointer to char after root.  */
static int
parse_root (const char * name, const char ** pPath)
{
  const char * start = name;

  if (name == NULL)
    return 0;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      /* skip past drive specifier */
      name += 2;
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      int slashes = 2;

      name += 2;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  name++;
	}
      while ( *name );
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }

  if (pPath)
    *pPath = name;

  return name - start;
}

/* Get long base name for name; name is assumed to be absolute.  */
static int
get_long_basename (char * name, char * buf, int size)
{
  HANDLE dir_handle = INVALID_HANDLE_VALUE;
  char fname_utf8[MAX_UTF8_PATH];
  int len = 0;
  int cstatus = -1;

  /* Must be valid filename, no wild cards or other invalid characters.  */
  if (strpbrk (name, "*?|<>\""))
    return 0;

  if (w32_unicode_filenames)
    {
      wchar_t fname_utf16[MAX_PATH];
      WIN32_FIND_DATAW find_data_wide;

      filename_to_utf16 (name, fname_utf16);
      dir_handle = FindFirstFileW (fname_utf16, &find_data_wide);
      if (dir_handle != INVALID_HANDLE_VALUE)
	cstatus = filename_from_utf16 (find_data_wide.cFileName, fname_utf8);
    }
  else
    {
      char fname_ansi[MAX_PATH];
      WIN32_FIND_DATAA find_data_ansi;

      filename_to_ansi (name, fname_ansi);
      /* If the ANSI name includes ? characters, it is not encodable
	 in the ANSI codepage.  In that case, we deliver the question
	 marks to the caller; calling FindFirstFileA in this case
	 could return some unrelated file name in the same
	 directory.  */
      if (_mbspbrk (fname_ansi, "?"))
	{
	  /* Find the basename of fname_ansi.  */
	  char *p = strrchr (fname_ansi, '\\');

	  if (!p)
	    p = fname_ansi;
	  else
	    p++;
	  cstatus = filename_from_ansi (p, fname_utf8);
	}
      else
	{
	  dir_handle = FindFirstFileA (fname_ansi, &find_data_ansi);
	  if (dir_handle != INVALID_HANDLE_VALUE)
	    cstatus = filename_from_ansi (find_data_ansi.cFileName, fname_utf8);
	}
    }

  if (cstatus == 0 && (len = strlen (fname_utf8)) < size)
    memcpy (buf, fname_utf8, len + 1);
  else
    len = 0;

  if (dir_handle != INVALID_HANDLE_VALUE)
    FindClose (dir_handle);

  return len;
}

/* Get long name for file, if possible (assumed to be absolute).  */
BOOL
w32_get_long_filename (const char * name, char * buf, int size)
{
  char * o = buf;
  char * p;
  const char * q;
  char full[ MAX_UTF8_PATH ];
  int len;

  len = strlen (name);
  if (len >= MAX_UTF8_PATH)
    return FALSE;

  /* Use local copy for destructive modification.  */
  memcpy (full, name, len+1);
  unixtodos_filename (full);

  /* Copy root part verbatim.  */
  len = parse_root (full, (const char **)&p);
  memcpy (o, full, len);
  o += len;
  *o = '\0';
  size -= len;

  while (p != NULL && *p)
    {
      q = p;
      p = strchr (q, '\\');
      if (p) *p = '\0';
      len = get_long_basename (full, o, size);
      if (len > 0)
	{
	  o += len;
	  size -= len;
	  if (p != NULL)
	    {
	      *p++ = '\\';
	      if (size < 2)
		return FALSE;
	      *o++ = '\\';
	      size--;
	      *o = '\0';
	    }
	}
      else
	return FALSE;
    }

  return TRUE;
}

unsigned int
w32_get_short_filename (const char * name, char * buf, int size)
{
  if (w32_unicode_filenames)
    {
      wchar_t name_utf16[MAX_PATH], short_name[MAX_PATH];
      unsigned int retval;

      filename_to_utf16 (name, name_utf16);
      retval = GetShortPathNameW (name_utf16, short_name, size);
      if (retval && retval < size)
	filename_from_utf16 (short_name, buf);
      return retval;
    }
  else
    {
      char name_ansi[MAX_PATH];

      filename_to_ansi (name, name_ansi);
      return GetShortPathNameA (name_ansi, buf, size);
    }
}

/* Re-encode FILENAME, a UTF-8 encoded unibyte string, using the
   MS-Windows ANSI codepage.  If FILENAME includes characters not
   supported by the ANSI codepage, return the 8+3 alias of FILENAME,
   if it exists.  This is needed because the w32 build wants to
   support file names outside of the system locale, but image
   libraries typically don't support wide (a.k.a. "Unicode") APIs
   required for that.  */

Lisp_Object
ansi_encode_filename (Lisp_Object filename)
{
  Lisp_Object encoded_filename;
  char fname[MAX_PATH];

  filename_to_ansi (SSDATA (filename), fname);
  if (_mbspbrk (fname, "?"))
    {
      char shortname[MAX_PATH];

      if (w32_get_short_filename (SSDATA (filename), shortname, MAX_PATH))
	{
	  dostounix_filename (shortname);
	  encoded_filename = build_string (shortname);
	}
      else
	encoded_filename = build_unibyte_string (fname);
    }
  else
    encoded_filename = build_unibyte_string (fname);
  return encoded_filename;
}

static int
is_unc_volume (const char *filename)
{
  const char *ptr = filename;

  if (!IS_DIRECTORY_SEP (ptr[0]) || !IS_DIRECTORY_SEP (ptr[1]) || !ptr[2])
    return 0;

  if (strpbrk (ptr + 2, "*?|<>\"\\/"))
    return 0;

  return 1;
}

/* Emulate the Posix unsetenv.  */
int
unsetenv (const char *name)
{
  char *var;
  size_t name_len;

  if (name == NULL || *name == '\0' || strchr (name, '=') != NULL)
    {
      errno = EINVAL;
      return -1;
    }
  name_len = strlen (name);
  /* MS docs says an environment variable cannot be longer than 32K.  */
  if (name_len > 32767)
    {
      errno = ENOMEM;
      return 0;
    }
  /* It is safe to use 'alloca' with 32K size, since the stack is at
     least 2MB, and we set it to 8MB in the link command line.  */
  var = alloca (name_len + 2);
  strncpy (var, name, name_len);
  var[name_len++] = '=';
  var[name_len] = '\0';
  return _putenv (var);
}

/* MS _putenv doesn't support removing a variable when the argument
   does not include the '=' character, so we fix that here.  */
int
sys_putenv (char *str)
{
  const char *const name_end = strchr (str, '=');

  if (name_end == NULL)
    {
      /* Remove the variable from the environment.  */
      return unsetenv (str);
    }

  if (strncmp (str, "TZ=<", 4) == 0)
    {
      /* MS-Windows does not support POSIX.1-2001 angle-bracket TZ
	 abbreviation syntax.  Convert to POSIX.1-1988 syntax if possible,
	 and to the undocumented placeholder "ZZZ" otherwise.  */
      bool supported_abbr = true;
      for (char *p = str + 4; *p; p++)
	{
	  if (('0' <= *p && *p <= '9') || *p == '-' || *p == '+')
	    supported_abbr = false;
	  else if (*p == '>')
	    {
	      ptrdiff_t abbrlen;
	      if (supported_abbr)
		{
		  abbrlen = p - (str + 4);
		  memmove (str + 3, str + 4, abbrlen);
		}
	      else
		{
		  abbrlen = 3;
		  memset (str + 3, 'Z', abbrlen);
		}
	      memmove (str + 3 + abbrlen, p + 1, strlen (p));
	      break;
	    }
	}
    }

  return _putenv (str);
}

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

LPBYTE
w32_get_resource (const char *key, LPDWORD lpdwtype)
{
  LPBYTE lpvalue;
  HKEY hrootkey = NULL;
  DWORD cbData;

  /* Check both the current user and the local machine to see if
     we have any resources.  */

  if (RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS
	  && (lpvalue = xmalloc (cbData)) != NULL
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
          RegCloseKey (hrootkey);
	  return (lpvalue);
	}

      xfree (lpvalue);

      RegCloseKey (hrootkey);
    }

  if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS
	  && (lpvalue = xmalloc (cbData)) != NULL
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
          RegCloseKey (hrootkey);
	  return (lpvalue);
	}

      xfree (lpvalue);

      RegCloseKey (hrootkey);
    }

  return (NULL);
}

/* The argv[] array holds ANSI-encoded strings, and so this function
   works with ANS_encoded strings.  */
void
init_environment (char ** argv)
{
  static const char * const tempdirs[] = {
    "$TMPDIR", "$TEMP", "$TMP", "c:/"
  };

  int i;

  const int imax = ARRAYELTS (tempdirs);

  /* Implementation note: This function explicitly works with ANSI
     file names, not with UTF-8 encoded file names.  This is because
     this function pushes variables into the Emacs's environment, and
     the environment variables are always assumed to be in the
     locale-specific encoding.  Do NOT call any functions that accept
     UTF-8 file names from this function!  */

  /* Make sure they have a usable $TMPDIR.  Many Emacs functions use
     temporary files and assume "/tmp" if $TMPDIR is unset, which
     will break on DOS/Windows.  Refuse to work if we cannot find
     a directory, not even "c:/", usable for that purpose.  */
  for (i = 0; i < imax ; i++)
    {
      const char *tmp = tempdirs[i];

      if (*tmp == '$')
	tmp = getenv (tmp + 1);
      /* Note that `access' can lie to us if the directory resides on a
	 read-only filesystem, like CD-ROM or a write-protected floppy.
	 The only way to be really sure is to actually create a file and
	 see if it succeeds.  But I think that's too much to ask.  */

      /* MSVCRT's _access crashes with D_OK, so we use our replacement.  */
      if (tmp && sys_access (tmp, D_OK) == 0)
	{
	  char * var = alloca (strlen (tmp) + 8);
	  sprintf (var, "TMPDIR=%s", tmp);
	  _putenv (strdup (var));
	  break;
	}
    }
  if (i >= imax)
    cmd_error_internal
      (Fcons (Qerror,
	      Fcons (build_string ("no usable temporary directories found!!"),
		     Qnil)),
       "While setting TMPDIR: ");

  /* Check for environment variables and use registry settings if they
     don't exist.  Fallback on default values where applicable.  */
  {
    int i;
    LPBYTE lpval;
    DWORD dwType;
    char locale_name[32];
    char default_home[MAX_PATH];
    int appdata = 0;

    static const struct env_entry
    {
      const char * name;
      const char * def_value;
    } dflt_envvars[] =
    {
      /* If the default value is NULL, we will use the value from the
	 outside environment or the Registry, but will not push the
	 variable into the Emacs environment if it is defined neither
	 in the Registry nor in the outside environment.  */
      {"HOME", "C:/"},
      {"PRELOAD_WINSOCK", NULL},
      {"emacs_dir", "C:/emacs"},
      {"EMACSLOADPATH", NULL},
      {"SHELL", "cmdproxy.exe"}, /* perhaps it is somewhere on PATH */
      {"EMACSDATA", NULL},
      {"EMACSPATH", NULL},
      {"INFOPATH", NULL},
      {"EMACSDOC", NULL},
      {"TERM", "cmd"},
      {"LANG", NULL},
    };

#define N_ENV_VARS ARRAYELTS (dflt_envvars)

    /* We need to copy dflt_envvars[] and work on the copy because we
       don't want the dumped Emacs to inherit the values of
       environment variables we saw during dumping (which could be on
       a different system).  The defaults above must be left intact.  */
    struct env_entry env_vars[N_ENV_VARS];

    for (i = 0; i < N_ENV_VARS; i++)
      env_vars[i] = dflt_envvars[i];

    /* For backwards compatibility, check if a .emacs file exists in C:/
       If not, then we can try to default to the appdata directory under the
       user's profile, which is more likely to be writable.   */
    if (sys_access ("C:/.emacs", F_OK) != 0)
      {
	HRESULT profile_result;
	/* Dynamically load ShGetFolderPath, as it won't exist on versions
	   of Windows 95 and NT4 that have not been updated to include
	   MSIE 5.  */
	ShGetFolderPath_fn get_folder_path;
	get_folder_path = (ShGetFolderPath_fn)
	  GetProcAddress (GetModuleHandle ("shell32.dll"), "SHGetFolderPathA");

	if (get_folder_path != NULL)
	  {
	    profile_result = get_folder_path (NULL, CSIDL_APPDATA, NULL,
					      0, default_home);

	    /* If we can't get the appdata dir, revert to old behavior.	 */
	    if (profile_result == S_OK)
	      {
		env_vars[0].def_value = default_home;
		appdata = 1;
	      }
	  }
      }

  /* Get default locale info and use it for LANG.  */
  if (GetLocaleInfo (LOCALE_USER_DEFAULT,
                     LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
                     locale_name, sizeof (locale_name)))
    {
      for (i = 0; i < N_ENV_VARS; i++)
        {
          if (strcmp (env_vars[i].name, "LANG") == 0)
            {
              env_vars[i].def_value = locale_name;
              break;
            }
        }
    }

#define SET_ENV_BUF_SIZE (4 * MAX_PATH)	/* to cover EMACSLOADPATH */

    /* Treat emacs_dir specially: set it unconditionally based on our
       location.  */
    {
      char *p;
      char modname[MAX_PATH];

      if (!GetModuleFileNameA (NULL, modname, MAX_PATH))
	emacs_abort ();
      if ((p = _mbsrchr (modname, '\\')) == NULL)
	emacs_abort ();
      *p = 0;

      if ((p = _mbsrchr (modname, '\\'))
	  /* From bin means installed Emacs, from src means uninstalled.  */
	  && (xstrcasecmp (p, "\\bin") == 0 || xstrcasecmp (p, "\\src") == 0))
	{
	  char buf[SET_ENV_BUF_SIZE];
	  int within_build_tree = xstrcasecmp (p, "\\src") == 0;

	  *p = 0;
	  for (p = modname; *p; p = CharNext (p))
	    if (*p == '\\') *p = '/';

	  _snprintf (buf, sizeof (buf)-1, "emacs_dir=%s", modname);
	  _putenv (strdup (buf));
	  /* If we are running from the Posix-like build tree, define
	     SHELL to point to our own cmdproxy.  The loop below will
	     then disregard PATH_EXEC and the default value.  */
	  if (within_build_tree)
	    {
	      _snprintf (buf, sizeof (buf) - 1,
			 "SHELL=%s/nt/cmdproxy.exe", modname);
	      _putenv (strdup (buf));
	    }
	}
    }

    for (i = 0; i < N_ENV_VARS; i++)
      {
	if (!getenv (env_vars[i].name))
	  {
	    int dont_free = 0;
	    char bufc[SET_ENV_BUF_SIZE];

	    if ((lpval = w32_get_resource (env_vars[i].name, &dwType)) == NULL
		/* Also ignore empty environment variables.  */
		|| *lpval == 0)
	      {
		xfree (lpval);
		dont_free = 1;
		if (strcmp (env_vars[i].name, "SHELL") == 0)
		  {
		    /* Look for cmdproxy.exe in every directory in
		       PATH_EXEC.  FIXME: This does not find cmdproxy
		       in nt/ when we run uninstalled.  */
		    char fname[MAX_PATH];
		    const char *pstart = PATH_EXEC, *pend;

		    do {
		      pend = _mbschr (pstart, ';');
		      if (!pend)
			pend = pstart + strlen (pstart);
		      /* Be defensive against series of ;;; characters.  */
		      if (pend > pstart)
			{
			  strncpy (fname, pstart, pend - pstart);
			  fname[pend - pstart] = '/';
			  strcpy (&fname[pend - pstart + 1], "cmdproxy.exe");
			  ExpandEnvironmentStrings ((LPSTR) fname, bufc,
						    sizeof (bufc));
			  if (sys_access (bufc, F_OK) == 0)
			    {
			      lpval = bufc;
			      dwType = REG_SZ;
			      break;
			    }
			}
		      if (*pend)
			pstart = pend + 1;
		      else
			pstart = pend;
		      if (!*pstart)
			{
			  /* If not found in any directory, use the
			     default as the last resort.  */
			  lpval = (char *)env_vars[i].def_value;
			  dwType = REG_EXPAND_SZ;
			}
		    } while (*pstart);
		  }
		else
		  {
		    lpval = (char *)env_vars[i].def_value;
		    dwType = REG_EXPAND_SZ;
		  }
		if (strcmp (env_vars[i].name, "HOME") == 0 && !appdata)
		  Vdelayed_warnings_list
                    = Fcons
                    (listn (CONSTYPE_HEAP, 2,
                            intern ("initialization"), build_string
                            ("Use of `C:\\.emacs' without defining `HOME'\n"
                             "in the environment is deprecated, "
                             "see `Windows HOME' in the Emacs manual.")),
                     Vdelayed_warnings_list);
	      }

	    if (lpval)
	      {
		char buf1[SET_ENV_BUF_SIZE], buf2[SET_ENV_BUF_SIZE];

		if (dwType == REG_EXPAND_SZ)
		  ExpandEnvironmentStrings ((LPSTR) lpval, buf1, sizeof (buf1));
		else if (dwType == REG_SZ)
		  strcpy (buf1, (char *)lpval);
		if (dwType == REG_EXPAND_SZ || dwType == REG_SZ)
		  {
		    _snprintf (buf2, sizeof (buf2)-1, "%s=%s", env_vars[i].name,
			       buf1);
		    _putenv (strdup (buf2));
		  }

		if (!dont_free)
		  xfree (lpval);
	      }
	  }
      }
  }

  /* Rebuild system configuration to reflect invoking system.  */
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  /* Another special case: on NT, the PATH variable is actually named
     "Path" although cmd.exe (perhaps NT itself) arranges for
     environment variable lookup and setting to be case insensitive.
     However, Emacs assumes a fully case sensitive environment, so we
     need to change "Path" to "PATH" to match the expectations of
     various elisp packages.  We do this by the sneaky method of
     modifying the string in the C runtime environ entry.

     The same applies to COMSPEC.  */
  {
    char ** envp;
    const char *path = "PATH=";
    int path_len = strlen (path);
    const char *comspec = "COMSPEC=";
    int comspec_len = strlen (comspec);

    for (envp = environ; *envp; envp++)
      if (_strnicmp (*envp, path, path_len) == 0)
        memcpy (*envp, path, path_len);
      else if (_strnicmp (*envp, comspec, comspec_len) == 0)
        memcpy (*envp, comspec, comspec_len);

    /* Make the same modification to `process-environment' which has
       already been initialized in set_initial_environment.  */
    for (Lisp_Object env = Vprocess_environment; CONSP (env); env = XCDR (env))
    {
      Lisp_Object entry = XCAR (env);
      if (_strnicmp (SDATA (entry), path, path_len) == 0)
        for (int i = 0; i < path_len; i++)
          SSET (entry, i, path[i]);
      else if (_strnicmp (SDATA (entry), comspec, comspec_len) == 0)
        for (int i = 0; i < comspec_len; i++)
          SSET (entry, i, comspec[i]);
    }
  }

  /* Remember the initial working directory for getcwd.  */
  /* FIXME: Do we need to resolve possible symlinks in startup_dir?
     Does it matter anywhere in Emacs?  */
  if (w32_unicode_filenames)
    {
      wchar_t wstartup_dir[MAX_PATH];

      if (!GetCurrentDirectoryW (MAX_PATH, wstartup_dir))
	emacs_abort ();
      filename_from_utf16 (wstartup_dir, startup_dir);
    }
  else
    {
      char astartup_dir[MAX_PATH];

      if (!GetCurrentDirectoryA (MAX_PATH, astartup_dir))
	emacs_abort ();
      filename_from_ansi (astartup_dir, startup_dir);
    }

  {
    static char modname[MAX_PATH];

    if (!GetModuleFileNameA (NULL, modname, MAX_PATH))
      emacs_abort ();
    argv[0] = modname;
  }

  /* Determine if there is a middle mouse button, to allow parse_button
     to decide whether right mouse events should be mouse-2 or
     mouse-3. */
  w32_num_mouse_buttons = GetSystemMetrics (SM_CMOUSEBUTTONS);

  init_user_info ();
}

/* Called from expand-file-name when default-directory is not a string.  */

char *
emacs_root_dir (void)
{
  static char root_dir[MAX_UTF8_PATH];
  const char *p;

  p = getenv ("emacs_dir");
  if (p == NULL)
    emacs_abort ();
  filename_from_ansi (p, root_dir);
  root_dir[parse_root (root_dir, NULL)] = '\0';
  dostounix_filename (root_dir);
  return root_dir;
}

/* Emulate fdutimens.  */

/* Set the access and modification time stamps of FD (a.k.a. FILE) to be
   TIMESPEC[0] and TIMESPEC[1], respectively.
   FD must be either negative -- in which case it is ignored --
   or a file descriptor that is open on FILE.
   If FD is nonnegative, then FILE can be NULL, which means
   use just futimes instead of utimes.
   If TIMESPEC is null, FAIL.
   Return 0 on success, -1 (setting errno) on failure.  */

int
fdutimens (int fd, char const *file, struct timespec const timespec[2])
{
  if (!timespec)
    {
      errno = ENOSYS;
      return -1;
    }
  if (fd < 0 && !file)
    {
      errno = EBADF;
      return -1;
    }
  /* _futime's prototype defines 2nd arg as having the type 'struct
     _utimbuf', while utime needs to accept 'struct utimbuf' for
     compatibility with Posix.  So we need to use 2 different (but
     equivalent) types to avoid compiler warnings, sigh.  */
  if (fd >= 0)
    {
      struct _utimbuf _ut;

      _ut.actime = timespec[0].tv_sec;
      _ut.modtime = timespec[1].tv_sec;
      return _futime (fd, &_ut);
    }
  else
    {
      struct utimbuf ut;

      ut.actime = timespec[0].tv_sec;
      ut.modtime = timespec[1].tv_sec;
      /* Call 'utime', which is implemented below, not the MS library
	 function, which fails on directories.  */
      return utime (file, &ut);
    }
}


/* ------------------------------------------------------------------------- */
/* IO support and wrapper functions for the Windows API. */
/* ------------------------------------------------------------------------- */

/* Place a wrapper around the MSVC version of ctime.  It returns NULL
   on network directories, so we handle that case here.
   (Ulrich Leodolter, 1/11/95).  */
char *
sys_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : (char *)"Sun Jan 01 00:00:00 1970");
}

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
sys_sleep (int seconds)
{
  Sleep (seconds * 1000);
}

/* Internal MSVC functions for low-level descriptor munging */
extern int __cdecl _set_osfhnd (int fd, long h);
extern int __cdecl _free_osfhnd (int fd);

/* parallel array of private info on file handles */
filedesc fd_info [ MAXDESC ];

typedef struct volume_info_data {
  struct volume_info_data * next;

  /* time when info was obtained */
  DWORD     timestamp;

  /* actual volume info */
  char *    root_dir;
  DWORD     serialnum;
  DWORD     maxcomp;
  DWORD     flags;
  char *    name;
  char *    type;
} volume_info_data;

/* Global referenced by various functions.  */
static volume_info_data volume_info;

/* Vector to indicate which drives are local and fixed (for which cached
   data never expires).  */
static BOOL fixed_drives[26];

/* Consider cached volume information to be stale if older than 10s,
   at least for non-local drives.  Info for fixed drives is never stale.  */
#define DRIVE_INDEX( c ) ( (c) <= 'Z' ? (c) - 'A' : (c) - 'a' )
#define VOLINFO_STILL_VALID( root_dir, info )		\
  ( ( isalpha (root_dir[0]) &&				\
      fixed_drives[ DRIVE_INDEX (root_dir[0]) ] )	\
    || GetTickCount () - info->timestamp < 10000 )

/* Cache support functions.  */

/* Simple linked list with linear search is sufficient.  */
static volume_info_data *volume_cache = NULL;

static volume_info_data *
lookup_volume_info (char * root_dir)
{
  volume_info_data * info;

  for (info = volume_cache; info; info = info->next)
    if (xstrcasecmp (info->root_dir, root_dir) == 0)
      break;
  return info;
}

static void
add_volume_info (char * root_dir, volume_info_data * info)
{
  info->root_dir = xstrdup (root_dir);
  unixtodos_filename (info->root_dir);
  info->next = volume_cache;
  volume_cache = info;
}


/* Wrapper for GetVolumeInformation, which uses caching to avoid
   performance penalty (~2ms on 486 for local drives, 7.5ms for local
   cdrom drive, ~5-10ms or more for remote drives on LAN).  */
static volume_info_data *
GetCachedVolumeInformation (char * root_dir)
{
  volume_info_data * info;
  char default_root[ MAX_UTF8_PATH ];
  char  name[MAX_PATH+1];
  char  type[MAX_PATH+1];

  /* NULL for root_dir means use root from current directory.  */
  if (root_dir == NULL)
    {
      if (w32_unicode_filenames)
	{
	  wchar_t curdirw[MAX_PATH];

	  if (GetCurrentDirectoryW (MAX_PATH, curdirw) == 0)
	    return NULL;
	  filename_from_utf16 (curdirw, default_root);
	}
      else
	{
	  char curdira[MAX_PATH];

	  if (GetCurrentDirectoryA (MAX_PATH, curdira) == 0)
	    return NULL;
	  filename_from_ansi (curdira, default_root);
	}
      parse_root (default_root, (const char **)&root_dir);
      *root_dir = 0;
      root_dir = default_root;
    }

  /* Local fixed drives can be cached permanently.  Removable drives
     cannot be cached permanently, since the volume name and serial
     number (if nothing else) can change.  Remote drives should be
     treated as if they are removable, since there is no sure way to
     tell whether they are or not.  Also, the UNC association of drive
     letters mapped to remote volumes can be changed at any time (even
     by other processes) without notice.

     As a compromise, so we can benefit from caching info for remote
     volumes, we use a simple expiry mechanism to invalidate cache
     entries that are more than ten seconds old.  */

#if 0
  /* No point doing this, because WNetGetConnection is even slower than
     GetVolumeInformation, consistently taking ~50ms on a 486 (FWIW,
     GetDriveType is about the only call of this type which does not
     involve network access, and so is extremely quick).  */

  /* Map drive letter to UNC if remote. */
  if (isalpha (root_dir[0]) && !fixed[DRIVE_INDEX (root_dir[0])])
    {
      char remote_name[ 256 ];
      char drive[3] = { root_dir[0], ':' };

      if (WNetGetConnection (drive, remote_name, sizeof (remote_name))
	  == NO_ERROR)
	/* do something */ ;
    }
#endif

  info = lookup_volume_info (root_dir);

  if (info == NULL || ! VOLINFO_STILL_VALID (root_dir, info))
    {
      DWORD serialnum;
      DWORD maxcomp;
      DWORD flags;

      /* Info is not cached, or is stale. */
      if (w32_unicode_filenames)
	{
	  wchar_t root_w[MAX_PATH];
	  wchar_t  name_w[MAX_PATH+1];
	  wchar_t  type_w[MAX_PATH+1];

	  filename_to_utf16 (root_dir, root_w);
	  if (!GetVolumeInformationW (root_w,
				     name_w, sizeof (name_w),
				     &serialnum,
				     &maxcomp,
				     &flags,
				     type_w, sizeof (type_w)))
	    return NULL;
	  /* Hmm... not really 100% correct, as these 2 are not file
	     names...  */
	  filename_from_utf16 (name_w, name);
	  filename_from_utf16 (type_w, type);
	}
      else
	{
	  char root_a[MAX_PATH];
	  char  name_a[MAX_PATH+1];
	  char  type_a[MAX_PATH+1];

	  filename_to_ansi (root_dir, root_a);
	  if (!GetVolumeInformationA (root_a,
				     name_a, sizeof (name_a),
				     &serialnum,
				     &maxcomp,
				     &flags,
				     type_a, sizeof (type_a)))
	    return NULL;
	  filename_from_ansi (name_a, name);
	  filename_from_ansi (type_a, type);
	}

      /* Cache the volume information for future use, overwriting existing
	 entry if present.  */
      if (info == NULL)
	{
	  info = xmalloc (sizeof (volume_info_data));
	  add_volume_info (root_dir, info);
	}
      else
	{
	  xfree (info->name);
	  xfree (info->type);
	}

      info->name = xstrdup (name);
      unixtodos_filename (info->name);
      info->serialnum = serialnum;
      info->maxcomp = maxcomp;
      info->flags = flags;
      info->type = xstrdup (type);
      info->timestamp = GetTickCount ();
    }

  return info;
}

/* Get information on the volume where NAME is held; set path pointer to
   start of pathname in NAME (past UNC header\volume header if present),
   if pPath is non-NULL.

   Note: if NAME includes symlinks, the information is for the volume
   of the symlink, not of its target.  That's because, even though
   GetVolumeInformation returns information about the symlink target
   of its argument, we only pass the root directory to
   GetVolumeInformation, not the full NAME.  */
static int
get_volume_info (const char * name, const char ** pPath)
{
  char temp[MAX_UTF8_PATH];
  char *rootname = NULL;  /* default to current volume */
  volume_info_data * info;
  int root_len = parse_root (name, pPath);

  if (name == NULL)
    return FALSE;

  /* Copy the root name of the volume, if given.  */
  if (root_len)
    {
      strncpy (temp, name, root_len);
      temp[root_len] = '\0';
      unixtodos_filename (temp);
      rootname = temp;
    }

  info = GetCachedVolumeInformation (rootname);
  if (info != NULL)
    {
      /* Set global referenced by other functions.  */
      volume_info = *info;
      return TRUE;
    }
  return FALSE;
}

/* Determine if volume is FAT format (ie. only supports short 8.3
   names); also set path pointer to start of pathname in name, if
   pPath is non-NULL.  */
static int
is_fat_volume (const char * name, const char ** pPath)
{
  if (get_volume_info (name, pPath))
    return (volume_info.maxcomp == 12);
  return FALSE;
}

/* Convert all slashes in a filename to backslashes, and map filename
   to a valid 8.3 name if necessary.  The result is a pointer to a
   static buffer, so CAVEAT EMPTOR!  */
const char *map_w32_filename (const char *, const char **);

const char *
map_w32_filename (const char * name, const char ** pPath)
{
  static char shortname[MAX_UTF8_PATH];
  char * str = shortname;
  char c;
  char * path;
  const char * save_name = name;

  if (strlen (name) >= sizeof (shortname))
    {
      /* Return a filename which will cause callers to fail.  */
      strcpy (shortname, "?");
      return shortname;
    }

  if (!fatal_error_in_progress	/* disable fancy processing during crash */
      && is_fat_volume (name, (const char **)&path)) /* truncate to 8.3 */
    {
      register int left = 8;	/* maximum number of chars in part */
      register int extn = 0;	/* extension added? */
      register int dots = 2;	/* maximum number of dots allowed */

      while (name < path)
	*str++ = *name++;	/* skip past UNC header */

      while ((c = *name++))
        {
	  switch ( c )
	    {
	    case ':':
	    case '\\':
	    case '/':
	      *str++ = (c == ':' ? ':' : '\\');
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case '.':
	      if ( dots )
	        {
		  /* Convert path components of the form .xxx to _xxx,
		     but leave . and .. as they are.  This allows .emacs
		     to be read as _emacs, for example.  */

		  if (! *name ||
		      *name == '.' ||
		      IS_DIRECTORY_SEP (*name))
		    {
		      *str++ = '.';
		      dots--;
		    }
		  else
		    {
		      *str++ = '_';
		      left--;
		      dots = 0;
		    }
		}
	      else if ( !extn )
	        {
		  *str++ = '.';
		  extn = 1;		/* we've got an extension */
		  left = 3;		/* 3 chars in extension */
		}
	      else
	        {
		  /* any embedded dots after the first are converted to _ */
		  *str++ = '_';
		}
	      break;
	    case '~':
	    case '#':			/* don't lose these, they're important */
	      if ( ! left )
		str[-1] = c;		/* replace last character of part */
	      /* FALLTHRU */
	      FALLTHROUGH;
	    default:
	      if ( left && 'A' <= c && c <= 'Z' )
	        {
		  *str++ = tolower (c);	/* map to lower case (looks nicer) */
		  left--;
		  dots = 0;		/* started a path component */
		}
	      break;
	    }
	}
      *str = '\0';
    }
  else
    {
      strcpy (shortname, name);
      unixtodos_filename (shortname);
    }

  if (pPath)
    *pPath = shortname + (path - save_name);

  return shortname;
}

static int
is_exec (const char * name)
{
  char * p = strrchr (name, '.');
  return
    (p != NULL
     && (xstrcasecmp (p, ".exe") == 0 ||
	 xstrcasecmp (p, ".com") == 0 ||
	 xstrcasecmp (p, ".bat") == 0 ||
	 xstrcasecmp (p, ".cmd") == 0));
}

/* Emulate the Unix directory procedures opendir, closedir, and
   readdir.  We rename them to sys_* names because some versions of
   MinGW startup code call opendir and readdir to glob wildcards, and
   the code that calls them doesn't grok UTF-8 encoded file names we
   produce in dirent->d_name[].  */

struct dirent dir_static;       /* simulated directory contents */
static HANDLE dir_find_handle = INVALID_HANDLE_VALUE;
static int    dir_is_fat;
static char   dir_pathname[MAX_UTF8_PATH];
static WIN32_FIND_DATAW dir_find_data_w;
static WIN32_FIND_DATAA dir_find_data_a;
#define DIR_FIND_DATA_W 1
#define DIR_FIND_DATA_A 2
static int    last_dir_find_data = -1;

/* Support shares on a network resource as subdirectories of a read-only
   root directory. */
static HANDLE wnet_enum_handle = INVALID_HANDLE_VALUE;
static HANDLE open_unc_volume (const char *);
static void  *read_unc_volume (HANDLE, wchar_t *, char *, int);
static void   close_unc_volume (HANDLE);

DIR *
sys_opendir (const char *filename)
{
  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we defer the open until read time.  */

  if (dir_find_handle != INVALID_HANDLE_VALUE)
    return NULL;
  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    return NULL;

  /* Note: We don't support traversal of UNC volumes via symlinks.
     Doing so would mean punishing 99.99% of use cases by resolving
     all the possible symlinks in FILENAME, recursively. */
  if (is_unc_volume (filename))
    {
      wnet_enum_handle = open_unc_volume (filename);
      if (wnet_enum_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }

  if (!(dirp = (DIR *) malloc (sizeof (DIR))))
    return NULL;

  dirp->dd_fd = 0;
  dirp->dd_loc = 0;
  dirp->dd_size = 0;

  strncpy (dir_pathname, map_w32_filename (filename, NULL), MAX_UTF8_PATH - 1);
  dir_pathname[MAX_UTF8_PATH - 1] = '\0';
  /* Note: We don't support symlinks to file names on FAT volumes.
     Doing so would mean punishing 99.99% of use cases by resolving
     all the possible symlinks in FILENAME, recursively.  */
  dir_is_fat = is_fat_volume (filename, NULL);

  return dirp;
}

void
sys_closedir (DIR *dirp)
{
  /* If we have a find-handle open, close it.  */
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    {
      FindClose (dir_find_handle);
      dir_find_handle = INVALID_HANDLE_VALUE;
    }
  else if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      close_unc_volume (wnet_enum_handle);
      wnet_enum_handle = INVALID_HANDLE_VALUE;
    }
  xfree ((char *) dirp);
}

struct dirent *
sys_readdir (DIR *dirp)
{
  int downcase = !NILP (Vw32_downcase_file_names);

  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      if (!read_unc_volume (wnet_enum_handle,
                            dir_find_data_w.cFileName,
                            dir_find_data_a.cFileName,
                            MAX_PATH))
	return NULL;
    }
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  else if (dir_find_handle == INVALID_HANDLE_VALUE)
    {
      char filename[MAX_UTF8_PATH];
      int ln;
      bool last_slash = true;

      /* Note: We don't need to worry about dir_pathname being longer
	 than MAX_UTF8_PATH, as sys_opendir already took care of that
	 when it called map_w32_filename: that function will put a "?"
	 in its return value in that case, thus failing all the calls
	 below.  */
      strcpy (filename, dir_pathname);
      ln = strlen (filename);
      if (!IS_DIRECTORY_SEP (filename[ln - 1]))
	last_slash = false;

      /* Note: No need to resolve symlinks in FILENAME, because
	 FindFirst opens the directory that is the target of a
	 symlink.  */
      if (w32_unicode_filenames)
	{
	  wchar_t fnw[MAX_PATH + 2];

	  filename_to_utf16 (filename, fnw);
	  if (!last_slash)
	    wcscat (fnw, L"\\");
	  wcscat (fnw, L"*");
	  dir_find_handle = FindFirstFileW (fnw, &dir_find_data_w);
	}
      else
	{
	  char fna[MAX_PATH + 2];

	  filename_to_ansi (filename, fna);
	  if (!last_slash)
	    strcat (fna, "\\");
	  strcat (fna, "*");
	  /* If FILENAME is not representable by the current ANSI
	     codepage, we don't want FindFirstFileA to interpret the
	     '?' characters as a wildcard.  */
	  if (_mbspbrk (fna, "?"))
	    dir_find_handle = INVALID_HANDLE_VALUE;
	  else
	    dir_find_handle = FindFirstFileA (fna, &dir_find_data_a);
	}

      if (dir_find_handle == INVALID_HANDLE_VALUE)
	{
	  /* Any changes in the value of errno here should be in sync
	     with what directory_files_internal does when it calls
	     readdir.  */
	  switch (GetLastError ())
	    {
	      /* Windows uses this value when FindFirstFile finds no
		 files that match the wildcard.  This is not supposed
		 to happen, since our wildcard is "*", but just in
		 case, if there's some weird empty directory with not
		 even "." and ".." entries...  */
	    case ERROR_FILE_NOT_FOUND:
	      errno = 0;
	      /* FALLTHRU */
	    default:
	      break;
	    case ERROR_ACCESS_DENIED:
	    case ERROR_NETWORK_ACCESS_DENIED:
	      errno = EACCES;
	      break;
	    case ERROR_PATH_NOT_FOUND:
	    case ERROR_INVALID_DRIVE:
	    case ERROR_NOT_READY:
	    case ERROR_BAD_NETPATH:
	    case ERROR_BAD_NET_NAME:
	      errno = ENOENT;
	      break;
	    }
	  return NULL;
	}
    }
  else if (w32_unicode_filenames)
    {
      if (!FindNextFileW (dir_find_handle, &dir_find_data_w))
	{
	  errno = 0;
	  return NULL;
	}
    }
  else
    {
      if (!FindNextFileA (dir_find_handle, &dir_find_data_a))
	{
	  errno = 0;
	  return NULL;
	}
    }

  /* Emacs never uses this value, so don't bother making it match
     value returned by stat().  */
  dir_static.d_ino = 1;

  if (w32_unicode_filenames)
    {
      if (downcase || dir_is_fat)
	{
	  wchar_t tem[MAX_PATH];

	  wcscpy (tem, dir_find_data_w.cFileName);
	  CharLowerW (tem);
	  filename_from_utf16 (tem, dir_static.d_name);
	}
      else
	filename_from_utf16 (dir_find_data_w.cFileName, dir_static.d_name);
      last_dir_find_data = DIR_FIND_DATA_W;
    }
  else
    {
      char tem[MAX_PATH];

      /* If the file name in cFileName[] includes `?' characters, it
	 means the original file name used characters that cannot be
	 represented by the current ANSI codepage.  To avoid total
	 lossage, retrieve the short 8+3 alias of the long file
	 name.  */
      if (_mbspbrk (dir_find_data_a.cFileName, "?"))
	{
	  strcpy (tem, dir_find_data_a.cAlternateFileName);
	  /* 8+3 aliases are returned in all caps, which could break
	     various alists that look at filenames' extensions.  */
	  downcase = 1;
	}
      else if (downcase || dir_is_fat)
	strcpy (tem, dir_find_data_a.cFileName);
      else
	filename_from_ansi (dir_find_data_a.cFileName, dir_static.d_name);
      if (downcase || dir_is_fat)
	{
	  _mbslwr (tem);
	  filename_from_ansi (tem, dir_static.d_name);
	}
      last_dir_find_data = DIR_FIND_DATA_A;
    }

  dir_static.d_namlen = strlen (dir_static.d_name);
  dir_static.d_reclen = sizeof (struct dirent) - MAX_UTF8_PATH + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;

  return &dir_static;
}

static HANDLE
open_unc_volume (const char *path)
{
  const char *fn = map_w32_filename (path, NULL);
  DWORD result;
  HANDLE henum;

  if (w32_unicode_filenames)
    {
      NETRESOURCEW nrw;
      wchar_t fnw[MAX_PATH];

      nrw.dwScope = RESOURCE_GLOBALNET;
      nrw.dwType = RESOURCETYPE_DISK;
      nrw.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER;
      nrw.dwUsage = RESOURCEUSAGE_CONTAINER;
      nrw.lpLocalName = NULL;
      filename_to_utf16 (fn, fnw);
      nrw.lpRemoteName = fnw;
      nrw.lpComment = NULL;
      nrw.lpProvider = NULL;

      result = WNetOpenEnumW (RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
			      RESOURCEUSAGE_CONNECTABLE, &nrw, &henum);
    }
  else
    {
      NETRESOURCEA nra;
      char fna[MAX_PATH];

      nra.dwScope = RESOURCE_GLOBALNET;
      nra.dwType = RESOURCETYPE_DISK;
      nra.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER;
      nra.dwUsage = RESOURCEUSAGE_CONTAINER;
      nra.lpLocalName = NULL;
      filename_to_ansi (fn, fna);
      nra.lpRemoteName = fna;
      nra.lpComment = NULL;
      nra.lpProvider = NULL;

      result = WNetOpenEnumA (RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
			      RESOURCEUSAGE_CONNECTABLE, &nra, &henum);
    }
  if (result == NO_ERROR)
    return henum;
  else
    {
      /* Make sure directory_files_internal reports a sensible error.  */
      errno = ENOENT;
      return INVALID_HANDLE_VALUE;
    }
}

static void *
read_unc_volume (HANDLE henum, wchar_t *fname_w, char *fname_a, int size)
{
  DWORD count;
  int result;
  char *buffer;
  DWORD bufsize = 512;
  void *retval;

  count = 1;
  if (w32_unicode_filenames)
    {
      wchar_t *ptrw;

      bufsize *= 2;
      buffer = alloca (bufsize);
      result = WNetEnumResourceW (henum, &count, buffer, &bufsize);
      if (result != NO_ERROR)
	return NULL;
      /* WNetEnumResource returns \\resource\share...skip forward to "share". */
      ptrw = ((LPNETRESOURCEW) buffer)->lpRemoteName;
      ptrw += 2;
      while (*ptrw && *ptrw != L'/' && *ptrw != L'\\') ptrw++;
      ptrw++;
      wcsncpy (fname_w, ptrw, size);
      retval = fname_w;
    }
  else
    {
      int dbcs_p = max_filename_mbslen () > 1;
      char *ptra;

      buffer = alloca (bufsize);
      result = WNetEnumResourceA (henum, &count, buffer, &bufsize);
      if (result != NO_ERROR)
	return NULL;
      ptra = ((LPNETRESOURCEA) buffer)->lpRemoteName;
      ptra += 2;
      if (!dbcs_p)
	while (*ptra && !IS_DIRECTORY_SEP (*ptra)) ptra++;
      else
	{
	  while (*ptra && !IS_DIRECTORY_SEP (*ptra))
	    ptra = CharNextExA (file_name_codepage, ptra, 0);
	}
      ptra++;
      strncpy (fname_a, ptra, size);
      retval = fname_a;
    }

  return retval;
}

static void
close_unc_volume (HANDLE henum)
{
  if (henum != INVALID_HANDLE_VALUE)
    WNetCloseEnum (henum);
}

static DWORD
unc_volume_file_attributes (const char *path)
{
  HANDLE henum;
  DWORD attrs;

  henum = open_unc_volume (path);
  if (henum == INVALID_HANDLE_VALUE)
    return -1;

  attrs = FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_DIRECTORY;

  close_unc_volume (henum);

  return attrs;
}

/* Ensure a network connection is authenticated.  */
static void
logon_network_drive (const char *path)
{
  char share[MAX_UTF8_PATH];
  int n_slashes;
  char drive[4];
  UINT drvtype;
  char *p;
  DWORD val;

  if (IS_DIRECTORY_SEP (path[0]) && IS_DIRECTORY_SEP (path[1]))
    drvtype = DRIVE_REMOTE;
  else if (path[0] == '\0' || path[1] != ':')
    drvtype = GetDriveType (NULL);
  else
    {
      drive[0] = path[0];
      drive[1] = ':';
      drive[2] = '\\';
      drive[3] = '\0';
      drvtype = GetDriveType (drive);
    }

  /* Only logon to networked drives.  */
  if (drvtype != DRIVE_REMOTE)
    return;

  n_slashes = 2;
  strncpy (share, path, MAX_UTF8_PATH);
  /* Truncate to just server and share name.  */
  for (p = share + 2; *p && p < share + MAX_UTF8_PATH; p++)
    {
      if (IS_DIRECTORY_SEP (*p) && ++n_slashes > 3)
        {
          *p = '\0';
          break;
        }
    }

  if (w32_unicode_filenames)
    {
      NETRESOURCEW resourcew;
      wchar_t share_w[MAX_PATH];

      resourcew.dwScope = RESOURCE_GLOBALNET;
      resourcew.dwType = RESOURCETYPE_DISK;
      resourcew.dwDisplayType = RESOURCEDISPLAYTYPE_SHARE;
      resourcew.dwUsage = RESOURCEUSAGE_CONTAINER;
      resourcew.lpLocalName = NULL;
      filename_to_utf16 (share, share_w);
      resourcew.lpRemoteName = share_w;
      resourcew.lpProvider = NULL;

      val = WNetAddConnection2W (&resourcew, NULL, NULL, CONNECT_INTERACTIVE);
    }
  else
    {
      NETRESOURCEA resourcea;
      char share_a[MAX_PATH];

      resourcea.dwScope = RESOURCE_GLOBALNET;
      resourcea.dwType = RESOURCETYPE_DISK;
      resourcea.dwDisplayType = RESOURCEDISPLAYTYPE_SHARE;
      resourcea.dwUsage = RESOURCEUSAGE_CONTAINER;
      resourcea.lpLocalName = NULL;
      filename_to_ansi (share, share_a);
      resourcea.lpRemoteName = share_a;
      resourcea.lpProvider = NULL;

      val = WNetAddConnection2A (&resourcea, NULL, NULL, CONNECT_INTERACTIVE);
    }

  switch (val)
    {
    case NO_ERROR:
    case ERROR_ALREADY_ASSIGNED:
      break;
    case ERROR_ACCESS_DENIED:
    case ERROR_LOGON_FAILURE:
      errno = EACCES;
      break;
    case ERROR_BUSY:
      errno = EAGAIN;
      break;
    case ERROR_BAD_NET_NAME:
    case ERROR_NO_NET_OR_BAD_PATH:
    case ERROR_NO_NETWORK:
    case ERROR_CANCELLED:
    default:
      errno = ENOENT;
      break;
    }
}

/* Emulate faccessat(2).  */
int
faccessat (int dirfd, const char * path, int mode, int flags)
{
  DWORD attributes;
  char fullname[MAX_UTF8_PATH];

  /* Rely on a hack: an open directory is modeled as file descriptor 0,
     and its actual file name is stored in dir_pathname by opendir.
     This is good enough for the current usage in Emacs, but is fragile.  */
  if (dirfd != AT_FDCWD
      && !(IS_DIRECTORY_SEP (path[0])
	   || IS_DEVICE_SEP (path[1])))
    {
      char lastc = dir_pathname[strlen (dir_pathname) - 1];

      if (_snprintf (fullname, sizeof fullname, "%s%s%s",
		     dir_pathname, IS_DIRECTORY_SEP (lastc) ? "" : "/", path)
	  < 0)
	{
	  errno = ENAMETOOLONG;
	  return -1;
	}
      path = fullname;
    }

  /* When dired.c calls us with F_OK and a trailing slash, it actually
     wants to know whether PATH is a directory.  */
  if (IS_DIRECTORY_SEP (path[strlen (path) - 1]) && mode == F_OK)
    mode |= D_OK;

  /* MSVCRT implementation of 'access' doesn't recognize D_OK, and its
     newer versions blow up when passed D_OK.  */
  path = map_w32_filename (path, NULL);
  /* If the last element of PATH is a symlink, we need to resolve it
     to get the attributes of its target file.  Note: any symlinks in
     PATH elements other than the last one are transparently resolved
     by GetFileAttributes below.  */
  if ((volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) != 0
      && (flags & AT_SYMLINK_NOFOLLOW) == 0)
    path = chase_symlinks (path);

  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      attributes = GetFileAttributesW (path_w);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      attributes = GetFileAttributesA (path_a);
    }

  if (attributes == -1)
    {
      DWORD w32err = GetLastError ();

      switch (w32err)
	{
	case ERROR_INVALID_NAME:
	case ERROR_BAD_PATHNAME:
	  if (is_unc_volume (path))
	    {
	      attributes = unc_volume_file_attributes (path);
	      if (attributes == -1)
		{
		  errno = EACCES;
		  return -1;
		}
	      goto check_attrs;
	    }
	  /* FALLTHROUGH */
	  FALLTHROUGH;
	case ERROR_FILE_NOT_FOUND:
	case ERROR_BAD_NETPATH:
	  errno = ENOENT;
	  break;
	default:
	  errno = EACCES;
	  break;
	}
      return -1;
    }

 check_attrs:
  if ((mode & X_OK) != 0
      && !(is_exec (path) || (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0))
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & W_OK) != 0 && (attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & D_OK) != 0 && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

/* A special test for DIRNAME being a directory accessible by the
   current user.  This is needed because the security permissions in
   directory's ACLs are not visible in the Posix-style mode bits
   returned by 'stat' and in attributes returned by GetFileAttributes.
   So a directory would seem like it's readable by the current user,
   but will in fact error out with EACCES when they actually try.  */
int
w32_accessible_directory_p (const char *dirname, ptrdiff_t dirlen)
{
  char pattern[MAX_UTF8_PATH];
  bool last_slash = dirlen > 0 && IS_DIRECTORY_SEP (dirname[dirlen - 1]);
  HANDLE dh;

  /* Network volumes need a different reading method.  */
  if (is_unc_volume (dirname))
    {
      void *read_result = NULL;
      wchar_t fnw[MAX_PATH];
      char fna[MAX_PATH];

      dh = open_unc_volume (dirname);
      if (dh != INVALID_HANDLE_VALUE)
	{
	  read_result = read_unc_volume (dh, fnw, fna, MAX_PATH);
	  close_unc_volume (dh);
	}
      /* Treat empty volumes as accessible.  */
      return read_result != NULL || GetLastError () == ERROR_NO_MORE_ITEMS;
    }

  /* Note: map_w32_filename makes sure DIRNAME is not longer than
     MAX_UTF8_PATH.  */
  strcpy (pattern, map_w32_filename (dirname, NULL));

  /* Note: No need to resolve symlinks in FILENAME, because FindFirst
     opens the directory that is the target of a symlink.  */
  if (w32_unicode_filenames)
    {
      wchar_t pat_w[MAX_PATH + 2];
      WIN32_FIND_DATAW dfd_w;

      filename_to_utf16 (pattern, pat_w);
      if (!last_slash)
	wcscat (pat_w, L"\\");
      wcscat (pat_w, L"*");
      dh = FindFirstFileW (pat_w, &dfd_w);
    }
  else
    {
      char pat_a[MAX_PATH + 2];
      WIN32_FIND_DATAA dfd_a;

      filename_to_ansi (pattern, pat_a);
      if (!last_slash)
	strcpy (pat_a, "\\");
      strcat (pat_a, "*");
      /* In case DIRNAME cannot be expressed in characters from the
	 current ANSI codepage.  */
      if (_mbspbrk (pat_a, "?"))
	dh = INVALID_HANDLE_VALUE;
      else
	dh = FindFirstFileA (pat_a, &dfd_a);
    }

  if (dh == INVALID_HANDLE_VALUE)
    return 0;
  FindClose (dh);
  return 1;
}

/* A version of 'access' to be used locally with file names in
   locale-specific encoding.  Does not resolve symlinks and does not
   support file names on FAT12 and FAT16 volumes, but that's OK, since
   we only invoke this function for files inside the Emacs source or
   installation tree, on directories (so any symlinks should have the
   directory bit set), and on short file names such as "C:/.emacs".  */
static int
sys_access (const char *fname, int mode)
{
  char fname_copy[MAX_PATH], *p;
  DWORD attributes;

  strcpy (fname_copy, fname);
  /* Do the equivalent of unixtodos_filename.  */
  for (p = fname_copy; *p; p = CharNext (p))
    if (*p == '/')
      *p = '\\';

  if ((attributes = GetFileAttributesA (fname_copy)) == -1)
    {
      DWORD w32err = GetLastError ();

      switch (w32err)
	{
	case ERROR_INVALID_NAME:
	case ERROR_BAD_PATHNAME:
	case ERROR_FILE_NOT_FOUND:
	case ERROR_BAD_NETPATH:
	  errno = ENOENT;
	  break;
	default:
	  errno = EACCES;
	  break;
	}
      return -1;
    }
  if ((mode & X_OK) != 0
      && !(is_exec (fname_copy)
	   || (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0))
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & W_OK) != 0 && (attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & D_OK) != 0 && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

/* Shadow some MSVC runtime functions to map requests for long filenames
   to reasonable short names if necessary.  This was originally added to
   permit running Emacs on NT 3.1 on a FAT partition, which doesn't support
   long file names.  */

int
sys_chdir (const char * path)
{
  path = map_w32_filename (path, NULL);
  if (w32_unicode_filenames)
    {
      wchar_t newdir_w[MAX_PATH];

      if (filename_to_utf16 (path, newdir_w) == 0)
	return _wchdir (newdir_w);
      return -1;
    }
  else
    {
      char newdir_a[MAX_PATH];

      if (filename_to_ansi (path, newdir_a) == 0)
	return _chdir (newdir_a);
      return -1;
    }
}

int
sys_chmod (const char * path, int mode)
{
  path = chase_symlinks (map_w32_filename (path, NULL));
  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      return _wchmod (path_w, mode);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      return _chmod (path_a, mode);
    }
}

int
sys_creat (const char * path, int mode)
{
  path = map_w32_filename (path, NULL);
  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      return _wcreat (path_w, mode);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      return _creat (path_a, mode);
    }
}

FILE *
sys_fopen (const char * path, const char * mode)
{
  int fd;
  int oflag;
  const char * mode_save = mode;

  /* Force all file handles to be non-inheritable.  This is necessary to
     ensure child processes don't unwittingly inherit handles that might
     prevent future file access. */

  if (mode[0] == 'r')
    oflag = O_RDONLY;
  else if (mode[0] == 'w' || mode[0] == 'a')
    oflag = O_WRONLY | O_CREAT | O_TRUNC;
  else
    return NULL;

  /* Only do simplistic option parsing. */
  while (*++mode)
    if (mode[0] == '+')
      {
	oflag &= ~(O_RDONLY | O_WRONLY);
	oflag |= O_RDWR;
      }
    else if (mode[0] == 'b')
      {
	oflag &= ~O_TEXT;
	oflag |= O_BINARY;
      }
    else if (mode[0] == 't')
      {
	oflag &= ~O_BINARY;
	oflag |= O_TEXT;
      }
    else break;

  path = map_w32_filename (path, NULL);
  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      fd = _wopen (path_w, oflag | _O_NOINHERIT, 0644);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      fd = _open (path_a, oflag | _O_NOINHERIT, 0644);
    }
  if (fd < 0)
    return NULL;

  return _fdopen (fd, mode_save);
}

/* This only works on NTFS volumes, but is useful to have.  */
int
sys_link (const char * old, const char * new)
{
  HANDLE fileh;
  int   result = -1;
  char oldname[MAX_UTF8_PATH], newname[MAX_UTF8_PATH];
  wchar_t oldname_w[MAX_PATH];
  char oldname_a[MAX_PATH];

  if (old == NULL || new == NULL)
    {
      errno = ENOENT;
      return -1;
    }

  strcpy (oldname, map_w32_filename (old, NULL));
  strcpy (newname, map_w32_filename (new, NULL));

  if (w32_unicode_filenames)
    {
      filename_to_utf16 (oldname, oldname_w);
      fileh = CreateFileW (oldname_w, 0, 0, NULL, OPEN_EXISTING,
			   FILE_FLAG_BACKUP_SEMANTICS, NULL);
    }
  else
    {
      filename_to_ansi (oldname, oldname_a);
      fileh = CreateFileA (oldname_a, 0, 0, NULL, OPEN_EXISTING,
			   FILE_FLAG_BACKUP_SEMANTICS, NULL);
    }
  if (fileh != INVALID_HANDLE_VALUE)
    {
      int wlen;

      /* Confusingly, the "alternate" stream name field does not apply
         when restoring a hard link, and instead contains the actual
         stream data for the link (ie. the name of the link to create).
         The WIN32_STREAM_ID structure before the cStreamName field is
         the stream header, which is then immediately followed by the
         stream data.  */

      struct {
	WIN32_STREAM_ID wid;
	WCHAR wbuffer[MAX_PATH];	/* extra space for link name */
      } data;

      /* We used to pass MB_PRECOMPOSED as the 2nd arg here, but MSDN
	 indicates that flag is unsupported for CP_UTF8, and OTOH says
	 it is the default anyway.  */
      wlen = pMultiByteToWideChar (CP_UTF8, 0, newname, -1,
				   data.wid.cStreamName, MAX_PATH);
      if (wlen > 0)
	{
	  LPVOID context = NULL;
	  DWORD wbytes = 0;

	  data.wid.dwStreamId = BACKUP_LINK;
	  data.wid.dwStreamAttributes = 0;
	  data.wid.Size.LowPart = wlen * sizeof (WCHAR);
	  data.wid.Size.HighPart = 0;
	  data.wid.dwStreamNameSize = 0;

	  if (BackupWrite (fileh, (LPBYTE)&data,
			   offsetof (WIN32_STREAM_ID, cStreamName)
			   + data.wid.Size.LowPart,
			   &wbytes, FALSE, FALSE, &context)
	      && BackupWrite (fileh, NULL, 0, &wbytes, TRUE, FALSE, &context))
	    {
	      /* succeeded */
	      result = 0;
	    }
	  else
	    {
	      DWORD err = GetLastError ();
	      DWORD attributes;

	      switch (err)
		{
		case ERROR_ACCESS_DENIED:
		  /* This is what happens when OLDNAME is a directory,
		     since Windows doesn't support hard links to
		     directories.  Posix says to set errno to EPERM in
		     that case.  */
		  if (w32_unicode_filenames)
		    attributes = GetFileAttributesW (oldname_w);
		  else
		    attributes = GetFileAttributesA (oldname_a);
		  if (attributes != -1
		      && (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
		    errno = EPERM;
		  else if (attributes == -1
			   && is_unc_volume (oldname)
			   && unc_volume_file_attributes (oldname) != -1)
		    errno = EPERM;
		  else
		    errno = EACCES;
		  break;
		case ERROR_TOO_MANY_LINKS:
		  errno = EMLINK;
		  break;
		case ERROR_NOT_SAME_DEVICE:
		  errno = EXDEV;
		  break;
		default:
		  errno = EINVAL;
		  break;
		}
	    }
	}

      CloseHandle (fileh);
    }
  else
    errno = ENOENT;

  return result;
}

int
sys_mkdir (const char * path, mode_t mode)
{
  path = map_w32_filename (path, NULL);

  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      return _wmkdir (path_w);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      return _mkdir (path_a);
    }
}

int
sys_open (const char * path, int oflag, int mode)
{
  const char* mpath = map_w32_filename (path, NULL);
  int res = -1;

  if (w32_unicode_filenames)
    {
      wchar_t mpath_w[MAX_PATH];

      filename_to_utf16 (mpath, mpath_w);
      /* If possible, try to open file without _O_CREAT, to be able to
	 write to existing hidden and system files.  Force all file
	 handles to be non-inheritable. */
      if ((oflag & (_O_CREAT | _O_EXCL)) != (_O_CREAT | _O_EXCL))
	res = _wopen (mpath_w, (oflag & ~_O_CREAT) | _O_NOINHERIT, mode);
      if (res < 0)
	res = _wopen (mpath_w, oflag | _O_NOINHERIT, mode);
    }
  else
    {
      char mpath_a[MAX_PATH];

      filename_to_ansi (mpath, mpath_a);
      if ((oflag & (_O_CREAT | _O_EXCL)) != (_O_CREAT | _O_EXCL))
	res = _open (mpath_a, (oflag & ~_O_CREAT) | _O_NOINHERIT, mode);
      if (res < 0)
	res = _open (mpath_a, oflag | _O_NOINHERIT, mode);
    }

  return res;
}

int
fchmod (int fd, mode_t mode)
{
  return 0;
}

int
sys_rename_replace (const char *oldname, const char *newname, BOOL force)
{
  BOOL result;
  char temp[MAX_UTF8_PATH], temp_a[MAX_PATH];;
  int newname_dev;
  int oldname_dev;
  bool have_temp_a = false;

  /* MoveFile on Windows 95 doesn't correctly change the short file name
     alias in a number of circumstances (it is not easy to predict when
     just by looking at oldname and newname, unfortunately).  In these
     cases, renaming through a temporary name avoids the problem.

     A second problem on Windows 95 is that renaming through a temp name when
     newname is uppercase fails (the final long name ends up in
     lowercase, although the short alias might be uppercase) UNLESS the
     long temp name is not 8.3.

     So, on Windows 95 we always rename through a temp name, and we make sure
     the temp name has a long extension to ensure correct renaming.  */

  strcpy (temp, map_w32_filename (oldname, NULL));

  /* volume_info is set indirectly by map_w32_filename.  */
  oldname_dev = volume_info.serialnum;

  if (os_subtype == OS_9X)
    {
      char * o;
      char * p;
      int    i = 0;
      char oldname_a[MAX_PATH];

      oldname = map_w32_filename (oldname, NULL);
      filename_to_ansi (oldname, oldname_a);
      filename_to_ansi (temp, temp_a);
      if ((o = strrchr (oldname_a, '\\')))
	o++;
      else
	o = (char *) oldname_a;

      if ((p = strrchr (temp_a, '\\')))
	p++;
      else
	p = temp_a;

      do
	{
	  /* Force temp name to require a manufactured 8.3 alias - this
	     seems to make the second rename work properly.  */
	  sprintf (p, "_.%s.%d", o, i);
	  i++;
	  result = rename (oldname_a, temp_a);
	}
      /* This loop must surely terminate!  */
      while (result < 0 && errno == EEXIST);
      if (result < 0)
	return -1;
      have_temp_a = true;
    }

  /* If FORCE, emulate Unix behavior - newname is deleted if it already exists
     (at least if it is a file; don't do this for directories).

     Since we mustn't do this if we are just changing the case of the
     file name (we would end up deleting the file we are trying to
     rename!), we let rename detect if the destination file already
     exists - that way we avoid the possible pitfalls of trying to
     determine ourselves whether two names really refer to the same
     file, which is not always possible in the general case.  (Consider
     all the permutations of shared or subst'd drives, etc.)  */

  newname = map_w32_filename (newname, NULL);

  /* volume_info is set indirectly by map_w32_filename.  */
  newname_dev = volume_info.serialnum;

  if (w32_unicode_filenames)
    {
      wchar_t temp_w[MAX_PATH], newname_w[MAX_PATH];

      filename_to_utf16 (temp, temp_w);
      filename_to_utf16 (newname, newname_w);
      result = _wrename (temp_w, newname_w);
      if (result < 0)
	{
	  DWORD w32err = GetLastError ();

	  if (errno == EACCES
	      && newname_dev != oldname_dev)
	    {
	      DWORD attributes;
	      /* The implementation of `rename' on Windows does not return
		 errno = EXDEV when you are moving a directory to a
		 different storage device (ex. logical disk).  It returns
		 EACCES instead.  So here we handle such situations and
		 return EXDEV.  */
	      if ((attributes = GetFileAttributesW (temp_w)) != -1
		  && (attributes & FILE_ATTRIBUTE_DIRECTORY))
		errno = EXDEV;
	    }
	  else if (errno == EEXIST && force)
	    {
	      DWORD attributes_old;
	      DWORD attributes_new;

	      if (_wchmod (newname_w, 0666) != 0)
		return result;
	      attributes_old = GetFileAttributesW (temp_w);
	      attributes_new = GetFileAttributesW (newname_w);
	      if (attributes_old != -1 && attributes_new != -1
		  && ((attributes_old & FILE_ATTRIBUTE_DIRECTORY)
		      != (attributes_new & FILE_ATTRIBUTE_DIRECTORY)))
		{
		  if ((attributes_old & FILE_ATTRIBUTE_DIRECTORY) != 0)
		    errno = ENOTDIR;
		  else
		    errno = EISDIR;
		  return -1;
		}
	      if ((attributes_new & FILE_ATTRIBUTE_DIRECTORY) != 0)
		{
		  if (_wrmdir (newname_w) != 0)
		    return result;
		}
	      else if (_wunlink (newname_w) != 0)
		return result;
	      result = _wrename (temp_w, newname_w);
	    }
	  else if (w32err == ERROR_PRIVILEGE_NOT_HELD
		   && is_symlink (temp))
	    {
	      /* This is Windows prohibiting the user from creating a
		 symlink in another place, since that requires
		 privileges.  */
	      errno = EPERM;
	    }
	}
    }
  else
    {
      char newname_a[MAX_PATH];

      if (!have_temp_a)
	filename_to_ansi (temp, temp_a);
      filename_to_ansi (newname, newname_a);
      result = rename (temp_a, newname_a);
      if (result < 0)
	{
	  DWORD w32err = GetLastError ();

	  if (errno == EACCES
	      && newname_dev != oldname_dev)
	    {
	      DWORD attributes;
	      if ((attributes = GetFileAttributesA (temp_a)) != -1
		  && (attributes & FILE_ATTRIBUTE_DIRECTORY))
		errno = EXDEV;
	    }
	  else if (errno == EEXIST && force)
	    {
	      DWORD attributes_old;
	      DWORD attributes_new;

	      if (_chmod (newname_a, 0666) != 0)
		return result;
	      attributes_old = GetFileAttributesA (temp_a);
	      attributes_new = GetFileAttributesA (newname_a);
	      if (attributes_old != -1 && attributes_new != -1
		  && ((attributes_old & FILE_ATTRIBUTE_DIRECTORY)
		      != (attributes_new & FILE_ATTRIBUTE_DIRECTORY)))
		{
		  if ((attributes_old & FILE_ATTRIBUTE_DIRECTORY) != 0)
		    errno = ENOTDIR;
		  else
		    errno = EISDIR;
		  return -1;
		}
	      if ((attributes_new & FILE_ATTRIBUTE_DIRECTORY) != 0)
		{
		  if (_rmdir (newname_a) != 0)
		    return result;
		}
	      else if (_unlink (newname_a) != 0)
		return result;
	      result = rename (temp_a, newname_a);
	    }
	  else if (w32err == ERROR_PRIVILEGE_NOT_HELD
		   && is_symlink (temp))
	    errno = EPERM;
	}
    }

  return result;
}

int
sys_rename (char const *old, char const *new)
{
  return sys_rename_replace (old, new, TRUE);
}

int
sys_rmdir (const char * path)
{
  path = map_w32_filename (path, NULL);

  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      return _wrmdir (path_w);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      return _rmdir (path_a);
    }
}

int
sys_unlink (const char * path)
{
  int rmstatus, e;

  path = map_w32_filename (path, NULL);

  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      /* On Unix, unlink works without write permission.  */
      _wchmod (path_w, 0666);
      rmstatus = _wunlink (path_w);
      e = errno;
      /* Symlinks to directories can only be deleted by _rmdir;
	 _unlink returns EACCES.  */
      if (rmstatus != 0
	  && errno == EACCES
	  && (is_symlink (path) & FILE_ATTRIBUTE_DIRECTORY) != 0)
	rmstatus = _wrmdir (path_w);
      else
	errno = e;
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      _chmod (path_a, 0666);
      rmstatus = _unlink (path_a);
      e = errno;
      if (rmstatus != 0
	  && errno == EACCES
	  && (is_symlink (path) & FILE_ATTRIBUTE_DIRECTORY) != 0)
	rmstatus = _rmdir (path_a);
      else
	errno = e;
    }

  return rmstatus;
}

static FILETIME utc_base_ft;
static ULONGLONG utc_base;  /* In 100ns units */
static int init = 0;

#define FILETIME_TO_U64(result, ft)        \
  do {                                     \
    ULARGE_INTEGER uiTemp;                 \
    uiTemp.LowPart = (ft).dwLowDateTime;   \
    uiTemp.HighPart = (ft).dwHighDateTime; \
    result = uiTemp.QuadPart;              \
  } while (0)

static void
initialize_utc_base (void)
{
  /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
  SYSTEMTIME st;

  st.wYear = 1970;
  st.wMonth = 1;
  st.wDay = 1;
  st.wHour = 0;
  st.wMinute = 0;
  st.wSecond = 0;
  st.wMilliseconds = 0;

  SystemTimeToFileTime (&st, &utc_base_ft);
  FILETIME_TO_U64 (utc_base, utc_base_ft);
}

static time_t
convert_time (FILETIME ft)
{
  ULONGLONG tmp;

  if (!init)
    {
      initialize_utc_base ();
      init = 1;
    }

  if (CompareFileTime (&ft, &utc_base_ft) < 0)
    return 0;

  FILETIME_TO_U64 (tmp, ft);
  return (time_t) ((tmp - utc_base) / 10000000L);
}

static void
convert_from_time_t (time_t time, FILETIME * pft)
{
  ULARGE_INTEGER tmp;

  if (!init)
    {
      initialize_utc_base ();
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp.QuadPart = (ULONGLONG) time * 10000000L + utc_base;
  pft->dwHighDateTime = tmp.HighPart;
  pft->dwLowDateTime = tmp.LowPart;
}

static PSECURITY_DESCRIPTOR
get_file_security_desc_by_handle (HANDLE h)
{
  PSECURITY_DESCRIPTOR psd = NULL;
  DWORD err;
  SECURITY_INFORMATION si = OWNER_SECURITY_INFORMATION
    | GROUP_SECURITY_INFORMATION  /* | DACL_SECURITY_INFORMATION */ ;

  err = get_security_info (h, SE_FILE_OBJECT, si,
			   NULL, NULL, NULL, NULL, &psd);
  if (err != ERROR_SUCCESS)
    return NULL;

  return psd;
}

static PSECURITY_DESCRIPTOR
get_file_security_desc_by_name (const char *fname)
{
  PSECURITY_DESCRIPTOR psd = NULL;
  DWORD sd_len, err;
  SECURITY_INFORMATION si = OWNER_SECURITY_INFORMATION
    | GROUP_SECURITY_INFORMATION  /* | DACL_SECURITY_INFORMATION */ ;

  if (!get_file_security (fname, si, psd, 0, &sd_len))
    {
      err = GetLastError ();
      if (err != ERROR_INSUFFICIENT_BUFFER)
	return NULL;
    }

  psd = xmalloc (sd_len);
  if (!get_file_security (fname, si, psd, sd_len, &sd_len))
    {
      xfree (psd);
      return NULL;
    }

  return psd;
}

static DWORD
get_rid (PSID sid)
{
  unsigned n_subauthorities;

  /* Use the last sub-authority value of the RID, the relative
     portion of the SID, as user/group ID. */
  n_subauthorities = *get_sid_sub_authority_count (sid);
  if (n_subauthorities < 1)
    return 0;	/* the "World" RID */
  return *get_sid_sub_authority (sid, n_subauthorities - 1);
}

/* Caching SID and account values for faster lokup.  */

struct w32_id {
  unsigned rid;
  struct w32_id *next;
  char name[GNLEN+1];
  unsigned char sid[FLEXIBLE_ARRAY_MEMBER];
};

static struct w32_id *w32_idlist;

static int
w32_cached_id (PSID sid, unsigned *id, char *name)
{
  struct w32_id *tail, *found;

  for (found = NULL, tail = w32_idlist; tail; tail = tail->next)
    {
      if (equal_sid ((PSID)tail->sid, sid))
	{
	  found = tail;
	  break;
	}
    }
  if (found)
    {
      *id = found->rid;
      strcpy (name, found->name);
      return 1;
    }
  else
    return 0;
}

static void
w32_add_to_cache (PSID sid, unsigned id, char *name)
{
  DWORD sid_len;
  struct w32_id *new_entry;

  /* We don't want to leave behind stale cache from when Emacs was
     dumped.  */
  if (initialized)
    {
      sid_len = get_length_sid (sid);
      new_entry = xmalloc (offsetof (struct w32_id, sid) + sid_len);
      if (new_entry)
	{
	  new_entry->rid = id;
	  strcpy (new_entry->name, name);
	  copy_sid (sid_len, (PSID)new_entry->sid, sid);
	  new_entry->next = w32_idlist;
	  w32_idlist = new_entry;
	}
    }
}

#define UID 1
#define GID 2

static int
get_name_and_id (PSECURITY_DESCRIPTOR psd, unsigned *id, char *nm, int what)
{
  PSID sid = NULL;
  BOOL dflt;
  SID_NAME_USE ignore;
  char name[UNLEN+1];
  DWORD name_len = sizeof (name);
  char domain[1024];
  DWORD domain_len = sizeof (domain);
  int use_dflt = 0;
  int result;

  if (what == UID)
    result = get_security_descriptor_owner (psd, &sid, &dflt);
  else if (what == GID)
    result = get_security_descriptor_group (psd, &sid, &dflt);
  else
    result = 0;

  if (!result || !is_valid_sid (sid))
    use_dflt = 1;
  else if (!w32_cached_id (sid, id, nm))
    {
      if (!lookup_account_sid (NULL, sid, name, &name_len,
			       domain, &domain_len, &ignore)
	  || name_len > UNLEN+1)
	use_dflt = 1;
      else
	{
	  *id = get_rid (sid);
	  strcpy (nm, name);
	  w32_add_to_cache (sid, *id, name);
	}
    }
  return use_dflt;
}

static void
get_file_owner_and_group (PSECURITY_DESCRIPTOR psd, struct stat *st)
{
  int dflt_usr = 0, dflt_grp = 0;

  if (!psd)
    {
      dflt_usr = 1;
      dflt_grp = 1;
    }
  else
    {
      if (get_name_and_id (psd, &st->st_uid, st->st_uname, UID))
	dflt_usr = 1;
      if (get_name_and_id (psd, &st->st_gid, st->st_gname, GID))
	dflt_grp = 1;
    }
  /* Consider files to belong to current user/group, if we cannot get
     more accurate information.  */
  if (dflt_usr)
    {
      st->st_uid = dflt_passwd.pw_uid;
      strcpy (st->st_uname, dflt_passwd.pw_name);
    }
  if (dflt_grp)
    {
      st->st_gid = dflt_passwd.pw_gid;
      strcpy (st->st_gname, dflt_group.gr_name);
    }
}

/* Return non-zero if NAME is a potentially slow filesystem.  */
int is_slow_fs (const char *);

int
is_slow_fs (const char *name)
{
  char drive_root[4];
  UINT devtype;

  if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    devtype = DRIVE_REMOTE;	   /* assume UNC name is remote */
  else if (!(strlen (name) >= 2 && IS_DEVICE_SEP (name[1])))
    devtype = GetDriveType (NULL); /* use root of current drive */
  else
    {
      /* GetDriveType needs the root directory of the drive.  */
      strncpy (drive_root, name, 2);
      drive_root[2] = '\\';
      drive_root[3] = '\0';
      devtype = GetDriveType (drive_root);
    }
  return !(devtype == DRIVE_FIXED || devtype == DRIVE_RAMDISK);
}

/* If this is non-zero, the caller wants accurate information about
   file's owner and group, which could be expensive to get.  dired.c
   uses this flag when needed for the job at hand.  */
int w32_stat_get_owner_group;

/* MSVC stat function can't cope with UNC names and has other bugs, so
   replace it with our own.  This also allows us to calculate consistent
   inode values and owner/group without hacks in the main Emacs code,
   and support file names encoded in UTF-8. */

static int
stat_worker (const char * path, struct stat * buf, int follow_symlinks)
{
  char *name, *save_name, *r;
  WIN32_FIND_DATAW wfd_w;
  WIN32_FIND_DATAA wfd_a;
  HANDLE fh;
  unsigned __int64 fake_inode = 0;
  int permission;
  int len;
  int rootdir = FALSE;
  PSECURITY_DESCRIPTOR psd = NULL;
  int is_a_symlink = 0;
  DWORD file_flags = FILE_FLAG_BACKUP_SEMANTICS;
  DWORD access_rights = 0;
  DWORD fattrs = 0, serialnum = 0, fs_high = 0, fs_low = 0, nlinks = 1;
  FILETIME ctime, atime, wtime;
  wchar_t name_w[MAX_PATH];
  char name_a[MAX_PATH];

  if (path == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  save_name = name = (char *) map_w32_filename (path, &path);
  /* Must be valid filename, no wild cards or other invalid
     characters.  */
  if (strpbrk (name, "*?|<>\""))
    {
      errno = ENOENT;
      return -1;
    }

  len = strlen (name);
  /* Allocate 1 extra byte so that we could append a slash to a root
     directory, down below.  */
  name = strcpy (alloca (len + 2), name);

  /* Avoid a somewhat costly call to is_symlink if the filesystem
     doesn't support symlinks.  */
  if ((volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) != 0)
    is_a_symlink = is_symlink (name);

  /* Plan A: Open the file and get all the necessary information via
     the resulting handle.  This solves several issues in one blow:

      . retrieves attributes for the target of a symlink, if needed
      . gets attributes of root directories and symlinks pointing to
        root directories, thus avoiding the need for special-casing
        these and detecting them by examining the file-name format
      . retrieves more accurate attributes (e.g., non-zero size for
        some directories, esp. directories that are junction points)
      . correctly resolves "c:/..", "/.." and similar file names
      . avoids run-time penalties for 99% of use cases

     Plan A is always tried first, unless the user asked not to (but
     if the file is a symlink and we need to follow links, we try Plan
     A even if the user asked not to).

     If Plan A fails, we go to Plan B (below), where various
     potentially expensive techniques must be used to handle "special"
     files such as UNC volumes etc.  */
  if (!(NILP (Vw32_get_true_file_attributes)
	|| (EQ (Vw32_get_true_file_attributes, Qlocal) && is_slow_fs (name)))
      /* Following symlinks requires getting the info by handle.  */
      || (is_a_symlink && follow_symlinks))
    {
      BY_HANDLE_FILE_INFORMATION info;

      if (is_a_symlink && !follow_symlinks)
	file_flags |= FILE_FLAG_OPEN_REPARSE_POINT;
      /* READ_CONTROL access rights are required to get security info
	 by handle.  But if the OS doesn't support security in the
	 first place, we don't need to try.  */
      if (is_windows_9x () != TRUE)
	access_rights |= READ_CONTROL;

      if (w32_unicode_filenames)
	{
	  filename_to_utf16 (name, name_w);
	  fh = CreateFileW (name_w, access_rights, 0, NULL, OPEN_EXISTING,
			   file_flags, NULL);
	  /* If CreateFile fails with READ_CONTROL, try again with
	     zero as access rights.  */
	  if (fh == INVALID_HANDLE_VALUE && access_rights)
	    fh = CreateFileW (name_w, 0, 0, NULL, OPEN_EXISTING,
			     file_flags, NULL);
	}
      else
	{
	  filename_to_ansi (name, name_a);
	  fh = CreateFileA (name_a, access_rights, 0, NULL, OPEN_EXISTING,
			   file_flags, NULL);
	  if (fh == INVALID_HANDLE_VALUE && access_rights)
	    fh = CreateFileA (name_a, 0, 0, NULL, OPEN_EXISTING,
			     file_flags, NULL);
	}
      if (fh == INVALID_HANDLE_VALUE)
	goto no_true_file_attributes;

      /* This is more accurate in terms of getting the correct number
	 of links, but is quite slow (it is noticeable when Emacs is
	 making a list of file name completions). */
      if (GetFileInformationByHandle (fh, &info))
	{
	  nlinks = info.nNumberOfLinks;
	  /* Might as well use file index to fake inode values, but this
	     is not guaranteed to be unique unless we keep a handle open
	     all the time (even then there are situations where it is
	     not unique).  Reputedly, there are at most 48 bits of info
	     (on NTFS, presumably less on FAT). */
	  fake_inode = info.nFileIndexHigh;
	  fake_inode <<= 32;
	  fake_inode += info.nFileIndexLow;
	  serialnum = info.dwVolumeSerialNumber;
	  fs_high = info.nFileSizeHigh;
	  fs_low  = info.nFileSizeLow;
	  ctime = info.ftCreationTime;
	  atime = info.ftLastAccessTime;
	  wtime = info.ftLastWriteTime;
	  fattrs = info.dwFileAttributes;
	}
      else
	{
	  /* We don't go to Plan B here, because it's not clear that
	     it's a good idea.  The only known use case where
	     CreateFile succeeds, but GetFileInformationByHandle fails
	     (with ERROR_INVALID_FUNCTION) is for character devices
	     such as NUL, PRN, etc.  For these, switching to Plan B is
	     a net loss, because we lose the character device
	     attribute returned by GetFileType below (FindFirstFile
	     doesn't set that bit in the attributes), and the other
	     fields don't make sense for character devices anyway.
	     Emacs doesn't really care for non-file entities in the
	     context of l?stat, so neither do we.  */

	  /* w32err is assigned so one could put a breakpoint here and
	     examine its value, when GetFileInformationByHandle
	     fails. */
	  DWORD w32err = GetLastError ();

	  switch (w32err)
	    {
	    case ERROR_FILE_NOT_FOUND: /* can this ever happen? */
	      errno = ENOENT;
	      return -1;
	    }
	}

      /* Test for a symlink before testing for a directory, since
	 symlinks to directories have the directory bit set, but we
	 don't want them to appear as directories.  */
      if (is_a_symlink && !follow_symlinks)
	buf->st_mode = S_IFLNK;
      else if (fattrs & FILE_ATTRIBUTE_DIRECTORY)
	buf->st_mode = S_IFDIR;
      else
	{
	  DWORD ftype = GetFileType (fh);

	  switch (ftype)
	    {
	    case FILE_TYPE_DISK:
	      buf->st_mode = S_IFREG;
	      break;
	    case FILE_TYPE_PIPE:
	      buf->st_mode = S_IFIFO;
	      break;
	    case FILE_TYPE_CHAR:
	    case FILE_TYPE_UNKNOWN:
	    default:
	      buf->st_mode = S_IFCHR;
	    }
	}
      /* We produce the fallback owner and group data, based on the
	 current user that runs Emacs, in the following cases:

	  . caller didn't request owner and group info
	  . this is Windows 9X
	  . getting security by handle failed, and we need to produce
	    information for the target of a symlink (this is better
	    than producing a potentially misleading info about the
	    symlink itself)

	 If getting security by handle fails, and we don't need to
	 resolve symlinks, we try getting security by name.  */
      if (!w32_stat_get_owner_group || is_windows_9x () == TRUE)
	get_file_owner_and_group (NULL, buf);
      else
	{
	  psd = get_file_security_desc_by_handle (fh);
	  if (psd)
	    {
	      get_file_owner_and_group (psd, buf);
	      LocalFree (psd);
	    }
	  else if (!(is_a_symlink && follow_symlinks))
	    {
	      psd = get_file_security_desc_by_name (name);
	      get_file_owner_and_group (psd, buf);
	      xfree (psd);
	    }
	  else
	    get_file_owner_and_group (NULL, buf);
	}
      CloseHandle (fh);
    }
  else
    {
    no_true_file_attributes:
      /* Plan B: Either getting a handle on the file failed, or the
	 caller explicitly asked us to not bother making this
	 information more accurate.

	 Implementation note: In Plan B, we never bother to resolve
	 symlinks, even if we got here because we tried Plan A and
	 failed.  That's because, even if the caller asked for extra
	 precision by setting Vw32_get_true_file_attributes to t,
	 resolving symlinks requires acquiring a file handle to the
	 symlink, which we already know will fail.  And if the user
	 did not ask for extra precision, resolving symlinks will fly
	 in the face of that request, since the user then wants the
	 lightweight version of the code.  */
      rootdir = (path >= save_name + len - 1
		 && (IS_DIRECTORY_SEP (*path) || *path == 0));

      /* If name is "c:/.." or "/.." then stat "c:/" or "/".  */
      r = IS_DEVICE_SEP (name[1]) ? &name[2] : name;
      if (IS_DIRECTORY_SEP (r[0])
	  && r[1] == '.' && r[2] == '.' && r[3] == '\0')
	r[1] = r[2] = '\0';

      /* Note: If NAME is a symlink to the root of a UNC volume
	 (i.e. "\\SERVER"), we will not detect that here, and we will
	 return data about the symlink as result of FindFirst below.
	 This is unfortunate, but that marginal use case does not
	 justify a call to chase_symlinks which would impose a penalty
	 on all the other use cases.  (We get here for symlinks to
	 roots of UNC volumes because CreateFile above fails for them,
	 unlike with symlinks to root directories X:\ of drives.)  */
      if (is_unc_volume (name))
	{
	  fattrs = unc_volume_file_attributes (name);
	  if (fattrs == -1)
	    return -1;

	  ctime = atime = wtime = utc_base_ft;
	}
      else if (rootdir)
	{
	  /* Make sure root directories end in a slash.  */
	  if (!IS_DIRECTORY_SEP (name[len-1]))
	    strcpy (name + len, "\\");
	  if (GetDriveType (name) < 2)
	    {
	      errno = ENOENT;
	      return -1;
	    }

	  fattrs = FILE_ATTRIBUTE_DIRECTORY;
	  ctime = atime = wtime = utc_base_ft;
	}
      else
	{
	  int have_wfd = -1;

	  /* Make sure non-root directories do NOT end in a slash,
	     otherwise FindFirstFile might fail.  */
	  if (IS_DIRECTORY_SEP (name[len-1]))
	    name[len - 1] = 0;

	  /* (This is hacky, but helps when doing file completions on
	     network drives.)  Optimize by using information available from
	     active readdir if possible.  */
	  len = strlen (dir_pathname);
	  if (IS_DIRECTORY_SEP (dir_pathname[len-1]))
	    len--;
	  if (dir_find_handle != INVALID_HANDLE_VALUE
	      && last_dir_find_data != -1
	      && !(is_a_symlink && follow_symlinks)
	      /* The 2 file-name comparisons below support only ASCII
		 characters, and will lose (compare not equal) when
		 the file names include non-ASCII characters that are
		 the same but for the case.  However, doing this
		 properly involves: (a) converting both file names to
		 UTF-16, (b) lower-casing both names using CharLowerW,
		 and (c) comparing the results; this would be quite a
		 bit slower, whereas Plan B is for users who want
		 lightweight albeit inaccurate version of 'stat'.  */
	      && c_strncasecmp (save_name, dir_pathname, len) == 0
	      && IS_DIRECTORY_SEP (name[len])
	      && xstrcasecmp (name + len + 1, dir_static.d_name) == 0)
	    {
	      have_wfd = last_dir_find_data;
	      /* This was the last entry returned by readdir.  */
	      if (last_dir_find_data == DIR_FIND_DATA_W)
		wfd_w = dir_find_data_w;
	      else
		wfd_a = dir_find_data_a;
	    }
	  else
	    {
	      logon_network_drive (name);

	      if (w32_unicode_filenames)
		{
		  filename_to_utf16 (name, name_w);
		  fh = FindFirstFileW (name_w, &wfd_w);
		  have_wfd = DIR_FIND_DATA_W;
		}
	      else
		{
		  filename_to_ansi (name, name_a);
		  /* If NAME includes characters not representable by
		     the current ANSI codepage, filename_to_ansi
		     usually replaces them with a '?'.  We don't want
		     to let FindFirstFileA interpret those as wildcards,
		     and "succeed", returning us data from some random
		     file in the same directory.  */
		  if (_mbspbrk (name_a, "?"))
		    fh = INVALID_HANDLE_VALUE;
		  else
		    fh = FindFirstFileA (name_a, &wfd_a);
		  have_wfd = DIR_FIND_DATA_A;
		}
	      if (fh == INVALID_HANDLE_VALUE)
		{
		  errno = ENOENT;
		  return -1;
		}
	      FindClose (fh);
	    }
	  /* Note: if NAME is a symlink, the information we get from
	     FindFirstFile is for the symlink, not its target.  */
	  if (have_wfd == DIR_FIND_DATA_W)
	    {
	      fattrs = wfd_w.dwFileAttributes;
	      ctime = wfd_w.ftCreationTime;
	      atime = wfd_w.ftLastAccessTime;
	      wtime = wfd_w.ftLastWriteTime;
	      fs_high = wfd_w.nFileSizeHigh;
	      fs_low = wfd_w.nFileSizeLow;
	    }
	  else
	    {
	      fattrs = wfd_a.dwFileAttributes;
	      ctime = wfd_a.ftCreationTime;
	      atime = wfd_a.ftLastAccessTime;
	      wtime = wfd_a.ftLastWriteTime;
	      fs_high = wfd_a.nFileSizeHigh;
	      fs_low = wfd_a.nFileSizeLow;
	    }
	  fake_inode = 0;
	  nlinks = 1;
	  serialnum = volume_info.serialnum;
	}
      if (is_a_symlink && !follow_symlinks)
	buf->st_mode = S_IFLNK;
      else if (fattrs & FILE_ATTRIBUTE_DIRECTORY)
	buf->st_mode = S_IFDIR;
      else
	buf->st_mode = S_IFREG;

      get_file_owner_and_group (NULL, buf);
    }

  buf->st_ino = fake_inode;

  buf->st_dev = serialnum;
  buf->st_rdev = serialnum;

  buf->st_size = fs_high;
  buf->st_size <<= 32;
  buf->st_size += fs_low;
  buf->st_nlink = nlinks;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (wtime);
  buf->st_atime = convert_time (atime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (ctime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (is_a_symlink && !follow_symlinks)
    permission = S_IREAD | S_IWRITE | S_IEXEC; /* Posix expectations */
  else
    {
      if (fattrs & FILE_ATTRIBUTE_READONLY)
	permission = S_IREAD;
      else
	permission = S_IREAD | S_IWRITE;

      if (fattrs & FILE_ATTRIBUTE_DIRECTORY)
	permission |= S_IEXEC;
      else if (is_exec (name))
	permission |= S_IEXEC;
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

int
stat (const char * path, struct stat * buf)
{
  return stat_worker (path, buf, 1);
}

int
lstat (const char * path, struct stat * buf)
{
  return stat_worker (path, buf, 0);
}

int
fstatat (int fd, char const *name, struct stat *st, int flags)
{
  /* Rely on a hack: an open directory is modeled as file descriptor 0.
     This is good enough for the current usage in Emacs, but is fragile.

     FIXME: Add proper support for fdopendir, fstatat, readlinkat.
     Gnulib does this and can serve as a model.  */
  char fullname[MAX_UTF8_PATH];

  if (fd != AT_FDCWD)
    {
      char lastc = dir_pathname[strlen (dir_pathname) - 1];

      if (_snprintf (fullname, sizeof fullname, "%s%s%s",
		     dir_pathname, IS_DIRECTORY_SEP (lastc) ? "" : "/", name)
	  < 0)
	{
	  errno = ENAMETOOLONG;
	  return -1;
	}
      name = fullname;
    }

  return stat_worker (name, st, ! (flags & AT_SYMLINK_NOFOLLOW));
}

/* Provide fstat and utime as well as stat for consistent handling of
   file timestamps. */
int
fstat (int desc, struct stat * buf)
{
  HANDLE fh = (HANDLE) _get_osfhandle (desc);
  BY_HANDLE_FILE_INFORMATION info;
  unsigned __int64 fake_inode;
  int permission;

  switch (GetFileType (fh) & ~FILE_TYPE_REMOTE)
    {
    case FILE_TYPE_DISK:
      buf->st_mode = S_IFREG;
      if (!GetFileInformationByHandle (fh, &info))
	{
	  errno = EACCES;
	  return -1;
	}
      break;
    case FILE_TYPE_PIPE:
      buf->st_mode = S_IFIFO;
      goto non_disk;
    case FILE_TYPE_CHAR:
    case FILE_TYPE_UNKNOWN:
    default:
      buf->st_mode = S_IFCHR;
    non_disk:
      memset (&info, 0, sizeof (info));
      info.dwFileAttributes = 0;
      info.ftCreationTime = utc_base_ft;
      info.ftLastAccessTime = utc_base_ft;
      info.ftLastWriteTime = utc_base_ft;
    }

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      buf->st_mode = S_IFDIR;

  buf->st_nlink = info.nNumberOfLinks;
  /* Might as well use file index to fake inode values, but this
     is not guaranteed to be unique unless we keep a handle open
     all the time (even then there are situations where it is
     not unique).  Reputedly, there are at most 48 bits of info
     (on NTFS, presumably less on FAT). */
  fake_inode = info.nFileIndexHigh;
  fake_inode <<= 32;
  fake_inode += info.nFileIndexLow;

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = fake_inode ^ (fake_inode >> 16);
  else
    buf->st_ino = fake_inode;

  /* If the caller so requested, get the true file owner and group.
     Otherwise, consider the file to belong to the current user.  */
  if (!w32_stat_get_owner_group || is_windows_9x () == TRUE)
    get_file_owner_and_group (NULL, buf);
  else
    {
      PSECURITY_DESCRIPTOR psd = NULL;

      psd = get_file_security_desc_by_handle (fh);
      if (psd)
	{
	  get_file_owner_and_group (psd, buf);
	  LocalFree (psd);
	}
      else
	get_file_owner_and_group (NULL, buf);
    }

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_rdev = info.dwVolumeSerialNumber;

  buf->st_size = info.nFileSizeHigh;
  buf->st_size <<= 32;
  buf->st_size += info.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (info.ftLastWriteTime);
  buf->st_atime = convert_time (info.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (info.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = S_IREAD;
  else
    permission = S_IREAD | S_IWRITE;

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= S_IEXEC;
  else
    {
#if 0 /* no way of knowing the filename */
      char * p = strrchr (name, '.');
      if (p != NULL &&
	  (xstrcasecmp (p, ".exe") == 0 ||
	   xstrcasecmp (p, ".com") == 0 ||
	   xstrcasecmp (p, ".bat") == 0 ||
	   xstrcasecmp (p, ".cmd") == 0))
	permission |= S_IEXEC;
#endif
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* A version of 'utime' which handles directories as well as
   files.  */

int
utime (const char *name, struct utimbuf *times)
{
  struct utimbuf deftime;
  HANDLE fh;
  FILETIME mtime;
  FILETIME atime;

  if (times == NULL)
    {
      deftime.modtime = deftime.actime = time (NULL);
      times = &deftime;
    }

  if (w32_unicode_filenames)
    {
      wchar_t name_utf16[MAX_PATH];

      if (filename_to_utf16 (name, name_utf16) != 0)
	return -1;	/* errno set by filename_to_utf16 */

      /* Need write access to set times.  */
      fh = CreateFileW (name_utf16, FILE_WRITE_ATTRIBUTES,
			/* If NAME specifies a directory, FILE_SHARE_DELETE
			   allows other processes to delete files inside it,
			   while we have the directory open.  */
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    }
  else
    {
      char name_ansi[MAX_PATH];

      if (filename_to_ansi (name, name_ansi) != 0)
	return -1;	/* errno set by filename_to_ansi */

      fh = CreateFileA (name_ansi, FILE_WRITE_ATTRIBUTES,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			0, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    }
  if (fh != INVALID_HANDLE_VALUE)
    {
      convert_from_time_t (times->actime, &atime);
      convert_from_time_t (times->modtime, &mtime);
      if (!SetFileTime (fh, NULL, &atime, &mtime))
	{
	  CloseHandle (fh);
	  errno = EACCES;
	  return -1;
	}
      CloseHandle (fh);
    }
  else
    {
      DWORD err = GetLastError ();

      switch (err)
	{
	case ERROR_FILE_NOT_FOUND:
	case ERROR_PATH_NOT_FOUND:
	case ERROR_INVALID_DRIVE:
	case ERROR_BAD_NETPATH:
	case ERROR_DEV_NOT_EXIST:
	  /* ERROR_INVALID_NAME is the error CreateFile sets when the
	     file name includes ?s, i.e. translation to ANSI failed.  */
	case ERROR_INVALID_NAME:
	  errno = ENOENT;
	  break;
	case ERROR_TOO_MANY_OPEN_FILES:
	  errno = ENFILE;
	  break;
	case ERROR_ACCESS_DENIED:
	case ERROR_SHARING_VIOLATION:
	  errno = EACCES;
	  break;
	default:
	  errno = EINVAL;
	  break;
	}
      return -1;
    }
  return 0;
}

int
sys_umask (int mode)
{
  static int current_mask;
  int retval, arg = 0;

  /* The only bit we really support is the write bit.  Files are
     always readable on MS-Windows, and the execute bit does not exist
     at all.  */
  /* FIXME: if the GROUP and OTHER bits are reset, we should use ACLs
     to prevent access by other users on NTFS.  */
  if ((mode & S_IWRITE) != 0)
    arg |= S_IWRITE;

  retval = _umask (arg);
  /* Merge into the return value the bits they've set the last time,
     which msvcrt.dll ignores and never returns.  Emacs insists on its
     notion of mask being identical to what we return.  */
  retval |= (current_mask & ~S_IWRITE);
  current_mask = mode;

  return retval;
}


/* Symlink-related functions.  */
#ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
#define SYMBOLIC_LINK_FLAG_DIRECTORY 0x1
#endif

int
symlink (char const *filename, char const *linkname)
{
  char linkfn[MAX_UTF8_PATH], *tgtfn;
  DWORD flags = 0;
  int dir_access, filename_ends_in_slash;

  /* Diagnostics follows Posix as much as possible.  */
  if (filename == NULL || linkname == NULL)
    {
      errno = EFAULT;
      return -1;
    }
  if (!*filename)
    {
      errno = ENOENT;
      return -1;
    }
  if (strlen (filename) > MAX_UTF8_PATH || strlen (linkname) > MAX_UTF8_PATH)
    {
      errno = ENAMETOOLONG;
      return -1;
    }

  strcpy (linkfn, map_w32_filename (linkname, NULL));
  if ((volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) == 0)
    {
      errno = EPERM;
      return -1;
    }

  /* Note: since empty FILENAME was already rejected, we can safely
     refer to FILENAME[1].  */
  if (!(IS_DIRECTORY_SEP (filename[0]) || IS_DEVICE_SEP (filename[1])))
    {
      /* Non-absolute FILENAME is understood as being relative to
	 LINKNAME's directory.  We need to prepend that directory to
	 FILENAME to get correct results from faccessat below, since
	 otherwise it will interpret FILENAME relative to the
	 directory where the Emacs process runs.  Note that
	 make-symbolic-link always makes sure LINKNAME is a fully
	 expanded file name.  */
      char tem[MAX_UTF8_PATH];
      char *p = linkfn + strlen (linkfn);

      while (p > linkfn && !IS_ANY_SEP (p[-1]))
	p--;
      if (p > linkfn)
	strncpy (tem, linkfn, p - linkfn);
      strcpy (tem + (p - linkfn), filename);
      dir_access = faccessat (AT_FDCWD, tem, D_OK, AT_EACCESS);
    }
  else
    dir_access = faccessat (AT_FDCWD, filename, D_OK, AT_EACCESS);

  /* Since Windows distinguishes between symlinks to directories and
     to files, we provide a kludgy feature: if FILENAME doesn't
     exist, but ends in a slash, we create a symlink to directory.  If
     FILENAME exists and is a directory, we always create a symlink to
     directory.  */
  filename_ends_in_slash = IS_DIRECTORY_SEP (filename[strlen (filename) - 1]);
  if (dir_access == 0 || filename_ends_in_slash)
    flags = SYMBOLIC_LINK_FLAG_DIRECTORY;

  tgtfn = (char *)map_w32_filename (filename, NULL);
  if (filename_ends_in_slash)
    tgtfn[strlen (tgtfn) - 1] = '\0';

  errno = 0;
  if (!create_symbolic_link (linkfn, tgtfn, flags))
    {
      /* ENOSYS is set by create_symbolic_link, when it detects that
	 the OS doesn't support the CreateSymbolicLink API.  */
      if (errno != ENOSYS)
	{
	  DWORD w32err = GetLastError ();

	  switch (w32err)
	    {
	      /* ERROR_SUCCESS is sometimes returned when LINKFN and
		 TGTFN point to the same file name, go figure.  */
	    case ERROR_SUCCESS:
	    case ERROR_FILE_EXISTS:
	      errno = EEXIST;
	      break;
	    case ERROR_ACCESS_DENIED:
	      errno = EACCES;
	      break;
	    case ERROR_FILE_NOT_FOUND:
	    case ERROR_PATH_NOT_FOUND:
	    case ERROR_BAD_NETPATH:
	    case ERROR_INVALID_REPARSE_DATA:
	      errno = ENOENT;
	      break;
	    case ERROR_DIRECTORY:
	      errno = EISDIR;
	      break;
	    case ERROR_PRIVILEGE_NOT_HELD:
	    case ERROR_NOT_ALL_ASSIGNED:
	      errno = EPERM;
	      break;
	    case ERROR_DISK_FULL:
	      errno = ENOSPC;
	      break;
	    default:
	      errno = EINVAL;
	      break;
	    }
	}
      return -1;
    }
  return 0;
}

/* A quick inexpensive test of whether FILENAME identifies a file that
   is a symlink.  Returns non-zero if it is, zero otherwise.  FILENAME
   must already be in the normalized form returned by
   map_w32_filename.  If the symlink is to a directory, the
   FILE_ATTRIBUTE_DIRECTORY bit will be set in the return value.

   Note: for repeated operations on many files, it is best to test
   whether the underlying volume actually supports symlinks, by
   testing the FILE_SUPPORTS_REPARSE_POINTS bit in volume's flags, and
   avoid the call to this function if it doesn't.  That's because the
   call to GetFileAttributes takes a non-negligible time, especially
   on non-local or removable filesystems.  See stat_worker for an
   example of how to do that.  */
static int
is_symlink (const char *filename)
{
  DWORD attrs;
  wchar_t filename_w[MAX_PATH];
  char filename_a[MAX_PATH];
  WIN32_FIND_DATAW wfdw;
  WIN32_FIND_DATAA wfda;
  HANDLE fh;
  int attrs_mean_symlink;

  if (w32_unicode_filenames)
    {
      filename_to_utf16 (filename, filename_w);
      attrs = GetFileAttributesW (filename_w);
    }
  else
    {
      filename_to_ansi (filename, filename_a);
      attrs = GetFileAttributesA (filename_a);
    }
  if (attrs == -1)
    {
      DWORD w32err = GetLastError ();

      switch (w32err)
	{
	case ERROR_BAD_NETPATH:	/* network share, can't be a symlink */
	  break;
	case ERROR_ACCESS_DENIED:
	  errno = EACCES;
	  break;
	case ERROR_FILE_NOT_FOUND:
	case ERROR_PATH_NOT_FOUND:
	default:
	  errno = ENOENT;
	  break;
	}
      return 0;
    }
  if ((attrs & FILE_ATTRIBUTE_REPARSE_POINT) == 0)
    return 0;
  logon_network_drive (filename);
  if (w32_unicode_filenames)
    {
      fh = FindFirstFileW (filename_w, &wfdw);
      attrs_mean_symlink =
	(wfdw.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0
	&& (wfdw.dwReserved0 & IO_REPARSE_TAG_SYMLINK) == IO_REPARSE_TAG_SYMLINK;
      if (attrs_mean_symlink)
	attrs_mean_symlink |= (wfdw.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
    }
  else if (_mbspbrk (filename_a, "?"))
    {
      /* filename_to_ansi failed to convert the file name.  */
      errno = ENOENT;
      return 0;
    }
  else
    {
      fh = FindFirstFileA (filename_a, &wfda);
      attrs_mean_symlink =
	(wfda.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0
	&& (wfda.dwReserved0 & IO_REPARSE_TAG_SYMLINK) == IO_REPARSE_TAG_SYMLINK;
      if (attrs_mean_symlink)
	attrs_mean_symlink |= (wfda.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
    }
  if (fh == INVALID_HANDLE_VALUE)
    return 0;
  FindClose (fh);
  return attrs_mean_symlink;
}

/* If NAME identifies a symbolic link, copy into BUF the file name of
   the symlink's target.  Copy at most BUF_SIZE bytes, and do NOT
   null-terminate the target name, even if it fits.  Return the number
   of bytes copied, or -1 if NAME is not a symlink or any error was
   encountered while resolving it.  The file name copied into BUF is
   encoded in the current ANSI codepage.  */
ssize_t
readlink (const char *name, char *buf, size_t buf_size)
{
  const char *path;
  TOKEN_PRIVILEGES privs;
  int restore_privs = 0;
  HANDLE sh;
  ssize_t retval;
  char resolved[MAX_UTF8_PATH];

  if (name == NULL)
    {
      errno = EFAULT;
      return -1;
    }
  if (!*name)
    {
      errno = ENOENT;
      return -1;
    }

  path = map_w32_filename (name, NULL);

  if (strlen (path) > MAX_UTF8_PATH)
    {
      errno = ENAMETOOLONG;
      return -1;
    }

  errno = 0;
  if (is_windows_9x () == TRUE
      || (volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) == 0
      || !is_symlink (path))
    {
      if (!errno)
	errno = EINVAL;	/* not a symlink */
      return -1;
    }

  /* Done with simple tests, now we're in for some _real_ work.  */
  if (enable_privilege (SE_BACKUP_NAME, TRUE, &privs))
    restore_privs = 1;
  /* Implementation note: From here and onward, don't return early,
     since that will fail to restore the original set of privileges of
     the calling thread.  */

  retval = -1;	/* not too optimistic, are we? */

  /* Note: In the next call to CreateFile, we use zero as the 2nd
     argument because, when the symlink is a hidden/system file,
     e.g. 'C:\Users\All Users', GENERIC_READ fails with
     ERROR_ACCESS_DENIED.  Zero seems to work just fine, both for file
     and directory symlinks.  */
  if (w32_unicode_filenames)
    {
      wchar_t path_w[MAX_PATH];

      filename_to_utf16 (path, path_w);
      sh = CreateFileW (path_w, 0, 0, NULL, OPEN_EXISTING,
			FILE_FLAG_OPEN_REPARSE_POINT
			| FILE_FLAG_BACKUP_SEMANTICS,
			NULL);
    }
  else
    {
      char path_a[MAX_PATH];

      filename_to_ansi (path, path_a);
      sh = CreateFileA (path_a, 0, 0, NULL, OPEN_EXISTING,
			FILE_FLAG_OPEN_REPARSE_POINT
			| FILE_FLAG_BACKUP_SEMANTICS,
			NULL);
    }
  if (sh != INVALID_HANDLE_VALUE)
    {
      BYTE reparse_buf[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
      REPARSE_DATA_BUFFER *reparse_data = (REPARSE_DATA_BUFFER *)&reparse_buf[0];
      DWORD retbytes;

      if (!DeviceIoControl (sh, FSCTL_GET_REPARSE_POINT, NULL, 0,
			    reparse_buf, MAXIMUM_REPARSE_DATA_BUFFER_SIZE,
			    &retbytes, NULL))
	errno = EIO;
      else if (reparse_data->ReparseTag != IO_REPARSE_TAG_SYMLINK)
	errno = EINVAL;
      else
	{
	  /* Copy the link target name, in wide characters, from
	     reparse_data, then convert it to multibyte encoding in
	     the current locale's codepage.  */
	  WCHAR *lwname;
	  size_t lname_size;
	  USHORT lwname_len =
	    reparse_data->SymbolicLinkReparseBuffer.PrintNameLength;
	  WCHAR *lwname_src =
	    reparse_data->SymbolicLinkReparseBuffer.PathBuffer
	    + reparse_data->SymbolicLinkReparseBuffer.PrintNameOffset/sizeof(WCHAR);
	  size_t size_to_copy = buf_size;

	  /* According to MSDN, PrintNameLength does not include the
	     terminating null character.  */
	  lwname = alloca ((lwname_len + 1) * sizeof(WCHAR));
	  memcpy (lwname, lwname_src, lwname_len);
	  lwname[lwname_len/sizeof(WCHAR)] = 0; /* null-terminate */
	  filename_from_utf16 (lwname, resolved);
	  dostounix_filename (resolved);
	  lname_size = strlen (resolved) + 1;
	  if (lname_size <= buf_size)
	    size_to_copy = lname_size;
	  strncpy (buf, resolved, size_to_copy);
	  /* Success!  */
	  retval = size_to_copy;
	}
      CloseHandle (sh);
    }
  else
    {
      /* CreateFile failed.  */
      DWORD w32err2 = GetLastError ();

      switch (w32err2)
	{
	case ERROR_FILE_NOT_FOUND:
	case ERROR_PATH_NOT_FOUND:
	  errno = ENOENT;
	  break;
	case ERROR_ACCESS_DENIED:
	case ERROR_TOO_MANY_OPEN_FILES:
	  errno = EACCES;
	  break;
	default:
	  errno = EPERM;
	  break;
	}
    }
  if (restore_privs)
    {
      restore_privilege (&privs);
      revert_to_self ();
    }

  return retval;
}

ssize_t
readlinkat (int fd, char const *name, char *buffer,
	    size_t buffer_size)
{
  /* Rely on a hack: an open directory is modeled as file descriptor 0,
     as in fstatat.  FIXME: Add proper support for readlinkat.  */
  char fullname[MAX_UTF8_PATH];

  if (fd != AT_FDCWD)
    {
      if (_snprintf (fullname, sizeof fullname, "%s/%s", dir_pathname, name)
	  < 0)
	{
	  errno = ENAMETOOLONG;
	  return -1;
	}
      name = fullname;
    }

  return readlink (name, buffer, buffer_size);
}

/* If FILE is a symlink, return its target (stored in a static
   buffer); otherwise return FILE.

   This function repeatedly resolves symlinks in the last component of
   a chain of symlink file names, as in foo -> bar -> baz -> ...,
   until it arrives at a file whose last component is not a symlink,
   or some error occurs.  It returns the target of the last
   successfully resolved symlink in the chain.  If it succeeds to
   resolve even a single symlink, the value returned is an absolute
   file name with backslashes (result of GetFullPathName).  By
   contrast, if the original FILE is returned, it is unaltered.

   Note: This function can set errno even if it succeeds.

   Implementation note: we only resolve the last portion ("basename")
   of the argument FILE and of each following file in the chain,
   disregarding any possible symlinks in its leading directories.
   This is because Windows system calls and library functions
   transparently resolve symlinks in leading directories and return
   correct information, as long as the basename is not a symlink.  */
static char *
chase_symlinks (const char *file)
{
  static char target[MAX_UTF8_PATH];
  char link[MAX_UTF8_PATH];
  wchar_t target_w[MAX_PATH], link_w[MAX_PATH];
  char target_a[MAX_PATH], link_a[MAX_PATH];
  ssize_t res, link_len;
  int loop_count = 0;

  if (is_windows_9x () == TRUE || !is_symlink (file))
    return (char *)file;

  if (w32_unicode_filenames)
    {
      wchar_t file_w[MAX_PATH];

      filename_to_utf16 (file, file_w);
      if (GetFullPathNameW (file_w, MAX_PATH, link_w, NULL) == 0)
	return (char *)file;
      filename_from_utf16 (link_w, link);
    }
  else
    {
      char file_a[MAX_PATH];

      filename_to_ansi (file, file_a);
      if (GetFullPathNameA (file_a, MAX_PATH, link_a, NULL) == 0)
	return (char *)file;
      filename_from_ansi (link_a, link);
    }
  link_len = strlen (link);

  target[0] = '\0';
  do {

    /* Remove trailing slashes, as we want to resolve the last
       non-trivial part of the link name.  */
    while (link_len > 3 && IS_DIRECTORY_SEP (link[link_len-1]))
      link[link_len--] = '\0';

    res = readlink (link, target, MAX_UTF8_PATH);
    if (res > 0)
      {
	target[res] = '\0';
	if (!(IS_DEVICE_SEP (target[1])
	      || (IS_DIRECTORY_SEP (target[0]) && IS_DIRECTORY_SEP (target[1]))))
	  {
	    /* Target is relative.  Append it to the directory part of
	       the symlink, then copy the result back to target.  */
	    char *p = link + link_len;

	    while (p > link && !IS_ANY_SEP (p[-1]))
	      p--;
	    strcpy (p, target);
	    strcpy (target, link);
	  }
	/* Resolve any "." and ".." to get a fully-qualified file name
	   in link[] again. */
	if (w32_unicode_filenames)
	  {
	    filename_to_utf16 (target, target_w);
	    link_len = GetFullPathNameW (target_w, MAX_PATH, link_w, NULL);
	    if (link_len > 0)
	      filename_from_utf16 (link_w, link);
	  }
	else
	  {
	    filename_to_ansi (target, target_a);
	    link_len = GetFullPathNameA (target_a, MAX_PATH, link_a, NULL);
	    if (link_len > 0)
	      filename_from_ansi (link_a, link);
	  }
	link_len = strlen (link);
      }
  } while (res > 0 && link_len > 0 && ++loop_count <= 100);

  if (loop_count > 100)
    errno = ELOOP;

  if (target[0] == '\0') /* not a single call to readlink succeeded */
    return (char *)file;
  return target;
}


/* Posix ACL emulation.  */

int
acl_valid (acl_t acl)
{
  return is_valid_security_descriptor ((PSECURITY_DESCRIPTOR)acl) ? 0 : -1;
}

char *
acl_to_text (acl_t acl, ssize_t *size)
{
  LPTSTR str_acl;
  SECURITY_INFORMATION flags =
    OWNER_SECURITY_INFORMATION |
    GROUP_SECURITY_INFORMATION |
    DACL_SECURITY_INFORMATION;
  char *retval = NULL;
  ULONG local_size;
  int e = errno;

  errno = 0;

  if (convert_sd_to_sddl ((PSECURITY_DESCRIPTOR)acl, SDDL_REVISION_1, flags, &str_acl, &local_size))
    {
      errno = e;
      /* We don't want to mix heaps, so we duplicate the string in our
	 heap and free the one allocated by the API.  */
      retval = xstrdup (str_acl);
      if (size)
	*size = local_size;
      LocalFree (str_acl);
    }
  else if (errno != ENOTSUP)
    errno = EINVAL;

  return retval;
}

acl_t
acl_from_text (const char *acl_str)
{
  PSECURITY_DESCRIPTOR psd, retval = NULL;
  ULONG sd_size;
  int e = errno;

  errno = 0;

  if (convert_sddl_to_sd (acl_str, SDDL_REVISION_1, &psd, &sd_size))
    {
      errno = e;
      retval = xmalloc (sd_size);
      memcpy (retval, psd, sd_size);
      LocalFree (psd);
    }
  else if (errno != ENOTSUP)
    errno = EINVAL;

  return retval;
}

int
acl_free (void *ptr)
{
  xfree (ptr);
  return 0;
}

acl_t
acl_get_file (const char *fname, acl_type_t type)
{
  PSECURITY_DESCRIPTOR psd = NULL;
  const char *filename;

  if (type == ACL_TYPE_ACCESS)
    {
      DWORD sd_len, err;
      SECURITY_INFORMATION si =
	OWNER_SECURITY_INFORMATION |
	GROUP_SECURITY_INFORMATION |
	DACL_SECURITY_INFORMATION ;
      int e = errno;

      filename = map_w32_filename (fname, NULL);
      if ((volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) != 0)
	fname = chase_symlinks (filename);
      else
	fname = filename;

      errno = 0;
      if (!get_file_security (fname, si, psd, 0, &sd_len)
	  && errno != ENOTSUP)
	{
	  err = GetLastError ();
	  if (err == ERROR_INSUFFICIENT_BUFFER)
	    {
	      psd = xmalloc (sd_len);
	      if (!get_file_security (fname, si, psd, sd_len, &sd_len))
		{
		  xfree (psd);
		  errno = EIO;
		  psd = NULL;
		}
	    }
	  else if (err == ERROR_FILE_NOT_FOUND
		   || err == ERROR_PATH_NOT_FOUND
		   /* ERROR_INVALID_NAME is what we get if
		      w32-unicode-filenames is nil and the file cannot
		      be encoded in the current ANSI codepage. */
		   || err == ERROR_INVALID_NAME)
	    errno = ENOENT;
	  else
	    errno = EIO;
	}
      else if (!errno)
	errno = e;
    }
  else if (type != ACL_TYPE_DEFAULT)
    errno = EINVAL;

  return psd;
}

int
acl_set_file (const char *fname, acl_type_t type, acl_t acl)
{
  TOKEN_PRIVILEGES old1, old2;
  DWORD err;
  int st = 0, retval = -1;
  SECURITY_INFORMATION flags = 0;
  PSID psidOwner, psidGroup;
  PACL pacl;
  BOOL dflt;
  BOOL dacl_present;
  int e;
  const char *filename;

  if (acl_valid (acl) != 0
      || (type != ACL_TYPE_DEFAULT && type != ACL_TYPE_ACCESS))
    {
      errno = EINVAL;
      return -1;
    }

  if (type == ACL_TYPE_DEFAULT)
    {
      errno = ENOSYS;
      return -1;
    }

  filename = map_w32_filename (fname, NULL);
  if ((volume_info.flags & FILE_SUPPORTS_REPARSE_POINTS) != 0)
    fname = chase_symlinks (filename);
  else
    fname = filename;

  if (get_security_descriptor_owner ((PSECURITY_DESCRIPTOR)acl, &psidOwner,
				     &dflt)
      && psidOwner)
    flags |= OWNER_SECURITY_INFORMATION;
  if (get_security_descriptor_group ((PSECURITY_DESCRIPTOR)acl, &psidGroup,
				     &dflt)
      && psidGroup)
    flags |= GROUP_SECURITY_INFORMATION;
  if (get_security_descriptor_dacl ((PSECURITY_DESCRIPTOR)acl, &dacl_present,
				    &pacl, &dflt)
      && dacl_present)
    flags |= DACL_SECURITY_INFORMATION;
  if (!flags)
    return 0;

  /* According to KB-245153, setting the owner will succeed if either:
     (1) the caller is the user who will be the new owner, and has the
         SE_TAKE_OWNERSHIP privilege, or
     (2) the caller has the SE_RESTORE privilege, in which case she can
         set any valid user or group as the owner

     We request below both SE_TAKE_OWNERSHIP and SE_RESTORE
     privileges, and disregard any failures in obtaining them.  If
     these privileges cannot be obtained, and do not already exist in
     the calling thread's security token, this function could fail
     with EPERM.  */
  if (enable_privilege (SE_TAKE_OWNERSHIP_NAME, TRUE, &old1))
    st++;
  if (enable_privilege (SE_RESTORE_NAME, TRUE, &old2))
    st++;

  e = errno;
  errno = 0;
  /* SetFileSecurity is deprecated by MS, and sometimes fails when
     DACL inheritance is involved, but it seems to preserve ownership
     better than SetNamedSecurityInfo, which is important e.g., in
     copy-file.  */
  if (!set_file_security (fname, flags, (PSECURITY_DESCRIPTOR)acl))
    {
      err = GetLastError ();

      if (errno != ENOTSUP)
	err = set_named_security_info (fname, SE_FILE_OBJECT, flags,
				       psidOwner, psidGroup, pacl, NULL);
    }
  else
    err = ERROR_SUCCESS;
  if (err != ERROR_SUCCESS)
    {
      if (errno == ENOTSUP)
	;
      else if (err == ERROR_INVALID_OWNER
	       || err == ERROR_NOT_ALL_ASSIGNED
	       || err == ERROR_ACCESS_DENIED)
	{
	  /* Maybe the requested ACL and the one the file already has
	     are identical, in which case we can silently ignore the
	     failure.  (And no, Windows doesn't.)  */
	  acl_t current_acl = acl_get_file (fname, ACL_TYPE_ACCESS);

	  errno = EPERM;
	  if (current_acl)
	    {
	      char *acl_from = acl_to_text (current_acl, NULL);
	      char *acl_to = acl_to_text (acl, NULL);

	      if (acl_from && acl_to && xstrcasecmp (acl_from, acl_to) == 0)
		{
		  retval = 0;
		  errno = e;
		}
	      if (acl_from)
		acl_free (acl_from);
	      if (acl_to)
		acl_free (acl_to);
	      acl_free (current_acl);
	    }
	}
      else if (err == ERROR_FILE_NOT_FOUND
	       || err == ERROR_PATH_NOT_FOUND
	       /* ERROR_INVALID_NAME is what we get if
		  w32-unicode-filenames is nil and the file cannot be
		  encoded in the current ANSI codepage. */
	       || err == ERROR_INVALID_NAME)
	errno = ENOENT;
      else
	errno = EACCES;
    }
  else
    {
      retval = 0;
      errno = e;
    }

  if (st)
    {
      if (st >= 2)
	restore_privilege (&old2);
      restore_privilege (&old1);
      revert_to_self ();
    }

  return retval;
}

/* Return true if errno value ERRNUM indicates that ACLs are well
   supported on this system.  ERRNUM should be an errno value obtained
   after an ACL-related system call fails.  */
bool
acl_errno_valid (int errnum)
{
  switch (errnum)
    {
    case EBUSY:
    case EINVAL:
    case ENOTSUP:
      return false;
    default:
      return true;
    }
}


/* MS-Windows version of careadlinkat (cf. ../lib/careadlinkat.c).  We
   have a fixed max size for file names, so we don't need the kind of
   alloc/malloc/realloc dance the gnulib version does.  We also don't
   support FD-relative symlinks.  */
char *
careadlinkat (int fd, char const *filename,
              char *buffer, size_t buffer_size,
              struct allocator const *alloc,
              ssize_t (*preadlinkat) (int, char const *, char *, size_t))
{
  char linkname[MAX_UTF8_PATH];
  ssize_t link_size;

  link_size = preadlinkat (fd, filename, linkname, sizeof(linkname));

  if (link_size > 0)
    {
      char *retval = buffer;

      linkname[link_size++] = '\0';
      if (link_size > buffer_size)
	retval = (char *)(alloc ? alloc->allocate : xmalloc) (link_size);
      if (retval)
	memcpy (retval, linkname, link_size);

      return retval;
    }
  return NULL;
}

int
w32_copy_file (const char *from, const char *to,
	       int keep_time, int preserve_ownership, int copy_acls)
{
  acl_t acl = NULL;
  BOOL copy_result;
  wchar_t from_w[MAX_PATH], to_w[MAX_PATH];
  char from_a[MAX_PATH], to_a[MAX_PATH];

  /* We ignore preserve_ownership for now.  */
  preserve_ownership = preserve_ownership;

  if (copy_acls)
    {
      acl = acl_get_file (from, ACL_TYPE_ACCESS);
      if (acl == NULL && acl_errno_valid (errno))
	return -2;
    }
  if (w32_unicode_filenames)
    {
      filename_to_utf16 (from, from_w);
      filename_to_utf16 (to, to_w);
      copy_result = CopyFileW (from_w, to_w, FALSE);
    }
  else
    {
      filename_to_ansi (from, from_a);
      filename_to_ansi (to, to_a);
      copy_result = CopyFileA (from_a, to_a, FALSE);
    }
  if (!copy_result)
    {
      /* CopyFile doesn't set errno when it fails.  By far the most
	 "popular" reason is that the target is read-only.  */
      DWORD err = GetLastError ();

      switch (err)
	{
	case ERROR_FILE_NOT_FOUND:
	  errno = ENOENT;
	  break;
	case ERROR_ACCESS_DENIED:
	  errno = EACCES;
	  break;
	case ERROR_ENCRYPTION_FAILED:
	  errno = EIO;
	  break;
	default:
	  errno = EPERM;
	  break;
	}

      if (acl)
	acl_free (acl);
      return -1;
    }
  /* CopyFile retains the timestamp by default.  However, see
     "Community Additions" for CopyFile: it sounds like that is not
     entirely true.  Testing on Windows XP confirms that modified time
     is copied, but creation and last-access times are not.
     FIXME?  */
  else if (!keep_time)
    {
      struct timespec now;
      DWORD attributes;

      if (w32_unicode_filenames)
	{
	  /* Ensure file is writable while its times are set.  */
	  attributes = GetFileAttributesW (to_w);
	  SetFileAttributesW (to_w, attributes & ~FILE_ATTRIBUTE_READONLY);
	  now = current_timespec ();
	  if (set_file_times (-1, to, now, now))
	    {
	      /* Restore original attributes.  */
	      SetFileAttributesW (to_w, attributes);
	      if (acl)
		acl_free (acl);
	      return -3;
	    }
	  /* Restore original attributes.  */
	  SetFileAttributesW (to_w, attributes);
	}
      else
	{
	  attributes = GetFileAttributesA (to_a);
	  SetFileAttributesA (to_a, attributes & ~FILE_ATTRIBUTE_READONLY);
	  now = current_timespec ();
	  if (set_file_times (-1, to, now, now))
	    {
	      SetFileAttributesA (to_a, attributes);
	      if (acl)
		acl_free (acl);
	      return -3;
	    }
	  SetFileAttributesA (to_a, attributes);
	}
    }
  if (acl != NULL)
    {
      bool fail =
	acl_set_file (to, ACL_TYPE_ACCESS, acl) != 0;
      acl_free (acl);
      if (fail && acl_errno_valid (errno))
	return -4;
    }

  return 0;
}


/* Support for browsing other processes and their attributes.  See
   process.c for the Lisp bindings.  */

/* Helper wrapper functions.  */

static HANDLE WINAPI
create_toolhelp32_snapshot (DWORD Flags, DWORD Ignored)
{
  static CreateToolhelp32Snapshot_Proc s_pfn_Create_Toolhelp32_Snapshot = NULL;

  if (g_b_init_create_toolhelp32_snapshot == 0)
    {
      g_b_init_create_toolhelp32_snapshot = 1;
      s_pfn_Create_Toolhelp32_Snapshot = (CreateToolhelp32Snapshot_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"CreateToolhelp32Snapshot");
    }
  if (s_pfn_Create_Toolhelp32_Snapshot == NULL)
    {
      return INVALID_HANDLE_VALUE;
    }
  return (s_pfn_Create_Toolhelp32_Snapshot (Flags, Ignored));
}

static BOOL WINAPI
process32_first (HANDLE hSnapshot, LPPROCESSENTRY32 lppe)
{
  static Process32First_Proc s_pfn_Process32_First = NULL;

  if (g_b_init_process32_first == 0)
    {
      g_b_init_process32_first = 1;
      s_pfn_Process32_First = (Process32First_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"Process32First");
    }
  if (s_pfn_Process32_First == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Process32_First (hSnapshot, lppe));
}

static BOOL WINAPI
process32_next (HANDLE hSnapshot, LPPROCESSENTRY32 lppe)
{
  static Process32Next_Proc s_pfn_Process32_Next = NULL;

  if (g_b_init_process32_next == 0)
    {
      g_b_init_process32_next = 1;
      s_pfn_Process32_Next = (Process32Next_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"Process32Next");
    }
  if (s_pfn_Process32_Next == NULL)
    {
      return FALSE;
    }
  return (s_pfn_Process32_Next (hSnapshot, lppe));
}

static BOOL WINAPI
open_thread_token (HANDLE ThreadHandle,
		   DWORD DesiredAccess,
		   BOOL OpenAsSelf,
		   PHANDLE TokenHandle)
{
  static OpenThreadToken_Proc s_pfn_Open_Thread_Token = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      SetLastError (ERROR_NOT_SUPPORTED);
      return FALSE;
    }
  if (g_b_init_open_thread_token == 0)
    {
      g_b_init_open_thread_token = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Open_Thread_Token =
        (OpenThreadToken_Proc) GetProcAddress (hm_advapi32, "OpenThreadToken");
    }
  if (s_pfn_Open_Thread_Token == NULL)
    {
      SetLastError (ERROR_NOT_SUPPORTED);
      return FALSE;
    }
  return (
      s_pfn_Open_Thread_Token (
          ThreadHandle,
          DesiredAccess,
	  OpenAsSelf,
          TokenHandle)
      );
}

static BOOL WINAPI
impersonate_self (SECURITY_IMPERSONATION_LEVEL ImpersonationLevel)
{
  static ImpersonateSelf_Proc s_pfn_Impersonate_Self = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_impersonate_self == 0)
    {
      g_b_init_impersonate_self = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Impersonate_Self =
        (ImpersonateSelf_Proc) GetProcAddress (hm_advapi32, "ImpersonateSelf");
    }
  if (s_pfn_Impersonate_Self == NULL)
    {
      return FALSE;
    }
  return s_pfn_Impersonate_Self (ImpersonationLevel);
}

static BOOL WINAPI
revert_to_self (void)
{
  static RevertToSelf_Proc s_pfn_Revert_To_Self = NULL;
  HMODULE hm_advapi32 = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_revert_to_self == 0)
    {
      g_b_init_revert_to_self = 1;
      hm_advapi32 = LoadLibrary ("Advapi32.dll");
      s_pfn_Revert_To_Self =
        (RevertToSelf_Proc) GetProcAddress (hm_advapi32, "RevertToSelf");
    }
  if (s_pfn_Revert_To_Self == NULL)
    {
      return FALSE;
    }
  return s_pfn_Revert_To_Self ();
}

static BOOL WINAPI
get_process_memory_info (HANDLE h_proc,
			 PPROCESS_MEMORY_COUNTERS mem_counters,
			 DWORD bufsize)
{
  static GetProcessMemoryInfo_Proc s_pfn_Get_Process_Memory_Info = NULL;
  HMODULE hm_psapi = NULL;
  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_process_memory_info == 0)
    {
      g_b_init_get_process_memory_info = 1;
      hm_psapi = LoadLibrary ("Psapi.dll");
      if (hm_psapi)
	s_pfn_Get_Process_Memory_Info = (GetProcessMemoryInfo_Proc)
	  GetProcAddress (hm_psapi, "GetProcessMemoryInfo");
    }
  if (s_pfn_Get_Process_Memory_Info == NULL)
    {
      return FALSE;
    }
  return s_pfn_Get_Process_Memory_Info (h_proc, mem_counters, bufsize);
}

static BOOL WINAPI
get_process_working_set_size (HANDLE h_proc,
			      PSIZE_T minrss,
			      PSIZE_T maxrss)
{
  static GetProcessWorkingSetSize_Proc
    s_pfn_Get_Process_Working_Set_Size = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_get_process_working_set_size == 0)
    {
      g_b_init_get_process_working_set_size = 1;
      s_pfn_Get_Process_Working_Set_Size = (GetProcessWorkingSetSize_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GetProcessWorkingSetSize");
    }
  if (s_pfn_Get_Process_Working_Set_Size == NULL)
    {
      return FALSE;
    }
  return s_pfn_Get_Process_Working_Set_Size (h_proc, minrss, maxrss);
}

static BOOL WINAPI
global_memory_status (MEMORYSTATUS *buf)
{
  static GlobalMemoryStatus_Proc s_pfn_Global_Memory_Status = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_global_memory_status == 0)
    {
      g_b_init_global_memory_status = 1;
      s_pfn_Global_Memory_Status = (GlobalMemoryStatus_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GlobalMemoryStatus");
    }
  if (s_pfn_Global_Memory_Status == NULL)
    {
      return FALSE;
    }
  return s_pfn_Global_Memory_Status (buf);
}

static BOOL WINAPI
global_memory_status_ex (MEMORY_STATUS_EX *buf)
{
  static GlobalMemoryStatusEx_Proc s_pfn_Global_Memory_Status_Ex = NULL;

  if (is_windows_9x () == TRUE)
    {
      return FALSE;
    }
  if (g_b_init_global_memory_status_ex == 0)
    {
      g_b_init_global_memory_status_ex = 1;
      s_pfn_Global_Memory_Status_Ex = (GlobalMemoryStatusEx_Proc)
	GetProcAddress (GetModuleHandle ("kernel32.dll"),
			"GlobalMemoryStatusEx");
    }
  if (s_pfn_Global_Memory_Status_Ex == NULL)
    {
      return FALSE;
    }
  return s_pfn_Global_Memory_Status_Ex (buf);
}

Lisp_Object
list_system_processes (void)
{
  Lisp_Object proclist = Qnil;
  HANDLE h_snapshot;

  h_snapshot = create_toolhelp32_snapshot (TH32CS_SNAPPROCESS, 0);

  if (h_snapshot != INVALID_HANDLE_VALUE)
    {
      PROCESSENTRY32 proc_entry;
      DWORD proc_id;
      BOOL res;

      proc_entry.dwSize = sizeof (PROCESSENTRY32);
      for (res = process32_first (h_snapshot, &proc_entry); res;
	   res = process32_next  (h_snapshot, &proc_entry))
	{
	  proc_id = proc_entry.th32ProcessID;
	  proclist = Fcons (make_fixnum_or_float (proc_id), proclist);
	}

      CloseHandle (h_snapshot);
      proclist = Fnreverse (proclist);
    }

  return proclist;
}

static int
enable_privilege (LPCTSTR priv_name, BOOL enable_p, TOKEN_PRIVILEGES *old_priv)
{
  TOKEN_PRIVILEGES priv;
  DWORD priv_size = sizeof (priv);
  DWORD opriv_size = sizeof (*old_priv);
  HANDLE h_token = NULL;
  HANDLE h_thread = GetCurrentThread ();
  int ret_val = 0;
  BOOL res;

  res = open_thread_token (h_thread,
			   TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
			   FALSE, &h_token);
  if (!res && GetLastError () == ERROR_NO_TOKEN)
    {
      if (impersonate_self (SecurityImpersonation))
	  res = open_thread_token (h_thread,
				   TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
				   FALSE, &h_token);
    }
  if (res)
    {
      priv.PrivilegeCount = 1;
      priv.Privileges[0].Attributes = enable_p ? SE_PRIVILEGE_ENABLED : 0;
      LookupPrivilegeValue (NULL, priv_name, &priv.Privileges[0].Luid);
      if (AdjustTokenPrivileges (h_token, FALSE, &priv, priv_size,
				 old_priv, &opriv_size)
	  && GetLastError () != ERROR_NOT_ALL_ASSIGNED)
	ret_val = 1;
    }
  if (h_token)
    CloseHandle (h_token);

  return ret_val;
}

static int
restore_privilege (TOKEN_PRIVILEGES *priv)
{
  DWORD priv_size = sizeof (*priv);
  HANDLE h_token = NULL;
  int ret_val = 0;

  if (open_thread_token (GetCurrentThread (),
			 TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES,
			 FALSE, &h_token))
    {
      if (AdjustTokenPrivileges (h_token, FALSE, priv, priv_size, NULL, NULL)
	  && GetLastError () != ERROR_NOT_ALL_ASSIGNED)
	ret_val = 1;
    }
  if (h_token)
    CloseHandle (h_token);

  return ret_val;
}

static Lisp_Object
ltime (ULONGLONG time_100ns)
{
  ULONGLONG time_sec = time_100ns / 10000000;
  int subsec = time_100ns % 10000000;
  return list4i (time_sec >> 16, time_sec & 0xffff,
		 subsec / 10, subsec % 10 * 100000);
}

#define U64_TO_LISP_TIME(time) ltime (time)

static int
process_times (HANDLE h_proc, Lisp_Object *ctime, Lisp_Object *etime,
	       Lisp_Object *stime, Lisp_Object *utime, Lisp_Object *ttime,
	       double *pcpu)
{
  FILETIME ft_creation, ft_exit, ft_kernel, ft_user, ft_current;
  ULONGLONG tem1, tem2, tem3, tem;

  if (!h_proc
      || !get_process_times_fn
      || !(*get_process_times_fn) (h_proc, &ft_creation, &ft_exit,
				   &ft_kernel, &ft_user))
    return 0;

  GetSystemTimeAsFileTime (&ft_current);

  FILETIME_TO_U64 (tem1, ft_kernel);
  *stime = U64_TO_LISP_TIME (tem1);

  FILETIME_TO_U64 (tem2, ft_user);
  *utime = U64_TO_LISP_TIME (tem2);

  tem3 = tem1 + tem2;
  *ttime = U64_TO_LISP_TIME (tem3);

  FILETIME_TO_U64 (tem, ft_creation);
  /* Process no 4 (System) returns zero creation time.  */
  if (tem)
    tem -= utc_base;
  *ctime = U64_TO_LISP_TIME (tem);

  if (tem)
    {
      FILETIME_TO_U64 (tem3, ft_current);
      tem = (tem3 - utc_base) - tem;
    }
  *etime = U64_TO_LISP_TIME (tem);

  if (tem)
    {
      *pcpu = 100.0 * (tem1 + tem2) / tem;
      if (*pcpu > 100)
	*pcpu = 100.0;
    }
  else
    *pcpu = 0;

  return 1;
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  Lisp_Object attrs = Qnil;
  Lisp_Object cmd_str, decoded_cmd, tem;
  HANDLE h_snapshot, h_proc;
  DWORD proc_id;
  int found_proc = 0;
  char uname[UNLEN+1], gname[GNLEN+1], domain[1025];
  DWORD ulength = sizeof (uname), dlength = sizeof (domain), needed;
  DWORD glength = sizeof (gname);
  HANDLE token = NULL;
  SID_NAME_USE user_type;
  unsigned char *buf = NULL;
  DWORD blen = 0;
  TOKEN_USER user_token;
  TOKEN_PRIMARY_GROUP group_token;
  unsigned euid;
  unsigned egid;
  PROCESS_MEMORY_COUNTERS mem;
  PROCESS_MEMORY_COUNTERS_EX mem_ex;
  SIZE_T minrss, maxrss;
  MEMORYSTATUS memst;
  MEMORY_STATUS_EX memstex;
  double totphys = 0.0;
  Lisp_Object ctime, stime, utime, etime, ttime;
  double pcpu;
  BOOL result = FALSE;

  CHECK_NUMBER_OR_FLOAT (pid);
  proc_id = FLOATP (pid) ? XFLOAT_DATA (pid) : XINT (pid);

  h_snapshot = create_toolhelp32_snapshot (TH32CS_SNAPPROCESS, 0);

  if (h_snapshot != INVALID_HANDLE_VALUE)
    {
      PROCESSENTRY32 pe;
      BOOL res;

      pe.dwSize = sizeof (PROCESSENTRY32);
      for (res = process32_first (h_snapshot, &pe); res;
	   res = process32_next  (h_snapshot, &pe))
	{
	  if (proc_id == pe.th32ProcessID)
	    {
	      if (proc_id == 0)
		decoded_cmd = build_string ("Idle");
	      else
		{
		  /* Decode the command name from locale-specific
		     encoding.  */
		  cmd_str = build_unibyte_string (pe.szExeFile);

		  decoded_cmd =
		    code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
		}
	      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);
	      attrs = Fcons (Fcons (Qppid,
				    make_fixnum_or_float (pe.th32ParentProcessID)),
			     attrs);
	      attrs = Fcons (Fcons (Qpri, make_number (pe.pcPriClassBase)),
			     attrs);
	      attrs = Fcons (Fcons (Qthcount,
				    make_fixnum_or_float (pe.cntThreads)),
			     attrs);
	      found_proc = 1;
	      break;
	    }
	}

      CloseHandle (h_snapshot);
    }

  if (!found_proc)
    return Qnil;

  h_proc = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
			FALSE, proc_id);
  /* If we were denied a handle to the process, try again after
     enabling the SeDebugPrivilege in our process.  */
  if (!h_proc)
    {
      TOKEN_PRIVILEGES priv_current;

      if (enable_privilege (SE_DEBUG_NAME, TRUE, &priv_current))
	{
	  h_proc = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
				FALSE, proc_id);
	  restore_privilege (&priv_current);
	  revert_to_self ();
	}
    }
  if (h_proc)
    {
      result = open_process_token (h_proc, TOKEN_QUERY, &token);
      if (result)
	{
	  result = get_token_information (token, TokenUser, NULL, 0, &blen);
	  if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
	    {
	      buf = xmalloc (blen);
	      result = get_token_information (token, TokenUser,
					      (LPVOID)buf, blen, &needed);
	      if (result)
		{
		  memcpy (&user_token, buf, sizeof (user_token));
		  if (!w32_cached_id (user_token.User.Sid, &euid, uname))
		    {
		      euid = get_rid (user_token.User.Sid);
		      result = lookup_account_sid (NULL, user_token.User.Sid,
						   uname, &ulength,
						   domain, &dlength,
						   &user_type);
		      if (result)
			w32_add_to_cache (user_token.User.Sid, euid, uname);
		      else
			{
			  strcpy (uname, "unknown");
			  result = TRUE;
			}
		    }
		  ulength = strlen (uname);
		}
	    }
	}
      if (result)
	{
	  /* Determine a reasonable euid and gid values.  */
	  if (xstrcasecmp ("administrator", uname) == 0)
	    {
	      euid = 500;	/* well-known Administrator uid */
	      egid = 513;	/* well-known None gid */
	    }
	  else
	    {
	      /* Get group id and name.  */
	      result = get_token_information (token, TokenPrimaryGroup,
					      (LPVOID)buf, blen, &needed);
	      if (!result && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
		{
		  buf = xrealloc (buf, blen = needed);
		  result = get_token_information (token, TokenPrimaryGroup,
						  (LPVOID)buf, blen, &needed);
		}
	      if (result)
		{
		  memcpy (&group_token, buf, sizeof (group_token));
		  if (!w32_cached_id (group_token.PrimaryGroup, &egid, gname))
		    {
		      egid = get_rid (group_token.PrimaryGroup);
		      dlength = sizeof (domain);
		      result =
			lookup_account_sid (NULL, group_token.PrimaryGroup,
					    gname, &glength, NULL, &dlength,
					    &user_type);
		      if (result)
			w32_add_to_cache (group_token.PrimaryGroup,
					  egid, gname);
		      else
			{
			  strcpy (gname, "None");
			  result = TRUE;
			}
		    }
		  glength = strlen (gname);
		}
	    }
	}
      xfree (buf);
    }
  if (!result)
    {
      if (!is_windows_9x ())
	{
	  /* We couldn't open the process token, presumably because of
	     insufficient access rights.  Assume this process is run
	     by the system.  */
	  strcpy (uname, "SYSTEM");
	  strcpy (gname, "None");
	  euid = 18;	/* SYSTEM */
	  egid = 513;	/* None */
	  glength = strlen (gname);
	  ulength = strlen (uname);
	}
      /* If we are running under Windows 9X, where security calls are
	 not supported, we assume all processes are run by the current
	 user.  */
      else if (GetUserName (uname, &ulength))
	{
	  if (xstrcasecmp ("administrator", uname) == 0)
	    euid = 0;
	  else
	    euid = 123;
	  egid = euid;
	  strcpy (gname, "None");
	  glength = strlen (gname);
	  ulength = strlen (uname);
	}
      else
	{
	  euid = 123;
	  egid = 123;
	  strcpy (uname, "administrator");
	  ulength = strlen (uname);
	  strcpy (gname, "None");
	  glength = strlen (gname);
	}
      if (token)
	CloseHandle (token);
    }

  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (euid)), attrs);
  tem = make_unibyte_string (uname, ulength);
  attrs = Fcons (Fcons (Quser,
			 code_convert_string_norecord (tem, Vlocale_coding_system, 0)),
		 attrs);
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (egid)), attrs);
  tem = make_unibyte_string (gname, glength);
  attrs = Fcons (Fcons (Qgroup,
			 code_convert_string_norecord (tem, Vlocale_coding_system, 0)),
		 attrs);

  if (global_memory_status_ex (&memstex))
#if __GNUC__ || (defined (_MSC_VER) && _MSC_VER >= 1300)
    totphys = memstex.ullTotalPhys / 1024.0;
#else
  /* Visual Studio 6 cannot convert an unsigned __int64 type to
     double, so we need to do this for it...  */
    {
      DWORD tot_hi = memstex.ullTotalPhys >> 32;
      DWORD tot_md = (memstex.ullTotalPhys & 0x00000000ffffffff) >> 10;
      DWORD tot_lo = memstex.ullTotalPhys % 1024;

      totphys = tot_hi * 4194304.0 + tot_md + tot_lo / 1024.0;
    }
#endif	/* __GNUC__ || _MSC_VER >= 1300 */
  else if (global_memory_status (&memst))
    totphys = memst.dwTotalPhys / 1024.0;

  if (h_proc
      && get_process_memory_info (h_proc, (PROCESS_MEMORY_COUNTERS *)&mem_ex,
				  sizeof (mem_ex)))
    {
      SIZE_T rss = mem_ex.WorkingSetSize / 1024;

      attrs = Fcons (Fcons (Qmajflt,
			    make_fixnum_or_float (mem_ex.PageFaultCount)),
		     attrs);
      attrs = Fcons (Fcons (Qvsize,
			    make_fixnum_or_float (mem_ex.PrivateUsage / 1024)),
		     attrs);
      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (rss)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }
  else if (h_proc
	   && get_process_memory_info (h_proc, &mem, sizeof (mem)))
    {
      SIZE_T rss = mem_ex.WorkingSetSize / 1024;

      attrs = Fcons (Fcons (Qmajflt,
			    make_fixnum_or_float (mem.PageFaultCount)),
		     attrs);
      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (rss)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }
  else if (h_proc
	   && get_process_working_set_size (h_proc, &minrss, &maxrss))
    {
      DWORD rss = maxrss / 1024;

      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (maxrss / 1024)), attrs);
      if (totphys)
	attrs = Fcons (Fcons (Qpmem, make_float (100. * rss / totphys)), attrs);
    }

  if (process_times (h_proc, &ctime, &etime, &stime, &utime, &ttime, &pcpu))
    {
      attrs = Fcons (Fcons (Qutime, utime), attrs);
      attrs = Fcons (Fcons (Qstime, stime), attrs);
      attrs = Fcons (Fcons (Qtime,  ttime), attrs);
      attrs = Fcons (Fcons (Qstart, ctime), attrs);
      attrs = Fcons (Fcons (Qetime, etime), attrs);
      attrs = Fcons (Fcons (Qpcpu, make_float (pcpu)), attrs);
    }

  /* FIXME: Retrieve command line by walking the PEB of the process.  */

  if (h_proc)
    CloseHandle (h_proc);
  return attrs;
}

int
w32_memory_info (unsigned long long *totalram, unsigned long long *freeram,
		 unsigned long long *totalswap, unsigned long long *freeswap)
{
  MEMORYSTATUS memst;
  MEMORY_STATUS_EX memstex;

  /* Use GlobalMemoryStatusEx if available, as it can report more than
     2GB of memory.  */
  if (global_memory_status_ex (&memstex))
    {
      *totalram  = memstex.ullTotalPhys;
      *freeram   = memstex.ullAvailPhys;
      *totalswap = memstex.ullTotalPageFile;
      *freeswap  = memstex.ullAvailPageFile;
      return 0;
    }
  else if (global_memory_status (&memst))
    {
      *totalram = memst.dwTotalPhys;
      *freeram   = memst.dwAvailPhys;
      *totalswap = memst.dwTotalPageFile;
      *freeswap  = memst.dwAvailPageFile;
      return 0;
    }
  else
    return -1;
}


/* Wrappers for  winsock functions to map between our file descriptors
   and winsock's handles; also set h_errno for convenience.

   To allow Emacs to run on systems which don't have winsock support
   installed, we dynamically link to winsock on startup if present, and
   otherwise provide the minimum necessary functionality
   (eg. gethostname). */

/* function pointers for relevant socket functions */
int (PASCAL *pfn_WSAStartup) (WORD wVersionRequired, LPWSADATA lpWSAData);
void (PASCAL *pfn_WSASetLastError) (int iError);
int (PASCAL *pfn_WSAGetLastError) (void);
int (PASCAL *pfn_WSAEventSelect) (SOCKET s, HANDLE hEventObject, long lNetworkEvents);
int (PASCAL *pfn_WSAEnumNetworkEvents) (SOCKET s, HANDLE hEventObject,
					WSANETWORKEVENTS *NetworkEvents);

HANDLE (PASCAL *pfn_WSACreateEvent) (void);
int (PASCAL *pfn_WSACloseEvent) (HANDLE hEvent);
int (PASCAL *pfn_socket) (int af, int type, int protocol);
int (PASCAL *pfn_bind) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_connect) (SOCKET s, const struct sockaddr *addr, int namelen);
int (PASCAL *pfn_ioctlsocket) (SOCKET s, long cmd, u_long *argp);
int (PASCAL *pfn_recv) (SOCKET s, char * buf, int len, int flags);
int (PASCAL *pfn_send) (SOCKET s, const char * buf, int len, int flags);
int (PASCAL *pfn_closesocket) (SOCKET s);
int (PASCAL *pfn_shutdown) (SOCKET s, int how);
int (PASCAL *pfn_WSACleanup) (void);

u_short (PASCAL *pfn_htons) (u_short hostshort);
u_short (PASCAL *pfn_ntohs) (u_short netshort);
unsigned long (PASCAL *pfn_inet_addr) (const char * cp);
int (PASCAL *pfn_gethostname) (char * name, int namelen);
struct hostent * (PASCAL *pfn_gethostbyname) (const char * name);
struct servent * (PASCAL *pfn_getservbyname) (const char * name, const char * proto);
int (PASCAL *pfn_getpeername) (SOCKET s, struct sockaddr *addr, int * namelen);
int (PASCAL *pfn_setsockopt) (SOCKET s, int level, int optname,
			      const char * optval, int optlen);
int (PASCAL *pfn_listen) (SOCKET s, int backlog);
int (PASCAL *pfn_getsockname) (SOCKET s, struct sockaddr * name,
			       int * namelen);
SOCKET (PASCAL *pfn_accept) (SOCKET s, struct sockaddr * addr, int * addrlen);
int (PASCAL *pfn_recvfrom) (SOCKET s, char * buf, int len, int flags,
		       struct sockaddr * from, int * fromlen);
int (PASCAL *pfn_sendto) (SOCKET s, const char * buf, int len, int flags,
			  const struct sockaddr * to, int tolen);

int (PASCAL *pfn_getaddrinfo) (const char *, const char *,
			       const struct addrinfo *, struct addrinfo **);
void (PASCAL *pfn_freeaddrinfo) (struct addrinfo *);

/* SetHandleInformation is only needed to make sockets non-inheritable. */
BOOL (WINAPI *pfn_SetHandleInformation) (HANDLE object, DWORD mask, DWORD flags);
#ifndef HANDLE_FLAG_INHERIT
#define HANDLE_FLAG_INHERIT	1
#endif

HANDLE winsock_lib;
static int winsock_inuse;

BOOL term_winsock (void);

BOOL
term_winsock (void)
{
  if (winsock_lib != NULL && winsock_inuse == 0)
    {
      release_listen_threads ();
      /* Not sure what would cause WSAENETDOWN, or even if it can happen
	 after WSAStartup returns successfully, but it seems reasonable
	 to allow unloading winsock anyway in that case. */
      if (pfn_WSACleanup () == 0 ||
	  pfn_WSAGetLastError () == WSAENETDOWN)
	{
	  if (FreeLibrary (winsock_lib))
	  winsock_lib = NULL;
	  return TRUE;
	}
    }
  return FALSE;
}

BOOL
init_winsock (int load_now)
{
  WSADATA  winsockData;

  if (winsock_lib != NULL)
    return TRUE;

  pfn_SetHandleInformation
    = (void *) GetProcAddress (GetModuleHandle ("kernel32.dll"),
			       "SetHandleInformation");

  winsock_lib = LoadLibrary ("Ws2_32.dll");

  if (winsock_lib != NULL)
    {
      /* dynamically link to socket functions */

#define LOAD_PROC(fn) \
      if ((pfn_##fn = (void *) GetProcAddress (winsock_lib, #fn)) == NULL) \
        goto fail;

      LOAD_PROC (WSAStartup);
      LOAD_PROC (WSASetLastError);
      LOAD_PROC (WSAGetLastError);
      LOAD_PROC (WSAEventSelect);
      LOAD_PROC (WSAEnumNetworkEvents);
      LOAD_PROC (WSACreateEvent);
      LOAD_PROC (WSACloseEvent);
      LOAD_PROC (socket);
      LOAD_PROC (bind);
      LOAD_PROC (connect);
      LOAD_PROC (ioctlsocket);
      LOAD_PROC (recv);
      LOAD_PROC (send);
      LOAD_PROC (closesocket);
      LOAD_PROC (shutdown);
      LOAD_PROC (htons);
      LOAD_PROC (ntohs);
      LOAD_PROC (inet_addr);
      LOAD_PROC (gethostname);
      LOAD_PROC (gethostbyname);
      LOAD_PROC (getservbyname);
      LOAD_PROC (getpeername);
      LOAD_PROC (WSACleanup);
      LOAD_PROC (setsockopt);
      LOAD_PROC (listen);
      LOAD_PROC (getsockname);
      LOAD_PROC (accept);
      LOAD_PROC (recvfrom);
      LOAD_PROC (sendto);
#undef LOAD_PROC

      /* Try loading functions not available before XP.  */
      pfn_getaddrinfo = (void *) GetProcAddress (winsock_lib, "getaddrinfo");
      pfn_freeaddrinfo = (void *) GetProcAddress (winsock_lib, "freeaddrinfo");
      /* Paranoia: these two functions should go together, so if one
	 is absent, we cannot use the other.  */
      if (pfn_getaddrinfo == NULL)
	pfn_freeaddrinfo = NULL;
      else if (pfn_freeaddrinfo == NULL)
	pfn_getaddrinfo = NULL;

      /* specify version 1.1 of winsock */
      if (pfn_WSAStartup (0x101, &winsockData) == 0)
        {
	  if (winsockData.wVersion != 0x101)
	    goto fail;

	  if (!load_now)
	    {
	      /* Report that winsock exists and is usable, but leave
		 socket functions disabled.  I am assuming that calling
		 WSAStartup does not require any network interaction,
		 and in particular does not cause or require a dial-up
		 connection to be established. */

	      pfn_WSACleanup ();
	      FreeLibrary (winsock_lib);
	      winsock_lib = NULL;
	    }
	  winsock_inuse = 0;
	  return TRUE;
	}

    fail:
      FreeLibrary (winsock_lib);
      winsock_lib = NULL;
    }

  return FALSE;
}


int h_errno = 0;

/* Function to map winsock error codes to errno codes for those errno
   code defined in errno.h (errno values not defined by errno.h are
   already in nt/inc/sys/socket.h).  */
static void
set_errno (void)
{
  int wsa_err;

  h_errno = 0;
  if (winsock_lib == NULL)
    wsa_err = EINVAL;
  else
    wsa_err = pfn_WSAGetLastError ();

  switch (wsa_err)
    {
    case WSAEACCES:		errno = EACCES; break;
    case WSAEBADF: 		errno = EBADF; break;
    case WSAEFAULT:		errno = EFAULT; break;
    case WSAEINTR: 		errno = EINTR; break;
    case WSAEINVAL:		errno = EINVAL; break;
    case WSAEMFILE:		errno = EMFILE; break;
    case WSAENAMETOOLONG: 	errno = ENAMETOOLONG; break;
    case WSAENOTEMPTY:		errno = ENOTEMPTY; break;
    case WSAEWOULDBLOCK:	errno = EWOULDBLOCK; break;
    case WSAENOTCONN:		errno = ENOTCONN; break;
    default:			errno = wsa_err; break;
    }
}

static void
check_errno (void)
{
  h_errno = 0;
  if (winsock_lib != NULL)
    pfn_WSASetLastError (0);
}

/* Extend strerror to handle the winsock-specific error codes.  */
struct {
  int errnum;
  const char * msg;
} _wsa_errlist[] = {
  {WSAEINTR                , "Interrupted function call"},
  {WSAEBADF                , "Bad file descriptor"},
  {WSAEACCES               , "Permission denied"},
  {WSAEFAULT               , "Bad address"},
  {WSAEINVAL               , "Invalid argument"},
  {WSAEMFILE               , "Too many open files"},

  {WSAEWOULDBLOCK          , "Resource temporarily unavailable"},
  {WSAEINPROGRESS          , "Operation now in progress"},
  {WSAEALREADY             , "Operation already in progress"},
  {WSAENOTSOCK             , "Socket operation on non-socket"},
  {WSAEDESTADDRREQ         , "Destination address required"},
  {WSAEMSGSIZE             , "Message too long"},
  {WSAEPROTOTYPE           , "Protocol wrong type for socket"},
  {WSAENOPROTOOPT          , "Bad protocol option"},
  {WSAEPROTONOSUPPORT      , "Protocol not supported"},
  {WSAESOCKTNOSUPPORT      , "Socket type not supported"},
  {WSAEOPNOTSUPP           , "Operation not supported"},
  {WSAEPFNOSUPPORT         , "Protocol family not supported"},
  {WSAEAFNOSUPPORT         , "Address family not supported by protocol family"},
  {WSAEADDRINUSE           , "Address already in use"},
  {WSAEADDRNOTAVAIL        , "Cannot assign requested address"},
  {WSAENETDOWN             , "Network is down"},
  {WSAENETUNREACH          , "Network is unreachable"},
  {WSAENETRESET            , "Network dropped connection on reset"},
  {WSAECONNABORTED         , "Software caused connection abort"},
  {WSAECONNRESET           , "Connection reset by peer"},
  {WSAENOBUFS              , "No buffer space available"},
  {WSAEISCONN              , "Socket is already connected"},
  {WSAENOTCONN             , "Socket is not connected"},
  {WSAESHUTDOWN            , "Cannot send after socket shutdown"},
  {WSAETOOMANYREFS         , "Too many references"},	    /* not sure */
  {WSAETIMEDOUT            , "Connection timed out"},
  {WSAECONNREFUSED         , "Connection refused"},
  {WSAELOOP                , "Network loop"},		    /* not sure */
  {WSAENAMETOOLONG         , "Name is too long"},
  {WSAEHOSTDOWN            , "Host is down"},
  {WSAEHOSTUNREACH         , "No route to host"},
  {WSAENOTEMPTY            , "Buffer not empty"},	    /* not sure */
  {WSAEPROCLIM             , "Too many processes"},
  {WSAEUSERS               , "Too many users"},		    /* not sure */
  {WSAEDQUOT               , "Double quote in host name"},  /* really not sure */
  {WSAESTALE               , "Data is stale"},		    /* not sure */
  {WSAEREMOTE              , "Remote error"},		    /* not sure */

  {WSASYSNOTREADY          , "Network subsystem is unavailable"},
  {WSAVERNOTSUPPORTED      , "WINSOCK.DLL version out of range"},
  {WSANOTINITIALISED       , "Winsock not initialized successfully"},
  {WSAEDISCON              , "Graceful shutdown in progress"},
#ifdef WSAENOMORE
  {WSAENOMORE              , "No more operations allowed"}, /* not sure */
  {WSAECANCELLED           , "Operation cancelled"},	    /* not sure */
  {WSAEINVALIDPROCTABLE    , "Invalid procedure table from service provider"},
  {WSAEINVALIDPROVIDER     , "Invalid service provider version number"},
  {WSAEPROVIDERFAILEDINIT  , "Unable to initialize a service provider"},
  {WSASYSCALLFAILURE       , "System call failure"},
  {WSASERVICE_NOT_FOUND    , "Service not found"},	    /* not sure */
  {WSATYPE_NOT_FOUND       , "Class type not found"},
  {WSA_E_NO_MORE           , "No more resources available"}, /* really not sure */
  {WSA_E_CANCELLED         , "Operation already cancelled"}, /* really not sure */
  {WSAEREFUSED             , "Operation refused"},	    /* not sure */
#endif

  {WSAHOST_NOT_FOUND       , "Host not found"},
  {WSATRY_AGAIN            , "Authoritative host not found during name lookup"},
  {WSANO_RECOVERY          , "Non-recoverable error during name lookup"},
  {WSANO_DATA              , "Valid name, no data record of requested type"},

  {-1, NULL}
};

char *
sys_strerror (int error_no)
{
  int i;
  static char unknown_msg[40];

  if (error_no >= 0 && error_no < sys_nerr)
    return sys_errlist[error_no];

  for (i = 0; _wsa_errlist[i].errnum >= 0; i++)
    if (_wsa_errlist[i].errnum == error_no)
      return (char *)_wsa_errlist[i].msg;

  sprintf (unknown_msg, "Unidentified error: %d", error_no);
  return unknown_msg;
}

/* [andrewi 3-May-96] I've had conflicting results using both methods,
   but I believe the method of keeping the socket handle separate (and
   insuring it is not inheritable) is the correct one. */

#define SOCK_HANDLE(fd) ((SOCKET) fd_info[fd].hnd)

static int socket_to_fd (SOCKET s);

int
sys_socket (int af, int type, int protocol)
{
  SOCKET s;

  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return -1;
    }

  check_errno ();

  /* call the real socket function */
  s = pfn_socket (af, type, protocol);

  if (s != INVALID_SOCKET)
    return socket_to_fd (s);

  set_errno ();
  return -1;
}

/* Convert a SOCKET to a file descriptor.  */
static int
socket_to_fd (SOCKET s)
{
  int fd;
  child_process * cp;

  /* Although under NT 3.5 _open_osfhandle will accept a socket
     handle, if opened with SO_OPENTYPE == SO_SYNCHRONOUS_NONALERT,
     that does not work under NT 3.1.  However, we can get the same
     effect by using a backdoor function to replace an existing
     descriptor handle with the one we want. */

  /* allocate a file descriptor (with appropriate flags) */
  fd = _open ("NUL:", _O_RDWR);
  if (fd >= 0)
    {
      /* Make a non-inheritable copy of the socket handle.  Note
	 that it is possible that sockets aren't actually kernel
	 handles, which appears to be the case on Windows 9x when
	 the MS Proxy winsock client is installed.  */
      {
	/* Apparently there is a bug in NT 3.51 with some service
	   packs, which prevents using DuplicateHandle to make a
	   socket handle non-inheritable (causes WSACleanup to
	   hang).  The work-around is to use SetHandleInformation
	   instead if it is available and implemented. */
	if (pfn_SetHandleInformation)
	  {
	    pfn_SetHandleInformation ((HANDLE) s, HANDLE_FLAG_INHERIT, 0);
	  }
	else
	  {
	    HANDLE parent = GetCurrentProcess ();
	    HANDLE new_s = INVALID_HANDLE_VALUE;

	    if (DuplicateHandle (parent,
				 (HANDLE) s,
				 parent,
				 &new_s,
				 0,
				 FALSE,
				 DUPLICATE_SAME_ACCESS))
	      {
		/* It is possible that DuplicateHandle succeeds even
		   though the socket wasn't really a kernel handle,
		   because a real handle has the same value.  So
		   test whether the new handle really is a socket.  */
		unsigned long nonblocking = 0;
		if (pfn_ioctlsocket ((SOCKET) new_s, FIONBIO, &nonblocking) == 0)
		  {
		    pfn_closesocket (s);
		    s = (SOCKET) new_s;
		  }
		else
		  {
		    CloseHandle (new_s);
		  }
	      }
	  }
      }
      eassert (fd < MAXDESC);
      fd_info[fd].hnd = (HANDLE) s;

      /* set our own internal flags */
      fd_info[fd].flags = FILE_SOCKET | FILE_BINARY | FILE_READ | FILE_WRITE;

      cp = new_child ();
      if (cp)
	{
	  cp->fd = fd;
	  cp->status = STATUS_READ_ACKNOWLEDGED;

	  /* attach child_process to fd_info */
	  if (fd_info[ fd ].cp != NULL)
	    {
	      DebPrint (("sys_socket: fd_info[%d] apparently in use!\n", fd));
	      emacs_abort ();
	    }

	  fd_info[ fd ].cp = cp;

	  /* success! */
	  winsock_inuse++;	/* count open sockets */
	  return fd;
	}

      /* clean up */
      _close (fd);
    }
  else
  pfn_closesocket (s);
  errno = EMFILE;
  return -1;
}

int
sys_bind (int s, const struct sockaddr * addr, int namelen)
{
  if (winsock_lib == NULL)
    {
      errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_bind (SOCK_HANDLE (s), addr, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_connect (int s, const struct sockaddr * name, int namelen)
{
  if (winsock_lib == NULL)
    {
      errno = ENOTSOCK;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_connect (SOCK_HANDLE (s), name, namelen);
      if (rc == SOCKET_ERROR)
	{
	  set_errno ();
	  /* If this is a non-blocking 'connect', set the bit in flags
	     that will tell reader_thread to wait for connection
	     before trying to read.  */
	  if (errno == EWOULDBLOCK && (fd_info[s].flags & FILE_NDELAY) != 0)
	    {
	      errno = EINPROGRESS; /* that's what process.c expects */
	      fd_info[s].flags |= FILE_CONNECT;
	    }
	}
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

u_short
sys_htons (u_short hostshort)
{
  return (winsock_lib != NULL) ?
    pfn_htons (hostshort) : hostshort;
}

u_short
sys_ntohs (u_short netshort)
{
  return (winsock_lib != NULL) ?
    pfn_ntohs (netshort) : netshort;
}

unsigned long
sys_inet_addr (const char * cp)
{
  return (winsock_lib != NULL) ?
    pfn_inet_addr (cp) : INADDR_NONE;
}

int
sys_gethostname (char * name, int namelen)
{
  if (winsock_lib != NULL)
    {
      int retval;

      check_errno ();
      retval = pfn_gethostname (name, namelen);
      if (retval == SOCKET_ERROR)
	set_errno ();
      return retval;
    }

  if (namelen > MAX_COMPUTERNAME_LENGTH)
    return !GetComputerName (name, (DWORD *)&namelen);

  errno = EFAULT;
  return SOCKET_ERROR;
}

struct hostent *
sys_gethostbyname (const char * name)
{
  struct hostent * host;
  int h_err = h_errno;

  if (winsock_lib == NULL)
    {
      h_errno = NO_RECOVERY;
      errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  host = pfn_gethostbyname (name);
  if (!host)
    {
      set_errno ();
      h_errno = errno;
    }
  else
    h_errno = h_err;
  return host;
}

struct servent *
sys_getservbyname (const char * name, const char * proto)
{
  struct servent * serv;

  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return NULL;
    }

  check_errno ();
  serv = pfn_getservbyname (name, proto);
  if (!serv)
    set_errno ();
  return serv;
}

int
sys_getpeername (int s, struct sockaddr *addr, int * namelen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_getpeername (SOCK_HANDLE (s), addr, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_getaddrinfo (const char *node, const char *service,
		 const struct addrinfo *hints, struct addrinfo **res)
{
  int rc;

  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (pfn_getaddrinfo)
    rc = pfn_getaddrinfo (node, service, hints, res);
  else
    {
      int port = 0;
      struct hostent *host_info;
      struct gai_storage {
	struct addrinfo addrinfo;
	struct sockaddr_in sockaddr_in;
      } *gai_storage;

      /* We don't (yet) support any flags, as Emacs doesn't need that.  */
      if (hints && hints->ai_flags != 0)
	return WSAEINVAL;
      /* NODE cannot be NULL, since process.c has fallbacks for that.  */
      if (!node)
	return WSAHOST_NOT_FOUND;

      if (service)
	{
	  const char *protocol =
	    (hints && hints->ai_socktype == SOCK_DGRAM) ? "udp" : "tcp";
	  struct servent *srv = sys_getservbyname (service, protocol);

	  if (srv)
	    port = srv->s_port;
	  else if (*service >= '0' && *service <= '9')
	    {
	      char *endp;

	      port = strtoul (service, &endp, 10);
	      if (*endp || port > 65536)
		return WSAHOST_NOT_FOUND;
	      port = sys_htons ((unsigned short) port);
	    }
	  else
	    return WSAHOST_NOT_FOUND;
	}

      gai_storage = xzalloc (sizeof *gai_storage);
      gai_storage->sockaddr_in.sin_port = port;
      host_info = sys_gethostbyname (node);
      if (host_info)
	{
	  memcpy (&gai_storage->sockaddr_in.sin_addr,
		  host_info->h_addr, host_info->h_length);
	  gai_storage->sockaddr_in.sin_family = host_info->h_addrtype;
	}
      else
	{
	  /* Attempt to interpret host as numeric inet address.  */
	  unsigned long numeric_addr = sys_inet_addr (node);

	  if (numeric_addr == -1)
	    {
	      free (gai_storage);
	      return WSAHOST_NOT_FOUND;
	    }

	  memcpy (&gai_storage->sockaddr_in.sin_addr, &numeric_addr,
		  sizeof (gai_storage->sockaddr_in.sin_addr));
	  gai_storage->sockaddr_in.sin_family = (hints) ? hints->ai_family : 0;
	}

      gai_storage->addrinfo.ai_addr =
	(struct sockaddr *)&gai_storage->sockaddr_in;
      gai_storage->addrinfo.ai_addrlen = sizeof (gai_storage->sockaddr_in);
      gai_storage->addrinfo.ai_protocol = (hints) ? hints->ai_protocol : 0;
      gai_storage->addrinfo.ai_socktype = (hints) ? hints->ai_socktype : 0;
      gai_storage->addrinfo.ai_family = gai_storage->sockaddr_in.sin_family;
      gai_storage->addrinfo.ai_next = NULL;

      *res = &gai_storage->addrinfo;
      rc = 0;
    }

  return rc;
}

void
sys_freeaddrinfo (struct addrinfo *ai)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return;
    }

  check_errno ();
  if (pfn_freeaddrinfo)
    pfn_freeaddrinfo (ai);
  else
    {
      eassert (ai->ai_next == NULL);
      xfree (ai);
    }
}

int
sys_shutdown (int s, int how)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_shutdown (SOCK_HANDLE (s), how);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_setsockopt (int s, int level, int optname, const void * optval, int optlen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_setsockopt (SOCK_HANDLE (s), level, optname,
			       (const char *)optval, optlen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_listen (int s, int backlog)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_listen (SOCK_HANDLE (s), backlog);
      if (rc == SOCKET_ERROR)
	set_errno ();
      else
	fd_info[s].flags |= FILE_LISTEN;
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_getsockname (int s, struct sockaddr * name, int * namelen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_getsockname (SOCK_HANDLE (s), name, namelen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_accept (int s, struct sockaddr * addr, int * addrlen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return -1;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_LISTEN)
    {
      SOCKET t = pfn_accept (SOCK_HANDLE (s), addr, addrlen);
      int fd = -1;
      if (t == INVALID_SOCKET)
	set_errno ();
      else
	fd = socket_to_fd (t);

      if (fd >= 0)
	{
	  fd_info[s].cp->status = STATUS_READ_ACKNOWLEDGED;
	  ResetEvent (fd_info[s].cp->char_avail);
	}
      return fd;
    }
  errno = ENOTSOCK;
  return -1;
}

int
sys_recvfrom (int s, char * buf, int len, int flags,
	      struct sockaddr * from, int * fromlen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_recvfrom (SOCK_HANDLE (s), buf, len, flags, from, fromlen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

int
sys_sendto (int s, const char * buf, int len, int flags,
	    const struct sockaddr * to, int tolen)
{
  if (winsock_lib == NULL)
    {
      errno = ENETDOWN;
      return SOCKET_ERROR;
    }

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      int rc = pfn_sendto (SOCK_HANDLE (s), buf, len, flags, to, tolen);
      if (rc == SOCKET_ERROR)
	set_errno ();
      return rc;
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}

/* Windows does not have an fcntl function.  Provide an implementation
   good enough for Emacs.  */
int
fcntl (int s, int cmd, int options)
{
  /* In the w32 Emacs port, fcntl (fd, F_DUPFD_CLOEXEC, fd1) is always
     invoked in a context where fd1 is closed and all descriptors less
     than fd1 are open, so sys_dup is an adequate implementation.  */
  if (cmd == F_DUPFD_CLOEXEC)
    return sys_dup (s);

  check_errno ();
  if (fd_info[s].flags & FILE_SOCKET)
    {
      if (winsock_lib == NULL)
	{
	  errno = ENETDOWN;
	  return -1;
	}

      if (cmd == F_SETFL && options == O_NONBLOCK)
	{
	  unsigned long nblock = 1;
	  int rc = pfn_ioctlsocket (SOCK_HANDLE (s), FIONBIO, &nblock);
	  if (rc == SOCKET_ERROR)
	    set_errno ();
	  /* Keep track of the fact that we set this to non-blocking.  */
	  fd_info[s].flags |= FILE_NDELAY;
	  return rc;
	}
      else
	{
	  errno = EINVAL;
	  return SOCKET_ERROR;
	}
    }
  else if ((fd_info[s].flags & (FILE_PIPE | FILE_WRITE))
	   == (FILE_PIPE | FILE_WRITE))
    {
      /* Force our writes to pipes be non-blocking.  */
      if (cmd == F_SETFL && options == O_NONBLOCK)
	{
	  HANDLE h = (HANDLE)_get_osfhandle (s);
	  DWORD pipe_mode = PIPE_NOWAIT;

	  if (!SetNamedPipeHandleState (h, &pipe_mode, NULL, NULL))
	    {
	      DebPrint (("SetNamedPipeHandleState: %lu\n", GetLastError ()));
	      return SOCKET_ERROR;
	    }
	  fd_info[s].flags |= FILE_NDELAY;
	  return 0;
	}
      else
	{
	  errno = EINVAL;
	  return SOCKET_ERROR;
	}
    }
  errno = ENOTSOCK;
  return SOCKET_ERROR;
}


/* Shadow main io functions: we need to handle pipes and sockets more
   intelligently.  */

int
sys_close (int fd)
{
  int rc;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].cp)
    {
      child_process * cp = fd_info[fd].cp;

      fd_info[fd].cp = NULL;

      if (CHILD_ACTIVE (cp))
        {
	  /* if last descriptor to active child_process then cleanup */
	  int i;
	  for (i = 0; i < MAXDESC; i++)
	    {
	      if (i == fd)
		continue;
	      if (fd_info[i].cp == cp)
		break;
	    }
	  if (i == MAXDESC)
	    {
	      if (fd_info[fd].flags & FILE_SOCKET)
		{
		  if (winsock_lib == NULL) emacs_abort ();

		  pfn_shutdown (SOCK_HANDLE (fd), 2);
		  rc = pfn_closesocket (SOCK_HANDLE (fd));

		  winsock_inuse--; /* count open sockets */
		}
	      /* If the process handle is NULL, it's either a socket
		 or serial connection, or a subprocess that was
		 already reaped by reap_subprocess, but whose
		 resources were not yet freed, because its output was
		 not fully read yet by the time it was reaped.  (This
		 usually happens with async subprocesses whose output
		 is being read by Emacs.)  Otherwise, this process was
		 not reaped yet, so we set its FD to a negative value
		 to make sure sys_select will eventually get to
		 calling the SIGCHLD handler for it, which will then
		 invoke waitpid and reap_subprocess.  */
	      if (cp->procinfo.hProcess == NULL)
		delete_child (cp);
	      else
		cp->fd = -1;
	    }
	}
    }

  if (fd >= 0 && fd < MAXDESC)
    fd_info[fd].flags = 0;

  /* Note that sockets do not need special treatment here (at least on
     NT and Windows 95 using the standard tcp/ip stacks) - it appears that
     closesocket is equivalent to CloseHandle, which is to be expected
     because socket handles are fully fledged kernel handles. */
  rc = _close (fd);

  return rc;
}

int
sys_dup (int fd)
{
  int new_fd;

  new_fd = _dup (fd);
  if (new_fd >= 0 && new_fd < MAXDESC)
    {
      /* duplicate our internal info as well */
      fd_info[new_fd] = fd_info[fd];
    }
  return new_fd;
}

int
sys_dup2 (int src, int dst)
{
  int rc;

  if (dst < 0 || dst >= MAXDESC)
    {
      errno = EBADF;
      return -1;
    }

  /* MS _dup2 seems to have weird side effect when invoked with 2
     identical arguments: an attempt to fclose the corresponding stdio
     stream after that hangs (we do close standard streams in
     init_ntproc).  Attempt to avoid that by not calling _dup2 that
     way: if SRC is valid, we know that dup2 should be a no-op, so do
     nothing and return DST.  */
  if (src == dst)
    {
      if ((HANDLE)_get_osfhandle (src) == INVALID_HANDLE_VALUE)
	{
	  errno = EBADF;
	  return -1;
	}
      return dst;
    }

  /* Make sure we close the destination first if it's a pipe or socket.  */
  if (fd_info[dst].flags != 0)
    sys_close (dst);

  rc = _dup2 (src, dst);
  if (rc == 0)
    {
      /* Duplicate our internal info as well.  */
      fd_info[dst] = fd_info[src];
    }
  return rc == 0 ? dst : rc;
}

int
pipe2 (int * phandles, int pipe2_flags)
{
  int rc;
  unsigned flags;
  unsigned pipe_size = 0;

  eassert (pipe2_flags == (O_BINARY | O_CLOEXEC));

  /* Allow Lisp to override the default buffer size of the pipe.  */
  if (w32_pipe_buffer_size > 0 && w32_pipe_buffer_size < UINT_MAX)
    pipe_size = w32_pipe_buffer_size;

  /* make pipe handles non-inheritable; when we spawn a child, we
     replace the relevant handle with an inheritable one.  Also put
     pipes into binary mode; we will do text mode translation ourselves
     if required.  */
  rc = _pipe (phandles, pipe_size, _O_NOINHERIT | _O_BINARY);

  if (rc == 0)
    {
      /* Protect against overflow, since Windows can open more handles than
	 our fd_info array has room for.  */
      if (phandles[0] >= MAXDESC || phandles[1] >= MAXDESC)
	{
	  _close (phandles[0]);
	  _close (phandles[1]);
	  errno = EMFILE;
	  rc = -1;
	}
      else
	{
	  flags = FILE_PIPE | FILE_READ | FILE_BINARY;
	  fd_info[phandles[0]].flags = flags;

	  flags = FILE_PIPE | FILE_WRITE | FILE_BINARY;
	  fd_info[phandles[1]].flags = flags;
	}
    }

  return rc;
}

/* Function to do blocking read of one byte, needed to implement
   select.  It is only allowed on communication ports, sockets, or
   pipes. */
int
_sys_read_ahead (int fd)
{
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  if ((fd_info[fd].flags & (FILE_PIPE | FILE_SERIAL | FILE_SOCKET)) == 0
      || (fd_info[fd].flags & FILE_READ) == 0)
    {
      DebPrint (("_sys_read_ahead: internal error: fd %d is not a pipe, serial port, or socket!\n", fd));
      emacs_abort ();
    }

  if ((fd_info[fd].flags & FILE_CONNECT) != 0)
    DebPrint (("_sys_read_ahead: read requested from fd %d, which waits for async connect!\n", fd));
  cp->status = STATUS_READ_IN_PROGRESS;

  if (fd_info[fd].flags & FILE_PIPE)
    {
      rc = _read (fd, &cp->chr, sizeof (char));

      /* Give subprocess time to buffer some more output for us before
	 reporting that input is available; we need this because Windows 95
	 connects DOS programs to pipes by making the pipe appear to be
	 the normal console stdout - as a result most DOS programs will
	 write to stdout without buffering, ie. one character at a
	 time.  Even some W32 programs do this - "dir" in a command
	 shell on NT is very slow if we don't do this. */
      if (rc > 0)
	{
	  int wait = w32_pipe_read_delay;

	  if (wait > 0)
	    Sleep (wait);
	  else if (wait < 0)
	    while (++wait <= 0)
	      /* Yield remainder of our time slice, effectively giving a
		 temporary priority boost to the child process. */
	      Sleep (0);
	}
    }
  else if (fd_info[fd].flags & FILE_SERIAL)
    {
      HANDLE hnd = fd_info[fd].hnd;
      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_read;
      COMMTIMEOUTS ct;

      /* Configure timeouts for blocking read.  */
      if (!GetCommTimeouts (hnd, &ct))
	{
	  cp->status = STATUS_READ_ERROR;
	  return STATUS_READ_ERROR;
	}
      ct.ReadIntervalTimeout		= 0;
      ct.ReadTotalTimeoutMultiplier	= 0;
      ct.ReadTotalTimeoutConstant	= 0;
      if (!SetCommTimeouts (hnd, &ct))
	{
	  cp->status = STATUS_READ_ERROR;
	  return STATUS_READ_ERROR;
	}

      if (!ReadFile (hnd, &cp->chr, sizeof (char), (DWORD*) &rc, ovl))
	{
	  if (GetLastError () != ERROR_IO_PENDING)
	    {
	      cp->status = STATUS_READ_ERROR;
	      return STATUS_READ_ERROR;
	    }
	  if (!GetOverlappedResult (hnd, ovl, (DWORD*) &rc, TRUE))
	    {
	      cp->status = STATUS_READ_ERROR;
	      return STATUS_READ_ERROR;
	    }
	}
    }
  else if (fd_info[fd].flags & FILE_SOCKET)
    {
      unsigned long nblock = 0;
      /* We always want this to block, so temporarily disable NDELAY.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);

      rc = pfn_recv (SOCK_HANDLE (fd), &cp->chr, sizeof (char), 0);

      if (fd_info[fd].flags & FILE_NDELAY)
	{
	  nblock = 1;
	  pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);
	}
    }

  if (rc == sizeof (char))
    cp->status = STATUS_READ_SUCCEEDED;
  else
    cp->status = STATUS_READ_FAILED;

  return cp->status;
}

int
_sys_wait_accept (int fd)
{
  HANDLE hEv;
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  cp->status = STATUS_READ_FAILED;

  hEv = pfn_WSACreateEvent ();
  rc = pfn_WSAEventSelect (SOCK_HANDLE (fd), hEv, FD_ACCEPT);
  if (rc != SOCKET_ERROR)
    {
      do {
	rc = WaitForSingleObject (hEv, 500);
	Sleep (5);
      } while (rc == WAIT_TIMEOUT
	       && cp->status != STATUS_READ_ERROR
	       && cp->char_avail);
      pfn_WSAEventSelect (SOCK_HANDLE (fd), NULL, 0);
      if (rc == WAIT_OBJECT_0)
	cp->status = STATUS_READ_SUCCEEDED;
    }
  pfn_WSACloseEvent (hEv);

  return cp->status;
}

int
_sys_wait_connect (int fd)
{
  HANDLE hEv;
  child_process * cp;
  int rc;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;
  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  cp->status = STATUS_READ_FAILED;

  hEv = pfn_WSACreateEvent ();
  rc = pfn_WSAEventSelect (SOCK_HANDLE (fd), hEv, FD_CONNECT);
  if (rc != SOCKET_ERROR)
    {
      do {
	rc = WaitForSingleObject (hEv, 500);
	Sleep (5);
      } while (rc == WAIT_TIMEOUT
	       && cp->status != STATUS_READ_ERROR
	       && cp->char_avail);
      if (rc == WAIT_OBJECT_0)
	{
	  /* We've got an event, but it could be a successful
	     connection, or it could be a failure.  Find out
	     which one is it.  */
	  WSANETWORKEVENTS events;

	  pfn_WSAEnumNetworkEvents (SOCK_HANDLE (fd), hEv, &events);
	  if ((events.lNetworkEvents & FD_CONNECT) != 0
	      && events.iErrorCode[FD_CONNECT_BIT])
	    {
	      cp->status = STATUS_CONNECT_FAILED;
	      cp->errcode = events.iErrorCode[FD_CONNECT_BIT];
	    }
	  else
	    {
	      cp->status = STATUS_READ_SUCCEEDED;
	      cp->errcode = 0;
	    }
	}
      pfn_WSAEventSelect (SOCK_HANDLE (fd), NULL, 0);
    }
  else
    pfn_WSACloseEvent (hEv);

  return cp->status;
}

int
sys_read (int fd, char * buffer, unsigned int count)
{
  int nchars;
  int to_read;
  DWORD waiting;
  char * orig_buffer = buffer;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET | FILE_SERIAL))
    {
      child_process *cp = fd_info[fd].cp;

      if ((fd_info[fd].flags & FILE_READ) == 0)
        {
	  errno = EBADF;
	  return -1;
	}

      nchars = 0;

      /* re-read CR carried over from last read */
      if (fd_info[fd].flags & FILE_LAST_CR)
	{
	  if (fd_info[fd].flags & FILE_BINARY) emacs_abort ();
	  *buffer++ = 0x0d;
	  count--;
	  nchars++;
	  fd_info[fd].flags &= ~FILE_LAST_CR;
	}

      /* presence of a child_process structure means we are operating in
	 non-blocking mode - otherwise we just call _read directly.
	 Note that the child_process structure might be missing because
	 reap_subprocess has been called; in this case the pipe is
	 already broken, so calling _read on it is okay. */
      if (cp)
        {
	  int current_status = cp->status;

	  switch (current_status)
	    {
	    case STATUS_READ_FAILED:
	    case STATUS_READ_ERROR:
	      /* report normal EOF if nothing in buffer */
	      if (nchars <= 0)
		fd_info[fd].flags |= FILE_AT_EOF;
	      return nchars;

	    case STATUS_READ_READY:
	    case STATUS_READ_IN_PROGRESS:
#if 0
	      /* This happens all the time during GnuTLS handshake
		 with the remote, evidently because GnuTLS waits for
		 the read to complete by retrying the read operation
		 upon EAGAIN.  So I'm disabling the DebPrint to avoid
		 wasting cycles on something that is not a real
		 problem.  Enable if you need to debug something that
		 bumps into this.  */
	      DebPrint (("sys_read called when read is in progress %d\n",
			 current_status));
#endif
	      errno = EWOULDBLOCK;
	      return -1;

	    case STATUS_READ_SUCCEEDED:
	      /* consume read-ahead char */
	      *buffer++ = cp->chr;
	      count--;
	      nchars++;
	      cp->status = STATUS_READ_ACKNOWLEDGED;
	      ResetEvent (cp->char_avail);

	    case STATUS_READ_ACKNOWLEDGED:
	    case STATUS_CONNECT_FAILED:
	      break;

	    default:
	      DebPrint (("sys_read: bad status %d\n", current_status));
	      errno = EBADF;
	      return -1;
	    }

	  if (fd_info[fd].flags & FILE_PIPE)
	    {
	      PeekNamedPipe ((HANDLE) _get_osfhandle (fd), NULL, 0, NULL, &waiting, NULL);
	      to_read = min (waiting, (DWORD) count);

	      if (to_read > 0)
		nchars += _read (fd, buffer, to_read);
	    }
	  else if (fd_info[fd].flags & FILE_SERIAL)
	    {
	      HANDLE hnd = fd_info[fd].hnd;
	      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_read;
	      int rc = 0;
	      COMMTIMEOUTS ct;

	      if (count > 0)
		{
		  /* Configure timeouts for non-blocking read.  */
		  if (!GetCommTimeouts (hnd, &ct))
		    {
		      errno = EIO;
		      return -1;
		    }
		  ct.ReadIntervalTimeout	 = MAXDWORD;
		  ct.ReadTotalTimeoutMultiplier	 = 0;
		  ct.ReadTotalTimeoutConstant	 = 0;
		  if (!SetCommTimeouts (hnd, &ct))
		    {
		      errno = EIO;
		      return -1;
		    }

		  if (!ResetEvent (ovl->hEvent))
		    {
		      errno = EIO;
		      return -1;
		    }
		  if (!ReadFile (hnd, buffer, count, (DWORD*) &rc, ovl))
		    {
		      if (GetLastError () != ERROR_IO_PENDING)
			{
			  errno = EIO;
			  return -1;
			}
		      if (!GetOverlappedResult (hnd, ovl, (DWORD*) &rc, TRUE))
			{
			  errno = EIO;
			  return -1;
			}
		    }
		  nchars += rc;
		}
	    }
	  else /* FILE_SOCKET */
	    {
	      if (winsock_lib == NULL) emacs_abort ();

	      /* When a non-blocking 'connect' call fails,
		 wait_reading_process_output detects this by calling
		 'getpeername', and then attempts to obtain the connection
		 error code by trying to read 1 byte from the socket.  If
		 we try to serve that read by calling 'recv' below, the
		 error we get is a generic WSAENOTCONN, not the actual
		 connection error.  So instead, we use the actual error
		 code stashed by '_sys_wait_connect' in cp->errcode.
		 Alternatively, we could have used 'getsockopt', like on
		 GNU/Linux, but: (a) I have no idea whether the winsock
		 version could hang, as it does "on some systems" (see the
		 comment in process.c); and (b) 'getsockopt' on Windows is
		 documented to clear the socket error for the entire
		 process, which I'm not sure is TRT; FIXME.  */
	      if (current_status == STATUS_CONNECT_FAILED
		  && (fd_info[fd].flags & FILE_CONNECT) != 0
		  && cp->errcode != 0)
		{
		  pfn_WSASetLastError (cp->errcode);
		  set_errno ();
		  return -1;
		}
	      /* Do the equivalent of a non-blocking read.  */
	      pfn_ioctlsocket (SOCK_HANDLE (fd), FIONREAD, &waiting);
	      if (waiting == 0 && nchars == 0)
	        {
		  errno = EWOULDBLOCK;
		  return -1;
		}

	      if (waiting)
	        {
		  /* always use binary mode for sockets */
		  int res = pfn_recv (SOCK_HANDLE (fd), buffer, count, 0);
		  if (res == SOCKET_ERROR)
		    {
		      set_errno ();
		      DebPrint (("sys_read.recv failed with error %d on socket %ld\n",
				 errno, SOCK_HANDLE (fd)));
		      return -1;
		    }
		  nchars += res;
		}
	    }
	}
      else
	{
	  int nread = _read (fd, buffer, count);
	  if (nread >= 0)
	    nchars += nread;
	  else if (nchars == 0)
	    nchars = nread;
	}

      if (nchars <= 0)
	fd_info[fd].flags |= FILE_AT_EOF;
      /* Perform text mode translation if required.  */
      else if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  nchars = crlf_to_lf (nchars, orig_buffer);
	  /* If buffer contains only CR, return that.  To be absolutely
	     sure we should attempt to read the next char, but in
	     practice a CR to be followed by LF would not appear by
	     itself in the buffer.  */
	  if (nchars > 1 && orig_buffer[nchars - 1] == 0x0d)
	    {
	      fd_info[fd].flags |= FILE_LAST_CR;
	      nchars--;
	    }
	}
    }
  else
    nchars = _read (fd, buffer, count);

  return nchars;
}

/* From w32xfns.c */
extern HANDLE interrupt_handle;

int
sys_write (int fd, const void * buffer, unsigned int count)
{
  int nchars;
  USE_SAFE_ALLOCA;

  if (fd < 0)
    {
      errno = EBADF;
      return -1;
    }

  if (fd < MAXDESC && fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET | FILE_SERIAL))
    {
      if ((fd_info[fd].flags & FILE_WRITE) == 0)
	{
	  errno = EBADF;
	  return -1;
	}

      /* Perform text mode translation if required.  */
      if ((fd_info[fd].flags & FILE_BINARY) == 0)
	{
	  char * tmpbuf;
	  const unsigned char * src = buffer;
	  unsigned char * dst;
	  int nbytes = count;

	  SAFE_NALLOCA (tmpbuf, 2, count);
	  dst = (unsigned char *)tmpbuf;

	  while (1)
	    {
	      unsigned char *next;
	      /* Copy next line or remaining bytes.  */
	      next = _memccpy (dst, src, '\n', nbytes);
	      if (next)
		{
		  /* Copied one line ending with '\n'.  */
		  int copied = next - dst;
		  nbytes -= copied;
		  src += copied;
		  /* Insert '\r' before '\n'.  */
		  next[-1] = '\r';
		  next[0] = '\n';
		  dst = next + 1;
		  count++;
		}
	      else
		/* Copied remaining partial line -> now finished.  */
		break;
	    }
	  buffer = tmpbuf;
	}
    }

  if (fd < MAXDESC && fd_info[fd].flags & FILE_SERIAL)
    {
      HANDLE hnd = (HANDLE) _get_osfhandle (fd);
      OVERLAPPED *ovl = &fd_info[fd].cp->ovl_write;
      HANDLE wait_hnd[2] = { interrupt_handle, ovl->hEvent };
      DWORD active = 0;

      /* This is async (a.k.a. "overlapped") I/O, so the return value
	 of FALSE from WriteFile means either an error or the output
	 will be completed asynchronously (ERROR_IO_PENDING).  */
      if (!WriteFile (hnd, buffer, count, (DWORD*) &nchars, ovl))
	{
	  if (GetLastError () != ERROR_IO_PENDING)
	    {
	      errno = EIO;
	      nchars = -1;
	    }
	  else
	    {
	      /* Wait for the write to complete, and watch C-g while
		 at that.  */
	      if (detect_input_pending ())
		active = MsgWaitForMultipleObjects (2, wait_hnd, FALSE,
						    INFINITE, QS_ALLINPUT);
	      else
		active = WaitForMultipleObjects (2, wait_hnd, FALSE, INFINITE);
	      switch (active)
		{
		case WAIT_OBJECT_0:
		  /* User pressed C-g, cancel write, then leave.
		     Don't bother cleaning up as we may only get stuck
		     in buggy drivers.  */
		  PurgeComm (hnd, PURGE_TXABORT | PURGE_TXCLEAR);
		  CancelIo (hnd);
		  errno = EIO;	/* Why not EINTR? */
		  nchars = -1;
		  break;
		case WAIT_OBJECT_0 + 1:
		  if (!GetOverlappedResult (hnd, ovl, (DWORD*) &nchars, TRUE))
		    {
		      errno = EIO;
		      nchars = -1;
		    }
		  break;
		}
	    }
	}
    }
  else if (fd < MAXDESC && fd_info[fd].flags & FILE_SOCKET)
    {
      unsigned long nblock = 0;
      if (winsock_lib == NULL) emacs_abort ();

      child_process *cp = fd_info[fd].cp;

      /* If this is a non-blocking socket whose connection is in
	 progress or terminated with an error already, return the
	 proper error code to the caller. */
      if (cp != NULL && (fd_info[fd].flags & FILE_CONNECT) != 0)
	{
	  /* In case connection is in progress, ENOTCONN that would
	     result from calling pfn_send is not what callers expect. */
	  if (cp->status != STATUS_CONNECT_FAILED)
	    {
	      errno = EWOULDBLOCK;
	      return -1;
	    }
	  /* In case connection failed, use the actual error code
	     stashed by '_sys_wait_connect' in cp->errcode. */
	  else if (cp->errcode != 0)
	    {
	      pfn_WSASetLastError (cp->errcode);
	      set_errno ();
	      return -1;
	    }
	}

      /* TODO: implement select() properly so non-blocking I/O works. */
      /* For now, make sure the write blocks.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);

      nchars =  pfn_send (SOCK_HANDLE (fd), buffer, count, 0);

      if (nchars == SOCKET_ERROR)
        {
	  set_errno ();
	  DebPrint (("sys_write.send failed with error %d on socket %ld\n",
		     pfn_WSAGetLastError (), SOCK_HANDLE (fd)));
	}

      /* Set the socket back to non-blocking if it was before,
	 for other operations that support it.  */
      if (fd_info[fd].flags & FILE_NDELAY)
	{
	  nblock = 1;
	  pfn_ioctlsocket (SOCK_HANDLE (fd), FIONBIO, &nblock);
	}
    }
  else
    {
      /* Some networked filesystems don't like too large writes, so
	 break them into smaller chunks.  See the Comments section of
	 the MSDN documentation of WriteFile for details behind the
	 choice of the value of CHUNK below.  See also the thread
	 http://thread.gmane.org/gmane.comp.version-control.git/145294
	 in the git mailing list.  */
      const unsigned char *p = buffer;
      const bool is_pipe = (fd < MAXDESC
			    && ((fd_info[fd].flags & (FILE_PIPE | FILE_NDELAY))
				== (FILE_PIPE | FILE_NDELAY)));
      /* Some programs, notably Node.js's node.exe, seem to never
	 completely empty the pipe, so writing more than the size of
	 the pipe's buffer always returns ENOSPC, and we loop forever
	 between send_process and here.  As a workaround, write no
	 more than the pipe's buffer can hold.  */
      DWORD pipe_buffer_size;
      if (is_pipe)
	{
	  if (!GetNamedPipeInfo ((HANDLE)_get_osfhandle (fd),
				NULL, &pipe_buffer_size, NULL, NULL))
	    {
	      DebPrint (("GetNamedPipeInfo: error %u\n", GetLastError ()));
	      pipe_buffer_size = 4096;
	    }
	}
      const unsigned chunk = is_pipe ? pipe_buffer_size : 30 * 1024 * 1024;

      nchars = 0;
      errno = 0;
      while (count > 0)
	{
	  unsigned this_chunk = count < chunk ? count : chunk;
	  int n = _write (fd, p, this_chunk);

	  if (n > 0)
	    nchars += n;
	  if (n < 0)
	    {
	      /* When there's no buffer space in a pipe that is in the
		 non-blocking mode, _write returns ENOSPC.  We return
		 EAGAIN instead, which should trigger the logic in
		 send_process that enters waiting loop and calls
		 wait_reading_process_output to allow process input to
		 be accepted during the wait.  Those calls to
		 wait_reading_process_output allow sys_select to
		 notice when process input becomes available, thus
		 avoiding deadlock whereby each side of the pipe is
		 blocked on write, waiting for the other party to read
		 its end of the pipe.  */
	      if (errno == ENOSPC && is_pipe)
		errno = EAGAIN;
	      if (nchars == 0)
		nchars = -1;
	      break;
	    }
	  else if (n < this_chunk)
	    break;
	  count -= n;
	  p += n;
	}
    }

  SAFE_FREE ();
  return nchars;
}


/* Emulation of SIOCGIFCONF and getifaddrs, see process.c.  */

/* Return information about network interface IFNAME, or about all
   interfaces (if IFNAME is nil).  */
static Lisp_Object
network_interface_get_info (Lisp_Object ifname)
{
  ULONG ainfo_len = sizeof (IP_ADAPTER_INFO);
  IP_ADAPTER_INFO *adapter, *ainfo = xmalloc (ainfo_len);
  DWORD retval = get_adapters_info (ainfo, &ainfo_len);
  Lisp_Object res = Qnil;

  if (retval == ERROR_BUFFER_OVERFLOW)
    {
      ainfo = xrealloc (ainfo, ainfo_len);
      retval = get_adapters_info (ainfo, &ainfo_len);
    }

  if (retval == ERROR_SUCCESS)
    {
      int eth_count = 0, tr_count = 0, fddi_count = 0, ppp_count = 0;
      int sl_count = 0, wlan_count = 0, lo_count = 0, ifx_count = 0;
      int if_num;
      struct sockaddr_in sa;

      /* For the below, we need some winsock functions, so make sure
	 the winsock DLL is loaded.  If we cannot successfully load
	 it, they will have no use of the information we provide,
	 anyway, so punt.  */
      if (!winsock_lib && !init_winsock (1))
	goto done;

      for (adapter = ainfo; adapter; adapter = adapter->Next)
	{
	  char namebuf[MAX_ADAPTER_NAME_LENGTH + 4];
	  u_long ip_addr;
	  /* Present Unix-compatible interface names, instead of the
	     Windows names, which are really GUIDs not readable by
	     humans.  */
	  static const char *ifmt[] = {
	    "eth%d", "tr%d", "fddi%d", "ppp%d", "sl%d", "wlan%d",
	    "lo", "ifx%d"
	  };
	  enum {
	    NONE = -1,
	    ETHERNET = 0,
	    TOKENRING = 1,
	    FDDI = 2,
	    PPP = 3,
	    SLIP = 4,
	    WLAN = 5,
	    LOOPBACK = 6,
	    OTHER_IF = 7
	  } ifmt_idx;

	  switch (adapter->Type)
	    {
	    case MIB_IF_TYPE_ETHERNET:
	      /* Windows before Vista reports wireless adapters as
		 Ethernet.  Work around by looking at the Description
		 string.  */
	      if (strstr (adapter->Description, "Wireless "))
		{
		  ifmt_idx = WLAN;
		  if_num = wlan_count++;
		}
	      else
		{
		  ifmt_idx = ETHERNET;
		  if_num = eth_count++;
		}
	      break;
	    case MIB_IF_TYPE_TOKENRING:
	      ifmt_idx = TOKENRING;
	      if_num = tr_count++;
	      break;
	    case MIB_IF_TYPE_FDDI:
	      ifmt_idx = FDDI;
	      if_num = fddi_count++;
	      break;
	    case MIB_IF_TYPE_PPP:
	      ifmt_idx = PPP;
	      if_num = ppp_count++;
	      break;
	    case MIB_IF_TYPE_SLIP:
	      ifmt_idx = SLIP;
	      if_num = sl_count++;
	      break;
	    case IF_TYPE_IEEE80211:
	      ifmt_idx = WLAN;
	      if_num = wlan_count++;
	      break;
	    case MIB_IF_TYPE_LOOPBACK:
	      if (lo_count < 0)
		{
		  ifmt_idx = LOOPBACK;
		  if_num = lo_count++;
		}
	      else
		ifmt_idx = NONE;
	      break;
	    default:
	      ifmt_idx = OTHER_IF;
	      if_num = ifx_count++;
	      break;
	    }
	  if (ifmt_idx == NONE)
	    continue;
	  sprintf (namebuf, ifmt[ifmt_idx], if_num);

	  sa.sin_family = AF_INET;
	  ip_addr = sys_inet_addr (adapter->IpAddressList.IpAddress.String);
	  if (ip_addr == INADDR_NONE)
	    {
	      /* Bogus address, skip this interface.  */
	      continue;
	    }
	  sa.sin_addr.s_addr = ip_addr;
	  sa.sin_port = 0;
	  if (NILP (ifname))
	    res = Fcons (Fcons (build_string (namebuf),
				conv_sockaddr_to_lisp ((struct sockaddr*) &sa,
						       sizeof (struct sockaddr))),
			 res);
	  else if (strcmp (namebuf, SSDATA (ifname)) == 0)
	    {
	      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
	      register struct Lisp_Vector *p = XVECTOR (hwaddr);
	      Lisp_Object flags = Qnil;
	      int n;
	      u_long net_mask;

	      /* Flags.  We guess most of them by type, since the
		 Windows flags are different and hard to get by.  */
	      flags = Fcons (intern ("up"), flags);
	      if (ifmt_idx == ETHERNET || ifmt_idx == WLAN)
		{
		  flags = Fcons (intern ("broadcast"), flags);
		  flags = Fcons (intern ("multicast"), flags);
		}
	      flags = Fcons (intern ("running"), flags);
	      if (ifmt_idx == PPP)
		{
		  flags = Fcons (intern ("pointopoint"), flags);
		  flags = Fcons (intern ("noarp"), flags);
		}
	      if (adapter->HaveWins)
		flags = Fcons (intern ("WINS"), flags);
	      if (adapter->DhcpEnabled)
		flags = Fcons (intern ("dynamic"), flags);

	      res = Fcons (flags, res);

	      /* Hardware address and its family.  */
	      for (n = 0; n < adapter->AddressLength; n++)
		p->contents[n] = make_number ((int) adapter->Address[n]);
	      /* Windows does not support AF_LINK or AF_PACKET family
		 of addresses.  Use an arbitrary family number that is
		 identical to what GNU/Linux returns.  */
	      res = Fcons (Fcons (make_number (1), hwaddr), res);

	      /* Network mask.  */
	      sa.sin_family = AF_INET;
	      net_mask = sys_inet_addr (adapter->IpAddressList.IpMask.String);
	      if (net_mask != INADDR_NONE)
		{
		  sa.sin_addr.s_addr = net_mask;
		  sa.sin_port = 0;
		  res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						      sizeof (struct sockaddr)),
			       res);
		}
	      else
		res = Fcons (Qnil, res);

	      sa.sin_family = AF_INET;
	      if (ip_addr != INADDR_NONE)
		{
		  /* Broadcast address is only reported by
		     GetAdaptersAddresses, which is of limited
		     availability.  Generate it on our own.  */
		  u_long bcast_addr = (ip_addr & net_mask) | ~net_mask;

		  sa.sin_addr.s_addr = bcast_addr;
		  sa.sin_port = 0;
		  res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						      sizeof (struct sockaddr)),
			       res);

		  /* IP address.  */
		  sa.sin_addr.s_addr = ip_addr;
		  sa.sin_port = 0;
		  res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						      sizeof (struct sockaddr)),
			       res);
		}
	      else
		res = Fcons (Qnil, Fcons (Qnil, res));
	    }
	}
      /* GetAdaptersInfo is documented to not report loopback
	 interfaces, so we generate one out of thin air.  */
      if (!lo_count)
	{
	  sa.sin_family = AF_INET;
	  sa.sin_port = 0;
	  if (NILP (ifname))
	    {
	      sa.sin_addr.s_addr = sys_inet_addr ("127.0.0.1");
	      res = Fcons (Fcons (build_string ("lo"),
				  conv_sockaddr_to_lisp ((struct sockaddr*) &sa,
							 sizeof (struct sockaddr))),
			   res);
	    }
	  else if (strcmp (SSDATA (ifname), "lo") == 0)
	    {
	      res = Fcons (Fcons (intern ("running"),
				  Fcons (intern ("loopback"),
					 Fcons (intern ("up"), Qnil))), Qnil);
	      /* 772 is what 3 different GNU/Linux systems report for
		 the loopback interface.  */
	      res = Fcons (Fcons (make_number (772),
				  Fmake_vector (make_number (6),
						make_number (0))),
			   res);
	      sa.sin_addr.s_addr = sys_inet_addr ("255.0.0.0");
	      res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						  sizeof (struct sockaddr)),
			   res);
	      sa.sin_addr.s_addr = sys_inet_addr ("0.0.0.0");
	      res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						  sizeof (struct sockaddr)),
			   res);
	      sa.sin_addr.s_addr = sys_inet_addr ("127.0.0.1");
	      res = Fcons (conv_sockaddr_to_lisp ((struct sockaddr *) &sa,
						  sizeof (struct sockaddr)),
			   res);
	    }

	}
    }

 done:
  xfree (ainfo);
  return res;
}

Lisp_Object
network_interface_list (void)
{
  return network_interface_get_info (Qnil);
}

Lisp_Object
network_interface_info (Lisp_Object ifname)
{
  CHECK_STRING (ifname);
  return network_interface_get_info (ifname);
}


/* The Windows CRT functions are "optimized for speed", so they don't
   check for timezone and DST changes if they were last called less
   than 1 minute ago (see http://support.microsoft.com/kb/821231).  So
   all Emacs features that repeatedly call time functions (e.g.,
   display-time) are in real danger of missing timezone and DST
   changes.  Calling tzset before each localtime call fixes that.  */
struct tm *
sys_localtime (const time_t *t)
{
  tzset ();
  return localtime (t);
}



/* Try loading LIBRARY_ID from the file(s) specified in
   Vdynamic_library_alist.  If the library is loaded successfully,
   return the handle of the DLL, and record the filename in the
   property :loaded-from of LIBRARY_ID.  If the library could not be
   found, or when it was already loaded (because the handle is not
   recorded anywhere, and so is lost after use), return NULL.

   We could also save the handle in :loaded-from, but currently
   there's no use case for it.  */
HMODULE
w32_delayed_load (Lisp_Object library_id)
{
  HMODULE dll_handle = NULL;

  CHECK_SYMBOL (library_id);

  if (CONSP (Vdynamic_library_alist)
      && NILP (Fassq (library_id, Vlibrary_cache)))
    {
      Lisp_Object found = Qnil;
      Lisp_Object dlls = Fassq (library_id, Vdynamic_library_alist);

      if (CONSP (dlls))
        for (dlls = XCDR (dlls); CONSP (dlls); dlls = XCDR (dlls))
          {
	    Lisp_Object dll = XCAR (dlls);
	    char name[MAX_UTF8_PATH];
	    DWORD res = -1;

	    CHECK_STRING (dll);
	    dll = ENCODE_FILE (dll);
	    if (w32_unicode_filenames)
	      {
		wchar_t name_w[MAX_PATH];

		filename_to_utf16 (SSDATA (dll), name_w);
		dll_handle = LoadLibraryW (name_w);
		if (dll_handle)
		  {
		    res = GetModuleFileNameW (dll_handle, name_w,
					      sizeof (name_w));
		    if (res > 0)
		      filename_from_utf16 (name_w, name);
		  }
	      }
	    else
	      {
		char name_a[MAX_PATH];

		filename_to_ansi (SSDATA (dll), name_a);
		dll_handle = LoadLibraryA (name_a);
		if (dll_handle)
		  {
		    res = GetModuleFileNameA (dll_handle, name_a,
					      sizeof (name_a));
		    if (res > 0)
		      filename_from_ansi (name_a, name);
		  }
	      }
	    if (dll_handle)
	      {
		ptrdiff_t len = strlen (name);
		found = Fcons (dll,
			       (res > 0)
			       /* Possibly truncated */
			       ? make_specified_string (name, -1, len, 1)
			       : Qnil);
		/* This prevents thread start and end notifications
		   from being sent to the DLL, for every thread we
		   start.  We don't need those notifications because
		   threads we create never use any of these DLLs, only
		   the main thread uses them.  This is supposed to
		   speed up thread creation.  */
		DisableThreadLibraryCalls (dll_handle);
		break;
	      }
	  }

      Fput (library_id, QCloaded_from, found);
    }

  return dll_handle;
}


void
check_windows_init_file (void)
{
  /* A common indication that Emacs is not installed properly is when
     it cannot find the Windows installation file.  If this file does
     not exist in the expected place, tell the user.  */

  if (!noninteractive && !inhibit_window_system
      /* Vload_path is not yet initialized when we are loading
	 loadup.el.  */
      && NILP (Vpurify_flag))
    {
      Lisp_Object init_file;
      int fd;

      /* Implementation note: this function runs early during Emacs
	 startup, before startup.el is run.  So Vload_path is still in
	 its initial unibyte form, but it holds UTF-8 encoded file
	 names, since init_callproc was already called.  So we do not
	 need to ENCODE_FILE here, but we do need to convert the file
	 names from UTF-8 to ANSI.  */
      init_file = build_string ("term/w32-win");
      fd = openp (Vload_path, init_file, Fget_load_suffixes (), NULL, Qnil, 0);
      if (fd < 0)
	{
	  Lisp_Object load_path_print = Fprin1_to_string (Vload_path, Qnil);
	  char *init_file_name = SSDATA (init_file);
	  char *load_path = SSDATA (load_path_print);
	  char *buffer = alloca (1024
				 + strlen (init_file_name)
				 + strlen (load_path));
	  char *msg = buffer;
	  int needed;

	  sprintf (buffer,
		   "The Emacs Windows initialization file \"%s.el\" "
		   "could not be found in your Emacs installation.  "
		   "Emacs checked the following directories for this file:\n"
		   "\n%s\n\n"
		   "When Emacs cannot find this file, it usually means that it "
		   "was not installed properly, or its distribution file was "
		   "not unpacked properly.\nSee the README.W32 file in the "
		   "top-level Emacs directory for more information.",
		   init_file_name, load_path);
	  needed = pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags,
					 buffer, -1, NULL, 0);
	  if (needed > 0)
	    {
	      wchar_t *msg_w = alloca ((needed + 1) * sizeof (wchar_t));

	      pMultiByteToWideChar (CP_UTF8, multiByteToWideCharFlags, buffer,
				    -1, msg_w, needed);
	      needed = pWideCharToMultiByte (CP_ACP, 0, msg_w, -1,
					     NULL, 0, NULL, NULL);
	      if (needed > 0)
		{
		  char *msg_a = alloca (needed + 1);

		  pWideCharToMultiByte (CP_ACP, 0, msg_w, -1, msg_a, needed,
					NULL, NULL);
		  msg = msg_a;
		}
	    }
	  MessageBox (NULL,
		      msg,
		      "Emacs Abort Dialog",
		      MB_OK | MB_ICONEXCLAMATION | MB_TASKMODAL);
	  /* Use the low-level system abort. */
	  abort ();
	}
      else
	{
	  _close (fd);
	}
    }
}

void
term_ntproc (int ignored)
{
  (void)ignored;

  term_timers ();

  /* shutdown the socket interface if necessary */
  term_winsock ();

  term_w32select ();
}

void
init_ntproc (int dumping)
{
  sigset_t initial_mask = 0;

  /* Initialize the socket interface now if available and requested by
     the user by defining PRELOAD_WINSOCK; otherwise loading will be
     delayed until open-network-stream is called (w32-has-winsock can
     also be used to dynamically load or reload winsock).

     Conveniently, init_environment is called before us, so
     PRELOAD_WINSOCK can be set in the registry. */

  /* Always initialize this correctly. */
  winsock_lib = NULL;

  if (getenv ("PRELOAD_WINSOCK") != NULL)
    init_winsock (TRUE);

  /* Initial preparation for subprocess support: replace our standard
     handles with non-inheritable versions. */
  {
    HANDLE parent;
    HANDLE stdin_save =  INVALID_HANDLE_VALUE;
    HANDLE stdout_save = INVALID_HANDLE_VALUE;
    HANDLE stderr_save = INVALID_HANDLE_VALUE;

    parent = GetCurrentProcess ();

    /* ignore errors when duplicating and closing; typically the
       handles will be invalid when running as a gui program. */
    DuplicateHandle (parent,
		     GetStdHandle (STD_INPUT_HANDLE),
		     parent,
		     &stdin_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    DuplicateHandle (parent,
		     GetStdHandle (STD_OUTPUT_HANDLE),
		     parent,
		     &stdout_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    DuplicateHandle (parent,
		     GetStdHandle (STD_ERROR_HANDLE),
		     parent,
		     &stderr_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);

    fclose (stdin);
    fclose (stdout);
    fclose (stderr);

    if (stdin_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((intptr_t) stdin_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_RDONLY);
    _fdopen (0, "r");

    if (stdout_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((intptr_t) stdout_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (1, "w");

    if (stderr_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((intptr_t) stderr_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (2, "w");
  }

  /* unfortunately, atexit depends on implementation of malloc */
  /* atexit (term_ntproc); */
  if (!dumping)
    {
      /* Make sure we start with all signals unblocked.  */
      sigprocmask (SIG_SETMASK, &initial_mask, NULL);
      signal (SIGABRT, term_ntproc);
    }
  init_timers ();

  /* determine which drives are fixed, for GetCachedVolumeInformation */
  {
    /* GetDriveType must have trailing backslash. */
    char drive[] = "A:\\";

    /* Loop over all possible drive letters */
    while (*drive <= 'Z')
    {
      /* Record if this drive letter refers to a fixed drive. */
      fixed_drives[DRIVE_INDEX (*drive)] =
	(GetDriveType (drive) == DRIVE_FIXED);

      (*drive)++;
    }

    /* Reset the volume info cache.  */
    volume_cache = NULL;
  }
}

/*
        shutdown_handler ensures that buffers' autosave files are
	up to date when the user logs off, or the system shuts down.
*/
static BOOL WINAPI
shutdown_handler (DWORD type)
{
  /* Ctrl-C and Ctrl-Break are already suppressed, so don't handle them.  */
  if (type == CTRL_CLOSE_EVENT        /* User closes console window.  */
      || type == CTRL_LOGOFF_EVENT    /* User logs off.  */
      || type == CTRL_SHUTDOWN_EVENT) /* User shutsdown.  */
    {
      /* Shut down cleanly, making sure autosave files are up to date.  */
      shut_down_emacs (0, Qnil);
    }

  /* Allow other handlers to handle this signal.  */
  return FALSE;
}

/* On Windows 9X, load UNICOWS.DLL and return its handle, or die.  On
   NT, return a handle to GDI32.DLL.  */
HANDLE
maybe_load_unicows_dll (void)
{
  if (os_subtype == OS_9X)
    {
      HANDLE ret = LoadLibrary ("Unicows.dll");
      if (ret)
	{
	  /* These two functions are present on Windows 9X as stubs
	     that always fail.  We need the real implementations from
	     UNICOWS.DLL, so we must call these functions through
	     pointers, and assign the correct addresses to these
	     pointers at program startup (see emacs.c, which calls
	     this function early on).  */
	  pMultiByteToWideChar =
	    (MultiByteToWideChar_Proc)GetProcAddress (ret, "MultiByteToWideChar");
	  pWideCharToMultiByte =
	    (WideCharToMultiByte_Proc)GetProcAddress (ret, "WideCharToMultiByte");
          multiByteToWideCharFlags = MB_ERR_INVALID_CHARS;
	  return ret;
	}
      else
	{
	  int button;

	  button = MessageBox (NULL,
			       "Emacs cannot load the UNICOWS.DLL library.\n"
			       "This library is essential for using Emacs\n"
			       "on this system.  You need to install it.\n\n"
			       "Emacs will exit when you click OK.",
			       "Emacs cannot load UNICOWS.DLL",
			       MB_ICONERROR | MB_TASKMODAL
			       | MB_SETFOREGROUND | MB_OK);
	  switch (button)
	    {
	    case IDOK:
	    default:
	      exit (1);
	    }
	}
    }
  else
    {
      /* On NT family of Windows, these two functions are always
	 linked in, so we just assign their addresses to the 2
	 pointers; no need for the LoadLibrary dance.  */
      pMultiByteToWideChar = MultiByteToWideChar;
      pWideCharToMultiByte = WideCharToMultiByte;
      /* On NT 4.0, though, MB_ERR_INVALID_CHARS is not supported.  */
      if (w32_major_version < 5)
        multiByteToWideCharFlags = 0;
      else
        multiByteToWideCharFlags = MB_ERR_INVALID_CHARS;
      return LoadLibrary ("Gdi32.dll");
    }
}

/*
	globals_of_w32 is used to initialize those global variables that
	must always be initialized on startup even when the global variable
	initialized is non zero (see the function main in emacs.c).
*/
void
globals_of_w32 (void)
{
  HMODULE kernel32 = GetModuleHandle ("kernel32.dll");

  get_process_times_fn = (GetProcessTimes_Proc)
    GetProcAddress (kernel32, "GetProcessTimes");

  DEFSYM (QCloaded_from, ":loaded-from");

  g_b_init_is_windows_9x = 0;
  g_b_init_open_process_token = 0;
  g_b_init_get_token_information = 0;
  g_b_init_lookup_account_sid = 0;
  g_b_init_get_sid_sub_authority = 0;
  g_b_init_get_sid_sub_authority_count = 0;
  g_b_init_get_security_info = 0;
  g_b_init_get_file_security_w = 0;
  g_b_init_get_file_security_a = 0;
  g_b_init_get_security_descriptor_owner = 0;
  g_b_init_get_security_descriptor_group = 0;
  g_b_init_is_valid_sid = 0;
  g_b_init_create_toolhelp32_snapshot = 0;
  g_b_init_process32_first = 0;
  g_b_init_process32_next = 0;
  g_b_init_open_thread_token = 0;
  g_b_init_impersonate_self = 0;
  g_b_init_revert_to_self = 0;
  g_b_init_get_process_memory_info = 0;
  g_b_init_get_process_working_set_size = 0;
  g_b_init_global_memory_status = 0;
  g_b_init_global_memory_status_ex = 0;
  g_b_init_equal_sid = 0;
  g_b_init_copy_sid = 0;
  g_b_init_get_length_sid = 0;
  g_b_init_get_native_system_info = 0;
  g_b_init_get_system_times = 0;
  g_b_init_create_symbolic_link_w = 0;
  g_b_init_create_symbolic_link_a = 0;
  g_b_init_get_security_descriptor_dacl = 0;
  g_b_init_convert_sd_to_sddl = 0;
  g_b_init_convert_sddl_to_sd = 0;
  g_b_init_is_valid_security_descriptor = 0;
  g_b_init_set_file_security_w = 0;
  g_b_init_set_file_security_a = 0;
  g_b_init_set_named_security_info_w = 0;
  g_b_init_set_named_security_info_a = 0;
  g_b_init_get_adapters_info = 0;
  g_b_init_compare_string_w = 0;
  g_b_init_debug_break_process = 0;
  num_of_processors = 0;
  /* The following sets a handler for shutdown notifications for
     console apps. This actually applies to Emacs in both console and
     GUI modes, since we had to fool windows into thinking emacs is a
     console application to get console mode to work.  */
  SetConsoleCtrlHandler (shutdown_handler, TRUE);

  /* "None" is the default group name on standalone workstations.  */
  strcpy (dflt_group_name, "None");

  /* Reset, in case it has some value inherited from dump time.  */
  w32_stat_get_owner_group = 0;

  /* If w32_unicode_filenames is non-zero, we will be using Unicode
     (a.k.a. "wide") APIs to invoke functions that accept file
     names.  */
  if (is_windows_9x ())
    w32_unicode_filenames = 0;
  else
    w32_unicode_filenames = 1;

#ifdef HAVE_MODULES
  dynlib_reset_last_error ();
#endif

  w32_crypto_hprov = (HCRYPTPROV)0;
}

/* For make-serial-process  */
int
serial_open (Lisp_Object port_obj)
{
  char *port = SSDATA (port_obj);
  HANDLE hnd;
  child_process *cp;
  int fd = -1;

  hnd = CreateFile (port, GENERIC_READ | GENERIC_WRITE, 0, 0,
		    OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
  if (hnd == INVALID_HANDLE_VALUE)
    error ("Could not open %s", port);
  fd = (int) _open_osfhandle ((intptr_t) hnd, 0);
  if (fd == -1)
    error ("Could not open %s", port);

  cp = new_child ();
  if (!cp)
    error ("Could not create child process");
  cp->fd = fd;
  cp->status = STATUS_READ_ACKNOWLEDGED;
  fd_info[ fd ].hnd = hnd;
  fd_info[ fd ].flags |=
    FILE_READ | FILE_WRITE | FILE_BINARY | FILE_SERIAL;
  if (fd_info[ fd ].cp != NULL)
    {
      error ("fd_info[fd = %d] is already in use", fd);
    }
  fd_info[ fd ].cp = cp;
  cp->ovl_read.hEvent = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->ovl_read.hEvent == NULL)
      error ("Could not create read event");
  cp->ovl_write.hEvent = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->ovl_write.hEvent == NULL)
      error ("Could not create write event");

  return fd;
}

/* For serial-process-configure  */
void
serial_configure (struct Lisp_Process *p, Lisp_Object contact)
{
  Lisp_Object childp2 = Qnil;
  Lisp_Object tem = Qnil;
  HANDLE hnd;
  DCB dcb;
  COMMTIMEOUTS ct;
  char summary[4] = "???"; /* This usually becomes "8N1".  */

  if ((fd_info[ p->outfd ].flags & FILE_SERIAL) == 0)
    error ("Not a serial process");
  hnd = fd_info[ p->outfd ].hnd;

  childp2 = Fcopy_sequence (p->childp);

  /* Initialize timeouts for blocking read and blocking write.  */
  if (!GetCommTimeouts (hnd, &ct))
    error ("GetCommTimeouts() failed");
  ct.ReadIntervalTimeout	 = 0;
  ct.ReadTotalTimeoutMultiplier	 = 0;
  ct.ReadTotalTimeoutConstant	 = 0;
  ct.WriteTotalTimeoutMultiplier = 0;
  ct.WriteTotalTimeoutConstant	 = 0;
  if (!SetCommTimeouts (hnd, &ct))
    error ("SetCommTimeouts() failed");
  /* Read port attributes and prepare default configuration.  */
  memset (&dcb, 0, sizeof (dcb));
  dcb.DCBlength = sizeof (DCB);
  if (!GetCommState (hnd, &dcb))
    error ("GetCommState() failed");
  dcb.fBinary	    = TRUE;
  dcb.fNull	    = FALSE;
  dcb.fAbortOnError = FALSE;
  /* dcb.XonLim and dcb.XoffLim are set by GetCommState() */
  dcb.ErrorChar	    = 0;
  dcb.EofChar	    = 0;
  dcb.EvtChar       = 0;

  /* Configure speed.  */
  if (!NILP (Fplist_member (contact, QCspeed)))
    tem = Fplist_get (contact, QCspeed);
  else
    tem = Fplist_get (p->childp, QCspeed);
  CHECK_NUMBER (tem);
  dcb.BaudRate = XINT (tem);
  childp2 = Fplist_put (childp2, QCspeed, tem);

  /* Configure bytesize.  */
  if (!NILP (Fplist_member (contact, QCbytesize)))
    tem = Fplist_get (contact, QCbytesize);
  else
    tem = Fplist_get (p->childp, QCbytesize);
  if (NILP (tem))
    tem = make_number (8);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 7 && XINT (tem) != 8)
    error (":bytesize must be nil (8), 7, or 8");
  dcb.ByteSize = XINT (tem);
  summary[0] = XINT (tem) + '0';
  childp2 = Fplist_put (childp2, QCbytesize, tem);

  /* Configure parity.  */
  if (!NILP (Fplist_member (contact, QCparity)))
    tem = Fplist_get (contact, QCparity);
  else
    tem = Fplist_get (p->childp, QCparity);
  if (!NILP (tem) && !EQ (tem, Qeven) && !EQ (tem, Qodd))
    error (":parity must be nil (no parity), `even', or `odd'");
  dcb.fParity = FALSE;
  dcb.Parity = NOPARITY;
  dcb.fErrorChar = FALSE;
  if (NILP (tem))
    {
      summary[1] = 'N';
    }
  else if (EQ (tem, Qeven))
    {
      summary[1] = 'E';
      dcb.fParity = TRUE;
      dcb.Parity = EVENPARITY;
      dcb.fErrorChar = TRUE;
    }
  else if (EQ (tem, Qodd))
    {
      summary[1] = 'O';
      dcb.fParity = TRUE;
      dcb.Parity = ODDPARITY;
      dcb.fErrorChar = TRUE;
    }
  childp2 = Fplist_put (childp2, QCparity, tem);

  /* Configure stopbits.  */
  if (!NILP (Fplist_member (contact, QCstopbits)))
    tem = Fplist_get (contact, QCstopbits);
  else
    tem = Fplist_get (p->childp, QCstopbits);
  if (NILP (tem))
    tem = make_number (1);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 1 && XINT (tem) != 2)
    error (":stopbits must be nil (1 stopbit), 1, or 2");
  summary[2] = XINT (tem) + '0';
  if (XINT (tem) == 1)
    dcb.StopBits = ONESTOPBIT;
  else if (XINT (tem) == 2)
    dcb.StopBits = TWOSTOPBITS;
  childp2 = Fplist_put (childp2, QCstopbits, tem);

  /* Configure flowcontrol.  */
  if (!NILP (Fplist_member (contact, QCflowcontrol)))
    tem = Fplist_get (contact, QCflowcontrol);
  else
    tem = Fplist_get (p->childp, QCflowcontrol);
  if (!NILP (tem) && !EQ (tem, Qhw) && !EQ (tem, Qsw))
    error (":flowcontrol must be nil (no flowcontrol), `hw', or `sw'");
  dcb.fOutxCtsFlow	= FALSE;
  dcb.fOutxDsrFlow	= FALSE;
  dcb.fDtrControl	= DTR_CONTROL_DISABLE;
  dcb.fDsrSensitivity	= FALSE;
  dcb.fTXContinueOnXoff	= FALSE;
  dcb.fOutX		= FALSE;
  dcb.fInX		= FALSE;
  dcb.fRtsControl	= RTS_CONTROL_DISABLE;
  dcb.XonChar		= 17; /* Control-Q  */
  dcb.XoffChar		= 19; /* Control-S  */
  if (NILP (tem))
    {
      /* Already configured.  */
    }
  else if (EQ (tem, Qhw))
    {
      dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
      dcb.fOutxCtsFlow = TRUE;
    }
  else if (EQ (tem, Qsw))
    {
      dcb.fOutX = TRUE;
      dcb.fInX = TRUE;
    }
  childp2 = Fplist_put (childp2, QCflowcontrol, tem);

  /* Activate configuration.  */
  if (!SetCommState (hnd, &dcb))
    error ("SetCommState() failed");

  childp2 = Fplist_put (childp2, QCsummary, build_string (summary));
  pset_childp (p, childp2);
}

/* For make-pipe-process */
void
register_aux_fd (int infd)
{
  child_process *cp;

  cp = new_child ();
  if (!cp)
    error ("Could not create child process");
  cp->fd = infd;
  cp->status = STATUS_READ_ACKNOWLEDGED;

  if (fd_info[ infd ].cp != NULL)
    {
      error ("fd_info[fd = %d] is already in use", infd);
    }
  fd_info[ infd ].cp = cp;
  fd_info[ infd ].hnd = (HANDLE) _get_osfhandle (infd);
}

#ifdef HAVE_GNUTLS

ssize_t
emacs_gnutls_pull (gnutls_transport_ptr_t p, void* buf, size_t sz)
{
  int n, err;
  struct Lisp_Process *process = (struct Lisp_Process *)p;
  int fd = process->infd;

  n = sys_read (fd, (char*)buf, sz);

  if (n >= 0)
    return n;

  err = errno;

  /* Translate the WSAEWOULDBLOCK alias EWOULDBLOCK to EAGAIN. */
  if (err == EWOULDBLOCK)
    err = EAGAIN;

  emacs_gnutls_transport_set_errno (process->gnutls_state, err);

  return -1;
}

ssize_t
emacs_gnutls_push (gnutls_transport_ptr_t p, const void* buf, size_t sz)
{
  struct Lisp_Process *process = (struct Lisp_Process *)p;
  int fd = process->outfd;
  ssize_t n = sys_write (fd, buf, sz);

  /* 0 or more bytes written means everything went fine.  */
  if (n >= 0)
    return n;

  /* Negative bytes written means we got an error in errno.
     Translate the WSAEWOULDBLOCK alias EWOULDBLOCK to EAGAIN.  */
  emacs_gnutls_transport_set_errno (process->gnutls_state,
                                    errno == EWOULDBLOCK ? EAGAIN : errno);

  return -1;
}
#endif /* HAVE_GNUTLS */

/* end of w32.c */
