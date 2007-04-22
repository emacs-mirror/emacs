/* confdefs.h.  */
#define PACKAGE_NAME ""
#define PACKAGE_TARNAME ""
#define PACKAGE_VERSION ""
#define PACKAGE_STRING ""
#define PACKAGE_BUGREPORT ""
#define MAIL_USE_POP 1
#define _GNU_SOURCE 1
#define _FILE_OFFSET_BITS 64
#define STDC_HEADERS 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_MEMORY_H 1
#define HAVE_STRINGS_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_STDINT_H 1
#define HAVE_UNISTD_H 1
#define HAVE_SYS_SOUNDCARD_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_UTIME_H 1
#define HAVE_LINUX_VERSION_H 1
#define HAVE_TERMIOS_H 1
#define HAVE_LIMITS_H 1
#define HAVE_STRING_H 1
#define HAVE_STDLIB_H 1
#define HAVE_TERMCAP_H 1
#define HAVE_STDIO_EXT_H 1
#define HAVE_FCNTL_H 1
#define HAVE_STRINGS_H 1
#define HAVE_PTY_H 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_VLIMIT_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_LOCALE_H 1
#define HAVE_SYS_UTSNAME_H 1
#define HAVE_PWD_H 1
#define HAVE_PERSONALITY_LINUX32 1
#define HAVE_TERM_H 1
#define STDC_HEADERS 1
#define TIME_WITH_SYS_TIME 1
#define HAVE_DECL_SYS_SIGLIST 0
#define HAVE_DECL___SYS_SIGLIST 0
#define HAVE_SYS_WAIT_H 1
#define HAVE_STRUCT_UTIMBUF 1
#define RETSIGTYPE void
#define HAVE_SPEED_T 1
#define HAVE_TIMEVAL 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_NET_IF_H 1
#define HAVE_STRUCT_TM_TM_ZONE 1
#define HAVE_TM_ZONE 1
#define HAVE_TM_GMTOFF 1
#define HAVE_STRUCT_IFREQ_IFR_FLAGS 1
#define HAVE_STRUCT_IFREQ_IFR_HWADDR 1
#define HAVE_STRUCT_IFREQ_IFR_NETMASK 1
#define HAVE_STRUCT_IFREQ_IFR_BROADADDR 1
#define HAVE_STRUCT_IFREQ_IFR_ADDR 1
#define PROTOTYPES 1
#define __PROTOTYPES 1
#define POINTER_TYPE void
#define HAVE_LONG_FILE_NAMES 1
#define DOUG_LEA_MALLOC 1
#define HAVE_STDLIB_H 1
#define HAVE_UNISTD_H 1
#define HAVE_GETPAGESIZE 1
#define HAVE_MMAP 1
#define HAVE_XKBGETKEYBOARD 1
#define HAVE_XRMSETDATABASE 1
#define HAVE_XSCREENRESOURCESTRING 1
#define HAVE_XSCREENNUMBEROFSCREEN 1
#define HAVE_XSETWMPROTOCOLS 1
#define HAVE_X11R6 1
#define HAVE_X11R5 1
#define HAVE_GTK_MAIN 1
#define HAVE_GTK 1
#define HAVE_GDK_DISPLAY_OPEN 1
#define HAVE_GTK_MULTIDISPLAY 1
#define HAVE_GTK_FILE_SELECTION_NEW 1
#define HAVE_GTK_FILE_CHOOSER_DIALOG_NEW 1
#define HAVE_GTK_FILE_BOTH 1
#define HAVE_PTHREAD_H 1
#define HAVE_GTK_AND_PTHREAD 1
#define HAVE_XFT 1
#define USE_TOOLKIT_SCROLL_BARS 1
#define HAVE_XIM 1
#define USE_XIM 1
#define XRegisterIMInstantiateCallback_arg6 XPointer
#define HAVE_XPM 1
#define HAVE_JPEG 1
#define HAVE_JPEG 1
#define HAVE_PNG_H 1
#define HAVE_LIBPNG_PNG_H 1
#define HAVE_PNG 1
#define HAVE_TIFF 1
#define HAVE_GIF 1
#define HAVE_X_SM 1
#define HAVE_H_ERRNO 1
#define HAVE_ALLOCA_H 1
#define HAVE_ALLOCA 1
#define HAVE_LIBM 1
#define HAVE_LIBLOCKFILE 1
#define HAVE_TOUCHLOCK 1
#define HAVE_MAILLOCK_H 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETDOMAINNAME 1
#define HAVE_DUP2 1
#define HAVE_RENAME 1
#define HAVE_CLOSEDIR 1
#define HAVE_MKDIR 1
#define HAVE_RMDIR 1
#define HAVE_SYSINFO 1
#define HAVE_GETRUSAGE 1
#define HAVE_GET_CURRENT_DIR_NAME 1
#define HAVE_RANDOM 1
#define HAVE_LRAND48 1
#define HAVE_BCOPY 1
#define HAVE_BCMP 1
#define HAVE_LOGB 1
#define HAVE_FREXP 1
#define HAVE_FMOD 1
#define HAVE_RINT 1
#define HAVE_CBRT 1
#define HAVE_FTIME 1
#define HAVE_SETSID 1
#define HAVE_STRERROR 1
#define HAVE_FPATHCONF 1
#define HAVE_SELECT 1
#define HAVE_MKTIME 1
#define HAVE_EUIDACCESS 1
#define HAVE_GETPAGESIZE 1
#define HAVE_TZSET 1
/* end confdefs.h.  */
/* Define setlocale to an innocuous variant, in case <limits.h> declares setlocale.
   For example, HP-UX 11i <limits.h> declares gettimeofday.  */
#define setlocale innocuous_setlocale

/* System header to define __stub macros and hopefully few prototypes,
    which can conflict with char setlocale (); below.
    Prefer <limits.h> to <assert.h> if __STDC__ is defined, since
    <limits.h> exists even on freestanding compilers.  */

#ifdef __STDC__
# include <limits.h>
#else
# include <assert.h>
#endif

#undef setlocale

/* Override any GCC internal prototype to avoid an error.
   Use char because int might match the return type of a GCC
   builtin and then its argument prototype would still apply.  */
#ifdef __cplusplus
extern "C"
#endif
char setlocale ();
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined __stub_setlocale || defined __stub___setlocale
choke me
#endif

int
main ()
{
return setlocale ();
  ;
  return 0;
}
