/* The version here should match that in configure/configure.in	*/
/* Eventually this one may become unnecessary.  For now we need	*/
/* it to keep the old-style build process working.		*/
#define GC_TMP_VERSION_MAJOR 6
#define GC_TMP_VERSION_MINOR 2
#define GC_TMP_ALPHA_VERSION 6

#if defined(GC_VERSION_MAJOR)
# if GC_TMP_VERSION_MAJOR != GC_VERSION_MAJOR || \
     GC_TMP_VERSION_MINOR != GC_VERSION_MINOR || \
     GC_TMP_ALPHA_VERSION != GC_ALPHA_VERSION
#   error Inconsistent version info.  Check version.h and configure.in.
# endif
#else
# define GC_VERSION_MAJOR GC_TMP_VERSION_MAJOR
# define GC_VERSION_MINOR GC_TMP_VERSION_MINOR
# define GC_ALPHA_VERSION GC_TMP_ALPHA_VERSION
#endif

#ifndef GC_NOT_ALPHA
#   define GC_NOT_ALPHA 0xff
#endif

#ifndef GC_NO_VERSION_VAR

unsigned GC_version = ((GC_VERSION_MAJOR << 16) | (GC_VERSION_MINOR << 8) | GC_ALPHA_VERSION);

#endif /* GC_NO_VERSION_VAR */ 
