/* Definitions file for GNU Emacs running on NEC's UX/4800 (SVR4.2MP) */
#include "usg5-4-2.h"
#undef LIB_MOTIF
#ifndef nec_ews
#define nec_ews
#endif
#ifndef nec_ews_svr4
#define nec_ews_svr4
#endif
#define XOS_NEEDS_TIME_H
#define HAVE_CLOCK
#ifdef __STDC__
#define MKDIR_PROTOTYPE int mkdir(char *dpath, mode_t dmode)
#endif
#ifndef __GNUC__
#define C_DEBUG_SWITCH -O  -KOlimit=3000 -ZXNd=5000
#endif

/* arch-tag: e42eeb13-028a-490b-8427-0b57010f2ab9
   (do not change this comment) */
