#ifdef POSIX_SIGNALS

#define SIGMASKTYPE sigset_t
#define SIGEMPTYMASK (signal_empty_mask)
#define SIGFULLMASK (signal_full_mask)
extern sigset_t signal_empty_mask, signal_full_mask;

#ifdef sigmask
#undef sigmask
#endif
#define sigmask(SIG) \
(_mask = SIGEMPTYMASK, sigaddset (&_mask, SIG), _mask)

/* Local mask is used to avoid problems if code using any of the macros 
   below could be reentered due to a signal occurring.
   This can't happen in Emacs 18.58, but just to be safe... - DJB
   These macros require GCC.  */
#define sigpause(SIG)    ({ sigset_t _mask; sys_sigpause(SIG); })
#define sigblock(SIG)    ({ sigset_t _mask; sys_sigblock(SIG); })
#define sigunblock(SIG)  ({ sigset_t _mask; sys_sigunblock(SIG); })
#define sigsetmask(SIG)  ({ sigset_t _mask; sys_sigsetmask(SIG); })
#define sighold(SIG)     ONLY_USED_IN_BSD_4_1
#define sigrelse(SIG)    ONLY_USED_IN_BSD_4_1

int (*sys_signal (int signal_number, int (*action)())) ();
int sys_sigpause (sigset_t new_mask);
sigset_t sys_sigblock (sigset_t new_mask);
sigset_t sys_sigunblock (sigset_t new_mask);
sigset_t sys_sigsetmask (sigset_t new_mask);

#define sys_sigdel(MASK,SIG) sigdelset(&MASK,SIG)
#endif /* POSIX_SIGNALS */

#ifndef SIGMASKTYPE
#define SIGMASKTYPE int
#endif

#ifndef SIGEMPTYMASK
#define SIGEMPTYMASK 0
#endif

#ifndef SIGFULLMASK
#define SIGFULLMASK 0xffffffff
#endif

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#ifndef sigunblock
#define sigunblock(SIG) \
{ SIGMASKTYPE omask = sigblock (SIGFULLMASK); sigsetmask (omask & ~SIG); }
#endif

#ifndef sys_sigdel
#define sys_sigdel(MASK,SIG) MASK &= ~(1 << SIG)
#endif
