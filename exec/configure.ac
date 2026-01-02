dnl  Autoconf script for GNU Emacs's exec library.
dnl To rebuild the 'configure' script from this, execute the command
dnl	autoconf
dnl in the directory containing this script.
dnl If you changed any AC_DEFINES, also run autoheader.
dnl
dnl Copyright (C) 2023-2026 Free Software Foundation, Inc.
dnl
dnl  This file is part of GNU Emacs.
dnl
dnl  GNU Emacs is free software: you can redistribute it and/or modify
dnl  it under the terms of the GNU General Public License as published by
dnl  the Free Software Foundation, either version 3 of the License, or
dnl  (at your option) any later version.
dnl
dnl  GNU Emacs is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

AC_PREREQ([2.65])
AC_INIT([libexec], [31.0.50], [bug-gnu-emacs@gnu.org], [],
  [https://www.gnu.org/software/emacs/])

AH_TOP([/* Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

GNU Emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */])

AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL

AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINTPTR_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_PID_T

AC_HEADER_STDBOOL
AC_CHECK_FUNCS([getpagesize stpcpy])
AC_CHECK_DECLS([stpcpy])
AC_CHECK_FUNC([process_vm_readv],
  [AC_CHECK_FUNC([process_vm_writev],
    [AC_CHECK_DECL([process_vm_readv],
      [AC_DEFINE([HAVE_PROCESS_VM], [1],
        [Define to 1 if process_vm_readv is available.])],
      [], [[
#include <sys/uio.h>
      ]])])])
AC_CHECK_HEADERS([sys/param.h sys/uio.h])
AC_CHECK_MEMBERS([siginfo_t.si_syscall], [], [],
[[
#include <signal.h>
]])

AH_BOTTOM([
#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#else
# ifndef HAVE__BOOL
#  ifdef __cplusplus
typedef bool _Bool;
#  else
#   define _Bool signed char
#  endif
# endif
# define bool _Bool
# define false 0
# define true 1
# define __bool_true_false_are_defined 1
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif /* MAX */

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif /* MIN */
])

AC_C_BIGENDIAN

AH_TEMPLATE([SYSCALL_HEADER], [Define to header holding system call numbers.])
AH_TEMPLATE([USER_HEADER], [Define to header holding USER_REGS_STRUCT.])
AH_TEMPLATE([USER_REGS_STRUCT], [Define to structure holding user registers.])
AH_TEMPLATE([SYSCALL_NUM_REG], [Define to register holding the system call number.])
AH_TEMPLATE([SYSCALL_ARG_REG], [Define to register holding arg0 to system calls.])
AH_TEMPLATE([SYSCALL_ARG1_REG], [Define to register holding arg1 to system calls.])
AH_TEMPLATE([SYSCALL_ARG2_REG], [Define to register holding arg2 to system calls.])
AH_TEMPLATE([SYSCALL_ARG3_REG], [Define to register holding arg3 to system calls.])
AH_TEMPLATE([SYSCALL_RET_REG], [Define to register holding value of system calls.])
AH_TEMPLATE([STACK_POINTER], [Define to register holding the stack pointer.])
AH_TEMPLATE([PROGRAM_COUNTER], [Define to register holding the program counter.])
AH_TEMPLATE([EXEC_SYSCALL], [Define to number of the `exec' system call.])
AH_TEMPLATE([USER_WORD], [Define to word type used by tracees.])
AH_TEMPLATE([USER_SWORD], [Define to signed word type used by tracees.])
AH_TEMPLATE([EXEC_64], [Define to 1 if the system utilizes 64-bit ELF.])
AH_TEMPLATE([STACK_GROWS_DOWNWARDS], [Define to 1 if the stack grows downwards.])
AH_TEMPLATE([ABI_RED_ZONE], [Define to number of reserved bytes past the stack frame.])
AH_TEMPLATE([EXECUTABLE_BASE], [Virtual address for loading PIC executables])
AH_TEMPLATE([INTERPRETER_BASE], [Virtual address for loading PIC interpreters])
AH_TEMPLATE([CLONE_SYSCALL], [Define to number of the `clone' system call.])
AH_TEMPLATE([CLONE3_SYSCALL], [Define to number of the `clone3' system call.])
AH_TEMPLATE([READLINK_SYSCALL], [Define to number of the `readlink' system call.])
AH_TEMPLATE([READLINKAT_SYSCALL], [Define to number of the `readlinkat' system call.])
AH_TEMPLATE([OPEN_SYSCALL], [Define to number of the `open' system call.])
AH_TEMPLATE([OPENAT_SYSCALL], [Define to number of the `openat' system call.])
AH_TEMPLATE([HAVE_SECCOMP], [Define to 1 if secure computing filters are available
to accelerate interception of system calls.])

AC_CANONICAL_HOST

# Check whether or not sys/user exists.  If it doesn't, try
# asm/user.h, and croak if that doesn't exist either.
AS_CASE([$host], [*mips*], [], [*],
  [AC_CHECK_HEADER([sys/user.h], [user_h="<sys/user.h>"],
    [AC_CHECK_HEADER([asm/user.h], [user_h="<asm/user.h>"],
      [AC_MSG_ERROR([Can not find working user.h])])])])

# Look for required tools.

AC_ARG_VAR([M4], [`m4' preprocessor command.])
AC_ARG_VAR([AS], [`as' assembler command.])
AC_ARG_VAR([LD], [`ld' linker command.])

# Check for a working m4.
AC_CHECK_PROGS([M4], [gm4 m4],
  [AC_MSG_ERROR([Cannot find m4])])

# Check for a working assembler.
AC_CHECK_TOOL([AS], [as],
  [AC_MSG_ERROR([Cannot find a working assembler])])

# And ar.
AC_CHECK_TOOL([AR], [ar],
  [AC_MSG_ERROR([Cannot find a working ar])])

# And ld.
AC_CHECK_TOOL([LD], [ld],
  [AC_MSG_ERROR([Cannot find a working linker])])

# Now check if ld is a C compiler.
LDPREFIX=
AC_CACHE_CHECK([whether ld is a C compiler],
  [exec_cv_ld_is_cc],
  [cat <<_ACEOF > conftest.c
AC_LANG_PROGRAM(,)
_ACEOF
   exec_cv_ld_is_cc=yes
   $LD -c conftest.c -o conftest.$OBJEXT >&AS_MESSAGE_LOG_FD 2>&1 \
   || exec_cv_ld_is_cc=no
   rm -f conftest.c conftest.$OBJEXT])

# And if as is a C compiler.
AC_CACHE_CHECK([whether as is a C compiler],
  [exec_cv_as_is_cc],
  [cat <<_ACEOF > conftest.c
AC_LANG_PROGRAM(,)
_ACEOF
   exec_cv_as_is_cc=yes
   $AS -c conftest.c -o conftest.$OBJEXT >&AS_MESSAGE_LOG_FD 2>&1 \
   || exec_cv_as_is_cc=no
   rm -f conftest.c conftest.$OBJEXT])

# If ld is a C compiler, pass `-nostdlib', `-nostartfiles', and
# `-static'.  Also, set LDPREFIX to -Wl,
AS_IF([test "x$exec_cv_ld_is_cc" = "xyes"],
  [LOADERFLAGS="$LOADERFLAGS -nostdlib -nostartfiles -static"
   LDPREFIX=-Wl,])

# If as is a C compiler, add `-c' to ASFLAGS.
AS_IF([test "x$exec_cv_as_is_cc" = "xyes"],
  [ASFLAGS="$ASFLAGS -c"])

AC_DEFUN([exec_CHECK_LINUX_CLONE3],
[
AC_CHECK_DECL([__NR_clone3],
  [AC_DEFINE([CLONE3_SYSCALL], [__NR_clone3])],
  [], [[
#include <asm/unistd.h>
]])
])

AC_DEFUN([exec_CHECK_MIPS_NABI],
[
AC_CACHE_CHECK([whether MIPS NABI calling convention is used],
  [exec_cv_mips_nabi],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sgidefs.h>
]], [[
#if !defined __mips64__ && !defined __LP64__
#if _MIPS_SIM == _ABIO32
OABI in use.
#endif /* _MIPS_SIM == _ABIO32 */
#endif /* !__mips64__ && !defined __LP64__ */
]])], [exec_cv_mips_nabi=yes],
  [exec_cv_mips_nabi=no])])

dnl mips64 systems use N64 calling convention, a variant of nabi
dnl calling convention.
AS_IF([test "x$exec_cv_mips_nabi" != "xno"],
  [AC_DEFINE([MIPS_NABI], [1],
    [Define to 1 if MIPS NABI calling convention is being used.])],
   [OBJS="$OBJS mipsfpu.o"])
])

# Determine the system type and define appropriate macros.
exec_loader=
is_mips=
OBJS="exec.o trace.o"
DADDI_BROKEN=no

AS_CASE([$host], [x86_64-*linux*],
  [AC_CHECK_MEMBER([struct user_regs_struct.rdi],
    [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
     AC_DEFINE_UNQUOTED([USER_HEADER], [$user_h])
     AC_DEFINE([USER_REGS_STRUCT], [struct user_regs_struct])
     AC_DEFINE([SYSCALL_NUM_REG], [orig_rax])
     AC_DEFINE([SYSCALL_RET_REG], [rax])
     AC_DEFINE([SYSCALL_ARG_REG], [rdi])
     AC_DEFINE([SYSCALL_ARG1_REG], [rsi])
     AC_DEFINE([SYSCALL_ARG2_REG], [rdx])
     AC_DEFINE([SYSCALL_ARG3_REG], [r10])
     AC_DEFINE([STACK_POINTER], [rsp])
     AC_DEFINE([PROGRAM_COUNTER], [rip])
     AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
     AC_DEFINE([USER_WORD], [uintptr_t])
     AC_DEFINE([USER_SWORD], [intptr_t])
     AC_DEFINE([EXEC_64], [1])
     AC_DEFINE([ABI_RED_ZONE], [128])
     AC_DEFINE([EXECUTABLE_BASE], [0x555555554000])
     AC_DEFINE([INTERPRETER_BASE], [0x600000000000])
     AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
     AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
     AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
     AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
     AC_DEFINE([OPEN_SYSCALL], [__NR_open])
     AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
     exec_CHECK_LINUX_CLONE3
     # Make sure the loader doesn't conflict with other position
     # dependent code.
     LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x200000000000"
     exec_loader=loader-x86_64.s],
    [AC_MSG_ERROR([Missing `rdi' in user_regs_struct])],
    [[
#include $user_h
    ]])], [i[[34567]]86-*linux*],
  [AC_CHECK_MEMBER([struct user_regs_struct.edi],
    [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
     AC_DEFINE_UNQUOTED([USER_HEADER], [$user_h])
     AC_DEFINE([USER_REGS_STRUCT], [struct user_regs_struct])
     AC_DEFINE([SYSCALL_NUM_REG], [orig_eax])
     AC_DEFINE([SYSCALL_RET_REG], [eax])
     AC_DEFINE([SYSCALL_ARG_REG], [ebx])
     AC_DEFINE([SYSCALL_ARG1_REG], [ecx])
     AC_DEFINE([SYSCALL_ARG2_REG], [edx])
     AC_DEFINE([SYSCALL_ARG3_REG], [esi])
     AC_DEFINE([STACK_POINTER], [esp])
     AC_DEFINE([PROGRAM_COUNTER], [eip])
     AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
     AC_DEFINE([USER_WORD], [uintptr_t])
     AC_DEFINE([USER_SWORD], [intptr_t])
     AC_DEFINE([EXECUTABLE_BASE], [0x0f000000])
     AC_DEFINE([INTERPRETER_BASE], [0xaf000000])
     AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
     AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
     AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
     AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
     AC_DEFINE([OPEN_SYSCALL], [__NR_open])
     AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
     exec_CHECK_LINUX_CLONE3
     # Make sure the loader doesn't conflict with other position
     # dependent code.
     LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0xa0000000"
     exec_loader=loader-x86.s],
    [AC_MSG_ERROR([Missing `edi' in user_regs_struct])],
    [[
#include $user_h
    ]])], [aarch64-*linux*],
  [AC_CHECK_MEMBER([struct user_regs_struct.sp],
    [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
     AC_DEFINE_UNQUOTED([USER_HEADER], [$user_h])
     AC_DEFINE([USER_REGS_STRUCT], [struct user_regs_struct])
     AC_DEFINE([SYSCALL_NUM_REG], [[regs[8]]])
     AC_DEFINE([SYSCALL_RET_REG], [[regs[0]]])
     AC_DEFINE([SYSCALL_ARG_REG], [[regs[0]]])
     AC_DEFINE([SYSCALL_ARG1_REG], [[regs[1]]])
     AC_DEFINE([SYSCALL_ARG2_REG], [[regs[2]]])
     AC_DEFINE([SYSCALL_ARG3_REG], [[regs[3]]])
     AC_DEFINE([STACK_POINTER], [sp])
     AC_DEFINE([PROGRAM_COUNTER], [pc])
     AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
     AC_DEFINE([USER_WORD], [uintptr_t])
     AC_DEFINE([USER_SWORD], [intptr_t])
     AC_DEFINE([EXEC_64], [1])
     AC_DEFINE([EXECUTABLE_BASE], [0x3000000000])
     AC_DEFINE([INTERPRETER_BASE], [0x3f00000000])
     AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
     AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
     # Note that aarch64 has neither `readlink' nor `open'.
     AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
     AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
     exec_CHECK_LINUX_CLONE3
     # Make sure the loader doesn't conflict with other position
     # dependent code.  ARM places rather significant restrictions on
     # virtual addresses for a 64 bit architecture.
     LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x2000000000"
     exec_loader=loader-aarch64.s],
    [AC_MSG_ERROR([Missing `sp' in user_regs_struct])],
    [[
#include $user_h
    ]])], [arm*linux*eabi* | armv7*linux*],
  [AC_CHECK_MEMBER([struct user_regs.uregs],
    [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
     AC_DEFINE_UNQUOTED([USER_HEADER], [$user_h])
     AC_DEFINE([USER_REGS_STRUCT], [struct user_regs])
     AC_DEFINE([SYSCALL_NUM_REG], [[uregs[7]]])
     AC_DEFINE([SYSCALL_RET_REG], [[uregs[0]]])
     AC_DEFINE([SYSCALL_ARG_REG], [[uregs[0]]])
     AC_DEFINE([SYSCALL_ARG1_REG], [[uregs[1]]])
     AC_DEFINE([SYSCALL_ARG2_REG], [[uregs[2]]])
     AC_DEFINE([SYSCALL_ARG3_REG], [[uregs[3]]])
     AC_DEFINE([STACK_POINTER], [[uregs[13]]])
     AC_DEFINE([PROGRAM_COUNTER], [[uregs[15]]])
     AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
     AC_DEFINE([USER_WORD], [uintptr_t])
     AC_DEFINE([USER_SWORD], [intptr_t])
     AC_DEFINE([EXECUTABLE_BASE], [0x0f000000])
     AC_DEFINE([INTERPRETER_BASE], [0x1f000000])
     AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
     AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
     AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
     AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
     AC_DEFINE([OPEN_SYSCALL], [__NR_open])
     AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
     exec_CHECK_LINUX_CLONE3
     LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x20000000"
     exec_loader=loader-armeabi.s],
    [AC_CHECK_MEMBER([struct pt_regs.uregs],
      [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
       AC_DEFINE_UNQUOTED([USER_HEADER], [<asm/ptrace.h>])
       AC_DEFINE([USER_REGS_STRUCT], [struct pt_regs])
       AC_DEFINE([SYSCALL_NUM_REG], [[uregs[7]]])
       AC_DEFINE([SYSCALL_RET_REG], [[uregs[0]]])
       AC_DEFINE([SYSCALL_ARG_REG], [[uregs[0]]])
       AC_DEFINE([SYSCALL_ARG1_REG], [[uregs[1]]])
       AC_DEFINE([SYSCALL_ARG2_REG], [[uregs[2]]])
       AC_DEFINE([SYSCALL_ARG3_REG], [[uregs[3]]])
       AC_DEFINE([STACK_POINTER], [[uregs[13]]])
       AC_DEFINE([STACK_POINTER], [[uregs[15]]])
       AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
       AC_DEFINE([USER_WORD], [uintptr_t])
       AC_DEFINE([USER_SWORD], [intptr_t])
       AC_DEFINE([EXECUTABLE_BASE], [0x0f000000])
       AC_DEFINE([INTERPRETER_BASE], [0x1f000000])
       AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
       AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
       AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
       AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
       AC_DEFINE([OPEN_SYSCALL], [__NR_open])
       AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
       exec_CHECK_LINUX_CLONE3
       LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x20000000"
       exec_loader=loader-armeabi.s],
      [AC_MSG_ERROR([Missing `uregs' in user_regs_struct or pt_regs])],
      [[
#include <asm/ptrace.h>
      ]])],
    [[
#include $user_h
    ]])], [mipsel*linux*],
  [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
   AC_DEFINE([USER_HEADER], ["mipsel-user.h"])
   AC_DEFINE([USER_REGS_STRUCT], [struct mipsel_regs])
   AC_DEFINE([SYSCALL_NUM_REG], [[gregs[2]]]) # v0
   AC_DEFINE([SYSCALL_RET_REG], [[gregs[4]]]) # a0
   AC_DEFINE([SYSCALL_ARG_REG], [[gregs[4]]]) # a0
   AC_DEFINE([SYSCALL_ARG1_REG], [[gregs[5]]]) # a1
   AC_DEFINE([SYSCALL_ARG2_REG], [[gregs[4]]]) # a2
   AC_DEFINE([SYSCALL_ARG3_REG], [[gregs[5]]]) # a3
   AC_DEFINE([STACK_POINTER], [[gregs[29]]]) # sp
   AC_DEFINE([PROGRAM_COUNTER], [[cp0_epc]]) # pc
   AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
   AC_DEFINE([USER_WORD], [uintptr_t])
   AC_DEFINE([USER_SWORD], [intptr_t])
   AC_DEFINE([EXECUTABLE_BASE], [0x0f000000])
   AC_DEFINE([INTERPRETER_BASE], [0x1f000000])
   AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
   AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
   AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
   AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
   AC_DEFINE([OPEN_SYSCALL], [__NR_open])
   AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
   AC_CHECK_DECL([_MIPS_SIM], [exec_CHECK_MIPS_NABI],
     [AC_MSG_ERROR([_MIPS_SIM could not be determined]),
     [[
#include <sgidefs.h>
]]])
   exec_CHECK_LINUX_CLONE3
   LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x20000000"
   is_mips=yes
   exec_loader=loader-mipsel.s], [mips64el*linux*],
  [AC_DEFINE([SYSCALL_HEADER], [<asm/unistd.h>])
   AC_DEFINE([USER_HEADER], ["mipsel-user.h"])
   AC_DEFINE([USER_REGS_STRUCT], [struct mipsel_regs])
   AC_DEFINE([SYSCALL_NUM_REG], [[gregs[2]]]) # v0
   AC_DEFINE([SYSCALL_RET_REG], [[gregs[4]]]) # a0
   AC_DEFINE([SYSCALL_ARG_REG], [[gregs[4]]]) # a0
   AC_DEFINE([SYSCALL_ARG1_REG], [[gregs[5]]]) # a1
   AC_DEFINE([SYSCALL_ARG2_REG], [[gregs[4]]]) # a2
   AC_DEFINE([SYSCALL_ARG3_REG], [[gregs[5]]]) # a3
   AC_DEFINE([STACK_POINTER], [[gregs[29]]]) # sp
   AC_DEFINE([PROGRAM_COUNTER], [[cp0_epc]]) # pc
   AC_DEFINE([EXEC_SYSCALL], [__NR_execve])
   AC_DEFINE([USER_WORD], [uintptr_t])
   AC_DEFINE([USER_SWORD], [intptr_t])
   AC_DEFINE([EXEC_64], [1])
   AC_DEFINE([EXECUTABLE_BASE], [0x400000])
   AC_DEFINE([INTERPRETER_BASE], [0x3f00000000])
   AC_DEFINE([STACK_GROWS_DOWNWARDS], [1])
   AC_DEFINE([CLONE_SYSCALL], [__NR_clone])
   AC_DEFINE([READLINK_SYSCALL], [__NR_readlink])
   AC_DEFINE([READLINKAT_SYSCALL], [__NR_readlinkat])
   AC_DEFINE([OPEN_SYSCALL], [__NR_open])
   AC_DEFINE([OPENAT_SYSCALL], [__NR_openat])
   AC_CACHE_CHECK([whether as understands `daddi'],
     [exec_cv_as_daddi],
     [exec_cv_as_daddi=no
      cat <<_ACEOF >conftest.s
      .section text
      .global __start
__start:
	li	\$t0, 0
	li	\$t1, 0
	daddi	\$t0, \$t1, 1
	daddi	\$t0, \$t1, -1
	daddi	\$t0, -1
	daddi	\$t0, 1

_ACEOF
      $AS $ASFLAGS conftest.s -o conftest.$OBJEXT \
	>&AS_MESSAGE_LOG_FD 2>&1		  \
	&& exec_cv_as_daddi=yes
      rm -f conftest.s conftest.$OBJEXT])
   AS_IF([test "x$exec_cv_as_daddi" != "xyes"],
     [DADDI_BROKEN=yes])
   exec_CHECK_LINUX_CLONE3
   AC_DEFINE([MIPS_NABI], [1],
    [Define to 1 if MIPS NABI calling convention is being used.])
   LOADERFLAGS="$LOADERFLAGS $LDPREFIX-Ttext=0x3e00000000"
   is_mips=yes
   exec_loader=loader-mips64el.s], [*],
  [AC_MSG_ERROR([Please port libexec to $host])])

AC_SUBST([DADDI_BROKEN])

MIPS_N32=$exec_cv_mips_nabi

AC_ARG_VAR([LOADERFLAGS], [Flags used to link the loader.])
AC_ARG_VAR([ARFLAGS], [Flags for the archiver.])
AC_ARG_VAR([ASFLAGS], [Flags for the assembler.])

# Search for seccomp headers and declarations.
AC_CHECK_HEADERS([linux/seccomp.h linux/filter.h],
  [AC_CHECK_DECLS([SECCOMP_SET_MODE_FILTER, SECCOMP_FILTER_FLAG_TSYNC, SECCOMP_RET_TRACE],
    [AC_DEFINE([HAVE_SECCOMP], [1])], [],
    [[#include <linux/seccomp.h>]])])

# Make the assembler optimize for code size.  Don't do this on MIPS,
# as the assembler code manages branch delays manually.

AC_CACHE_CHECK([whether as understands -O],
  [exec_cv_as_O],
  [exec_cv_as_O=no
   cat <<_ACEOF >conftest.s
   .section text
   .global _start
_start:

_ACEOF
   $AS $ASFLAGS -O conftest.s -o conftest.$OBJEXT \
     >&AS_MESSAGE_LOG_FD 2>&1			  \
     && exec_cv_as_O=yes
   rm -f conftest.s conftest.$OBJEXT])

AS_IF([test "$exec_cv_as_O" = "yes" \
       && test "$is_mips" != "yes"],
  [ASFLAGS="$ASFLAGS -O"])

# Make the assembler generate debug information.

AC_CACHE_CHECK([whether as understands -g],
  [exec_cv_as_g],
  [exec_cv_as_g=no
   cat <<_ACEOF >conftest.s
   .section text
   .global _start
_start:

_ACEOF
   $AS $ASFLAGS -g conftest.s -o conftest.$OBJEXT \
     >&AS_MESSAGE_LOG_FD 2>&1			  \
     && exec_cv_as_g=yes
   rm -f conftest.s conftest.$OBJEXT])
AS_IF([test "$exec_cv_as_g" = "yes"], [ASFLAGS="$ASFLAGS -g"])

# Check for the ability to automatically generate dependencies for C
# source files.
AUTO_DEPEND=no
AS_IF([test "x$GCC" = xyes],
  [AC_CACHE_CHECK([whether gcc understands -MMD -MF],
     [exec_cv_autodepend],
     [SAVE_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -MMD -MF deps.d -MP"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
	[exec_cv_autodepend=yes],
	[exec_cv_autodepend=no])
     CFLAGS="$SAVE_CFLAGS"
     test -f deps.d || emacs_cv_autodepend=no
     rm -rf deps.d])
   AS_IF([test "x$exec_cv_autodepend" = xyes],
     [AUTO_DEPEND=yes
      AS_MKDIR_P([deps])])])

# Now check for some other stuff.

AC_CACHE_CHECK([for 'find' args to delete a file],
  [exec_cv_find_delete],
  [AS_IF([touch conftest.tmp && find conftest.tmp -delete 2>/dev/null &&
      test ! -f conftest.tmp], [exec_cv_find_delete="-delete"],
   [exec_cv_find_delete="-exec rm -f {} ';'"])])
FIND_DELETE=$exec_cv_find_delete
AC_SUBST([FIND_DELETE])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AS_IF([test "x$is_mips" = xyes],
  [AC_CONFIG_FILES([config-mips.m4])])

AC_SUBST([AUTO_DEPEND])
AC_SUBST([LOADERFLAGS])
AC_SUBST([ARFLAGS])
AC_SUBST([ASFLAGS])
AC_SUBST([exec_loader])
AC_SUBST([MIPS_N32])
AC_SUBST([OBJS])
AC_SUBST([is_mips])

AC_OUTPUT
