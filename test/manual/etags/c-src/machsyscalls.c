/* Aliases for basic Mach system calls:
   mach_task_self -> __mach_task_self, etc.  */

#include <gnu-stabs.h>

#define	SYSCALL(name, number, type, args, typed_args) \
  function_alias (name, __##name, type, args, \
		  name typed_args)

#include "mach_syscalls.h"
