/* noaslr.c: Disable ASLR on OS X Mavericks
 *
 * $Id: //info.ravenbrook.com/project/mps/master/code/eventcnv.c#26 $
 * Copyright (c) 2014-2020 Ravenbrook Limited. See end of file for license.
 *
 * This is a command-line tool that runs another program with address
 * space layout randomization (ASLR) disabled.
 *
 * The technique is taken from GDB via "How gdb disables ASLR in Mac
 * OS X Lion"
 * <http://reverse.put.as/2011/08/11/how-gdb-disables-aslr-in-mac-os-x-lion/>
 *
 * On OS X Mavericks, the _POSIX_SPAWN_DISABLE_ASLR constant is not
 * defined in any header, but the LLDB sources reveal its value, and
 * experimentally this value works.
 * <https://llvm.org/svn/llvm-project/lldb/trunk/tools/darwin-debug/darwin-debug.cpp>
 */

#include <errno.h>
#include <spawn.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef _POSIX_SPAWN_DISABLE_ASLR
#define _POSIX_SPAWN_DISABLE_ASLR 0x100
#endif

int main(int argc, char **argv)
{
  extern char **environ;
  pid_t pid;
  posix_spawnattr_t attr;
  int res, status = 1;
  const char *program = argv[0];
  char *default_argv[] = {"/bin/sh", NULL};

  if (argc >= 2)
    ++ argv;
  else
    argv = default_argv;

  res = posix_spawnattr_init(&attr);
  if (res != 0) {
    errno = res;
    perror(program);
    return res;
  }

  res = posix_spawnattr_setflags(&attr, _POSIX_SPAWN_DISABLE_ASLR);
  if (res != 0) {
    errno = res;
    perror(program);
    return res;
  }

  res = posix_spawn(&pid, argv[0], NULL, &attr, argv, environ);
  if (res != 0) {
    errno = res;
    perror(program);
    return res;
  }

  if (waitpid(pid, &status, 0) == -1)
    return 1;

  if (!WIFEXITED(status))
    return 1;

  return WEXITSTATUS(status);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
