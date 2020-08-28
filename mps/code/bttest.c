/* bttest.c: BIT TABLE TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */


#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpstd.h"

#include <stdio.h> /* fflush, fgets, printf, putchar, puts */
#include <stdlib.h> /* exit, strtol */

SRCID(bttest, "$Id$");


static BT bt; /* the BT which we will use */
static Size btSize; /* the size of the current BT */
static Arena arena; /* the arena which we use to allocate the BT */


#define MAX_ARGS 3


static Word args[MAX_ARGS];
static Count argCount;


static Bool argInRange(Index arg)
{
  if (bt == NULL) {
    printf("no BT\n");
    return FALSE;
  }
  if (args[arg] >= btSize) {
    printf("out of range\n");
    return FALSE;
  }
  return TRUE;
}


static Bool checkDefaultRange(Index arg)
{
  if (bt == NULL) {
    printf("no BT\n");
    return FALSE;
  }
  if (argCount == arg+1) {
    printf("range half-specified\n");
    return FALSE;
  }
  if (argCount == arg) { /* use default range */
    args[arg] = 0;
    args[arg+1] = btSize;
    return TRUE;
  }
  if (args[arg] >= args[arg+1]) {
    printf("range ill-formed\n");
    return FALSE;
  }
  if (args[arg+1] > btSize) {
    printf("range too high\n");
    return FALSE;
  }
  return TRUE; /* explicit valid range */
}


static void quit(void)
{
  exit(0);
}


static void destroy(void)
{
  if (bt != NULL) {
    BTDestroy(bt, arena, btSize);
    bt = NULL;
  } else {
    printf("No BT to destroy\n");
  }
}

static void create(void)
{
  Res res;
  if (args[0] < 1) {
    printf("can't create a BT of size 0\n");
    return;
  }
  if (bt != NULL)
    destroy();
  res = BTCreate(&bt, arena, args[0]);
  if (res == ResOK) {
    btSize = args[0];
    BTResRange(bt, 0, btSize);
  } else {
    printf("BTCreate returned %d\n",res);
  }
}


static void set(void)
{
  if (argInRange(0))
    (BTSet)(bt, args[0]);
}


static void reset(void)
{
  if (argInRange(0))
    (BTRes)(bt, args[0]);
}


static void get(void)
{
  if (argInRange(0)) {
    Bool b = (BTGet)(bt, args[0]);
    puts(b ? "TRUE" : "FALSE");
  }
}


static void setRange(void)
{
  if (checkDefaultRange(0))
    BTSetRange(bt, args[0], args[1]);
}


static void resetRange(void)
{
  if (checkDefaultRange(0))
    BTResRange(bt, args[0], args[1]);
}


static void isSetRange(void)
{
  if (checkDefaultRange(0)) {
    Bool b = BTIsSetRange(bt, args[0], args[1]);
    puts(b ? "TRUE" : "FALSE");
  }
}


static void isResRange(void)
{
  if (checkDefaultRange(0)) {
    Bool b = BTIsResRange(bt, args[0], args[1]);
    puts(b ? "TRUE" : "FALSE");
  }
}


static void findShortResRange(void)
{
  if (checkDefaultRange(1)) {
    if (args[0] > (args[2] - args[1])) {
      printf("can't fit length in range\n");
    } else {
      Index base, limit;
      Bool b = BTFindShortResRange(&base, &limit, bt,
                                   args[1], args[2], args[0]);
      if (b)
        printf("%"PRIuLONGEST" - %"PRIuLONGEST"\n",
               (ulongest_t)base, (ulongest_t)limit);
      else
        printf("FALSE\n");
    }
  }
}


static void findShortResRangeHigh(void)
{
  if (checkDefaultRange(1)) {
    if (args[0] > (args[2] - args[1])) {
      printf("can't fit length in range\n");
    } else {
      Index base, limit;
      Bool b = BTFindShortResRangeHigh(&base, &limit, bt,
                                       args[1], args[2], args[0]);
      if (b)
        printf("%"PRIuLONGEST" - %"PRIuLONGEST"\n",
               (ulongest_t)base, (ulongest_t)limit);
      else
        printf("FALSE\n");
    }
  }
}

static void findLongResRange(void)
{
  if (checkDefaultRange(1)) {
    if (args[0] > (args[2] - args[1])) {
      printf("can't fit length in range\n");
    } else {
      Index base, limit;
      Bool b = BTFindLongResRange(&base, &limit, bt,
                                  args[1], args[2], args[0]);
      if (b)
        printf("%"PRIuLONGEST" - %"PRIuLONGEST"\n",
               (ulongest_t)base, (ulongest_t)limit);
      else
        printf("FALSE\n");
    }
  }
}


static void help(void)
{
  printf("c <s>             create a BT of size 's'\n"
         "d                 destroy the current BT\n"
         "s <i>             set the bit index 'i'\n"
         "r <i>             reset the bit index 'i'\n"
         "g <i>             get the bit index 'i'\n");
  printf("sr [<i> <i>]      set the specified range\n"
         "rr [<i> <i>]      reset the specified range\n"
         "is [<i> <i>]      is the specified range set?\n"
         "ir [<i> <i>]      is the specified range reset?\n");
  printf("f <l> [<i> <i>]   find a reset range of length 'l'.\n"
         "fh <l> [<i> <i>]  find a reset range length 'l', working downwards\n"
         "fl <l> [<i> <i>]  find a reset range of length at least 'l'\n"
         "q                 quit\n"
         "?                 print this message\n");
  printf("\n"
         "No way of testing BTSize, BTRangesSame, or BTCopyInvertRange.\n");
}


static struct commandShapeStruct {
  const char *name;
  Count min_args;
  Count max_args;
  void (*fun)(void);
} commandShapes[] = {
  {"c", 1, 1, create},
  {"d", 0, 0, destroy},
  {"s", 1, 1, set},
  {"r", 1, 1, reset},
  {"g", 1, 1, get},
  {"sr", 0, 2, setRange},
  {"rr", 0, 2, resetRange},
  {"is", 0, 2, isSetRange},
  {"ir", 0, 2, isResRange},
  {"f", 1, 3, findShortResRange},
  {"fh", 1, 3, findShortResRangeHigh},
  {"fl", 1, 3, findLongResRange},
  {"?", 0, 0, help},
  {"q", 0, 0, quit},
  { NULL, 0, 0, NULL}
};


typedef struct commandShapeStruct *commandShape;


static void obeyCommand(const char *command)
{
  commandShape shape = commandShapes;
  while(shape->name != NULL) {
    const char *csp = shape->name;
    const char *p = command;
    while (*csp == *p) {
      csp++;
      p++;
    }
    if ((*csp == 0) && ((*p == '\n') || (*p == ' '))) { /* complete match */
      argCount = 0;
      while ((*p == ' ') && (argCount < shape->max_args)) {
        /* get an argument */
        char *newP;
        long l;
        l = strtol(p, &newP, 0);
        if(l < 0) { /* negative integer */
          printf("negative integer arguments are invalid\n");
          return;
        }
        args[argCount] = (unsigned long)l;
        if (newP == p) { /* strtoul failed */
          printf("couldn't parse an integer argument\n");
          return;
        }
        p = newP;
        ++ argCount;
      }
      if (argCount < shape->min_args) {
        printf("insufficient arguments to command\n");
      } else if (*p != '\n') {
        printf("too many arguments to command\n");
      } else { /* do the command */
        shape->fun();
      }
      return;
    } else {
      ++ shape; /* try next command */
    }
  }
  printf("command not understood\n");
  help();
}


static void showBT(void)
{
  Index i;
  char c;
  if (bt == NULL)
    return;
  i = 0;
  while((i < btSize) && (i < 50)) {
    if (i % 10 == 0)
      c = (char)(((i / 10) % 10) + '0');
    else
      c = ' ';
    putchar(c);
    ++ i;
  }
  putchar('\n');
  i = 0;
  while((i < btSize) && (i < 50)) {
    c = (char)((i % 10) +'0');
    putchar(c);
    ++ i;
  }
  putchar('\n');
  i = 0;
  while(i < btSize) {
    if (BTGet(bt,i))
      c = 'O';
    else
      c = '.';
    putchar(c);
    ++ i;
    if (i % 50 == 0)
      putchar('\n');
  }
  putchar('\n');
}


#define testArenaSIZE (((size_t)64)<<20)

int main(int argc, char *argv[])
{
  bt = NULL;
  btSize = 0;

  testlib_init(argc, argv);

  die(mps_arena_create((mps_arena_t*)&arena, mps_arena_class_vm(),
                       testArenaSIZE),
      "mps_arena_create");
  while(1) {
    char input[100];
    printf("bt test> ");
    (void)fflush(stdout);
    if (fgets(input, 100, stdin)) {
      obeyCommand(input);
      showBT();
    } else {
      return 0;
    }
  }
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
