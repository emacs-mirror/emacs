/*  impl.c.teletest: TELEMETRY TEST
 *
 *  $HopeName: !teletest.c(trunk.4) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .source: The command parser here was taken and adapted from bttest.c.
 */


#include <stdio.h>
#include <stdlib.h>

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"

#ifdef MPS_OS_SU
#include "ossu.h"
#endif /* MPS_OS_SU */


SRCID(teletest, "$HopeName: !teletest.c(trunk.4) $");


static mps_arena_t arena;


#define MAX_ARGS 3
#define INPUT_BUFFER_SIZE 512

#if (MPS_WORD_WIDTH == 32)
#define WORD_FORMAT "0x%08lx"
#elif (MPS_WORD_WIDTH == 64)
#define WORD_FORMAT "0x%016lx"
#else
#error "Unrecognized word width"
#endif

static mps_word_t args[MAX_ARGS];
static char *stringArg;
static Count argCount;

static void callControl(mps_word_t reset, mps_word_t flip)
{
  mps_word_t old, new;
  old = mps_telemetry_control(reset, flip);
  new = mps_telemetry_control((mps_word_t)0, (mps_word_t)0);

  (void)printf(WORD_FORMAT " -> " WORD_FORMAT "\n", old, new);
}

static void doControl(void)
{
  callControl(args[0], args[1]);
}

static void doRead(void)
{
  mps_word_t old;
  old = mps_telemetry_control((mps_word_t)0, (mps_word_t)0);

  (void)printf(WORD_FORMAT "\n", old);
}


static void doSet(void)
{
  callControl(args[0], args[0]);
}

static void doReset(void)
{
  callControl(args[0], (mps_word_t)0);
}

static void doFlip(void)
{
  callControl((mps_word_t)0, args[0]);
}

static void doIntern(void)
{
  mps_word_t id;

  id = mps_telemetry_intern(stringArg);
  (void)printf(WORD_FORMAT "\n", id);
}

static void doLabel(void)
{
  mps_telemetry_label((mps_addr_t)args[0], args[1]);
}

static void doFlush(void)
{
  mps_telemetry_flush();
}

static void doQuit(void)
{
  mps_arena_destroy(arena);
  exit(0);
}


static void doHelp(void)
{
  (void)printf("control <reset> <flip>   -> <old> <new>    Control filter\n"
               "read                     -> <old>          Read filter\n"
               "set <mask>               -> <old> <new>    Set filter\n"
               "reset <mask>             -> <old> <new>    Reset filter\n"
               "flip <mask>              -> <old> <new>    Toggle filter\n"
               "intern <string>          -> <id>           Intern string\n"
               "label <address> <id>                       Label address\n"
               "flush                                      Flush buffer\n"
               "help                                       Print this message\n"
               "quit                                       Quit\n");
}


static struct commandShapeStruct {
  char *name;
  Count int_args;
  mps_bool_t string_arg;
  void (*fun)(void);
} commandShapes[] = {
  {"control", 2, 0, doControl},
  {"read", 0, 0, doRead},
  {"set", 1, 0, doSet},
  {"reset", 1, 0, doReset},
  {"flip", 1, 0, doFlip},
  {"intern", 0, 1, doIntern},
  {"label", 2, 0, doLabel},
  {"flush", 0, 0, doFlush},
  {"help", 0, 0, doHelp},
  {"quit", 0, 0, doQuit},
  {NULL, 0, 0, NULL}
};


typedef struct commandShapeStruct *commandShape;


static void obeyCommand(char *command)
{
  commandShape shape = commandShapes;
  while(shape->name != NULL) {
    char *csp = shape->name;
    char *p = command;
    while (*csp == *p) {
      csp++;
      p++;
    }
    if ((*csp == 0) && ((*p == '\n') || (*p == ' '))) { /* complete match */
      argCount = 0;
      while ((*p == ' ') && (argCount < shape->int_args)) {
        /* get an argument */
        char *newP;
	mps_word_t word;
        word = (mps_word_t)strtoul(p, &newP, 0);
        args[argCount] = word;
        if (newP == p) { /* strtoul failed */
          printf("couldn't parse an integer argument\n");
          return;
        }
        p = newP;
        ++ argCount;
      }
      if(shape->string_arg) {
	char *end;
	while(*p == ' ')
	  ++p;
        for(end = p; *end != '\n'; end++)
	  NOOP;
        *end = '\0';
	stringArg = p;
      } else {
	stringArg = NULL;
      }
      if (argCount < shape->int_args) {
        printf("insufficient arguments to command\n");
      } else if (*p != '\n' && stringArg == NULL) {
        printf("too many arguments to command\n");
      } else { /* do the command */
        shape->fun();
      }
      return;
    } else {
      ++ shape; /* try next command */
    }
  }
  printf("command not understood\n> %s\n", command);
  doHelp();
}


extern int main(int argc, char *argv[])
{
  mps_res_t res;

  testlib_unused(argc);
  testlib_unused(argv);

  res = mps_arena_create(&arena,
                         mps_arena_class_vm());
  if (res != MPS_RES_OK) {
    printf("failed to create VM arena.\n");
    return 1;
  }
  doHelp();
  while(1) {
    char input[INPUT_BUFFER_SIZE];
    printf("telemetry test> ");
    fflush(stdout);
    if (fgets(input, INPUT_BUFFER_SIZE , stdin)) {
      obeyCommand(input);
    } else {
      doQuit();
    }
  }
}
