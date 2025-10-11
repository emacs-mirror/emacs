/* prmctest.c: TEST INSTRUCTION EMULATION
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "prmc.h"
#include "prmcix.h"
#include "testlib.h"

#include <stdio.h>

#if defined MPS_PF_LII6GC && !defined CONFIG_PF_ANSI

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define VERBOSE 0

#pragma GCC diagnostic ignored "-Wpedantic"

static void **asmcode(int) __attribute__((optimize(0)));
static void **asmcode(int f)
{
  static void *labels[] = {&&start, &&end};
  if (f)
    return labels;
start:
  __asm__(
      "mov (%rax),%rax\n"
      "mov (%rcx),%r15\n"
      "mov (%r12),%rdx\n"
      "mov (%r13),%r8\n"
      "mov mem(%rip),%r8\n"
      "mov 0x8(%r14),%rsi\n"
      "mov 0xabcdef(%rax),%rax\n"
      "mov 0x1(%r12),%r8\n"
      "mov (%rax,%rdx,8),%rsi\n"
      "mov (%r8,%r9,1),%r8\n"
      "mov (%r8,%r12,2),%r8\n"
      "mov 0x10(%rax,%rdx,8),%rsi\n"
      /* TODO: "movq (%rax),%xmm1\n */
  );
end:
  goto start;
}


static mps_word_t mem[8];

static void testMovRm64ToR64(MutatorContext ctx, size_t srcreg,
                             size_t disp, size_t scale, size_t idxreg,
                             size_t dstreg, size_t len)
{
  mps_word_t *gregs = (void *)ctx->ucontext->uc_mcontext.gregs;
  siginfo_t *siginfo = ctx->info;
  size_t ip0 = gregs[REG_RIP];
  const size_t some_value = (size_t)rnd_addr();
  memset(gregs, 0, sizeof(ctx->ucontext->uc_mcontext.gregs));
  memset(mem, 0, sizeof(mem));
  gregs[REG_RIP] = ip0;
  if (scale) {
    assert(idxreg != srcreg);
    gregs[idxreg] = rnd();
  }
  if (srcreg != REG_RIP) {
    gregs[srcreg]
        = (size_t)((Byte *)mem - disp - gregs[idxreg] * scale);
  }
  siginfo->si_addr = mem;
  mem[0] = some_value;
  if (!(dstreg == srcreg || (scale && dstreg == idxreg)))
    assert(gregs[dstreg] == 0);
  assert(gregs[dstreg] != some_value);
  assert(MutatorContextCanStepInstruction(ctx));
  assert(MutatorContextStepInstruction(ctx) == MPS_RES_OK);
  assert(gregs[dstreg] == some_value);
  assert(mem[0] == some_value);
  assert(gregs[REG_RIP] - ip0 == len);
}

struct regname {
  size_t num;
  const char *name;
};

static struct regname const gprs[] = {
    {REG_R8, "%r8"},   {REG_R9, "%r9"},   {REG_R10, "%r10"},
    {REG_R11, "%r11"}, {REG_R12, "%r12"}, {REG_R13, "%r13"},
    {REG_R14, "%r14"}, {REG_R15, "%r15"}, {REG_RDI, "%rdi"},
    {REG_RSI, "%rsi"}, {REG_RBP, "%rbp"}, {REG_RBX, "%rbx"},
    {REG_RDX, "%rdx"}, {REG_RAX, "%rax"}, {REG_RCX, "%rcx"},
    {REG_RSP, "%rsp"}, {REG_RIP, "%rip"},
};

static const char *regname(size_t regnum)
{
  size_t i;
  for (i = 0; i < NELEMS(gprs); i++)
    if (gprs[i].num == regnum)
      return gprs[i].name;
  assert(0);
}

static char *open_tmpfile(const char *mode, FILE **f)
{
  static const char *template = "/tmp/prmc-XXXXXX";
  char buf[32];
  int fd;
  strcpy(buf, template);
  fd = mkstemp(buf);
  assert(fd != -1);
  *f = fdopen(fd, mode);
  assert(*f != NULL);
  return strdup(buf);
}

static Byte *genCode(const char *txt, size_t *len)
{
  FILE *a, *o, *s;
  char *aname = open_tmpfile("wb", &a);
  char *oname = open_tmpfile("rb", &o);
  char *sname = open_tmpfile("rb", &s);
  char cmd[256];
  size_t size, offset;
  Byte *code;
  if (VERBOSE)
    fprintf(stderr, "%s", txt);
  fprintf(a, "%s", txt);
  fflush(a);
  snprintf(cmd, sizeof cmd,
           "as %s -o %s"
           " && objdump -h %s "
           " | awk '/text/ "
           " { print $2 \" size: \" $3 \" offset: \" $6 }'"
           " >%s",
           aname, oname, oname, sname);
  assert(system(cmd) == 0);
  assert(fscanf(s, ".text size: %lx offset: %lx\n", &size, &offset)
         == 2);
  assert(fseek(o, offset, SEEK_SET) == 0);
  code = malloc(size);
  assert(code);
  assert(fread(code, 1, size, o) == size);
  remove(aname);
  remove(oname);
  remove(sname);
  free(aname);
  free(oname);
  free(sname);
  fclose(a);
  fclose(o);
  fclose(s);
  *len = size;
  return code;
}

static void genTest(MutatorContext ctx, size_t base, int32_t disp,
                    size_t scale, size_t index, size_t dst)
{
  char buf[1024];
  size_t len;
  Byte *code;
  if (scale)
    snprintf(buf, sizeof buf, "mov %d(%s,%s,%ld),%s\n", disp,
             regname(base), regname(index), scale, regname(dst));
  else
    snprintf(buf, sizeof buf, "mov %d(%s),%s\n", disp, regname(base),
             regname(dst));
  code = genCode(buf, &len);
  ctx->ucontext->uc_mcontext.gregs[REG_RIP] = (size_t)code;
  testMovRm64ToR64(ctx, base, disp, scale, index, dst, len);
}

static int32_t displacemts[] = {0, -1, 255, -256, 256, 0xabcdef01};

static Bool filterRandomly(double probability)
{
  return probability < rnd_double();
}

static void genTests(MutatorContext ctx)
{
  const size_t NSCALEBITS = 3;
  const size_t NTESTS = NELEMS(gprs) * (NSCALEBITS + 1) * NELEMS(gprs)
                        * NELEMS(gprs) * NELEMS(displacemts);
  const double probability = (double)500 / NTESTS;
  size_t b, sbits, i, d, e;
  for (b = 0; b < NELEMS(gprs); b++)
    for (sbits = 0; sbits < NSCALEBITS + 1; sbits++)
      for (i = 0; i < NELEMS(gprs); i++)
        for (d = 0; d < NELEMS(gprs); d++) {
          for (e = 0; e < NELEMS(displacemts); e++) {
            size_t base = gprs[b].num;
            size_t index = gprs[i].num;
            size_t dst = gprs[d].num;
            int32_t disp = displacemts[e];
            size_t scale = sbits ? (1 << sbits) : 0;
            if (dst == REG_RIP || (!sbits && i)
                || (sbits && base == index) || index == REG_RSP
                || base == REG_RIP || index == REG_RIP
                || filterRandomly(probability))
              continue;
            genTest(ctx, base, disp, scale, index, dst);
          }
        }
}

static void runTests(void)
{
  siginfo_t siginfo;
  struct ucontext_t ucontext;
  struct MutatorContextStruct ctx;
  MutatorContextInitFault(&ctx, &siginfo, &ucontext);
  ucontext.uc_mcontext.gregs[REG_RIP] = (size_t)(asmcode(1)[0]);

  testMovRm64ToR64(&ctx, REG_RAX, 0, 0, 0, REG_RAX, 3);
  testMovRm64ToR64(&ctx, REG_RCX, 0, 0, 0, REG_R15, 3);
  testMovRm64ToR64(&ctx, REG_R12, 0, 0, 0, REG_RDX, 4);
  testMovRm64ToR64(&ctx, REG_R13, 0, 0, 0, REG_R8, 4);
  testMovRm64ToR64(&ctx, REG_RIP, 8, 0, 0, REG_R8, 7);
  testMovRm64ToR64(&ctx, REG_R14, 8, 0, 0, REG_RSI, 4);
  testMovRm64ToR64(&ctx, REG_RAX, 0xabcdef, 0, 0, REG_RAX, 7);
  testMovRm64ToR64(&ctx, REG_R12, 0x1, 0, 0, REG_R8, 5);
  testMovRm64ToR64(&ctx, REG_RAX, 0, 8, REG_RDX, REG_RSI, 4);
  testMovRm64ToR64(&ctx, REG_R8, 0, 1, REG_R9, REG_R8, 4);
  testMovRm64ToR64(&ctx, REG_R8, 0, 2, REG_R12, REG_R8, 4);
  testMovRm64ToR64(&ctx, REG_RAX, 0x10, 8, REG_RDX, REG_RSI, 5);

  genTests(&ctx);
}

#endif

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

#if defined MPS_PF_LII6GC && !defined CONFIG_PF_ANSI
  runTests();
#endif

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
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
