# ssxcpp.s: STACK SCANNING FOR MAC OS X / POWERPC
#
# .quote.why: The following is in quotes because otherwise the
# preprocessor seems to barf.
# "$Header$"
#
# Copyright (c) 2005 Ravenbrook Limited.  See end of file for license.
#
# .readership: Any MPS developer that is prepared to read PowerPC
# assembly code in GNU 'as' syntax.
#
#
# It's possible that since this is mostly PowerPC and GNU 'as' that we
# could use the same code for lippgc.  See .future.abi
#
# 
# REFERENCES
# 
# [OSXAG] "Introduction to Mac OS X Assembler Guide", 
# http://developer.apple.com/documentation/DeveloperTools/Reference/Assembler/ASMIntroduction/chapter_1_section_1.html
# http://developer.apple.com/documentation/DeveloperTools/Reference/Assembler/Assembler.pdf
#
# [PEM] "PowerPC Microprocessor Family: The Programming Environments for
# 32-bit Microprocessors", Motorola, 1997-01.  MPCFPE32B/AD REV.1
#
# [MORT] "Introduction to Mach-O Runtime Architecture",
# http://developer.apple.com/documentation/DeveloperTools/Conceptual/MachORuntime/index.html
# http://developer.apple.com/documentation/DeveloperTools/Conceptual/MachORuntime/MachORuntime.pdf
#
# [MORT] Chapter 2 is the place to go for the stack layout.  But it is
# still a bit vague.  For example, what is the exact format of the
# Linkage Area?
#
#
# Stack Layout and Register Usage
#
# Let SPEP be the value of the stack pointer (r1) on entry to the
# function.  r1 is the Stack Pointer, descending full stack (that is
# -4(r1) is the address of the next available free slot on the stack).
#
# On entry, stack looks like (numerically higher addresses appear higher
# up the page, stack grows down (towards address 0) in memory and on
# paper):
#
# |                                   |
# +-----------------------------------+
# | -- Parameter area --              |
# | Lowest addresses have leftmost    |
# | actual arguments.                 |
# | Note that some actual arguments   |
# | may be in registers.  They        |
# | still have a slot reserved in     |
# | this area, even if the value is   |
# | not written in.                   |
# +-----------------------------------+
# | -- Linkage area --                |
# | Apparently this is 24 bytes long. |
# | (See [MORT] Listing 2-1)          |
# | 12(SPEP)-23(SPEP) unknown         |
# | 8(SPEP) - optional lr             |
# | 4(SPEP) - optional cr             |
# | 0(SPEP) - previous SPEP           |
# +-----------------------------------+
# 8(SPEP) - available to store the callee's lr if desired.
# 4(SPEP) - available to store the callee's cr if desired.
# 0(SPEP) - not available.  The caller saves their SPEP here.  In other
# words, this points to the highest address of the caller's frame.  The
# callee will decrement sp and store SPEP at the new sp, thus creating a
# new linkage area in preparation for calling another function.
#
# Note that the callee can access locations -1(SPEP) to -224(SPEP)
# without changing the stack pointer.  This is called a Red Zone (see
# [MORT] Chapter 2, section "The Red Zone").  This means that we must
# overscan when scanning the stacks of other threads.
#
# [MORT] Table 2-4 specifies which registers are saved (required to be
# preserved by a callee when called, aka nonvolatile).  They are: r1,
# r13-r31, fr14-fr31, v20-v31, VRSAVE, cr2-cr4.
#
# Stack Scanner Design
#
# All we have to do is create a stack frame, push the saved registers
# onto it, and call TraceScanArea with appropriate arguments.
# We assume that of the saved registers, only the general purpose ones
# can contain roots (in other words the client doesn't store pointers in
# floating pointer registers, and other silliness).  So we'll only store
# r13 through to r31.  r1 will be stored in the callee's new linkage
# area, as per convention.  lr will be stored in the caller's linkage
# area, as per convention.  CTR (a future PC value, so a potential root)
# is volatile so cannot be a root.
#
# Future Directions
#
# .future.clear: We could poke all the stack locations that we have
# access to and set them to 0 (the ones that we are not using), just
# in case any false references are hiding there.
#
# .future.abi:
# One can imagine a sort of minimal ABI version that pushed
# all registers and only used stack space that it had reserved itself.
# We would, alas, still have to assume that r1 was SP though.
#
# To do this we would need to:
# - save more registers
# - possibly not use the caller's stack

# Exported symbols
.globl _StackScan

linkageArea = 24 ; size of linkage area to create
paramArea = 12 ; size of param area to create
# .local.size: this size is directly related to what registers we save.
localArea = 76 ; size of local workspace (for 19 registers, r13-r31)
frameSize = linkageArea + paramArea + localArea

_StackScan:
# r1 is SPEP
    mflr r0
# value of localArea depends on r13, see .local.size
    stmw r13, -localArea(r1)
    stw r0, 8(r1)
    stwu r1, -frameSize(r1)
# r1 + frameSize is SPEP

    lwz r1, 0(r1)
# r1 is SPEP again
    lwz r0, 8(r1)
    mtlr r0
# See .local.size
    lmw r13, -localArea(r1)
    li r3, 0
    blr
