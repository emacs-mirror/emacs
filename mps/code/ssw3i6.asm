; ssw3i6.asm : WIN32/x64 STACK SCANNER
;
; $Id$
; Copyright (c) 2012 Ravenbrook Limited.  See end of file for license.
; Portions copyright (C) 2002 Global Graphics Software.

; For register usage see 
; MSDN -> x64 Software Conventions -> Register Usage
; <http://msdn.microsoft.com/en-us/library/9z1stfyw>

.CODE
EXTERN TraceScanAreaTagged : PROC

StackScan PROC FRAME
   ; Prolog follows.  See 
   ; MSDN -> x64 Software Conventions -> Prolog and Epilog
   ; <http://msdn.microsoft.com/en-us/library/tawsa7cb>
   ; Also see 
   ; MSDN -> Microsoft Macro Assembler Reference -> Directives Reference
   ; <http://msdn.microsoft.com/en-us/library/8t163bt0>
   ; for the use of .pushreg and other directives.
   ; Push the callee-save registers.   
   ; Stack must be 16-byte aligned; the return address is on top of the stack;
   ; after pushing another register it will be 16-byte aligned again.
   push rdi
.pushreg rdi

   push rsi
.pushreg rsi
   push rbx
.pushreg rbx
   
   push rbp
.pushreg rbp
   push r12
.pushreg r12
   
   push r13
.pushreg r13
   push r14
.pushreg r14
   
   push r15
.pushreg r15
   ; An odd number of qwords have been pushed on the stack (including the
   ; return address) so we need to subtract an extra 8 bytes on top of the
   ; 4 qwords we need for to reserve for the register parameter stack area.
   ; See
   ; MSDN -> x64 Software Conventions -> Stack Usage -> Stack Allocation
   ; <http://msdn.microsoft.com/en-us/library/ew5tede7> 
   sub rsp, 40 
.allocstack 40
.endprolog
   ; for convenience set up arguments in reverse order.
   mov r8, rdx     ; stackBot
   mov rdx, rsp    ; top of stack
   add rdx, 40     ; where last callee-save register stored
   ; mov rcx, rcx  ; ss already in the right register.
   call TraceScanAreaTagged
   add rsp, 40
   pop r15         ; pop the callee-save registers
   pop r14
   pop r13
   pop r12
   pop rbp
   pop rbx
   pop rsi
   pop rdi
   ret

StackScan ENDP

END
; C. COPYRIGHT AND LICENSE
;
; Copyright (C) 2001-2012 Ravenbrook Limited <http://www.ravenbrook.com/>.
; All rights reserved.  This is an open source license.  Contact
; Ravenbrook for commercial licensing options.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
; 
; 1. Redistributions of source code must retain the above copyright
; notice, this list of conditions and the following disclaimer.
; 
; 2. Redistributions in binary form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
; 
; 3. Redistributions in any form must be accompanied by information on how
; to obtain complete source code for this software and any accompanying
; software that uses this software.  The source code must either be
; included in the distribution or be available for no more than the cost
; of distribution plus a nominal fee, and must be freely redistributable
; under reasonable conditions.  For an executable file, complete source
; code means the source code for all modules it contains. It does not
; include source code for modules or files that typically accompany the
; major components of the operating system on which the executable file
; runs.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
; PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

