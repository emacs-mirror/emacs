; @@@@ FIXME: W3I6MV add copyright and license
; $Id$

text SEGMENT
EXTERN TraceScanArea : PROC

StackScan PROC FRAME

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
   
   sub rsp, 40 
.allocstack 40
.endprolog

   mov r8, rdx     ; stackBot
   mov rdx, rsp    ; top of stack
   add rdx, 40     ; where last callee saved register stored
   ; mov rcx, rcx  ; ss
   call TraceScanArea
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

text ENDS
END