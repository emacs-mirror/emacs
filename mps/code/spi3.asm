;  impl.asm.spi3: STACK PROBE
;
;  $Id$
;  $HopeName: MMsrc!spi3.asm(trunk.2) $
;  Copyright (c) 2001 Ravenbrook Limited.
;
;  This function reads a location that is probeDepth words beyond
;  the current stack pointer.  On intel platforms the stack grows
;  downwards so this means reading from a location with a lesser address.
;
;  The registers edi, esi, ebx are the registers defined to be preserved
;  across function calls, so we do not use those.

.386
.model flat
.code

_StackProbe proc public ; (Size probeDepth)
  push ebp              ; frame pointer
  mov  ebp,esp
  mov  eax, [ebp+08]
  neg  eax
  mov  eax, [esp+eax*4]   ; do the actual probe
  leave
  ret                   ; return
_StackProbe endp

end
