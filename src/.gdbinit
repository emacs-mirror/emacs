# Copyright (C) 1992, 93, 94, 95, 96, 97, 1998, 2000, 01, 2004
#   Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# Force loading of symbols, enough to give us gdb_valbits etc.
set main

# Find lwlib source files too.
dir ../lwlib
#dir /gd/gnu/lesstif-0.89.9/lib/Xm

# Don't enter GDB when user types C-g to quit.
# This has one unfortunate effect: you can't type C-c
# at the GDB to stop Emacs, when using X.
# However, C-z works just as well in that case.
handle 2 noprint pass

# Don't pass SIGALRM to Emacs.  This makes problems when
# debugging.
handle SIGALRM ignore

# Set up a mask to use.
# This should be EMACS_INT, but in some cases that is a macro.
# long ought to work in all cases right now.

define xgetptr
  set $ptr = (gdb_use_union ? $arg0.u.val : $arg0 & $valmask) | gdb_data_seg_bits
end

define xgetint
  set $int = gdb_use_union ? $arg0.s.val : (gdb_use_lsb ? $arg0 : $arg0 << gdb_gctypebits) >> gdb_gctypebits
end

define xgettype
  set $type = gdb_use_union ? $arg0.s.type : (enum Lisp_Type) (gdb_use_lsb ? $arg0 & $tagmask : $arg0 >> gdb_valbits)
end

# Set up something to print out s-expressions.
define pr
  set debug_print ($)
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

define xtype
  xgettype $
  output $type
  echo \n
  if $type == Lisp_Misc
    xmisctype
  else
    if $type == Lisp_Vectorlike
      xvectype
    end
  end
end
document xtype
Print the type of $, assuming it is an Emacs Lisp value.
If the first type printed is Lisp_Vector or Lisp_Misc,
a second line gives the more precise type.
end

define xvectype
  xgetptr $
  set $size = ((struct Lisp_Vector *) $ptr)->size
  output ($size & PVEC_FLAG) ? (enum pvec_type) ($size & PVEC_TYPE_MASK) : $size
  echo \n
end
document xvectype
Print the size or vector subtype of $, assuming it is a vector or pseudovector.
end

define xmisctype
  xgetptr $
  output (enum Lisp_Misc_Type) (((struct Lisp_Free *) $ptr)->type)
  echo \n
end
document xmisctype
Print the specific type of $, assuming it is some misc type.
end

define xint
  xgetint $
  print $int
end
document xint
Print $, assuming it is an Emacs Lisp integer.  This gets the sign right.
end

define xptr
  xgetptr $
  print (void *) $ptr
end
document xptr
Print the pointer portion of $, assuming it is an Emacs Lisp value.
end

define xmarker
  xgetptr $
  print (struct Lisp_Marker *) $ptr
end
document xmarker
Print $ as a marker pointer, assuming it is an Emacs Lisp marker value.
end

define xoverlay
  xgetptr $
  print (struct Lisp_Overlay *) $ptr
end
document xoverlay
Print $ as a overlay pointer, assuming it is an Emacs Lisp overlay value.
end

define xmiscfree
  xgetptr $
  print (struct Lisp_Free *) $ptr
end
document xmiscfree
Print $ as a misc free-cell pointer, assuming it is an Emacs Lisp Misc value.
end

define xintfwd
  xgetptr $
  print (struct Lisp_Intfwd *) $ptr
end
document xintfwd
Print $ as an integer forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xboolfwd
  xgetptr $
  print (struct Lisp_Boolfwd *) $ptr
end
document xboolfwd
Print $ as a boolean forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xobjfwd
  xgetptr $
  print (struct Lisp_Objfwd *) $ptr
end
document xobjfwd
Print $ as an object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbufobjfwd
  xgetptr $
  print (struct Lisp_Buffer_Objfwd *) $ptr
end
document xbufobjfwd
Print $ as a buffer-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xkbobjfwd
  xgetptr $
  print (struct Lisp_Kboard_Objfwd *) $ptr
end
document xkbobjfwd
Print $ as a kboard-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbuflocal
  xgetptr $
  print (struct Lisp_Buffer_Local_Value *) $ptr
end
document xbuflocal
Print $ as a buffer-local-value pointer, assuming it is an Emacs Lisp Misc value.
end

define xsymbol
  xgetptr $
  print (struct Lisp_Symbol *) $ptr
  xprintsym $
  echo \n
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Emacs Lisp symbol value.
end

define xstring
  xgetptr $
  print (struct Lisp_String *) $ptr
  output ($->size > 1000) ? 0 : ($->data[0])@($->size_byte < 0 ? $->size : $->size_byte)
  echo \n
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Emacs Lisp string value.
end

define xvector
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->size > 50) ? 0 : ($->contents[0])@($->size)
echo \n
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Emacs Lisp vector value.
end

define xprocess
  xgetptr $
  print (struct Lisp_Process *) $ptr
  output *$
  echo \n
end
document xprocess
Print the address of the struct Lisp_process which the Lisp_Object $ points to.
end

define xframe
  xgetptr $
  print (struct frame *) $ptr
end
document xframe
Print $ as a frame pointer, assuming it is an Emacs Lisp frame value.
end

define xcompiled
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->contents[0])@($->size & 0xff)
end
document xcompiled
Print $ as a compiled function pointer, assuming it is an Emacs Lisp compiled value.
end

define xwindow
  xgetptr $
  print (struct window *) $ptr
  printf "%dx%d+%d+%d\n", $->width, $->height, $->left, $->top
end
document xwindow
Print $ as a window pointer, assuming it is an Emacs Lisp window value.
Print the window's position as "WIDTHxHEIGHT+LEFT+TOP".
end

define xwinconfig
  xgetptr $
  print (struct save_window_data *) $ptr
end
document xwinconfig
Print $ as a window configuration pointer, assuming it is an Emacs Lisp window configuration value.
end

define xsubr
  xgetptr $
  print (struct Lisp_Subr *) $ptr
  output *$
  echo \n
end
document xsubr
Print the address of the subr which the Lisp_Object $ points to.
end

define xchartable
  xgetptr $
  print (struct Lisp_Char_Table *) $ptr
  printf "Purpose: "
  xprintsym $->purpose
  printf "  %d extra slots", ($->size & 0x1ff) - 388
  echo \n
end
document xchartable
Print the address of the char-table $, and its purpose.
This command assumes that $ is an Emacs Lisp char-table value.
end

define xboolvector
  xgetptr $
  print (struct Lisp_Bool_Vector *) $ptr
  output ($->size > 256) ? 0 : ($->data[0])@(($->size + 7)/ 8)
  echo \n
end
document xboolvector
Print the contents and address of the bool-vector $.
This command assumes that $ is an Emacs Lisp bool-vector value.
end

define xbuffer
  xgetptr $
  print (struct buffer *) $ptr
  xgetptr $->name
  output ((struct Lisp_String *) $ptr)->data
  echo \n
end
document xbuffer
Set $ as a buffer pointer, assuming it is an Emacs Lisp buffer value.
Print the name of the buffer.
end

define xhashtable
  xgetptr $
  print (struct Lisp_Hash_Table *) $ptr
end
document xhashtable
Set $ as a hash table pointer, assuming it is an Emacs Lisp hash table value.
end

define xcons
  xgetptr $
  print (struct Lisp_Cons *) $ptr
  output/x *$
  echo \n
end
document xcons
Print the contents of $, assuming it is an Emacs Lisp cons.
end

define nextcons
  p $.cdr
  xcons
end
document nextcons
Print the contents of the next cell in a list.
This assumes that the last thing you printed was a cons cell contents
(type struct Lisp_Cons) or a pointer to one.
end
define xcar
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->car : 0)
end
document xcar
Print the car of $, assuming it is an Emacs Lisp pair.
end

define xcdr
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->cdr : 0)
end
document xcdr
Print the cdr of $, assuming it is an Emacs Lisp pair.
end

define xfloat
  xgetptr $
  print ((struct Lisp_Float *) $ptr)->data
end
document xfloat
Print $ assuming it is a lisp floating-point number.
end

define xscrollbar
  xgetptr $
  print (struct scrollbar *) $ptr
output *$
echo \n
end
document xscrollbar
Print $ as a scrollbar pointer.
end

define xprintsym
  xgetptr $arg0
  set $sym = (struct Lisp_Symbol *) $ptr
  xgetptr $sym->xname
  set $sym_name = (struct Lisp_String *) $ptr
  output ($sym_name->data[0])@($sym_name->size_byte < 0 ? $sym_name->size : $sym_name->size_byte)
end
document xprintsym
  Print argument as a symbol.
end

define xbacktrace
  set $bt = backtrace_list
  while $bt
    xgettype (*$bt->function)
    if $type == Lisp_Symbol
      xprintsym (*$bt->function)
      echo \n
    else
      printf "0x%x ", *$bt->function
      if $type == Lisp_Vectorlike
	xgetptr (*$bt->function)
        set $size = ((struct Lisp_Vector *) $ptr)->size
        output ($size & PVEC_FLAG) ? (enum pvec_type) ($size & PVEC_TYPE_MASK) : $size
      else
        printf "Lisp type %d", $type
      end
      echo \n
    end
    set $bt = $bt->next
  end
end
document xbacktrace
  Print a backtrace of Lisp function calls from backtrace_list.
  Set a breakpoint at Fsignal and call this to see from where
  an error was signaled.
end

define xreload
  set $tagmask = (((long)1 << gdb_gctypebits) - 1)
  set $valmask = gdb_use_lsb ? ~($tagmask) : ((long)1 << gdb_valbits) - 1
end
document xreload
  When starting Emacs a second time in the same gdb session under
  FreeBSD 2.2.5, gdb 4.13, $valmask have lost
  their values.  (The same happens on current (2000) versions of GNU/Linux
  with gdb 5.0.)
  This function reloads them.
end
xreload

define hook-run
  xreload
end

# Call xreload if a new Emacs executable is loaded.
define hookpost-run
  xreload
end

set print pretty on
set print sevenbit-strings

show environment DISPLAY
show environment TERM
set args -geometry 80x40+0+0

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command above as well.
break abort

# If we are running in synchronous mode, we want a chance to look around
# before Emacs exits.  Perhaps we should put the break somewhere else
# instead...
break x_error_quitter

# arch-tag: 12f34321-7bfa-4240-b77a-3cd3a1696dfe
