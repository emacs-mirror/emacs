# Force loading of symbols, enough to give us gdb_valbits etc.
set main

# Find lwlib source files too.
dir ../lwlib

# Don't enter GDB when user types C-g to quit.
# This has one unfortunate effect: you can't type C-c
# at the GDB to stop Emacs, when using X.
# However, C-z works just as well in that case.
handle 2 noprint pass

# Set up a mask to use.
# This should be EMACS_INT, but in some cases that is a macro.
# long ought to work in all cases right now.
set $valmask = ((long)1 << gdb_valbits) - 1
set $nonvalbits = gdb_emacs_intbits - gdb_valbits

# Set up something to print out s-expressions.
define pr
set debug_print ($)
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

define xtype
output (enum Lisp_Type) (($ >> gdb_valbits) & 0x7)
echo \n
output ((($ >> gdb_valbits) & 0x7) == Lisp_Misc ? (enum Lisp_Misc_Type) (((struct Lisp_Free *) (($ & $valmask) | gdb_data_seg_bits))->type) : (($ >> gdb_valbits) & 0x7) == Lisp_Vectorlike ? ($size = ((struct Lisp_Vector *) (($ & $valmask) | gdb_data_seg_bits))->size, (enum pvec_type) (($size & PVEC_FLAG) ? $size & PVEC_TYPE_MASK : 0)) : 0)
echo \n
end
document xtype
Print the type of $, assuming it is an Emacs Lisp value.
If the first type printed is Lisp_Vector or Lisp_Misc,
the second line gives the more precise type.
Otherwise the second line doesn't mean anything.
end

define xvectype
set $size = ((struct Lisp_Vector *) (($ & $valmask) | gdb_data_seg_bits))->size
output (enum pvec_type) (($size & PVEC_FLAG) ? $size & PVEC_TYPE_MASK : 0)
echo \n
end
document xvectype
Print the vector subtype of $, assuming it is a vector or pseudovector.
end

define xmisctype
output (enum Lisp_Misc_Type) (((struct Lisp_Free *) (($ & $valmask) | gdb_data_seg_bits))->type)
echo \n
end
document xmisctype
Print the specific type of $, assuming it is some misc type.
end

define xint
print (($ & $valmask) << $nonvalbits) >> $nonvalbits
end
document xint
Print $, assuming it is an Emacs Lisp integer.  This gets the sign right.
end

define xptr
print (void *) (($ & $valmask) | gdb_data_seg_bits)
end
document xptr
Print the pointer portion of $, assuming it is an Emacs Lisp value.
end

define xmarker
print (struct Lisp_Marker *) (($ & $valmask) | gdb_data_seg_bits)
end
document xmarker
Print $ as a marker pointer, assuming it is an Emacs Lisp marker value.
end

define xoverlay
print (struct Lisp_Overlay *) (($ & $valmask) | gdb_data_seg_bits)
end
document xoverlay
Print $ as a overlay pointer, assuming it is an Emacs Lisp overlay value.
end

define xmiscfree
print (struct Lisp_Free *) (($ & $valmask) | gdb_data_seg_bits)
end
document xmiscfree
Print $ as a misc free-cell pointer, assuming it is an Emacs Lisp Misc value.
end

define xintfwd
print (struct Lisp_Intfwd *) (($ & $valmask) | gdb_data_seg_bits)
end
document xintfwd
Print $ as an integer forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xboolfwd
print (struct Lisp_Boolfwd *) (($ & $valmask) | gdb_data_seg_bits)
end
document xboolfwd
Print $ as a boolean forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xobjfwd
print (struct Lisp_Objfwd *) (($ & $valmask) | gdb_data_seg_bits)
end
document xobjfwd
Print $ as an object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbufobjfwd
print (struct Lisp_Buffer_Objfwd *) (($ & $valmask) | gdb_data_seg_bits)
end
document xbufobjfwd
Print $ as a buffer-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xkbobjfwd
print (struct Lisp_Kboard_Objfwd *) (($ & $valmask) | gdb_data_seg_bits)
end
document xkbobjfwd
Print $ as a kboard-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbuflocal
print (struct Lisp_Buffer_Local_Value *) (($ & $valmask) | gdb_data_seg_bits)
end
document xbuflocal
Print $ as a buffer-local-value pointer, assuming it is an Emacs Lisp Misc value.
end

define xsymbol
print (struct Lisp_Symbol *) ((((int) $) & $valmask) | gdb_data_seg_bits)
output (char*)$->name->data
echo \n
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Emacs Lisp symbol value.
end

define xstring
print (struct Lisp_String *) (($ & $valmask) | gdb_data_seg_bits)
output ($->size > 1000) ? 0 : ($->data[0])@($->size_byte < 0 ? $->size : $->size_byte)
echo \n
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Emacs Lisp string value.
end

define xvector
print (struct Lisp_Vector *) (($ & $valmask) | gdb_data_seg_bits)
output ($->size > 50) ? 0 : ($->contents[0])@($->size)
echo \n
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Emacs Lisp vector value.
end

define xprocess
print (struct Lisp_Process *) (($ & $valmask) | gdb_data_seg_bits)
output *$
echo \n
end
document xprocess
Print the address of the struct Lisp_process which the Lisp_Object $ points to.
end

define xframe
print (struct frame *) (($ & $valmask) | gdb_data_seg_bits)
end
document xframe
Print $ as a frame pointer, assuming it is an Emacs Lisp frame value.
end

define xcompiled
print (struct Lisp_Vector *) (($ & $valmask) | gdb_data_seg_bits)
output ($->contents[0])@($->size & 0xff)
end
document xcompiled
Print $ as a compiled function pointer, assuming it is an Emacs Lisp compiled value.
end

define xwindow
print (struct window *) (($ & $valmask) | gdb_data_seg_bits)
printf "%dx%d+%d+%d\n", $->width, $->height, $->left, $->top
end
document xwindow
Print $ as a window pointer, assuming it is an Emacs Lisp window value.
Print the window's position as "WIDTHxHEIGHT+LEFT+TOP".
end

define xwinconfig
print (struct save_window_data *) (($ & $valmask) | gdb_data_seg_bits)
end
document xwinconfig
Print $ as a window configuration pointer, assuming it is an Emacs Lisp window configuration value.
end

define xsubr
print (struct Lisp_Subr *) (($ & $valmask) | gdb_data_seg_bits)
output *$
echo \n
end
document xsubr
Print the address of the subr which the Lisp_Object $ points to.
end

define xchartable
print (struct Lisp_Char_Table *) (($ & $valmask) | gdb_data_seg_bits)
printf "Purpose: "
output (char*)&((struct Lisp_Symbol *) ((((int) $->purpose) & $valmask) | gdb_data_seg_bits))->name->data
printf "  %d extra slots", ($->size & 0x1ff) - 388
echo \n
end
document xchartable
Print the address of the char-table $, and its purpose.
This command assumes that $ is an Emacs Lisp char-table value.
end

define xboolvector
print (struct Lisp_Bool_Vector *) (($ & $valmask) | gdb_data_seg_bits)
output ($->size > 256) ? 0 : ($->data[0])@(($->size + 7)/ 8)
echo \n
end
document xboolvector
Print the contents and address of the bool-vector $.
This command assumes that $ is an Emacs Lisp bool-vector value.
end

define xbuffer
print (struct buffer *) (($ & $valmask) | gdb_data_seg_bits)
output &((struct Lisp_String *) ((($->name) & $valmask) | gdb_data_seg_bits))->data
echo \n
end
document xbuffer
Set $ as a buffer pointer, assuming it is an Emacs Lisp buffer value.
Print the name of the buffer.
end

define xcons
print (struct Lisp_Cons *) (($ & $valmask) | gdb_data_seg_bits)
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
print/x ((($ >> gdb_valbits) & 0xf) == Lisp_Cons ? ((struct Lisp_Cons *) (($ & $valmask) | gdb_data_seg_bits))->car : 0)
end
document xcar
Print the car of $, assuming it is an Emacs Lisp pair.
end

define xcdr
print/x ((($ >> gdb_valbits) & 0xf) == Lisp_Cons ? ((struct Lisp_Cons *) (($ & $valmask) | gdb_data_seg_bits))->cdr : 0)
end
document xcdr
Print the cdr of $, assuming it is an Emacs Lisp pair.
end

define xfloat
print ((struct Lisp_Float *) (($ & $valmask) | gdb_data_seg_bits))->data
end
document xfloat
Print $ assuming it is a lisp floating-point number.
end

define xscrollbar
print (struct scrollbar *) (($ & $valmask) | gdb_data_seg_bits)
output *$
echo \n
end
document xscrollbar
Print $ as a scrollbar pointer.
end

define xprintsym
  set $sym = (struct Lisp_Symbol *) ((((int) $arg0) & $valmask) | gdb_data_seg_bits)
  output (char*)$sym->name->data
  echo \n
end
document xprintsym
  Print argument as a symbol.
end

define xbacktrace
  set $bt = backtrace_list
  while $bt 
    xprintsym *$bt->function
    set $bt = $bt->next
  end
end
document xbacktrace
  Print a backtrace of Lisp function calls from backtrace_list.
  Set a breakpoint at Fsignal and call this to see from where 
  an error was signalled.
end

define xreload
  set $valmask = ((long)1 << gdb_valbits) - 1
  set $nonvalbits = gdb_emacs_intbits - gdb_valbits
end
document xreload
  When starting Emacs a second time in the same gdb session under
  FreeBSD 2.2.5, gdb 4.13, $valmask and $nonvalbits have lost
  their values.  This function reloads them.
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
