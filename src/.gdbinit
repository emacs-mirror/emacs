# Set up something to print out s-expressions.
define pr
set debug_print ($)
echo \n
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

# Set this to the same thing as the DATA_SEG_BITS macro in your
# machine-description files.
set $data_seg_bits = 0

define mips
set $data_seg_bits = 0x10000000
end
document mips
Set up the xfoo macros to deal with the MIPS processor.
Specifically, this sets $data_seg_bits to the right thing.
end

define xtype
output (enum Lisp_Type) (($ >> 28) & 0x7)
echo \n
output ((($ >> 28) & 0x7) == Lisp_Misc ? (enum Lisp_Misc_Type) (((struct Lisp_Free *) (($ & 0x0fffffff) | $data_seg_bits))->type) : (($ >> 28) & 0x7) == Lisp_Vectorlike ? ($size = ((struct Lisp_Vector *) (($ & 0x0fffffff) | $data_seg_bits))->size, (enum pvec_type) (($size & PVEC_FLAG) ? $size & PVEC_TYPE_MASK : 0)) : 0)
echo \n
end
document xtype
Print the type of $, assuming it is an Emacs Lisp value.
If the first type printed is Lisp_Vector or Lisp_Misc,
the second line gives the more precise type.
Otherwise the second line doesn't mean anything.
end

define xvectype
set $size = ((struct Lisp_Vector *) (($ & 0x0fffffff) | $data_seg_bits))->size
output (enum pvec_type) (($size & PVEC_FLAG) ? $size & PVEC_TYPE_MASK : 0)
echo \n
end
document xvectype
Print the vector subtype of $, assuming it is a vector or pseudovector.
end

define xmisctype
output (enum Lisp_Misc_Type) (((struct Lisp_Free *) (($ & 0x0fffffff) | $data_seg_bits))->type)
echo \n
end
document xmisctype
Print the specific type of $, assuming it is some misc type.
end

define xint
print (($ & 0x0fffffff) << 4) >> 4
end
document xint
Print $, assuming it is an Emacs Lisp integer.  This gets the sign right.
end

define xptr
print (void *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xptr
Print the pointer portion of $, assuming it is an Emacs Lisp value.
end

define xwindow
print (struct window *) (($ & 0x0fffffff) | $data_seg_bits)
printf "%dx%d+%d+%d\n", $->width, $->height, $->left, $->top
end
document xwindow
Print $ as a window pointer, assuming it is an Emacs Lisp window value.
Print the window's position as "WIDTHxHEIGHT+LEFT+TOP".
end

define xmarker
print (struct Lisp_Marker *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xmarker
Print $ as a marker pointer, assuming it is an Emacs Lisp marker value.
end

define xoverlay
print (struct Lisp_Overlay *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xoverlay
Print $ as a overlay pointer, assuming it is an Emacs Lisp overlay value.
end

define xmiscfree
print (struct Lisp_Free *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xmiscfree
Print $ as a misc free-cell pointer, assuming it is an Emacs Lisp Misc value.
end

define xintfwd
print (struct Lisp_Intfwd *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xintfwd
Print $ as an integer forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xboolfwd
print (struct Lisp_Boolfwd *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xboolfwd
Print $ as a boolean forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xobjfwd
print (struct Lisp_Objfwd *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xobjfwd
Print $ as an object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbufobjfwd
print (struct Lisp_Buffer_Objfwd *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xbufobjfwd
Print $ as a buffer-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xdispobjfwd
print (struct Lisp_Display_Objfwd *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xdispobjfwd
Print $ as a display-local object forwarding pointer, assuming it is an Emacs Lisp Misc value.
end

define xbuflocal
print (struct Lisp_Buffer_Local_Value *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xbuflocal
Print $ as a buffer-local-value pointer, assuming it is an Emacs Lisp Misc value.
end

define xbuffer
print (struct buffer *) (($ & 0x0fffffff) | $data_seg_bits)
output &((struct Lisp_String *) ((($->name) & 0x0fffffff) | $data_seg_bits))->data
echo \n
end
document xbuffer
Set $ as a buffer pointer, assuming it is an Emacs Lisp buffer value.
Print the name of the buffer.
end

define xsymbol
print (struct Lisp_Symbol *) ((((int) $) & 0x0fffffff) | $data_seg_bits)
output &$->name->data
echo \n
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Emacs Lisp symbol value.
end

define xstring
print (struct Lisp_String *) (($ & 0x0fffffff) | $data_seg_bits)
output ($->size > 1000) ? 0 : ($->data[0])@($->size)
echo \n
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Emacs Lisp string value.
end

define xvector
print (struct Lisp_Vector *) (($ & 0x0fffffff) | $data_seg_bits)
output ($->size > 50) ? 0 : ($->contents[0])@($->size)
echo \n
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Emacs Lisp vector value.
end

define xframe
print (struct frame *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xframe
Print $ as a frame pointer, assuming it is an Emacs Lisp frame value.
end

define xwinconfig
print (struct save_window_data *) (($ & 0x0fffffff) | $data_seg_bits)
end
document xwinconfig
Print $ as a window configuration pointer, assuming it is an Emacs Lisp window configuration value.
end

define xcompiled
print (struct Lisp_Vector *) (($ & 0x0fffffff) | $data_seg_bits)
output ($->contents[0])@($->size & 0xff)
end
document xcompiled
Print $ as a compiled function pointer, assuming it is an Emacs Lisp compiled value.
end

define xcons
print (struct Lisp_Cons *) (($ & 0x0fffffff) | $data_seg_bits)
output *$
echo \n
end
document xcons
Print the contents of $, assuming it is an Emacs Lisp cons.
end

define xcar
print ((($ >> 28) & 0xf) == Lisp_Cons ? ((struct Lisp_Cons *) (($ & 0x0fffffff) | $data_seg_bits))->car : 0)
end
document xcar
Print the car of $, assuming it is an Emacs Lisp pair.
end

define xcdr
print ((($ >> 28) & 0xf) == Lisp_Cons ? ((struct Lisp_Cons *) (($ & 0x0fffffff) | $data_seg_bits))->cdr : 0)
end
document xcdr
Print the cdr of $, assuming it is an Emacs Lisp pair.
end

define xsubr
print (struct Lisp_Subr *) (($ & 0x0fffffff) | $data_seg_bits)
output *$
echo \n
end
document xsubr
Print the address of the subr which the Lisp_Object $ points to.
end

define xprocess
print (struct Lisp_Process *) (($ & 0x0fffffff) | $data_seg_bits)
output *$
echo \n
end
document xprocess
Print the address of the struct Lisp_process which the Lisp_Object $ points to.
end

define xfloat
print ((struct Lisp_Float *) (($ & 0x0fffffff) | $data_seg_bits))->data
end
document xfloat
Print $ assuming it is a lisp floating-point number.
end

define xscrollbar
print (struct scrollbar *) (($ & 0x0fffffff) | $data_seg_bits)
output *$
echo \n
end
document xscrollbar
Print $ as a scrollbar pointer.
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


