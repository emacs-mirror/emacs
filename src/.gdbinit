# Set up something to print out s-expressions.
define pr
set Fprin1 ($, Qexternal_debugging_output)
echo \n
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

define xtype
print (enum Lisp_Type) (($ >> 24) & 0x7f)
p $$
end
document xtype
Print the type of $, assuming it is an Elisp value.
end

define xint
print (($ & 0x00ffffff) << 8) >> 8
end
document xint
Print $, assuming it is an Elisp integer.  This gets the sign right.
end

define xptr
print (void *) ($ & 0x00ffffff)
end
document xptr
Print the pointer portion of $, assuming it is an Elisp value.
end

define xwindow
print (struct window *) ($ & 0x00ffffff)
print ($->left)@4
print $$
end
document xwindow
Print $ as a window pointer, assuming it is an Elisp window value.
Print the window's position as { left, top, height, width }.
end

define xmarker
print (struct Lisp_Marker *) ($ & 0x00ffffff)
end
document xmarker
Print $ as a marker pointer, assuming it is an Elisp marker value.
end

define xbuffer
print (struct buffer *) ($ & 0x00ffffff)
print &((struct Lisp_String *) (($->name) & 0x00ffffff))->data
print $$
end
document xbuffer
Set $ as a buffer pointer, assuming it is an Elisp buffer value.
Print the name of the buffer.
end

define xsymbol
print (struct Lisp_Symbol *) ($ & 0x00ffffff)
print &$->name->data
print $$
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Elisp symbol value.
end

define xstring
print (struct Lisp_String *) ($ & 0x00ffffff)
print ($->size > 10000) ? "big string" : ($->data[0])@($->size)
print $$
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Elisp string value.
end

define xvector
set $temp = (struct Lisp_Vector *) ($ & 0x00ffffff)
print ($temp->size > 10000) ? "big vector" : ($temp->contents[0])@($temp->size)
print $temp
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Elisp vector value.
end

define xscreen
print (struct screen *) ($ & 0x00ffffff)
end
document xwindow
Print $ as a screen pointer, assuming it is an Elisp screen value.
end

define xcons
print (struct Lisp_Cons *) ($ & 0x00ffffff)
print *$
print $$
end
document xcons
Print the contents of $, assuming it is an Elisp cons.
end

define xcar
print ((($ >> 24) & 0x7f) == Lisp_Cons ? ((struct Lisp_Cons *) ($ & 0x00ffffff))->car : 0)
end
document xcar
Print the car of $, assuming it is an Elisp pair.
end

define xcdr
print ((($ >> 24) & 0x7f) == Lisp_Cons ? ((struct Lisp_Cons *) ($ & 0x00ffffff))->cdr : 0)
end
document xcdr
Print the cdr of $, assuming it is an Elisp pair.
end

set print pretty on

unset environment TERMCAP
unset environment TERM
set environment DISPLAY :0.0
show environment DISPLAY
set args -q

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command below as well.
break abort

# If we are running in synchronous mode, we want a chance to look around
# before Emacs exits.  Perhaps we should put the break somewhere else
# instead...
break _XPrintDefaultError

