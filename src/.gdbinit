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

define xint
print (($ & 0x00ffffff) << 8) >> 8
end

define xptr
print (void *) ($ & 0x00ffffff)
end

define xwindow
print (struct window *) ($ & 0x00ffffff)
end

define xmarker
print (struct Lisp_Marker *) ($ & 0x00ffffff)
end

define xbuffer
print (struct buffer *) ($ & 0x00ffffff)
end

define xsymbol
print (struct Lisp_Symbol *) ($ & 0x00ffffff)
print &$->name->data
print $$
end

define xstring
print (struct Lisp_String *) ($ & 0x00ffffff)
print ($->data[0])@($->size)
print $$
end

document xstring
Assume that $ is an Emacs Lisp string object, print the string's
contents, and set $ to a pointer to the string.
end

define xvector
set $temp = (struct Lisp_Vector *) ($ & 0x00ffffff)
print ($temp->contents[0])@($temp->size)
print $temp
end

document xvector
Assume that $ is an Emacs Lisp vector object, print the vector's
contents, and set $ to a pointer to the vector.
end

define xscreen
print (struct screen *) ($ & 0x00ffffff)
end

define xcons
print (struct Lisp_Cons *) ($ & 0x00ffffff)
print *$
end

define xcar
print ((($ >> 24) & 0x7f) == Lisp_Cons ? ((struct Lisp_Cons *) ($ & 0x00ffffff))->car : 0)
end

define xcdr
print ((($ >> 24) & 0x7f) == Lisp_Cons ? ((struct Lisp_Cons *) ($ & 0x00ffffff))->cdr : 0)
end

set prettyprint on

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command below as well.
break abort

# If we are running in synchronous mode, we want a chance to look around
# before Emacs exits.  Perhaps we should put the break somewhere else
# instead...
break _XPrintDefaultError

unset env TERMCAP
unset env TERM
set env DISPLAY :0.0
info env DISPLAY
set args -q
