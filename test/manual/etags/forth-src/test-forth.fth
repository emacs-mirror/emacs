\
\ This is a file that tests Forth tags
\
\ You should get:
\	a-forth-word		(twice)
\	a-forth-constant!
\	a-forth-value?
\	:a-forth-dictionary-entry
\	#a-defer-word
\	(another-forth-word)
\	(a-forth-constant
\	#some-storage
\	assemby-code-word


\ This is a forth comment

( Another forth comment )

: a-forth-word ( a b c -- a*b+c )  + * ;

99 constant a-forth-constant!
55 value a-forth-value?
create :a-forth-dictionary-entry
0 c, 9 c, 5 c, 7 c, 999999 ,

defer #a-defer-word

: (another-forth-word) ( -- )
    ." Hello world"
;

' (another-forth-word) to #a-defer-word

struct
   9   field   >field1
   5   field   >field2
constant (a-forth-constant


2000 buffer: #some-storage

code assemby-code-word ( dunno what it does )
    g1 g2 mov		\ Move from here to there
    sc2 h# 13 sc2 sllx	\ shift stuff 'round
c;

\ And for the heck of it, redefine a-forth-word.

: a-forth-word ( a b c -- )
    a-forth-word dup 200 > abort" Eek.  The number is too big"
    ." Result is " . cr
;

: (foo) 1 ;
: foo (foo) ;

: create-bar foo ;
create-bar  \ Do NOT create a tag here

3 4 2constant 2const
2const 2value 2val
2variable 2var

3.1415e fconstant fconst
fconst fvalue fval
fvariable fvar

synonym mypi fconst

BEGIN-STRUCTURE point \ create the named structure
  1 CELLS +FIELD p.x    \ A single cell filed named p.x
  1 CELLS +FIELD p.y    \ A single cell field named p.y
END-STRUCTURE
