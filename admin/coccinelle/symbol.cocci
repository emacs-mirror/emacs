// Change direct access to Lisp_Object fields of struct Lisp_Symbol to SVAR.
@@
struct Lisp_Symbol *S;
Lisp_Object O;
@@
(
- S->xname
+ SVAR (S, xname)
|
- S->val.value
+ SVAR (S, val.value)
|
- S->function
+ SVAR (S, function)
|
- S->plist
+ SVAR (S, plist)

|

- XSYMBOL (O)->xname
+ SVAR (XSYMBOL (O), xname)
|
- XSYMBOL (O)->val.value
+ SVAR (XSYMBOL (O), val.value)
|
- XSYMBOL (O)->function
+ SVAR (XSYMBOL (O), function)
|
- XSYMBOL (O)->plist
+ SVAR (XSYMBOL (O), plist)
)
