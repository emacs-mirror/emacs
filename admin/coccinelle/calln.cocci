// Use the calln macro where possible.
@@
@@
- CALLN ( Ffuncall,
+ calln (
  ...)

@@
constant c;
expression e;
@@
- Ffuncall ( c, &e )
+ calln ( e )

@@
constant c;
expression e;
@@
- Ffuncall ( c, &e,
+ calln ( e,
  ...)
