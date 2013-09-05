// Omit redundant type check, consistently use CONSP.
@@
identifier A;
expression X;
statement S;
@@
(
for (A = X; 
- !NILP (A); 
+ CONSP (A);
- A = Fcdr (A))
+ A = XCDR (A))
S
|
for (A = X; CONSP (A);
- A = Fcdr (A))
+ A = XCDR (A))
S
)
