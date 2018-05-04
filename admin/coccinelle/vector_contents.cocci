// Avoid direct access to 'contents' member of
// Lisp_Vector, use AREF and ASET where possible.
@expression@
identifier I1, I2;
expression E1, E2;
@@
(
- XVECTOR (I1)->contents[I2++] = E1
+ ASET (I1, I2, E1), I2++
|
- XVECTOR (I1)->contents[E1] = E2
+ ASET (I1, E1, E2)
|
- XVECTOR (I1)->contents[E1]
+ AREF (I1, E1)
)
