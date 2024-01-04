// Prefer NILP (x) to EQ (x, Qnil)
@@
expression X;
@@
- EQ (X, Qnil)
+ NILP (X)
