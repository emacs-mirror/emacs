// Adjust users of XSAVE_POINTER and XSAVE_INTEGER.
@@
expression E;
@@
(
- XSAVE_POINTER (E)
+ XSAVE_POINTER (E, 0)
|
- XSAVE_INTEGER (E)
+ XSAVE_INTEGER (E, 1)
)
