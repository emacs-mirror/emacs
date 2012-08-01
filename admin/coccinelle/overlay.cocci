// Change direct access to Lisp_Object fields of struct
// Lisp_Overlay to MVAR.  Beginning M denotes "misc", and
// MVAR is likely to be used for other second-class objects.
@@
struct Lisp_Overlay *V;
Lisp_Object O;
@@
(
- V->start
+ MVAR (V, start)
|
- V->end
+ MVAR (V, end)
|
- V->plist
+ MVAR (V, plist)

|

- XOVERLAY (O)->start
+ MVAR (XOVERLAY (O), start)
|
- XOVERLAY (O)->end
+ MVAR (XOVERLAY (O), end)
|
- XOVERLAY (O)->plist
+ MVAR (XOVERLAY (O), plist)
)
