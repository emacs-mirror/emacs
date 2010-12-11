(* -*- mode: modula-2; m2-indent:3 -*- *)

IMPLEMENTATION MODULE Indent ;

(* This is (* a nested comment *) *)
// This is a single-line comment.

FROM SYSTEM IMPORT ADR, TSIZE, SIZE, WORD ;

CONST
   c1 = 2;

TYPE
   t = POINTER TO ARRAY [0..10] OF LONGINT;

VAR x: t;
    y:LONGINT;


PROCEDURE f1 (f: File) : INTEGER ;
   VAR
      fd: FileDescriptor ;
   PROCEDURE foo (a:CARDINAL) : INTEGER;
   BEGIN
   END foo;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL THEN
         RETURN( fd^.unixfd )
      ELSE
         CASE z OF
            1: do1();
          | 2: do2();
               toto(x);
          | 3: ;
          | 4: do4();
         ELSE do5();
         END ; (* CASE selection *)

      END
   END ;
   FormatError1('file %d has not been opened or is out of range\n', f) ;
   RETURN( -1 )
END f1 ;


BEGIN
   init
FINALLY
   done
END Indent.
