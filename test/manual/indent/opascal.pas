{ -*- opascal -*- }

procedure Toto ();
begin
   for i := 0 to 1 do
      Write (str.Chars[i]);

   // bug#36348
   for var i := 0 to 1 do
      Write (str.Chars[i]);

end;
