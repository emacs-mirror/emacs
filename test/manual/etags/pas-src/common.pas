#include "common.i"
#include "common.h"

type
  NSPoolP = ^NSPoolRec;
  NSPoolRec = record
    Data: NameStringPointer;
    Next: NSPoolP;
  end;

var
  GlobalNSPool: record
    Avail, Empty: NSPoolP;
  end;

var
  AvailString : TextString;
  NameList : BinNodePointer;  
  AvailNameList : BinNodePointer;  



(*------------------------------------------------------------------*) 
(*                    InitializeStringPackage                       *) 
(*------------------------------------------------------------------*) 
procedure InitializeStringPackage;
begin (* InitializeStringPackage *) 
  AvailString := nil;
end; (* InitializeStringPackage *) 

(*------------------------------------------------------------------*) 
(*                    newtextstring                                 *) 
(*------------------------------------------------------------------*) 
function newtextstring; (*: TextString;*)
var
  Temp : TextString;
begin (* newtextstring *) 
  if AvailString = nil then
    new (Temp)
  else begin
    Temp := AvailString;
    AvailString := Temp^.Next;
  end;
  Temp^.String.Length := 0;
  Temp^.Next := nil;
  newtextstring := Temp;
end; (* newtextstring *) 

(*------------------------------------------------------------------*) 
(*                    disposetextstring                             *) 
(*------------------------------------------------------------------*) 
procedure disposetextstring;(*(
  var S : TextString);*)
var
  Temp : TextString;
  Temp2 : TextString;
begin (* disposetextstring *) 
  if S <> nil then begin
    Temp := S;
(*
    while Temp^.Next <> nil do
      Temp := Temp^.Next;
    Temp^.Next := AvailString;
    AvailString := S;
*)
    S := nil;
    repeat
      Temp2 := Temp^.Next;
      dispose(Temp);
      Temp := Temp2;
    until Temp = nil;
   end;
end; (* disposetextstring *) 

(*------------------------------------------------------------------*) 
(*                    ConcatT                                       *) 
(*------------------------------------------------------------------*) 
function ConcatT;(*(
  ToString : TextString;
  S        : TextString) : TextString;*)
var
  Index : integer;
begin (* ConcatT *) 
  ConcatT := ToString;
  if ToString = nil then
    writeln (output, 'Error in ConcatT, ToString is nil')
  else
    if S = nil then
      writeln (output, 'Error in ConcatT, S is nil')
    else
      if S^.Next <> nil then
        writeln (output,
          'Error in ConcatT, S contains several linked TextStrings')
      else begin
        while ToString^.Next <> nil do
          ToString := ToString^.Next;
        if ToString^.String.Length+S^.String.Length > NameStringLength then begin
          ToString^.Next := newtextstring;
          ToString := ToString^.Next;
        end;
        with ToString^, String do begin
          for Index := 1 to S^.String.Length do
            Value[Length+Index] := S^.String.Value[Index];
          Length := Length+S^.String.Length;
        end;
      end;
end; (* ConcatT *) 

(*------------------------------------------------------------------*) 
(*                    AppendTextString                              *) 
(*------------------------------------------------------------------*) 
function AppendTextString;(*(
  ToString : TextString;
  S        : TextString) : TextString;*)
begin (* AppendTextString *) 
  AppendTextString := ToString;
  if ToString = nil then
    writeln (output, 'Error in AppendTextString, ToString is nil')
  else
    if S = nil then
      writeln (output, 'Error in AppendTextString, S is nil')
    else begin
      while ToString^.Next <> nil do
        ToString := ToString^.Next;
      ToString^.Next := S;
    end;
end; (* AppendTextString *) 

(*------------------------------------------------------------------*) 
(*                    CopyTextString                                *) 
(*------------------------------------------------------------------*) 
function CopyTextString;(*(
  S : TextString
  ) : TextString;*)
var
  Temp : TextString;
begin   (* CopyTextString *) 
  if S <> nil then begin
    Temp := newtextstring;
    Temp^.String := S^.String;
    Temp^.Next := CopyTextString(S^.Next);
    CopyTextString := Temp;
  end
  else
    CopyTextString := nil;
end;    (* CopyTextString *) 

(*------------------------------------------------------------------*)
(*                    CONVERT_CHARSTRING_TO_VALUE                   *)
(*------------------------------------------------------------------*)
procedure CONVERT_CHARSTRING_TO_VALUE;(*(
      S : NameString;
  var V : NameString);*)
var
  Pos : integer;
  VPos : integer;
  Ch : char;
begin (* CONVERT_CHARSTRING_TO_VALUE *)
  VPos := 0;
  for Pos := 2 to S.Length - 1 do begin
    Ch := S.Value[Pos];
    if not ((Ch = '''') and (Pos > 2) and (S.Value[Pos - 1] = '''')) then
      VPos := VPos + 1;
    V.Value[VPos] := Ch;
  end;
  V.Length := VPos;
end; (* CONVERT_CHARSTRING_TO_VALUE *)

(*------------------------------------------------------------------*)
(*                    append_string                                 *)
(*------------------------------------------------------------------*)
procedure append_string;(*(
  var Txt    : TextString;
  var String : NameString);*)
var
  Temp : TextString;
begin (* append_string *)
  Temp := newtextstring;
  Temp^.String := String;
  if Txt = nil then
    Txt := Temp
  else
    Txt := AppendTextString(Txt, Temp);
end; (* append_string *)

function To_Upper;(*(ch:char) : char;*)
begin
  if ch in ['a'..'z'] then
    To_Upper := chr(ord(ch) + ord('A')-ord('a'))
  else
    To_Upper := ch;
end;

function To_Lower;(*(ch:char) : char;*)
begin
  if ch in ['A'..'Z'] then
    To_Lower := chr(ord(ch) - ord('A') + ord('a'))
  else
    To_Lower := ch;
end;

(*----------------------------------------------------------------------*)
(*              Operations on NameString                                *)
(*----------------------------------------------------------------------*)

(*------------------------------------------------------------------*)
(*                    EmptyNmStr                                    *)
(*------------------------------------------------------------------*)
function EmptyNmStr(* : NameString*);
var
  Nm : NameString;
begin   (* EmptyNmStr *)
  Nm.Length := 0;
  EmptyNmStr := Nm;
end;    (* EmptyNmStr *)


(* returns a namestring containing one character, the inparameter Ch *)
function chartonmstr; (*(
  Ch : Char) : NameString; *)
var
  String : NameString;
begin
  String.Value[1] := Ch;
  String.Length := 1;
  chartonmstr := String;
end;

(* returns a namestring containing the inparameter Str in lowercase letters *)
function LowerCaseNmStr; (*(
  Str : NameString) : NameString; *)
var
  i : integer;
begin (* LowerCaseNmStr *)
  with Str do
    for i := 1 to Length do
      Value[i] := To_Lower(Value[i]);
  LowerCaseNmStr := Str;
end; (* LowerCaseNmStr *)

(* returns a namestring containing inparameter S1 concatenated with inpar. S2 *)
function concatenatenamestrings; (*(
  S1 : NameString;
  S2 : NameString) : NameString; *)
var
  Temp : NameString;
  Pos : integer;
begin (* concatenatenamestrings *)
  Temp := S1;
  with Temp do begin
    Pos := 0;
    while Pos < S2.Length do begin
      Pos := Pos + 1;
      if Length < NameStringLength then begin
        Length := Length + 1;
        Value[Length] := S2.Value[Pos];
      end;
    end; (* while *)
  end; (* with *)
  concatenatenamestrings := Temp;
end; (* concatenatenamestrings *)

procedure writenamestring;(*(
  var TextFile : text;
  var Name     : NameString);*)
var
  Pos : integer;
begin
  with Name do
    for Pos := 1 to Length do
      write(TextFile, Value[Pos]);
end;

(*------------------------------------------------------------------*)
(*                    IsControlChar                                 *)
(*------------------------------------------------------------------*)
function IsControlChar; (*(
  Ch : char) : boolean; *)
begin (* IsControlChar *)
  IsControlChar := ord(Ch) in [0..32, 127];
end; (* IsControlChar *)

function namestringequal;(*(var Name1,Name2 : NameString) : Boolean;*)
var i : Integer;
    equal : Boolean;
begin
  if Name1.Length = Name2.Length then begin
    equal := true;
    i := 1;
    while (i <= Name1.Length) and equal do begin
       equal := To_Upper(Name1.Value[i]) = To_Upper(Name2.Value[i]);
       i := i + 1;
    end;
    namestringequal := equal;
  end 
  else
    namestringequal := false;
end;

(* Character strings are case sensitive *)

function NameStringLess;(*(var Name1,Name2 : NameString) : Boolean;*)
var i, minlength : Integer;
    equal : Boolean;
    C1, C2 : char;
    Charstring : boolean;
begin
  C1 := ' ';
  C2 := ' ';
  if Name1.Length < Name2.Length then
    minlength := Name1.Length 
  else
    minlength := Name2.Length;
  if MinLength > 0 then
    Charstring := (Name1.Value[1] = '''') or (Name2.Value[1] = '''')
  else
    Charstring := false;
(*   Charstring := true; force case sensitive *)
  i := 1;
  equal := true;
  if i <= minlength then
    while (i <= minlength) and equal do begin
      if Charstring then begin
        C1 := Name1.Value[i];
        C2 := Name2.Value[i];
      end
      else begin
        C1 := To_Upper(Name1.Value[i]);
        C2 := To_Upper(Name2.Value[i]);
      end;
      equal := C1 = C2;
      i := i + 1;
    end; (* while *)
  if equal then
    NameStringLess := Name1.Length < Name2.Length
  else 
    NameStringLess := C1 < C2;
end;

(*------------------------------------------------------------------*)
(*                    IsControlCharName                             *)
(*------------------------------------------------------------------*)
function IsControlCharName(
  Str : NameString;
  Pos : integer) : boolean;
begin (* IsControlCharName *)
  with Str do begin
    if Pos <= Length then
      IsControlCharName := IsControlChar(Value[Pos])
    else
      IsControlCharName := false;
  end;
end; (* IsControlCharName *)

(*------------------------------------------------------------------*)
(*                    SubString                                     *)
(*------------------------------------------------------------------*)
function SubString; (*(
  Str : NameString;
  Start : integer;
  Len : integer) : NameString; *)
var
  i : integer;
begin (* SubString *)
  with Str do begin
    if Len > 0 then
      for i := Start to Start + Len - 1 do
        Value[i- Start + 1] := Value[i]
    else if Len < 0 then
      Len := 0;
    Length := Len;
  end;
  SubString := Str;
end; (* SubString *)

(*------------------------------------------------------------------*)
(*                    SkipChars                                     *)
(*------------------------------------------------------------------*)
function SkipChars; (*(
  Str : NameString;
  Start : integer;
  Len : integer) : NameString; *)
var
  i : integer;
begin (* SkipChars *)
  with Str do begin
    for i := Start to Length - Len do
      Value[i] := Value[i + Len];
    Length := Length - Len;
  end;
  SkipChars := Str;
end; (* SkipChars *)

(*------------------------------------------------------------------*)
(*                    RemoveUnderlineControl                        *)
(*------------------------------------------------------------------*)
function RemoveUnderlineControl; (*(
      Str : NameString) : NameString; *)
var
  Len : integer;
  i : integer;
  Start : integer;
begin (* RemoveUnderlineControl *)
  with Str do begin
    i := 1;
    while i <= Length do begin
      if Value[i] = '_' then begin
        Len := 0;
        Start := i;
        while IsControlCharName(Str, i + 1 + Len) do
          Len := Len + 1;
        if Len > 0 then
          Str := SkipChars(Str, Start, Len + 1)
        else
          i := i + 1;
      end
      else
        i := i + 1;
    end; (* while *)
  end; (* with *)
  RemoveUnderlineControl := Str;
end; (* RemoveUnderlineControl *)

(*------------------------------------------------------------------*)
(*                    First100Chars                                 *)
(*------------------------------------------------------------------*)
procedure First100Chars; (*(
      Txt : TextString;
  var Str : NameString;
  var Truncated : boolean); *)
var
  Len : integer;
  i : integer;
begin (* First100Chars *)
  Str.Length := 0;
  if Txt <> nil then begin
    Str := Txt^.String;
    Txt := Txt^.Next;
  end;
  while (Txt <> nil) and (Str.Length < NameStringLength) do
    with Txt^, String do begin
      Str.Length := Str.Length + 1;
      Str.Value[Str.Length] := ' ';
      if Str.Length + Length <= NameStringLength then
        Len := Str.Length + Length
      else
        Len := NameStringLength;
      for i := Str.Length + 1 to Len do
        Str.Value[i] := Value[i - Str.Length];
      Str.Length := Len;
      Txt := Txt^.Next;
    end; (* while with *)
  Truncated := Txt <> nil;
end; (* First100Chars *)


(*------------------------------------------------------------------*)
(*                    SkipSpaces                                    *)
(*------------------------------------------------------------------*)
(* changes I to contain the first index in Str (starting at I) that *)
(* is not a space                                                   *)
procedure SkipSpaces; (* (Str : NameString; var I : Integer);*)
var Stop : boolean;
begin   (* SkipSpaces *)
  Stop := false;
  while (I < Str.Length) and not Stop do
    if Str.Value[I] <> ' ' then
      Stop := true
    else
      I := I+1; 
end;    (* SkipSpaces *)


(*------------------------------------------------------------------*)
(*                    SkipBlanks                                    *)
(*------------------------------------------------------------------*)
function SkipBlanks; (*(
  TextLine: NameString) : NameString; *)
var
  i : integer;
  j : integer;
  SpaceFound : boolean;
begin (* SkipBlanks *)
  with TextLine do begin
    SpaceFound := true;
    i := 1;
    while SpaceFound and (i <= Length) do begin
      SpaceFound := (Value[i] in [' ', chr(9)]);
      if SpaceFound then
        i := i + 1;
    end; (* while *)
    i := i - 1;
    if i > 0 then
      for j := 1 to Length - i do
        if j <= Length - i then
          Value[j] := Value[j + i];
    Length := Length - i;
  end; (* with *)
  SkipBlanks := TextLine;
end; (* SkipBlanks *)

(*------------------------------------------------------------------*)
(*                    stripname                                     *)
(*------------------------------------------------------------------*)
function stripname; (* (
  TextLine: NameString) : NameString; *)
var
  SpaceFound : boolean;
begin (* stripname *)
  TextLine := SkipBlanks(TextLine);
  with TextLine do begin
    SpaceFound := true;
    while SpaceFound and (Length > 0) do begin
      SpaceFound := (Value[Length ] in [' ', chr(9)]);
      if SpaceFound then
        Length := Length - 1;
    end; (* while *)
  end; (* with *)
  stripname := TextLine;
end; (* stripname *)

function Locate; (*(
 Str : NameString;
 Chars : SetOfChar) : integer; *)
var
  Pos : integer;
  Found : boolean;
begin (* Locate *)
  Found := false;
  Pos := 0;
  with Str do
    while not Found and (Pos < Length) do begin
      Pos := Pos + 1;
      Found := Value[Pos] in Chars;
    end;
  Locate := Pos;
end; (* Locate *)


(*------------------------------------------------------------------*)
(*                    NameHasChar                                   *)
(*------------------------------------------------------------------*)
function NameHasChar; (* (TheName : NameString; TheChar : char) : boolean;*)
var i : integer;
    found : boolean;
    
begin   (* NameHasChar *)
  found := false;
  i := 0;
  while not found and (i < TheName.Length) do begin
    i := i+1;
    found := TheName.Value[i] = TheChar;
  end;
  NameHasChar := found;
end;    (* NameHasChar *)


(*------------------------------------------------------------------*)
(*                    integertonmstr                                *)
(*------------------------------------------------------------------*)
function integertonmstr; (* (TheInteger : integer) : NameString; *)
var Nm : NameString;
    Index,
    Size,
    TempNumber : integer;
begin   (* integertonmstr *)
  Size := 1;
  TempNumber := TheInteger;
  while TempNumber div 10 > 0 do begin
    Size := Size + 1;
    TempNumber := TempNumber div 10;
  end;
  Nm.Length := Size;
  TempNumber := TheInteger;
  for Index := Size downto 1 do begin
    Nm.Value[Index] := chr(TempNumber mod 10 + ord('0'));
    TempNumber := TempNumber div 10;
  end;
  integertonmstr := Nm;
end;    (* integertonmstr *)

(*------------------------------------------------------------------*)
(*                    NmStrToInteger                                *)
(*------------------------------------------------------------------*)
function NmStrToInteger; (* (Str : NameString) : integer; *)
var
  Index : integer;
  Numb : integer;
  Max : integer;
begin   (* NmStrToInteger *)
 Max := (maxint div 10) - 10;
 Numb := 0;
  for Index := 1 to Str.Length do begin
    if (Numb <= Max) and (Str.Value[Index] in ['0'..'9']) then
      Numb := 10 * Numb + ord(Str.Value[Index]) - ord('0');
  end;
  NmStrToInteger := Numb;
end;    (* NmStrToInteger *)

function AddNullToNmStr; (*(
  Nm : NameString) : NameString; *)
begin (* AddNullToNmStr *)
  with Nm do
    if Length < NameStringLength then
      Value[Length + 1] := chr(0)
    else
      Value[Length] := chr(0);
  AddNullToNmStr := Nm;
end; (* AddNullToNmStr *)

function ValToNmStr; (*(
  Nm : NameString) : NameString; *)
begin (* ValToNmStr *)
  with Nm do begin
    length := 0;
    while value[length + 1] <> chr(0) do
      length := length + 1;
  end;
  ValToNmStr := Nm;
end; (* ValToNmStr *)

(*------------------------------------------------------------------*)
(*                    ChangeFileType                                *)
(*------------------------------------------------------------------*)
function ChangeFileType; (*(FileName : NameString;
  NewType : NameString) : NameString;*)
var
  Pos : integer;
  Found : boolean;
begin (* ChangeFileType *)
  with Filename do begin
    Pos := FileName.Length;
    Found := false;
    while not Found and (Pos > 0) do begin
      Found := Value[Pos] = '.';
      Pos := Pos - 1;
    end;
    if Found then
      Length := Pos;
  end; (* with *)
  ChangeFileType := concatenatenamestrings(FileName, NewType);
end; (* ChangeFileType*)

(*------------------------------------------------------------------*)
(*                    StripPath                                     *)
(*------------------------------------------------------------------*)
function StripPath; (*(
      Str : NameString) : NameString; *)
var
  i : integer;
  Len : integer;
  Found : boolean;
begin (* StripPath *)
  with Str do begin
    i := Length;
    Found := false;
    while not Found and (i > 0) do begin
      Found := Value[i] in  ['/', '\'];
      if not Found  then
        i := i - 1;
    end; (* while *)
    if Found then begin
      Len := Length - i + 1;
      if i < Length then begin
        i := i + 1;
        Len := Len - 1;
      end;
      StripPath := SubString(Str, i, Len);
    end
    else
      StripPath := Str;
  end; (* with *)
end; (* StripPath *)

function ReprOfChar; (*( ch : char) : NameString;*)
var
  Repr : NameString;
begin
  if (ch >= ' ') and (ch <= '~') then
    Repr := chartonmstr(ch)
  else
    Repr := concatenatenamestrings(concatenatenamestrings(chartonmstr('<'),
              integertonmstr(ord(ch))), chartonmstr('>'));
  ReprOfChar := Repr;
end; (* ReprOfChar *)

(*------------------------------------------------------------------*) 
(*                    ExtractCommentInfo                            *) 
(*------------------------------------------------------------------*) 
(* check if Comment contains graphic reference or include directive *)
(* /*#<graphref>*/ or /*#<include-dir>*/                            *)
(*    <graphref>    =G pagename xcoord ycoord                       *)
(*                   T pagename xcoord ycoord                       *)
(*                   M diagramtype diagramname pagename xcoord ycoord *)
(*                   D databankname                                 *)
(*    <include-dir> =INCLUDE 'filename'                             *)
(* InfoType will contain the type of the comment                    *)
(* Info will contain <graphref> or the filename in <include-dir> if *)
(* the Comment isn't an ordinary comment                            *)
(* /*#E*/ do not count this line                                    *)
(* /*#S*/ substructure generated from graphic short hand            *)
procedure ExtractCommentInfo; (*(
  var Comment, 
      Info     : NameString; 
  var InfoType : TypeOfComment); *)

const
  CommentMarkLength = 2;
  IncludeMarkLength = 7;  (* = INCLUDE *) 
  GRRefLen = 6;
var StartIndex,
    Index : integer;
begin   (* ExtractCommentInfo *) 
  Info.Length := 0;
  with Comment do begin
    InfoType := Ordinary;
    StartIndex := CommentMarkLength + 1;
    if Length > StartIndex then
      if Value[StartIndex] = '#' then
        if Value[StartIndex+1] in ['I','i', 'S'] then begin
          if (Value[StartIndex+1] = 'S') and (Length = StartIndex+1+2) then
            InfoType := SubstrShortHand
          else if (Value[StartIndex+1] = 'S') and
            (Length > StartIndex + GRRefLen) then begin
              if Value[StartIndex+2] = 'D' then
                if Value[StartIndex+3] = 'T' then
                  if Value[StartIndex+4] = 'R' then
                    if Value[StartIndex+5] = 'E' then
                      if Value[StartIndex+6] = 'F' then
                        InfoType := GRRef;
            end
          else begin
            if Length > StartIndex + IncludeMarkLength then
              if Value[StartIndex+2] in ['N','n'] then
                if Value[StartIndex+3] in ['C','c'] then
                  if Value[StartIndex+4] in ['L','l'] then
                   if Value[StartIndex+5] in ['U','u'] then
                     if Value[StartIndex+6] in ['D','d'] then
                       if Value[StartIndex+7] in ['E','e'] then
                         InfoType := IncludeClause;
          end;
        end;

    if InfoType = IncludeClause then begin
      InfoType := Ordinary;
      StartIndex := StartIndex + IncludeMarkLength + 1;
      if StartIndex+3 <= Length-2 then (* excluding the comment-end '*/' *) begin
        if Value[StartIndex] = ' ' then begin
          while (StartIndex <= Length-2) and (Value[StartIndex] = ' ') do 
            StartIndex := StartIndex + 1;           (* Skip the spaces *)
          if Value[StartIndex] = '''' then begin
            Index := StartIndex+1;
            while (Index <= Length-2) and (Value[Index] <> '''') do begin
              Info.Value[Index-StartIndex] := Value[Index];
              Index := Index + 1;
            end;
            if Value[Index] = '''' then begin
              Info.Length := Index - StartIndex - 1;
              Index := Index + 1;
              while (Index <= Length-2) and (Value[Index] = ' ') do 
                Index := Index + 1;           (* Skip the ending spaces *)
              if Index = Length-1 then
                InfoType := IncludeClause;  (* => a correct include directive *)
            end;
          end;
        end;
      end;
    end
    else if InfoType = SubstrShortHand then
      Info := chartonmstr('S')
    else if InfoType = GRRef then begin
      if (Value[Length] = '/') and (Value[Length - 1] = '*') then
        Info := SubString(Comment, StartIndex, Length - StartIndex + 1 - 2)
      else (* truncated *)
        Info := SubString(Comment, StartIndex, Length - StartIndex + 1);
    end;
  end;
end;    (* ExtractCommentInfo *) 

(*---------------------------------------------------------------------------*)
(* inserts a node in a binary tree sorted after value. If node 
   is in tree Found returns true.  *)

procedure INSERT_TREE_NODE;(*( 
            New_node: BinNodePointer;  node to insert 
            var Node: BinNodePointer;  tree to insert in 
            var FoundNode : BinNodePointer; 
            var Found : boolean;       return status of operation 
            var Higher: boolean);   returned true if the subtree height has 
                                       increased *)

var

  Node_1,                    (* helpvariable to rotate nodes *)
  Node_2: BinNodePointer; (* helpvariable to rotate nodes *)

begin

  if Node = nil then
  begin  (* Value is not in tree, insert *)
    Node:= New_node;
    FoundNode := Node;
    Higher:=  true;
  end
  else

       (* New_node^.Value < Node^.Value *)
    if NameStringLess(New_node^.NameP^, Node^.NameP^) then
    begin  (* New Value is lower than actual Value *)
      INSERT_TREE_NODE( New_node, Node^.left, FoundNode, Found, Higher);

      if Higher then  (* left bransch has grown higher *)
      case Node^.bal of

        1: begin
             Node^.bal:= 0;
             Higher:= false;
           end;

        0: begin
             Node^.bal:= -1;
           end;

       -1: begin  (* rebalance *)
             Node_1:= Node^.left;

             if Node_1^.bal = -1 then
             begin  (* single LL rotation *)
               Node^.left:= Node_1^.right;
               Node_1^.right:= Node;
               Node^.bal:= 0;
               Node:= Node_1;
             end
             else

             begin  (* double LR rotation *)
               Node_2:= Node_1^.right;
               Node_1^.right:= Node_2^.left;
               Node_2^.left:= Node_1;
               Node^.left:= Node_2^.right;
               Node_2^.right:= Node;

               if Node_2^.bal = -1 then
                 Node^.bal:= 1
               else
                 Node^.bal:= 0;

               if Node_2^.bal = 1 then
                 Node_1^.bal:= -1
               else
                 Node_1^.bal:= 0;
               Node:= Node_2;
             end;
             Node^.bal:= 0;
             Higher:= false;
           end;
      end; (* end case Node^.bal of *)
    end
    else

         (* New_node^.value > Node^.value *)
      if NameStringLess(Node^.NameP^, New_Node^.NameP^) then
      begin  (* New value is higher than actual value *)
        INSERT_TREE_NODE( New_node, Node^.right, FoundNode, Found, Higher);

        if Higher then  (* Right bransch has grown higher *)
        case Node^.bal of

         -1: begin
               Node^.bal:= 0;
               Higher:= false;
             end;

          0: begin
               Node^.bal:= 1;
             end;

          1: begin  (* Rebalance *)
               Node_1:= Node^.right;

               if Node_1^.bal = 1 then
               begin  (* single RR rotation *)
                 Node^.right:= Node_1^.left;
                 Node_1^.left:= Node;
                 Node^.bal:= 0;
                 Node:= Node_1;
               end
               else
               begin  (* double RL rotation *)
                 Node_2:= Node_1^.left;
                 Node_1^.left:= Node_2^.right;
                 Node_2^.right:= Node_1;
                 Node^.right:= Node_2^.left;
                 Node_2^.left:= Node;

                 if Node_2^.bal = 1 then
                   Node^.bal:= -1
                 else
                   Node^.bal:= 0;

                 if Node_2^.bal = -1 then
                   Node_1^.bal:= 1
                 else
                   Node_1^.bal:= 0;
                 Node:= Node_2;
               end;
               Node^.bal:= 0;
               Higher:= false;
             end;
        end; (* end case Node^.bal of *)
      end
      else
      begin  (* New value is equal to actual value *)
        Found := true;
        FoundNode := Node;
        Higher:= false;
      end;
end;  (* end INSERT_TREE_NODE *)

function GetNameList; (* : BinNodePointer;*)
begin
  GetNameList := NameList;
end;

procedure DisposeANameList( 
  var NodeP : BinNodePointer);
begin (* DisposeANameList *)
  if NodeP <> nil then begin
    DisposeANameList(NodeP^.Left);
    DisposeANameList(NodeP^.Right);
    NodeP^.Left := AvailNameList;
    NodeP^.Right := nil;
    AvailNameList := NodeP;
    NodeP := nil;
  end;
end; (* DisposeANameList *)

procedure DisposeNameList;
begin
  DisposeANameList(NameList);
end;

function GetNewNameListNode;(*(
  var Name  : NameString) : BinNodePointer;*)
var
  NodeP : BinNodePointer;
begin (* GetNewNameListNode *)
  if AvailNameList = nil then begin
    new(NodeP);
    with NodeP^ do begin
      Left := nil;
      Right := nil;
      Bal := 0;
      new(NameP);
      Namep^ := Name;
    end;
  end
  else begin
    NodeP := AvailNameList;
    AvailNameList := NodeP^.Left;
    with NodeP^ do begin
      Left := nil;
      Bal := 0;
      Namep^ := Name;
    end;
  end;
  GetNewNameListNode := NodeP;
end; (* GetNewNameListNode *)

(*---------------------------------------------------------------------------*)

function insertname;(*(
      Name  : NameString;
  var Found : boolean) : NameStringPointer;*)
var
  Higher : boolean;
  NodeP  : BinNodePointer;
  FoundNode : BinNodePointer;
begin (* insertname *)
  NodeP := GetNewNameListNode(Name);
  Found := false;
  INSERT_TREE_NODE(NodeP, NameList, FoundNode, Found, Higher);
  insertname := FoundNode^.NameP;
  if Found then
    DisposeANameList(NodeP);
end; (* insertname *)

procedure InitNameList;
begin
  NameList := nil;
  AvailNameList := nil;
end;

(********************************************************************) 
(*                      NameString - Dynamic Memory Allocation      *) 
(********************************************************************) 

procedure InitNameStringPool;
begin
  GlobalNSPool.Avail := nil;
  GlobalNSPool.Empty := nil;
end;

procedure NewNameString; (* (var NSP: NameStringPointer );*)
(*var Temp: NSPoolP;*)
begin
(*
  if GlobalNSPool.Avail=nil then
    new( NSP )
  else begin
    Temp := GlobalNSPool.Avail;
    GlobalNSPool.Avail := Temp^.Next;
    Temp^.Next := GlobalNSPool.Empty;
    GlobalNSPool.Empty := Temp;
    NSP := Temp^.Data;
  end;
*)
  new(NSP);
  NSP^.Length := 0;
end;

procedure ReleaseNameString; (* (var NSP: NameStringPointer );*)
(*var Temp: NSPoolP;*)
begin
  if NSP <> nil then begin
(*
    if GlobalNSPool.Empty=nil then begin
      new(Temp);
      Temp^.Next := GlobalNSPool.Avail;
      GlobalNSPool.Avail := Temp;
    end
    else begin
      Temp := GlobalNSPool.Empty;
      GlobalNSPool.Empty := Temp^.Next;
      Temp^.Next := GlobalNSPool.Avail;
      GlobalNSPool.Avail := Temp;
    end;
    Temp^.Data := NSP;
*)
    dispose(NSP);
    NSP := nil;
  end;
end;

procedure SDTrefStringToRec (* (
  var S : SDTrefString;
  var R : SDTrefRec;
  var Error : integer) *) ;

(* Converts SDTrefString S to a record R (SDTrefRec). If an error is
   detected Error is on exit the position in S where the error where
   detected. If correct Error is 0. *)

label 99;
var
  Len : integer;
  ErrorFound, EndFound : Boolean;

procedure SDTrefSkipSpaces;
var Found : Boolean;
begin
  Found := false;
  while not Found and (Len <= S.Length) do
    if (S.Value[Len] = ' ') or (S.Value[Len] = chr(9)) then
      Len := Len+1
    else
      Found := true;
end;

function SDTrefIsEnd : Boolean;
begin
  SDTrefIsEnd := false;
  if S.Value[Len] = ')' then
  begin
    Len := Len+1;
    SDTrefSkipSpaces;
    if Len > S.Length then
      SDTrefIsEnd := true;
  end;
end;

function SDTrefGetInteger : integer;
var
  Temp : NameString;
  Found : Boolean;
begin
  Temp.Length := 0;
  Found := false;
  while not Found and (Temp.Length <= NameStringLength) and
                      (Len <= S.Length) do
    if S.Value[Len] in ['0'..'9'] then
    begin
      Temp.Length := Temp.Length+1;
      Temp.Value[Temp.Length] := S.Value[Len];
      Len := Len+1;
    end
    else
      Found := true;
  if Temp.Length > 0 then
    SDTrefGetInteger := NmStrToInteger(Temp)
  else
    SDTrefGetInteger := SDTrefUndefInt;
end;

begin
  ErrorFound := true;
  R.IsSDTGR := true;
  R.FileName.Length := 0;
  R.PageName.Length := 0;
  R.ObjectId := SDTrefUndefInt;
  R.XCoord := SDTrefUndefInt;
  R.YCoord := SDTrefUndefInt;
  R.LineNumber := SDTrefUndefInt;
  R.Column := SDTrefUndefInt;

  Len := 1;
  if S.Length = 0 then goto 99;
  if S.Value[1] <> '#' then goto 99;
  Len := 2;
  if S.Value[2] <> 'S' then goto 99;
  Len := 3;
  if S.Value[3] <> 'D' then goto 99;
  Len := 4;
  if S.Value[4] <> 'T' then goto 99;
  Len := 5;
  if S.Value[5] <> 'R' then goto 99;
  Len := 6;
  if S.Value[6] <> 'E' then goto 99;
  Len := 7;
  if S.Value[7] <> 'F' then goto 99;
  Len := 8;
  if S.Value[8] <> '(' then goto 99;
  Len := 9;

  if S.Value[9] = 'S' then
  begin
    Len := 10;
    if S.Value[10] <> 'D' then goto 99;
    Len := 11;
    if S.Value[11] <> 'L' then goto 99;
    Len := 12; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* First comma *)
    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* FileName *)
    EndFound := false;
    while not EndFound and (Len <= S.Length) do
      if S.Value[Len] in [',', ')', '(', ' ', chr(9)] then
        EndFound := true
      else
      begin
        R.FileName.Length := R.FileName.Length+1;
        if R.FileName.Length > S.Length then goto 99;
        R.FileName.Value[R.FileName.Length] := S.Value[Len];
        Len := Len+1;
        if Len > S.Length then goto 99;
      end;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* PageName *)
    if S.Value[Len] = '(' then
    begin
      Len := Len+1; SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      EndFound := false;
      while not EndFound and (Len <= S.Length) do
        if S.Value[Len] in [',', ')', '(', ' ', chr(9)] then
          EndFound := true
        else
        begin
          R.PageName.Length := R.PageName.Length+1;
          if R.PageName.Length > NameStringLength then goto 99;
          R.PageName.Value[R.PageName.Length] := S.Value[Len];
          Len := Len+1;
          if Len > S.Length then goto 99;
        end;
      SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      if S.Value[Len] <> ')' then goto 99;
      Len := Len+1; SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
    end;
    if SDTrefIsEnd then begin ErrorFound := false; goto 99; end;

    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* ObjectId *)
    R.ObjectId := SDTrefGetInteger;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* Object_Coordinates *)
    if S.Value[Len] = '(' then
    begin
      Len := Len+1; SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      R.XCoord := SDTrefGetInteger;
      SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      if S.Value[Len] <> ',' then goto 99;
      Len := Len+1; SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      R.YCoord := SDTrefGetInteger;
      SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
      if S.Value[Len] <> ')' then goto 99;
      Len := Len+1; SDTrefSkipSpaces;
      if Len > S.Length then goto 99;
    end;
    if SDTrefIsEnd then begin ErrorFound := false; goto 99; end;

    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* LineNumber *)
    R.LineNumber := SDTrefGetInteger;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;
    if SDTrefIsEnd then begin ErrorFound := false; goto 99; end;

    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* Column *)
    R.Column := SDTrefGetInteger;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;
    if SDTrefIsEnd then ErrorFound := false;
  end

  else if S.Value[9] = 'T' then
  begin
    Len := 10;
    R.IsSDTGR := false;
    if S.Value[10] <> 'E' then goto 99;
    Len := 11; 
    if S.Value[11] <> 'X' then goto 99;
    Len := 12;
    if S.Value[12] <> 'T' then goto 99;
    Len := 13; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* First comma *)
    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* FileName *)
    EndFound := false;
    while not EndFound and (Len <= S.Length) do
      if S.Value[Len] in [',', ')', '(', ' ', chr(9)] then
        EndFound := true
      else
      begin
        R.FileName.Length := R.FileName.Length+1;
        if R.FileName.Length > S.Length then goto 99;
        R.FileName.Value[R.FileName.Length] := S.Value[Len];
        Len := Len+1;
        if Len > S.Length then goto 99;
      end;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;
    if SDTrefIsEnd then begin ErrorFound := false; goto 99; end;

    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* LineNumber *)
    R.LineNumber := SDTrefGetInteger;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;
    if SDTrefIsEnd then begin ErrorFound := false; goto 99; end;

    if S.Value[Len] <> ',' then goto 99;
    Len := Len+1; SDTrefSkipSpaces;
    if Len > S.Length then goto 99;

    (* Column *)
    R.Column := SDTrefGetInteger;
    SDTrefSkipSpaces;
    if Len > S.Length then goto 99;
    if SDTrefIsEnd then ErrorFound := false;
  end;

99:
  if ErrorFound then
    Error := Len
  else
    Error := 0;
end;


procedure SDTrefRecToString  (* (
  var R : SDTrefRec;
  var S : SDTrefString) *) ;

(* Converts SDTrefRec R to a string S (SDTrefString). If an error is
   detected (string is not long enough) S.Length becomes 0 on exit *)

label 99;
var
  Len, I : integer;
  Temp : NameString;
begin
  S.Value[1] := '#';
  S.Value[2] := 'S';
  S.Value[3] := 'D';
  S.Value[4] := 'T';
  S.Value[5] := 'R';
  S.Value[6] := 'E';
  S.Value[7] := 'F';
  S.Value[8] := '(';
  S.Length := 8;
  if R.IsSDTGR then
  begin
    Temp.Value[1] := 'S';
    Temp.Value[2] := 'D';
    Temp.Value[3] := 'L';
    Temp.Value[4] := ',';
    Temp.Length := 4;
    S := Concatenatenamestrings(S, Temp);
    Len := S.Length;
    (* FileName *)
    for I := 1 to R.FileName.Length do
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := R.FileName.Value[I];
    end;

    (* PageName *)
    if R.PageName.Length > 0 then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := '(';
      for I := 1 to R.PageName.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := R.PageName.Value[I];
      end;
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ')';
    end;

    (* ObjectId *)
    if R.ObjectId <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.ObjectId);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
    end;

    (* Object_Coordinates *)
    if R.XCoord <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := '(';
      Temp := integertonmstr(R.XCoord);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.YCoord);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ')';
    end;

    (* LineNumber *)
    if R.LineNumber <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.LineNumber);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
    end;

    (* Column *)
    if R.Column <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.Column);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
    end;

    Len := Len+1;
    if Len > SDTrefStringLength then goto 99;
    S.Value[Len] := ')';
  end

  else  (* if PR *)

  begin
    Temp.Value[1] := 'T';
    Temp.Value[2] := 'E';
    Temp.Value[3] := 'X';
    Temp.Value[4] := 'T';
    Temp.Value[5] := ',';
    Temp.Length := 5;
    S := Concatenatenamestrings(S, Temp);
    Len := S.Length;
    (* FileName *)
    for I := 1 to R.FileName.Length do
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := R.FileName.Value[I];
    end;

    (* LineNumber *)
    if R.LineNumber <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.LineNumber);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
    end;

    (* Column *)
    if R.Column <> SDTrefUndefInt then
    begin
      Len := Len+1;
      if Len > SDTrefStringLength then goto 99;
      S.Value[Len] := ',';
      Temp := integertonmstr(R.Column);
      for I := 1 to Temp.Length do
      begin
        Len := Len+1;
        if Len > SDTrefStringLength then goto 99;
        S.Value[Len] := Temp.Value[I];
      end;
    end;

    Len := Len+1;
    if Len > SDTrefStringLength then goto 99;
    S.Value[Len] := ')';
  end;

99:
  if Len > SDTrefStringLength then
    S.Length := 0
  else
    S.Length := Len;
end;

function NmStrToErrStr;(*(
  NmStr : NameString) : ErrorString;*)
var
  ErrStr : ErrorString;
  i : integer;
begin
  for i := 1 to NmStr.Length do
    ErrStr.Value[i] := NmStr.Value[i];
  ErrStr.Length := NmStr.Length;
  NmStrToErrStr := ErrStr;
end;

function ErrStrToNmStr;(*(
  ErrStr : ErrorString) : NameString;*)
var
  NmStr : NameString;
  i : integer;
  n : integer;
begin
  if ErrStr.Length < NameStringLength then
    n := ErrStr.Length
  else
    n := NameStringLength;
  for i := 1 to n do
    NmStr.Value[i] := ErrStr.Value[i];
  NmStr.Length := n;
  ErrStrToNmStr := NmStr;
end;

(*------------------------------------------------------------------*)
(*                   GetTextRef                                     *)
(*------------------------------------------------------------------*)
function GetTextRef;(*(
  FNm : NameString;
  Ln : integer;
  Col : integer) : NameString;*)
var
  Ref : SDTrefRec;
  S : NameString;
begin(* GetTextRef *)
  Ref.IsSDTGR := false;
  Ref.FileName := FNm;
  Ref.LineNumber := Ln;
  Ref.Column := Col;
  SDTrefRecToString(Ref, S);
  GetTextRef := S;
end; (* GetTextRef *)

 (* module COMMON *)
