{ GPC demo program for the CRT unit.

Copyright (C) 1999-2006, 2013-2020 Free Software Foundation, Inc.

Author: Frank Heckenbach <frank@pascal.gnu.de>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, version 3.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

As a special exception, if you incorporate even large parts of the
code of this demo program into another program with substantially
different functionality, this does not cause the other program to
be covered by the GNU General Public License. This exception does
not however invalidate any other reasons why it might be covered
by the GNU General Public License. }

{$gnu-pascal,I+}

(* second style of comment *)
// Free-pascal style comment.
var x:Char = 12 /* 45;   // This /* does not start a comment.
var x:Char = (/ 4);      // This (/ does not start a comment.
var a_to_b : integer;    // 'to' should not be highlighted

program CRTDemo;

uses GPC, CRT;

type
   TFrameChars = array [1 .. 8] of Char;
   TSimulateBlockCursorKind = (bc_None, bc_Blink, bc_Static);

const
   SingleFrame: TFrameChars = (chCornerTLS, chLineHS, chCornerTRS, chLineVS, chLineVS, chCornerBLS, chLineHS, chCornerBRS);
   DoubleFrame: TFrameChars = (chCornerTLD, chLineHD, chCornerTRD, chLineVD, chLineVD, chCornerBLD, chLineHD, chCornerBRD);

var
   ScrollState: Boolean = True;
   SimulateBlockCursorKind: TSimulateBlockCursorKind = bc_None;
   CursorShape: TCursorShape = CursorNormal;
   MainPanel: TPanel;
   OrigScreenSize: TPoint;

procedure FrameWin (const Title: String; const Frame: TFrameChars; TitleInverse: Boolean);
var
   w, h, y, Color: Integer;
   Attr: TTextAttr;
begin
   HideCursor;
   SetPCCharSet (True);
   ClrScr;
   w := GetXMax;
   h := GetYMax;
   WriteCharAt (1, 1, 1,     Frame[1], TextAttr);
   WriteCharAt (2, 1, w - 2, Frame[2], TextAttr);
   WriteCharAt (w, 1, 1,     Frame[3], TextAttr);
   for y := 2 to h - 1 do
   begin
      WriteCharAt (1, y, 1, Frame[4], TextAttr);
      WriteCharAt (w, y, 1, Frame[5], TextAttr)
   end;
   WriteCharAt (1, h, 1,     Frame[6], TextAttr);
   WriteCharAt (2, h, w - 2, Frame[7], TextAttr);
   WriteCharAt (w, h, 1,     Frame[8], TextAttr);
   SetPCCharSet (False);
   Attr := TextAttr;
   if TitleInverse then
   begin
      Color := GetTextColor;
      TextColor (GetTextBackground);
      TextBackground (Color)
   end;
   WriteStrAt ((w - Length (Title)) div 2 + 1, 1, Title, TextAttr);
   TextAttr := Attr
end;

function GetKey (TimeOut: Integer) = Key: TKey; forward;

procedure ClosePopUpWindow;
begin
   PanelDelete (GetActivePanel);
   PanelDelete (GetActivePanel)
end;

function PopUpConfirm (XSize, YSize: Integer; const Msg: String): Boolean;
var
   ax, ay: Integer;
   Key: TKey;
   SSize: TPoint;
begin
   repeat
      SSize := ScreenSize;
      ax := (SSize.x - XSize - 4) div 2 + 1;
      ay := (SSize.y - YSize - 4) div 2 + 1;
      PanelNew (ax, ay, ax + XSize + 3, ay + YSize + 1, False);
      TextBackground (Black);
      TextColor (Yellow);
      SetControlChars (True);
      FrameWin ('', DoubleFrame, False);
      NormalCursor;
      PanelNew (ax + 2, ay + 1, ax + XSize + 2, ay + YSize, False);
      ClrScr;
      Write (Msg);
      Key := GetKey (-1);
      if Key = kbScreenSizeChanged then ClosePopUpWindow
      until Key <> kbScreenSizeChanged;
   PopUpConfirm := not (Key in [kbEsc, kbAltEsc])
end;

procedure MainDraw;
begin
   WriteLn ('3, F3 : Open a window');
   WriteLn ('4, F4 : Close window');
   WriteLn ('5, F5 : Previous window');
   WriteLn ('6, F6 : Next window');
   WriteLn ('7, F7 : Move window');
   WriteLn ('8, F8 : Resize window');
   Write   ('q, Esc: Quit')
end;

procedure StatusDraw;
const
   YesNo: array [Boolean] of String [3] = ('No', 'Yes');
   SimulateBlockCursorIDs: array [TSimulateBlockCursorKind] of String [8] = ('Off', 'Blinking', 'Static');
   CursorShapeIDs: array [TCursorShape] of String [7] = ('Ignored', 'Hidden', 'Normal', 'Fat', 'Block');
var
   SSize: TPoint;
begin
   WriteLn ('You can change some of the following');
   WriteLn ('settings  by pressing the key  shown');
   WriteLn ('in parentheses. Naturally, color and');
   WriteLn ('changing the cursor  shape or screen');
   WriteLn ('size does not work on all terminals.');
   WriteLn;
   WriteLn ('XCurses version:          ', YesNo[XCRT]);
   WriteLn ('CRTSavePreviousScreen:    ', YesNo[CRTSavePreviousScreenWorks]);
   WriteLn ('(M)onochrome:             ', YesNo[IsMonochrome]);
   SSize := ScreenSize;
   WriteLn ('Screen (C)olumns:         ', SSize.x);
   WriteLn ('Screen (L)ines:           ', SSize.y);
   WriteLn ('(R)estore screen size');
   WriteLn ('(B)reak checking:         ', YesNo[CheckBreak]);
   WriteLn ('(S)crolling:              ', YesNo[ScrollState]);
   WriteLn ('S(i)mulated block cursor: ', SimulateBlockCursorIDs[SimulateBlockCursorKind]);
   Write   ('C(u)rsor shape:           ', CursorShapeIDs[CursorShape]);
   GotoXY (36, WhereY)
end;

procedure RedrawAll; forward;
procedure CheckScreenSize; forward;

procedure StatusKey (Key: TKey);
var SSize, NewSize: TPoint;
begin
   case LoCase (Key2Char (Key)) of
     'm': begin
	SetMonochrome (not IsMonochrome);
	RedrawAll
     end;
     'c': begin
	SSize := ScreenSize;
	if SSize.x > 40 then
	   NewSize.x := 40
	else
	   NewSize.x := 80;
	if SSize.y > 25 then
	   NewSize.y := 50
	else
	   NewSize.y := 25;
	SetScreenSize (NewSize.x, NewSize.y);
	CheckScreenSize
     end;
     'l': begin
	SSize := ScreenSize;
	if SSize.x > 40 then
	   NewSize.x := 80
	else
	   NewSize.x := 40;
	if SSize.y > 25 then
	   NewSize.y := 25
	else
	   NewSize.y := 50;
	SetScreenSize (NewSize.x, NewSize.y);
	CheckScreenSize
     end;
     'r': begin
	SetScreenSize (OrigScreenSize.x, OrigScreenSize.y);
	CheckScreenSize
     end;
     'b': CheckBreak := not CheckBreak;
     's': ScrollState := not ScrollState;
     'i': if SimulateBlockCursorKind = High (SimulateBlockCursorKind) then
	SimulateBlockCursorKind := Low (SimulateBlockCursorKind)
     else
	Inc (SimulateBlockCursorKind);
     'u': case CursorShape of
       CursorNormal: CursorShape := CursorBlock;
       CursorFat,
       CursorBlock : CursorShape := CursorHidden;
     else          CursorShape := CursorNormal
     end;
   end;
   ClrScr;
   StatusDraw
end;

procedure TextAttrDemo;
var f, b, y, x1, y1, x2, y2, Fill, n1, n2, n3: Integer;
begin
   GetWindow (x1, y1, x2, y2);
   Window (x1 - 1, y1, x2, y2);
   TextColor (White);
   TextBackground (Blue);
   ClrScr;
   SetScroll (False);
   Fill := GetXMax - 32;
   for y := 1 to GetYMax do
   begin
      GotoXY (1, y);
      b := (y - 1) mod 16;
      n1 := 0;
      for f := 0 to 15 do
      begin
	 TextAttr := f + 16 * b;
	 n2 := (Fill * (1 + 2 * f) + 16) div 32;
	 n3 := (Fill * (2 + 2 * f) + 16) div 32;
	 Write ('' : n2 - n1, NumericBaseDigitsUpper[b], NumericBaseDigitsUpper[f], '' : n3 - n2);
	 n1 := n3
      end
   end
end;

procedure CharSetDemo (UsePCCharSet: Boolean);
var h, l, y, x1, y1, x2, y2, Fill, n1, n2: Integer;
begin
   GetWindow (x1, y1, x2, y2);
   Window (x1 - 1, y1, x2, y2);
   ClrScr;
   SetScroll (False);
   SetPCCharSet (UsePCCharSet);
   SetControlChars (False);
   Fill := GetXMax - 35;
   for y := 1 to GetYMax do
   begin
      GotoXY (1, y);
      h := (y - 2) mod 16;
      n1 := (Fill + 9) div 18;
      if y = 1 then
	 Write ('' : 3 + n1)
      else
	 Write (16 * h : 3 + n1);
      for l := 0 to 15 do
      begin
	 n2 := (Fill * (2 + l) + 9) div 18;
	 if y = 1 then
	    Write ('' : n2 - n1, l : 2)
	 else
	    Write ('' : n2 - n1 + 1, Chr (16 * h + l));
	 n1 := n2
      end
   end
end;

procedure NormalCharSetDemo;
begin
   CharSetDemo (False)
end;

procedure PCCharSetDemo;
begin
   CharSetDemo (True)
end;

procedure FKeyDemoDraw;
var x1, y1, x2, y2: Integer;
begin
   GetWindow (x1, y1, x2, y2);
   Window (x1, y1, x2 - 1, y2);
   ClrScr;
   SetScroll (False);
   WriteLn ('You can type the following keys');
   WriteLn ('(function keys if present on the');
   WriteLn ('terminal, letters as alternatives):');
   GotoXY (1, 4);
   WriteLn ('S, Left     : left (wrap-around)');
   WriteLn ('D, Right    : right (wrap-around)');
   WriteLn ('E, Up       : up (wrap-around)');
   WriteLn ('X, Down     : down (wrap-around)');
   WriteLn ('A, Home     : go to first column');
   WriteLn ('F, End      : go to last column');
   WriteLn ('R, Page Up  : go to first line');
   WriteLn ('C, Page Down: go to last line');
   WriteLn ('Y, Ctrl-PgUp: first column and line');
   GotoXY (1, 13);
   WriteLn ('B, Ctrl-PgDn: last column and line');
   WriteLn ('Z, Ctrl-Home: clear screen');
   WriteLn ('N, Ctrl-End : clear to end of line');
   WriteLn ('V, Insert   : insert a line');
   WriteLn ('T, Delete   : delete a line');
   WriteLn ('#           : beep');
   WriteLn ('*           : flash');
   WriteLn ('Tab, Enter, Backspace, other');
   WriteLn ('  normal characters: write text')
end;

procedure FKeyDemoKey (Key: TKey);
const TabSize = 8;
var
   ch: Char;
   NewX: Integer;
begin
   case LoCaseKey (Key) of
     Ord ('s'), kbLeft    : if WhereX = 1 then GotoXY (GetXMax, WhereY) else GotoXY (WhereX - 1, WhereY);
     Ord ('d'), kbRight   : if WhereX = GetXMax then GotoXY (1, WhereY) else GotoXY (WhereX + 1, WhereY);
     Ord ('e'), kbUp      : if WhereY = 1 then GotoXY (WhereX, GetYMax) else GotoXY (WhereX, WhereY - 1);
     Ord ('x'), kbDown    : if WhereY = GetYMax then GotoXY (WhereX, 1) else GotoXY (WhereX, WhereY + 1);
     Ord ('a'), kbHome    : Write (chCR);
     Ord ('f'), kbEnd     : GotoXY (GetXMax, WhereY);
     Ord ('r'), kbPgUp    : GotoXY (WhereX, 1);
     Ord ('c'), kbPgDn    : GotoXY (WhereX, GetYMax);
     Ord ('y'), kbCtrlPgUp: GotoXY (1, 1);
     Ord ('b'), kbCtrlPgDn: GotoXY (GetXMax, GetYMax);
     Ord ('z'), kbCtrlHome: ClrScr;
     Ord ('n'), kbCtrlEnd : ClrEOL;
     Ord ('v'), kbIns     : InsLine;
     Ord ('t'), kbDel     : DelLine;
     Ord ('#')            : Beep;
     Ord ('*')            : Flash;
     kbTab                : begin
			       NewX := ((WhereX - 1) div TabSize + 1) * TabSize + 1;
			       if NewX <= GetXMax then GotoXY (NewX, WhereY) else WriteLn
			    end;
     kbCR                 : WriteLn;
     kbBkSp               : Write (chBkSp, ' ', chBkSp);
   else                   ch := Key2Char (Key);
     if ch <> #0 then Write (ch)
   end
end;

procedure KeyDemoDraw;
begin
   WriteLn ('Press some keys ...')
end;

procedure KeyDemoKey (Key: TKey);
var ch: Char;
begin
   ch := Key2Char (Key);
   if ch <> #0 then
   begin
      Write ('Normal key');
      if IsPrintable (ch) then Write (' `', ch, '''');
      WriteLn (', ASCII #', Ord (ch))
   end
   else
      WriteLn ('Special key ', Ord (Key2Scan (Key)))
end;

procedure IOSelectPeriodical;
var
   CurrentTime: TimeStamp;
   s: String (8);
   i: Integer;
begin
   GetTimeStamp (CurrentTime);
   with CurrentTime do
      WriteStr (s, Hour : 2, ':', Minute : 2, ':', Second : 2);
   for i := 1 to Length (s) do
      if s[i] = ' ' then s[i] := '0';
   GotoXY (1, 12);
   Write ('The time is: ', s)
end;

procedure IOSelectDraw;
begin
   WriteLn ('IOSelect is a way to handle I/O from');
   WriteLn ('or to several places simultaneously,');
   WriteLn ('without  having  to use  threads  or');
   WriteLn ('signal/interrupt  handlers  or waste');
   WriteLn ('CPU time with busy waiting.');
   WriteLn;
   WriteLn ('This demo  shows how  IOSelect works');
   WriteLn ('in connection with CRT.  It displays');
   WriteLn ('a clock,  but still  reacts  to user');
   WriteLn ('input immediately.');
   IOSelectPeriodical
end;

procedure ModifierPeriodical;
const
   Pressed: array [Boolean] of String [8] = ('Released', 'Pressed');
   ModifierNames: array [1 .. 7] of record
		     Modifier: Integer;
		     Name: String (17)
		  end =
   ((shLeftShift,  'Left Shift'),
    (shRightShift, 'Right Shift'),
    (shLeftCtrl,   'Left Control'),
    (shRightCtrl,  'Right Control'),
    (shAlt,        'Alt (left)'),
    (shAltGr,      'AltGr (right Alt)'),
    (shExtra,      'Extra'));
var
   ShiftState, i: Integer;
begin
   ShiftState := GetShiftState;
   for i := 1 to 7 do
      with ModifierNames[i] do
      begin
	 GotoXY (1, 4 + i);
	 ClrEOL;
	 Write (Name, ':');
	 GotoXY (20, WhereY);
	 Write (Pressed[(ShiftState and Modifier) <> 0])
      end
end;

procedure ModifierDraw;
begin
   WriteLn ('Modifier keys (NOTE: only');
   WriteLn ('available on some systems;');
   WriteLn ('X11: only after key press):');
   ModifierPeriodical
end;

procedure ChecksDraw;
begin
   WriteLn ('(O)S shell');
   WriteLn ('OS shell with (C)learing');
   WriteLn ('(R)efresh check');
   Write   ('(S)ound check')
end;

procedure ChecksKey (Key: TKey);
var
   i, j: Integer;
   WasteTime: Real; attribute (volatile);

   procedure DoOSShell;
   var
      Result: Integer;
      Shell: TString;
   begin
      Shell := GetShellPath (Null);
      {$I-}
      Result := Execute (Shell);
      {$I+}
      if (InOutRes <> 0) or (Result <> 0) then
      begin
	 ClrScr;
	 if InOutRes <> 0 then
	    WriteLn (GetIOErrorMessage, ' while trying to execute `', Shell, '''.')
	 else
	    WriteLn ('`', Shell, ''' returned status ', Result, '.');
	 Write ('Any key to continue.');
	 BlockCursor;
	 Discard (GetKey (-1))
      end
   end;

begin
   case LoCase (Key2Char (Key)) of
     'o': begin
	if PopUpConfirm (36, 12, 'You will now get an OS shell. Unless' + NewLine +
			 'CRTDemo is running  in its own (GUI)' + NewLine +
			 'window,  the shell  will run  on the' + NewLine +
			 'same screen as CRTDemo  which is not' + NewLine +
			 'cleared before the shell is started.' + NewLine +
			 'If possible, the screen contents are' + NewLine +
			 'restored to the state before CRTDemo' + NewLine +
			 'was started. After leaving the shell' + NewLine +
			 'in the usual way (usually  by enter-' + NewLine +
			 'ing  `exit''), you will  get back to' + NewLine +
			 'the demo.  <ESC> to abort, any other' + NewLine +
			 'key to start.') then
	begin
	   RestoreTerminal (True);
	   DoOSShell
	end;
	ClosePopUpWindow
     end;
     'c': begin
	if PopUpConfirm (36, 9, 'You will now get an OS shell. Unless' + NewLine +
			 'CRTDemo is running in  its own (GUI)' + NewLine +
			 'window, the screen  will be cleared,' + NewLine +
			 'and the cursor will be  moved to the' + NewLine +
			 'top  before  the  shell  is started.' + NewLine +
			 'After leaving the shell in the usual' + NewLine +
			 'way  (usually  by entering  `exit''),' + NewLine +
			 'you will get back to the demo. <ESC>' + NewLine +
			 'to abort, any other key to start.') then
	begin
	   RestoreTerminalClearCRT;
	   DoOSShell
	end;
	ClosePopUpWindow
     end;
     'r': begin
	if PopUpConfirm (36, 11, 'The program will  now get  busy with' + NewLine +
			 'some  dummy  computations.  However,' + NewLine +
			 'CRT output in  the form of dots will' + NewLine +
			 'still appear continuously one by one' + NewLine +
			 '(rather than the  whole line at once' + NewLine +
			 'in the end). While running, the test' + NewLine +
			 'cannot  be  interrupted.   <ESC>  to' + NewLine +
			 'abort, any other key to start.') then
	begin
	   SetCRTUpdate (UpdateRegularly);
	   BlockCursor;
	   WriteLn;
	   WriteLn;
	   for i := 1 to GetXMax - 2 do
	   begin
	      Write ('.');
	      for j := 1 to 400000 do WasteTime := Random
	   end;
	   SetCRTUpdate (UpdateInput);
	   WriteLn;
	   Write ('Press any key.');
	   Discard (GetKey (-1))
	end;
	ClosePopUpWindow
     end;
     's': begin
	if PopUpConfirm (32, 4, 'You will now hear some sounds if' + NewLine +
			 'supported  (otherwise there will' + NewLine +
			 'just be a short pause). <ESC> to' + NewLine +
			 'abort, any other key to start.') then
	begin
	   BlockCursor;
	   for i := 0 to 7 do
	   begin
	      Sound (Round (440 * 2 ** (Round (i * 12 / 7 + 0.3) / 12)));
	      if GetKey (400000) in [kbEsc, kbAltEsc] then Break
	   end;
	   NoSound
	end;
	ClosePopUpWindow
     end;
   end
end;

type
   PWindowList = ^TWindowList;
   TWindowList = record
		    Next, Prev: PWindowList;
		    Panel, FramePanel: TPanel;
		    WindowType: Integer;
		    x1, y1, xs, ys: Integer;
		    State: (ws_None, ws_Moving, ws_Resizing);
		 end;

TKeyProc = procedure (Key: TKey);
TProcedure = procedure;

const
   MenuNameLength = 16;
   WindowTypes: array [0 .. 9] of record
		   DrawProc,
		   PeriodicalProc: procedure;
		   KeyProc       : TKeyProc;
		   Name          : String (MenuNameLength);
		   Color,
		   Background,
		   MinSizeX,
		   MinSizeY,
		   PrefSizeX,
		   PrefSizeY     : Integer;
		   RedrawAlways,
		   WantCursor    : Boolean
				   end =
((MainDraw         , nil               , nil        , 'CRT Demo'        , LightGreen, Blue     , 26,  7,  0,  0, False, False),
 (StatusDraw       , nil               , StatusKey  , 'Status'          , White     , Red      , 38, 16,  0,  0, True,  True),
 (TextAttrDemo     , nil               , nil        , 'Text Attributes' , White     , Blue     , 32, 16, 64, 16, False, False),
 (NormalCharSetDemo, nil               , nil        , 'Character Set'   , Black     , Green    , 35, 17, 53, 17, False, False),
 (PCCharSetDemo    , nil               , nil        , 'PC Character Set', Black     , Brown    , 35, 17, 53, 17, False, False),
 (KeyDemoDraw      , nil               , KeyDemoKey , 'Keys'            , Blue      , LightGray, 29,  5, -1, -1, False, True),
 (FKeyDemoDraw     , nil               , FKeyDemoKey, 'Function Keys'   , Blue      , LightGray, 37, 22, -1, -1, False, True),
 (ModifierDraw     , ModifierPeriodical, nil        , 'Modifier Keys'   , Black     , Cyan     , 29, 11,  0,  0, True,  False),
 (IOSelectDraw     , IOSelectPeriodical, nil        , 'IOSelect Demo'   , White     , Magenta  , 38, 12,  0,  0, False, False),
 (ChecksDraw       , nil               , ChecksKey  , 'Various Checks'  , Black     , Red      , 26,  4,  0,  0, False, False));

MenuMax = High (WindowTypes);
MenuXSize = MenuNameLength + 4;
MenuYSize = MenuMax + 2;

var
   WindowList: PWindowList = nil;

   procedure RedrawFrame (p: PWindowList);
   begin
      with p^, WindowTypes[WindowType] do
      begin
	 PanelActivate (FramePanel);
	 Window (x1, y1, x1 + xs - 1, y1 + ys - 1);
	 ClrScr;
	 case State of
	   ws_None    : if p = WindowList then
	      FrameWin (' ' + Name + ' ', DoubleFrame, True)
	   else
	      FrameWin (' ' + Name + ' ', SingleFrame, False);
	   ws_Moving  : FrameWin (' Move Window ', SingleFrame, True);
	   ws_Resizing: FrameWin (' Resize Window ', SingleFrame, True);
	 end
      end
   end;

   procedure DrawWindow (p: PWindowList);
   begin
      with p^, WindowTypes[WindowType] do
      begin
	 RedrawFrame (p);
	 PanelActivate (Panel);
	 Window (x1 + 2, y1 + 1, x1 + xs - 2, y1 + ys - 2);
	 ClrScr;
	 DrawProc
      end
   end;

   procedure RedrawAll;
   var
      LastPanel: TPanel;
      p: PWindowList;
      x2, y2: Integer;
   begin
      LastPanel := GetActivePanel;
      PanelActivate (MainPanel);
      TextBackground (Blue);
      ClrScr;
      p := WindowList;
      if p <> nil then
	 repeat
	    with p^ do
	    begin
	       PanelActivate (FramePanel);
	       GetWindow (x1, y1, x2, y2);  { updated automatically by CRT }
	       xs := x2 - x1 + 1;
	       ys := y2 - y1 + 1
	    end;
	    DrawWindow (p);
	    p := p^.Next
	 until p = WindowList;
      PanelActivate (LastPanel)
   end;

   procedure CheckScreenSize;
   var
      LastPanel: TPanel;
      MinScreenSizeX, MinScreenSizeY, i: Integer;
      SSize: TPoint;
   begin
      LastPanel := GetActivePanel;
      PanelActivate (MainPanel);
      HideCursor;
      MinScreenSizeX := MenuXSize;
      MinScreenSizeY := MenuYSize;
      for i := Low (WindowTypes) to High (WindowTypes) do
	 with WindowTypes[i] do
	 begin
	    MinScreenSizeX := Max (MinScreenSizeX, MinSizeX + 2);
	    MinScreenSizeY := Max (MinScreenSizeY, MinSizeY + 2)
	 end;
      SSize := ScreenSize;
      Window (1, 1, SSize.x, SSize.y);
      if (SSize.x < MinScreenSizeX) or (SSize.y < MinScreenSizeY) then
      begin
	 NormVideo;
	 ClrScr;
	 RestoreTerminal (True);
	 WriteLn (StdErr, 'Sorry, your screen is too small for this demo (', SSize.x, 'x', SSize.y, ').');
	 WriteLn (StdErr, 'You need at least ', MinScreenSizeX, 'x', MinScreenSizeY, ' characters.');
	 Halt (2)
      end;
      PanelActivate (LastPanel);
      RedrawAll
   end;

   procedure Die; attribute (noreturn);
   begin
      NoSound;
      RestoreTerminalClearCRT;
      WriteLn (StdErr, 'You''re trying to kill me. Since I have break checking turned off,');
      WriteLn (StdErr, 'I''m not dying, but I''ll do you a favor and terminate now.');
      Halt (3)
   end;

   function GetKey (TimeOut: Integer) = Key: TKey;
   var
      NeedSelect, SelectValue: Integer;
      SimulateBlockCursorCurrent: TSimulateBlockCursorKind;
      SelectInput: array [1 .. 1] of PAnyFile = (@Input);
      NextSelectTime: MicroSecondTimeType = 0; attribute (static);
      TimeOutTime: MicroSecondTimeType;
      LastPanel: TPanel;
      p: PWindowList;
   begin
      LastPanel := GetActivePanel;
      if TimeOut < 0 then
	 TimeOutTime := High (TimeOutTime)
      else
	 TimeOutTime := GetMicroSecondTime + TimeOut;
      NeedSelect := 0;
      if TimeOut >= 0 then
	 Inc (NeedSelect);
      SimulateBlockCursorCurrent := SimulateBlockCursorKind;
      if SimulateBlockCursorCurrent <> bc_None then
	 Inc (NeedSelect);
      p := WindowList;
      repeat
	 if @WindowTypes[p^.WindowType].PeriodicalProc <> nil then
	    Inc (NeedSelect);
	 p := p^.Next
      until p = WindowList;
      p := WindowList;
      repeat
	 with p^, WindowTypes[WindowType] do
	    if RedrawAlways then
	    begin
	       PanelActivate (Panel);
	       ClrScr;
	       DrawProc
	    end;
	 p := p^.Next
      until p = WindowList;
      if NeedSelect <> 0 then
	 repeat
	    CRTUpdate;
	    SelectValue := IOSelectRead (SelectInput, Max (0, Min (NextSelectTime, TimeOutTime) - GetMicroSecondTime));
	    if SelectValue = 0 then
	    begin
	       case SimulateBlockCursorCurrent of
		 bc_None  : ;
		 bc_Blink : SimulateBlockCursor;
		 bc_Static: begin
		    SimulateBlockCursor;
		    SimulateBlockCursorCurrent := bc_None;
		    Dec (NeedSelect)
		 end
	       end;
	       NextSelectTime := GetMicroSecondTime + 120000;
	       p := WindowList;
	       repeat
		  with p^, WindowTypes[WindowType] do
		     if @PeriodicalProc <> nil then
		     begin
			PanelActivate (Panel);
			PeriodicalProc
		     end;
		  p := p^.Next
	       until p = WindowList
	    end;
	 until (NeedSelect = 0) or (SelectValue <> 0) or ((TimeOut >= 0) and (GetMicroSecondTime >= TimeOutTime));
      if NeedSelect = 0 then
	 SelectValue := 1;
      if SelectValue = 0 then
	 Key := 0
      else
	 Key := ReadKeyWord;
      if SimulateBlockCursorKind <> bc_None then
	 SimulateBlockCursorOff;
      if IsDeadlySignal (Key) then Die;
      if Key = kbScreenSizeChanged then CheckScreenSize;
      PanelActivate (LastPanel)
   end;

   function Menu = n: Integer;
   var
      i, ax, ay: Integer;
      Key: TKey;
      Done: Boolean;
      SSize: TPoint;
   begin
      n := 1;
      repeat
	 SSize := ScreenSize;
	 ax := (SSize.x - MenuXSize) div 2 + 1;
	 ay := (SSize.y - MenuYSize) div 2 + 1;
	 PanelNew (ax, ay, ax + MenuXSize - 1, ay + MenuYSize - 1, False);
	 SetControlChars (True);
	 TextColor (Blue);
	 TextBackground (LightGray);
	 FrameWin (' Select Window ', DoubleFrame, True);
	 IgnoreCursor;
	 PanelNew (ax + 1, ay + 1, ax + MenuXSize - 2, ay + MenuYSize - 2, False);
	 ClrScr;
	 TextColor (Black);
	 SetScroll (False);
	 Done := False;
	 repeat
	    for i := 1 to MenuMax do
	    begin
	       GotoXY (1, i);
	       if i = n then
		  TextBackground (Green)
	       else
		  TextBackground (LightGray);
	       ClrEOL;
	       Write (' ', WindowTypes[i].Name);
	       ChangeTextAttr (2, i, 1, Red + $10 * GetTextBackground)
	    end;
	    Key := GetKey (-1);
	    case LoCaseKey (Key) of
	      kbUp                  : if n = 1 then n := MenuMax else Dec (n);
	      kbDown                : if n = MenuMax then n := 1 else Inc (n);
	      kbHome,
	      kbPgUp,
	      kbCtrlPgUp,
	      kbCtrlHome            : n := 1;
	      kbEnd,
	      kbPgDn,
	      kbCtrlPgDn,
	      kbCtrlEnd             : n := MenuMax;
	      kbCR                  : Done := True;
	      kbEsc, kbAltEsc       : begin
		 n := -1;
		 Done := True
	      end;
	      Ord ('a') .. Ord ('z'): begin
		 i := MenuMax;
		 while (i > 0) and (LoCase (Key2Char (Key)) <> LoCase (WindowTypes[i].Name[1])) do Dec (i);
		 if i > 0 then
		 begin
		    n := i;
		    Done := True
		 end
	      end;
	    end
	 until Done or (Key = kbScreenSizeChanged);
	 ClosePopUpWindow
      until Key <> kbScreenSizeChanged
   end;

   procedure NewWindow (WindowType, ax, ay: Integer);
   var
      p, LastWindow: PWindowList;
      MaxX1, MaxY1: Integer;
      SSize: TPoint;
   begin
      New (p);
      if WindowList = nil then
      begin
	 p^.Prev := p;
	 p^.Next := p
      end
      else
      begin
	 p^.Prev := WindowList;
	 p^.Next := WindowList^.Next;
	 p^.Prev^.Next := p;
	 p^.Next^.Prev := p;
      end;
      p^.WindowType := WindowType;
      with p^, WindowTypes[WindowType] do
      begin
	 SSize := ScreenSize;
	 if PrefSizeX > 0 then xs := PrefSizeX else xs := MinSizeX;
	 if PrefSizeY > 0 then ys := PrefSizeY else ys := MinSizeY;
	 xs := Min (xs + 2, SSize.x);
	 ys := Min (ys + 2, SSize.y);
	 MaxX1 := SSize.x - xs + 1;
	 MaxY1 := SSize.y - ys + 1;
	 if ax = 0 then x1 := Random (MaxX1) + 1 else x1 := Min (ax, MaxX1);
	 if ay = 0 then y1 := Random (MaxY1) + 1 else y1 := Min (ay, MaxY1);
	 if (ax = 0) and (PrefSizeX < 0) then Inc (xs, Random (SSize.x - x1 - xs + 2));
	 if (ax = 0) and (PrefSizeY < 0) then Inc (ys, Random (SSize.y - y1 - ys + 2));
	 State := ws_None;
	 PanelNew (1, 1, 1, 1, False);
	 FramePanel := GetActivePanel;
	 SetControlChars (True);
	 TextColor (Color);
	 TextBackground (Background);
	 PanelNew (1, 1, 1, 1, False);
	 SetPCCharSet (False);
	 Panel := GetActivePanel;
      end;
      LastWindow := WindowList;
      WindowList := p;
      if LastWindow <> nil then RedrawFrame (LastWindow);
      DrawWindow (p)
   end;

   procedure OpenWindow;
   var WindowType: Integer;
   begin
      WindowType := Menu;
      if WindowType >= 0 then NewWindow (WindowType, 0, 0)
   end;

   procedure NextWindow;
   var LastWindow: PWindowList;
   begin
      LastWindow := WindowList;
      WindowList := WindowList^.Next;
      PanelTop (WindowList^.FramePanel);
      PanelTop (WindowList^.Panel);
      RedrawFrame (LastWindow);
      RedrawFrame (WindowList)
   end;

   procedure PreviousWindow;
   var LastWindow: PWindowList;
   begin
      PanelMoveAbove (WindowList^.Panel, MainPanel);
      PanelMoveAbove (WindowList^.FramePanel, MainPanel);
      LastWindow := WindowList;
      WindowList := WindowList^.Prev;
      RedrawFrame (LastWindow);
      RedrawFrame (WindowList)
   end;

   procedure CloseWindow;
   var p: PWindowList;
   begin
      if WindowList^.WindowType <> 0 then
      begin
	 p := WindowList;
	 NextWindow;
	 PanelDelete (p^.FramePanel);
	 PanelDelete (p^.Panel);
	 p^.Next^.Prev := p^.Prev;
	 p^.Prev^.Next := p^.Next;
	 Dispose (p)
      end
   end;

   procedure MoveWindow;
   var
      Done, Changed: Boolean;
      SSize: TPoint;
   begin
      with WindowList^ do
      begin
	 Done := False;
	 Changed := True;
	 State := ws_Moving;
	 repeat
	    if Changed then DrawWindow (WindowList);
	    Changed := True;
	    case LoCaseKey (GetKey (-1)) of
	      Ord ('s'), kbLeft    : if x1 > 1 then Dec (x1);
	      Ord ('d'), kbRight   : if x1 + xs - 1 < ScreenSize.x then Inc (x1);
	      Ord ('e'), kbUp      : if y1 > 1 then Dec (y1);
	      Ord ('x'), kbDown    : if y1 + ys - 1 < ScreenSize.y then Inc (y1);
	      Ord ('a'), kbHome    : x1 := 1;
	      Ord ('f'), kbEnd     : x1 := ScreenSize.x - xs + 1;
	      Ord ('r'), kbPgUp    : y1 := 1;
	      Ord ('c'), kbPgDn    : y1 := ScreenSize.y - ys + 1;
	      Ord ('y'), kbCtrlPgUp: begin
		 x1 := 1;
		 y1 := 1
	      end;
	      Ord ('b'), kbCtrlPgDn: begin
		 SSize := ScreenSize;
		 x1 := SSize.x - xs + 1;
		 y1 := SSize.y - ys + 1
	      end;
	      kbCR,
	      kbEsc, kbAltEsc      : Done := True;
	    else                   Changed := False
	    end
	 until Done;
	 State := ws_None;
	 DrawWindow (WindowList)
      end
   end;

   procedure ResizeWindow;
   var
      Done, Changed: Boolean;
      SSize: TPoint;
   begin
      with WindowList^, WindowTypes[WindowType] do
      begin
	 Done := False;
	 Changed := True;
	 State := ws_Resizing;
	 repeat
	    if Changed then DrawWindow (WindowList);
	    Changed := True;
	    case LoCaseKey (GetKey (-1)) of
	      Ord ('s'), kbLeft    : if xs > MinSizeX + 2 then Dec (xs);
	      Ord ('d'), kbRight   : if x1 + xs - 1 < ScreenSize.x then Inc (xs);
	      Ord ('e'), kbUp      : if ys > MinSizeY + 2 then Dec (ys);
	      Ord ('x'), kbDown    : if y1 + ys - 1 < ScreenSize.y then Inc (ys);
	      Ord ('a'), kbHome    : xs := MinSizeX + 2;
	      Ord ('f'), kbEnd     : xs := ScreenSize.x - x1 + 1;
	      Ord ('r'), kbPgUp    : ys := MinSizeY + 2;
	      Ord ('c'), kbPgDn    : ys := ScreenSize.y - y1 + 1;
	      Ord ('y'), kbCtrlPgUp: begin
		 xs := MinSizeX + 2;
		 ys := MinSizeY + 2
	      end;
	      Ord ('b'), kbCtrlPgDn: begin
		 SSize := ScreenSize;
		 xs := SSize.x - x1 + 1;
		 ys := SSize.y - y1 + 1
	      end;
	      kbCR,
	      kbEsc, kbAltEsc      : Done := True;
	    else                   Changed := False
	    end
	 until Done;
	 State := ws_None;
	 DrawWindow (WindowList)
      end
   end;

   procedure ActivateCursor;
   begin
      with WindowList^, WindowTypes[WindowType] do
      begin
	 PanelActivate (Panel);
	 if WantCursor then
	    SetCursorShape (CursorShape)
	 else
	    HideCursor
      end;
      SetScroll (ScrollState)
   end;

var
   Key: TKey;
   ScreenShot, Done: Boolean;

begin
   ScreenShot := ParamStr (1) = '--screenshot';
   if ParamCount <> Ord (ScreenShot) then
   begin
      RestoreTerminal (True);
      WriteLn (StdErr, ParamStr (0), ': invalid argument `', ParamStr (Ord (ScreenShot) + 1), '''');
      Halt (1)
   end;
   CRTSavePreviousScreen (True);
   SetCRTUpdate (UpdateInput);
   MainPanel := GetActivePanel;
   CheckScreenSize;
   OrigScreenSize := ScreenSize;
   if ScreenShot then
   begin
      CursorShape := CursorBlock;
      NewWindow (6,      1,      1);
      NewWindow (2,      1, MaxInt);
      NewWindow (8, MaxInt,      1);
      NewWindow (5,      1,     27);
      KeyDemoKey (Ord ('f'));
      KeyDemoKey (246);
      KeyDemoKey (kbDown);
      NewWindow (3, MaxInt,     13);
      NewWindow (4, MaxInt,     31);
      NewWindow (7, MaxInt, MaxInt);
      NewWindow (9, MaxInt,     33);
      NewWindow (0,      1,      2);
      NewWindow (1,      1,     14);
      ActivateCursor;
      OpenWindow
   end
   else
      NewWindow (0, 3, 2);
   Done := False;
   repeat
      ActivateCursor;
      Key := GetKey (-1);
      case LoCaseKey (Key) of
	Ord ('3'), kbF3 : OpenWindow;
	Ord ('4'), kbF4 : CloseWindow;
	Ord ('5'), kbF5 : PreviousWindow;
	Ord ('6'), kbF6 : NextWindow;
	Ord ('7'), kbF7 : MoveWindow;
	Ord ('8'), kbF8 : ResizeWindow;
	Ord ('q'), kbEsc,
	kbAltEsc:         Done := True;
      else
	 if WindowList <> nil then
	    with WindowList^, WindowTypes[WindowType] do
	       if @KeyProc <> nil then
	       begin
		  TextColor (Color);
		  TextBackground (Background);
		  KeyProc (Key)
	       end
      end
   until Done
end.
