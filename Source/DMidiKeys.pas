unit DMidiKeys;

{$I JEDI.INC}
{$R DMidiKeys.res}
{$R-}

interface

uses {$IFDEF FPC} LCLIntf, LResources, LMessages,
     {$ELSE} Windows, {$ENDIF}
     Classes, Graphics, Forms, Controls, ExtCtrls, Messages;

type
  TMidiKeyEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer) of object;
  TKeyColorEvent = procedure(Sender: TObject; Key: Integer; var Color : TColor) of object;

  TMidiKeys = class(TGraphicControl)
  private
    fBlackKeyHeight : Integer;
    fBuffer         : TBitmap;
    fKeysDown       : array[0..127] of Boolean;
    fNumOctaves     : Word;
    fBaseOct        : Integer;
    fOnMidiKeyUp    : TMidiKeyEvent;
    fOnMidiKeyDown  : TMidiKeyEvent;
    fOnKeyDown      : TKeyEvent;
    fOnKeyPress     : TKeyPressEvent;
    fOnKeyUp        : TKeyEvent;
    fLastNote       : Word;
    fShadows        : array [0..2] of TColor;
    FOnKeyColor: TKeyColorEvent;
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetNumOctaves(const Value: word);
    procedure SetBaseOct(const Value: integer);
    function GetKeysDown(index: Integer): Boolean;
    procedure SetKeysDown(index: Integer; const Value: Boolean);
    procedure CalcColors(Color: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF FPC}
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    {$ELSE}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    {$ENDIF}
    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function PositionToNote(x,y: Integer): integer;
    property KeyDown[index:Integer]: Boolean read GetKeysDown write SetKeysDown;
  published
    property BaseOctave: Integer read FBaseOct write SetBaseOct default 2;
    property NumOctaves: Word read FNumOctaves write SetNumOctaves default 3;
    property Anchors;
    property Align;
    property Constraints;
    property Color;
    property Enabled;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMidiKeyDown: TMidiKeyEvent read FOnMidiKeyDown write FOnMidiKeyDown;
    property OnMidiKeyUp: TMidiKeyEvent read FOnMidiKeyUp write FOnMidiKeyUp;
    property OnKeyColor: TKeyColorEvent read FOnKeyColor write FOnKeyColor;
  end;

procedure Register;

implementation

uses SysUtils;


procedure TMidiKeys.CalcColors(Color : TColor);
begin
 fShadows[0]:=round(0.8*((Color shr 16) and $FF)) shl 16 + round(0.8*((Color shr 8) and $FF)) shl 8 + round(0.8*(Color and $FF));
 fShadows[1]:=round(0.5*((Color shr 16) and $FF)) shl 16 + round(0.5*((Color shr 8) and $FF)) shl 8 + round(0.5*(Color and $FF));
 fShadows[2]:=round(0.3*((Color shr 16) and $FF)) shl 16 + round(0.3*((Color shr 8) and $FF)) shl 8 + round(0.3*(Color and $FF));
end;

procedure TMidiKeys.CMColorchanged(var Message: TMessage);
begin
 CalcColors(Color);
end;

constructor TMidiKeys.Create(AOwner: TComponent);
var i: integer;
begin
 inherited Create(AOwner);
 fBaseOct := 2;
 fBuffer := TBitmap.Create;
 fNumOctaves := 3;
 Color := clWhite;
 ControlStyle := ControlStyle+[csOpaque];
 for i := 0 to 127 do fKeysDown[i] := false;
end;

destructor TMidiKeys.Destroy;
begin
 fBuffer.Free;
 inherited;
end;

procedure TMidiKeys.Paint;
var i,o  : Integer;
    s    : Single;
//    kd   : Boolean;
    kcol : TColor;
    ncol : TColor;
    key  : Integer;
begin
 with fBuffer.Canvas do
  begin
   Pen.Color:=clBlack;
   Pen.Style:=psSolid;
   kcol:=Self.Color;

   // Render white keys
   s:=Width/(7*NumOctaves+1);
   for i:=0 to 7*NumOctaves do
    begin
     o:=12*(i div 7);
     case (i mod 7) of
      0: key:=BaseOctave*12+o   ;
      1: key:=BaseOctave*12+o+ 2;
      2: key:=BaseOctave*12+o+ 4;
      3: key:=BaseOctave*12+o+ 5;
      4: key:=BaseOctave*12+o+ 7;
      5: key:=BaseOctave*12+o+ 9;
      6: key:=BaseOctave*12+o+11;
     end;

     if assigned(FOnKeyColor) then
      begin
       FOnKeyColor(Self,key,kcol);
       CalcColors(kcol);
      end;

     if fKeysDown[key]
      then Brush.Color:=fShadows[0]
      else Brush.Color:=kcol;
     Rectangle(Round(i*s), 0, Round(i*s+s), Height);
     Pen.Color:=fShadows[0]; MoveTo(Round(i*s+s)-1,1);
     LineTo(Round(i*s+s)-1,Height-1); Pen.Color:=clBlack;
    end;

   // Render black keys
   Brush.Color:=clBlack;
   s:=((7*NumOctaves)/(7*NumOctaves+1))*Width/(12*NumOctaves);
   for i:=0 to 12*NumOctaves+1 do
    begin
     if (i mod 12) in [1,3,6,8,10] then
      begin
       Key:=BaseOctave*12+i;

       if assigned(FOnKeyColor) then
        begin
         FOnKeyColor(Self,key,kcol);
         CalcColors(kcol);
        end;

       if fKeysDown[Key]
        then Brush.Color:=fShadows[2]
        else Brush.Color:=clBlack;
       Rectangle(Round((i  )*s), 0, Round((i+1)*s+1), fBlackKeyHeight);
       Pen.Color:=fShadows[1];
       MoveTo(Round(i*s),1);
       LineTo(Round(i*s),fBlackKeyHeight-1);
       LineTo(Round(i*s+s)+1,fBlackKeyHeight-1);
       Pen.Color:=clBlack;
      end;
    end;
  end;
 with Canvas do
  begin
   CopyMode := cmSrcCopy;
   Draw(0, 0, fBuffer);
  end;
end;

procedure TMidiKeys.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var i : Integer;
begin
 MouseCapture := True;
 inherited MouseDown(Button, Shift, X, Y);
 if (x < 0) or (x > width) or (y < 0) or (y > height) then exit;
 i := PositionToNote(x, y);
 if (i<0) or (i>127) then exit;

 if (fKeysDown[i] = false) then
 begin
  if assigned(FOnMidiKeyDown) then FOnMidiKeyDown(self, shift, X, Y, i);
  fKeysDown[i] := true;
 end else
 begin
  if (ssCtrl in shift) then
  begin
   if assigned(FOnMidiKeyUp) then FOnMidiKeyUp(self, shift, X, Y, i);
   fKeysDown[i] := false;
  end;
 end;
 fLastNote := i;
 Invalidate;
end;

procedure TMidiKeys.MouseMove(Shift: TShiftState; X, Y: Integer);
var i : Integer;
begin
 inherited MouseMove(Shift, X, Y);
 if (x < 0) or (x > Width) or (y < 0) or (y > Height) then Exit;
 if ((ssLeft in Shift) or (ssMiddle in Shift)) and not (ssCtrl in Shift) then
  begin
   i := PositionToNote(x, y);
   if (i<0) or (i>127) then Exit;

   if (i <> fLastNote) then
    begin
     fKeysDown[fLastNote] := false;
     if assigned(FOnMidiKeyDown)
      then FOnMidiKeyUp(Self, Shift, X, Y, fLastNote);
     fLastNote := i;
     fKeysDown[i] := true;
     if assigned(FOnMidiKeyUp)
      then FOnMidiKeyDown(Self, Shift, X, Y, fLastNote);
     Invalidate;
    end;
  end;
end;

procedure TMidiKeys.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := False;
 inherited MouseUp(Button, Shift, X, Y);
 if (fKeysDown[fLastNote])and not (ssCtrl in Shift) then
  begin
   if assigned(FOnMidiKeyUp)
    then FOnMidiKeyUp(self, shift, X, Y, fLastNote);
   fKeysDown[fLastNote] := False;
  end;
 Invalidate;
end;

function TMidiKeys.PositionToNote(x, y: Integer): Integer;
var virtWidth : Single;
    i,o       : Integer;
begin
 if y<fBlackKeyHeight
  then
   begin
    virtWidth:=Width*((7*NumOctaves)/(7*NumOctaves+1));
    virtWidth:=virtWidth/(12*NumOctaves);
    Result:=BaseOctave*12+round(x/virtWidth-0.5);
   end
  else
   begin
    i:=Round(x*(7*NumOctaves+1)/width-0.5);
    o:=12*(i div 7);
    case (i mod 7) of
     0: Result:=BaseOctave*12+o   ;
     1: Result:=BaseOctave*12+o+ 2;
     2: Result:=BaseOctave*12+o+ 4;
     3: Result:=BaseOctave*12+o+ 5;
     4: Result:=BaseOctave*12+o+ 7;
     5: Result:=BaseOctave*12+o+ 9;
     6: Result:=BaseOctave*12+o+11;
    end;
   end;
end;

procedure TMidiKeys.SetBaseOct(const Value: integer);
begin
 if (Value < 0) or (Value > 10) then exit;
 fBaseOct := Value;
 while fBaseOct + fNumOctaves > 10 do SetNumOctaves(fNumOctaves - 1);
end;

procedure TMidiKeys.SetKeysDown(index: Integer; const Value: Boolean);
begin
 fKeysDown[index]:=Value;
 Invalidate;
end;

function TMidiKeys.GetKeysDown(index: Integer): Boolean;
begin
 result:=fKeysDown[index];
end;

procedure TMidiKeys.SetNumOctaves(const Value: word);
begin
 if (Value < 1) or (Value > 10) then exit;
 FNumOctaves := Value;
 while fBaseOct + fNumOctaves > 10
  do fNumOctaves := fNumOctaves - 1;
 Invalidate;
end;

{$IFDEF FPC}
procedure TMidiKeys.WMEraseBkgnd(var m: TWMEraseBkgnd); begin m.Result := PtrInt(False); end;
procedure TMidiKeys.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
 if Assigned(fBuffer) then
  begin
   fBuffer.Width := Width;
   fBuffer.Height := Height;
   fBlackKeyHeight := Round(0.63*Height);
  end;
end;
{$ELSE}
procedure TMidiKeys.WMEraseBkgnd(var m: TWMEraseBkgnd); begin m.Result := LRESULT(False); end;
procedure TMidiKeys.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
 if Assigned(fBuffer) then
  begin
   fBuffer.Width := Width;
   fBuffer.Height := Height;
   fBlackKeyHeight := Round(0.63*Height);
  end;
end;
{$ENDIF}

procedure Register;
begin
 RegisterComponents('Audio', [TMidiKeys]);
end;

end.
