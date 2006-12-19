unit DWaveform;

{$R DWaveform.res}
{$R-}

interface

uses Windows, Types, Classes, Graphics, Forms, Controls, ExtCtrls, Messages;

type
  TWaveform = class;

  TWaveform = class(TGraphicControl)
  private
    fBuffer         : TBitmap;
    fLineWidth      : Integer;
    fHalfHeight     : Integer;
    fOnKeyDown      : TKeyEvent;
    fOnKeyPress     : TKeyPressEvent;
    fOnKeyUp        : TKeyEvent;
    fLineColor      : TColor;
    fTransparent    : Boolean;
    fNormalize      : Boolean;
    fNormalizeFak   : Single;
    fWavedata       : TSingleDynArray;
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetLinewidth(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWaveLength(const Value: Integer);
    function GetWaveLength: Integer;
    procedure SetNormalize(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure RedrawBuffer;
    property Wavedata : TSingleDynArray read fWavedata;
    property WaveLength : Integer read GetWaveLength write SetWaveLength;
  published
    property Anchors;
    property Align;
    property Constraints;
    property Enabled;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Color;
    property Normalize: Boolean read fNormalize write SetNormalize default False;
    property LineWidth: Integer read fLineWidth write SetLineWidth default 1;
    property LineColor: TColor read fLineColor write SetLineColor;
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation

uses SysUtils;

{ TWaveform }

constructor TWaveform.Create(AOwner: TComponent);
var i : Integer;
begin
 inherited Create(AOwner);
 fLineWidth := 1;
 fLineColor := clBlack;
 fTransparent := False;
 fNormalize := False;
 fBuffer := TBitmap.Create;
 WaveLength:=1024;
 for i:=0 to WaveLength - 1
  do fWaveData[i]:=sin(2*Pi*i/WaveLength);

 ControlStyle := ControlStyle+[csOpaque];
end;

destructor TWaveform.Destroy;
begin
 fBuffer.Free;
 inherited;
end;

function TWaveform.GetWaveLength: Integer;
begin
 result:=Length(fWavedata);
end;

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
 with Control do
  begin
   if Parent = nil then Exit;
   DC := Dest.Handle;
   SaveIndex := SaveDC(DC);
   GetViewportOrgEx(DC, Position);
   SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
   IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
   Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
   Parent.Perform(WM_PAINT, Longint(DC), 0);
   RestoreDC(DC, SaveIndex);
  end;
end;

procedure TWaveform.Paint;
begin
 with Canvas do
  begin
   CopyMode := cmSrcCopy;
   Draw(0, 0, fBuffer);
  end;
end;

procedure TWaveform.RedrawBuffer;
var i,p,o     : Integer;
    r,w,mn,mx : Single;
begin
 fBuffer.Canvas.Brush.Color:=Self.Color;

 if fTransparent
  then DrawParentImage(Self, fBuffer.Canvas)
  else fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

 if Length(fWavedata)<1 then exit;
 if fNormalize then
  begin
   fNormalizeFak:=0;
   for i := 0 to Length(fWavedata) - 1 do
    if abs(fWavedata[i])>fNormalizeFak
     then fNormalizeFak:=abs(fWavedata[i]);
   if fNormalizeFak=0
    then fNormalizeFak:=1
    else fNormalizeFak:=1/fNormalizeFak;
  end else fNormalizeFak:=1;

 with fBuffer.Canvas do
  begin
   Pen.Width:=fLineWidth;
   Pen.Color:=fLineColor;
   r:=Width/Length(fWavedata); i:=1; w:=0; p:=0;
   mn:=Wavedata[0]*fNormalizeFak; mx:=mn;
   MoveTo(0,round(((mn*0.5)+0.5)*fBuffer.Height));
   while i<Length(fWavedata) do
    begin
     if Wavedata[i]*fNormalizeFak>mx then mx:=Wavedata[i]*fNormalizeFak else
     if Wavedata[i]*fNormalizeFak<mn then mn:=Wavedata[i]*fNormalizeFak;
     w:=w+r;
     if w>p then
      begin
       p:=round(w);
       if mn=mx then LineTo(p,round(((mn*0.5)+0.5)*fBuffer.Height)) else
        begin
         o:=fBuffer.Canvas.PenPos.Y-fHalfHeight;
         if abs(o-mn*fHalfHeight)<abs(o-mx*fHalfHeight)
          then
           begin
            LineTo(p,round(mn*fHalfHeight+fHalfHeight));
            LineTo(p,round(mx*fHalfHeight+fHalfHeight));
           end else
           begin
            LineTo(p,round(mx*fHalfHeight+fHalfHeight));
            LineTo(p,round(mn*fHalfHeight+fHalfHeight));
           end;
        end;
       mn:=Wavedata[i]*fNormalizeFak;
       mx:=mn;
      end;
     inc(i);
    end;

   if mn=mx then LineTo(Width,round(mn*fHalfHeight+fHalfHeight)) else
    begin
     o:=fBuffer.Canvas.PenPos.Y-fHalfHeight;
     if abs(o-mn*fHalfHeight)<abs(o-mx*fHalfHeight)
      then
       begin
        LineTo(Width,round(mn*fHalfHeight+fHalfHeight));
        LineTo(Width,round(mx*fHalfHeight+fHalfHeight));
       end else
       begin
        LineTo(Width,round(mx*fHalfHeight+fHalfHeight));
        LineTo(Width,round(mn*fHalfHeight+fHalfHeight));
       end;
    end;
  end;
 Invalidate; 
end;

procedure TWaveform.SetLineColor(const Value: TColor);
begin
 if fLineColor<>Value then
  begin
   fLineColor := Value;
   Invalidate;
  end;
end;

procedure TWaveform.SetLinewidth(const Value: Integer);
begin
 if (Value>0) and (Value<200) and (fLinewidth<>Value) then
  begin
   fLinewidth := Value;
   Invalidate;
  end;
end;

procedure TWaveform.SetNormalize(const Value: Boolean);
begin
 if fNormalize<>Value then
  begin
   fNormalize := Value;
   RedrawBuffer;
  end;
end;

procedure TWaveform.SetTransparent(const Value: Boolean);
begin
 if fTransparent<>Value then
  begin
   fTransparent := Value;
   RedrawBuffer;
  end;
end;

procedure TWaveform.SetWaveLength(const Value: Integer);
begin
 if Length(fWaveData)<>Value then
  begin
   SetLength(fWaveData,Value);
   RedrawBuffer;
  end;
end;

procedure TWaveform.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 MouseCapture := True;
 inherited MouseDown(Button, Shift, X, Y);
 if (x < 0) or (x > width) or (y < 0) or (y > height) or not (ssLeft in Shift) then exit;

// Invalidate;
end;

procedure TWaveform.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited MouseMove(Shift, X, Y);
end;

procedure TWaveform.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := False;
 inherited MouseUp(Button, Shift, X, Y);
// Invalidate;
end;

procedure TWaveform.WMEraseBkgnd(var m: TWMEraseBkgnd); begin m.Result := LRESULT(False); end;

procedure TWaveform.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
 if Assigned(fBuffer) then
  begin
   fBuffer.Width := Width;
   fBuffer.Height := Height;
   fHalfHeight := Height div 2;
   RedrawBuffer;
  end;
end;

procedure Register;
begin
 RegisterComponents('Audio', [TWaveform]);
end;

end.
