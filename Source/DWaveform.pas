unit DWaveform;

{-$R DWaveform.res}
{$R-}

interface

uses Windows, DDSPBase, Classes, Graphics, Forms, Controls, ExtCtrls, Messages;

type
  TWaveform = class;

  TWaveform = class(TGraphicControl)
  private
    fBuffer         : TBitmap;
    fLineWidth      : Integer;
    fOnKeyDown      : TKeyEvent;
    fOnKeyPress     : TKeyPressEvent;
    fOnKeyUp        : TKeyEvent;
    fLineColor      : TColor;
    fTransparent    : Boolean;
    fWavedata       : TSingleArray;
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetLinewidth(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure RedrawBuffer;
    procedure SetWaveLength(const Value: Integer);
    function GetWaveLength: Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Wavedata : TSingleArray read fWavedata write fWavedata;
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
    property LineWidth: Integer read fLineWidth write SetLineWidth;
    property LineColor: TColor read fLineColor write SetLineColor;
    property Transparent: Boolean read fTransparent write SetTransparent;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation

uses SysUtils;

{ TWaveform }

constructor TWaveform.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 fLineWidth := 1;
 fLineColor := clBlack;
 fTransparent := False;
 fBuffer := TBitmap.Create;
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
 fBuffer.Canvas.Brush.Color:=Self.Color;

 if fTransparent
  then DrawParentImage(Self, fBuffer.Canvas)
  else fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

 with fBuffer.Canvas do
  begin
   Pen.Width:=1;
   Pen.Style:=psDash;
   Pen.Color:=clSilver;

  end;
 with Canvas do
  begin
   CopyMode := cmSrcCopy;
   Draw(0, 0, fBuffer);
  end;
end;

procedure TWaveform.RedrawBuffer;
var i,p   : Integer;
    r,w   : Single;
    mn,mx : Single;
begin
 if Length(fWavedata)<1 then exit;
 with fBuffer.Canvas do
  begin
   r:=Width/Length(fWavedata); i:=1; w:=0; p:=0;
   mn:=Wavedata[0]; mx:=mn;
   MoveTo(0,round(mn*Height*0.5));
   while i<Length(fWavedata) do
    begin
     if Wavedata[i]>mx then mx:=Wavedata[i] else
     if Wavedata[i]<mn then mn:=Wavedata[i];
     w:=w+r;
     if w>p then
      begin
       p:=round(w);
       LineTo(0,round(mn*Height*0.5));
       if mn<>mx then LineTo(0,round(mx*Height*0.5));
       mn:=Wavedata[i];
       mx:=mn;
      end;
     inc(i); 
    end;
  end;
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

procedure TWaveform.SetTransparent(const Value: Boolean);
begin
 if fTransparent<>Value then
  begin
   fTransparent := Value;
   Invalidate;
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

 Invalidate;
end;

procedure TWaveform.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited MouseMove(Shift, X, Y);
end;

procedure TWaveform.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := False;
 inherited MouseUp(Button, Shift, X, Y);
 Invalidate;
end;

procedure TWaveform.WMEraseBkgnd(var m: TWMEraseBkgnd); begin m.Result := LRESULT(False); end;

procedure TWaveform.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
 if Assigned(fBuffer) then
  begin
   fBuffer.Width := Width;
   fBuffer.Height := Height;
   RedrawBuffer;
  end;
end;

procedure Register;
begin
 RegisterComponents('Audio', [TWaveform]);
end;

end.
