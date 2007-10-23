unit DGuiADSRGraph;

{$I ASIOVST.INC}
{$IFNDEF FPC} {$R DGuiADSRGraph.res} {$ENDIF}
{$R-}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Controls, ExtCtrls, Messages;

type
  TADSRGraphMouseEdit = (meNone, meAttack, meDecay, meSustain, meRelease);
  TADSRGraph = class;
  TADSRSettings = class(TPersistent)
  private
    fAttack,
    fDecay,
    fSustain,
    fRelease         : Single;
    fOwner           : TADSRGraph;
  public
    constructor Create(AOwner: TADSRGraph);
    destructor Destroy; override;
    procedure SetAttack(Value: Single);
    procedure SetDecay(Value: Single);
    procedure SetRelease(Value: Single);
    procedure SetSustain(Value: Single);
    function GetOwner: TPersistent; override;
  published
    property Attack : Single read fAttack write SetAttack;
    property Decay : Single read fDecay write SetDecay;
    property Sustain : Single read fSustain write SetSustain;
    property Release : Single read fRelease write SetRelease;
  end;

  { TADSRGraph }

  TADSRGraph = class(TGraphicControl)
  private
    fBuffer          : TBitmap;
    fLineWidth       : Integer;
    fOnKeyDown       : TKeyEvent;
    fOnKeyPress      : TKeyPressEvent;
    fOnKeyUp         : TKeyEvent;
    fADSRSettings    : TADSRSettings;
    fLineColor       : TColor;
    fTransparent     : Boolean;
    fMouseEdit       : TADSRGraphMouseEdit;
    fOnAttackChange,
    fOnSustainChange,
    fOnDecayChange,
    fOnReleaseChange : TNotifyEvent;
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetLinewidth(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure CalcIntValues;
    function GetAttack: Single;
    function GetDecay: Single;
    function GetRelease: Single;
    function GetSustain: Single;
    procedure SetAttack(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetSustain(const Value: Single);
  protected
    fA,fD,fS,fR     : Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Attack : Single read GetAttack write SetAttack;
    property Decay : Single read GetDecay write SetDecay;
    property Sustain : Single read GetSustain write SetSustain;
    property Release : Single read GetRelease write SetRelease;
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
    property ADSRSettings: TADSRSettings read fADSRSettings write fADSRSettings;
    property LineWidth: Integer read fLineWidth write SetLineWidth;
    property LineColor: TColor read fLineColor write SetLineColor;
    property Transparent: Boolean read fTransparent write SetTransparent;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnAttackChange : TNotifyEvent read fOnAttackChange write fOnAttackChange;
    property OnDecayChange : TNotifyEvent read fOnDecayChange write fOnDecayChange;
    property OnSustainChange : TNotifyEvent read fOnSustainChange write fOnSustainChange;
    property OnReleaseChange : TNotifyEvent read fOnReleaseChange write fOnReleaseChange;
  end;

procedure Register;

implementation

uses SysUtils;

{ TADSRSettings }

constructor TADSRSettings.Create(AOwner: TADSRGraph);
begin
 inherited Create;
 fAttack:=0.5;
 fDecay:=0.5;
 fSustain:=0.5;
 fRelease:=0.5;
 fOwner:=AOwner;
end;

destructor TADSRSettings.Destroy;
begin
  inherited;
end;

function TADSRSettings.GetOwner: TPersistent;
begin
 Result:=fOwner;
end;

procedure TADSRSettings.SetAttack(Value: Single);
begin
 if Value<0 then Value:=0 else if Value>1 then Value:=1;
 if (fAttack<>Value) then
  begin
   fAttack := Value;
   fOwner.fA:=Round(0.25*fOwner.Width*fAttack);
   fOwner.fD:=fOwner.fA+Round(0.25*fOwner.Width*fDecay);
   if Assigned(fOwner.fOnAttackChange)
    then fOwner.fOnAttackChange(fOwner);
   fOwner.Invalidate;
  end;
end;

procedure TADSRSettings.SetDecay(Value: Single);
begin
 if Value<0 then Value:=0 else if Value>1 then Value:=1;
 if (fDecay<>Value) then
  begin
   fDecay := Value;
   fOwner.fD:=fOwner.fA+Round(0.25*fOwner.Width*fDecay);
   if Assigned(fOwner.fOnDecayChange)
    then fOwner.fOnDecayChange(fOwner);
   fOwner.Invalidate;
  end;
end;

procedure TADSRSettings.SetRelease(Value: Single);
begin
 if Value<0 then Value:=0 else if Value>1 then Value:=1;
 if (fRelease<>Value) then
  begin
   fRelease := Value;
   fOwner.fR:=fOwner.Width-round(0.25*fOwner.Width*fRelease);
   if Assigned(fOwner.fOnReleaseChange)
    then fOwner.fOnReleaseChange(fOwner);
   fOwner.Invalidate;
  end;
end;

procedure TADSRSettings.SetSustain(Value: Single);
begin
 if Value<0 then Value:=0 else if Value>1 then Value:=1;
 if (fSustain<>Value) then
  begin
   fSustain := Value;
   fOwner.fS:=Round(fOwner.Height*(1-fSustain));
   if Assigned(fOwner.fOnSustainChange)
    then fOwner.fOnSustainChange(fOwner);
   fOwner.Invalidate;
  end;
end;

{ TADSRGraph }

constructor TADSRGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 fLineWidth := 1;
 fLineColor := clBlack;
 fTransparent := False;
 fBuffer := TBitmap.Create;
 fADSRSettings := TADSRSettings.Create(Self);
 ControlStyle := ControlStyle+[csOpaque];
end;

destructor TADSRGraph.Destroy;
begin
 fBuffer.Free;
 fADSRSettings.Free;
 inherited;
end;

function TADSRGraph.GetAttack: Single;
begin
 Result:=ADSRSettings.Attack;
end;

function TADSRGraph.GetDecay: Single;
begin
 Result:=ADSRSettings.Decay;
end;

function TADSRGraph.GetRelease: Single;
begin
 Result:=ADSRSettings.Release;
end;

function TADSRGraph.GetSustain: Single;
begin
 Result:=ADSRSettings.Sustain;
end;

{$IFNDEF FPC}
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
{$ENDIF}

procedure TADSRGraph.Paint;
begin
 fBuffer.Canvas.Brush.Color:=Self.Color;

 {$IFNDEF FPC}
 if fTransparent
  then DrawParentImage(Self, fBuffer.Canvas)
  else
 {$ENDIF}
 fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

 with fBuffer.Canvas do
  begin
   Pen.Width:=1;
   Pen.Style:=psDash;
   Pen.Color:=clSilver;
   MoveTo(fA,0); LineTo(fA,Height);
   MoveTo(fD,0); LineTo(fD,Height);
   MoveTo(fR,0); LineTo(fR,Height);

   Pen.Color:=fLineColor;
   Pen.Style:=psSolid;
   Pen.Width:=fLinewidth;

   MoveTo(0,Height);
   LineTo(fA,0);
   LineTo(fD,fS);
   LineTo(fR,fS);
   Pen.Style:=psSolid;
   LineTo(Width,Height);
  end;
 with Canvas do
  begin
   CopyMode := cmSrcCopy;
   Draw(0, 0, fBuffer);
  end;
end;

procedure TADSRGraph.CalcIntValues;
begin
 fA:=Round(0.25*Width*fADSRSettings.Attack);
 fD:=fA+Round(0.25*Width*fADSRSettings.Decay);
 fS:=Round(Height*(1-fADSRSettings.Sustain));
 fR:=Width-round(0.25*Width*fADSRSettings.Release);
end;

procedure TADSRGraph.SetAttack(const Value: Single);
begin
 ADSRSettings.Attack:=Value;
end;

procedure TADSRGraph.SetDecay(const Value: Single);
begin
 ADSRSettings.Decay:=Value;
end;

procedure TADSRGraph.SetLineColor(const Value: TColor);
begin
 if fLineColor<>Value then
  begin
   fLineColor := Value;
   Invalidate;
  end;
end;

procedure TADSRGraph.SetLinewidth(const Value: Integer);
begin
 if (Value>0) and (Value<200) and (fLinewidth<>Value) then
  begin
   fLinewidth := Value;
   Invalidate;
  end;
end;

procedure TADSRGraph.SetRelease(const Value: Single);
begin
 ADSRSettings.Release:=Value;
end;

procedure TADSRGraph.SetSustain(const Value: Single);
begin
 ADSRSettings.Sustain:=Value;
end;

procedure TADSRGraph.SetTransparent(const Value: Boolean);
begin
 if fTransparent<>Value then
  begin
   fTransparent := Value;
   Invalidate;
  end;
end;

procedure TADSRGraph.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 MouseCapture := True;
 inherited MouseDown(Button, Shift, X, Y);
 if (x < 0) or (x > width) or (y < 0) or (y > height) or not (ssLeft in Shift) then exit;
 if (x>fA-5) and (x<fA+5) then fMouseEdit:=meAttack else
 if (x>fD-5) and (x<fD+5) then fMouseEdit:=meDecay else
 if (x>fR-5) and (x<fR+5) then fMouseEdit:=meRelease else
 if (y>fS-5) and (y<fS+5) and (x>=fD) and (x<=fR) then fMouseEdit:=meSustain
  else fMouseEdit:=meNone;

 Invalidate;
end;

procedure TADSRGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited MouseMove(Shift, X, Y);
 if not (ssLeft in Shift) then fMouseEdit:=meNone else
  case fMouseEdit of
   meAttack   : fADSRSettings.Attack:=x/(0.25*Width);
   meDecay    : fADSRSettings.Decay:=(x-fA)/(0.25*Width);
   meSustain  : fADSRSettings.Sustain:=1-y/Height;
   meRelease  : fADSRSettings.Release:=(Width-x)/(0.25*Width);
  end;
end;

procedure TADSRGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := False;
 inherited MouseUp(Button, Shift, X, Y);
 fMouseEdit:=meNone;
 Invalidate;
end;

procedure TADSRGraph.Resize;
begin
 inherited Resize;
 fBuffer.Width := Width;
 fBuffer.Height := Height;
 CalcIntValues;
end;

procedure TADSRGraph.ReadState(Reader: TReader);
begin
 inherited ReadState(Reader);
 fBuffer.Width := Width;
 fBuffer.Height := Height;
 CalcIntValues;
end;

procedure TADSRGraph.WMEraseBkgnd(var m: TWMEraseBkgnd); begin m.Result := 0; end;

procedure Register;
begin
 RegisterComponents('Audio', [TADSRGraph]);
end;

initialization
 {$IFDEF FPC}
 {$i TADSRGraph.lrs}
 {$ENDIF}

end.
