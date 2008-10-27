unit DAV_GuiADSRGraph;

{$I ..\ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Controls, ExtCtrls, DAV_GuiBaseControl;

type
  TGuiADSRGraphMouseEdit = (meNone, meAttack, meDecay, meSustain, meRelease);
  TGuiADSRGraph = class;
  TGuiADSROnChange = procedure (Sender: TObject; EditType: TGuiADSRGraphMouseEdit) of object;

  TGuiADSRSettings = class(TPersistent)
  private
    fAttack,
    fDecay,
    fSustain,
    fRelease:  Single;
    FOnChange: TGuiADSROnChange;
    property OnChange: TGuiADSROnChange read FOnChange write FOnChange;
  protected
    procedure Changed(EditType: TGuiADSRGraphMouseEdit);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetAttack(Value: Single);
    procedure SetDecay(Value: Single);
    procedure SetRelease(Value: Single);
    procedure SetSustain(Value: Single);
  published
    property Attack : Single read fAttack write SetAttack;
    property Decay : Single read fDecay write SetDecay;
    property Sustain : Single read fSustain write SetSustain;
    property Release : Single read fRelease write SetRelease;
  end;



  TGuiADSRGraph = class(TGuiBaseControl)
  private
    fADSRSettings    : TGuiADSRSettings;
    fMouseEdit       : TGuiADSRGraphMouseEdit;
    fOnAttackChange,
    fOnSustainChange,
    fOnDecayChange,
    fOnReleaseChange : TNotifyEvent;
    fGridColor: TColor;
    fGridWidth: Integer;
    fGridStyle: TPenStyle;
    fGridVPadding: Integer;
    fEnvVPadding: Integer;
    fEnvHPadding: Integer;

    procedure CalcIntValues;
    function GetAttack: Single;
    function GetDecay: Single;
    function GetRelease: Single;
    function GetSustain: Single;
    procedure SetAttack(Value: Single);
    procedure SetDecay(Value: Single);
    procedure SetRelease(Value: Single);
    procedure SetSustain(Value: Single);

    procedure SetGridColor(Value: TColor);
    procedure SetGridWidth(Value: Integer);
    procedure SetGridStyle(Value: TPenStyle);
    procedure SetGridVPadding(Value: Integer);
    procedure SetEnvVPadding(Value: Integer);
    procedure SetEnvHPadding(Value: Integer);
  protected
    fA,fD,fS,fR: Integer;
    fCursorADR: TCursor;
    fCursorS: TCursor;
    fCursorDefault: TCursor;
    procedure ResizeBuffer; override;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;

    procedure SettingsChanged(Sender: TObject; EditType: TGuiADSRGraphMouseEdit); dynamic;
    function CheckForMouseFunc(x,y: Integer): TGuiADSRGraphMouseEdit; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Attack : Single read GetAttack write SetAttack;
    property Decay : Single read GetDecay write SetDecay;
    property Sustain : Single read GetSustain write SetSustain;
    property Release : Single read GetRelease write SetRelease;
  published    
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property LineWidth;
    property LineColor;
    property Color;
    
    property ADSRSettings: TGuiADSRSettings read fADSRSettings write fADSRSettings;
    property OnAttackChange : TNotifyEvent read fOnAttackChange write fOnAttackChange;
    property OnDecayChange : TNotifyEvent read fOnDecayChange write fOnDecayChange;
    property OnSustainChange : TNotifyEvent read fOnSustainChange write fOnSustainChange;
    property OnReleaseChange : TNotifyEvent read fOnReleaseChange write fOnReleaseChange;

    property GridColor: TColor read fGridColor write SetGridColor default clSilver;
    property GridWidth: Integer read fGridWidth write SetGridWidth default 1;
    property GridStyle: TPenStyle read fGridStyle write SetGridStyle default psSolid;
    property GridVPadding: Integer read fGridVPadding write SetGridVPadding default 0;
    
    property EnvVPadding: Integer read fEnvVPadding write SetEnvVPadding default 0;
    property EnvHPadding: Integer read fEnvHPadding write SetEnvHPadding default 0;
    
    property CursorDefault: TCursor read fCursorDefault write fCursorDefault default crDefault;
    property CursorADR: TCursor read fCursorADR write fCursorADR default crSizeWE;
    property CursorS: TCursor read fCursorS write fCursorS default crSizeNS;
  end;

implementation

uses SysUtils;

constructor TGuiADSRSettings.Create;
begin
  inherited Create;
  Attack  := 0.5;
  fDecay   := 0.5;
  fSustain := 0.5;
  fRelease := 0.5;
end;

destructor TGuiADSRSettings.Destroy;
begin
  inherited;
end;

procedure TGuiADSRSettings.Changed(EditType: TGuiADSRGraphMouseEdit);
begin
  if Assigned(FOnChange) then FOnChange(Self, EditType);
end;

procedure TGuiADSRSettings.SetAttack(Value: Single);
begin
  if Value<0 then Value:=0 else if Value>1 then Value:=1;

  if (fAttack<>Value) then
  begin
    fAttack := Value;
    Changed(meAttack);
  end;
end;

procedure TGuiADSRSettings.SetDecay(Value: Single);
begin
  if Value<0 then Value:=0 else if Value>1 then Value:=1;

  if (fDecay<>Value) then
  begin
    fDecay := Value;
    Changed(meDecay);
  end;
end;

procedure TGuiADSRSettings.SetSustain(Value: Single);
begin
  if Value<0 then Value:=0 else if Value>1 then Value:=1;

  if (fSustain<>Value) then
  begin
    fSustain := Value;
    Changed(meSustain);
  end;
end;

procedure TGuiADSRSettings.SetRelease(Value: Single);
begin
  if Value<0 then Value:=0 else if Value>1 then Value:=1;

  if (fRelease<>Value) then
  begin
    fRelease := Value;
    Changed(meRelease);
  end;
end;





constructor TGuiADSRGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fADSRSettings := TGuiADSRSettings.Create;
  fADSRSettings.OnChange := SettingsChanged;

  fGridColor:=clSilver;
  fGridWidth:=1;
  fGridStyle:=psSolid;
  fGridVPadding:=0;  
  fEnvVPadding:=0;
  fEnvHPadding:=0;
  fCursorADR:=crSizeWE;
  fCursorS:=crSizeNS;
  fCursorDefault:=crDefault;
end;

destructor TGuiADSRGraph.Destroy;
begin
  fADSRSettings.Free;
  inherited;
end;

procedure TGuiADSRGraph.SettingsChanged(Sender: TObject; EditType: TGuiADSRGraphMouseEdit);
begin
  CalcIntValues;
  if (EditType=meAttack)  and Assigned(fOnAttackChange)  then fOnAttackChange(self);
  if (EditType=meDecay)   and Assigned(fOnDecayChange)   then fOnDecayChange(self);
  if (EditType=meSustain) and Assigned(fOnSustainChange) then fOnSustainChange(self);
  if (EditType=meRelease) and Assigned(fOnReleaseChange) then fOnReleaseChange(self);
  RedrawBuffer(true);
end;

function TGuiADSRGraph.GetAttack: Single;  begin Result:=ADSRSettings.Attack; end;
function TGuiADSRGraph.GetDecay: Single;   begin Result:=ADSRSettings.Decay; end;
function TGuiADSRGraph.GetRelease: Single; begin Result:=ADSRSettings.Release; end;
function TGuiADSRGraph.GetSustain: Single; begin Result:=ADSRSettings.Sustain; end;

procedure TGuiADSRGraph.SetAttack(Value: Single);  begin ADSRSettings.Attack:=Value; end;
procedure TGuiADSRGraph.SetDecay(Value: Single);   begin ADSRSettings.Decay:=Value; end;
procedure TGuiADSRGraph.SetRelease(Value: Single); begin ADSRSettings.Release:=Value; end;
procedure TGuiADSRGraph.SetSustain(Value: Single); begin ADSRSettings.Sustain:=Value; end;

procedure TGuiADSRGraph.SetGridColor(Value: TColor);
begin
  if fGridColor<>Value then
  begin
    fGridColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetGridWidth(Value: Integer);
begin
  if fGridWidth<>Value then
  begin
    fGridWidth := Value;
    RedrawBuffer(true);
  end;  
end;

procedure TGuiADSRGraph.SetGridStyle(Value: TPenStyle);
begin
  if fGridStyle<>Value then
  begin
    fGridStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetGridVPadding(Value: Integer);
begin
  if fGridVPadding<>Value then
  begin
    fGridVPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetEnvVPadding(Value: Integer);
begin
  if fEnvVPadding<>Value then
  begin
    fEnvVPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetEnvHPadding(Value: Integer);
begin
  if fEnvHPadding<>Value then
  begin
    fEnvHPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.RedrawBuffer(doBufferFlip: Boolean);
begin
  if (Width>0) and (Height>0) then with fBuffer.Canvas do
  begin
    Lock;
    Brush.Color:=Self.Color;

    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);
    if fGridStyle<>psClear then
    begin
      Pen.Width:=fGridWidth;
      Pen.Style:=fGridStyle;
      Pen.Color:=fGridColor;

      MoveTo(fA,fGridVPadding); LineTo(fA,Height-fGridVPadding);
      MoveTo(fD,fGridVPadding); LineTo(fD,Height-fGridVPadding);
      MoveTo(fR,fGridVPadding); LineTo(fR,Height-fGridVPadding);
    end;

    Pen.Color:=fLineColor;
    Pen.Style:=psSolid;
    Pen.Width:=fLinewidth;

    MoveTo(fEnvHPadding,Height-fEnvVPadding-1);
    LineTo(fA,fEnvVPadding);
    LineTo(fD,fS);
    LineTo(fR,fS);
    LineTo(Width-fEnvHPadding-1,Height-fEnvVPadding-1);

    UnLock;
  end;
  
  if doBufferFlip then Invalidate;
end;

procedure TGuiADSRGraph.CalcIntValues;
var nwidth, nheight: integer;
begin
  nwidth:=Width-2*fEnvHPadding-1;
  nheight:=Height-2*fEnvVPadding-1;

  fA:=Round(0.25*nwidth*fADSRSettings.Attack);
  fD:=fA+Round(0.25*nwidth*fADSRSettings.Decay);
  fS:=Round(nheight*(1-fADSRSettings.Sustain));
  fR:=nwidth-round(0.25*nwidth*fADSRSettings.Release);

  fA:=fA+fEnvHPadding;
  fD:=fD+fEnvHPadding;
  fS:=fS+fEnvVPadding;
  fR:=fR+fEnvHPadding;
end;

procedure TGuiADSRGraph.ResizeBuffer;
begin         
  CalcIntValues;
  inherited;
end;

function TGuiADSRGraph.CheckForMouseFunc(x,y: Integer): TGuiADSRGraphMouseEdit;
begin
  Result:=meNone;
  if (x < fEnvHPadding) or (x > width-fEnvHPadding) or (y < fEnvVPadding) or (y > height-fEnvVPadding) then exit;

  if (x>fA-5) and (x<fA+fLineWidth) then Result:=meAttack
  else if (x>fD-fLineWidth) and (x<fD+5) then Result:=meDecay
  else if (x>fR-fLineWidth) and (x<fR+5) then Result:=meRelease
  else if (y>fS-5) and (y<fS+5) and (x>=fD+5) and (x<=fR-5) then Result:=meSustain
  else Result:=meNone;
end;

procedure TGuiADSRGraph.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    if not (ssLeft in Shift) then exit;
    fMouseEdit:=CheckForMouseFunc(x,y);
  end;

  inherited;
end;


procedure TGuiADSRGraph.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
var nwidth, nheight: integer;
begin
  nwidth:=Width-2*fEnvHPadding-1;
  nheight:=Height-2*fEnvVPadding-1;
  x:=x-fEnvHPadding;
  y:=y-fEnvVPadding;
  if not (ssLeft in Shift) then fMouseEdit:=meNone else
  case fMouseEdit of
    meAttack   : fADSRSettings.Attack:=x/(0.25*nwidth);
    meDecay    : fADSRSettings.Decay:=(x-fA+fEnvHPadding)/(0.25*nwidth);
    meSustain  : fADSRSettings.Sustain:=1-y/nheight;
    meRelease  : fADSRSettings.Release:=(nwidth-x)/(0.25*nwidth);
  end;

  inherited;
end;

procedure TGuiADSRGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then fMouseEdit:=meNone;

  inherited;
end;

procedure TGuiADSRGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    case CheckForMouseFunc(x,y) of
      meNone:    cursor:=fCursorDefault;
      meSustain: cursor:=fCursorS;
      else cursor:=fCursorADR;
    end; 
  end;
  
  inherited;
end;

procedure TGuiADSRGraph.MouseLeave;
begin
  inherited;
end;

end.
