unit DAV_GuiADSRGraph;

interface

{$I ..\DAV_Compiler.inc}

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
    FAttack   :  Single;
    FDecay    :  Single;
    FSustain  :  Single;
    FRelease  :  Single;
    FOnChange : TGuiADSROnChange;
    procedure SetAttack(Value: Single);
    procedure SetDecay(Value: Single);
    procedure SetRelease(Value: Single);
    procedure SetSustain(Value: Single);
  protected
    procedure Changed(EditType: TGuiADSRGraphMouseEdit);
    procedure AssignTo(Dest: TPersistent); override;
    property OnChange: TGuiADSROnChange read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Attack : Single read FAttack write SetAttack;
    property Decay : Single read FDecay write SetDecay;
    property Sustain : Single read FSustain write SetSustain;
    property Release : Single read FRelease write SetRelease;
  end;



  TGuiADSRGraph = class(TGuiBaseControl)
  private
    FADSRSettings    : TGuiADSRSettings;
    FMouseEdit       : TGuiADSRGraphMouseEdit;
    FOnAttackChange  : TNotifyEvent;
    FOnSustainChange : TNotifyEvent;
    FOnDecayChange   : TNotifyEvent;
    FOnReleaseChange : TNotifyEvent;
    FGridColor       : TColor;
    FGridWidth       : Integer;
    FGridStyle       : TPenStyle;
    FGridVPadding    : Integer;
    FEnvVPadding     : Integer;
    FEnvHPadding     : Integer;

    procedure CalcIntValues;
    function GetAttack: Single;
    function GetDecay: Single;
    function GetRelease: Single;
    function GetSustain: Single;
    procedure SetAttack(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetSustain(const Value: Single);

    procedure SetGridColor(const Value: TColor);
    procedure SetGridWidth(const Value: Integer);
    procedure SetGridStyle(const Value: TPenStyle);
    procedure SetGridVPadding(const Value: Integer);
    procedure SetEnvVPadding(const Value: Integer);
    procedure SetEnvHPadding(const Value: Integer);
  protected
    FA, FD, FS, FR : Integer;
    FCursorADR     : TCursor;
    FCursorS       : TCursor;
    FCursorDefault : TCursor;
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

    property Attack: Single read GetAttack write SetAttack;
    property Decay: Single read GetDecay write SetDecay;
    property Sustain: Single read GetSustain write SetSustain;
    property Release: Single read GetRelease write SetRelease;
  published    
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property LineWidth;
    property LineColor;
    property Color;
    
    property ADSRSettings: TGuiADSRSettings read FADSRSettings write FADSRSettings;
    property OnAttackChange : TNotifyEvent read FOnAttackChange write FOnAttackChange;
    property OnDecayChange : TNotifyEvent read FOnDecayChange write FOnDecayChange;
    property OnSustainChange : TNotifyEvent read FOnSustainChange write FOnSustainChange;
    property OnReleaseChange : TNotifyEvent read FOnReleaseChange write FOnReleaseChange;

    property GridColor: TColor read FGridColor write SetGridColor default clSilver;
    property GridWidth: Integer read FGridWidth write SetGridWidth default 1;
    property GridStyle: TPenStyle read FGridStyle write SetGridStyle default psSolid;
    property GridVPadding: Integer read FGridVPadding write SetGridVPadding default 0;
    
    property EnvVPadding: Integer read FEnvVPadding write SetEnvVPadding default 0;
    property EnvHPadding: Integer read FEnvHPadding write SetEnvHPadding default 0;
    
    property CursorDefault: TCursor read FCursorDefault write FCursorDefault default crDefault;
    property CursorADR: TCursor read FCursorADR write FCursorADR default crSizeWE;
    property CursorS: TCursor read FCursorS write FCursorS default crSizeNS;
  end;

implementation

uses SysUtils;

constructor TGuiADSRSettings.Create;
begin
  inherited Create;
  Attack  := 0.5;
  FDecay   := 0.5;
  FSustain := 0.5;
  FRelease := 0.5;
end;

destructor TGuiADSRSettings.Destroy;
begin
  inherited;
end;

procedure TGuiADSRSettings.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiADSRSettings then
  with TGuiADSRSettings(Dest) do
   begin
    FAttack   := Self.FAttack;
    FDecay    := Self.FDecay;
    FSustain  := Self.FSustain;
    FRelease  := Self.FRelease;
    FOnChange := Self.FOnChange;
   end
 else inherited;
end;

procedure TGuiADSRSettings.Changed(EditType: TGuiADSRGraphMouseEdit);
begin
  if Assigned(FOnChange) then FOnChange(Self, EditType);
end;

procedure TGuiADSRSettings.SetAttack(Value: Single);
begin
 if Value < 0 then Value := 0 else if Value > 1 then Value := 1;

 if (FAttack <> Value) then
  begin
   FAttack := Value;
   Changed(meAttack);
  end;
end;

procedure TGuiADSRSettings.SetDecay(Value: Single);
begin
  if Value<0 then Value := 0 else if Value>1 then Value := 1;

  if (FDecay<>Value) then
  begin
    FDecay := Value;
    Changed(meDecay);
  end;
end;

procedure TGuiADSRSettings.SetSustain(Value: Single);
begin
  if Value<0 then Value := 0 else if Value>1 then Value := 1;

  if (FSustain<>Value) then
  begin
    FSustain := Value;
    Changed(meSustain);
  end;
end;

procedure TGuiADSRSettings.SetRelease(Value: Single);
begin
  if Value<0 then Value := 0 else if Value>1 then Value := 1;

  if (FRelease<>Value) then
  begin
    FRelease := Value;
    Changed(meRelease);
  end;
end;





constructor TGuiADSRGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FADSRSettings := TGuiADSRSettings.Create;
  FADSRSettings.OnChange := SettingsChanged;

  FGridColor := clSilver;
  FGridWidth := 1;
  FGridStyle := psSolid;
  FGridVPadding := 0;  
  FEnvVPadding := 0;
  FEnvHPadding := 0;
  FCursorADR := crSizeWE;
  FCursorS := crSizeNS;
  FCursorDefault := crDefault;
end;

destructor TGuiADSRGraph.Destroy;
begin
  FADSRSettings.Free;
  inherited;
end;

procedure TGuiADSRGraph.SettingsChanged(Sender: TObject; EditType: TGuiADSRGraphMouseEdit);
begin
  CalcIntValues;
  if (EditType=meAttack)  and Assigned(FOnAttackChange)  then FOnAttackChange(self);
  if (EditType=meDecay)   and Assigned(FOnDecayChange)   then FOnDecayChange(self);
  if (EditType=meSustain) and Assigned(FOnSustainChange) then FOnSustainChange(self);
  if (EditType=meRelease) and Assigned(FOnReleaseChange) then FOnReleaseChange(self);
  RedrawBuffer(true);
end;

function TGuiADSRGraph.GetAttack: Single;  begin Result := ADSRSettings.Attack; end;
function TGuiADSRGraph.GetDecay: Single;   begin Result := ADSRSettings.Decay; end;
function TGuiADSRGraph.GetRelease: Single; begin Result := ADSRSettings.Release; end;
function TGuiADSRGraph.GetSustain: Single; begin Result := ADSRSettings.Sustain; end;

procedure TGuiADSRGraph.SetAttack(const Value: Single);  begin ADSRSettings.Attack := Value; end;
procedure TGuiADSRGraph.SetDecay(const Value: Single);   begin ADSRSettings.Decay := Value; end;
procedure TGuiADSRGraph.SetRelease(const Value: Single); begin ADSRSettings.Release := Value; end;
procedure TGuiADSRGraph.SetSustain(const Value: Single); begin ADSRSettings.Sustain := Value; end;

procedure TGuiADSRGraph.SetGridColor(const Value: TColor);
begin
 if FGridColor <> Value then
  begin
    FGridColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetGridWidth(const Value: Integer);
begin
  if FGridWidth<>Value then
  begin
    FGridWidth := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetGridStyle(const Value: TPenStyle);
begin
  if FGridStyle<>Value then
  begin
    FGridStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetGridVPadding(const Value: Integer);
begin
  if FGridVPadding<>Value then
  begin
    FGridVPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetEnvVPadding(const Value: Integer);
begin
  if FEnvVPadding<>Value then
  begin
    FEnvVPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.SetEnvHPadding(const Value: Integer);
begin
  if FEnvHPadding<>Value then
  begin
    FEnvHPadding := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiADSRGraph.RedrawBuffer(doBufferFlip: Boolean);
begin
  if (Width>0) and (Height>0) then with fBuffer.Canvas do
  begin
    Lock;
    Brush.Color := Self.Color;

    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);
    if FGridStyle<>psClear then
    begin
      Pen.Width := FGridWidth;
      Pen.Style := FGridStyle;
      Pen.Color := FGridColor;

      MoveTo(FA, FGridVPadding); LineTo(FA, Height - FGridVPadding);
      MoveTo(FD, FGridVPadding); LineTo(FD, Height - FGridVPadding);
      MoveTo(FR, FGridVPadding); LineTo(FR, Height - FGridVPadding);
    end;

    Pen.Color := FLineColor;
    Pen.Style := psSolid;
    Pen.Width := FLinewidth;

    MoveTo(FEnvHPadding, Height - FEnvVPadding - 1);
    LineTo(FA, FEnvVPadding);
    LineTo(FD, FS);
    LineTo(FR, FS);
    LineTo(Width - FEnvHPadding - 1, Height - FEnvVPadding - 1);

    UnLock;
  end;
  
  if doBufferFlip then Invalidate;
end;

procedure TGuiADSRGraph.CalcIntValues;
var
  nwidth  : Integer;
  nheight : Integer;
begin
  nwidth := Width - 2 * FEnvHPadding - 1;
  nheight := Height -2 * FEnvVPadding - 1;

  with FADSRSettings do
   begin
    FA := Round(0.25 * nwidth * Attack);
    FD := FA+Round(0.25 * nwidth * Decay);
    FS := Round(nheight * (1 - Sustain));
    FR := nwidth - round(0.25 * nwidth * Release);
   end;

  FA := FA + FEnvHPadding;
  FD := FD + FEnvHPadding;
  FS := FS + FEnvVPadding;
  FR := FR + FEnvHPadding;
end;

procedure TGuiADSRGraph.ResizeBuffer;
begin         
  CalcIntValues;
  inherited;
end;

function TGuiADSRGraph.CheckForMouseFunc(x,y: Integer): TGuiADSRGraphMouseEdit;
begin
  Result := meNone;
  if (x < FEnvHPadding) or (x > width-FEnvHPadding) or (y < FEnvVPadding) or (y > height-FEnvVPadding) then exit;

  if (x > FA - 5) and (x < FA + FLinewidth) then Result := meAttack
  else if (x > FD - FLinewidth) and (x < FD + 5) then Result := meDecay
  else if (x > FR - FLinewidth) and (x < FR + 5) then Result := meRelease
  else if (y > FS - 5) and (y < FS + 5) and (x >= FD + 5) and (x <= FR - 5) then Result := meSustain
  else Result := meNone;
end;

procedure TGuiADSRGraph.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    if not (ssLeft in Shift) then exit;
    FMouseEdit := CheckForMouseFunc(x,y);
  end;

  inherited;
end;


procedure TGuiADSRGraph.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
var
  nwidth,
  nheight : Integer;
begin
  nwidth := Width - 2 * FEnvHPadding - 1;
  nheight := Height - 2 * FEnvVPadding - 1;
  x := x - FEnvHPadding;
  y := y - FEnvVPadding;
  if not (ssLeft in Shift) then FMouseEdit := meNone else
  case FMouseEdit of
    meAttack   : FADSRSettings.Attack := 4 * x / nwidth;
    meDecay    : FADSRSettings.Decay := 4 * (x - FA + FEnvHPadding) / nwidth;
    meSustain  : FADSRSettings.Sustain := 1 - y / nheight;
    meRelease  : FADSRSettings.Release := 4 * (nwidth - x) / nwidth;
  end;

  inherited;
end;

procedure TGuiADSRGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then FMouseEdit := meNone;

  inherited;
end;

procedure TGuiADSRGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    case CheckForMouseFunc(x,y) of
      meNone:    cursor := FCursorDefault;
      meSustain: cursor := FCursorS;
      else cursor := FCursorADR;
    end; 
  end;
  
  inherited;
end;

procedure TGuiADSRGraph.MouseLeave;
begin
  inherited;
end;

end.
