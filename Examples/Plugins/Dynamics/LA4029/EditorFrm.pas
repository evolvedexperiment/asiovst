unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, Controls,
  Graphics, StdCtrls, ExtCtrls, DGuiPanel, DGuiLabel, DGuiBaseControl,
  DGuiDial, DGuiLED, DGuiVUMeter, DGuiButton, Menus;

type
  TLevelState = (lsIn, lsGR, lsOut);
  TFmLA4029 = class(TForm)
    BtGR: TGuiButton;
    BtIn: TGuiButton;
    BtOut: TGuiButton;
    DialAttack: TGuiDial;
    DialInput: TGuiDial;
    DialKnee: TGuiDial;
    DialMix: TGuiDial;
    DialOutput: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    LbAttack: TGuiLabel;
    LbFast: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TLabel;
    LbLevelingAmplifier: TLabel;
    LbMix: TGuiLabel;
    LbMixValue: TLabel;
    LbOnOff: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbRatioValue: TLabel;
    LbRatioX: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSlow: TGuiLabel;
    LbTitle: TGuiLabel;
    LbVUMeterDisplay: TLabel;
    LEDOnOff: TGuiLED;
    MIFast: TMenuItem;
    MIMedium: TMenuItem;
    MISlow: TMenuItem;
    PnA: TGuiPanel;
    PnB: TGuiPanel;
    PnInputValue: TGuiPanel;
    PnKnee: TGuiPanel;
    PnMix: TGuiPanel;
    PnOutputValue: TGuiPanel;
    PnRatio: TGuiPanel;
    PopupVUMeterSpeed: TPopupMenu;
    SpDivide1: TShape;
    SpDivide2: TShape;
    Timer1: TTimer;
    VUMeter: TGuiVUMeter;
    procedure BtGRClick(Sender: TObject);
    procedure BtInClick(Sender: TObject);
    procedure BtOutClick(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LEDOnOffClick(Sender: TObject);
    procedure MIFastClick(Sender: TObject);
    procedure MIMediumClick(Sender: TObject);
    procedure MISlowClick(Sender: TObject);
    procedure PopupVUMeterSpeedPopup(Sender: TObject);
    procedure VUMeterTimerTimer(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fBackgrounBitmap : TBitmap;
    procedure SetLevelState(const Value: TLevelState);
    function GetLevelState: TLevelState;
    function VUMeterValueToPos(Value: Double): Integer;
  public
    procedure UpdateOnOff;
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMix;
    procedure UpdateLevelState;
  published
    property LevelState: TLevelState read GetLevelState write SetLevelState;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, LA4029DM;

procedure TFmLA4029.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  ClrBt  : Byte;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB32Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 fBackgrounBitmap := TBitmap.Create;
 ClrBt := $F + random($40);
 with fBackgrounBitmap do
  begin
   PixelFormat := pf32bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.9 * s[0] + 0.1 * (2 * random - 1);
       b := round($F * s[1]);
       s[0] := s[1];
       Line[x].B := ClrBt + b;
       Line[x].G := ClrBt + b;
       Line[x].R := ClrBt + b;
       Line[x].A := 0;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'PanKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialInput.DialBitmap.Assign(PngBmp);
   DialOutput.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'RatioKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialRatio.DialBitmap.Assign(PngBmp);
   DialKnee.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'AttackKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialAttack.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'ReleaseKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialRelease.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'VUMeter', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   VUMeter.VUMeterBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLA4029.FormDestroy(Sender: TObject);
begin
 FreeAndNil(fBackgrounBitmap);
end;

procedure TFmLA4029.FormPaint(Sender: TObject);
begin
 if assigned(fBackgrounBitmap)
  then Self.Canvas.Draw(0, 0, fBackgrounBitmap);
end;

procedure TFmLA4029.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdateInput;
 UpdateOutput;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
 UpdateMix;
 UpdateLevelState;
end;

function TFmLA4029.GetLevelState: TLevelState;
begin
 with TLA4029DataModule(Owner)
  do result := TLevelState(round(Parameter[8]));
end;

procedure TFmLA4029.LEDOnOffClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[0] := 1 - Parameter[0];
   UpdateOnOff;
  end;
end;

procedure TFmLA4029.MIFastClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 5;
end;

procedure TFmLA4029.MIMediumClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 50;
end;

procedure TFmLA4029.MISlowClick(Sender: TObject);
begin
 with TLA4029DataModule(Owner)
  do Parameter[9] := 500;
end;

procedure TFmLA4029.PopupVUMeterSpeedPopup(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   MIFast.Checked   := Parameter[9] < 20;
   MISlow.Checked   := Parameter[9] > 200;
   MIMedium.Checked := not (MIFast.Checked or MISlow.Checked);
  end;
end;

procedure TFmLA4029.SetLevelState(const Value: TLevelState);
begin
 with TLA4029DataModule(Owner) do
  if LevelState <> Value then
   begin
    Parameter[8] := Integer(Value);
    UpdateLevelState;
   end;
end;

procedure TFmLA4029.UpdateOnOff;
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[0] < 0.5
    then LEDOnOff.Brightness_Percent := 100
    else LEDOnOff.Brightness_Percent := 10;
  end;
end;

procedure TFmLA4029.UpdateInput;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialInput.Position <> Parameter[1]
    then DialInput.Position := Parameter[1];
   LbInputValue.Caption := FloatToStrF(DialInput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA4029.UpdateKnee;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialKnee.Position <> Parameter[6]
    then DialKnee.Position := Parameter[6];
   LbKneeValue.Caption := FloatToStrF(DialKnee.Position, ffFixed, 3, 1);
  end;
end;

procedure TFmLA4029.UpdateMix;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialMix.Position <> Parameter[7]
    then DialMix.Position := Parameter[7];
   LbMixValue.Caption := FloatToStrF(DialMix.Position, ffFixed, 3, 1) + '%';
  end;
end;

procedure TFmLA4029.UpdateLevelState;
begin
 case LevelState of
   lsIn : begin
           BtIn.ButtonColor  := $00202020;
           BtIn.Font.Color   := $00E2E2E2;
           BtIn.LineColor    := clSilver;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.LineColor    := $00333333;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.LineColor   := $00333333;
           LbVUMeterDisplay.Caption := 'Input';
          end;
   lsGR : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.LineColor    := $00333333;
           BtGR.ButtonColor  := $00202020;
           BtGR.Font.Color   := $00E2E2E2;
           BtGR.LineColor    := clSilver;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.LineColor   := $00333333;
           LbVUMeterDisplay.Caption := 'Gain Reduction';
          end;
  lsOut : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.LineColor    := $00333333;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.LineColor    := $00333333;
           BtOut.ButtonColor := $00202020;
           BtOut.Font.Color  := $00E2E2E2;
           BtOut.LineColor   := clSilver;
           LbVUMeterDisplay.Caption := 'Output';
          end;
 end;
end;

procedure TFmLA4029.UpdateOutput;
begin
 with TLA4029DataModule(Owner) do
  begin
   if DialOutput.Position <> Parameter[2]
    then DialOutput.Position := Parameter[2];
   LbOutputValue.Caption := FloatToStrF(DialOutput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA4029.UpdateAttack;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[3]);
   if DialAttack.Position <> s
    then DialAttack.Position := s;
//   LbAttackValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TFmLA4029.UpdateRelease;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[4]);
   if DialRelease.Position <> s
    then DialRelease.Position := s;
//   LbReleaseValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5) + ' ms';
  end;
end;

function TFmLA4029.VUMeterValueToPos(Value: Double): Integer;
begin
 // ToDo: Create a true mapping
 if Value < -40 then result := 0 else
 if Value >   3 then result := VUMeter.NumGlyphs - 1 else
 if Value < -10 then result := round(((40 + Value) / 30) * 22) else
 if Value <  -7 then result := round(22 + (10 + Value) * 1.66) else
 if Value <  -5 then result := round(28 + (7 + Value) * 2.5) else
 if Value <  -3 then result := round(33 + (5 + Value) * 2.5) else
 if Value <  -1 then result := round(38 + (3 + Value) * 2.5) else
 if Value <   0 then result := round(43 + (1 + Value) * 5)
  else result := round(48 + Value / 3 * (VUMeter.NumGlyphs - 49));
end;

procedure TFmLA4029.VUMeterTimerTimer(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  case LevelState of
    lsIn : VUMeter.Position := VUMeterValueToPos(InLevel_dB);
    lsGR : VUMeter.Position := VUMeterValueToPos(GRReduction_dB);
   lsOut : VUMeter.Position := VUMeterValueToPos(OutLevel_dB);
  end;
end;

procedure TFmLA4029.UpdateRatio;
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Log10(Parameter[5]);
   if DialRatio.Position <> s
    then DialRatio.Position := s;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[5], ffFixed, 3, 1);
  end;
end;

procedure TFmLA4029.DialInputChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[1] <> DialInput.Position
    then Parameter[1] := DialInput.Position;
   UpdateInput;
  end;
end;

procedure TFmLA4029.DialOutputChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   if Parameter[2] <> DialOutput.Position
    then Parameter[2] := DialOutput.Position;
   UpdateOutput;
  end;
end;

procedure TFmLA4029.BtGRClick(Sender: TObject);
begin
 LevelState := lsGR;
end;

procedure TFmLA4029.BtInClick(Sender: TObject);
begin
 LevelState := lsIn;
end;

procedure TFmLA4029.BtOutClick(Sender: TObject);
begin
 LevelState := lsOut;
end;

procedure TFmLA4029.DialAttackChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialAttack.Position);
   Parameter[3] := s;
  end;
end;

procedure TFmLA4029.DialReleaseChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialRelease.Position);
   Parameter[4] := s;
  end;
end;

procedure TFmLA4029.DialRatioChange(Sender: TObject);
var
  s : Single;
begin
 with TLA4029DataModule(Owner) do
  begin
   s := Power(10, DialRatio.Position);
   if abs (Parameter[5] - s) > 1E-3
    then Parameter[5] := s;
   UpdateRatio;
  end;
end;

procedure TFmLA4029.DialKneeChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[6] := DialKnee.Position;
   UpdateKnee;
  end;
end;

procedure TFmLA4029.DialMixChange(Sender: TObject);
begin
 with TLA4029DataModule(Owner) do
  begin
   Parameter[7] := DialMix.Position;
   UpdateMix;
  end;
end;

end.
