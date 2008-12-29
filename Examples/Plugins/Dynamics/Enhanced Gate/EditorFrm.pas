unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  Gauges, ExtCtrls, DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLED, DAV_GuiGroup;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TEditorForm = class(TForm)
    CBDuck: TCheckBox;
    CBOnOff: TGuiLED;
    CBSideChain: TComboBox;
    CBStereoLink: TCheckBox;
    DialAttack: TGuiDial;
    DialDecay: TGuiDial;
    DialHiCut: TGuiDial;
    DialHold: TGuiDial;
    DialKnee: TGuiDial;
    DialLoCut: TGuiDial;
    DialRange: TGuiDial;
    DialRatio: TGuiDial;
    DialThreshold: TGuiDial;
    EdAttack: TEdit;
    EdDecay: TEdit;
    EdHiCut: TEdit;
    EdHold: TEdit;
    EdKnee: TEdit;
    EdLoCut: TEdit;
    EdRange: TEdit;
    EdRatio: TEdit;
    EdThreshold: TEdit;
    GaugeL: TGauge;
    GaugeR: TGauge;
    GBDynamics: TGuiGroup;
    GBMain: TGuiGroup;
    GBSideChain: TGuiGroup;
    LbAttack: TLabel;
    LbDecay: TLabel;
    LbEnhancedAudioGate: TLabel;
    LbEnhancedAudioGateShadow: TLabel;
    LBHighCut: TLabel;
    LbHold: TLabel;
    LbKnee: TLabel;
    LBLowCut: TLabel;
    LbRange: TLabel;
    LbRatio: TLabel;
    LbSource: TLabel;
    LbThreshold: TLabel;
    VUTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CBOnOffClick(Sender: TObject);
    procedure CBDuckClick(Sender: TObject);
    procedure CBStereoLinkClick(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialHoldChange(Sender: TObject);
    procedure DialDecayChange(Sender: TObject);
    procedure DialLoCutChange(Sender: TObject);
    procedure DialHiCutChange(Sender: TObject);
    procedure CBSideChainChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialRangeChange(Sender: TObject);
    procedure VUTimerTimer(Sender: TObject);
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateHold;
    procedure UpdateDecay;
    procedure UpdateHiCut;
    procedure UpdateLoCut;
    procedure UpdateKnee;
    procedure UpdateRange;
    procedure UpdateRatio;
  end;

implementation

{$R *.DFM}

uses
  Math, EnhancedGateDM;

procedure TEditorForm.CBOnOffClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[0] := Integer(CBOnOff.Brightness_Percent > 90);
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[1] := DialThreshold.Position;
  UpdateThreshold;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'BigKnob3D', 'BMP');
 try
  DialThreshold.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
  DialAttack.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialHold.DialBitmap.LoadFromStream(RS);       RS.Position := 0;
  DialDecay.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
 finally
  RS.Free;
 end;
 RS := TResourceStream.Create(hInstance, 'SmallKnob3D', 'BMP');
 try
  DialLoCut.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
  DialHiCut.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
  DialRatio.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
  DialKnee.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DialRange.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormPaint(Sender: TObject);
var
  x, y : Integer;
  s    : array[0..1] of Single;
  b    : ShortInt;
  bmp  : TBitmap;
  Line : PRGB32Array;
begin
  bmp := TBitmap.Create;
  with bmp do
   try
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
        Line[x].B := $AC + b;
        Line[x].G := $D9 + b;
        Line[x].R := $F0 + b;
        Line[x].A := 0;
       end;
     end;
    Self.Canvas.Draw(0, 0, bmp);
   finally
    Free;
   end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner) do
  if Boolean(Round(Parameter[0]))
   then CBOnOff.Brightness_Percent := 100
   else CBOnOff.Brightness_Percent := 20;
 UpdateThreshold;
 UpdateAttack;
 UpdateHold;
 UpdateDecay;
 UpdateHiCut;
 UpdateLoCut;
 UpdateKnee;
 UpdateRange;
 UpdateRatio;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[2] := Power(10, DialAttack.Position);
  UpdateAttack;
end;

procedure TEditorForm.DialHoldChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[3] := Power(10, DialHold.Position);
  UpdateHold;
end;

procedure TEditorForm.DialDecayChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[4] := Power(10, DialDecay.Position);
  UpdateDecay;
end;

procedure TEditorForm.CBDuckClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[5] := Integer(CBDuck.Checked);
end;

procedure TEditorForm.CBStereoLinkClick(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[6] := Integer(CBStereoLink.Checked);
end;

procedure TEditorForm.CBSideChainChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[7] := CBSideChain.ItemIndex;
end;

procedure TEditorForm.DialLoCutChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[8] := Power(10, DialLoCut.Position);
  UpdateLoCut;
end;

procedure TEditorForm.DialHiCutChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner)
   do Parameter[9] := 0.001 * Power(10, DialHiCut.Position);
  UpdateHiCut;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[10] := DialRatio.Position;
  UpdateRatio;
end;

procedure TEditorForm.DialKneeChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[11] := DialKnee.Position;
  UpdateKnee;
end;

procedure TEditorForm.DialRangeChange(Sender: TObject);
begin
  with TEnhancedGateDataModule(Owner) do Parameter[12] := DialRange.Position;
  UpdateRange;
end;

procedure TEditorForm.UpdateThreshold;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialThreshold.Position <> Parameter[1]
     then DialThreshold.Position := Parameter[1];
    EdThreshold.Text := FloatToStrF(DialThreshold.Position, ffFixed, 5, 1) + ' dB';
   end;
end;

procedure TEditorForm.VUTimerTimer(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   GaugeL.Progress := round(100 * LevelLeft);
   GaugeR.Progress := round(100 * LevelRight);
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialAttack.Position <> Log10(Parameter[2])
     then DialAttack.Position := Log10(Parameter[2]);
    i := Round(1.499999 - DialAttack.Position);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdAttack.Text := FloatToStrF(Parameter[2], ffFixed, 5, i) + ' ms';
   end;
end;

procedure TEditorForm.UpdateHold;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialHold.Position <> Log10(Parameter[3])
     then DialHold.Position := Log10(Parameter[3]);
    i := Round(1.499999 - DialHold.Position);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdHold.Text := FloatToStrF(Parameter[3], ffFixed, 5, i) + ' s';
   end;
end;

procedure TEditorForm.UpdateDecay;
var
  i: Integer;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialDecay.Position <> Log10(Parameter[4])
     then DialDecay.Position := Log10(Parameter[4]);
    i := Round(1.499999 - DialDecay.Position);
    if i < 0 then i := 0 else if i > 2 then i := 2;
    EdDecay.Text := FloatToStrF(Parameter[4], ffFixed, 5, i) + ' ms';
   end;
end;

procedure TEditorForm.UpdateLoCut;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialLoCut.Position <> Log10(Parameter[8])
     then DialLoCut.Position := Log10(Parameter[8]);
    if Parameter[8] < 1000
     then EdLoCut.Text := FloatToStrF(Parameter[8], ffFixed, 5, Round(2.49999 - Log10(Parameter[8]))) + ' Hz'
     else EdLoCut.Text := FloatToStrF(0.001 * Parameter[8], ffFixed, 5, 1) + ' kHz';
    if Parameter[8] > Parameter[9] * 1100
     then GBSideChain.Font.Color := clRed
     else GBSideChain.Font.Color := clWhite;
   end;
end;

procedure TEditorForm.UpdateHiCut;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialHiCut.Position <> Log10(1000 * Parameter[9])
     then DialHiCut.Position := Log10(1000 * Parameter[9]);
    if Parameter[9] < 1000
     then EdHiCut.Text := FloatToStrF(1000 * Parameter[9], ffFixed, 5, 0) + ' Hz'
     else EdHiCut.Text := FloatToStrF(Parameter[9], ffFixed, 5, Round(4.49999 - Log10(Parameter[9]))) + ' kHz';
    if Parameter[8] > Parameter[9] * 1100
     then GBSideChain.Font.Color := clRed
     else GBSideChain.Font.Color := clWhite;
   end;
end;

procedure TEditorForm.UpdateRatio;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialRatio.Position <> Parameter[10]
     then DialRatio.Position := Parameter[10];
    EdRatio.Text := FloatToStrF(Parameter[10], ffGeneral, 5, 5);
   end;
end;

procedure TEditorForm.UpdateKnee;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialKnee.Position <> Parameter[11]
     then DialKnee.Position := Parameter[11];
    EdKnee.Text := FloatToStrF(Parameter[11], ffFixed, 5, 2) + ' dB';
   end;
end;

procedure TEditorForm.UpdateRange;
begin
  with TEnhancedGateDataModule(Owner) do
   begin
    if DialRange.Position <> Parameter[12]
     then DialRange.Position := Parameter[12];
    EdRange.Text := FloatToStrF(Parameter[12], ffFixed, 5, 1) + ' dB';
   end;
end;

end.
