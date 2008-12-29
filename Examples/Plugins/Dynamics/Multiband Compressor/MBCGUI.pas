unit MBCGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  ExtCtrls, DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmMBC = class(TForm)
    CBLimiter: TCheckBox;
    DlHighAttack: TGuiDial;
    DlHighGain: TGuiDial;
    DlHighRatio: TGuiDial;
    DlHighRelease: TGuiDial;
    DlHighThreshold: TGuiDial;
    DlLowAttack: TGuiDial;
    DlLowGain: TGuiDial;
    DlLowRatio: TGuiDial;
    DlLowRelease: TGuiDial;
    DlLowThreshold: TGuiDial;
    DlMidAttack: TGuiDial;
    DlMidGain: TGuiDial;
    DlMidRatio: TGuiDial;
    DlMidRelease: TGuiDial;
    DlMidThreshold: TGuiDial;
    LbAbout1: TLabel;
    LbAbout2: TLabel;
    LbCrossover: TLabel;
    LbHighAttack: TLabel;
    LbHighAttackValue: TLabel;
    LbHighBand: TLabel;
    LbHighBandVU: TLabel;
    LbHighFreqHz: TLabel;
    LbHighGain: TLabel;
    LbHighGaindB: TLabel;
    LbHighInput: TLabel;
    LbHighRatio: TLabel;
    LbHighRatioValue: TLabel;
    LbHighRed: TLabel;
    LbHighRelease: TLabel;
    LbHighReleaseValue: TLabel;
    LbHighThreshold: TLabel;
    LbHighThresholddB: TLabel;
    LbInput: TLabel;
    LbInputL: TLabel;
    LbInputR: TLabel;
    LbLowAttack: TLabel;
    LbLowAttackValue: TLabel;
    LbLowBand: TLabel;
    LbLowBandVU: TLabel;
    LbLowFreqHz: TLabel;
    LbLowGain: TLabel;
    LbLowGaindB: TLabel;
    LbLowInput: TLabel;
    LbLowRatio: TLabel;
    LbLowRatioValue: TLabel;
    LbLowRed: TLabel;
    LbLowRelease: TLabel;
    LbLowReleaseValue: TLabel;
    LbLowThreshold: TLabel;
    LbLowThresholddB: TLabel;
    LbMasterGain: TLabel;
    LbMasterGaindB: TLabel;
    LbMidAttack: TLabel;
    LbMidAttackValue: TLabel;
    LbMidBand: TLabel;
    LbMidBandVU: TLabel;
    LbMidGain: TLabel;
    LbMidGaindB: TLabel;
    LbMidInput: TLabel;
    LbMidRatio: TLabel;
    LbMidRatioValue: TLabel;
    LbMidRed: TLabel;
    LbMidRelease: TLabel;
    LbMidReleaseValue: TLabel;
    LbMidThreshold: TLabel;
    LbMidThresholddB: TLabel;
    LbOutput: TLabel;
    LbOutputLeft: TLabel;
    LbOutputRight: TLabel;
    MeterIn: TPaintBox;
    MeterOut: TPaintBox;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    RBBWIIR: TRadioButton;
    RbLPFIR: TRadioButton;
    SbHighFreq: TScrollBar;
    SbLowFreq: TScrollBar;
    SbMasterGain: TScrollBar;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SbLowFreqChange(Sender: TObject);
    procedure SbHighFreqChange(Sender: TObject);
    procedure SbMasterGainChange(Sender: TObject);
    procedure DlLowGainChange(Sender: TObject);
    procedure DlLowThresholdChange(Sender: TObject);
    procedure DlLowRatioChange(Sender: TObject);
    procedure DlLowAttackChange(Sender: TObject);
    procedure DlLowReleaseChange(Sender: TObject);
    procedure DlMidThresholdChange(Sender: TObject);
    procedure DlMidRatioChange(Sender: TObject);
    procedure DlMidAttackChange(Sender: TObject);
    procedure DlMidReleaseChange(Sender: TObject);
    procedure DlMidGainChange(Sender: TObject);
    procedure DlHighThresholdChange(Sender: TObject);
    procedure DlHighRatioChange(Sender: TObject);
    procedure DlHighAttackChange(Sender: TObject);
    procedure DlHighReleaseChange(Sender: TObject);
    procedure DlHighGainChange(Sender: TObject);
    procedure MeterInPaint(Sender: TObject);
    procedure MeterOutPaint(Sender: TObject);
  private
    FBackground : TBitmap;
  end;

implementation

{$R *.DFM}

uses
  Math, MBCDM;

procedure TFmMBC.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 FBackground := TBitmap.Create;
 RS := TResourceStream.Create(hInstance, 'SlimSlowKnob', 'BMP');
 try
  DlLowThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DlLowRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DlLowAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DlLowRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DlLowGain.DialBitmap.LoadFromStream(RS);      RS.Position := 0;

  DlMidThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DlMidRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DlMidAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DlMidRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DlMidGain.DialBitmap.LoadFromStream(RS);      RS.Position := 0;

  DlHighThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DlHighRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DlHighAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DlHighRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DlHighGain.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TFmMBC.FormDestroy(Sender: TObject);
begin
 FBackground.Free;
end;

procedure TFmMBC.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmMBC.FormResize(Sender: TObject);
var
  x, y : Integer;
begin
 with FBackground do
  begin
   Width := ClientWidth;
   Height := ClientHeight;
   for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
     if (x mod 2 = 0) and (y mod 2 = 0)
      then Canvas.Pixels[X, Y] := $9BA0A2
      else Canvas.Pixels[X, Y] := $BAC0C3;
   Canvas.MoveTo(10, 30);
   Canvas.LineTo(Width - 10, 30);
   Canvas.Brush.Color := clBtnFace;
   Canvas.Pen.Color   := $8C9091;
   Canvas.Rectangle(20, 40, 251, 181);
   Canvas.Rectangle(260, 40, 326, 181);
   Canvas.Rectangle(335, 40, 401, 181);
   Canvas.Rectangle(410, 40, 521, 181);
   Canvas.Rectangle(530, 40, 641, 181);
   Canvas.Rectangle(650, 40, 761, 181);

   Canvas.Rectangle(20, 190, 261, 300);
   Canvas.Rectangle(270, 190, 511, 300);
   Canvas.Rectangle(520, 190, 761, 300);
  end;
end;

procedure TFmMBC.MeterInPaint(Sender: TObject);
begin
 with TPaintBox(Sender).Canvas do
  begin
   Brush.Color := clBlack;
   FrameRect(ClipRect);
  end;
end;

procedure TFmMBC.MeterOutPaint(Sender: TObject);
begin
 with TPaintBox(Sender).Canvas do
  begin
   Brush.Color := clBlack;
   FrameRect(ClipRect);
  end;
end;

procedure TFmMBC.DlLowGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[0] := DlLowGain.Position
end;

procedure TFmMBC.DlLowThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[3] := DlLowThreshold.Position;
end;

procedure TFmMBC.DlLowRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[4] := Power(10, DlLowRatio.Position);
end;

procedure TFmMBC.DlLowAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[5] := Power(10, DlLowAttack.Position);
end;

procedure TFmMBC.DlLowReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[6] := Power(10, DlLowRelease.Position);
end;

procedure TFmMBC.SbLowFreqChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[1] := FreqLinearToLog(SbLowFreq.Position * 0.0001);
end;

procedure TFmMBC.DlMidGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[7] := DlMidGain.Position;
end;

procedure TFmMBC.DlMidThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[8] := DlMidThreshold.Position;
end;

procedure TFmMBC.DlMidRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[9] := Power(10, DlMidRatio.Position);
end;

procedure TFmMBC.DlMidAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[10] := Power(10, DlMidAttack.Position);
end;

procedure TFmMBC.DlMidReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[11] := Power(10, DlMidRelease.Position);
end;

procedure TFmMBC.DlHighGainChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[14] := DlHighGain.Position
end;

procedure TFmMBC.DlHighThresholdChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[15] := DlHighThreshold.Position;
end;

procedure TFmMBC.DlHighRatioChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[16] := Power(10, DlHighRatio.Position);
end;

procedure TFmMBC.DlHighAttackChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[17] := Power(10, DlHighAttack.Position);
end;

procedure TFmMBC.DlHighReleaseChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[18] := Power(10, DlHighRelease.Position);
end;

procedure TFmMBC.SbMasterGainChange(Sender: TObject);
begin
 LbMasterGaindB.Caption := FloatToStrF(0.1 * SbMasterGain.Position, ffGeneral, 5, 2) + 'dB';
end;

procedure TFmMBC.SbHighFreqChange(Sender: TObject);
begin
 (Owner as TMBCDataModule).Parameter[12] := FreqLinearToLog(SbHighFreq.Position * 0.0001);
end;

end.
