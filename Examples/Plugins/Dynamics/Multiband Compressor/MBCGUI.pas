unit MBCGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, Graphics, DVSTModule,
  Controls, StdCtrls, DGuiDial, ExtCtrls, DGuiBaseControl;

type
  TFmMBC = class(TForm)
    LbAbout1: TLabel;
    LbAbout2: TLabel;
    CBLimiter: TCheckBox;
    RbLPFIR: TRadioButton;
    RBBWIIR: TRadioButton;
    SbMasterGain: TScrollBar;
    SbLowFreq: TScrollBar;
    SbHighFreq: TScrollBar;
    LbMasterGain: TLabel;
    LbCrossover: TLabel;
    LbMasterGaindB: TLabel;
    LbLowFreqHz: TLabel;
    LbHighFreqHz: TLabel;
    DlLowThreshold: TGuiDial;
    DlLowRatio: TGuiDial;
    DlLowAttack: TGuiDial;
    DlLowRelease: TGuiDial;
    DlLowGain: TGuiDial;
    DlMidThreshold: TGuiDial;
    DlMidRatio: TGuiDial;
    DlMidAttack: TGuiDial;
    DlMidRelease: TGuiDial;
    DlMidGain: TGuiDial;
    DlHighThreshold: TGuiDial;
    DlHighRatio: TGuiDial;
    DlHighAttack: TGuiDial;
    DlHighRelease: TGuiDial;
    DlHighGain: TGuiDial;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LbLowThreshold: TLabel;
    LbLowRatio: TLabel;
    LbLowAttack: TLabel;
    LbLowRelease: TLabel;
    LbLowGain: TLabel;
    LbMidThreshold: TLabel;
    LbMidRatio: TLabel;
    LbMidAttack: TLabel;
    LbMidRelease: TLabel;
    LbMidGain: TLabel;
    LbHighThreshold: TLabel;
    LbHighRatio: TLabel;
    LbHighAttack: TLabel;
    LbHighRelease: TLabel;
    LbHighGain: TLabel;
    LbLowThresholddB: TLabel;
    LbLowRatioValue: TLabel;
    LbLowAttackValue: TLabel;
    LbLowReleaseValue: TLabel;
    LbLowGaindB: TLabel;
    LbMidThresholddB: TLabel;
    LbMidRatioValue: TLabel;
    LbMidAttackValue: TLabel;
    LbMidReleaseValue: TLabel;
    LbMidGaindB: TLabel;
    LbHighThresholddB: TLabel;
    LbHighRatioValue: TLabel;
    LbHighAttackValue: TLabel;
    LbHighReleaseValue: TLabel;
    LbHighGaindB: TLabel;
    MeterIn: TPaintBox;
    MeterOut: TPaintBox;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    LbInputL: TLabel;
    LbInputR: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
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
    fBackground : TBitmap;
  end;

implementation

{$R *.DFM}

uses Math, MBCDM;

procedure TFmMBC.FormCreate(Sender: TObject);
begin
 fBackground := TBitmap.Create;
end;

procedure TFmMBC.FormDestroy(Sender: TObject);
begin
 fBackground.Free;
end;

procedure TFmMBC.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, fBackground);
end;

procedure TFmMBC.FormResize(Sender: TObject);
var x, y : Integer;
begin
 with fBackground do
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
