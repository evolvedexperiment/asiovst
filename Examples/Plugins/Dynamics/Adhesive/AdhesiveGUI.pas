unit AdhesiveGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, ExtCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiVUMeter;

type
  TFmAdhesive = class(TForm)
    DialAttack: TGuiDial;
    DialFilter: TGuiDial;
    DialKnee: TGuiDial;
    DialMakeUpGain: TGuiDial;
    DialMix: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    LbTimeConstants: TGuiLabel;
    LbCharacteristic: TGuiLabel;
    LbIO: TGuiLabel;
    LbAttack: TGuiLabel;
    LbExt: TGuiLabel;
    LbIn: TGuiLabel;
    LbKnee: TGuiLabel;
    LbMakeUpGain: TGuiLabel;
    LbMix: TGuiLabel;
    LbPeakClip: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSCHP: TGuiLabel;
    LbSideChain: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbTitle: TGuiLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    SwLimit: TGuiSwitch;
    SwOnOff: TGuiSwitch;
    SwSideChain: TGuiSwitch;
    Timer: TTimer;
    VUMeter: TGuiVUMeter;
    LbThresholdMin: TGuiLabel;
    LbThresholdMid: TGuiLabel;
    LbThresholdMax: TGuiLabel;
    LbMakeupMin: TGuiLabel;
    LbMakeUpMid: TGuiLabel;
    LbMakeUpMax: TGuiLabel;
    LbRatioMin: TGuiLabel;
    LbRatioMax: TGuiLabel;
    LbRatioMed: TGuiLabel;
    LbKneeMin: TGuiLabel;
    LbKneeMax: TGuiLabel;
    GuiLabel1: TGuiLabel;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    LbSCmin: TGuiLabel;
    GuiLabel4: TGuiLabel;
    GuiLabel5: TGuiLabel;
    GuiLabel6: TGuiLabel;
    GuiLabel7: TGuiLabel;
    GuiLabel8: TGuiLabel;
    GuiLabel9: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDLimitClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialFilterChange(Sender: TObject);
    procedure SwOnOffChange(Sender: TObject);
    procedure SwSideChainChange(Sender: TObject);
    procedure SwLimitChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackground : TBitmap;
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateMix;
    procedure UpdateSideChainFilter;
    procedure UpdateOnOff;
    procedure UpdatePeakClip;
    procedure UpdateExtSideChain;
  end;

implementation

uses
  AdhesiveDM, PngImage, DAV_GuiCommon, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmAdhesive.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 FBackground := TBitmap.Create;
 FBackground.PixelFormat := pf24bit;
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'CytomicBlue', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList.DialImages.Add, DialBitmap do
    begin
     NumGlyphs := 65;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialThreshold.DialImageIndex := 0;
   DialKnee.DialImageIndex := 0;
   DialRatio.DialImageIndex := 0;
   DialMakeUpGain.DialImageIndex := 0;
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'CytomicYellow', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList.DialImages.Add, DialBitmap do
    begin
     NumGlyphs := 65;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialMix.DialImageIndex := 1;
   DialFilter.DialImageIndex := 1;
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'CytomicGreen', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList.DialImages.Add, DialBitmap do
    begin
     NumGlyphs := 65;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialAttack.DialImageIndex := 2;
   DialRelease.DialImageIndex := 2;
  finally
   RS.Free;
  end;

 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmAdhesive.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmAdhesive.FormResize(Sender: TObject);
var
  x, y : Integer;
  b    : Byte;
  Line : PRGB24Array;
begin
 with FBackground do
  begin
   Width := ClientWidth;
   Height := ClientHeight;
   for y := 0 to Height - 1 do
    begin
     Line := ScanLine[y];
     for x := 0 to Width - 1 do
      begin
       b := random(16);
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;
end;

procedure TFmAdhesive.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
 UpdateMakeUp;
 UpdateOnOff;
 UpdatePeakClip;
 UpdateExtSideChain;
end;

procedure TFmAdhesive.LEDAutoGainClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.LEDLimitClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[7] := Integer(LEDLimit.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.LEDStereoClick(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
//   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmAdhesive.DialThresholdChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[0] := -DialThreshold.Position;
  end;
end;

procedure TFmAdhesive.DialMakeUpGainChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[1] := DialMakeUpGain.Position;
  end;
end;

procedure TFmAdhesive.DialRatioChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[2] := DialRatio.Position;
  end;
end;

procedure TFmAdhesive.DialKneeChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[3] := DialKnee.Position;
  end;
end;

procedure TFmAdhesive.DialAttackChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[4] := DialAttack.Position;
  end;
end;

procedure TFmAdhesive.DialReleaseChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[5] := 1E3 * DialRelease.Position;
  end;
end;

procedure TFmAdhesive.DialMixChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[6] := DialMix.Position;
  end;
end;

procedure TFmAdhesive.SwOnOffChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[7] := 1 - SwOnOff.GlyphNr;
  end;
end;

procedure TFmAdhesive.SwLimitChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[8] := 1 - SwLimit.GlyphNr;
  end;
end;

procedure TFmAdhesive.DialFilterChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[9] := DialFilter.Position;
  end;
end;

procedure TFmAdhesive.SwSideChainChange(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Parameter[10] := 1 - SwSideChain.GlyphNr;
  end;
end;

procedure TFmAdhesive.TimerTimer(Sender: TObject);
begin
 with TAdhesiveDataModule(Owner).FastCompressor, VUMeter
  do GlyphIndex := round(NumGlyphs * limit(-GainReductiondB, 0, 40) / 40);
end;

procedure TFmAdhesive.UpdateThreshold;
var
  Threshold : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Threshold := -Parameter[0];
   if Threshold <> DialThreshold.Position
    then DialThreshold.Position := Threshold;
  end;
end;

procedure TFmAdhesive.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   MakeUp := Parameter[1];
   if MakeUp <> DialMakeUpGain.Position
    then DialMakeUpGain.Position := MakeUp;
  end;
end;

procedure TFmAdhesive.UpdateRatio;
var
  Ratio : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Ratio := Parameter[2];
   if Ratio <> DialRatio.Position
    then DialRatio.Position := Ratio;
  end;
end;

procedure TFmAdhesive.UpdateKnee;
var
  Knee : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Knee := Parameter[3];
   if Knee <> DialKnee.Position
    then DialKnee.Position := Knee;
  end;
end;

procedure TFmAdhesive.UpdateAttack;
var
  Attack : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Attack := Parameter[4];
   if Attack <> DialAttack.Position
    then DialAttack.Position := Attack;
  end;
end;

procedure TFmAdhesive.UpdateRelease;
var
  Release : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Release := 1E-3 * Parameter[5];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
  end;
end;

procedure TFmAdhesive.UpdateSideChainFilter;
var
  SidechainFilter : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SidechainFilter := Parameter[9];
   if SidechainFilter <> DialFilter.Position
    then DialFilter.Position := SidechainFilter;
  end;
end;

procedure TFmAdhesive.UpdateMix;
var
  Mix : Single;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   Mix := Parameter[6];
   if Mix <> DialMix.Position
    then DialMix.Position := Mix;
  end;
end;

procedure TFmAdhesive.UpdateOnOff;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwOnOff.GlyphNr := 1 - round(Parameter[7]);
  end;
end;

procedure TFmAdhesive.UpdateExtSideChain;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwSideChain.GlyphNr := 1 - round(Parameter[10]);
  end;
end;

procedure TFmAdhesive.UpdatePeakClip;
begin
 with TAdhesiveDataModule(Owner) do
  begin
   SwLimit.GlyphNr := 1 - round(Parameter[8]);
  end;
end;

end.
