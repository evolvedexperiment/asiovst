unit BassExtenderGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, Controls, ExtCtrls, DAV_GuiPanel,
  DAV_GuiDial, DAV_GuiLabel;

type
  TFmBassExtender = class(TForm)
    PnMain: TGuiPanel;
    DialAttack: TGuiDial;
    DialBalance: TGuiDial;
    DialCompression: TGuiDial;
    DialDivide: TGuiDial;
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialShape: TGuiDial;
    DialThreshold: TGuiDial;
    GuiLabel2: TGuiLabel;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbBalance: TGuiLabel;
    LbBalanceValue: TGuiLabel;
    LbCompression: TGuiLabel;
    LbCompressionValue: TGuiLabel;
    LbDivide: TGuiLabel;
    LbDivideValue: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbShape: TGuiLabel;
    LbShapeValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialDivideChange(Sender: TObject);
    procedure DialShapeChange(Sender: TObject);
    procedure DialBalanceChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialCompressionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateAttack;
    procedure UpdateBalance;
    procedure UpdateCompressionMix;
    procedure UpdateDivider;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateShape;
    procedure UpdateSplitFrequency;
    procedure UpdateSplitOrder;
    procedure UpdateThreshold;
  end;

implementation

uses
  Math, DAV_GuiCommon, BassExtenderDM, PngImage;

{$R *.DFM}

procedure TFmBassExtender.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($30 - $24 * (s[1] - h));
       Line[x].G := round($44 - $38 * (s[1] - h));
       Line[x].R := round($4D - $40 * (s[1] - h));
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'BassExtender', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFrequency.DialBitmap.Assign(PngBmp);
   DialOrder.DialBitmap.Assign(PngBmp);
   DialDivide.DialBitmap.Assign(PngBmp);
   DialShape.DialBitmap.Assign(PngBmp);
   DialRatio.DialBitmap.Assign(PngBmp);
   DialAttack.DialBitmap.Assign(PngBmp);
   DialRelease.DialBitmap.Assign(PngBmp);
   DialCompression.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'BassExtenderPan', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialBalance.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'BassExtenderThreshold', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialThreshold.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmBassExtender.DialAttackChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Attack'] <> DialAttack.Position
    then ParameterByName['Attack'] := DialAttack.Position;
  end;
end;

procedure TFmBassExtender.DialBalanceChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Balance'] <> DialBalance.Position
    then ParameterByName['Balance'] := DialBalance.Position;
  end;
end;

procedure TFmBassExtender.DialCompressionChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Compression Mix'] <> DialCompression.Position
    then ParameterByName['Compression Mix'] := DialCompression.Position;
  end;
end;

procedure TFmBassExtender.DialDivideChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Divider'] <> DialDivide.Position
    then ParameterByName['Divider'] := DialDivide.Position;
  end;
end;

procedure TFmBassExtender.DialFrequencyChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Split Frequency'] <> DialFrequency.Position
    then ParameterByName['Split Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmBassExtender.DialOrderChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Split Order'] <> DialOrder.Position
    then ParameterByName['Split Order'] := DialOrder.Position;
  end;
end;

procedure TFmBassExtender.DialRatioChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Ratio'] <> DialRatio.Position
    then ParameterByName['Ratio'] := DialRatio.Position;
  end;
end;

procedure TFmBassExtender.DialReleaseChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Release'] <> DialRelease.Position
    then ParameterByName['Release'] := DialRelease.Position;
  end;
end;

procedure TFmBassExtender.DialShapeChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Shape'] <> DialShape.Position
    then ParameterByName['Shape'] := DialShape.Position;
  end;
end;

procedure TFmBassExtender.DialThresholdChange(Sender: TObject);
begin
 with TBassExtenderModule(Owner) do
  begin
   if ParameterByName['Threshold'] <> DialThreshold.Position
    then ParameterByName['Threshold'] := DialThreshold.Position;
  end;
end;

procedure TFmBassExtender.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmBassExtender.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateBalance;
 UpdateCompressionMix;
 UpdateDivider;
 UpdateRatio;
 UpdateRelease;
 UpdateShape;
 UpdateSplitFrequency;
 UpdateSplitOrder;
 UpdateThreshold;
end;

procedure TFmBassExtender.UpdateAttack;
var
  Attack : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Attack := ParameterByName['Attack'];
   if DialAttack.Position <> Attack
    then DialAttack.Position := Attack;
   if Attack < 1000
    then LbAttackValue.Caption := FloatToStrF(Attack, ffGeneral, 3, 3) + ' µs'
    else LbAttackValue.Caption := FloatToStrF(1E-3 * Attack, ffGeneral, 3, 3) + ' ms';
  end;
end;

procedure TFmBassExtender.UpdateBalance;
var
  Balance : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Balance := ParameterByName['Balance'];
   if DialBalance.Position <> Balance
    then DialBalance.Position := Balance;
//   Balance := round(1E5 * Balance) * 1E-5;
   LbBalanceValue.Caption := FloatToStrF(Balance, ffGeneral, 3, 4) + '%';
  end;
end;

procedure TFmBassExtender.UpdateCompressionMix;
var
  Compression : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Compression := ParameterByName['Compression Mix'];
   if DialCompression.Position <> Compression
    then DialCompression.Position := Compression;
   LbCompressionValue.Caption := FloatToStrF(Compression, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmBassExtender.UpdateDivider;
var
  Divider : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Divider := ParameterByName['Divider'];
   if DialDivide.Position <> Divider
    then DialDivide.Position := Divider;
   LbDivideValue.Caption := FloatToStrF(Divider, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmBassExtender.UpdateRatio;
var
  Ratio : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Ratio := ParameterByName['Ratio'];
   if DialRatio.Position <> Ratio
    then DialRatio.Position := Ratio;
   if Ratio = 1000
    then LbRatioValue.Caption := '1 : oo'
    else LbRatioValue.Caption := '1 : ' + FloatToStrF(Ratio, ffGeneral, 3, 4);
  end;
end;

procedure TFmBassExtender.UpdateRelease;
var
  Release : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Release := ParameterByName['Release'];
   if DialRelease.Position <> Release
    then DialRelease.Position := Release;
   if Release < 1000
    then LbReleaseValue.Caption := FloatToStrF(Release, ffGeneral, 3, 2) + ' ms'
    else LbReleaseValue.Caption := FloatToStrF(Release * 1E-3, ffGeneral, 3, 2) + ' s';
  end;
end;

procedure TFmBassExtender.UpdateShape;
var
  Shape : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Shape := ParameterByName['Shape'];
   if DialShape.Position <> Shape
    then DialShape.Position := Shape;
   LbShapeValue.Caption := FloatToStrF(Shape, ffGeneral, 3, 2) + '%';
  end;
end;

procedure TFmBassExtender.UpdateSplitFrequency;
var
  Frequency : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Frequency := ParameterByName['Split Frequency'];
   if DialFrequency.Position <> Frequency
    then DialFrequency.Position := Frequency;
   if Frequency < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Frequency, ffGeneral, 3, 2) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Frequency * 1E-3, ffGeneral, 3, 2) + ' kHz';
  end;
end;

procedure TFmBassExtender.UpdateSplitOrder;
var
  Order : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Order := ParameterByName['Split Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(2 * round(Order));
  end;
end;

procedure TFmBassExtender.UpdateThreshold;
var
  Threshold : Single;
begin
 with TBassExtenderModule(Owner) do
  begin
   Threshold := ParameterByName['Threshold'];
   if DialThreshold.Position <> Threshold
    then DialThreshold.Position := Threshold;
   Threshold := round(1E4 * Threshold) * 1E-4;
   LbThresholdValue.Caption := FloatToStrF(Threshold, ffGeneral, 3, 4) + 'dB';
  end;
end;

end.