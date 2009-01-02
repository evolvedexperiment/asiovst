unit CrossoverDistortionGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, ExtCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiPanel;

type
  TFmCrossoverDistortion = class(TForm)
    DialFreq: TGuiDial;
    DialHighDist: TGuiDial;
    DialLowDist: TGuiDial;
    DialOrder: TGuiDial;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbHighDist: TGuiLabel;
    LbHighDistValue: TGuiLabel;
    LbLowDist: TGuiLabel;
    LbLowDistValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    PnControl: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialLowDistChange(Sender: TObject);
    procedure DialHighDistChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateLowDistortion;
    procedure UpdateHighDistortion;
  end;

implementation

{$R *.DFM}

uses
  PNGImage, DAV_VSTModuleWithPrograms, CrossoverDistortionDM;

procedure TFmCrossoverDistortion.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
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
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := round($3F + $1A * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'CrossoverKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFreq.DialBitmap.Assign(PngBmp);
   DialOrder.DialBitmap.Assign(PngBmp);
   DialHighDist.DialBitmap.Assign(PngBmp);
   DialLowDist.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmCrossoverDistortion.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmCrossoverDistortion.DialFreqChange(Sender: TObject);
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   ParameterByName['Crossover Frequency'] := DialFreq.Position;
  end;
end;

procedure TFmCrossoverDistortion.DialHighDistChange(Sender: TObject);
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   ParameterByName['High Distortion'] := DialHighDist.Position;
  end; 
end;

procedure TFmCrossoverDistortion.DialLowDistChange(Sender: TObject);
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   ParameterByName['Low Distortion'] := DialLowDist.Position;
  end;
end;

procedure TFmCrossoverDistortion.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   DesiredOrder := round(DialOrder.Position);
   CurrentOrder := ParameterByName['Crossover Order'];
   if round(CurrentOrder) = DesiredOrder then
    if DialOrder.Position < CurrentOrder
     then ParameterByName['Crossover Order'] := DesiredOrder - 1 else
    if DialOrder.Position > CurrentOrder
     then ParameterByName['Crossover Order'] := DesiredOrder + 1 else
  end;
end;

procedure TFmCrossoverDistortion.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
 UpdateLowDistortion;
 UpdateHighDistortion;
end;

procedure TFmCrossoverDistortion.UpdateFrequency;
var
  Freq : Single;
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   Freq := ParameterByName['Crossover Frequency'];
   if Freq < 1000
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
   if DialFreq.Position <> Freq
    then DialFreq.Position := Freq;
  end;
end;

procedure TFmCrossoverDistortion.UpdateHighDistortion;
var
  HighDist : Single;
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   HighDist := ParameterByName['High Distortion'];
   LbHighDistValue.Caption := FloatToStrF(HighDist, ffGeneral, 3, 1) + '%';
   if DialHighDist.Position <> HighDist
    then DialHighDist.Position := HighDist;
  end;
end;

procedure TFmCrossoverDistortion.UpdateLowDistortion;
var
  LowDist : Single;
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   LowDist := ParameterByName['Low Distortion'];
   LbLowDistValue.Caption := FloatToStrF(LowDist, ffGeneral, 3, 1) + '%';
   if DialLowDist.Position <> LowDist
    then DialLowDist.Position := LowDist;
  end;
end;

procedure TFmCrossoverDistortion.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TCrossoverDistortionDataModule do
  begin
   Order := round(ParameterByName['Crossover Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
  end;
end;

end.