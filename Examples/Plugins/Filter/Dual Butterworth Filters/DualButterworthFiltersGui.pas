unit DualButterworthFiltersGui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DAV_GuiPanel, DAV_GuiLabel, DAV_GuiBaseControl,
  DAV_GuiDial, StdCtrls, DAV_GuiGroup, DAV_GuiEQGraph, DAV_GuiLED;

type
  TFmLinkwitzRiley = class(TForm)
    GpLiknwitzRiley: TGuiGroup;
    DialLowpassFrequency: TGuiDial;
    DialLowpassSlope: TGuiDial;
    LbFrequency: TGuiLabel;
    LbSlope: TGuiLabel;
    PnDisplay: TGuiPanel;
    LbDisplay: TGuiLabel;
    GbFrequencyResponse: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    DialHighpassFrequency: TGuiDial;
    DialHighpassSlope: TGuiDial;
    LbLowpass: TGuiLabel;
    LbHighpass: TGuiLabel;
    LedHighCut: TGuiLED;
    LedLowCut: TGuiLED;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DialLowpassSlopeChange(Sender: TObject);
    procedure DialLowpassFrequencyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject;
      const Frequency: Single): Single;
    procedure DialHighpassSlopeChange(Sender: TObject);
    procedure DialHighpassFrequencyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LedHighCutClick(Sender: TObject);
    procedure LedLowCutClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateLowpassFrequency;
    procedure UpdateLowpassSlope;
    procedure UpdateHighpassFrequency;
    procedure UpdateHighpassSlope;
    procedure UpdateType;
  end;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon, PNGImage, DualButterworthFiltersDM, DAV_VSTModuleWithPrograms;

procedure TFmLinkwitzRiley.FormCreate(Sender: TObject);
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
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($70 - $34 * (s[1] - h));
       Line[x].G := round($84 - $48 * (s[1] - h));
       Line[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ClipperKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialLowpassFrequency.DialBitmap.Assign(PngBmp);
   DialLowpassSlope.DialBitmap.Assign(PngBmp);
   DialHighpassFrequency.DialBitmap.Assign(PngBmp);
   DialHighpassSlope.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLinkwitzRiley.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateHighpassFrequency;
 UpdateHighpassSlope;
 UpdateType;
 LbDisplay.Caption := 'Linkwitz-Riley';
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   Result := -5;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassFrequencyChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[0] <> DialLowpassFrequency.Position
    then Parameter[0] := DialLowpassFrequency.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[1] <> DialLowpassSlope.Position
    then Parameter[1] := DialLowpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassFrequencyChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[2] <> DialHighpassFrequency.Position
    then Parameter[2] := DialHighpassFrequency.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[3] <> DialHighpassSlope.Position
    then Parameter[3] := DialHighpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.LedHighCutClick(Sender: TObject);
var
  CurrentBit : Integer;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   Parameter[4] := (CurrentBit and $2) or ((not CurrentBit) and $1)
  end;
end;

procedure TFmLinkwitzRiley.LedLowCutClick(Sender: TObject);
var
  CurrentBit : Integer;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   Parameter[4] := (CurrentBit and $1) or ((not CurrentBit) and $2)
  end;
end;

procedure TFmLinkwitzRiley.UpdateLowpassFrequency;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if DialLowpassFrequency.Position <> Parameter[0]
    then DialLowpassFrequency.Position := Parameter[0];
   LbDisplay.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLinkwitzRiley.UpdateLowpassSlope;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if DialLowpassSlope.Position <> Parameter[1]
    then DialLowpassSlope.Position := Parameter[1];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLinkwitzRiley.UpdateHighpassFrequency;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if DialHighpassFrequency.Position <> Parameter[2]
    then DialHighpassFrequency.Position := Parameter[2];
   LbDisplay.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
  end;
end;

procedure TFmLinkwitzRiley.UpdateHighpassSlope;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if DialHighpassSlope.Position <> Parameter[3]
    then DialHighpassSlope.Position := Parameter[3];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[3] + ' ' + ParameterLabel[3];
  end;
end;

procedure TFmLinkwitzRiley.UpdateType;
var
  CurrentBit : Integer;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   if (CurrentBit and $1) > 0
    then LedHighCut.Brightness_Percent := 90
    else LedHighCut.Brightness_Percent := 10;
   if (CurrentBit and $2) > 0
    then LedLowCut.Brightness_Percent := 90
    else LedLowCut.Brightness_Percent := 10;

   LbDisplay.Caption := ParameterDisplay[4];
  end;
end;

end.
