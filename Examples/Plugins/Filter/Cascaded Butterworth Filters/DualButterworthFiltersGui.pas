unit DualButterworthFiltersGui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, DAV_GuiPanel, DAV_GuiLabel,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGroup, DAV_GuiEQGraph, DAV_GuiLED;

type
  TFmLinkwitzRiley = class(TForm)
    DialHighpassFrequency: TGuiDial;
    DialHighpassSlope: TGuiDial;
    DialLowpassFrequency: TGuiDial;
    DialLowpassSlope: TGuiDial;
    GbFrequencyResponse: TGuiGroup;
    GpDualLiknwitzRiley: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    LbDisplay: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbHighpass: TGuiLabel;
    LbLowpass: TGuiLabel;
    LbSlope: TGuiLabel;
    LedHighCut: TGuiLED;
    LedLowCut: TGuiLED;
    PnDisplay: TGuiPanel;
    PuFrequency: TPopupMenu;
    Mi20Hz: TMenuItem;
    Mi31Hz5: TMenuItem;
    N251: TMenuItem;
    Mi16Hz: TMenuItem;
    Mi40Hz: TMenuItem;
    Mi50Hz: TMenuItem;
    Mi63Hz: TMenuItem;
    Mi80Hz: TMenuItem;
    Mi100Hz: TMenuItem;
    Mi125Hz: TMenuItem;
    Mi160Hz: TMenuItem;
    Mi200Hz: TMenuItem;
    Mi250Hz: TMenuItem;
    Mi315Hz: TMenuItem;
    Mi400Hz: TMenuItem;
    Mi500Hz: TMenuItem;
    Mi630Hz: TMenuItem;
    Mi800Hz: TMenuItem;
    Mi1kHz: TMenuItem;
    Mi1k25Hz: TMenuItem;
    Mi1k6Hz: TMenuItem;
    Mi2kHz: TMenuItem;
    Mi2k5Hz: TMenuItem;
    Mi31k5Hz: TMenuItem;
    Mi4kHz: TMenuItem;
    Mi5kHz: TMenuItem;
    Mi6k3Hz: TMenuItem;
    Mi8kHz: TMenuItem;
    Mi10kHz: TMenuItem;
    Mi12k5Hz: TMenuItem;
    Mi16kHz: TMenuItem;
    Mi20kHz: TMenuItem;
    PuPreset: TPopupMenu;
    MiLoadHigh: TMenuItem;
    MiStoreHigh: TMenuItem;
    MiLoadA: TMenuItem;
    MiLoadB: TMenuItem;
    MiLoadC: TMenuItem;
    MiLoadD: TMenuItem;
    MiLoadE: TMenuItem;
    MiLoadF: TMenuItem;
    MiStoreA: TMenuItem;
    MiStoreB: TMenuItem;
    MiStoreC: TMenuItem;
    MiStoreD: TMenuItem;
    MiStoreE: TMenuItem;
    MiStoreF: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure DialHighpassFrequencyChange(Sender: TObject);
    procedure DialHighpassFrequencyMouseEnter(Sender: TObject);
    procedure DialHighpassSlopeChange(Sender: TObject);
    procedure DialHighpassSlopeMouseEnter(Sender: TObject);
    procedure DialLowpassFrequencyChange(Sender: TObject);
    procedure DialLowpassFrequencyMouseEnter(Sender: TObject);
    procedure DialLowpassSlopeChange(Sender: TObject);
    procedure DialLowpassSlopeMouseEnter(Sender: TObject);
    procedure LbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LedHighCutClick(Sender: TObject);
    procedure LedLowCutClick(Sender: TObject);
    procedure Mi31Hz5Click(Sender: TObject);
    procedure MiFrequencyClick(Sender: TObject);
    procedure MiLoadClick(Sender: TObject);
    procedure MiStoreClick(Sender: TObject);
    procedure PuFrequencyPopup(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
    FCurrentDial     : TGuiDial;
    FIsLow           : Boolean;
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
  Math, PNGImage, DAV_GuiCommon, DualButterworthFiltersDM,
  DAV_VSTModuleWithPrograms;

resourcestring
  RCStrLinkwitzRiley = 'Linkwitz-Riley';

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
 UpdateLowpassFrequency;
 UpdateLowpassSlope;
 UpdateHighpassFrequency;
 UpdateHighpassSlope;
 UpdateType;
 LbDisplay.Caption := RCStrLinkwitzRiley;
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   Result := -5;
  end;
end;

function RoundFrequency(Value: Single): Single;
var
  Base10  : Double;
begin
 Base10 := Log10(Value);
 Result := RoundTo(Value, round(Base10 - 1.5));
end;

procedure TFmLinkwitzRiley.DialLowpassFrequencyChange(Sender: TObject);
var
  NewValue : Single;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if ssAlt in KeyboardStateToShiftState
    then NewValue := RoundFrequency(DialLowpassFrequency.Position)
    else NewValue := DialLowpassFrequency.Position;

   if Parameter[0] <> NewValue
    then Parameter[0] := NewValue;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassFrequencyMouseEnter(Sender: TObject);
begin
 UpdateLowpassFrequency;
 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.DialLowpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[1] <> DialLowpassSlope.Position
    then Parameter[1] := DialLowpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassSlopeMouseEnter(Sender: TObject);
begin
 UpdateLowpassSlope;
end;

procedure TFmLinkwitzRiley.DialHighpassFrequencyChange(Sender: TObject);
var
  NewValue : Single;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if ssAlt in KeyboardStateToShiftState
    then NewValue := RoundFrequency(DialHighpassFrequency.Position)
    else NewValue := DialHighpassFrequency.Position;

   if Parameter[2] <> NewValue
    then Parameter[2] := NewValue;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassFrequencyMouseEnter(Sender: TObject);
begin
 UpdateHighpassFrequency;
 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.DialHighpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if Parameter[3] <> DialHighpassSlope.Position
    then Parameter[3] := DialHighpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassSlopeMouseEnter(Sender: TObject);
begin
 UpdateHighpassSlope;
end;

procedure TFmLinkwitzRiley.LbMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FIsLow := Sender = LbLowpass;

 if Button = mbRight
  then PuPreset.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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

procedure TFmLinkwitzRiley.MiFrequencyClick(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 if assigned(FCurrentDial)
  then FCurrentDial.Position := TMenuItem(Sender).Tag;
end;

procedure TFmLinkwitzRiley.MiLoadClick(Sender: TObject);
begin
 with TDualButterworthFiltersModule(Owner), TComponent(Sender) do
  if FIsLow then LoadLow(Tag) else LoadHigh(Tag) 
end;

procedure TFmLinkwitzRiley.MiStoreClick(Sender: TObject);
begin
 with TDualButterworthFiltersModule(Owner), TComponent(Sender) do
  if FIsLow then StoreLow(Tag) else StoreHigh(Tag); 
end;

procedure TFmLinkwitzRiley.PuFrequencyPopup(Sender: TObject);
begin
 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.Mi31Hz5Click(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 if assigned(FCurrentDial)
  then FCurrentDial.Position := 31.5;
end;

procedure TFmLinkwitzRiley.UpdateLowpassFrequency;
begin
 with Owner as TDualButterworthFiltersModule do
  begin
   if DialLowpassFrequency.Position <> Parameter[0]
    then DialLowpassFrequency.Position := Parameter[0];
   LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[0] + ' ' + ParameterLabel[0];
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
   LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[2] + ' ' + ParameterLabel[2];
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
