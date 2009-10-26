unit ParametriQLiteGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, Graphics, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, ExtCtrls,
  DAV_GuiVUMeter, Menus;

type
  TFmParametriQLite = class(TForm)
    Box1: TShape;
    Box2: TShape;
    Box3: TShape;
    Box4: TShape;
    Box5: TShape;
    Box6: TShape;
    Box7: TShape;
    Box8: TShape;
    DialBW1: TGuiDial;
    DialBW2: TGuiDial;
    DialBW3: TGuiDial;
    DialBW4: TGuiDial;
    DialBW5: TGuiDial;
    DialBW6: TGuiDial;
    DialBW7: TGuiDial;
    DialBW8: TGuiDial;
    DialFreq1: TGuiDial;
    DialFreq2: TGuiDial;
    DialFreq3: TGuiDial;
    DialFreq4: TGuiDial;
    DialFreq5: TGuiDial;
    DialFreq6: TGuiDial;
    DialFreq7: TGuiDial;
    DialFreq8: TGuiDial;
    DialGain1: TGuiDial;
    DialGain2: TGuiDial;
    DialGain3: TGuiDial;
    DialGain4: TGuiDial;
    DialGain5: TGuiDial;
    DialGain6: TGuiDial;
    DialGain7: TGuiDial;
    DialGain8: TGuiDial;
    DialInput: TGuiDial;
    DialOutput: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    GuiMax1: TGuiLabel;
    GuiMax2: TGuiLabel;
    GuiMax3: TGuiLabel;
    GuiMax4: TGuiLabel;
    GuiMax5: TGuiLabel;
    GuiMax6: TGuiLabel;
    GuiMax7: TGuiLabel;
    GuiMax8: TGuiLabel;
    GuiMin1: TGuiLabel;
    GuiMin2: TGuiLabel;
    GuiMin3: TGuiLabel;
    GuiMin4: TGuiLabel;
    GuiMin5: TGuiLabel;
    GuiMin6: TGuiLabel;
    GuiMin7: TGuiLabel;
    GuiMin8: TGuiLabel;
    LbBW1: TGuiLabel;
    LbBW2: TGuiLabel;
    LbBW3: TGuiLabel;
    LbBW4: TGuiLabel;
    LbBW5: TGuiLabel;
    LbBW6: TGuiLabel;
    LbBW7: TGuiLabel;
    LbBW8: TGuiLabel;
    LbdB: TGuiLabel;
    LbFreq1: TGuiLabel;
    LbFreq2: TGuiLabel;
    LbFreq3: TGuiLabel;
    LbFreq4: TGuiLabel;
    LbFreq5: TGuiLabel;
    LbFreq6: TGuiLabel;
    LbFreq7: TGuiLabel;
    LbFreq8: TGuiLabel;
    LbFreqValue1: TGuiLabel;
    LbFreqValue2: TGuiLabel;
    LbFreqValue3: TGuiLabel;
    LbFreqValue4: TGuiLabel;
    LbFreqValue5: TGuiLabel;
    LbFreqValue6: TGuiLabel;
    LbFreqValue7: TGuiLabel;
    LbFreqValue8: TGuiLabel;
    LbGain: TGuiLabel;
    LbGain1: TGuiLabel;
    LbGain2: TGuiLabel;
    LbGain3: TGuiLabel;
    LbGain4: TGuiLabel;
    LbGain5: TGuiLabel;
    LbGain6: TGuiLabel;
    LbGain7: TGuiLabel;
    LbGain8: TGuiLabel;
    LbIn: TGuiLabel;
    LbInput: TGuiLabel;
    LbOut: TGuiLabel;
    LbOutput: TGuiLabel;
    LbTitle: TGuiLabel;
    LbType1: TGuiLabel;
    LbType2: TGuiLabel;
    LbType3: TGuiLabel;
    LbType4: TGuiLabel;
    LbType5: TGuiLabel;
    LbType6: TGuiLabel;
    LbType7: TGuiLabel;
    LbType8: TGuiLabel;
    LbTypeValue1: TGuiLabel;
    LbTypeValue2: TGuiLabel;
    LbTypeValue3: TGuiLabel;
    LbTypeValue4: TGuiLabel;
    LbTypeValue5: TGuiLabel;
    LbTypeValue6: TGuiLabel;
    LbTypeValue7: TGuiLabel;
    LbTypeValue8: TGuiLabel;
    SeparatorA1: TShape;
    SeparatorA2: TShape;
    SeparatorA3: TShape;
    SeparatorA4: TShape;
    SeparatorA5: TShape;
    SeparatorA6: TShape;
    SeparatorA7: TShape;
    SeparatorA8: TShape;
    SeparatorB1: TShape;
    SeparatorB2: TShape;
    SeparatorB3: TShape;
    SeparatorB4: TShape;
    SeparatorB5: TShape;
    SeparatorB6: TShape;
    SeparatorB7: TShape;
    SeparatorB8: TShape;
    SeparatorC1: TShape;
    SeparatorC2: TShape;
    SeparatorC3: TShape;
    SeparatorC4: TShape;
    SeparatorC5: TShape;
    SeparatorC6: TShape;
    SeparatorC7: TShape;
    SeparatorC8: TShape;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    ShapeInfo: TShape;
    ShapeInputBottom: TShape;
    ShapeInputLeft: TShape;
    ShapeOutputBottom: TShape;
    ShapeOutputRight: TShape;
    Switch: TGuiSwitch;
    VUMeter: TGuiVUMeter;
    PopupFilter: TPopupMenu;
    MIBypass: TMenuItem;
    MIPeak: TMenuItem;
    MILowshelf: TMenuItem;
    MIHighshelf: TMenuItem;
    MILowpass: TMenuItem;
    MIHighpass: TMenuItem;
    MIAllpass: TMenuItem;
    MINotch: TMenuItem;
    MIBandpass: TMenuItem;
    N1: TMenuItem;
    Timer: TTimer;
    PlotBox: TPaintBox;
    MILowShelfA: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure DialBWChange(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialGainChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure LbTypeClick(Sender: TObject);
    procedure MIAllpassClick(Sender: TObject);
    procedure MIBandpassClick(Sender: TObject);
    procedure MIBypassClick(Sender: TObject);
    procedure MIHighpassClick(Sender: TObject);
    procedure MIHighshelfClick(Sender: TObject);
    procedure MILowpassClick(Sender: TObject);
    procedure MILowshelfClick(Sender: TObject);
    procedure MINotchClick(Sender: TObject);
    procedure MIPeakClick(Sender: TObject);
    procedure PopupFilterPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure PlotBoxPaint(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MILowShelfAClick(Sender: TObject);
  private
    FBackgroundBitmap : TBitmap;
  public
    procedure UpdateGain(const Index: Integer);
    procedure UpdateBandwidth(const Index: Integer);
    procedure UpdateFrequency(const Index: Integer);
    procedure UpdateFilterType(const Index: Integer);
  end;

implementation

uses
  PngImage, ParametriQLiteDM, DAV_Common, DAV_GuiCommon, DAV_Approximations,
  DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmParametriQLite.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  rct    : TRect;
  Line   : PRGB24Array;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'Knob1', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.Brush.Color := Color;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialFreq1.DialImageIndex := 0;
   DialFreq2.DialImageIndex := 0;
   DialFreq3.DialImageIndex := 0;
   DialFreq4.DialImageIndex := 0;
   DialFreq5.DialImageIndex := 0;
   DialFreq6.DialImageIndex := 0;
   DialFreq7.DialImageIndex := 0;
   DialFreq8.DialImageIndex := 0;
   DialBW1.DialImageIndex   := 0;
   DialBW2.DialImageIndex   := 0;
   DialBW3.DialImageIndex   := 0;
   DialBW4.DialImageIndex   := 0;
   DialBW5.DialImageIndex   := 0;
   DialBW6.DialImageIndex   := 0;
   DialBW7.DialImageIndex   := 0;
   DialBW8.DialImageIndex   := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'Knob2', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[1].DialBitmap do
    begin
     Canvas.Brush.Color := Color;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialGain1.DialImageIndex := 1;
   DialGain2.DialImageIndex := 1;
   DialGain3.DialImageIndex := 1;
   DialGain4.DialImageIndex := 1;
   DialGain5.DialImageIndex := 1;
   DialGain6.DialImageIndex := 1;
   DialGain7.DialImageIndex := 1;
   DialGain8.DialImageIndex := 1;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'Knob3', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[2].DialBitmap do
    begin
     Canvas.Brush.Color := Color;
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialInput.DialImageIndex  := 2;
   DialOutput.DialImageIndex := 2;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

 PngBmp := TPngObject.Create;
 try
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

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'Switch', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   Switch.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

 // Create Background Image
 FBackgroundBitmap := TBitmap.Create;
 with FBackgroundBitmap do
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
       s[1] := 0.9 * s[0] + 0.1 * random;
       b := round($4 * s[1]) - $2;
       s[0] := s[1];
       Line[x].B := $2F + b;
       Line[x].G := $30 + b;
       Line[x].R := $2E + b;
      end;
    end;
   rct := Rect(8, 8, 221, 190);
   Canvas.Pen.Color := $00424341;
   Canvas.Brush.Color := $00161715;
   Canvas.Rectangle(rct);
  end;

 PlotBox.ControlStyle   := PlotBox.ControlStyle + [csOpaque];
 ShapeInfo.ControlStyle := ShapeInfo.ControlStyle + [csOpaque];
end;

procedure TFmParametriQLite.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgroundBitmap);
end;

procedure TFmParametriQLite.FormShow(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to 7 do
  begin
   UpdateGain(Band);
   UpdateBandwidth(Band);
   UpdateFrequency(Band);
   UpdateFilterType(Band);
  end;
end;

procedure TFmParametriQLite.LbTypeClick(Sender: TObject);
begin
 assert(Sender is TGuiLabel);
 PopupFilter.Tag := (TGuiLabel(Sender).Tag - 1);
 PopupFilter.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFmParametriQLite.MIBypassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 0;
  end;
end;

procedure TFmParametriQLite.MIPeakClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 1;
  end;
end;

procedure TFmParametriQLite.MILowshelfClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 2;
  end;
end;

procedure TFmParametriQLite.MIHighshelfClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 3;
  end;
end;

procedure TFmParametriQLite.MILowpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 4;
  end;
end;

procedure TFmParametriQLite.MILowShelfAClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 9;
  end;
end;

procedure TFmParametriQLite.MIHighpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 5;
  end;
end;

procedure TFmParametriQLite.MIBandpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 6;
  end;
end;

procedure TFmParametriQLite.MINotchClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 7;
  end;
end;

procedure TFmParametriQLite.MIAllpassClick(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[PopupFilter.Tag * 4 + 4] := 8;
  end;
end;

function FastFreqLogToLinear(Value: Single): Single;
const
  fltl1 : Single = 0.05;
  fltl2 : Single = 0.1;
begin
 Result := FastLog2MinError3(value * fltl1) * fltl2;
end;

function FastFreqLinearToLog(Value: Single): Single;
const
  fltl1 : Single = 9.9657840729;
begin
 Result := (CTwenty32 * FastPower2MinError3 (value * fltl1));
end;

procedure TFmParametriQLite.PlotBoxPaint(Sender: TObject);
var
  R         : TRect;
  HalfHght  : Integer;
  c, Wdth   : Integer;
  WdthRez   : Single;
  Magn, Frq : Single;
  Band      : Integer;
const
  CdBFactor : Single = 0.2006829232;
begin
 with TParametriQLiteDataModule(Owner), PlotBox.Canvas do
  begin
   Lock;
   R := PlotBox.ClientRect;
   Pen.Color := $00424341;
   RoundRect(R.Left, R.Top, R.Right, R.Bottom, 2, 2);
   InflateRect(R, -1, -1);
   Brush.Color := $00232422;
   FillRect(R);
   Wdth := R.Right - R.Left + 1;
   WdthRez := 1 / Wdth;
   HalfHght := (R.Bottom - R.Top) div 2;

   // draw 100 Hz
   Band := round(FastFreqLogToLinear(100) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw 1 kHz
   Band := round(FastFreqLogToLinear(1E3) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw 10 kHz
   Band := round(FastFreqLogToLinear(1E4) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw middle line
   MoveTo(1, HalfHght);
   LineTo(Wdth, HalfHght);

   Pen.Color := $00666765;
   Magn := Filter[0].MagnitudeSquared(20);
   for Band := 1 to 7
    do Magn := Magn * Filter[Band].MagnitudeSquared(20);
   MoveTo(1, round(HalfHght * (1 - FastLog2MinError5(Magn) * CdBFactor)));
   for c := 2 to Wdth do
    begin
     Frq := FastFreqLinearToLog(c * WdthRez);
     Magn := Filter[0].MagnitudeSquared(Frq);
     for Band := 1 to 7
      do Magn := Magn * Filter[Band].MagnitudeSquared(Frq);
     LineTo(c, round(HalfHght * (1 - FastLog2MinError5(Magn) * CdBFactor )));
    end;
   Unlock; 
  end;
end;

procedure TFmParametriQLite.PopupFilterPopup(Sender: TObject);
var
  FilterType: Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   FilterType          := round(Parameter[PopupFilter.Tag * 4 + 4]);
   MIBypass.Checked    := FilterType = 0;
   MIPeak.Checked      := FilterType = 1;
   MILowshelf.Checked  := FilterType = 2;
   MIHighshelf.Checked := FilterType = 3;
   MILowpass.Checked   := FilterType = 4;
   MIHighpass.Checked  := FilterType = 5;
   MIBandpass.Checked  := FilterType = 6;
   MINotch.Checked     := FilterType = 7;
   MIAllpass.Checked   := FilterType = 8;
  end;
end;

procedure TFmParametriQLite.TimerTimer(Sender: TObject);
var
  PeakLevel : Single;
const
  COne25th : Single = 1 / 25;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   if Switch.GlyphNr = 0
    then PeakLevel := InputPeakLevel
    else PeakLevel := OutputPeakLevel;

   VUMeter.GlyphIndex := round((25 + Limit(PeakLevel, -25, 0)) * VUMeter.NumGlyphs * COne25th);
  end;
end;

procedure TFmParametriQLite.UpdateFrequency(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialFreq1.Position <> Parameter[Index * 4 + 1]
          then DialFreq1.Position := Parameter[Index * 4 + 1];
//         LbFreq1.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    1 : begin
         if DialFreq2.Position <> Parameter[Index * 4 + 1]
          then DialFreq2.Position := Parameter[Index * 4 + 1];
//         LbFreq2.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    2 : begin
         if DialFreq3.Position <> Parameter[Index * 4 + 1]
          then DialFreq3.Position := Parameter[Index * 4 + 1];
//         LbFreq3.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    3 : begin
         if DialFreq4.Position <> Parameter[Index * 4 + 1]
          then DialFreq4.Position := Parameter[Index * 4 + 1];
//         LbFreq4.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    4 : begin
         if DialFreq5.Position <> Parameter[Index * 4 + 1]
          then DialFreq5.Position := Parameter[Index * 4 + 1];
//         LbFreq5.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    5 : begin
         if DialFreq6.Position <> Parameter[Index * 4 + 1]
          then DialFreq6.Position := Parameter[Index * 4 + 1];
//         LbFreq6.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    6 : begin
         if DialFreq7.Position <> Parameter[Index * 4 + 1]
          then DialFreq7.Position := Parameter[Index * 4 + 1];
//         LbFreq7.Caption := ParameterDisplay[Index * 4 + 1];
        end;
    7 : begin
         if DialFreq8.Position <> Parameter[Index * 4 + 1]
          then DialFreq8.Position := Parameter[Index * 4 + 1];
//         LbFreq8.Caption := ParameterDisplay[Index * 4 + 1];
        end;
   end;
  end;
 PlotBox.Invalidate;
end;

procedure TFmParametriQLite.UpdateBandwidth(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialBW1.Position <> Parameter[Index * 4 + 2]
          then DialBW1.Position := Parameter[Index * 4 + 2];
//         LbBW1.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    1 : begin
         if DialBW2.Position <> Parameter[Index * 4 + 2]
          then DialBW2.Position := Parameter[Index * 4 + 2];
//         LbBW2.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    2 : begin
         if DialBW3.Position <> Parameter[Index * 4 + 2]
          then DialBW3.Position := Parameter[Index * 4 + 2];
//         LbBW3.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    3 : begin
         if DialBW4.Position <> Parameter[Index * 4 + 2]
          then DialBW4.Position := Parameter[Index * 4 + 2];
//         LbBW4.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    4 : begin
         if DialBW5.Position <> Parameter[Index * 4 + 2]
          then DialBW5.Position := Parameter[Index * 4 + 2];
//         LbBW5.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    5 : begin
         if DialBW6.Position <> Parameter[Index * 4 + 2]
          then DialBW6.Position := Parameter[Index * 4 + 2];
//         LbBW6.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    6 : begin
         if DialBW7.Position <> Parameter[Index * 4 + 2]
          then DialBW7.Position := Parameter[Index * 4 + 2];
//         LbBW7.Caption := ParameterDisplay[Index * 4 + 2];
        end;
    7 : begin
         if DialBW8.Position <> Parameter[Index * 4 + 2]
          then DialBW8.Position := Parameter[Index * 4 + 2];
//         LbBW8.Caption := ParameterDisplay[Index * 4 + 2];
        end;
   end;
  end;
 PlotBox.Invalidate;
end;

procedure TFmParametriQLite.UpdateGain(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : begin
         if DialGain1.Position <> Parameter[Index * 4 + 3]
          then DialGain1.Position := Parameter[Index * 4 + 3];
//         LbGain1.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    1 : begin
         if DialGain2.Position <> Parameter[Index * 4 + 3]
          then DialGain2.Position := Parameter[Index * 4 + 3];
//         LbGain2.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    2 : begin
         if DialGain3.Position <> Parameter[Index * 4 + 3]
          then DialGain3.Position := Parameter[Index * 4 + 3];
//         LbGain3.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    3 : begin
         if DialGain4.Position <> Parameter[Index * 4 + 3]
          then DialGain4.Position := Parameter[Index * 4 + 3];
//         LbGain4.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    4 : begin
         if DialGain5.Position <> Parameter[Index * 4 + 3]
          then DialGain5.Position := Parameter[Index * 4 + 3];
//         LbGain5.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    5 : begin
         if DialGain6.Position <> Parameter[Index * 4 + 3]
          then DialGain6.Position := Parameter[Index * 4 + 3];
//         LbGain6.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    6 : begin
         if DialGain7.Position <> Parameter[Index * 4 + 3]
          then DialGain7.Position := Parameter[Index * 4 + 3];
//         LbGain7.Caption := ParameterDisplay[Index * 4 + 3];
        end;
    7 : begin
         if DialGain8.Position <> Parameter[Index * 4 + 3]
          then DialGain8.Position := Parameter[Index * 4 + 3];
//         LbGain8.Caption := ParameterDisplay[Index * 4 + 3];
        end;
   end;
  end;
 PlotBox.Invalidate;
end;

procedure TFmParametriQLite.UpdateFilterType(const Index: Integer);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   case Index of
    0 : LbTypeValue1.Caption := ParameterDisplay[Index * 4 + 4];
    1 : LbTypeValue2.Caption := ParameterDisplay[Index * 4 + 4];
    2 : LbTypeValue3.Caption := ParameterDisplay[Index * 4 + 4];
    3 : LbTypeValue4.Caption := ParameterDisplay[Index * 4 + 4];
    4 : LbTypeValue5.Caption := ParameterDisplay[Index * 4 + 4];
    5 : LbTypeValue6.Caption := ParameterDisplay[Index * 4 + 4];
    6 : LbTypeValue7.Caption := ParameterDisplay[Index * 4 + 4];
    7 : LbTypeValue8.Caption := ParameterDisplay[Index * 4 + 4];
   end;
  end;
 PlotBox.Invalidate;
end;

procedure TFmParametriQLite.DialFreqChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiDial(Sender).Tag - 1);
   Parameter[Band * 4 + 1] := TGuiDial(Sender).Position;
  end;
end;

procedure TFmParametriQLite.DialBWChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiDial(Sender).Tag - 1);
   Parameter[Band * 4 + 2] := TGuiDial(Sender).Position;
  end;
end;

procedure TFmParametriQLite.DialGainChange(Sender: TObject);
var
  Band : Integer;
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Band := (TGuiDial(Sender).Tag - 1);
   Parameter[Band * 4 + 3] := TGuiDial(Sender).Position;
  end;
end;

procedure TFmParametriQLite.DialInputChange(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[0] := DialInput.Position;
  end;
end;

procedure TFmParametriQLite.DialOutputChange(Sender: TObject);
begin
 with TParametriQLiteDataModule(Owner) do
  begin
   Parameter[numParams - 1] := DialOutput.Position;
  end;
end;

end.
