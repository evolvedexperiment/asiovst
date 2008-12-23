unit AdvancedClipperGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  ExtCtrls, DAV_Common, DAV_VSTModule, DAV_GuiGroup, DAV_GuiPanel, DAV_GuiLabel,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLED;

type
  TFmAdvancedClipper = class(TForm)
    GpStage1: TGuiGroup;
    GpStage2: TGuiGroup;
    DialInputGain: TGuiDial;
    DialOSFactor1: TGuiDial;
    DialFilterOrder1: TGuiDial;
    LbInputGain: TGuiLabel;
    LbOSFactor: TGuiLabel;
    LbFilterOrder: TGuiLabel;
    PnDisplay: TGuiPanel;
    LbDisplay: TGuiLabel;
    DialOSFactor2: TGuiDial;
    LbOSFactor2: TGuiLabel;
    LbFilterOrder2: TGuiLabel;
    DialFilterOrder2: TGuiDial;
    DialOutputGain: TGuiDial;
    LbOutputGain: TGuiLabel;
    GuiPanel1: TGuiPanel;
    LbHardClip: TGuiLabel;
    LEDHardClip: TGuiLED;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialInputGainChange(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure DialOSFactor1Change(Sender: TObject);
    procedure DialFilterOrder1Change(Sender: TObject);
    procedure DialOSFactor2Change(Sender: TObject);
    procedure DialFilterOrder2Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbHardClipClick(Sender: TObject);
  private
    fBackgrounBitmap : TBitmap;
  public
    procedure UpdateInputGain;
    procedure UpdateOSFactor1;
    procedure UpdateOSFactor2;
    procedure UpdateOrder1;
    procedure UpdateOrder2;
    procedure UpdateOutputGain;
    procedure UpdateHardClip;
  end;

implementation

{$R *.DFM}

uses
  PNGImage, AdvancedClipperDM;

procedure TFmAdvancedClipper.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 fBackgrounBitmap := TBitmap.Create;
 with fBackgrounBitmap do
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
   DialInputGain.DialBitmap.Assign(PngBmp);
   DialOutputGain.DialBitmap.Assign(PngBmp);
   DialOSFactor1.DialBitmap.Assign(PngBmp);
   DialOSFactor2.DialBitmap.Assign(PngBmp);
   DialFilterOrder1.DialBitmap.Assign(PngBmp);
   DialFilterOrder2.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmAdvancedClipper.DialFilterOrder1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Filter Order'] := DialFilterOrder1.Position;
  end;
end;

procedure TFmAdvancedClipper.DialFilterOrder2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Filter Order'] := DialFilterOrder2.Position;
  end;
end;

procedure TFmAdvancedClipper.DialInputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Input Gain'] := DialInputGain.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOutputGainChange(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Output Gain'] := DialOutputGain.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor1Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 1: Oversampling Factor'] := DialOSFactor1.Position;
  end;
end;

procedure TFmAdvancedClipper.DialOSFactor2Change(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Stage 2: Oversampling Factor'] := DialOSFactor2.Position;
  end;
end;

procedure TFmAdvancedClipper.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, fBackgrounBitmap);
end;

procedure TFmAdvancedClipper.FormShow(Sender: TObject);
begin
 UpdateInputGain;
 UpdateOSFactor1;
 UpdateOSFactor2;
 UpdateOrder1;
 UpdateOrder2;
 UpdateOutputGain;
 UpdateHardClip;
 LbDisplay.Caption := 'Advanced Clipper';
end;

procedure TFmAdvancedClipper.LbHardClipClick(Sender: TObject);
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   ParameterByName['Hard Clip'] := 1 - ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.UpdateHardClip;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   LEDHardClip.Brightness_Percent := 10 + 80 * ParameterByName['Hard Clip'];
  end;
end;

procedure TFmAdvancedClipper.UpdateInputGain;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Input Gain'];
   if DialInputGain.Position <> Value
    then DialInputGain.Position := Value;
   LbDisplay.Caption := 'Input Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder1;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Filter Order'];
   if DialFilterOrder1.Position <> Value
    then DialFilterOrder1.Position := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOrder2;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Filter Order'];
   if DialFilterOrder2.Position <> Value
    then DialFilterOrder2.Position := Value;
   LbDisplay.Caption := 'Filter Order: ' + IntToStr(round(Value));
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor1;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 1: Oversampling Factor'];
   if DialOSFactor1.Position <> Value
    then DialOSFactor1.Position := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOSFactor2;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Stage 2: Oversampling Factor'];
   if DialOSFactor2.Position <> Value
    then DialOSFactor2.Position := Value;
   LbDisplay.Caption := 'Oversampling: ' + IntToStr(round(Value)) + 'x';
  end;
end;

procedure TFmAdvancedClipper.UpdateOutputGain;
var
  value : Single;
begin
 with Owner as TAdvancedClipperDataModule do
  begin
   Value := ParameterByName['Output Gain'];
   if DialOutputGain.Position <> Value
    then DialOutputGain.Position := Value;
   LbDisplay.Caption := 'Output Gain: ' + FloatToStrF(Value, ffGeneral, 2, 2) + 'dB';
  end;
end;

end.