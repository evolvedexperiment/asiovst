unit OversamplerTemplateGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_Common, DAV_GuiBaseControl, DAV_GuiButton, DAV_GuiLabel,
  DAV_GuiLED, DAV_GuiDial, DAV_GuiPanel, DAV_GuiSelectBox;

type
  TFmOversamplerter = class(TForm)
    DialOversampling: TGuiDial;
    GuiLEDOversampling: TGuiLED;
    PnControl: TGuiPanel;
    LbOversampling: TGuiLabel;
    LbOversamplingFactor: TGuiLabel;
    PnGui: TPanel;
    ShBorder: TShape;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure GuiLEDOversamplingClick(Sender: TObject);
  private
    fBackground : TBitmap;  
  public
    procedure UpdateOSFactor;
    procedure UpdateOverSampling;
    procedure ShowPlugin(Index: Integer);
  end;

implementation

{$R *.dfm}

uses
  OversamplerTemplateDM, DAV_VSTModuleWithPrograms;

procedure TFmOversamplerter.ShowPlugin(Index: Integer);
var
  R        : TRect;
  Oversize : Integer;
begin
 with TOversamplerTemplateDataModule(Owner) do
  begin
   if VstHost[Index].Active and not VstHost[Index].EditVisible
    then VstHost[Index].ShowEdit(PnGui);
   if VstHost[1 - Index].EditVisible then VstHost[1 - Index].CloseEdit;

   PnGui.Visible    := assigned(VstHost[Index]) and VstHost[Index].Active;
   ShBorder.Visible := PnGui.Visible;

   // set plugin GUI size
   if PnGui.Visible then
    begin
     PnGui.Visible    := True;
     ShBorder.Visible := True;

     R        := VstHost[Index].GetRect;
     Oversize := PnControl.Width - (R.Right - R.Left);
     if Oversize < 0 then
      begin
       // current editor is too small, enlarge!
       PnGui.Align := alClient;
       ShBorder.Visible := False;
      end
     else
      begin
       PnGui.Align  := alNone;
       PnGui.Left   := Oversize div 2;
       PnGui.Width  := (R.Right - R.Left);

       // calculate new height and y position
       PnGui.Height := (R.Bottom - R.Top);
       PnGui.Top    := PnControl.Height + (ClientHeight - PnControl.Height - PnGui.Height) div 2;

       // show border
       ShBorder.Visible := True;
       ShBorder.SetBounds(PnGui.Left - ShBorder.Pen.Width,
                          PnGui.Top - ShBorder.Pen.Width,
                          PnGui.Width + 2 * ShBorder.Pen.Width,
                          PnGui.Height + 2 * ShBorder.Pen.Width);
      end;
    end;
  end;
end;

procedure TFmOversamplerter.DialOversamplingChange(Sender: TObject);
begin
 with TOversamplerTemplateDataModule(Owner) do
  begin
   ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmOversamplerter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TOversamplerTemplateDataModule(Owner) do
  try
   if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   if VstHost[1].EditVisible then VstHost[1].CloseEdit;
  except 
  end;
end;

procedure TFmOversamplerter.FormDestroy(Sender: TObject);
begin
 if assigned(fBackground)
  then FreeAndNil(fBackground); 
end;

procedure TFmOversamplerter.FormPaint(Sender: TObject);
begin
 if assigned(fBackground)
  then Canvas.Draw(0, PnControl.Height, fBackground);
end;

procedure Lighten(var Pixel: TRGB24; Amount: Byte);
begin
 Pixel.B := round($2B - Amount);
 Pixel.G := round($31 - Amount);
 Pixel.R := round($33 - Amount);
end;

procedure TFmOversamplerter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
  b      : Byte;
begin
 // Create Background Image (if not already done
 if not assigned(fBackground)
  then fBackground := TBitmap.Create;
 with fBackground do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height - PnControl.Height;
   if Height < 2 then exit;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;

   // start with separator
   Line := Scanline[0];
   for x := 0 to Width - 1 do
    begin
     s[1] := 0.97 * s[0] + 0.03 * random;
     s[0] := s[1];
     b    := round(-$2A * s[1]);
     Line[x].B := $2B + b;
     Line[x].G := $31 + b;
     Line[x].R := $33 + b;
    end;

   for y := 1 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];
       b    := round($2A * (s[1] + h));
       Line[x].B := $2B + b;
       Line[x].G := $31 + b;
       Line[x].R := $33 + b;
      end;
    end;
  end;
end;

procedure TFmOversamplerter.FormShow(Sender: TObject);
begin
 UpdateOSFactor;
 ShowPlugin(0);
end;

procedure TFmOversamplerter.GuiLEDOversamplingClick(Sender: TObject);
begin
 with TOversamplerTemplateDataModule(Owner) do
  begin
   ParameterByName['Oversampling'] := f_Limit(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmOversamplerter.UpdateOverSampling;
begin
 with TOversamplerTemplateDataModule(Owner) do
  begin
   GuiLEDOversampling.Brightness_Percent := 20 + 60 * (f_Limit(ParameterByName['Oversampling'], 0, 1));

   DialOversampling.Visible     := round(ParameterByName['Oversampling']) = 1;
   LbOversamplingFactor.Visible := DialOversampling.Visible;
   LbOversampling.Width         := 97 + 5 * round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmOversamplerter.UpdateOSFactor;
var
  OSFactor : Single;
begin
 with TOversamplerTemplateDataModule(Owner) do
  begin
   OSFactor := ParameterByName['OS Factor'];
   if DialOversampling.Position <> OSFactor
    then DialOversampling.Position := OSFactor;
   LbOversamplingFactor.Caption := IntToStr(round(OSFactor)) + 'x';
  end;
end;

end.
