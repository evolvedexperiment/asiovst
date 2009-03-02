unit OversampleTemplateGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_Common, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiLED,
  DAV_GuiDial, DAV_GuiPanel, DAV_VSTWindowSizer, Menus;

type
  TFmOversampler = class(TForm)
    DialOversampling: TGuiDial;
    GuiLEDOversampling: TGuiLED;
    PnControl: TGuiPanel;
    LbOversampling: TGuiLabel;
    LbOversamplingFactor: TGuiLabel;
    PnGui: TPanel;
    ShBorder: TShape;
    PUSettings: TPopupMenu;
    MIAllowResizing: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure GuiLEDOversamplingClick(Sender: TObject);
    procedure MIAllowResizingClick(Sender: TObject);
  private
    fBackground : TBitmap;
    fWinSizer   : TVstWindowSizer;
  public
    procedure UpdateOSFactor;
    procedure UpdateOverSampling;
    procedure ShowPlugin;
  end;

implementation

{$R *.dfm}

uses
  OversampleTemplateDM, DAV_GuiCommon, DAV_VSTModuleWithPrograms;

procedure TFmOversampler.ShowPlugin;
var
  R        : TRect;
  Oversize : Integer;
begin
 with TOversampleTemplateDataModule(Owner) do
  begin
   if VstHost[0].Active and not VstHost[0].EditVisible
    then VstHost[0].ShowEdit(PnGui);

   PnGui.Visible    := assigned(VstHost[0]) and VstHost[0].Active;
   ShBorder.Visible := PnGui.Visible;

   // set plugin GUI size
   if PnGui.Visible then
    begin
     PnGui.Visible    := True;
     ShBorder.Visible := True;

     R        := VstHost[0].GetRect;
     Oversize := PnControl.Width - (R.Right - R.Left);
     if Oversize <= 0 then
      begin
       // current editor is too small, enlarge!
       PnGui.Align := alClient;
       ShBorder.Visible := False;
       MIAllowResizing.Enabled := False;
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
       MIAllowResizing.Enabled := True;
      end;
    end;
  end;
end;

procedure TFmOversampler.DialOversamplingChange(Sender: TObject);
begin
 with TOversampleTemplateDataModule(Owner) do
  begin
   ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmOversampler.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TOversampleTemplateDataModule(Owner) do
  try
   if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   if VstHost[1].EditVisible then VstHost[1].CloseEdit;
   if assigned(fWinSizer)
    then FreeAndNil(fWinSizer);
   MIAllowResizing.Checked := False; 
  except
  end;
end;

procedure TFmOversampler.FormDestroy(Sender: TObject);
begin
 if assigned(fBackground)
  then FreeAndNil(fBackground);
 if assigned(fWinSizer)
  then FreeAndNil(fWinSizer);
end;

procedure TFmOversampler.FormPaint(Sender: TObject);
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

procedure TFmOversampler.FormResize(Sender: TObject);
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

procedure TFmOversampler.FormShow(Sender: TObject);
begin
 UpdateOSFactor;
 UpdateOverSampling;
 ShowPlugin;
end;

procedure TFmOversampler.GuiLEDOversamplingClick(Sender: TObject);
begin
 with TOversampleTemplateDataModule(Owner) do
  begin
   ParameterByName['Oversampling'] := Limit(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmOversampler.MIAllowResizingClick(Sender: TObject);
begin
 MIAllowResizing.Checked := not MIAllowResizing.Checked;
 if MIAllowResizing.Checked then
  begin
   if not assigned(fWinSizer)
    then fWinSizer := TVstWindowSizer.Create;
   fWinSizer.Effect := TOversampleTemplateDataModule(Owner);
   fWinSizer.SetEditorHwnd(Self.Handle);
  end
 else
  begin
   if assigned(fWinSizer)
    then FreeAndNil(fWinSizer);
  end;
end;

procedure TFmOversampler.UpdateOverSampling;
begin
 with TOversampleTemplateDataModule(Owner) do
  begin
   GuiLEDOversampling.Brightness_Percent := 20 + 60 * (Limit(ParameterByName['Oversampling'], 0, 1));

   DialOversampling.Visible     := round(ParameterByName['Oversampling']) = 1;
   LbOversamplingFactor.Visible := DialOversampling.Visible;
   LbOversampling.Width         := 80 + 5 * round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmOversampler.UpdateOSFactor;
var
  OSFactor : Single;
begin
 with TOversampleTemplateDataModule(Owner) do
  begin
   OSFactor := ParameterByName['OS Factor'];
   if DialOversampling.Position <> OSFactor
    then DialOversampling.Position := OSFactor;
   LbOversamplingFactor.Caption := IntToStr(round(OSFactor)) + 'x';
  end;
end;

end.
