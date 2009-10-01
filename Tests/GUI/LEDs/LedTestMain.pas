unit LedTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, DAV_GuiBaseControl, DAV_GuiLED;

type
  TFmLEDTest = class(TForm)
    LED1: TGuiLED;
    LED2: TGuiLED;
    LED3: TGuiLED;
    LED4: TGuiLED;
    LbUniformiy: TLabel;
    TbUniformity: TTrackBar;
    LbBrightness: TLabel;
    TbBrightness: TTrackBar;
    LbLineWidth: TLabel;
    TbLineWidth: TTrackBar;
    procedure TbUniformityChange(Sender: TObject);
    procedure TbBrightnessChange(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
  end;

var
  FmLEDTest: TFmLEDTest;

implementation

{$R *.dfm}

procedure TFmLEDTest.TbBrightnessChange(Sender: TObject);
begin
 LED1.Brightness_Percent := TbBrightness.Position;
 LED2.Brightness_Percent := TbBrightness.Position;
 LED3.Brightness_Percent := TbBrightness.Position;
 LED4.Brightness_Percent := TbBrightness.Position;
end;

procedure TFmLEDTest.TbLineWidthChange(Sender: TObject);
begin
 LED1.LineWidth := TbLineWidth.Position;
 LED2.LineWidth := TbLineWidth.Position;
 LED3.LineWidth := TbLineWidth.Position;
 LED4.LineWidth := TbLineWidth.Position;
end;

procedure TFmLEDTest.TbUniformityChange(Sender: TObject);
begin
 LED1.Uniformity_Percent := TbUniformity.Position;
 LED2.Uniformity_Percent := TbUniformity.Position;
 LED3.Uniformity_Percent := TbUniformity.Position;
 LED4.Uniformity_Percent := TbUniformity.Position;
end;

end.
