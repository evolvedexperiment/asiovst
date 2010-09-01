unit SliderTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiSlider;

type
  TFmSliderTest = class(TForm)
    GuiEQSlide1: TGuiSlider;
    GuiEQSlide2: TGuiSlider;
    GuiEQSlide3: TGuiSlider;
    GuiEQSlide4: TGuiSlider;
  public
    { Public-Deklarationen }
  end;

var
  FmSliderTest: TFmSliderTest;

implementation

{$R *.dfm}

uses
  DAV_Common, DAV_GuiCommon;

end.
