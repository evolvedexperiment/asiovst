unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_StitchedButton;

type
  TFmStitchedButtonTest = class(TForm)
    StitchedButton: TGuiStichedButton;
    GuiStitchedPNGList: TGuiStitchedPNGList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmStitchedButtonTest: TFmStitchedButtonTest;

implementation

{$R *.dfm}

end.

