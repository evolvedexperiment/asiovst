unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_StitchedButton;

type
  TFmStitchedButtonTest = class(TForm)
    StitchedButtonA: TGuiStichedButton;
    StitchedButtonB: TGuiStichedButton;
    GuiStitchedPNGList: TGuiStitchedPNGList;
    procedure StitchedButtonBClick(Sender: TObject);
    procedure StitchedButtonAClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmStitchedButtonTest: TFmStitchedButtonTest;

implementation

{$R *.dfm}

procedure TFmStitchedButtonTest.StitchedButtonAClick(Sender: TObject);
begin
 StitchedButtonB.Enabled := not StitchedButtonB.Enabled;
end;

procedure TFmStitchedButtonTest.StitchedButtonBClick(Sender: TObject);
begin
 StitchedButtonA.Enabled := not StitchedButtonA.Enabled;
end;

end.
