unit AbxAbout;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls;

type
  TFmAboutBox = class(TForm)
    OKButton: TButton;
    PnAbout: TPanel;
    ProductName: TLabel;
    ProgramIcon: TImage;
  end;

var
  FmAboutBox: TFmAboutBox;

implementation

{$R *.dfm}

end.
 
