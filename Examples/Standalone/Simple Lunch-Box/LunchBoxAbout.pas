unit LunchBoxAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFmAbout = class(TForm)
    Label1: TLabel;
    Lb: TLabel;
    Label2: TLabel;
    procedure FormClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmAbout: TFmAbout;

implementation

{$R *.dfm}

procedure TFmAbout.FormClick(Sender: TObject);
begin
 Close;
end;

end.
