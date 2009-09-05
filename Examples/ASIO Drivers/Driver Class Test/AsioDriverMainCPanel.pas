unit AsioDriverMainCPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_ASIODriver, DAV_ASIOExtendedDriver, StdCtrls;

type
  TDriverTestCP = class(TDavASIODriverCP)
    btnDone: TButton;
    lbStupid: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TDriverTestCP.FormCreate(Sender: TObject);
begin
  Caption:=Driver.GetDriverName + ' (Version ' + inttostr(Driver.GetDriverVersion) + ')';
end;

end.
