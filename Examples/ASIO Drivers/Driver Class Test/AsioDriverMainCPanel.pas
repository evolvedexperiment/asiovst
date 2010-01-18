unit AsioDriverMainCPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_ASIODriver, DAV_ASIOExtendedDriver, StdCtrls;

type
  TDriverTestCP = class(TDavASIODriverCP)
    btnDone: TButton;
    lbStupid: TLabel;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

uses AsioDriverMain;

procedure TDriverTestCP.Button1Click(Sender: TObject);
begin
  TDriverTest(Driver).ASIORequestReset;
end;

procedure TDriverTestCP.FormShow(Sender: TObject);
begin
  Caption:=Driver.GetDriverName + ' (Version ' + inttostr(Driver.GetDriverVersion) + ')';
end;

end.
