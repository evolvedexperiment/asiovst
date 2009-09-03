unit DrvrHostIntCPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_ASIODriverInterceptor, StdCtrls;

type
  TInterceptorTestCP = class(TDavASIOInterceptorCP)
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

procedure TInterceptorTestCP.FormCreate(Sender: TObject);
begin
  Caption:=Interceptor.GetDriverName + ' (Version ' + inttostr(Interceptor.GetDriverVersion) + ')';
end;

end.
