unit DrvrHostIntCPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_ASIODriverInterceptor, StdCtrls;

type
  TInterceptorTestCP = class(TDavASIOInterceptorCP)
    btnDone: TButton;
    cbDrivers: TComboBox;
    btnControlPanel: TButton;
    procedure cbDriversChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnControlPanelClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TInterceptorTestCP.btnControlPanelClick(Sender: TObject);
begin
  if assigned(Interceptor) then Interceptor.DriverControlPanel;
end;

procedure TInterceptorTestCP.cbDriversChange(Sender: TObject);
begin
  Interceptor.DriverIndex := cbDrivers.ItemIndex;
end;

procedure TInterceptorTestCP.FormShow(Sender: TObject);
begin
  if not assigned(Interceptor) then exit;
  Caption:=Interceptor.GetDriverName + ' (Version ' + inttostr(Interceptor.GetDriverVersion) + ')';
  cbDrivers.Items:=Interceptor.DriverNames;

  cbDrivers.ItemIndex := Interceptor.DriverIndex;
end;

end.
