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
    procedure FormCreate(Sender: TObject);
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
  Interceptor.DriverControlPanel;
end;

procedure TInterceptorTestCP.cbDriversChange(Sender: TObject);
begin
  Interceptor.DriverIndex := cbDrivers.ItemIndex;
end;

procedure TInterceptorTestCP.FormCreate(Sender: TObject);
begin
  Caption:=Interceptor.GetDriverName + ' (Version ' + inttostr(Interceptor.GetDriverVersion) + ')';
  cbDrivers.Items:=Interceptor.DriverNames;
end;

procedure TInterceptorTestCP.FormShow(Sender: TObject);
begin
  cbDrivers.ItemIndex := Interceptor.DriverIndex;
end;

end.
