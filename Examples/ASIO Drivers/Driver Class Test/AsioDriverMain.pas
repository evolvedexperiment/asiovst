unit AsioDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, ComObj, DAV_ASIO, DAV_ASIOExtendedDriver, DAV_ASIODriver;

const
  DTest_guid: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B47}';
  DTest_classname = 'DAVTestDriver';
  DTest_name = 'DAV Test Driver';

type
  IDriverTest = interface(IDavASIODriverInterface)
    ['{A8DD45FD-34CC-4996-9695-CDD2AE483B47}']
  end;

  TDriverTest = class(TDavASIOExtendedDriver)
  protected
    procedure InitializeDriverParams; override;
  end;

  TTestTCWrapper = class(TDavASIOTCWrapper, IDriverTest)
  protected
    function GetDriverClass: TTDavASIODriver; override;
  end;


implementation

uses
  ComServ;

function TTestTCWrapper.GetDriverClass: TTDavASIODriver;
begin
  result := TDriverTest;
end;

{ TDriverTest }

procedure TDriverTest.InitializeDriverParams;
begin
  SetDriverName(DTest_name);
  SetDriverVersion(1);
//  AddClock('Default Clock',0);
end;

initialization
  TDavAsioDriverFactory.Create(ComServer, TTestTCWrapper, DTest_guid,
    DTest_classname, DTest_name, ciSingleInstance, tmApartment);

end.
