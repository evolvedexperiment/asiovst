unit AsioDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, ComObj, DAV_ASIO, DAV_ASIOTCWrapper, DAV_ASIODriver;

const
  CClass_AsioDriver: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B47}';

type
  IDriverTest = interface(IDavASIODriverInterface)
    ['{A8DD45FD-34CC-4996-9695-CDD2AE483B47}']
  end;

  TDriverTest = TDavASIODriver;

  TTestTCWrapper = class(TDavASIOTCWrapper, IDriverTest)
  protected
    function GetDriverClass: TTDavASIODriver; override;
  end;


implementation

uses
  ComServ;

function TTestTCWrapper.GetDriverClass: TTDavASIODriver;
begin
  result:=TDriverTest;
end;

initialization
  TDavAsioDriverFactory.Create(ComServer, TTestTCWrapper, CClass_AsioDriver,
    'AsioDriver', 'DAV ASIO Driver', ciSingleInstance, tmApartment);

end.
