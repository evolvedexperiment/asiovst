unit DAV_SEHostRegister;

interface

{$I ..\DAV_Compiler.inc}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_SeHostRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_SeHost;

procedure Register;
begin
 RegisterComponents('ASIO/VST Modular', [TSEHost]);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_SeHostRegister.lrs}
{$ENDIF}

end.
