unit DAV_VSTHostRegister;

interface

{$I ..\ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_VSTHost.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_VSTHost;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TVstHost]);
end;

{$IFDEF FPC}
  initialization
  {$i ..\..\Resources\VSTHost.lrs}
{$ENDIF}

end.
