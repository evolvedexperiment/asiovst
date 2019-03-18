library SEPascalScript;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEPascalScriptModule in 'SEPascalScriptModule.pas';

{$E sem}
{$R *.res}

function GetModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := False;
  if (Index = 0) then
  begin
    TSEPascalScriptModule.GetModuleProperties(Properties);
    Result := True;
  end;
end;

function MakeModule(Index, ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
  Result := nil;
  if (Index = 0) and (ProcessType = 1) then
    Result := TSEPascalScriptModule.Create(SEAudioMaster, Reserved).Effect;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
