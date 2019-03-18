library SEPinkNoise;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEPinkNoiseModule in 'SEPinkNoiseModule.pas';

{$E sem}
{$R *.res}

function GetModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := False;
  if (Index = 0) then
  begin
    TSEPinkNoiseModule.GetModuleProperties(Properties);
    Result := True;
  end;
end;

function MakeModule(Index, ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
  Result := nil;
  if (Index = 0) and (ProcessType = 1) then
    Result := TSEPinkNoiseModule.Create(SEAudioMaster, Reserved).Effect;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
