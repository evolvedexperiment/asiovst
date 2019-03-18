library SEHRTF3D;

{$I DAV_Compiler.inc}

{$R 'HRTF3D.res' 'HRTF3D.rc'}

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEHRTF3DModule in 'SEHRTF3DModule.pas';

{$E sem}
{$R *.res}

function GetModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := False;
  if (Index = 0) then
  begin
    TSEHRTF3DModule.GetModuleProperties(Properties);
    Result := True;
  end;
end;

function MakeModule(Index, ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
  Result := nil;
  if (Index = 0) and (ProcessType = 1) then
    Result := TSEHRTF3DModule.Create(SEAudioMaster, Reserved).Effect;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
