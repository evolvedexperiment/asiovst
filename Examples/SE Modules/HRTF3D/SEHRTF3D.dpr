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

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEHRTF3DModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 if (ProcessType = 1) then
  case Index of
   0: SEModuleBase := TSEHRTF3DModule.Create(SEAudioMaster, Reserved);
  end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
