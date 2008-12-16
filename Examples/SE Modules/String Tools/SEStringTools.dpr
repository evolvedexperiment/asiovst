library SEStringTools;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEStringToolsModule in 'SEStringToolsModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEConcatStringModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;

 case Index of // !!TODO!! list your in / out plugs
  0: if (ProcessType = 1) then SEModuleBase := TSEConcatStringModule.Create(SEAudioMaster, Reserved);
 end;

 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
