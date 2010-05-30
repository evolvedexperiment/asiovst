library SEChorus;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChorusModule in 'SEChorusModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEChorusModule.GetModuleProperties(Properties);
  1: TSEStkChorusModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSEChorusModule.Create(SEAudioMaster, Reserved).Effect;
   1: result := TSEStkChorusModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
