library SEBassEnhancer;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEMaxxBassModule in 'SEMaxxBassModule.pas',
  SEBassEnhancerModule in 'SEBassEnhancerModule.pas',
  SERenaissanceBassModule in 'SERenaissanceBassModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEMaxBassModule.GetModuleProperties(Properties);
  1: TSEBassEnhancerModule.GetModuleProperties(Properties);
  2: TSERenaissanceBassModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSEMaxBassModule.Create(SEAudioMaster, Reserved).Effect;
   1: result := TSEBassEnhancerModule.Create(SEAudioMaster, Reserved).Effect;
   2: result := TSERenaissanceBassModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
