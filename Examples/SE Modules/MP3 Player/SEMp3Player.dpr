library SEMp3Player;

uses
  FastMM4,
  FastMove,
  DAV_SECommon,
  DAV_SEModule,
  SESimpleMp3PlayerModule in 'SESimpleMp3PlayerModule.pas',
  SEMp3PlayerModule in 'SEMp3PlayerModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSESimpleMp3PlayerModule.GetModuleProperties(Properties);
  1: TSEMp3PlayerModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 result := nil;
 case Index of
  0: if (ProcessType = 1) then
      begin
       SEModuleBase := TSESimpleMp3PlayerModule.Create(SEAudioMaster, Reserved);
       if assigned(SEModuleBase)
        then result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEMp3PlayerModule.Create(SEAudioMaster, Reserved);
       if assigned(SEModuleBase)
        then result := SEModuleBase.Effect;
      end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
