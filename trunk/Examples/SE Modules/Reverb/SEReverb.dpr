library SEReverb;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEReverbModule in 'SEReverbModule.pas',
  SEPlateVerbModule in 'SEPlateVerbModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
   0: TSEStkNReverbStaticModule.GetModuleProperties(Properties);
   1: TSEStkNReverbControllableModule.GetModuleProperties(Properties);
   2: TSEStkJCReverbStaticModule.GetModuleProperties(Properties);
   3: TSEStkJCReverbControllableModule.GetModuleProperties(Properties);
   4: TSEStkNReverb2StaticModule.GetModuleProperties(Properties);
   5: TSEStkNReverb2ControllableModule.GetModuleProperties(Properties);
   6: TSEStkJCReverb2StaticModule.GetModuleProperties(Properties);
   7: TSEStkJCReverb2ControllableModule.GetModuleProperties(Properties);
   8: TSEFreeverbStaticModule.GetModuleProperties(Properties);
   9: TSEFreeverbControllableModule.GetModuleProperties(Properties);
  10: TSEPlateReverbStaticModule.GetModuleProperties(Properties);
  11: TSEPlateReverbControllableModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
    0: result := TSEStkNReverbStaticModule.Create(SEAudioMaster, Reserved).Effect;
    1: result := TSEStkNReverbControllableModule.Create(SEAudioMaster, Reserved).Effect;
    2: result := TSEStkJCReverbStaticModule.Create(SEAudioMaster, Reserved).Effect;
    3: result := TSEStkJCReverbControllableModule.Create(SEAudioMaster, Reserved).Effect;
    4: result := TSEStkNReverb2StaticModule.Create(SEAudioMaster, Reserved).Effect;
    5: result := TSEStkNReverb2ControllableModule.Create(SEAudioMaster, Reserved).Effect;
    6: result := TSEStkJCReverb2StaticModule.Create(SEAudioMaster, Reserved).Effect;
    7: result := TSEStkJCReverb2ControllableModule.Create(SEAudioMaster, Reserved).Effect;
    8: result := TSEFreeverbStaticModule.Create(SEAudioMaster, Reserved).Effect;
    9: result := TSEFreeverbControllableModule.Create(SEAudioMaster, Reserved).Effect;
   10: result := TSEPlateReverbStaticModule.Create(SEAudioMaster, Reserved).Effect;
   11: result := TSEPlateReverbControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
