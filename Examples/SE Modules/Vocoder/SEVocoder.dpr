library SEVocoder;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEVocoderModule in 'SEVocoderModule.pas',
  SESimpleVocoderModule in 'SESimpleVocoderModule.pas',
  SEBarkVocoderModule in 'SEBarkVocoderModule.pas',
  DAV_DspDownsampleScheduler in '..\..\..\Source\DSP\DAV_DspDownsampleScheduler.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEVocoderStaticModule.GetModuleProperties(Properties);
  1: TSEVocoderControllableModule.GetModuleProperties(Properties);
  2: TSEVocoderAutomatableModule.GetModuleProperties(Properties);
  3: TSESimpleVocoderStaticModule.GetModuleProperties(Properties);
  4: TSESimpleVocoderControllableModule.GetModuleProperties(Properties);
  5: TSESimpleVocoderAutomatableModule.GetModuleProperties(Properties);
  6: TSEBarkVocoderStaticModule.GetModuleProperties(Properties);
  7: TSEBarkVocoderControllableModule.GetModuleProperties(Properties);
  8: TSEBarkVocoderAutomatableModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSEVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: result := TSEVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: result := TSEVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
   3: result := TSESimpleVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   4: result := TSESimpleVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   5: result := TSESimpleVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
   6: result := TSEBarkVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   7: result := TSEBarkVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   8: result := TSEBarkVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
