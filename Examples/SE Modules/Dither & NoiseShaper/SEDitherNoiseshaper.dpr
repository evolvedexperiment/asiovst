library SEDitherNoiseshaper;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDitherNoiseshaperModule in 'SEDitherNoiseshaperModule.pas',
  SEDitherHighshelfNoiseshaperModule in 'SEDitherHighshelfNoiseshaperModule.pas',
  SEDitherSharpNoiseshaperModule in 'SEDitherSharpNoiseshaperModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEDitherNoiseshaperStaticModule.GetModuleProperties(Properties);
  1: TSEDitherNoiseshaperControllableModule.GetModuleProperties(Properties);
  2: TSEDitherHighshelfNoiseshaperStaticModule.GetModuleProperties(Properties);
  3: TSEDitherHighshelfNoiseshaperControllableModule.GetModuleProperties(Properties);
  4: TSEDitherSharpNoiseshaperStaticModule.GetModuleProperties(Properties);
  5: TSEDitherSharpNoiseshaperControllableModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
   0: result := TSEDitherNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: result := TSEDitherNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: result := TSEDitherHighshelfNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   3: result := TSEDitherHighshelfNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
   4: result := TSEDitherSharpNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   5: result := TSEDitherSharpNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
