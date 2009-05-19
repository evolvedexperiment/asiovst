{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Vocoder;

uses
  FastMM4,
  FastMove,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  VocoderModule in 'VocoderModule.pas' {VSTSSModule: TVSTModule},
  VocoderGUI in 'VocoderGUI.pas' {VSTGUI},
  VocoderVoice in 'VocoderVoice.pas',
  VoiceList in 'VoiceList.pas',
  DAV_DspVocoder in '..\..\..\Source\DSP\DAV_DspVocoder.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.

