{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Vocoder;

uses
  FastMM4,
  madLinkDisAsm,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  {$IFDEF UseMadExcept}
  madExcept,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  VocoderModule in 'VocoderModule.pas' {VSTSSModule: TVSTModule},
  VocoderGUI in 'VocoderGUI.pas' {VSTGUI},
  VocoderVoice in 'VocoderVoice.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

