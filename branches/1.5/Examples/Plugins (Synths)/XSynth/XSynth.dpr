{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XSynth;

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
  XSynthModule in 'XSynthModule.pas' {VSTSSModule: TVSTModule},
  XSynthGUI in 'XSynthGUI.pas' {VSTGUI},
  XSynthVoice in 'XSynthVoice.pas',
  XSynthVoiceList in 'XSynthVoiceList.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

