{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SineSynth;

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
  SineSynthModule in 'SineSynthModule.pas' {VSTSSModule: TVSTModule},
  SineSynthGUI in 'SineSynthGUI.pas' {VSTGUI},
  SineSynthVoice in 'SineSynthVoice.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

