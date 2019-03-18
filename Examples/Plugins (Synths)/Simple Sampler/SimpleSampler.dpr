{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleSampler;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  {$ENDIF }
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SimpleSamplerModule in 'SimpleSamplerModule.pas' {VSTSSModule: TVSTModule},
  SimpleSamplerGUI in 'SimpleSamplerGUI.pas' {VSTGUI},
  SimpleSamplerVoice in 'SimpleSamplerVoice.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
                                                             
