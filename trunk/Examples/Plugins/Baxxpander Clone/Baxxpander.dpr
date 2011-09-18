{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Baxxpander;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BaxxpanderModule in 'BaxxpanderModule.pas' {BaxxpanderModule: TVSTModule},
  BaxxpanderGui in 'BaxxpanderGui.pas' {FmBaxxpanderGui};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TBaxxpanderModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
