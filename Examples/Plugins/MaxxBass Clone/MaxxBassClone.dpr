{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library MaxxBassClone;

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  MaxxBassCloneDM in 'MaxxBassCloneDM.pas' {MaxxBassCloneModule: TVSTModule},
  MaxxBassCloneGUI in 'MaxxBassCloneGUI.pas' {FmMaxxBassClone},
  DAV_DspPsychoacousticBassEnhancer in '..\..\..\Source\DSP\DAV_DspPsychoacousticBassEnhancer.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TMaxxBassCloneModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TMaxxBassCloneModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.