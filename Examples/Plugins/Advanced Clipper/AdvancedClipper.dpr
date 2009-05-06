{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AdvancedClipper;

{$R 'AdvancedClipper.res' 'AdvancedClipper.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,  // either download the library or comment if there is an error here
  FastMove,
  RTLVCLOptimize,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  AdvancedClipperDM in 'AdvancedClipperDM.pas' {AdvancedClipperDataModule: TVSTModule},
  AdvancedClipperGUI in 'AdvancedClipperGUI.pas' {FmAdvancedClipper};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TAdvancedClipperDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TAdvancedClipperDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
