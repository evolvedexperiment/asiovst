{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library TwoBandDistortion;

{$R 'TwoBandDistortion.res' 'TwoBandDistortion.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,  
  FastMove, // either download the library or comment if there is an error here
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  TwoBandDistortionDM in 'TwoBandDistortionDM.pas' {TwoBandDistortionDataModule: TVSTModule},
  TwoBandDistortionGUI in 'TwoBandDistortionGUI.pas' {FmTwoBandDistortion};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TTwoBandDistortionDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TTwoBandDistortionDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
