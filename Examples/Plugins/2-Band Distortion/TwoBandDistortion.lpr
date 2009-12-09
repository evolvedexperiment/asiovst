{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library TwoBandDistortion;

{$I DAV_Compiler.inc}

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'TwoBandDistortion.res' 'TwoBandDistortion.rc'}

uses
  Interfaces,
  Forms,
  DAV_VSTModule,
  DAV_VSTEffect,
  DAV_Common,
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
 Application.Initialize;
end.
