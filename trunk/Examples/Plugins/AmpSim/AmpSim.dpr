{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AmpSim;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'AmpKnob.res' 'AmpKnob.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  AmpSimDM in 'AmpSimDM.pas' {ComboDataModule: TVSTModule},
  AmpSimGUI in 'AmpSimGUI.pas' {FmCombo};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TComboDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TComboDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
