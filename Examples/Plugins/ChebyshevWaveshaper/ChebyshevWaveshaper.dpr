{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevWaveshaper;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'ChebyshevWaveshaper.res' 'ChebyshevWaveshaper.rc'}

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
  ChebyshevWaveshaperDM in 'ChebyshevWaveshaperDM.pas' {ChebyshevWaveshaperDataModule: TVSTModule},
  ChebyshevWaveshaperGUI in 'ChebyshevWaveshaperGUI.pas' {FmChebyshevWaveshaper};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TChebyshevWaveshaperDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TChebyshevWaveshaperDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
