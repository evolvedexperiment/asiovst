{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BarberpoleFlanger;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'Barberpole.res' 'Barberpole.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BarberpoleFlangerDM in 'BarberpoleFlangerDM.pas' {BarberpoleFlangerModule: TVSTModule},
  BarberpoleFlangerGUI in 'BarberpoleFlangerGUI.pas' {FmBarberpoleFlanger};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TBarberpoleFlangerModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TBarberpoleFlangerModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
