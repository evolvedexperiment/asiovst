{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BarberpoleFlanger;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
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
