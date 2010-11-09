{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library PartyDelay;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PartyDelayDM in 'PartyDelayDM.pas' {PartyDelayDataModule: TVSTModule},
  PartyDelayGui in 'PartyDelayGui.pas' {FmPartyDelay};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPartyDelayDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TPartyDelayDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
