{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SortedDisorder;

uses
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SortedDisorderDM in 'SortedDisorderDM.pas' {SortedDisorderModule: TVSTModule},
  SortedDisorderGui in 'SortedDisorderGui.pas' {FmSortedDisorder},
  DAV_DspBlockDistribution in '..\..\..\Source\DAV_DspBlockDistribution.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TSortedDisorderModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TSortedDisorderModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.