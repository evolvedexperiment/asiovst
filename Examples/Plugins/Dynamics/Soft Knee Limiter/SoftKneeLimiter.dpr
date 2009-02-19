{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SoftKneeLimiter;

{$R 'SoftKneeKnob.res' 'SoftKneeKnob.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SKLDM in 'SKLDM.pas' {SoftKneeLimiterDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSoftKneeLimiterDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSoftKneeLimiterDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
