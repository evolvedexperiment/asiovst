{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LayeredFrequencySplitter;

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}
  Interfaces,
  Forms,
  {$ENDIF}
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LayeredFreqSplitDSP in 'LayeredFreqSplitDSP.pas' {LayeredFreqSplitModule: TVSTModule},
  LayeredFreqSplitGUI in 'LayeredFreqSplitGUI.pas' {FmLayeredFreqSplit};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFDEF FPC}
 Application.Initialize;
 {$ENDIF}
 Result := VstModuleMain(AudioMasterCallback, TLayeredFreqSplitModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TLayeredFreqSplitModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
