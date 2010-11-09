{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ModDelay2;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ModDelay2DM in 'ModDelay2DM.pas' {ModDelay2Module: TVSTModule},
  ModDelay2GUI in 'ModDelay2GUI.pas' {FmModDelay2};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TModDelay2Module);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TModDelay2Module);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
 Application.Initialize;
end.
