{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleLimiter;

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}
  Interfaces,
  Forms,
  {$ENDIF}
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SimpleLimiterDM in 'SimpleLimiterDM.pas' {SimpleLimiterDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFDEF FPC}
 Application.Initialize;
 {$ENDIF}
 Result := VstModuleMain(AudioMasterCallback, TSimpleLimiterDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSimpleLimiterDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
