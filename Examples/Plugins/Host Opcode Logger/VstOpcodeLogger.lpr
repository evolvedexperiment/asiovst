{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VstOpcodeLogger;

{$I DAV_Compiler.inc}

{$IMAGEBASE $400000}

uses
  Interfaces,
  LCLIntf,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  VOLDM in 'VOLDM.pas' {VOLDataModule: TVSTModule},
  VOLGUI in 'VOLGUI.pas' {FmVOL};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVOLDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TVOLDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
 {$IFDEF FPC}
 Application.Initialize;
 {$ENDIF}
end.
