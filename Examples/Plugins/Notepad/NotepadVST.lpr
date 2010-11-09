{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NotepadVST;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PluginDM in 'PluginDM.pas' {PluginDataModule: TVSTModule},
  Editor in 'Editor.pas' {FmNotepad};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TPluginDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TPluginDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
