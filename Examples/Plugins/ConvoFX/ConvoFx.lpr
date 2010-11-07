{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ConvoFx;

{$I DAV_Compiler.inc}

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'Resources\IRs.res' 'Resources\IRs.rc'}

uses
  Interfaces,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ConvoFxDM in 'ConvoFxDM.pas' {ConvoFxDataModule: TVSTModule},
  ConvoFxGUI in 'ConvoFxGUI.pas' {FmConvoFx};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TConvoFxDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TConvoFxDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
 Application.Initialize;
end.
