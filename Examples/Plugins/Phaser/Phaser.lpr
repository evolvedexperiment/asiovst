{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Phaser;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PhaserDM in 'PhaserDM.pas' {PhaserModule: TPhaserModule},
  PhaserFrm in 'PhaserFrm.pas' {PhaserForm};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TPhaserModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

{$R *.res}

begin
end.
