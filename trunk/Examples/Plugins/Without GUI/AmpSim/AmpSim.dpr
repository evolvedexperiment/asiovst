{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library AmpSim;

uses
  FastMM4,
  FastMove,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  AmpSimDM in 'AmpSimDM.pas' {AmpSimModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TComboDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
