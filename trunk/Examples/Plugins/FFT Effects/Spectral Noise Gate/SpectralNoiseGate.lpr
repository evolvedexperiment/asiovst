{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SpectralNoiseGate;

{$I DAV_Compiler.inc}

{$R 'Resources.res' 'Resources.rc'}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SpectralNoiseGateDM in 'SpectralNoiseGateDM.pas' {SpectralNoiseGateModule: TVSTModule},
  SpectralNoiseGateGui in 'SpectralNoiseGateGui.pas' {FmSpectralNoiseGate};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TSpectralNoiseGateModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
