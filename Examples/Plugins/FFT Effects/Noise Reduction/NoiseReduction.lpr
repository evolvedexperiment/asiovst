{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NoiseReduction;

{$I DAV_Compiler.inc}

{$R *.res}

uses
  Interfaces, DAV_VSTEffect, DAV_VSTBasicModule, DAV_Common_Lazarus,
  DAV_DSP_Lazarus, DAV_VSTPlugin_Lazarus,
  NoiseReductionDM in 'NoiseReductionDM.pas' {NoiseReductionModule: TVSTModule},
  NoiseReductionGui in 'NoiseReductionGui.pas' {FmNoiseReduction};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TNoiseReductionModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
