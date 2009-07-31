{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library RingModulatorStereo;

{$IFNDEF Wrapper}
{$R 'BodeFrequencyShifter.res' 'BodeFrequencyShifter.rc'}

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  RingModulatorDM in 'BodeFrequencyShifterDM.pas' {RingModulatorDataModule: TVSTModule},
  RingModulatorGUI in 'BodeFrequencyShifterGUI.pas' {FmRingModulator};

{$ELSE}

uses
  DAV_VSTEffect;

function RingModulatorMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'BodeFrequencyShifter.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TRingModulatorDataModule);
 {$ELSE}
 Result := RingModulatorMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.
