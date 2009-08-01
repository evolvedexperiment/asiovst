{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BarberpoleShifterStereo;

{$IFNDEF Wrapper}
{$R 'BarberpoleShifter.res' 'BarberpoleShifter.rc'}

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BarberpoleShifterDM in 'BarberpoleShifterDM.pas' {BarberpoleShifterDataModule: TVSTModule},
  BarberpoleShifterGUI in 'BarberpoleShifterGUI.pas' {FmBarberpoleShifter};

{$ELSE}

uses
  DAV_VSTEffect;

function BarberpoleShifterMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'BarberpoleShifter.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TBarberpoleShifterDataModule);
 {$ELSE}
 Result := BarberpoleShifterMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.
