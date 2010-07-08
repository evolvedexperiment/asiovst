{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthHPMono;

{$IFNDEF Wrapper}
{$R 'Butterworth.res' 'Butterworth.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,// either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ButterworthDM in 'ButterworthDM.pas' {ButterworthHPModule: TVSTModule},
  ButterworthGUI in 'ButterworthGUI.pas' {FmButterworth};

{$ELSE}

uses
  DAV_VSTEffect;

function ButterworthHPMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'ButterworthHP.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TButterworthHPModule);
 {$ELSE}
 Result := ButterworthHPMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 1;
 Result^.numOutputs := 1;
 Result^.UniqueID[0] := '1';
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.