{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevLPMono;

{$IFNDEF Wrapper}
{$R 'Chebyshev.res' 'Chebyshev.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,// either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ChebyshevDM in 'ChebyshevDM.pas' {ChebyshevLPModule: TVSTModule},
  ChebyshevGUI in 'ChebyshevGUI.pas' {FmChebyshev};

{$ELSE}

uses
  DAV_VSTEffect;

function ChebyshevLPMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'ChebyshevLP.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TChebyshevLPModule);
 {$ELSE}
 Result := ChebyshevLPMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 1;
 Result^.numOutputs := 1;
 Result^.UniqueID[0] := '1';
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.
