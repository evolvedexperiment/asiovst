{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library StkReverb;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  StkReverbDM in 'StkReverbDM.pas' {StkReverbModule: TVSTModule},
  StkReverbGUI in 'StkReverbGUI.pas' {FmStkReverb};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TStkReverbModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
