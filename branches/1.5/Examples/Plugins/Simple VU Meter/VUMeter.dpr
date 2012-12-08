{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VUMeter;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VUMeterModule in 'VUMeterModule.pas' {VSTVUMeterModule: TVSTModule},
  VUMeterGUI in 'VUMeterGUI.pas' {VSTVUMeterGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVSTVUMeterModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports
  Main name 'main',
  Main name 'VSTPluginMain';

begin
end.
