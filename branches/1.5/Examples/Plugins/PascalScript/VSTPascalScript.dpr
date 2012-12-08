{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VSTPascalScript;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PSDM in 'PSDM.pas' {PascalScriptDataModule: TVSTModule},
  PSGUI in 'PSGUI.pas' {FmPascalScript};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TPascalScriptDataModule.Create(Application) do
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
