{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleCompressor;

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleCompressorDM in 'SimpleCompressorDM.pas' {SimpleCompressorDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SimpleCompressorDataModule: TSimpleCompressorDataModule;
begin
 try
  SimpleCompressorDataModule := TSimpleCompressorDataModule.Create(Application);
  SimpleCompressorDataModule.AudioMaster := audioMaster;
  with SimpleCompressorDataModule do
   begin
    Effect^.user := SimpleCompressorDataModule;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
