{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleCompressor2;

{$R 'SimpleCompressor2.res' 'SimpleCompressor2.rc'}

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleCompressor2DM in 'SimpleCompressor2DM.pas' {SoftKneeCompressorDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SoftKneeCompressorDataModule: TSoftKneeCompressorDataModule;
begin
  try
    SoftKneeCompressorDataModule := TSoftKneeCompressorDataModule.Create(Application);
    SoftKneeCompressorDataModule.AudioMaster := audioMaster;
    with SoftKneeCompressorDataModule do
    begin
      Effect^.user := SoftKneeCompressorDataModule;
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
