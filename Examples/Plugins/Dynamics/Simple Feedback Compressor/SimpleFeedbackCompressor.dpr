{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleFeedbackCompressor;

{$R 'SimpleFeedbackCompressor.res' 'SimpleFeedbackCompressor.rc'}

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleFeedbackCompressorDM in 'SimpleFeedbackCompressorDM.pas' {SimpleFeedbackCompressorDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTModule: TVSTModule;
begin
  try
    VSTModule := TSimpleFeedbackCompressorDataModule.Create(Application);
    VSTModule.AudioMaster := audioMaster;
    with VSTModule do
    begin
      Effect^.user := VSTModule;
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
