{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SoftKneeFeedbackCompressor;

{$R 'SoftKneeKnob.res' 'SoftKneeKnob.rc'}

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SoftKneeFeedbackCompressorDM in 'SoftKneeFeedbackCompressorDM.pas' {SoftKneeFeedbackCompressorDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SoftKneeFeedbackCompressorDataModule: TSoftKneeFeedbackCompressorDataModule;
begin
  try
    SoftKneeFeedbackCompressorDataModule := TSoftKneeFeedbackCompressorDataModule.Create(Application);
    SoftKneeFeedbackCompressorDataModule.AudioMaster := audioMaster;
    with SoftKneeFeedbackCompressorDataModule do
    begin
      Effect^.user := SoftKneeFeedbackCompressorDataModule;
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
