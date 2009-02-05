{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FastFeedbackCompressor;

{$R 'FeedbackCompressor.res' 'FeedbackCompressor.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  FastFeedbackCompressorDM in 'FastFeedbackCompressorDM.pas' {FastFeedbackCompressorDataModule: TVSTModule},
  FastFeedbackCompressorGUI in 'FastFeedbackCompressorGUI.pas' {FmFastFeedbackCompressor};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TFastFeedbackCompressorDataModule.Create(Application) do
     begin
      AudioMaster := AudioMasterCallback;
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
