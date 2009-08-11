program ConvertAudioFile;

uses
  Forms,
  CafMain in 'CafMain.pas' {FmConvertAudioFile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmConvertAudioFile, FmConvertAudioFile);
  Application.Run;
end.
