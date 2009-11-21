program ConvertAudioFile;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  CafMain in 'CafMain.pas' {FmConvertAudioFile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmConvertAudioFile, FmConvertAudioFile);
  Application.Run;
end.
