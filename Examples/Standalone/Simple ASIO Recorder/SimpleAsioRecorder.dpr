program SimpleAsioRecorder;

uses
  Forms,
  SarMain in 'SarMain.pas' {FmRecordAudio},
  DAV_DspBufferedAudioFileRecorder in '..\..\..\..\VSTPack\Source\DSP\DAV_DspBufferedAudioFileRecorder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmRecordAudio, FmRecordAudio);
  Application.Run;
end.
