program HRTFEditor;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,

  Forms,
  HEmain in 'HEmain.pas' {FmHRTFEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHRTFEditor, FmHRTFEditor);
  Application.Run;
end.
