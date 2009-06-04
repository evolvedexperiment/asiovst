program HRTFEditor;

{$R 'HRTF3D.res' 'HRTF3D.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  Forms,
  HEmain in 'HEmain.pas' {FmHRTFEditor},
  HEeti in 'HEeti.pas' {FmEtiImport};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHRTFEditor, FmHRTFEditor);
  Application.Run;
end.
