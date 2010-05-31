program FractionalOctaveProcessor;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  RTLVCLOptimize,
  Forms,
  FopMain in 'FopMain.pas' {FmFractionalOctaveProcessor};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Fractional Octave Processor';
  Application.CreateForm(TFmFractionalOctaveProcessor, FmFractionalOctaveProcessor);
  Application.Run;
end.
