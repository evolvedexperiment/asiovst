program VST2SEM;

{$R 'SEVST2SEM.res' 'SEVST2SEM.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  RtlVCLOptimize,
  Forms,
  V2Smain in 'V2Smain.pas' {FmVST2SEM};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmVST2SEM, FmVST2SEM);
  Application.Run;
end.
