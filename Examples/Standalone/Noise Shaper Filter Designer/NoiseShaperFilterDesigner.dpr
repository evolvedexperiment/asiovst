program NoiseShaperFilterDesigner;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  Forms,
  NSFDmain in 'NSFDmain.pas' {FmNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmNoiseshapingFilterDesigner, FmNoiseshapingFilterDesigner);
  Application.Run;
end.
