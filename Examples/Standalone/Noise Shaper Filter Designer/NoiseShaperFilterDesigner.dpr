program NoiseShaperFilterDesigner;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$ENDIF }
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  NSFDmain in 'NSFDmain.pas' {FormNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNoiseshapingFilterDesigner, FormNoiseshapingFilterDesigner);
  Application.Run;
end.
