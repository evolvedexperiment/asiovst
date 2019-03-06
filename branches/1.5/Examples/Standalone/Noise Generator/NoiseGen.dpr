program NoiseGen;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  NoiseGenForm in 'NoiseGenForm.pas' {FormASIONoiseGenerator};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFormASIONoiseGenerator, FormASIONoiseGenerator);
  Application.Run;
end.