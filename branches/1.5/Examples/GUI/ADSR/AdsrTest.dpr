program AdsrTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  AdsrTestMain in 'AdsrTestMain.pas' {FmAdsrTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAdsrTest, FmAdsrTest);
  Application.Run;
end.
