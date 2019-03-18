program AdsrTest;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  AdsrTestMain in 'AdsrTestMain.pas' {FmAdsrTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAdsrTest, FmAdsrTest);
  Application.Run;
end.
