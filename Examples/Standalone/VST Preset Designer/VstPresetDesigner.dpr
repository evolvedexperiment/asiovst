program VstPresetDesigner;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  VpdMain in 'VpdMain.pas' {FmVstPresetDesigner};
//  VpdModifier in 'VpdModifier.pas' {FmModifier};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Preset Designer';
  Application.CreateForm(TFmVstPresetDesigner, FmVstPresetDesigner);
//  Application.CreateForm(TFmModifier, FmModifier);
  Application.Run;
end.
