program VstPresetDesigner;

uses
  Forms,
  VpdMain in 'VpdMain.pas' {FmVstPresetDesigner};
//  VpdModifier in 'VpdModifier.pas' {FmModifier};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmVstPresetDesigner, FmVstPresetDesigner);
//  Application.CreateForm(TFmModifier, FmModifier);
  Application.Run;
end.
