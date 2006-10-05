{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
program GUITest;
{%File 'config.inc'}
{%File 'MidiHandler.inc'}

uses
  Forms,
  DVSTHost,
  uPlugin in 'uPlugin.pas',
  uEditor in 'uEditor.pas' {PluginEditorWindow},
  DAEffect;

{$R *.res}
var PluginEditorWindow : TPluginEditorWindow;
    VSTHost            : TVSTHost;
    Effect             : APlugin;
    Oome               : Boolean;

procedure CreateVSTHost;
begin
  VSTHost:=TVSTHost.Create(Application);
  VSTHost.VSTPlugIns.Add;
  VSTHost.BlockSize:=2048;
  VSTHost.VstTimeInfo.SampleRate:=44100;
  Effect:=APlugin.Create(audioMaster);
  VSTHost.VSTPlugIns[0].PVSTEffect:=Effect.Effect;
  VSTHost.VSTPlugIns[0].Open;
end;

procedure FreeVSTHost;
begin
  VSTHost.VSTPlugIns[0].Close;
  VSTHost.Free;
end;

begin
  Application.Initialize;
  Application.CreateForm(TPluginEditorWindow, PluginEditorWindow);
  CreateVSTHost;
  PluginEditorWindow.BorderStyle:=bsDialog;
  PluginEditorWindow.Height:=PluginEditorWindow.Height+20;
  Application.Run;
  FreeVSTHost;
end.
