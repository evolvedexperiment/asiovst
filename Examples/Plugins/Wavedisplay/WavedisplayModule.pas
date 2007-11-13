unit WavedisplayModule;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TWavedisplayModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs,
      Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  WavedisplayGUI;

procedure TWavedisplayModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
  GUI := TWavedisplayGUI.Create(Self);
end;

procedure TWavedisplayModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var i: Integer;
begin
  if Assigned(Editorform) then
  begin
    (EditorForm as TWavedisplayGUI).Display.ProcessBufferIndirect(Inputs, 2, SampleFrames);
    (EditorForm as TWavedisplayGUI).LevelMeter.ProcessBufferIndirect(Inputs, 2, SampleFrames);
  end;

  for i := 0 to 1 do
    move(inputs[i,0], outputs[i,0], SampleFrames * sizeOf(Single));
end;

end.
