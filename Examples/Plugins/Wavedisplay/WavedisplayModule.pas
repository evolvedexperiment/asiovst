unit WavedisplayModule;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TWavedisplayModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  WavedisplayGUI;

procedure TWavedisplayModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TWavedisplayGUI.Create(Self);
end;

procedure TWavedisplayModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i: Integer;
begin
  if Assigned(Editorform) then
    (EditorForm as TWavedisplayGUI).Display.ProcessBufferIndirect(Inputs, 2, SampleFrames);

  for i := 0 to 1 do
    move(inputs[i,0], outputs[i,0], SampleFrames * sizeOf(Single));
end;

end.
