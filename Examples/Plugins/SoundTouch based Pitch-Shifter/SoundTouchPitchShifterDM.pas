unit SoundTouchPitchShifterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_SoundTouchDLL; //DAV_SoundTouch;

type
  TSoundTouchPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSoundTouch : array [0..1] of TSoundTouch;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, SoundTouchPitchShifterGUI, DAV_VSTCustomModule;

procedure TSoundTouchPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1 do
  begin
   FSoundTouch[Channel] := TSoundTouch.Create;
   with FSoundTouch[Channel] do
    begin
     Samplerate := Self.Samplerate;
     Channels := 1;
    end;
  end;
 Parameter[0] := 1;
end;

procedure TSoundTouchPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1 do
  if assigned(FSoundTouch[Channel])
   then FSoundTouch[Channel].Pitch := Power(2, Value / 12);

 // update GUI
 if EditorForm is TFmSoundTouchPitchShifter
  then TFmSoundTouchPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1
  do FreeAndNil(FSoundTouch[Channel]);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSoundTouchPitchShifter.Create(Self);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1 do
  if assigned(FSoundTouch[Channel])
   then FSoundTouch[Channel].SampleRate := SampleRate;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1 do
  begin
   FSoundTouch[ch].PutSamples(@Inputs[ch, 0], SampleFrames);
   FSoundTouch[ch].ReceiveSamples(@Outputs[ch, 0], SampleFrames);
  end;
end;

end.
