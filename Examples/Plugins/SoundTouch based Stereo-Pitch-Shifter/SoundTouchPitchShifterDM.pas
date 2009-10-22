unit SoundTouchPitchShifterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_SoundTouchDLL, DAV_ChannelDataCoder; //DAV_SoundTouch;

type
  TSoundTouchPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleBlockSizeChange(Sender: TObject;
      const BlockSize: Integer);
  private
    FSoundTouch      : TSoundTouch;
    FDataCoder       : TChannel32DataCoderFloat32;
    FInterleavedData : PDAVSingleFixedArray;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, SoundTouchPitchShifterGUI, DAV_VSTCustomModule;

procedure TSoundTouchPitchShifterModule.VSTModuleOpen(Sender: TObject);
begin
 FSoundTouch := TSoundTouch.Create;
 with FSoundTouch do
  begin
   SampleRate := Self.SampleRate;
   Channels := 2;
  end;
 FDataCoder := TChannel32DataCoderFloat32.Create;
 Parameter[0] := 1;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleBlockSizeChange(
  Sender: TObject; const BlockSize: Integer);
begin
 ReallocMem(FInterleavedData, 2 * BlockSize * SizeOf(Single));
 FDataCoder.BlockSize := 2 * BlockSize * SizeOf(Single);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSoundTouch);
 FreeAndNil(FDataCoder);
end;

procedure TSoundTouchPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSoundTouch)
  then FSoundTouch.Pitch := Power(2, Value / 12);

 // update GUI
 if EditorForm is TFmSoundTouchPitchShifter
  then TFmSoundTouchPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSoundTouchPitchShifter.Create(Self);
end;

procedure TSoundTouchPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FSoundTouch)
  then FSoundTouch.SampleRate := SampleRate;
end;

procedure TSoundTouchPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 FDataCoder.BlockSize := 2 * SampleFrames * SizeOf(Single);
 Move(Inputs[0, 0], FDataCoder.ChannelPointer[0]^, SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], FDataCoder.ChannelPointer[1]^, SampleFrames * SizeOf(Single));
 FDataCoder.SaveToPointer(FInterleavedData);
 FSoundTouch.PutSamples(@FInterleavedData^[0], SampleFrames);
 FSoundTouch.ReceiveSamples(@FInterleavedData^[0], SampleFrames);
 FDataCoder.LoadFromPointer(FInterleavedData);
 Move(FDataCoder.ChannelPointer[0]^, Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(FDataCoder.ChannelPointer[1]^, Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

end.
