unit HrtfConvolverDM;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_AudioData, DAV_DspConvolution, DAV_Semaphore, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_DspHrtf;

type
  THrtfConvolverDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterAzimuthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterElevationChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRadiusChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSemaphore   : TSemaphore;
    FConvolution : array [0..1] of TLowLatencyConvolution32;
    FAdcHrtf     : TAudioDataCollection32;
    FAdcIR       : TAudioDataCollection32;
    FHRTFs       : THrtfs;
    function GetConvolution(Index: Integer): TLowLatencyConvolution32;
  public
    procedure HRTFChanged;

    property AudioDataCollectionHRTF: TAudioDataCollection32 read FAdcHrtf;
    property Convolution[Index: Integer]: TLowLatencyConvolution32 read GetConvolution;
    property HRTFs: THrtfs read FHRTFs;
    property Semaphore: TSemaphore read FSemaphore write FSemaphore;
  end;

implementation

{$R *.DFM}

uses
  HrtfConvolverGui;

resourcestring
  RCStrIndexOutOfBouds = 'Index out of bouds(%d)';

procedure THrtfConvolverDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := TSemaphore.Create;
end;

procedure THrtfConvolverDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FSemaphore);
end;

procedure THrtfConvolverDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
  RS      : TResourceStream;
begin
 for Channel := 0 to Length(FConvolution) - 1 do
  begin
   FConvolution[Channel] := TLowLatencyConvolution32.Create;
   FConvolution[Channel].MinimumIRBlockOrder := CeilLog2(InitialDelay);
   FConvolution[Channel].MaximumIRBlockOrder := 18;
  end;

 // create and load HRTF data
 FHRTFs := THRTFs.Create;
 RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
 try
  FHRTFs.LoadFromStream(RS);
 finally
  RS.Free;
 end;

 // create HRTF audio data collection
 FAdcHrtf := TAudioDataCollection32.Create(Self);
 with FAdcHrtf do
  begin
   SampleFrames := FHRTFs.MinimumHrirSize;
   ChannelCount := 2;
   SampleRate   := Self.SampleRate;
  end;

 FAdcIR := TAudioDataCollection32.Create(Self);
 with FAdcIR do
  begin
   SampleFrames := 1024;
   ChannelCount := 2;
   SampleRate   := Self.SampleRate;
  end;
end;

procedure THrtfConvolverDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FAdcHrtf);
 FreeAndNil(FAdcIR);
 FreeAndNil(FConvolution[0]);
 FreeAndNil(FConvolution[1]);
end;

procedure THrtfConvolverDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmHrtfConvolver.Create(Self);
end;

function THrtfConvolverDataModule.GetConvolution(Index: Integer): TLowLatencyConvolution32;
begin
 if Index in [0..1]
  then result := FConvolution[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBouds, [Index]);
end;

procedure THrtfConvolverDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 // lock processing
 FSemaphore.Enter;
 try
//  FPreDelay[0].ProcessBlock
  FConvolution[0].ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
  FConvolution[1].ProcessBlock(@Inputs[1, 0], @Outputs[1, 0], SampleFrames);
 finally
  FSemaphore.Leave;
 end;
end;

procedure THrtfConvolverDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 HRTFChanged;
end;

procedure THrtfConvolverDataModule.ParameterAzimuthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 HRTFChanged;
 if EditorForm is TFmHrtfConvolver
  then TFmHrtfConvolver(EditorForm).AzimuthChanged;
end;

procedure THrtfConvolverDataModule.ParameterElevationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 HRTFChanged;
 if EditorForm is TFmHrtfConvolver
  then TFmHrtfConvolver(EditorForm).ElevationChanged;
end;

procedure THrtfConvolverDataModule.ParameterRadiusChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 HRTFChanged;
 if EditorForm is TFmHrtfConvolver
  then TFmHrtfConvolver(EditorForm).RadiusChanged;
end;

procedure THrtfConvolverDataModule.HRTFChanged;
const
  CDeg2Rad = 2 * Pi / 360;
begin
 if assigned(FHRTFs) then
  with FAdcHrtf do
   begin
    FHRTFs.InterpolateHrir(Parameter[0] * CDeg2Rad, Parameter[1] * CDeg2Rad,
      SampleFrames, ChannelDataPointer[0], ChannelDataPointer[1]);

    // eventually resample data here

    FConvolution[0].LoadImpulseResponse(ChannelDataPointer[0], SampleFrames);
    FConvolution[1].LoadImpulseResponse(ChannelDataPointer[1], SampleFrames);
   end;
end;

end.
