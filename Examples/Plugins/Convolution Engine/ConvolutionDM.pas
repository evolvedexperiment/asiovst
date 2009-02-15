unit ConvolutionDM;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspConvolution;

type
  TConvolutionDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterMaximumIROrderChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLatencyChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FConvolutionClassic    : TConvolution32;
    FConvolutionLowLatency : TLowLatencyConvolution32;
    FSemaphore    : Integer;
  public
    procedure LoadIR(FileName: TFileName);
  end;

implementation

{$R *.DFM}

uses
  Math, WaveIOX, ConvolutionGUI;

procedure TConvolutionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 FConvolutionClassic := TConvolution32.Create;
 FConvolutionLowLatency := TLowLatencyConvolution32.Create;
 FConvolutionClassic.FFTOrder := max(7, CeilLog2(InitialDelay)) + 1;
 FConvolutionLowLatency.MinimumIRBlockOrder := max(7, CeilLog2(InitialDelay));
 FConvolutionLowLatency.MaximumIRBlockOrder := 17;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FConvolutionLowLatency);
 FreeAndNil(FConvolutionClassic);
end;

procedure TConvolutionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmConvolution.Create(Self);
end;

procedure TConvolutionDataModule.ParameterMaximumIROrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if Value >= FConvolutionLowLatency.MinimumIRBlockOrder
   then FConvolutionLowLatency.MaximumIRBlockOrder := round(Limit(Value, 7, 20))
   else Value := FConvolutionLowLatency.MinimumIRBlockOrder;
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.ParameterLatencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if Value > FConvolutionLowLatency.MaximumIRBlockOrder
   then Value := FConvolutionLowLatency.MaximumIRBlockOrder;
  FConvolutionLowLatency.MinimumIRBlockOrder := round(Value);
  FConvolutionClassic.FFTOrder := round(Value) + 1;
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  sr, sz, c : Integer;
  pt        : PSingle;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  pt := LoadWAVFileMono(FileName, sr, c, sz);

  FConvolutionLowLatency.LoadImpulseResponse(@pt^, sz);
  FConvolutionClassic.LoadImpulseResponse(@pt^, sz);
 finally
  dec(FSemaphore);
 end;
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 // lock processing
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  FConvolutionClassic.ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
  FConvolutionLowLatency.ProcessBlock(@Inputs[1, 0], @Outputs[1, 0], SampleFrames);
 finally
  Dec(FSemaphore);
 end;
end;

end.
