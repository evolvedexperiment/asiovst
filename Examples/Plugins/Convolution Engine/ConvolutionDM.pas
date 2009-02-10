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
 FConvolutionClassic.FFTOrder := 8; //CeilLog2(BlockModeSize);
 FConvolutionLowLatency.MinimumIRBlockOrder := 7
// FConvolutionLowLatency.FFTOrder := CeilLog2(BlockModeSize);
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
 inc(FSemaphore);
 try
  FConvolutionClassic.ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
  FConvolutionLowLatency.ProcessBlock(@Inputs[1, 0], @Outputs[1, 0], SampleFrames);
 finally
  dec(FSemaphore);
 end;
end;

end.
