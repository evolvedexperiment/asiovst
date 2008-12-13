unit ConvolutionDM;

interface

{$I ASIOVST.INC}
{-$DEFINE Use_IPPS}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  DAV_VSTModule;

type
  TConvolutionDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
  private
    FFilterKernel       : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FOutputBuffer       : array of PDAVSingleFixedArray;
    FSemaphore          : Integer;

    {$IFDEF Use_IPPS}
    FFft                : TFftReal2ComplexIPPSFloat32;
    {$ELSE}
    FFft                : TFftReal2ComplexNativeFloat32;
    {$ENDIF}

    FIRSize             : Integer;
    FOffsetSize         : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FFFTSizeQuarter     : Integer;
    procedure SetIRSizePadded(const Value: Integer);
    procedure SetIRBlockSize(const Value: Integer);
  protected
    procedure CalculateFilterBlockFrequencyResponses;
    procedure CalculatePaddedIRSize;
    procedure CalculateFrequencyResponseBlockCount;
    procedure IRSizePaddedChanged;
    procedure IRBlockSizeChanged;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray);

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    procedure LoadIR(FileName: TFileName);
    property IRBlockSize: Integer read FFFTSize write SetIRBlockSize;
  end;

implementation

{$R *.DFM}

uses
  Math, WaveIOX, ConvolutionGUI;

procedure TConvolutionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore          := 0;
 FFilterKernel       := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFFTSize            := 0;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FFFTSize            := 0;
 FFFTSizeHalf        := 0;
 FFFTSizeQuarter     := 0;
 BlockModeSize       := 512;
 BlockModeOverlap    := BlockModeSize div 2;
 InitialDelay        := BlockModeOverlap;
 IRBlockSize         := BlockModeSize;
 FOffsetSize         := BlockModeSize - BlockModeOverlap;
 SetLength(FOutputBuffer, max(numInputs, numOutputs));
end;

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 FIRSize := 0;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 Dispose(FFilterKernel);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 for i := 0 to Length(FOutputBuffer) - 1 do Dispose(FOutputBuffer[i]);
 for i := 0 to Length(FFilterFreqs)  - 1 do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
end;

procedure TConvolutionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmConvolution.Create(Self);
end;

procedure TConvolutionDataModule.SetIRBlockSize(const Value: Integer);
begin
 if FFFTSize <> Value then
  begin
   FFFTSize := Value;
   IRBlockSizeChanged;
  end;
end;

procedure TConvolutionDataModule.IRSizePaddedChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FOutputBuffer) - 1 do
  begin
   GetMem(FOutputBuffer[Channel], FIRSizePadded * SizeOf(Single));
   FillChar(FOutputBuffer[Channel]^[0], FIRSizePadded * SizeOf(Single), 0);
  end;
end;

procedure TConvolutionDataModule.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;

procedure TConvolutionDataModule.IRBlockSizeChanged;
var
  i : Integer;
begin
 i := CeilLog2(FFFTSize);
 if not assigned(FFft)
 {$IFDEF Use_IPPS}
  then FFft := TFftReal2ComplexIPPSFloat32.Create(i)
 {$ELSE}
  then
   begin
    FFft := TFftReal2ComplexNativeFloat32.Create(i);
    FFft.DataOrder := doPackedComplex;
   end
 {$ENDIF}
  else FFft.Order := i;

 FFFTSizeHalf    := FFFTSize div 2;
 FFFTSizeQuarter := FFFTSize div 4;

 ReallocMem(FFilterKernel, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  sr, c : Integer;
  pt    : PSingle;
begin
 if assigned(FFilterKernel) and assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    pt := LoadWAVFileMono(FileName, sr, c, FIRSize);
    ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
    Move(pt^, FFilterKernel^[0], FIRSize * SizeOf(Single));

    CalculatePaddedIRSize;

    CalculateFilterBlockFrequencyResponses;
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TConvolutionDataModule.CalculateFrequencyResponseBlockCount;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;
end;

procedure TConvolutionDataModule.CalculatePaddedIRSize;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;
end;

procedure TConvolutionDataModule.CalculateFilterBlockFrequencyResponses;
var
  TempIR     : PDAVSingleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   // calculate IR part size to be copied
   sz := IRSize - Blocks * FFFTSizeHalf;
   if sz > FFFTSizeHalf then sz := FFFTSizeHalf;

   // build temporary IR part
   move(FFilterKernel^[Blocks * FFFTSizeHalf], TempIR^[0], sz * SizeOf(Single));
   FillChar(TempIR^[sz], (FFFTSize - sz) * SizeOf(Single), 0);

   // perform FFT
   FFft.PerformFFT(FFilterFreqs[Blocks], TempIR);
  end;
end;

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 4 * ecx - 4].Single
  fadd  [edx + 4 * ecx - 4].Single
  fstp  [edx + 4 * ecx - 4].Single
  loop @Start
end;

procedure ComplexMultiply(InplaceBuffer: PDAVComplexSingleFixedArray; Filter: PDAVComplexSingleFixedArray; SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

procedure TConvolutionDataModule.PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 Half := FFFTSizeHalf;

 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   ComplexMultiply(@FConvolved^[0], @FFilterFreqs[Block]^[0], Half);

   FFft.PerformIFFT(PDAVComplexSingleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   MixBuffers_FPU(@FConvolvedTime^[Half], @SignalOut^[Block * Half], Half);
  end;
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel     : Integer;
begin
 // lock processing
 while FSemaphore > 0 do;
 inc(FSemaphore);

 try
  for Channel := 0 to Length(FOutputBuffer) - 1 do
   begin
    PerformConvolution(@Inputs[Channel, 0], FOutputBuffer[Channel]);

    Move(FOutputBuffer[Channel]^[0], Outputs[Channel, BlockModeOverlap], FOffsetSize * SizeOf(Single));
    Move(FOutputBuffer[Channel]^[FOffsetSize], FOutputBuffer[Channel]^[0], (FIRSizePadded - FOffsetSize) * SizeOf(Single));
    FillChar(FOutputBuffer[Channel]^[(FIRSizePadded - FOffsetSize)], FOffsetSize * SizeOf(Single), 0);
   end;
 finally
  dec(FSemaphore);
 end;
end;

end.
