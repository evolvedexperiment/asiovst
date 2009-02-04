unit DAV_DspConvolution;

interface

{$I DAV_Compiler.INC}
{$DEFINE Use_IPPS}
{.$DEFINE Use_CUDA}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TCustomConvolution32 = class(TDspObject)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
    procedure SetIRSizePadded(const Value: Integer);
  protected
    FFilterKernel       : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;

    {$IFDEF Use_IPPS}
    FFft          : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft          : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft          : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}

    FIRSize             : Integer;
    FOffsetSize         : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FFFTSizeQuarter     : Integer;
    procedure CalculateFftSizeVariables; virtual;
    procedure ImpulseResponseChanged; virtual;
    procedure IRSizePaddedChanged; virtual;
    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray); virtual;

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

implementation

uses
  SysUtils;

{ TCustomConvolution32 }

constructor TCustomConvolution32.Create;
begin
 FFilterKernel       := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FIRSize             := 0;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFTOrderChanged;
end;

destructor TCustomConvolution32.Destroy;
var
  i : Integer;
begin
 Dispose(FFilterKernel);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 Dispose(FOutputBuffer);
 for i := 0 to Length(FFilterFreqs) - 1
  do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomConvolution32.CalculateFftSizeVariables;
begin
 FFFTSize            := FFft.FFTSize;
 FFFTSizeHalf        := FFFTSize shr 1;
 FFFTSizeQuarter     := FFFTSize shr 2;
 FOffsetSize         := FFFTSize - FFFTSizeHalf;
end;

function TCustomConvolution32.GetFftOrder: Byte;
begin
 result := FFft.Order;
end;

procedure TCustomConvolution32.ImpulseResponseChanged;
var
  TempIR     : PDAVSingleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;

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
(*
*)
end;

procedure TCustomConvolution32.FFTOrderChanged;
var
  i : Integer;
begin
 CalculateFftSizeVariables;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TCustomConvolution32.IRSizePaddedChanged;
begin
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TCustomConvolution32.LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FFilterKernel^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FFilterKernel^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
   ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
   Move(Data^[0], FFilterKernel^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end;
end;

procedure TCustomConvolution32.LoadImpulseResponse(const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
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

procedure TCustomConvolution32.PerformConvolution(SignalIn,
  SignalOut: PDAVSingleFixedArray);
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

procedure TCustomConvolution32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 PerformConvolution(@Input[0], FOutputBuffer);

 Move(FOutputBuffer^[0], Output[FFFTSizeHalf], FOffsetSize * SizeOf(Single));
 Move(FOutputBuffer^[FOffsetSize], FOutputBuffer^[0], (FIRSizePadded - FOffsetSize) * SizeOf(Single));
 FillChar(FOutputBuffer^[(FIRSizePadded - FOffsetSize)], FOffsetSize * SizeOf(Single), 0);
end;

procedure TCustomConvolution32.SetFftOrder(const Value: Byte);
begin
 if FFft.Order <> Value then
  begin
   FFft.Order := Value;
   FFTOrderChanged;
  end;
end;

procedure TCustomConvolution32.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;

end.
