unit DAV_DspConvolution;

interface

{$I DAV_Compiler.INC}
{.$DEFINE Use_IPPS}
{.$DEFINE Use_CUDA}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TCustomConvolution = class(TDspObject)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT                : TFftReal2Complex;
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    procedure CalculateFftSizeVariables; virtual;
    procedure ImpulseResponseChanged; virtual; abstract;
    procedure FFTOrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

  TConvolution32 = class(TCustomConvolution)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure SetIRSizePadded(const Value: Integer);
    procedure SetIRSize(const Value: Integer);
  protected
    FImpulseResponse    : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FInputBuffer        : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;

    FIRSize             : Integer;
    FBlockPosition      : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;

    procedure CalculateFftSizeVariables; override;
    procedure ImpulseResponseChanged; override;
    procedure IRSizePaddedChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray); virtual;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read GetFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32  read GetFft;
    {$ENDIF}{$ENDIF}
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
    property IRSize: Integer read FIRSize write SetIRSize;
  end;

  TConvolution64 = class(TCustomConvolution)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
    procedure SetIRSizePadded(const Value: Integer);
  protected
    FImpulseResponse    : PDAVDoubleFixedArray;
    FFilterFreqs        : array of PDAVComplexDoubleFixedArray;
    FSignalFreq         : PDAVComplexDoubleFixedArray;
    FConvolved          : PDAVComplexDoubleFixedArray;
    FConvolvedTime      : PDAVDoubleFixedArray;
    FInputBuffer        : PDAVDoubleFixedArray;
    FOutputBuffer       : PDAVDoubleFixedArray;

    FIRSize             : Integer;
    FBlockPosition      : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;

    procedure CalculateFftSizeVariables; override;
    procedure ImpulseResponseChanged; override;
    procedure IRSizePaddedChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVDoubleFixedArray); virtual;

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVDoubleFixedArray; const SampleFrames: Integer); virtual;
    procedure LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVDoubleDynArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
  end;

  TCustomLowLatencyConvolution = class(TDspObject)
  end;

  TLowLatencyConvolutionStage32 = class
  private
    function GetCount: Integer;
  protected
    {$IFDEF Use_IPPS}
    FFft                : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft                : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft                : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FOutputPos          : Integer;
    FLatency            : Integer;
    FMod, FModAnd       : Integer;

    FIRSpectrums        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
  public
    constructor Create(const IROrder: Byte);
    destructor Destroy; override;
    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVSingleFixedArray); virtual;
    procedure CalculateIRSpectrums(const IR: PDAVSingleFixedArray; StartPos: Integer; const Latency, Count: Integer);
  published
    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read FFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read FFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32 read FFft;
    {$ENDIF}{$ENDIF}
    property Count: Integer read GetCount;
  end;

  // ToDo: - Input and Output buffers should become circular buffers in this
  //         approach!
  //       - replace FOutputBufferSize with FIRSizePadded and introduce
  //         new substitute for (FOutputBufferSize - FLatency)

  TLowLatencyConvolution32 = class(TCustomLowLatencyConvolution)
  private
    function GetMaximumIRBlockSize: Integer;
    procedure SetMinimumIRBlockOrder(const Value: Byte);
    procedure SetMaximumIRBlockOrder(const Value: Byte);
    procedure MaximumIRBlockOrderChanged;
  protected
    FImpulseResponse     : PDAVSingleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage32;
    FInputBuffer         : PDAVSingleFixedArray;
    FOutputBuffer        : PDAVSingleFixedArray;
    FInputBufferSize     : Integer;
    FOutputBufferSize    : Integer;
    FBlockPosition       : Integer;
    FIRSize              : Integer;
    FIRSizePadded        : Integer;
    FLatency             : Integer;
    FMinimumIRBlockOrder : Byte;
    FMaximumIRBlockOrder : Byte;

    procedure PartitionizeIR; virtual;
    procedure CalculatePaddedIRSize; virtual;
    procedure MinimumIRBlockOrderChanged; virtual;
    procedure ImpulseResponseChanged; virtual;

    property IRSize: Integer read FIRSize;
    property MinimumIRBlockSize: Integer read FLatency;
    property MaximumIRBlockSize: Integer read GetMaximumIRBlockSize;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property MinimumIRBlockOrder: Byte read FMinimumIRBlockOrder write SetMinimumIRBlockOrder;
    property MaximumIRBlockOrder: Byte read FMaximumIRBlockOrder write SetMaximumIRBlockOrder;
    property Latency: Integer read FLatency;
  end;

implementation

uses
  Math, SysUtils;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 4 * ecx - 4].Single
  fadd  [edx + 4 * ecx - 4].Single
  fstp  [edx + 4 * ecx - 4].Single
  loop @Start
end;

procedure MixBuffers_FPU(InBuffer: PDouble; MixBuffer: PDouble; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 8 * ecx - 8].Double
  fadd  [edx + 8 * ecx - 8].Double
  fstp  [edx + 8 * ecx - 8].Double
  loop @Start
end;

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
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

procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexSingleFixedArray); overload;
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
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
  fstp [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add ebx, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single

 pop ebx
end;

procedure ComplexMultiply(InplaceBuffer: PDAVComplexDoubleFixedArray; Filter: PDAVComplexDoubleFixedArray; SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 dec ecx
@Start:
  fld [eax    ].Double  // A.Re
  fld [eax + 8].Double  // A.Im, A.Re
  fld [edx    ].Double  // B.Re, A.Im, A.Re
  fld [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
end;

{ TCustomConvolution }

constructor TCustomConvolution.Create;
begin
 inherited;
end;

destructor TCustomConvolution.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomConvolution.CalculateFftSizeVariables;
begin
 FFFTSize            := FFft.FFTSize;
 FFFTSizeHalf        := FFFTSize shr 1;
end;

function TCustomConvolution.GetFftOrder: Byte;
begin
 result := FFft.Order;
end;

procedure TCustomConvolution.FFTOrderChanged;
begin
 CalculateFftSizeVariables;
 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TCustomConvolution.SetFftOrder(const Value: Byte);
begin
 if FFft.Order <> Value then
  begin
   FFft.Order := Value;
   FFTOrderChanged;
  end;
end;

{ TConvolution32 }

constructor TConvolution32.Create;
begin
 FImpulseResponse    := nil;
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

destructor TConvolution32.Destroy;
var
  i : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 Dispose(FOutputBuffer);
 for i := 0 to Length(FFilterFreqs) - 1
  do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TConvolution32.CalculateFftSizeVariables;
begin
 inherited;
end;

procedure TConvolution32.ImpulseResponseChanged;
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
 FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Single), 0);

 sz := IRSize;
 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   assert(sz > 0);
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   if sz < FFFTSizeHalf then
    begin
     // build temporary IR part
     move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], sz * SizeOf(Single));
     FillChar(TempIR^[FFFTSizeHalf + sz], (FFFTSizeHalf - sz) * SizeOf(Single), 0);
    end
   else move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));

   sz := sz - FFFTSizeHalf;

   // perform FFT
   FFft.PerformFFT(FFilterFreqs[Blocks], TempIR);
  end;
end;

procedure TConvolution32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);

 ImpulseResponseChanged;
end;

{$IFDEF Use_IPPS}
function TConvolution32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TConvolution32.GetFft : TFftReal2ComplexCUDA32;
begin
 result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TConvolution32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TConvolution32.IRSizePaddedChanged;
begin
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TConvolution32.LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
  end
 else
  begin
   ReallocMem(FImpulseResponse, SampleFrames * SizeOf(Single));
   Move(Data^[0], FImpulseResponse^[0], SampleFrames * SizeOf(Single));
   FIRSize := SampleFrames;
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution32.LoadImpulseResponse(const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
end;

procedure TConvolution32.PerformConvolution(SignalIn,
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

   ComplexMultiply(PDAVComplexSingleFixedArray(@FConvolved^[0]),
                   PDAVComplexSingleFixedArray(@FFilterFreqs[Block]^[0]), Half);

   FFft.PerformIFFT(PDAVComplexSingleFixedArray(@FConvolved^[0]), FConvolvedTime);

   // copy and combine
   MixBuffers_FPU(PSingle(@FConvolvedTime^[0]), PSingle(@SignalOut^[Block * Half]), Half);
  end;
end;

procedure TConvolution32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * Sizeof(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], (FIRSizePadded - FFFTSizeHalf) * SizeOf(Single));
    FillChar(FOutputBuffer^[(FIRSizePadded - FFFTSizeHalf)], FFFTSizeHalf * SizeOf(Single), 0);

    PerformConvolution(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * Sizeof(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TConvolution32.SetIRSize(const Value: Integer);
begin
 if FIRSize < Value then
  begin
   ReallocMem(FImpulseResponse, Value * SizeOf(Single));
   FillChar(FImpulseResponse^[FIRSize], (Value - FIRSize) * SizeOf(Single), 0);
   FIRSize := Value;
   ImpulseResponseChanged;
  end
 else
  begin
   FIRSize := Value;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution32.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;


{ TConvolution64 }

constructor TConvolution64.Create;
begin
 FImpulseResponse    := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FIRSize             := 0;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFTOrderChanged;
end;

destructor TConvolution64.Destroy;
var
  i : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 Dispose(FOutputBuffer);
 for i := 0 to Length(FFilterFreqs) - 1
  do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TConvolution64.CalculateFftSizeVariables;
begin
 inherited;
end;

procedure TConvolution64.ImpulseResponseChanged;
var
  TempIR     : PDAVDoubleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Double));

 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));

   // calculate IR part size to be copied
   sz := IRSize - Blocks * FFFTSizeHalf;
   if sz > FFFTSizeHalf then sz := FFFTSizeHalf;

   // build temporary IR part
   move(FImpulseResponse^[Blocks * FFFTSizeHalf], TempIR^[0], sz * SizeOf(Double));
   FillChar(TempIR^[sz], (FFFTSize - sz) * SizeOf(Double), 0);

   // perform FFT
   FFft.PerformFFT(FFilterFreqs[Blocks], TempIR);
  end;
end;

procedure TConvolution64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Double));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Double), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Double), 0);
end;

{$IFDEF Use_IPPS}
function TConvolution64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TConvolution64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TConvolution64.IRSizePaddedChanged;
begin
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Double), 0);
end;

procedure TConvolution64.LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   ImpulseResponseChanged;
  end;
end;

procedure TConvolution64.LoadImpulseResponse(const Data: TDAVDoubleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
end;

procedure TConvolution64.PerformConvolution(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 Half := FFFTSizeHalf;

 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));

   ComplexMultiply(PDAVComplexDoubleFixedArray(@FConvolved^[0]),
                   PDAVComplexDoubleFixedArray(@FFilterFreqs[Block]^[0]), Half);

   FFft.PerformIFFT(PDAVComplexDoubleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   MixBuffers_FPU(PDouble(@FConvolvedTime^[Half]), PDouble(@SignalOut^[Block * Half]), Half);
  end;
end;

procedure TConvolution64.ProcessBlock(const Input,
  Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * Sizeof(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FFFTSizeHalf], FOutputBuffer^[0], (FIRSizePadded - FFFTSizeHalf) * SizeOf(Double));
    FillChar(FOutputBuffer^[(FIRSizePadded - FFFTSizeHalf)], FFFTSizeHalf * SizeOf(Double), 0);

    PerformConvolution(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * Sizeof(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TConvolution64.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;

{ TLowLatencyConvolutionStage32 }

constructor TLowLatencyConvolutionStage32.Create(const IROrder: Byte);
begin
 FSignalFreq    := nil;
 FConvolvedTime := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(IROrder + 1);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(IROrder + 1);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(IROrder + 1);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFft.AutoScaleType := astDivideInvByN;
 FFTOrderChanged;
end;

destructor TLowLatencyConvolutionStage32.Destroy;
var
  i : Integer;
begin
 Dispose(FSignalFreq);
 Dispose(FConvolvedTime);
 for i := 0 to Length(FIRSpectrums) - 1
  do Dispose(FIRSpectrums[i]);
 FreeAndNil(FFft);
 inherited;
end;

procedure TLowLatencyConvolutionStage32.FFTOrderChanged;
begin
 FFFTSize            := FFft.FFTSize;
 FFFTSizeHalf        := FFFTSize shr 1;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);
end;

function TLowLatencyConvolutionStage32.GetCount: Integer;
begin
 result := Length(FIRSpectrums);
end;

procedure TLowLatencyConvolutionStage32.CalculateIRSpectrums(
  const IR: PDAVSingleFixedArray; StartPos: Integer; const Latency, Count: Integer);
var
  TempIR : PDAVSingleFixedArray;
  Blocks : Integer;
begin
 // get temporary buffer to store zero padded IR parts
 GetMem(TempIR, FFFTSize * SizeOf(Single));

 // zeropad first half
 FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Single), 0);

 FOutputPos := StartPos;
 FLatency   := Latency;
 FModAnd    := (FFFTSizeHalf div Latency) - 1;

 SetLength(FIRSpectrums, Count);
 for Blocks := 0 to Length(FIRSpectrums) - 1 do
  begin
   ReallocMem(FIRSpectrums[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   // build temporary IR part
   Move(IR^[StartPos + Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));

   // perform FFT
   FFft.PerformFFT(FIRSpectrums[Blocks], TempIR);
  end;
end;

procedure TLowLatencyConvolutionStage32.PerformConvolution(const SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
  Dest   : PDAVComplexSingleFixedArray;
begin
 if FMod = 0 then
  begin
   FFft.PerformFFT(FSignalFreq, @SignalIn[-FFFTSize]);
   Half := FFFTSizeHalf;

   if Length(FIRSpectrums) = 1 then Dest := FSignalFreq
    else
     begin
      move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
      Dest := FConvolved;
     end;

   for Block := 0 to Length(FIRSpectrums) - 1 do
    begin
     // complex multiply with frequency response
     ComplexMultiply(PDAVComplexSingleFixedArray(@FSignalFreq^[0]),
                     PDAVComplexSingleFixedArray(@FIRSpectrums[Block]^[0]),
                     Half, Dest);

     // transfer to frequency domain
     FFft.PerformIFFT(Dest, FConvolvedTime);

     // copy and combine
     MixBuffers_FPU(PSingle(@FConvolvedTime^[0]), PSingle(@SignalOut^[FOutputPos + FLatency - FFFTSizeHalf + Block * Half]), Half);
    end;
  end;

 FMod := (FMod + 1) and FModAnd
end;


{ TLowLatencyConvolution32 }

constructor TLowLatencyConvolution32.Create;
begin
 inherited;
 FImpulseResponse      := nil;
 FIRSizePadded         := 0;
 FIRSize               := 0;
 FMinimumIRBlockOrder  := 7;
 FMaximumIRBlockOrder  := 15;
 MinimumIRBlockOrderChanged;
end;

destructor TLowLatencyConvolution32.Destroy;
var
  Stage : Integer;
begin
 Dispose(FImpulseResponse);
 Dispose(FOutputBuffer);
 Dispose(FInputBuffer);
 for Stage := 0 to Length(FConvStages) - 1
  do FreeAndNil(FConvStages[Stage]);
 inherited;
end;

function TLowLatencyConvolution32.GetMaximumIRBlockSize: Integer;
begin
 result := 1 shl FMaximumIRBlockOrder;
end;

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   ImpulseResponseChanged;
  end;
end;

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data, Length(Data));
end;

procedure TLowLatencyConvolution32.SetMaximumIRBlockOrder(const Value: Byte);
begin
 if Value < FMinimumIRBlockOrder
  then raise Exception.Create(RCStrIRBlockOrderError);
 if FMaximumIRBlockOrder <> Value then
  begin
   FMaximumIRBlockOrder := Value;
   MaximumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution32.SetMinimumIRBlockOrder(const Value: Byte);
begin
 if FMinimumIRBlockOrder <> Value then
  begin
   FMinimumIRBlockOrder := Value;
   MinimumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution32.ImpulseResponseChanged;
begin
 CalculatePaddedIRSize;
end;

procedure TLowLatencyConvolution32.MaximumIRBlockOrderChanged;
begin
 PartitionizeIR;
end;

procedure TLowLatencyConvolution32.MinimumIRBlockOrderChanged;
begin
 FLatency := 1 shl FMinimumIRBlockOrder;
 CalculatePaddedIRSize;
end;

function BitCountToBits(const BitCount: Byte): Integer;
begin
 result := (2 shl BitCount) - 1;
end;

procedure TLowLatencyConvolution32.PartitionizeIR;
var
  c, cnt    : Integer;
  ResIRSize : Integer;
  StartPos  : Integer;
  MaxIROrd  : Byte;
begin
 // clear existing convolution stages
 for c := 0 to Length(FConvStages) - 1 do FreeAndNil(FConvStages[c]);
 if FIRSizePadded = 0 then exit;

 // calculate maximum FFT order (to create proper buffers later)
 MaxIROrd := TruncLog2(FIRSizePadded + MinimumIRBlockSize) - 1;

 // at least one block of each fft size is necessary
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // check if highest block is only convolved once otherwise decrease
 if (ResIRSize and (1 shl MaxIROrd)) shr MaxIROrd = 0
  then Dec(MaxIROrd);

 // check if max. possible IR block order exceeds the bound and clip
 if MaxIROrd > FMaximumIRBlockOrder
  then MaxIROrd := FMaximumIRBlockOrder;

 // recalculate since MaxIROrd could have changed
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // initialize convolution stage array
 SetLength(FConvStages, MaxIROrd - FMinimumIRBlockOrder + 1);

 StartPos := 0;
 for c := FMinimumIRBlockOrder to MaxIROrd - 1 do
  begin
   FConvStages[c - FMinimumIRBlockOrder] := TLowLatencyConvolutionStage32.Create(c);
   cnt := 1 + (ResIRSize and (1 shl c)) shr c;
   FConvStages[c - FMinimumIRBlockOrder].CalculateIRSpectrums(@FImpulseResponse^[0], StartPos, FLatency, cnt);
   StartPos := StartPos + cnt * (1 shl c);
   ResIRSize := ResIRSize - (cnt - 1) * (1 shl c);
  end;

 // last stage
 FConvStages[Length(FConvStages) - 1] := TLowLatencyConvolutionStage32.Create(MaxIROrd);
 cnt := 1 + ResIRSize div (1 shl MaxIROrd);
 FConvStages[Length(FConvStages) - 1].CalculateIRSpectrums(@FImpulseResponse^[0], StartPos, FLatency, cnt);


 FInputBufferSize := 2 shl MaxIROrd;
 ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Single));
 FillChar(FInputBuffer^, FInputBufferSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolution32.CalculatePaddedIRSize;
begin
 FIRSizePadded := MinimumIRBlockSize * ((IRSize + MinimumIRBlockSize - 1) div MinimumIRBlockSize);

 // zero pad filter
 ReallocMem(FImpulseResponse, FIRSizePadded * SizeOf(Single));
 FillChar(FImpulseResponse^[FIRSize], (FIRSizePadded - FIRSize) * SizeOf(Single), 0);

 FOutputBufferSize := FIRSizePadded;
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);

 PartitionizeIR;
end;

procedure TLowLatencyConvolution32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FInputBufferSize - FLatency + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FInputBufferSize - FLatency + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], (FOutputBufferSize - FLatency) * SizeOf(Single));
    FillChar(FOutputBuffer^[(FOutputBufferSize - FLatency)], FLatency * SizeOf(Single), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1
     do FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], (FInputBufferSize - FLatency) * Sizeof(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

end.
