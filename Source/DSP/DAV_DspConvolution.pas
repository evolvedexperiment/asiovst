unit DAV_DspConvolution;

interface

{$I DAV_Compiler.INC}

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
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
    destructor Destroy; override;
    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVSingleFixedArray); virtual;
    procedure CalculateIRSpectrums(const IR: PDAVSingleFixedArray);
  published
    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read FFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read FFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32 read FFft;
    {$ENDIF}{$ENDIF}
    property Count: Integer read GetCount;
    property Latency: Integer read FLatency;
  end;

  // ToDo: - Input and Output buffers should become circular buffers in this
  //         approach!

  TLowLatencyConvolution32 = class(TCustomLowLatencyConvolution)
  private
    function GetMaximumIRBlockSize: Integer;
    procedure SetMinimumIRBlockOrder(const Value: Byte);
    procedure SetMaximumIRBlockOrder(const Value: Byte);
    procedure SetIRSizePadded(const Value: Integer);
    function CalculatePaddedIRSize: Integer;
    procedure AllocatePaddedIRSizeDependentBuffers;
    procedure InputBufferSizeChanged;
    procedure OutputBufferSizeChanged;
    procedure CalculateLatency;
  protected
    FImpulseResponse     : PDAVSingleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage32;
    FInputBuffer         : PDAVSingleFixedArray;
    FOutputBuffer        : PDAVSingleFixedArray;
    FInputBufferSize     : Integer;
    FOutputHistorySize   : Integer;
    FInputHistorySize    : Integer;
    FBlockPosition       : Integer;
    FIRSize              : Integer;
    FIRSizePadded        : Integer;
    FLatency             : Integer;
    FMinimumIRBlockOrder : Byte;
    FMaximumIRBlockOrder : Byte;

    procedure BuildIRSpectrums; virtual;
    procedure MinimumIRBlockOrderChanged; virtual;
    procedure MaximumIRBlockOrderChanged; virtual;
    procedure PartitionizeIR; virtual;
    procedure PaddedIRSizeChanged; virtual;

    property MinimumIRBlockSize: Integer read FLatency;
    property MaximumIRBlockSize: Integer read GetMaximumIRBlockSize;
    property PaddedIRSize: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure ProcessBlock(const Inplace : PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVSingleDynArray); overload; virtual;
  published
    property MinimumIRBlockOrder: Byte read FMinimumIRBlockOrder write SetMinimumIRBlockOrder;
    property MaximumIRBlockOrder: Byte read FMaximumIRBlockOrder write SetMaximumIRBlockOrder;
    property Latency: Integer read FLatency;
    property IRSize: Integer read FIRSize;
  end;

  TLowLatencyConvolutionStereo32 = class(TLowLatencyConvolution32)
  protected
    FInputBuffer2  : PDAVSingleFixedArray;
    FOutputBuffer2 : PDAVSingleFixedArray;
    procedure PartitionizeIR; override;
    procedure PaddedIRSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Integer); reintroduce; virtual;
  end;

  TLowLatencyConvolutionStage64 = class
  private
    function GetCount: Integer;
  protected
    {$IFDEF Use_IPPS}
    FFft                : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    FFft                : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FOutputPos          : Integer;
    FLatency            : Integer;
    FMod, FModAnd       : Integer;

    FIRSpectrums        : array of PDAVComplexDoubleFixedArray;
    FSignalFreq         : PDAVComplexDoubleFixedArray;
    FConvolved          : PDAVComplexDoubleFixedArray;
    FConvolvedTime      : PDAVDoubleFixedArray;
  public
    constructor Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
    destructor Destroy; override;
    procedure FFTOrderChanged; virtual;
    procedure PerformConvolution(const SignalIn, SignalOut: PDAVDoubleFixedArray); virtual;
    procedure CalculateIRSpectrums(const IR: PDAVDoubleFixedArray);
  published
    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read FFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read FFft;
    {$ENDIF}
    property Count: Integer read GetCount;
    property Latency: Integer read FLatency;
  end;

  TLowLatencyConvolution64 = class(TCustomLowLatencyConvolution)
  private
    function GetMaximumIRBlockSize: Integer;
    procedure SetMinimumIRBlockOrder(const Value: Byte);
    procedure SetMaximumIRBlockOrder(const Value: Byte);
    procedure SetIRSizePadded(const Value: Integer);
    function CalculatePaddedIRSize: Integer;
    procedure AllocatePaddedIRSizeDependentBuffers;
    procedure InputBufferSizeChanged;
    procedure OutputBufferSizeChanged;
    procedure CalculateLatency;
  protected
    FImpulseResponse     : PDAVDoubleFixedArray;
    FConvStages          : array of TLowLatencyConvolutionStage64;
    FInputBuffer         : PDAVDoubleFixedArray;
    FOutputBuffer        : PDAVDoubleFixedArray;
    FInputBufferSize     : Integer;
    FOutputHistorySize   : Integer;
    FInputHistorySize    : Integer;
    FBlockPosition       : Integer;
    FIRSize              : Integer;
    FIRSizePadded        : Integer;
    FLatency             : Integer;
    FMinimumIRBlockOrder : Byte;
    FMaximumIRBlockOrder : Byte;
    procedure BuildIRSpectrums; virtual;
    procedure MinimumIRBlockOrderChanged; virtual;
    procedure MaximumIRBlockOrderChanged; virtual;
    procedure PartitionizeIR; virtual;
    procedure PaddedIRSizeChanged; virtual;
    property MinimumIRBlockSize: Integer read FLatency;
    property MaximumIRBlockSize: Integer read GetMaximumIRBlockSize;
    property PaddedIRSize: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output : PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure ProcessBlock(const Inplace : PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer); overload; virtual;
    procedure LoadImpulseResponse(const Data: TDAVDoubleDynArray); overload; virtual;
  published
    property MinimumIRBlockOrder: Byte read FMinimumIRBlockOrder write SetMinimumIRBlockOrder;
    property MaximumIRBlockOrder: Byte read FMaximumIRBlockOrder write SetMaximumIRBlockOrder;
    property Latency: Integer read FLatency;
    property IRSize: Integer read FIRSize;
  end;

  TLowLatencyConvolutionStereo64 = class(TLowLatencyConvolution64)
  protected
    FInputBuffer2  : PDAVDoubleFixedArray;
    FOutputBuffer2 : PDAVDoubleFixedArray;
    procedure PartitionizeIR; override;
    procedure PaddedIRSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Integer); reintroduce; virtual;
  end;

implementation

uses
  Math, SysUtils, DAV_BlockRoutines;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

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

constructor TLowLatencyConvolutionStage32.Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
begin
 FSignalFreq    := nil;
 FConvolvedTime := nil;
 FOutputPos     := StartPos;
 FLatency       := Latency;

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
 SetLength(FIRSpectrums, Count);
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

procedure TLowLatencyConvolutionStage32.CalculateIRSpectrums(const IR: PDAVSingleFixedArray);
var
  TempIR   : PDAVSingleFixedArray;
  Blocks   : Integer;
begin
 // get temporary buffer to store zero padded IR parts
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 try
  // zeropad first half
  FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Single), 0);

  FModAnd  := (FFFTSizeHalf div FLatency) - 1;

  for Blocks := 0 to Length(FIRSpectrums) - 1 do
   begin
    ReallocMem(FIRSpectrums[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

    // build temporary IR part
    Move(IR^[FOutputPos + Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));

    // perform FFT
    FFft.PerformFFT(FIRSpectrums[Blocks], TempIR);
   end;
 finally
  Dispose(TempIR);
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
 FMaximumIRBlockOrder  := 16;
 FLatency              := 1 shl FMinimumIRBlockOrder;
 FInputBufferSize      := 2 shl FMaximumIRBlockOrder;
 InputBufferSizeChanged;
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

procedure TLowLatencyConvolution32.InputBufferSizeChanged;
begin
 FInputHistorySize     := FInputBufferSize - FLatency;
 ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Single));
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolution32.OutputBufferSizeChanged;
begin
 FOutputHistorySize := (FIRSizePadded - FLatency);
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Single), 0);
end;

function TLowLatencyConvolution32.GetMaximumIRBlockSize: Integer;
begin
 result := 1 shl FMaximumIRBlockOrder;
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

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   BuildIRSpectrums;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Single));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Single));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution32.LoadImpulseResponse(
  const Data: TDAVSingleDynArray);
begin
 LoadImpulseResponse(@Data[0], Length(Data));
end;

procedure TLowLatencyConvolution32.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   PaddedIRSizeChanged;
  end;
end;

procedure TLowLatencyConvolution32.AllocatePaddedIRSizeDependentBuffers;
begin
 // zero pad filter
 ReallocMem(FImpulseResponse, FIRSizePadded * SizeOf(Single));
 FillChar(FImpulseResponse^[FIRSize], (FIRSizePadded - FIRSize) * SizeOf(Single), 0);

 // reallocate output buffer
 OutputBufferSizeChanged;
end;

procedure TLowLatencyConvolution32.PaddedIRSizeChanged;
begin
 AllocatePaddedIRSizeDependentBuffers;

 // re partitionize IR
 PartitionizeIR;
end;

function TLowLatencyConvolution32.CalculatePaddedIRSize: Integer;
begin
 result := MinimumIRBlockSize * ((IRSize + MinimumIRBlockSize - 1) div MinimumIRBlockSize);
end;

procedure TLowLatencyConvolution32.CalculateLatency;
begin
 FLatency           := 1 shl FMinimumIRBlockOrder;
 FInputHistorySize  := FInputBufferSize - FLatency;
 FOutputHistorySize := FIRSizePadded - FLatency;
end;

procedure TLowLatencyConvolution32.MinimumIRBlockOrderChanged;
begin
 CalculateLatency;

 if PaddedIRSize <> CalculatePaddedIRSize then
  begin
   PaddedIRSize := CalculatePaddedIRSize; // implicitely partitionize IR
   BuildIRSpectrums;
  end
 else
  begin
   PartitionizeIR;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution32.MaximumIRBlockOrderChanged;
begin
 PartitionizeIR;
 BuildIRSpectrums;
end;

procedure TLowLatencyConvolution32.BuildIRSpectrums;
var
  Stage : Integer;
begin
 for Stage := 0 to Length(FConvStages) - 1
  do FConvStages[Stage].CalculateIRSpectrums(FImpulseResponse);
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

 assert(FMaximumIRBlockOrder >= FMinimumIRBlockOrder);

 // calculate maximum FFT order (to create proper buffers later)
 MaxIROrd := TruncLog2(FIRSizePadded + MinimumIRBlockSize) - 1;

 // at least one block of each fft size is necessary
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // check if highest block is only convolved once otherwise decrease
 if ((ResIRSize and (1 shl MaxIROrd)) shr MaxIROrd = 0) and (MaxIROrd > FMinimumIRBlockOrder)
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
   cnt := 1 + (ResIRSize and (1 shl c)) shr c;
   FConvStages[c - FMinimumIRBlockOrder] := TLowLatencyConvolutionStage32.Create(c, StartPos, FLatency, cnt);
   StartPos := StartPos + cnt * (1 shl c);
   ResIRSize := ResIRSize - (cnt - 1) * (1 shl c);
  end;

 // last stage
 cnt := 1 + ResIRSize div (1 shl MaxIROrd);
 FConvStages[Length(FConvStages) - 1] := TLowLatencyConvolutionStage32.Create(MaxIROrd, StartPos, FLatency, cnt);

 FInputBufferSize := 2 shl MaxIROrd;
 InputBufferSizeChanged;
end;

procedure TLowLatencyConvolution32.ProcessBlock(
  const Inplace: PDAVSingleFixedArray; const SampleFrames: Integer);
begin
 ProcessBlock(Inplace, Inplace, SampleFrames);
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
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Single));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Single), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1
     do FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * Sizeof(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

{ TLowLatencyConvolutionStereo32 }

constructor TLowLatencyConvolutionStereo32.Create;
begin
 inherited;
 FInputBuffer2 := nil;
 FOutputBuffer2 := nil;
end;

destructor TLowLatencyConvolutionStereo32.Destroy;
begin
 Dispose(FInputBuffer2);
 Dispose(FOutputBuffer2);
 inherited;
end;

procedure TLowLatencyConvolutionStereo32.PaddedIRSizeChanged;
begin
 inherited;
 ReallocMem(FOutputBuffer2, FIRSizePadded * SizeOf(Single));
 FillChar(FOutputBuffer2^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolutionStereo32.PartitionizeIR;
begin
 inherited;
 ReallocMem(FInputBuffer2, FInputBufferSize * SizeOf(Single));
 FillChar(FInputBuffer2^, FInputBufferSize * SizeOf(Single), 0);
end;

procedure TLowLatencyConvolutionStereo32.ProcessBlock(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Single));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Single));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Single));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Single));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Single));
    Move(FOutputBuffer2^[FLatency], FOutputBuffer2^[0], FOutputHistorySize * SizeOf(Single));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Single), 0);
    FillChar(FOutputBuffer2^[FOutputHistorySize], FLatency * SizeOf(Single), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1 do
     with FConvStages[Part] do
      begin
       PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);
       FMod := (FMod + FModAnd) and FModAnd;
       PerformConvolution(@FInputBuffer2[FInputBufferSize], FOutputBuffer2);
      end;

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * Sizeof(Single));
    Move(FInputBuffer2[FLatency], FInputBuffer2[0], FInputHistorySize * Sizeof(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;


{ TLowLatencyConvolutionStage64 }

constructor TLowLatencyConvolutionStage64.Create(const IROrder: Byte; const StartPos, Latency, Count: Integer);
begin
 FSignalFreq    := nil;
 FConvolvedTime := nil;
 FOutputPos     := StartPos;
 FLatency       := Latency;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(IROrder + 1);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA64.Create(IROrder + 1);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(IROrder + 1);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFft.AutoScaleType := astDivideInvByN;
 FFTOrderChanged;
 SetLength(FIRSpectrums, Count);
end;

destructor TLowLatencyConvolutionStage64.Destroy;
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

procedure TLowLatencyConvolutionStage64.FFTOrderChanged;
begin
 FFFTSize            := FFft.FFTSize;
 FFFTSizeHalf        := FFFTSize shr 1;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
 ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Double));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
 FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Double), 0);
end;

function TLowLatencyConvolutionStage64.GetCount: Integer;
begin
 result := Length(FIRSpectrums);
end;

procedure TLowLatencyConvolutionStage64.CalculateIRSpectrums(const IR: PDAVDoubleFixedArray);
var
  TempIR   : PDAVDoubleFixedArray;
  Blocks   : Integer;
begin
 // get temporary buffer to store zero padded IR parts
 GetMem(TempIR, FFFTSize * SizeOf(Double));
 try
  // zeropad first half
  FillChar(TempIR^[0], FFFTSizeHalf * SizeOf(Double), 0);

  FModAnd  := (FFFTSizeHalf div FLatency) - 1;

  for Blocks := 0 to Length(FIRSpectrums) - 1 do
   begin
    ReallocMem(FIRSpectrums[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));

    // build temporary IR part
    Move(IR^[FOutputPos + Blocks * FFFTSizeHalf], TempIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Double));

    // perform FFT
    FFft.PerformFFT(FIRSpectrums[Blocks], TempIR);
   end;
 finally
  Dispose(TempIR);
 end;
end;

procedure TLowLatencyConvolutionStage64.PerformConvolution(const SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
  Dest   : PDAVComplexDoubleFixedArray;
begin
 if FMod = 0 then
  begin
   FFft.PerformFFT(FSignalFreq, @SignalIn[-FFFTSize]);
   Half := FFFTSizeHalf;

   if Length(FIRSpectrums) = 1 then Dest := FSignalFreq
    else
     begin
      move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
      Dest := FConvolved;
     end;

   for Block := 0 to Length(FIRSpectrums) - 1 do
    begin
     // complex multiply with frequency response
     ComplexMultiply(PDAVComplexDoubleFixedArray(@FSignalFreq^[0]),
                     PDAVComplexDoubleFixedArray(@FIRSpectrums[Block]^[0]),
                     Half, Dest);

     // transfer to frequency domain
     FFft.PerformIFFT(Dest, FConvolvedTime);

     // copy and combine
     MixBuffers_FPU(PDouble(@FConvolvedTime^[0]), PDouble(@SignalOut^[FOutputPos + FLatency - FFFTSizeHalf + Block * Half]), Half);
    end;
  end;

 FMod := (FMod + 1) and FModAnd
end;


{ TLowLatencyConvolution64 }

constructor TLowLatencyConvolution64.Create;
begin
 inherited;
 FImpulseResponse      := nil;
 FIRSizePadded         := 0;
 FIRSize               := 0;
 FMinimumIRBlockOrder  := 7;
 FMaximumIRBlockOrder  := 16;
 FLatency              := 1 shl FMinimumIRBlockOrder;
 FInputBufferSize      := 2 shl FMaximumIRBlockOrder;
 InputBufferSizeChanged;
end;

destructor TLowLatencyConvolution64.Destroy;
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

procedure TLowLatencyConvolution64.InputBufferSizeChanged;
begin
 FInputHistorySize     := FInputBufferSize - FLatency;
 ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Double));
 FillChar(FInputBuffer^[0], FInputBufferSize * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolution64.OutputBufferSizeChanged;
begin
 FOutputHistorySize := (FIRSizePadded - FLatency);
 ReallocMem(FOutputBuffer, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer^[0], FIRSizePadded * SizeOf(Double), 0);
end;

function TLowLatencyConvolution64.GetMaximumIRBlockSize: Integer;
begin
 result := 1 shl FMaximumIRBlockOrder;
end;

procedure TLowLatencyConvolution64.SetMaximumIRBlockOrder(const Value: Byte);
begin
 if Value < FMinimumIRBlockOrder
  then raise Exception.Create(RCStrIRBlockOrderError);
 if FMaximumIRBlockOrder <> Value then
  begin
   FMaximumIRBlockOrder := Value;
   MaximumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution64.SetMinimumIRBlockOrder(const Value: Byte);
begin
 if FMinimumIRBlockOrder <> Value then
  begin
   FMinimumIRBlockOrder := Value;
   MinimumIRBlockOrderChanged;
  end;
end;

procedure TLowLatencyConvolution64.LoadImpulseResponse(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 if FIRSize = SampleFrames then
  begin
   // size equal, only copy data and recalculate FFT frequency blocks
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   BuildIRSpectrums;
  end else
 if FIRSize > SampleFrames then
  begin
   // new size smaller than previous, dispose unused memory at the end
   FIRSize := SampleFrames;
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
  end
 else
  begin
   FIRSize := SampleFrames;
   ReallocMem(FImpulseResponse, FIRSize * SizeOf(Double));
   Move(Data^[0], FImpulseResponse^[0], FIRSize * SizeOf(Double));
   PaddedIRSize := CalculatePaddedIRSize;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution64.LoadImpulseResponse(
  const Data: TDAVDoubleDynArray);
begin
 LoadImpulseResponse(@Data[0], Length(Data));
end;

procedure TLowLatencyConvolution64.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   PaddedIRSizeChanged;
  end;
end;

procedure TLowLatencyConvolution64.AllocatePaddedIRSizeDependentBuffers;
begin
 // zero pad filter
 ReallocMem(FImpulseResponse, FIRSizePadded * SizeOf(Double));
 FillChar(FImpulseResponse^[FIRSize], (FIRSizePadded - FIRSize) * SizeOf(Double), 0);

 // reallocate output buffer
 OutputBufferSizeChanged;
end;

procedure TLowLatencyConvolution64.PaddedIRSizeChanged;
begin
 AllocatePaddedIRSizeDependentBuffers;

 // re partitionize IR
 PartitionizeIR;
end;

function TLowLatencyConvolution64.CalculatePaddedIRSize: Integer;
begin
 result := MinimumIRBlockSize * ((IRSize + MinimumIRBlockSize - 1) div MinimumIRBlockSize);
end;

procedure TLowLatencyConvolution64.CalculateLatency;
begin
 FLatency           := 1 shl FMinimumIRBlockOrder;
 FInputHistorySize  := FInputBufferSize - FLatency;
 FOutputHistorySize := FIRSizePadded - FLatency;
end;

procedure TLowLatencyConvolution64.MinimumIRBlockOrderChanged;
begin
 CalculateLatency;

 if PaddedIRSize <> CalculatePaddedIRSize then
  begin
   PaddedIRSize := CalculatePaddedIRSize; // implicitely partitionize IR
   BuildIRSpectrums;
  end
 else
  begin
   PartitionizeIR;
   BuildIRSpectrums;
  end;
end;

procedure TLowLatencyConvolution64.MaximumIRBlockOrderChanged;
begin
 PartitionizeIR;
 BuildIRSpectrums;
end;

procedure TLowLatencyConvolution64.BuildIRSpectrums;
var
  Stage : Integer;
begin
 for Stage := 0 to Length(FConvStages) - 1
  do FConvStages[Stage].CalculateIRSpectrums(FImpulseResponse);
end;

procedure TLowLatencyConvolution64.PartitionizeIR;
var
  c, cnt    : Integer;
  ResIRSize : Integer;
  StartPos  : Integer;
  MaxIROrd  : Byte;
begin
 // clear existing convolution stages
 for c := 0 to Length(FConvStages) - 1 do FreeAndNil(FConvStages[c]);
 if FIRSizePadded = 0 then exit;

 assert(FMaximumIRBlockOrder >= FMinimumIRBlockOrder);

 // calculate maximum FFT order (to create proper buffers later)
 MaxIROrd := TruncLog2(FIRSizePadded + MinimumIRBlockSize) - 1;

 // at least one block of each fft size is necessary
 ResIRSize := FIRSizePadded - (BitCountToBits(MaxIROrd) - BitCountToBits(FMinimumIRBlockOrder - 1));

 // check if highest block is only convolved once otherwise decrease
 if ((ResIRSize and (1 shl MaxIROrd)) shr MaxIROrd = 0) and (MaxIROrd > FMinimumIRBlockOrder)
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
   cnt := 1 + (ResIRSize and (1 shl c)) shr c;
   FConvStages[c - FMinimumIRBlockOrder] := TLowLatencyConvolutionStage64.Create(c, StartPos, FLatency, cnt);
   StartPos := StartPos + cnt * (1 shl c);
   ResIRSize := ResIRSize - (cnt - 1) * (1 shl c);
  end;

 // last stage
 cnt := 1 + ResIRSize div (1 shl MaxIROrd);
 FConvStages[Length(FConvStages) - 1] := TLowLatencyConvolutionStage64.Create(MaxIROrd, StartPos, FLatency, cnt);

 FInputBufferSize := 2 shl MaxIROrd;
 InputBufferSizeChanged;
end;

procedure TLowLatencyConvolution64.ProcessBlock(
  const Inplace: PDAVDoubleFixedArray; const SampleFrames: Integer);
begin
 ProcessBlock(Inplace, Inplace, SampleFrames);
end;

procedure TLowLatencyConvolution64.ProcessBlock(const Input,
  Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Double));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Double), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1
     do FConvStages[Part].PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * Sizeof(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

{ TLowLatencyConvolutionStereo64 }

constructor TLowLatencyConvolutionStereo64.Create;
begin
 inherited;
 FInputBuffer2 := nil;
 FOutputBuffer2 := nil;
end;

destructor TLowLatencyConvolutionStereo64.Destroy;
begin
 Dispose(FInputBuffer2);
 Dispose(FOutputBuffer2);
 inherited;
end;

procedure TLowLatencyConvolutionStereo64.PaddedIRSizeChanged;
begin
 inherited;
 ReallocMem(FOutputBuffer2, FIRSizePadded * SizeOf(Double));
 FillChar(FOutputBuffer2^[0], FIRSizePadded * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolutionStereo64.PartitionizeIR;
begin
 inherited;
 ReallocMem(FInputBuffer2, FInputBufferSize * SizeOf(Double));
 FillChar(FInputBuffer2^, FInputBufferSize * SizeOf(Double), 0);
end;

procedure TLowLatencyConvolutionStereo64.ProcessBlock(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Part            : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FLatency then
   begin
    // copy to ring buffer only
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));

    // increase block position and break
    inc(FBlockPosition, SampleFrames - CurrentPosition);
    break;
   end
  else
   begin
    Move(Left^[CurrentPosition], FInputBuffer2^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Double));
    Move(Right^[CurrentPosition], FInputBuffer^[FInputHistorySize + FBlockPosition], (FLatency - FBlockPosition) * Sizeof(Double));
    Move(FOutputBuffer2^[FBlockPosition], Left^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Double));
    Move(FOutputBuffer^[FBlockPosition], Right^[CurrentPosition], (FLatency - FBlockPosition) * Sizeof(Double));

    // discard already used output buffer part and make space for new data
    Move(FOutputBuffer^[FLatency], FOutputBuffer^[0], FOutputHistorySize * SizeOf(Double));
    Move(FOutputBuffer2^[FLatency], FOutputBuffer2^[0], FOutputHistorySize * SizeOf(Double));
    FillChar(FOutputBuffer^[FOutputHistorySize], FLatency * SizeOf(Double), 0);
    FillChar(FOutputBuffer2^[FOutputHistorySize], FLatency * SizeOf(Double), 0);

    // actually perform partitioned convolution
    for Part := 0 to Length(FConvStages) - 1 do
     with FConvStages[Part] do
      begin
       PerformConvolution(@FInputBuffer[FInputBufferSize], FOutputBuffer);
       FMod := (FMod + FModAnd) and FModAnd;
       PerformConvolution(@FInputBuffer2[FInputBufferSize], FOutputBuffer2);
      end;

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FLatency], FInputBuffer[0], FInputHistorySize * Sizeof(Double));
    Move(FInputBuffer2[FLatency], FInputBuffer2[0], FInputHistorySize * Sizeof(Double));

    // increase current position and reset block position
    Inc(CurrentPosition, (FLatency - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

end.
