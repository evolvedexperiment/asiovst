unit DAV_DspFftReal2Complex;

interface

{$I ASIOVST.inc}

{$DEFINE PUREPASCAL}

uses
  Windows, Classes, DAV_Common, DAV_Complex;

type
  TFftAutoScaleType = (astDivideFwdByN = 1, astDivideInvByN = 2,
    astDivideBySqrtN = 4, astDivideNoDivByAny = 8);

  TFftDataOrder = (doPackedRealImaginary, doPackedComplex, doComplex);

  TFftReal2Complex = class(TObject)
  private
    procedure SetBinCount(const Value: Integer);
    procedure SetFFTOrder(const Value: Integer);
    procedure SetFFTSize(l: Integer);
    procedure SetAutoScaleType(const Value: TFftAutoScaleType);
    procedure CalculateOrderDependentValues;
    procedure SetDataOrder(const Value: TFftDataOrder);
  protected
    FBinCount      : Integer;
    FFftSize       : Integer;
    FFFTSizeInv    : Double;
    FAutoScaleType : TFftAutoScaleType;
    FDataOrder     : TFftDataOrder;
    FOrder         : Integer;
    FOnSizeChanged : TNotifyEvent;
    procedure FFTOrderChanged; virtual;
    procedure AutoScaleTypeChanged; virtual;
    procedure DataOrderChanged; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const Order: Byte); overload; virtual;

    procedure ConvertSingleToDouble(Singles: PSingle; Doubles: PDouble);
    procedure ConvertDoubleToSingle(Doubles: PDouble; Singles: PSingle);

    property AutoScaleType: TFftAutoScaleType read FAutoScaleType write SetAutoScaleType;
    property BinCount: Integer read FBinCount write SetBinCount stored False;
    property DataOrder: TFftDataOrder read FDataOrder write SetDataOrder;
    property FFTSize: Integer read FFftSize write SetFFTSize stored False;
    property FFTSizeInverse: Double read FFFTSizeInv;
    property Order: Integer read FOrder write SetFFTOrder default 13;
    property OnSizeChanged: TNotifyEvent read FOnSizeChanged write FOnSizeChanged;
  end;

  TFFTLUTBitReversed = class
  public
    LUT: array of Integer;
    constructor Create(const BitCount: Integer);
    destructor Destroy; override;
    function GetPointer: pInteger;
  end;

  TFFTLUTListObject = class
  private
    FBrLUT   : TFFTLUTBitReversed;
    FFftSize : Integer;
  public
    constructor Create(const xFFTSize: Integer);
    destructor Destroy; override;
    property BRLUT: TFFTLUTBitReversed read FBrLUT write FBrLUT;
    property FFTSize: Integer read FFftSize write FFftSize;
  end;

  TFftReal2ComplexNative = class(TFftReal2Complex)
  private
    procedure CalculateScaleFactor;
  protected
    FBitRevLUT   : TFFTLUTBitReversed;
    FScaleFactor : Double;
    procedure SetFFTFunctionPointers; virtual; abstract;
    procedure CalculateTrigoLUT; virtual; abstract;
    procedure FFTOrderChanged; override;
    procedure AutoScaleTypeChanged; override;
  public
    constructor Create; overload; override;
    constructor Create(const Order: Byte); overload; override;
    property DataOrder default doPackedRealImaginary;
  end;

  TPerform32Old = procedure(const FrequencyDomain, TimeDomain: PDAVSingleFixedArray) of object;
  TPerform64Old = procedure(const FrequencyDomain, TimeDomain: PDAVDoubleFixedArray) of object;
  TPerform32New = procedure(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray) of object;
  TPerform64New = procedure(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray)of object;

  TFftReal2ComplexNativeFloat32 = class(TFftReal2ComplexNative)
  private
    procedure PerformFFTZero32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTZero32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOne32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOne32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTTwo32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTTwo32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTThree32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTThree32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOdd32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTOdd32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTEven32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformFFTEven32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTZero32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTZero32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOne32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOne32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTTwo32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTTwo32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTThree32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTThree32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTEven32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTEven32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOdd32(const FreqDomain, TimeDomain: PDAVSingleFixedArray); overload;
    procedure PerformIFFTOdd32(const FreqDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray); overload;
    procedure Rescale(Data: PDAVSingleFixedArray);
    procedure RescaleSqrt(Data: PDAVSingleFixedArray);
  protected
    FBuffer           : TDAVSingleDynArray;
    FPerformFFT32Old  : TPerform32Old;
    FPerformIFFT32Old : TPerform32Old;
    FPerformFFT32New  : TPerform32New;
    FPerformIFFT32New : TPerform32New;
    procedure CalculateTrigoLUT; override;
    procedure SetFFTFunctionPointers; override;
  public
    destructor Destroy; override;
    procedure PerformFFT32(const FrequencyDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray);
    procedure PerformIFFT32(const FrequencyDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray);
  published
    property Order;
    property OnSizeChanged;
  end;

  TFftReal2ComplexNativeFloat64 = class(TFftReal2ComplexNative)
  private
    procedure PerformFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOdd64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTEven64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTOdd64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray); overload;
    procedure PerformIFFTEven64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray); overload;
  protected
    FBuffer           : TDAVDoubleDynArray;
    FPerformFFT64Old  : TPerform64Old;
    FPerformIFFT64Old : TPerform64Old;
    FPerformFFT64New  : TPerform64New;
    FPerformIFFT64New : TPerform64New;
    procedure CalculateTrigoLUT; override;
    procedure SetFFTFunctionPointers; override;
  public
    destructor Destroy; override;
    procedure PerformFFT64(const FrequencyDomain: PDAVComplexDoubleFixedArray; const TimeDomain : PDAVDoubleFixedArray);
    procedure PerformIFFT64(const FrequencyDomain: PDAVComplexDoubleFixedArray; const TimeDomain : PDAVDoubleFixedArray);
  published
    property Order;
    property OnSizeChanged;
  end;


implementation

uses
  Math, SysUtils;

var
  CSQRT2Div2 : Double;
  LUTList    : TList;
  TrigoLUT   : PDAVDoubleFixedArray;
  TrigoLvl   : Integer;

{ TFftReal2Complex }

constructor TFftReal2Complex.Create;
begin
  inherited;
  FAutoScaleType := astDivideNoDivByAny;
  FOrder := 13;
  CalculateOrderDependentValues;
end;

constructor TFftReal2Complex.Create(const Order: Byte);
begin
  inherited Create;
  assert(Order <> 0);
  FOrder := Order;
  FAutoScaleType := astDivideNoDivByAny;
  CalculateOrderDependentValues;
end;

procedure TFftReal2Complex.ConvertSingleToDouble(Singles: PSingle;
  Doubles: PDouble);
asm
  push ebx
  mov ebx, Doubles
  mov ecx, [self.FFftSize]

  @MarioLand:
  fld  [edx + ecx * 4 - 4].Single
  fstp [ebx + ecx * 8 - 8].Double
  loop @MarioLand
  pop ebx
end;

procedure TFftReal2Complex.ConvertDoubleToSingle(Doubles: PDouble;
  Singles: PSingle);
asm
  push ebx
  mov ebx, Singles
  mov ecx,[self.FFftSize]

  @MarioLand:
  fld  [edx + ecx * 8 - 8].Double
  fstp [ebx + ecx * 4 - 4].Single
  loop @MarioLand
  pop ebx
end;

procedure TFftReal2Complex.SetFFTSize(l: Integer);
begin
  if FFftSize <> l then
   begin
    if abs(round(l) - l) > 1E-10 then
      raise Exception.Create('This FFT only works for a size of 2^n');
    Order := round(Log2(l));
   end;
end;

procedure TFftReal2Complex.SetAutoScaleType(const Value: TFftAutoScaleType);
begin
  if FAutoScaleType <> Value then
   begin
    FAutoScaleType := Value;
    AutoScaleTypeChanged;
   end;
end;

procedure TFftReal2Complex.AutoScaleTypeChanged;
begin
 // Nothing in here yet!
end;

procedure TFftReal2Complex.SetBinCount(const Value: Integer);
begin
  if FBinCount <> Value then FFTSize := 2 * (Value - 1);
end;

procedure TFftReal2Complex.DataOrderChanged;
begin
 // Nothing in here yet!
end;

procedure TFftReal2Complex.SetDataOrder(const Value: TFftDataOrder);
begin
 if FDataOrder <> Value then
  begin
   FDataOrder := Value;
   DataOrderChanged;
  end;
end;

procedure TFftReal2Complex.SetFFTOrder(const Value: Integer);
begin
  if FOrder <> Value then
   begin
    FOrder := Value;
    FFTOrderChanged;
   end;
end;

procedure TFftReal2Complex.CalculateOrderDependentValues;
begin
  FFftSize := round(IntPower(2, FOrder));
  FBinCount := FFftSize div 2 + 1;
  FFFTSizeInv := 1 / FFftSize;
end;

procedure TFftReal2Complex.FFTOrderChanged;
begin
  CalculateOrderDependentValues;
  if assigned(FOnSizeChanged) then FOnSizeChanged(Self);
end;

{ TFFTLUTBitReversed }

constructor TFFTLUTBitReversed.Create(const BitCount: Integer);
var
  Lngth    : Integer;
  cnt      : Integer;
  br_index : Integer;
  bit      : Integer;
begin
  inherited Create;
  Lngth := 1 shl BitCount;
  SetLength(LUT, Lngth);

  br_index := 0;
  LUT[0] := 0;
  for cnt := 1 to Lngth - 1 do
   begin
    bit := Lngth shr 1;
    br_index := br_index xor bit;
    while br_index and bit = 0 do
     begin
      bit := bit shr 1;
      br_index := br_index xor bit;
     end;
    LUT[cnt] := br_index;
   end;

 // 0, 4, 2, 6, 1, 5, 3, 7 // original
 // 0, 1, 4, 5, 2, 3, 6, 7

 // 0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15
 // 0, 1, 8, 9, 4, 5, 12, 13, 2, 3, 10, 11, 6, 7, 14, 15           

(*
  for cnt := 1 to Lngth - 1 do
   begin
    temp[i    ] := FrequencyDomain[i].Re * fScaleFactor;
    temp[i + h] := FrequencyDomain[i].Im * fScaleFactor;
   end
*)
end;

destructor TFFTLUTBitReversed.Destroy;
begin
  SetLength(LUT, 0);
  inherited;
end;

function TFFTLUTBitReversed.GetPointer: pInteger;
begin
  Result := @LUT[0];
end;

{ TFFTLUTListObject }

constructor TFFTLUTListObject.Create(const xFFTSize: Integer);

  function calcExt(Value: Integer): Integer;
  asm
    xor ecx, ecx
    @Start:
      inc ecx
      test eax,$2
      jnz @End
      shr eax,1

      jmp @Start
    @End:
    mov result.Integer, ecx
  end;

begin
  FFftSize := xFFTSize;
  if FFftSize > 1 then FBrLUT := TFFTLUTBitReversed.Create(calcExt(FFftSize));
end;

destructor TFFTLUTListObject.Destroy;
begin
  FreeAndNil(FBrLUT);
end;

procedure InitLUTList;
var
  i: Integer;
begin
  LUTList := TList.Create;
  for i := 1 to 15 do
    LUTList.Add(TFFTLUTListObject.Create(1 shl i));
end;

procedure DestroyLUTList;
begin
  while LUTList.Count > 0 do
   begin
    TFFTLUTListObject(LUTList.Items[0]).Free;
    LUTList.Delete(0);
   end;
  LUTList.Free;
end;

{ TFftReal2ComplexNative }

procedure TFftReal2ComplexNative.AutoScaleTypeChanged;
begin
  inherited;
  CalculateScaleFactor;
end;

procedure TFftReal2ComplexNative.CalculateScaleFactor;
begin
  case FAutoScaleType of
    astDivideFwdByN,
    astDivideInvByN : FScaleFactor := 1 / FFftSize;
    astDivideBySqrtN : FScaleFactor := 1 / sqrt(FFftSize);
  else FScaleFactor := 1;
  end;
end;

constructor TFftReal2ComplexNative.Create;
begin
  inherited Create;
  FFTOrderChanged;
  FDataOrder := doPackedRealImaginary;
end;

constructor TFftReal2ComplexNative.Create(const Order: Byte);
begin
  inherited Create(Order);
  FFTOrderChanged;
end;

procedure TFftReal2ComplexNative.FFTOrderChanged;
var
  i: Integer;
  tmp: TFFTLUTListObject;
begin
  inherited;
  CalculateTrigoLUT;
  for i := 0 to LUTList.Count - 1 do
    if TFFTLUTListObject(LUTList.Items[i]).FFTSize = FFftSize then
     begin
      FBitRevLUT := TFFTLUTListObject(LUTList.Items[i]).BRLUT;
      break;
     end;
  if i >= LUTList.Count then
   begin
    tmp := TFFTLUTListObject.Create(FFftSize);
    FBitRevLUT := tmp.BRLUT;
    LUTList.Add(tmp);
   end;
  SetFFTFunctionPointers;
  CalculateScaleFactor;
end;

procedure DoTrigoLUT(Bits: Integer);
var
  level, i  : Integer;
  len, offs : Integer;
  mul       : Extended;
begin
  if (Bits > TrigoLvl) then
   begin
    ReallocMem(TrigoLUT, ((1 shl (Bits - 1)) - 4) * SizeOf(Double));

    for level := TrigoLvl to Bits - 1 do
     begin
      len  := 1 shl (level - 1);
      offs := (len - 4);
      mul  := PI / (len shl 1);
      for i := 0 to len - 1
       do TrigoLUT[i + offs] := cos(i * mul);
     end;

    TrigoLvl := Bits;
   end;
end;

{ TFftReal2ComplexNativeFloat32 }

destructor TFftReal2ComplexNativeFloat32.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited;
end;

procedure TFftReal2ComplexNativeFloat32.SetFFTFunctionPointers;
begin
  SetLength(FBuffer, FFTSize);
  case fOrder of
    0 :
     begin
      FPerformFFT32Old  := PerformFFTZero32;
      FPerformIFFT32Old := PerformIFFTZero32;
      FPerformFFT32New  := PerformFFTZero32;
      FPerformIFFT32New := PerformIFFTZero32;
     end;
    1 :
     begin
      FPerformFFT32Old  := PerformFFTOne32;
      FPerformIFFT32Old := PerformIFFTOne32;
      FPerformFFT32New  := PerformFFTOne32;
      FPerformIFFT32New := PerformIFFTOne32;
     end;
    2 :
     begin
      FPerformFFT32Old  := PerformFFTTwo32;
      FPerformIFFT32Old := PerformIFFTTwo32;
      FPerformFFT32New  := PerformFFTTwo32;
      FPerformIFFT32New := PerformIFFTTwo32;
     end;
    3 :
     begin
      FPerformFFT32Old  := PerformFFTThree32;
      FPerformIFFT32Old := PerformIFFTThree32;
      FPerformFFT32New  := PerformFFTThree32;
      FPerformIFFT32New := PerformIFFTThree32;
     end;
  else
    if fOrder and 1 <> 0 then
     begin
      FPerformFFT32Old  := PerformFFTOdd32;
      FPerformIFFT32Old := PerformIFFTOdd32;
      FPerformFFT32New  := PerformFFTOdd32;
      FPerformIFFT32New := PerformIFFTOdd32;
     end
    else
     begin
      FPerformFFT32Old  := PerformFFTEven32;
      FPerformIFFT32Old := PerformIFFTEven32;
      FPerformFFT32New  := PerformFFTEven32;
      FPerformIFFT32New := PerformIFFTEven32;
     end;
   end;
end;

procedure TFftReal2ComplexNativeFloat32.CalculateTrigoLUT;
begin
  DoTrigoLUT(fOrder);
end;

procedure TFftReal2ComplexNativeFloat32.Rescale(Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  1 / FFTSize;
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexNativeFloat32.RescaleSqrt(Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  sqrt(1 / FFTSize);
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFT32(
  const FrequencyDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  i, h : Integer;
  temp : PDAVSingleFixedArray;
begin
 case FDataOrder of
  doPackedRealImaginary:
   begin
    FPerformFFT32Old(@FrequencyDomain[0], TimeDomain);
    case FAutoScaleType of
     astDivideFwdByN  : Rescale(@FrequencyDomain[0]);
     astDivideBySqrtN : RescaleSqrt(@FrequencyDomain[0]);
    end;
   end;
  doPackedComplex:
   begin
    h := fFFTSize div 2;
    GetMem(temp, fFFTSize * SizeOf(Single));
    try
     FPerformFFT32Old(temp, TimeDomain);

     if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
      for i := 0 to h - 1 do
       begin
        FrequencyDomain[i].Re := temp[i    ] * fScaleFactor;
        FrequencyDomain[i].Im := temp[i + h] * fScaleFactor;
       end
     else
      for i := 0 to h - 1 do
       begin
        FrequencyDomain[i].Re := temp[i    ];
        FrequencyDomain[i].Im := temp[i + h];
       end
    finally
     Dispose(temp);
    end;
   end;
  else Raise Exception.Create('Not supported'); 
 end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFT32(
  const FrequencyDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  i, h : Integer;
  temp : PDAVSingleFixedArray;
begin
 case FDataOrder of
  doPackedRealImaginary:
   begin
    FPerformIFFT32Old(@FrequencyDomain[0], TimeDomain);
    case FAutoScaleType of
     astDivideBySqrtN : RescaleSqrt(@TimeDomain[0]);
     astDivideInvByN  : Rescale(@TimeDomain[0]);
    end;
   end;
  doPackedComplex:
   begin
    h := fFFTSize div 2;
    GetMem(temp, fFFTSize * SizeOf(Single));
    try
     if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
      for i := 0 to h - 1 do
       begin
        temp[i    ] := FrequencyDomain[i].Re * fScaleFactor;
        temp[i + h] := FrequencyDomain[i].Im * fScaleFactor;
       end
     else
      for i := 0 to h - 1 do
       begin
        temp[i    ] := FrequencyDomain[i].Re;
        temp[i + h] := FrequencyDomain[i].Im;
       end;

     FPerformIFFT32Old(@temp[0], TimeDomain);

    finally
     Dispose(temp);
    end;
   end;
  else Raise Exception.Create('Not supported'); 
 end;
end;



{ FFT Routines }

procedure TFftReal2ComplexNativeFloat32.PerformFFTZero32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0] := TimeDomain[0];
end;
{$ELSE}
asm
 fld TimeDomain.Single
 fstp FreqDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTZero32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain^[0].Re := TimeDomain^[0];
end;
{$ELSE}
asm
 fld TimeDomain.Single
 fstp FreqDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTOne32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PDAV2SingleArray absolute FreqDomain;
begin
 FD[0] := TD[0] + TD[1];
 FD[1] := TD[0] - TD[1];
end;
{$ELSE}
asm
 fld  (TimeDomain     ).Single
 fld   st(0)
 fadd (TimeDomain + $4).Single
 fstp (FreqDomain     ).Single
 fsub (TimeDomain + $4).Single
 fstp (FreqDomain + $4).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTOne32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PComplexSingle absolute FreqDomain;
begin
 FD.Re := TD[0] + TD[1];
 FD.Im := TD[0] - TD[1];
end;
{$ELSE}
asm
 fld  (TimeDomain     ).Single
 fld   st(0)
 fadd (TimeDomain + $4).Single
 fstp (FreqDomain     ).Single
 fsub (TimeDomain + $4).Single
 fstp (FreqDomain + $4).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTTwo32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Single;
  TD  : PDAV4SingleArray absolute TimeDomain;
  FD  : PDAV4SingleArray absolute FreqDomain;
begin
  FD[1]  := TD[0] - TD[2];
  FD[3]  := TD[1] - TD[3];
  Tmp[0] := TD[0] + TD[2];
  Tmp[1] := TD[1] + TD[3];
  FD[0]  := Tmp[0] + Tmp[1];
  FD[2]  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
 fld  (TimeDomain     ).Single
 fsub (TimeDomain + $8).Single
 fstp (FreqDomain + $4).Single
 fld  (TimeDomain + $4).Single
 fsub (TimeDomain + $C).Single
 fstp (FreqDomain + $C).Single
 fld  (TimeDomain     ).Single
 fadd (TimeDomain + $8).Single
 fld  (TimeDomain + $4).Single
 fadd (TimeDomain + $C).Single
 fld   st(0)
 fadd  st(0),st(2)
 fstp (FreqDomain     ).Single
 fsubp
 fstp (FreqDomain + $8).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTTwo32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Single;
  TD  : PDAV4SingleArray absolute TimeDomain;
  FD  : PDAV2ComplexSingleArray absolute FreqDomain;
begin
  FD[1].Re  := TD[0] - TD[2];
  FD[1].Im  := TD[1] - TD[3];
  Tmp[0]    := TD[0] + TD[2];
  Tmp[1]    := TD[1] + TD[3];
  FD[0].Re  := Tmp[0] + Tmp[1];
  FD[0].Im  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
 fld  (TimeDomain     ).Single
 fsub (TimeDomain + $8).Single
 fstp (FreqDomain + $4).Single
 fld  (TimeDomain + $4).Single
 fsub (TimeDomain + $C).Single
 fstp (FreqDomain + $C).Single
 fld  (TimeDomain     ).Single
 fadd (TimeDomain + $8).Single
 fld  (TimeDomain + $4).Single
 fadd (TimeDomain + $C).Single
 fld   st(0)
 fadd  st(0),st(2)
 fstp (FreqDomain     ).Single
 fsubp
 fstp (FreqDomain + $8).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformFFTThree32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Tmp : array [0..1] of Double;
  TD  : PDAV8SingleArray absolute TimeDomain;
  FD  : PDAV8SingleArray absolute FreqDomain;
begin
  FBuffer[1] := TD[0] - TD[4];
  FBuffer[3] := TD[2] - TD[6];
  Tmp[0]     := TD[0] + TD[4];
  Tmp[1]     := TD[2] + TD[6];
  FBuffer[0] := Tmp[0] + Tmp[1];
  FD[2]      := Tmp[0] - Tmp[1];
  FBuffer[5] := TD[1] - TD[5];
  FBuffer[7] := TD[3] - TD[7];
  Tmp[0]     := TD[1] + TD[5];
  Tmp[1]     := TD[3] + TD[7];
  FBuffer[4] := Tmp[0] + Tmp[1];
  FD[6]      := Tmp[0] - Tmp[1];
  FD[0]      := FBuffer[0] + FBuffer[4];
  FD[4]      := FBuffer[0] - FBuffer[4];
  Tmp[0]     := (FBuffer[5] - FBuffer[7]) * CSQRT2Div2;
  Tmp[1]     := (FBuffer[5] + FBuffer[7]) * CSQRT2Div2;
  FD[1]      := FBuffer[1] + Tmp[0];
  FD[3]      := FBuffer[1] - Tmp[0];
  FD[5]      := Tmp[1] + FBuffer[3];
  FD[7]      := Tmp[1] - FBuffer[3];
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTThree32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Tmp : array [0..1] of Double;
  TD  : PDAV8SingleArray absolute TimeDomain;
  FD  : PDAV4ComplexSingleArray absolute FreqDomain;
begin
  FBuffer[1] := TD[0] - TD[4];
  FBuffer[3] := TD[2] - TD[6];
  Tmp[0]     := TD[0] + TD[4];
  Tmp[1]     := TD[2] + TD[6];
  FBuffer[0] := Tmp[0] + Tmp[1];
  FD[2].Re   := Tmp[0] - Tmp[1];
  FBuffer[5] := TD[1] - TD[5];
  FBuffer[7] := TD[3] - TD[7];
  Tmp[0]     := TD[1] + TD[5];
  Tmp[1]     := TD[3] + TD[7];
  FBuffer[4] := Tmp[0] + Tmp[1];
  FD[2].Im   := Tmp[0] - Tmp[1];
  FD[0].Re   := FBuffer[0] + FBuffer[4];
  FD[0].Im   := FBuffer[0] - FBuffer[4];
  Tmp[0]     := (FBuffer[5] - FBuffer[7]) * CSQRT2Div2;
  Tmp[1]     := (FBuffer[5] + FBuffer[7]) * CSQRT2Div2;
  FD[1].Re   := FBuffer[1] + Tmp[0];
  FD[3].Re   := FBuffer[1] - Tmp[0];
  FD[1].Im   := Tmp[1] + FBuffer[3];
  FD[3].Im   := Tmp[1] - FBuffer[3];
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTEven32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i    : Integer;
  NbrCoef        : Integer;
  NbrCoefH       : Integer;
  BitPos         : Array [0..1] of Integer;
  c, s, v        : Double;
  TmpBuffer      : Array [0..2] of PDAVSingleFixedArray;

begin
  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := fBitRevLUT.LUT[ci - 4];
    BitPos[1] := fBitRevLUT.LUT[ci - 3];
    FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := fBitRevLUT.LUT[ci - 2];
    BitPos[1] := fBitRevLUT.LUT[ci - 1];
    FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    FreqDomain[ci - 4] := s + c;
    FreqDomain[ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
    FBuffer[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
    FBuffer[ci + 2] := FreqDomain[ci + 2];
    FBuffer[ci + 6] := FreqDomain[ci + 6];

    v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer[ci + 1] := FreqDomain[ci + 1] + v;
    FBuffer[ci + 3] := FreqDomain[ci + 1] - v;

    v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
    FBuffer[ci + 5] := v + FreqDomain[ci + 3];
    FBuffer[ci + 7] := v - FreqDomain[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TmpBuffer[0] := @FreqDomain[0];
  TmpBuffer[1] := @FBuffer[0];

  for Pass := 3 to fOrder - 1 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TmpBuffer[0][0                 ] := TmpBuffer[1][0] + TmpBuffer[1][NbrCoef];
      TmpBuffer[0][NbrCoef           ] := TmpBuffer[1][0] - TmpBuffer[1][NbrCoef];
      TmpBuffer[0][          NbrCoefH] := TmpBuffer[1][          NbrCoefH];
      TmpBuffer[0][NbrCoef + NbrCoefH] := TmpBuffer[1][NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TmpBuffer[1][NbrCoef + i] * c - TmpBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TmpBuffer[0][+i] := TmpBuffer[1][i] + v;
        TmpBuffer[0][NbrCoef - i] := TmpBuffer[1][i] - v;

        v := TmpBuffer[1][NbrCoef + i] * s + TmpBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TmpBuffer[0][NbrCoef + i] := v + TmpBuffer[1][NbrCoefH + i];
        TmpBuffer[0][2 * NbrCoef - i] := v - TmpBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TmpBuffer[0], NbrCoef * 2);
      Inc(TmpBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TmpBuffer[0], fFFTSize);
    Dec(TmpBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TmpBuffer[2] := TmpBuffer[0];
    TmpBuffer[0] := TmpBuffer[1];
    TmpBuffer[1] := TmpBuffer[2];
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTEven32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i    : Integer;
  NbrCoef        : Integer;
  NbrCoefH       : Integer;
  BitPos         : Array [0..1] of Integer;
  c, s, v        : Double;
  TmpBuffer      : Array [0..2] of PDAVSingleFixedArray;

begin
  TmpBuffer[0] := @FreqDomain[0];
  TmpBuffer[1] := @FBuffer[0];

  // first and second pass at once
  ci := fFFTSize;
  repeat
    BitPos[0] := fBitRevLUT.LUT[ci - 4];
    BitPos[1] := fBitRevLUT.LUT[ci - 3];
    TmpBuffer[0][ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    BitPos[0] := fBitRevLUT.LUT[ci - 2];
    BitPos[1] := fBitRevLUT.LUT[ci - 1];
    TmpBuffer[0][ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
    c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

    TmpBuffer[0][ci - 4] := s + c;
    TmpBuffer[0][ci - 2] := s - c;

    Dec(ci, 4);
  until (ci <= 0);

  // third pass
  ci := 0;
  repeat
    FBuffer[ci    ] := TmpBuffer[0][ci] + TmpBuffer[0][ci + 4];
    FBuffer[ci + 4] := TmpBuffer[0][ci] - TmpBuffer[0][ci + 4];
    FBuffer[ci + 2] := TmpBuffer[0][ci + 2];
    FBuffer[ci + 6] := TmpBuffer[0][ci + 6];

    v := (TmpBuffer[0][ci + 5] - TmpBuffer[0][ci + 7]) * CSQRT2Div2;
    FBuffer[ci + 1] := TmpBuffer[0][ci + 1] + v;
    FBuffer[ci + 3] := TmpBuffer[0][ci + 1] - v;

    v := (TmpBuffer[0][ci + 5] + TmpBuffer[0][ci + 7]) * CSQRT2Div2;
    FBuffer[ci + 5] := v + TmpBuffer[0][ci + 3];
    FBuffer[ci + 7] := v - TmpBuffer[0][ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to fOrder - 1 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;

    repeat
      // Extreme coefficients are always real
      TmpBuffer[0][0                 ] := TmpBuffer[1][0] + TmpBuffer[1][NbrCoef];
      TmpBuffer[0][NbrCoef           ] := TmpBuffer[1][0] - TmpBuffer[1][NbrCoef];
      TmpBuffer[0][          NbrCoefH] := TmpBuffer[1][          NbrCoefH];
      TmpBuffer[0][NbrCoef + NbrCoefH] := TmpBuffer[1][NbrCoef + NbrCoefH];

      // Others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef  - 4 - i];

        v := TmpBuffer[1][NbrCoef + i] * c - TmpBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TmpBuffer[0][+i] := TmpBuffer[1][i] + v;
        TmpBuffer[0][NbrCoef - i] := TmpBuffer[1][i] - v;

        v := TmpBuffer[1][NbrCoef + i] * s + TmpBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TmpBuffer[0][NbrCoef + i] := v + TmpBuffer[1][NbrCoefH + i];
        TmpBuffer[0][2 * NbrCoef - i] := v - TmpBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TmpBuffer[0], NbrCoef * 2);
      Inc(TmpBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TmpBuffer[0], fFFTSize);
    Dec(TmpBuffer[1], fFFTSize);

    // Prepare to the next Pass
    TmpBuffer[2] := TmpBuffer[0];
    TmpBuffer[0] := TmpBuffer[1];
    TmpBuffer[1] := TmpBuffer[2];
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTOdd32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  r0, r1      : Integer;
  c, s, v     : Double;
  TmpBuffer   : Array [0..2] of PDAVSingleFixedArray;
begin
  // First and second pass at once
  ci := fFFTSize;
  repeat
    r0 := fBitRevLUT.LUT[ci - 4];
    r1 := fBitRevLUT.LUT[ci - 3];
    FBuffer[ci - 3] := TimeDomain[r0] - TimeDomain[r1];
    s := TimeDomain[r0] + TimeDomain[r1];

    r0 := fBitRevLUT.LUT[ci - 2];
    r1 := fBitRevLUT.LUT[ci - 1];
    FBuffer[ci - 1] := TimeDomain[r0] - TimeDomain[r1];
    c := TimeDomain[r0] + TimeDomain[r1];

    FBuffer[ci - 4] := s + c;
    FBuffer[ci - 2] := s - c;

    r0 := fBitRevLUT.LUT[ci - 8];
    r1 := fBitRevLUT.LUT[ci - 7];
    FBuffer[ci - 7] := TimeDomain[r0] - TimeDomain[r1];
    s := TimeDomain[r0] + TimeDomain[r1];

    r0 := fBitRevLUT.LUT[ci - 6];
    r1 := fBitRevLUT.LUT[ci - 5];
    FBuffer[ci - 5] := TimeDomain[r0] - TimeDomain[r1];
    c := TimeDomain[r0] + TimeDomain[r1];

    FBuffer[ci - 8] := s + c;
    FBuffer[ci - 6] := s - c;

    Dec(ci, 8);
  until (ci <= 0);

  // third pass at once

  ci := 0;
  repeat
    FreqDomain[ci] := FBuffer[ci] + FBuffer[ci + 4];
    FreqDomain[ci + 4] := FBuffer[ci] - FBuffer[ci + 4];
    FreqDomain[ci + 2] := FBuffer[ci + 2];
    FreqDomain[ci + 6] := FBuffer[ci + 6];

    v := (FBuffer[ci + 5] - FBuffer[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 1] := FBuffer[ci + 1] + v;
    FreqDomain[ci + 3] := FBuffer[ci + 1] - v;
    v := (FBuffer[ci + 5] + FBuffer[ci + 7]) * CSQRT2Div2;
    FreqDomain[ci + 5] := v + FBuffer[ci + 3];
    FreqDomain[ci + 7] := v - FBuffer[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  TmpBuffer[0] := @FBuffer[0];
  TmpBuffer[1] := @FreqDomain[0];
  for Pass := 3 to fOrder - 1 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TmpBuffer[0][0] := TmpBuffer[1][0] + TmpBuffer[1][NbrCoef];
      TmpBuffer[0][NbrCoef] := TmpBuffer[1][0] - TmpBuffer[1][NbrCoef];
      TmpBuffer[0][+NbrCoefH] := TmpBuffer[1][+NbrCoefH];
      TmpBuffer[0][NbrCoef + NbrCoefH] := TmpBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef - 4 - i];

        v := TmpBuffer[1][NbrCoef + i] * c - TmpBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TmpBuffer[0][+i] := TmpBuffer[1][i] + v;
        TmpBuffer[0][NbrCoef - i] := TmpBuffer[1][i] - v;

        v := TmpBuffer[1][NbrCoef + i] * s + TmpBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TmpBuffer[0][NbrCoef + i] := v + TmpBuffer[1][NbrCoefH + i];
        TmpBuffer[0][2 * NbrCoef - i] := v - TmpBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TmpBuffer[0], NbrCoef * 2);
      Inc(TmpBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TmpBuffer[0], fFFTSize);
    Dec(TmpBuffer[1], fFFTSize);

    // prepare to the next pass
    TmpBuffer[2] := TmpBuffer[0];
    TmpBuffer[0] := TmpBuffer[1];
    TmpBuffer[1] := TmpBuffer[2];
   end;
end;

procedure TFftReal2ComplexNativeFloat32.PerformFFTOdd32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  r0, r1      : Integer;
  c, s, v     : Double;
  TmpBuffer   : Array [0..2] of PDAVSingleFixedArray;
begin
  TmpBuffer[0] := @FBuffer[0];
  TmpBuffer[1] := @FreqDomain[0];

  // first and second pass at once
  ci := fFFTSize;
  repeat
    r0 := fBitRevLUT.LUT[ci - 4];
    r1 := fBitRevLUT.LUT[ci - 3];
    FBuffer[ci - 3] := TimeDomain[r0] - TimeDomain[r1];
    s := TimeDomain[r0] + TimeDomain[r1];

    r0 := fBitRevLUT.LUT[ci - 2];
    r1 := fBitRevLUT.LUT[ci - 1];
    FBuffer[ci - 1] := TimeDomain[r0] - TimeDomain[r1];
    c := TimeDomain[r0] + TimeDomain[r1];

    FBuffer[ci - 4] := s + c;
    FBuffer[ci - 2] := s - c;

    r0 := fBitRevLUT.LUT[ci - 8];
    r1 := fBitRevLUT.LUT[ci - 7];
    FBuffer[ci - 7] := TimeDomain[r0] - TimeDomain[r1];
    s := TimeDomain[r0] + TimeDomain[r1];

    r0 := fBitRevLUT.LUT[ci - 6];
    r1 := fBitRevLUT.LUT[ci - 5];
    FBuffer[ci - 5] := TimeDomain[r0] - TimeDomain[r1];
    c := TimeDomain[r0] + TimeDomain[r1];

    FBuffer[ci - 8] := s + c;
    FBuffer[ci - 6] := s - c;

    Dec(ci, 8);
  until (ci <= 0);

  // third pass at once
  ci := 0;
  repeat
    TmpBuffer[1][ci    ] := FBuffer[ci] + FBuffer[ci + 4];
    TmpBuffer[1][ci + 4] := FBuffer[ci] - FBuffer[ci + 4];
    TmpBuffer[1][ci + 2] := FBuffer[ci + 2];
    TmpBuffer[1][ci + 6] := FBuffer[ci + 6];

    v := (FBuffer[ci + 5] - FBuffer[ci + 7]) * CSQRT2Div2;
    TmpBuffer[1][ci + 1] := FBuffer[ci + 1] + v;
    TmpBuffer[1][ci + 3] := FBuffer[ci + 1] - v;
    v := (FBuffer[ci + 5] + FBuffer[ci + 7]) * CSQRT2Div2;
    TmpBuffer[1][ci + 5] := v + FBuffer[ci + 3];
    TmpBuffer[1][ci + 7] := v - FBuffer[ci + 3];

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // next pass
  for Pass := 3 to fOrder - 1 do
   begin
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    ci := 0;
    repeat
      // extreme coefficients are always real
      TmpBuffer[0][0] := TmpBuffer[1][0] + TmpBuffer[1][NbrCoef];
      TmpBuffer[0][NbrCoef] := TmpBuffer[1][0] - TmpBuffer[1][NbrCoef];
      TmpBuffer[0][+NbrCoefH] := TmpBuffer[1][+NbrCoefH];
      TmpBuffer[0][NbrCoef + NbrCoefH] := TmpBuffer[1][NbrCoef + NbrCoefH];

      // others are conjugate complex numbers
      for i := 1 to NbrCoefH - 1 do
       begin
        c := TrigoLUT[NbrCoefH - 4 + i];
        s := TrigoLUT[NbrCoef - 4 - i];

        v := TmpBuffer[1][NbrCoef + i] * c - TmpBuffer[1][NbrCoef + NbrCoefH + i] * s;
        TmpBuffer[0][+i] := TmpBuffer[1][i] + v;
        TmpBuffer[0][NbrCoef - i] := TmpBuffer[1][i] - v;

        v := TmpBuffer[1][NbrCoef + i] * s + TmpBuffer[1][NbrCoef + NbrCoefH + i] * c;
        TmpBuffer[0][NbrCoef + i] := v + TmpBuffer[1][NbrCoefH + i];
        TmpBuffer[0][2 * NbrCoef - i] := v - TmpBuffer[1][NbrCoefH + i];
       end;

      Inc(ci, NbrCoef * 2);
      Inc(TmpBuffer[0], NbrCoef * 2);
      Inc(TmpBuffer[1], NbrCoef * 2);
    until (ci >= fFFTSize);
    Dec(TmpBuffer[0], fFFTSize);
    Dec(TmpBuffer[1], fFFTSize);

    // prepare to the next pass
    TmpBuffer[2] := TmpBuffer[0];
    TmpBuffer[0] := TmpBuffer[1];
    TmpBuffer[1] := TmpBuffer[2];
   end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// IFFT ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TFftReal2ComplexNativeFloat32.PerformIFFTZero32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  TimeDomain^[0] := FreqDomain^[0];
end;
{$ELSE}
asm
 fld FreqDomain.Single
 fstp TimeDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTZero32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
begin
  TimeDomain^[0] := FreqDomain^[0].Re;
end;
{$ELSE}
asm
 fld FreqDomain.Single
 fstp TimeDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOne32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PDAV2SingleArray absolute FreqDomain;
begin
  TD[0] := FD[0] + FD[1];
  TD[1] := FD[0] - FD[1];
end;
{$ELSE}
asm
 fld   FreqDomain.Single
 fld  st(0)
 fadd (FreqDomain + 4).Single
 fstp (TimeDomain    ).Single
 fsub (FreqDomain + 4).Single
 fstp (TimeDomain + 4).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOne32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2SingleArray absolute TimeDomain;
  FD : PComplexSingle absolute FreqDomain;
begin
  TD[0] := FD.Re + FD.Im;
  TD[1] := FD.Re - FD.Im;
end;
{$ELSE}
asm
 fld   FreqDomain.Single
 fld  st(0)
 fadd (FreqDomain + 4).Single
 fstp (TimeDomain    ).Single
 fsub (FreqDomain + 4).Single
 fstp (TimeDomain + 4).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTTwo32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
var
  Tmp : Array [0..1] of Double;
  FD  : PDAV4SingleArray absolute FreqDomain;
  TD  : PDAV4SingleArray absolute TimeDomain;
begin
  Tmp[0] := FD[0] + FD[2];
  Tmp[1] := FD[0] - FD[2];

  TD[1]  := Tmp[1] + FD[3] * 2;
  TD[3]  := Tmp[1] - FD[3] * 2;
  TD[0]  := Tmp[0] + FD[1] * 2;
  TD[2]  := Tmp[0] - FD[1] * 2;
end;
{$ELSE}
const
  c2 : Double = 2;
asm
 fld (FreqDomain+8).Single
 fld FreqDomain.Single
 fld st(0)
 fadd st(0),st(2)
 fxch st(2)
 fsubp                        // b1, b0
 fld (FreqDomain+12).Single   // f3, b1, b0
 fmul c2                      // 2 * f3, b1, b0
 fld st(0)                    // 2 * f3, 2 * f3, b1, b0
 fadd st(0),st(2)             // b1 + 2 * f3, 2 * f3, b1, b0
 fstp (TimeDomain+4).Single   // 2 * f3, b1, b0
 fsubp                        // b1 - 2 * f3, b0
 fstp (TimeDomain+12).Single  // b0
 fld (FreqDomain+4).Single    // f1, b0
 fmul c2                      // 2 * f1, b0
 fld st(0)                    // 2 * f1, 2 * f1, b0
 fadd st(0),st(2)             // 2 * f1 + b0, 2 * f1, b0
 fstp (TimeDomain).Single     // 2 * f1, b0
 fsubp                        // b0 - 2 * f1
 fstp (TimeDomain+8).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTTwo32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
var
  Tmp : Array [0..1] of Double;
  FD  : PDAV2ComplexSingleArray absolute FreqDomain;
  TD  : PDAV4SingleArray absolute TimeDomain;
begin
  Tmp[0] := FD[0].Re + FD.Im;
  Tmp[1] := FD[0].Re - FD.Im;

  TD[1]  := Tmp[1] + FD[1].Im; * 2;
  TD[3]  := Tmp[1] - FD[1].Im; * 2;
  TD[0]  := Tmp[0] + FD[1].Re * 2;
  TD[2]  := Tmp[0] - FD[1].Re * 2;
end;
{$ELSE}
const
  c2 : Double = 2;
asm
 fld (FreqDomain+8).Single
 fld FreqDomain.Single
 fld st(0)
 fadd st(0),st(2)
 fxch st(2)
 fsubp                        // b1, b0
 fld (FreqDomain+12).Single   // f3, b1, b0
 fmul c2                      // 2 * f3, b1, b0
 fld st(0)                    // 2 * f3, 2 * f3, b1, b0
 fadd st(0),st(2)             // b1 + 2 * f3, 2 * f3, b1, b0
 fstp (TimeDomain+4).Single   // 2 * f3, b1, b0
 fsubp                        // b1 - 2 * f3, b0
 fstp (TimeDomain+12).Single  // b0
 fld (FreqDomain+4).Single    // f1, b0
 fmul c2                      // 2 * f1, b0
 fld st(0)                    // 2 * f1, 2 * f1, b0
 fadd st(0),st(2)             // 2 * f1 + b0, 2 * f1, b0
 fstp (TimeDomain).Single     // 2 * f1, b0
 fsubp                        // b0 - 2 * f1
 fstp (TimeDomain+8).Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat32.PerformIFFTThree32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Tmp : Array [0..3] of Double;
  FD  : PDAV8SingleArray absolute FreqDomain;
  TD  : PDAV8SingleArray absolute TimeDomain;
begin
  FBuffer[0] := FD[0] + FD[4];
  FBuffer[4] := FD[0] - FD[4];
  FBuffer[1] := FD[1] + FD[3];
  FBuffer[3] := FD[5] - FD[7];
  Tmp[0]     := FD[1] - FD[3];
  Tmp[1]     := FD[5] + FD[7];
  FBuffer[5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
  FBuffer[7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;
  Tmp[0]     := FBuffer[0] + FD[2] * 2;
  Tmp[2]     := FBuffer[0] - FD[2] * 2;
  Tmp[1]     := FBuffer[1] * 2;
  Tmp[3]     := FBuffer[3] * 2;
  TD[0]      := Tmp[0] + Tmp[1];
  TD[4]      := Tmp[0] - Tmp[1];
  TD[2]      := Tmp[2] + Tmp[3];
  TD[6]      := Tmp[2] - Tmp[3];
  Tmp[0]     := FBuffer[4] + FD[6] * 2;
  Tmp[2]     := FBuffer[4] - FD[6] * 2;
  Tmp[1]     := FBuffer[5] * 2;
  Tmp[3]     := FBuffer[7] * 2;
  TD[1]      := Tmp[0] + Tmp[1];
  TD[5]      := Tmp[0] - Tmp[1];
  TD[3]      := Tmp[2] + Tmp[3];
  TD[7]      := Tmp[2] - Tmp[3];
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTThree32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Tmp : Array [0..3] of Double;
  FD  : PDAV4ComplexSingleArray absolute FreqDomain;
  TD  : PDAV8SingleArray absolute TimeDomain;
begin
  FBuffer[0] := FD[0].Re + FD[0].Im;
  FBuffer[4] := FD[0].Re - FD[0].Im;
  FBuffer[1] := FD[1].Re + FD[3].Re;
  FBuffer[3] := FD[1].Im - FD[3].Im;
  Tmp[0]     := FD[1].Re - FD[3].Re;
  Tmp[1]     := FD[1].Im + FD[3].Im;
  FBuffer[5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
  FBuffer[7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;
  Tmp[0]     := FBuffer[0] + FD[2].Re * 2;
  Tmp[2]     := FBuffer[0] - FD[2].Re * 2;
  Tmp[1]     := FBuffer[1] * 2;
  Tmp[3]     := FBuffer[3] * 2;
  TD[0]      := Tmp[0] + Tmp[1];
  TD[4]      := Tmp[0] - Tmp[1];
  TD[2]      := Tmp[2] + Tmp[3];
  TD[6]      := Tmp[2] - Tmp[3];
  Tmp[0]     := FBuffer[4] + FD[2].Im * 2;
  Tmp[2]     := FBuffer[4] - FD[2].Im * 2;
  Tmp[1]     := FBuffer[5] * 2;
  Tmp[3]     := FBuffer[7] * 2;
  TD[1]      := Tmp[0] + Tmp[1];
  TD[5]      := Tmp[0] - Tmp[1];
  TD[3]      := Tmp[2] + Tmp[3];
  TD[7]      := Tmp[2] - Tmp[3];
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTEven32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass             : Integer;
  NbrCoef          : Integer;
  NbrCoefH         : Integer;
  NbrCoefD         : Integer;
  tof, i, ci       : Integer;
  c, s, vr, vi     : Double;
  Tmp              : Array [0..3] of Double;
  TempBuffer       : Array [0..2] of PDAVSingleFixedArray;
begin
  TempBuffer[0] := FreqDomain;
  TempBuffer[1] := TimeDomain;

  // Do the transformation in several passes

  // first pass
  for Pass := fOrder - 1 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    tof := NbrCoefH - 4;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci                     ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef           ] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
        s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

        vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := vr * c + vi * s;
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

   // prepare to the next pass
    if (Pass < fOrder - 1) then
     begin
      TempBuffer[2] := TempBuffer[0];
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := TempBuffer[2];
     end
    else
     begin
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := @FBuffer[0];
     end
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

    vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
    vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

    TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
    TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);


  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := TempBuffer[1][ci] + TempBuffer[1][ci + 2];
    Tmp[2] := TempBuffer[1][ci] - TempBuffer[1][ci + 2];
    Tmp[1] := TempBuffer[1][ci + 1] * 2;
    Tmp[3] := TempBuffer[1][ci + 3] * 2;

    TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
    Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
    Tmp[1] := TempBuffer[1][ci + 5] * 2;
    Tmp[3] := TempBuffer[1][ci + 7] * 2;

    TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTEven32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass             : Integer;
  NbrCoef          : Integer;
  NbrCoefH         : Integer;
  NbrCoefD         : Integer;
  tof, i, ci       : Integer;
  c, s, vr, vi     : Double;
  Tmp              : Array [0..3] of Double;
  TempBuffer       : Array [0..2] of PDAVSingleFixedArray;
begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := TimeDomain;

  // Do the transformation in several passes

  // first pass
  for Pass := fOrder - 1 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    tof := NbrCoefH - 4;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
        s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

        vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := vr * c + vi * s;
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

   // prepare to the next pass
    if (Pass < fOrder - 1) then
     begin
      TempBuffer[2] := TempBuffer[0];
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := TempBuffer[2];
     end
    else
     begin
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := @FBuffer[0];
     end
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
    TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
    TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

    TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
    TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

    vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
    vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

    TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
    TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);


  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := TempBuffer[1][ci] + TempBuffer[1][ci + 2];
    Tmp[2] := TempBuffer[1][ci] - TempBuffer[1][ci + 2];
    Tmp[1] := TempBuffer[1][ci + 1] * 2;
    Tmp[3] := TempBuffer[1][ci + 3] * 2;

    TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
    Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
    Tmp[1] := TempBuffer[1][ci + 5] * 2;
    Tmp[3] := TempBuffer[1][ci + 7] * 2;

    TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;


procedure TFftReal2ComplexNativeFloat32.PerformIFFTOdd32(
  const FreqDomain, TimeDomain: PDAVSingleFixedArray);
var
  Pass         : Integer;
  NbrCoef      : Integer;
  NbrCoefH     : Integer;
  NbrCoefD     : Integer;
  tof, i, ci   : Integer;
  c, s, vr, vi : Double;
  Tmp          : Array [0..3] of Single;
  TempBuffer   : Array [0..2] of PDAVSingleFixedArray;
begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer[0];

  // first pass
  for Pass := fOrder - 1 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    tof := NbrCoefH - 4;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
        s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

        vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := vr * c + vi * s;
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

    // prepare to the next pass
    if (Pass < fOrder - 1) then
     begin
      TempBuffer[2] := TempBuffer[0];
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := TempBuffer[2];
     end
    else
     begin
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := @TimeDomain[0];
     end
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer[ci] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer[ci + 5] := (vr + vi) * CSQRT2Div2;
    FBuffer[ci + 7] := (vi - vr) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer[ci] + FBuffer[ci + 2];
    Tmp[2] := FBuffer[ci] - FBuffer[ci + 2];
    Tmp[1] := FBuffer[ci + 1] * 2;
    Tmp[3] := FBuffer[ci + 3] * 2;

    TimeDomain[fBitRevLUT.LUT[ci]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := FBuffer[ci + 4] + FBuffer[ci + 6];
    Tmp[2] := FBuffer[ci + 4] - FBuffer[ci + 6];
    Tmp[1] := FBuffer[ci + 5] * 2;
    Tmp[3] := FBuffer[ci + 7] * 2;

    TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat32.PerformIFFTOdd32(
  const FreqDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  Pass         : Integer;
  NbrCoef      : Integer;
  NbrCoefH     : Integer;
  NbrCoefD     : Integer;
  tof, i, ci   : Integer;
  c, s, vr, vi : Double;
  Tmp          : Array [0..3] of Single;
  TempBuffer   : Array [0..2] of PDAVSingleFixedArray;
begin
  TempBuffer[0] := @FreqDomain[0];
  TempBuffer[1] := @FBuffer[0];

  // first pass
  for Pass := fOrder - 1 downto 3 do
   begin
    ci := 0;
    NbrCoef := 1 shl Pass;
    NbrCoefH := NbrCoef shr 1;
    NbrCoefD := NbrCoef shl 1;

    tof := NbrCoefH - 4;

    repeat
      // extreme coefficients are always real
      TempBuffer[1][ci] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
      TempBuffer[1][ci + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
      TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

      // others are conjugate complex numbers

      for i := 1 to NbrCoefH - 1 do
       begin
        TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];
        TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
        s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

        vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
        vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

        TempBuffer[1][ci + NbrCoef            + i] := vr * c + vi * s;
        TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
       end;

      Inc(ci, NbrCoefD);
    until (ci >= fFFTSize);

    // prepare to the next pass
    if (Pass < fOrder - 1) then
     begin
      TempBuffer[2] := TempBuffer[0];
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := TempBuffer[2];
     end
    else
     begin
      TempBuffer[0] := TempBuffer[1];
      TempBuffer[1] := @TimeDomain[0];
     end
   end;

  // antepenultimate pass
  ci := 0;
  repeat
    FBuffer[ci] := TimeDomain[ci] + TimeDomain[ci + 4];
    FBuffer[ci + 4] := TimeDomain[ci] - TimeDomain[ci + 4];
    FBuffer[ci + 2] := TimeDomain[ci + 2] * 2;
    FBuffer[ci + 6] := TimeDomain[ci + 6] * 2;

    FBuffer[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
    FBuffer[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

    vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
    vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

    FBuffer[ci + 5] := (vr + vi) * CSQRT2Div2;
    FBuffer[ci + 7] := (vi - vr) * CSQRT2Div2;

    Inc(ci, 8);
  until (ci >= fFFTSize);

  // penultimate and last pass at once
  ci := 0;
  repeat
    Tmp[0] := FBuffer[ci] + FBuffer[ci + 2];
    Tmp[2] := FBuffer[ci] - FBuffer[ci + 2];
    Tmp[1] := FBuffer[ci + 1] * 2;
    Tmp[3] := FBuffer[ci + 3] * 2;

    TimeDomain[fBitRevLUT.LUT[ci]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

    Tmp[0] := FBuffer[ci + 4] + FBuffer[ci + 6];
    Tmp[2] := FBuffer[ci + 4] - FBuffer[ci + 6];
    Tmp[1] := FBuffer[ci + 5] * 2;
    Tmp[3] := FBuffer[ci + 7] * 2;

    TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
    TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
    TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

    Inc(ci, 8);
  until (ci >= fFFTSize);
end;

{ TFftReal2ComplexNativeFloat64 }

destructor TFftReal2ComplexNativeFloat64.Destroy;
begin
 SetLength(FBuffer, 0);
 inherited;
end;

procedure TFftReal2ComplexNativeFloat64.SetFFTFunctionPointers;
begin
 SetLength(FBuffer, FFTSize);
 case fOrder of
   0: begin
       FPerformFFT64Old := PerformFFTZero64;
       FPerformIFFT64Old := PerformIFFTZero64;
       FPerformFFT64New := PerformFFTZero64;
       FPerformIFFT64New := PerformIFFTZero64;
      end;
   1: begin
       FPerformFFT64Old := PerformFFTOne64;
       FPerformIFFT64Old := PerformIFFTOne64;
       FPerformFFT64New := PerformFFTOne64;
       FPerformIFFT64New := PerformIFFTOne64;
      end;
   2: begin
       FPerformFFT64Old := PerformFFTTwo64;
       FPerformIFFT64Old := PerformIFFTTwo64;
       FPerformFFT64New := PerformFFTTwo64;
       FPerformIFFT64New := PerformIFFTTwo64;
      end;
   3: begin
       FPerformFFT64Old := PerformFFTThree64;
       FPerformIFFT64Old := PerformIFFTThree64;
       FPerformFFT64New := PerformFFTThree64;
       FPerformIFFT64New := PerformIFFTThree64;
      end;
  else
   begin
    if fOrder and 1 <> 0 then
     begin
      FPerformFFT64Old  := PerformFFTOdd64;
      FPerformIFFT64Old := PerformIFFTOdd64;
      FPerformFFT64New := PerformFFTOdd64;
      FPerformIFFT64New := PerformIFFTOdd64;
     end
    else
     begin
      FPerformFFT64Old  := PerformFFTEven64;
      FPerformIFFT64Old := PerformIFFTEven64;
      FPerformFFT64New := PerformFFTEven64;
      FPerformIFFT64New := PerformIFFTEven64;
     end;
   end;
 end;
end;

procedure TFftReal2ComplexNativeFloat64.CalculateTrigoLUT;
begin
 DoTrigoLUT(fOrder);
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFT64(
  const FrequencyDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  i, h : Integer;
  FD   : TDAVDoubleDynArray absolute FrequencyDomain;
  temp : TDAVDoubleDynArray;
begin
 case FDataOrder of
  doPackedRealImaginary:
   begin
    FPerformFFT64Old(@FrequencyDomain[0], TimeDomain);
   end;
  doPackedComplex:
   begin
    h := fFFTSize div 2;
    SetLength(temp, fFFTSize);

    FPerformFFT64Old(@temp[0], @TimeDomain[0]);

    if FAutoScaleType in [astDivideFwdByN, astDivideBySqrtN] then
     begin
      for i := 0 to h do FD[2 * i] := temp[i] * fScaleFactor;
      for i := 1 to h - 1 do FD[2 * i + 1] := temp[i + h] * fScaleFactor;
     end
    else
     begin
      for i := 0 to h do FD[2 * i] := temp[i];
      for i := 1 to h - 1 do FD[2 * i + 1] := temp[i + h];
     end
   end;
  else Raise Exception.Create('Not supported'); 
 end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFT64(const FrequencyDomain: PDAVComplexDoubleFixedArray; const TimeDomain : PDAVDoubleFixedArray);
var
  i, h : Integer;
  FD   : TDAVDoubleDynArray absolute FrequencyDomain;
  temp : TDAVDoubleDynArray;
begin
 case FDataOrder of
  doPackedRealImaginary: FPerformIFFT64Old(@FrequencyDomain[0], TimeDomain);
  doPackedComplex:
   begin
    h := fFFTSize div 2;
    SetLength(temp, fFFTSize);
    if FAutoScaleType in [astDivideInvByN, astDivideBySqrtN] then
     begin
      for i := 0 to h do temp[i] := FD[2 * i] * fScaleFactor;
      for i := 1 to h - 1 do temp[i + h] := FD[2 * i + 1] * fScaleFactor;
     end
    else
     begin
      for i := 0 to h do temp[i] := FD[2 * i];
      for i := 1 to h - 1 do temp[i + h] := FD[2 * i + 1];
     end;
    FPerformIFFT64Old(@temp[0], @TimeDomain[0]);
   end;
 end;
end;

{ FFT Routines }

procedure TFftReal2ComplexNativeFloat64.PerformFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0].Re := TimeDomain[0];
end;
{$ELSE}
asm
 fld TimeDomain.Single
 fstp FreqDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
  FreqDomain[0] := TimeDomain[0];
end;
{$ELSE}
asm
 fld TimeDomain.Single
 fstp FreqDomain.Single
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTOne64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2DoubleArray absolute TimeDomain;
  FD : PComplexDouble absolute FreqDomain;
begin
 FD.Re := TD[0] + TD[1];
 FD.Im := TD[0] - TD[1];
end;
{$ELSE}
asm
 fld   TimeDomain.Double
 fld   st(0)
 fadd (TimeDomain + $8).Double
 fstp  FreqDomain.Double
 fsub (TimeDomain + $8).Double
 fstp (FreqDomain + $8).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTOne64(
  const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  TD : PDAV2DoubleArray absolute TimeDomain;
  FD : PDAV2DoubleArray absolute FreqDomain;
begin
 FD[0] := TD[0] + TD[1];
 FD[1] := TD[0] - TD[1];
end;
{$ELSE}
asm
 fld   TimeDomain.Double
 fld   st(0)
 fadd (TimeDomain + $8).Double
 fstp  FreqDomain.Double
 fsub (TimeDomain + $8).Double
 fstp (FreqDomain + $8).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  TD  : PDAV4DoubleArray absolute TimeDomain;
  FD  : PDAV2ComplexDoubleArray absolute FreqDomain;
begin
  FD[1].Re := TD[0] - TD[2];
  FD[1].Im := TD[1] - TD[3];
  Tmp[0]   := TD[0] + TD[2];
  Tmp[1]   := TD[1] + TD[3];
  FD[0].Re := Tmp[0] + Tmp[1];
  FD[0].Im := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
 fld  (TimeDomain      ).Double
 fsub (TimeDomain + $10).Double
 fstp (FreqDomain +  $8).Double
 fld  (TimeDomain +  $8).Double
 fsub (TimeDomain + $18).Double
 fstp (FreqDomain + $18).Double
 fld  (TimeDomain      ).Double
 fadd (TimeDomain + $10).Double
 fld  (TimeDomain +  $8).Double
 fadd (TimeDomain + $18).Double
 fld   st(0)
 fadd  st(0),st(2)
 fstp  FreqDomain.Double
 fsubp
 fstp (FreqDomain + $10).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : array [0..1] of Double;
  TD  : PDAV4DoubleArray absolute TimeDomain;
  FD  : PDAV4DoubleArray absolute FreqDomain;
begin
  FD[1]  := TD[0] - TD[2];
  FD[3]  := TD[1] - TD[3];
  Tmp[0] := TD[0] + TD[2];
  Tmp[1] := TD[1] + TD[3];
  FD[0]  := Tmp[0] + Tmp[1];
  FD[2]  := Tmp[0] - Tmp[1];
end;
{$ELSE}
asm
 fld  (TimeDomain      ).Double
 fsub (TimeDomain + $10).Double
 fstp (FreqDomain +  $8).Double
 fld  (TimeDomain +  $8).Double
 fsub (TimeDomain + $18).Double
 fstp (FreqDomain + $18).Double
 fld  (TimeDomain      ).Double
 fadd (TimeDomain + $10).Double
 fld  (TimeDomain +  $8).Double
 fadd (TimeDomain + $18).Double
 fld   st(0)
 fadd  st(0),st(2)
 fstp  FreqDomain.Double
 fsubp
 fstp (FreqDomain + $10).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
var
  Tmp : Array [0..1] of Double;
  TD  : PDAV8DoubleArray absolute TimeDomain;
  FD  : PDAV4ComplexDoubleArray absolute FreqDomain;
begin
 FBuffer[1] := TD[0] - TD[4];
 FBuffer[3] := TD[2] - TD[6];
 Tmp[0]     := TD[0] + TD[4];
 Tmp[1]     := TD[2] + TD[6];
 FBuffer[0] := Tmp[0] + Tmp[1];
 FD[2].Re   := Tmp[0] - Tmp[1];
 FBuffer[5] := TD[1] - TD[5];
 FBuffer[7] := TD[3] - TD[7];
 Tmp[0]     := TD[1] + TD[5];
 Tmp[1]     := TD[3] + TD[7];
 FBuffer[4] := Tmp[0] + Tmp[1];
 FD[2].Im   := Tmp[0] - Tmp[1];
 FD[0].Re   := FBuffer[0] + FBuffer[4];
 FD[0].Im   := FBuffer[0] - FBuffer[4];
 Tmp[0]     := (FBuffer[5] - FBuffer[7]) * CSQRT2Div2;
 Tmp[1]     := (FBuffer[5] + FBuffer[7]) * CSQRT2Div2;
 FD[1].Re   := FBuffer[1] + Tmp[0];
 FD[1].Im   := Tmp[1] + FBuffer[3];
 FD[3].Re   := FBuffer[1] - Tmp[0];
 FD[3].Im   := Tmp[1] - FBuffer[3];
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Tmp : Array [0..1] of Double;
  TD  : PDAV8DoubleArray absolute TimeDomain;
  FD  : PDAV8DoubleArray absolute FreqDomain;
begin
 FBuffer[1] := TD[0] - TD[4];
 FBuffer[3] := TD[2] - TD[6];
 Tmp[0]     := TD[0] + TD[4];
 Tmp[1]     := TD[2] + TD[6];
 FBuffer[0] := Tmp[0] + Tmp[1];
 FD[2]      := Tmp[0] - Tmp[1];
 FBuffer[5] := TD[1] - TD[5];
 FBuffer[7] := TD[3] - TD[7];
 Tmp[0]     := TD[1] + TD[5];
 Tmp[1]     := TD[3] + TD[7];
 FBuffer[4] := Tmp[0] + Tmp[1];
 FD[6]      := Tmp[0] - Tmp[1];
 FD[0]      := FBuffer[0] + FBuffer[4];
 FD[4]      := FBuffer[0] - FBuffer[4];
 Tmp[0]     := (FBuffer[5] - FBuffer[7]) * CSQRT2Div2;
 Tmp[1]     := (FBuffer[5] + FBuffer[7]) * CSQRT2Div2;
 FD[1]      := FBuffer[1] + Tmp[0];
 FD[3]      := FBuffer[1] - Tmp[0];
 FD[5]      := Tmp[1] + FBuffer[3];
 FD[7]      := Tmp[1] - FBuffer[3];
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTEven64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : Array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer[0];

 // first and second pass at once
 ci := fFFTSize;
 repeat
  BitPos[0] := fBitRevLUT.LUT[ci - 4];
  BitPos[1] := fBitRevLUT.LUT[ci - 3];
  TempBuffer[0][ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
  s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

  BitPos[0] := fBitRevLUT.LUT[ci - 2];
  BitPos[1] := fBitRevLUT.LUT[ci - 1];
  TempBuffer[0][ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
  c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

  TempBuffer[0][ci - 4] := s + c;
  TempBuffer[0][ci - 2] := s - c;

  dec(ci, 4);
 until (ci <= 0);

 // Third Pass at once
 ci := 0;
 repeat
  FBuffer[ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
  FBuffer[ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
  FBuffer[ci + 2] := TempBuffer[0][ci + 2];
  FBuffer[ci + 6] := TempBuffer[0][ci + 6];

  v := (TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7]) * CSQRT2Div2;
  FBuffer[ci + 1] := TempBuffer[0][ci + 1] + v;
  FBuffer[ci + 3] := TempBuffer[0][ci + 1] - v;
  v := (TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7]) * CSQRT2Div2;
  FBuffer[ci + 5] := v + TempBuffer[0][ci + 3];
  FBuffer[ci + 7] := v - TempBuffer[0][ci + 3];

  inc(ci, 8);
 until (ci >= fFFTSize);

 // Next Pass
 for Pass := 3 to fOrder-1 do
  begin
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   ci := 0;

   repeat
    // Extreme coefficients are always real
    TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    TempBuffer[0][        + NbrCoefH] := TempBuffer[1][         + NbrCoefH];
    TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      TempBuffer[0][        + i] := TempBuffer[1][i] + v;
      TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      TempBuffer[0][    NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;

    inc(ci, NbrCoef * 2);
    inc(TempBuffer[0], NbrCoef * 2);
    inc(TempBuffer[1], NbrCoef * 2);
   until (ci >= fFFTSize);
  dec(TempBuffer[0], fFFTSize);
  dec(TempBuffer[1], fFFTSize);

  // Prepare to the next Pass
  TempBuffer[2] := TempBuffer[0];
  TempBuffer[0] := TempBuffer[1];
  TempBuffer[1] := TempBuffer[2];
 end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass, ci, i : Integer;
  NbrCoef     : Integer;
  NbrCoefH    : Integer;
  BitPos      : Array [0..1] of Integer;
  c, s, v     : Double;
  TempBuffer  : Array [0..2] of PDAVSingleFixedArray;
begin

 // First and second Pass at once
 ci := fFFTSize;
 repeat
  BitPos[0] := fBitRevLUT.LUT[ci - 4];
  BitPos[1] := fBitRevLUT.LUT[ci - 3];
  FreqDomain[ci - 3] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
  s := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

  BitPos[0] := fBitRevLUT.LUT[ci - 2];
  BitPos[1] := fBitRevLUT.LUT[ci - 1];
  FreqDomain[ci - 1] := TimeDomain[BitPos[0]] - TimeDomain[BitPos[1]];
  c := TimeDomain[BitPos[0]] + TimeDomain[BitPos[1]];

  FreqDomain[ci - 4] := s + c;
  FreqDomain[ci - 2] := s - c;

  dec(ci, 4);
 until (ci <= 0);

 // Third Pass at once

 ci := 0;
 repeat
  FBuffer[ci    ] := FreqDomain[ci] + FreqDomain[ci + 4];
  FBuffer[ci + 4] := FreqDomain[ci] - FreqDomain[ci + 4];
  FBuffer[ci + 2] := FreqDomain[ci + 2];
  FBuffer[ci + 6] := FreqDomain[ci + 6];

  v := (FreqDomain[ci + 5] - FreqDomain[ci + 7]) * CSQRT2Div2;
  FBuffer[ci + 1] := FreqDomain[ci + 1] + v;
  FBuffer[ci + 3] := FreqDomain[ci + 1] - v;
  v := (FreqDomain[ci + 5] + FreqDomain[ci + 7]) * CSQRT2Div2;
  FBuffer[ci + 5] := v + FreqDomain[ci + 3];
  FBuffer[ci + 7] := v - FreqDomain[ci + 3];

  inc(ci, 8);
 until (ci >= fFFTSize);

 // Next Pass
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer[0];
 // Next Pass
 for Pass := 3 to fOrder-1 do
  begin
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   ci := 0;

   repeat
    // Extreme coefficients are always real
    TempBuffer[0][0] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    TempBuffer[0][NbrCoef             ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    TempBuffer[0][         + NbrCoefH] := TempBuffer[1][         + NbrCoefH];
    TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[  NbrCoef - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      TempBuffer[0][        +i] := TempBuffer[1][i] + v;
      TempBuffer[0][NbrCoef-i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      TempBuffer[0][    NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;

    inc(ci, NbrCoef * 2);
    inc(TempBuffer[0], NbrCoef * 2);
    inc(TempBuffer[1], NbrCoef * 2);
   until (ci >= fFFTSize);
  dec(TempBuffer[0], fFFTSize);
  dec(TempBuffer[1], fFFTSize);

  // Prepare to the next Pass
  TempBuffer[2] := TempBuffer[0];
  TempBuffer[0] := TempBuffer[1];
  TempBuffer[1] := TempBuffer[2];
 end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTOdd64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pas, ci, i : Integer;
  NbrCoef    : Integer;
  NbrCoefH   : Integer;
  Bitpos     : array [0..1] of Integer;
  c, s, v    : Double;
  TempBuffer : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FBuffer[0];
 TempBuffer[1] := @FreqDomain[0];

 // first and second pass at once
 ci := fFFTSize;
 repeat
  Bitpos[0] := fBitRevLUT.LUT[ci - 4];
  Bitpos[1] := fBitRevLUT.LUT[ci - 3];
  FBuffer[ci - 3] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  s := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  Bitpos[0] := fBitRevLUT.LUT[ci - 2];
  Bitpos[1] := fBitRevLUT.LUT[ci - 1];
  FBuffer[ci - 1] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  c := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  FBuffer[ci - 4] := s + c;
  FBuffer[ci - 2] := s - c;

  Bitpos[0] := fBitRevLUT.LUT[ci - 8];
  Bitpos[1] := fBitRevLUT.LUT[ci - 7];
  FBuffer[ci - 7] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  s := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  Bitpos[0] := fBitRevLUT.LUT[ci - 6];
  Bitpos[1] := fBitRevLUT.LUT[ci - 5];
  FBuffer[ci - 5] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  c := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  FBuffer[ci - 8] := s + c;
  FBuffer[ci - 6] := s - c;

  dec(ci, 8);
 until (ci <= 0);

 // third pass at once
 ci := 0;
 repeat
  TempBuffer[1][ci    ] := FBuffer[ci] + FBuffer[ci + 4];
  TempBuffer[1][ci + 4] := FBuffer[ci] - FBuffer[ci + 4];
  TempBuffer[1][ci + 2] := FBuffer[ci + 2];
  TempBuffer[1][ci + 6] := FBuffer[ci + 6];

  v := (FBuffer[ci + 5] - FBuffer[ci + 7]) * CSQRT2Div2;
  TempBuffer[1][ci + 1] := FBuffer[ci + 1] + v;
  TempBuffer[1][ci + 3] := FBuffer[ci + 1] - v;
  v := (FBuffer[ci + 5] + FBuffer[ci + 7]) * CSQRT2Div2;
  TempBuffer[1][ci + 5] := v + FBuffer[ci + 3];
  TempBuffer[1][ci + 7] := v - FBuffer[ci + 3];

  inc(ci, 8);
 until (ci >= fFFTSize);

 // next pass
 for Pas := 3 to fOrder - 1 do
  begin
   NbrCoef := 1 shl Pas;
   NbrCoefH := NbrCoef shr 1;
   ci := 0;
   repeat
    // Extreme coefficients are always real
    TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    TempBuffer[0][        + NbrCoefH] := TempBuffer[1][NbrCoefH];
    TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH - 1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[NbrCoef  - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      TempBuffer[0][        + i] := TempBuffer[1][i] + v;
      TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;

    inc(ci,            NbrCoef * 2);
    inc(TempBuffer[0], NbrCoef * 2);
    inc(TempBuffer[1], NbrCoef * 2);
   until (ci >= fFFTSize);
  dec(TempBuffer[0], fFFTSize);
  dec(TempBuffer[1], fFFTSize);

  // Prepare to the next Pas
  TempBuffer[2] := TempBuffer[0];
  TempBuffer[0] := TempBuffer[1];
  TempBuffer[1] := TempBuffer[2];
 end;
end;

procedure TFftReal2ComplexNativeFloat64.PerformFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pas, ci, i : Integer;
  NbrCoef    : Integer;
  NbrCoefH   : Integer;
  Bitpos     : array [0..1] of Integer;
  c, s, v    : Double;
  TempBuffer : Array [0..2] of PDAVSingleFixedArray;
begin
 // First and second Pas at once
 ci := fFFTSize;
 repeat
  Bitpos[0] := fBitRevLUT.LUT[ci - 4];
  Bitpos[1] := fBitRevLUT.LUT[ci - 3];
  FBuffer[ci - 3] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  s := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  Bitpos[0] := fBitRevLUT.LUT[ci - 2];
  Bitpos[1] := fBitRevLUT.LUT[ci - 1];
  FBuffer[ci - 1] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  c := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  FBuffer[ci - 4] := s + c;
  FBuffer[ci - 2] := s - c;

  Bitpos[0] := fBitRevLUT.LUT[ci - 8];
  Bitpos[1] := fBitRevLUT.LUT[ci - 7];
  FBuffer[ci-7] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  s := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  Bitpos[0] := fBitRevLUT.LUT[ci - 6];
  Bitpos[1] := fBitRevLUT.LUT[ci - 5];
  FBuffer[ci-5] := TimeDomain[Bitpos[0]] - TimeDomain[Bitpos[1]];
  c := TimeDomain[Bitpos[0]] + TimeDomain[Bitpos[1]];

  FBuffer[ci-8] := s + c;
  FBuffer[ci-6] := s - c;

  dec(ci, 8);
 until (ci <= 0);

 // Third Pas at once

 ci := 0;
 repeat
  FreqDomain[ci] := FBuffer[ci] + FBuffer[ci + 4];
  FreqDomain[ci + 4] := FBuffer[ci] - FBuffer[ci + 4];
  FreqDomain[ci + 2] := FBuffer[ci + 2];
  FreqDomain[ci + 6] := FBuffer[ci + 6];

  v := (FBuffer[ci + 5] - FBuffer[ci + 7]) * CSQRT2Div2;
  FreqDomain[ci + 1] := FBuffer[ci + 1] + v;
  FreqDomain[ci + 3] := FBuffer[ci + 1] - v;
  v := (FBuffer[ci + 5] + FBuffer[ci + 7]) * CSQRT2Div2;
  FreqDomain[ci + 5] := v + FBuffer[ci + 3];
  FreqDomain[ci + 7] := v - FBuffer[ci + 3];

  inc(ci, 8);
 until (ci >= fFFTSize);

 // Next Pas
 TempBuffer[0]:=@FBuffer[0];
 TempBuffer[1]:=@FreqDomain[0];
 for Pas := 3 to fOrder-1 do
  begin
   NbrCoef := 1 shl Pas;
   NbrCoefH := NbrCoef shr 1;
   ci := 0;
   repeat
    // Extreme coefficients are always real
    TempBuffer[0][0                 ] := TempBuffer[1][0] + TempBuffer[1][NbrCoef];
    TempBuffer[0][NbrCoef           ] := TempBuffer[1][0] - TempBuffer[1][NbrCoef];
    TempBuffer[0][        + NbrCoefH] := TempBuffer[1][NbrCoefH];
    TempBuffer[0][NbrCoef + NbrCoefH] := TempBuffer[1][NbrCoef + NbrCoefH];

    // Others are conjugate complex numbers
    for i := 1 to NbrCoefH-1 do
     begin
      c := TrigoLUT[NbrCoefH - 4 + i];
      s := TrigoLUT[  NbrCoef - 4 - i];

      v := TempBuffer[1][NbrCoef + i] * c - TempBuffer[1][NbrCoef + NbrCoefH + i] * s;
      TempBuffer[0][        + i] := TempBuffer[1][i] + v;
      TempBuffer[0][NbrCoef - i] := TempBuffer[1][i] - v;

      v := TempBuffer[1][NbrCoef + i] * s + TempBuffer[1][NbrCoef + NbrCoefH + i] * c;
      TempBuffer[0][NbrCoef + i] := v + TempBuffer[1][NbrCoefH + i];
      TempBuffer[0][2 * NbrCoef - i] := v - TempBuffer[1][NbrCoefH + i];
     end;

    inc(ci,            NbrCoef * 2);
    inc(TempBuffer[0], NbrCoef * 2);
    inc(TempBuffer[1], NbrCoef * 2);
   until (ci >= fFFTSize);
  dec(TempBuffer[0], fFFTSize);
  dec(TempBuffer[1], fFFTSize);

  // Prepare to the next Pas
  TempBuffer[2] := TempBuffer[0];
  TempBuffer[0] := TempBuffer[1];
  TempBuffer[1] := TempBuffer[2];
 end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// IFFT ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TFftReal2ComplexNativeFloat64.PerformIFFTZero64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
 TimeDomain^[0] := FreqDomain^[0];
end;
{$ELSE}
asm
 fld FreqDomain.Double
 fstp TimeDomain.Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTZero64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
begin
 TimeDomain^[0] := FreqDomain^[0].Re;
end;
{$ELSE}
asm
 fld FreqDomain.Double
 fstp TimeDomain.Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOne64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  FD : PComplexDouble absolute FreqDomain;
  TD : PDAV2DoubleArray absolute TimeDomain;
begin
 TD[0] := FD.Re + FD.Im;
 TD[1] := FD.Re - FD.Im;
end;
{$ELSE}
asm
 fld FreqDomain.Double
 fld st(0)
 fadd (FreqDomain + 8).Double
 fstp TimeDomain.Double
 fsub (FreqDomain + 8).Double
 fstp (TimeDomain + 8).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOne64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  FD : PDAV2DoubleArray absolute FreqDomain;
  TD : PDAV2DoubleArray absolute TimeDomain;
begin
 TD[0] := FD[0] + FD[1];
 TD[1] := FD[0] - FD[1];
end;
{$ELSE}
asm
 fld FreqDomain.Double
 fld st(0)
 fadd (FreqDomain + 8).Double
 fstp TimeDomain.Double
 fsub (FreqDomain + 8).Double
 fstp (TimeDomain + 8).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTTwo64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : Array [0..1] of Single;
  FD  : PDAV2ComplexDoubleArray absolute FreqDomain;
  TD  : PDAV4DoubleArray absolute TimeDomain;
begin
 Tmp[0] := FD[0].Re + FD[0].Im;
 Tmp[1] := FD[0].Re - FD[0].Im;

 TD[1] := Tmp[1] + FD[1].Im * 2;
 TD[3] := Tmp[1] - FD[1].Im * 2;
 TD[0] := Tmp[0] + FD[1].Re * 2;
 TD[2] := Tmp[0] - FD[1].Re * 2;
end;
{$ELSE}
const
  c2 : Double = 2 ;
asm
 fld (FreqDomain + $10).Double
 fld FreqDomain.Double
 fld st(0)
 fadd st(0), st(2)
 fxch st(2)
 fsubp
 fld (FreqDomain + $18).Double
 fmul c2
 fld st(0)
 fadd st(0), st(2)
 fstp (TimeDomain + $8).Double
 fsubp
 fstp (TimeDomain + $18).Double
 fld (FreqDomain + $8).Double
 fmul c2
 fld st(0)
 fadd st(0), st(2)
 fstp (TimeDomain).Double    
 fsubp                       
 fstp (TimeDomain + $10).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTTwo64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
{$IFDEF PUREPASCAL}
var
  Tmp : Array [0..1] of Single;
  FD  : PDAV4DoubleArray absolute FreqDomain;
  TD  : PDAV4DoubleArray absolute TimeDomain;
begin
 Tmp[0] := FD[0] + FD[2];
 Tmp[1] := FD[0] - FD[2];

 TD[1] := Tmp[1] + FD[3] * 2;
 TD[3] := Tmp[1] - FD[3] * 2;
 TD[0] := Tmp[0] + FD[1] * 2;
 TD[2] := Tmp[0] - FD[1] * 2;
end;
{$ELSE}
const
  c2 : Double = 2 ;
asm
 fld (FreqDomain + $10).Double
 fld FreqDomain.Double
 fld st(0)
 fadd st(0), st(2)
 fxch st(2)
 fsubp
 fld (FreqDomain + $18).Double
 fmul c2
 fld st(0)
 fadd st(0), st(2)
 fstp (TimeDomain + $8).Double
 fsubp
 fstp (TimeDomain + $18).Double
 fld (FreqDomain + $8).Double
 fmul c2
 fld st(0)
 fadd st(0), st(2)
 fstp (TimeDomain).Double
 fsubp
 fstp (TimeDomain + $10).Double
end;
{$ENDIF}

procedure TFftReal2ComplexNativeFloat64.PerformIFFTThree64(const FreqDomain: PDAVComplexDoubleFixedArray; const TimeDomain: PDAVDoubleFixedArray);
var
  Tmp : Array [0..3] of Double;
  FD  : PDAV4ComplexDoubleArray absolute FreqDomain;
  TD  : PDAV8DoubleArray absolute TimeDomain;
begin
 FBuffer[0] := FD[0].Re + FD[0].Im;
 FBuffer[4] := FD[0].Re - FD[0].Im;
 FBuffer[1] := FD[1].Re + FD[3].Re;
 FBuffer[3] := FD[1].Im - FD[3].Im;
 Tmp[0]     := FD[1].Re - FD[3].Re;
 Tmp[1]     := FD[1].Im + FD[3].Im;
 FBuffer[5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
 FBuffer[7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;
 Tmp[0]     := FBuffer[0] + FD[2].Re * 2;
 Tmp[2]     := FBuffer[0] - FD[2].Re * 2;
 Tmp[1]     := FBuffer[1] * 2;
 Tmp[3]     := FBuffer[3] * 2;
 TD[0]      := Tmp[0] + Tmp[1];
 TD[4]      := Tmp[0] - Tmp[1];
 TD[2]      := Tmp[2] + Tmp[3];
 TD[6]      := Tmp[2] - Tmp[3];
 Tmp[0]     := FBuffer[4] + FD[2].Im * 2;
 Tmp[2]     := FBuffer[4] - FD[2].Im * 2;
 Tmp[1]     := FBuffer[5] * 2;
 Tmp[3]     := FBuffer[7] * 2;
 TD[1]      := Tmp[0] + Tmp[1];
 TD[5]      := Tmp[0] - Tmp[1];
 TD[3]      := Tmp[2] + Tmp[3];
 TD[7]      := Tmp[2] - Tmp[3];
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTThree64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Tmp : Array [0..3] of Double;
  FD  : PDAV8DoubleArray absolute FreqDomain;
  TD  : PDAV8DoubleArray absolute TimeDomain;
begin
 FBuffer[0] := FD[0] + FD[4];
 FBuffer[4] := FD[0] - FD[4];
 FBuffer[1] := FD[1] + FD[3];
 FBuffer[3] := FD[5] - FD[7];
 Tmp[0]     := FD[1] - FD[3];
 Tmp[1]     := FD[5] + FD[7];
 FBuffer[5] := (Tmp[0] + Tmp[1]) * CSQRT2Div2;
 FBuffer[7] := (Tmp[1] - Tmp[0]) * CSQRT2Div2;
 Tmp[0]     := FBuffer[0] + FD[2] * 2;
 Tmp[2]     := FBuffer[0] - FD[2] * 2;
 Tmp[1]     := FBuffer[1] * 2;
 Tmp[3]     := FBuffer[3] * 2;
 TD[0]      := Tmp[0] + Tmp[1];
 TD[4]      := Tmp[0] - Tmp[1];
 TD[2]      := Tmp[2] + Tmp[3];
 TD[6]      := Tmp[2] - Tmp[3];
 Tmp[0]     := FBuffer[4] + FD[6] * 2;
 Tmp[2]     := FBuffer[4] - FD[6] * 2;
 Tmp[1]     := FBuffer[5] * 2;
 Tmp[3]     := FBuffer[7] * 2;
 TD[1]      := Tmp[0] + Tmp[1];
 TD[5]      := Tmp[0] - Tmp[1];
 TD[3]      := Tmp[2] + Tmp[3];
 TD[7]      := Tmp[2] - Tmp[3];
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTEven64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : Array [0..3] of Single;
  TempBuffer    : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @TimeDomain[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := fOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci           ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci            + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH-1 do
     begin
      TempBuffer[1][ci + i             ] := TempBuffer[0][ci            + i] + TempBuffer[0][ci + NbrCoef            - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof           +i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof+NbrCoefH-i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci+i] - TempBuffer[0][ci + NbrCoef-i];    // - sfr [NbrCoef - i]
      vi := TempBuffer[0][ci + NbrCoef+i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < fOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @FBuffer[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;
  repeat
   TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
   TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

   TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
   TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

   vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
   vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

   TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
   TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

   inc(ci, 8);
  until (ci >= fFFTSize);


  // Penultimate and last Pass at once
  ci := 0;
  repeat
   Tmp[0] := TempBuffer[1][ci    ] + TempBuffer[1][ci + 2];
   Tmp[2] := TempBuffer[1][ci    ] - TempBuffer[1][ci + 2];
   Tmp[1] := TempBuffer[1][ci + 1] * 2;
   Tmp[3] := TempBuffer[1][ci + 3] * 2;

   TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
   Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
   Tmp[1] := TempBuffer[1][ci + 5] * 2;
   Tmp[3] := TempBuffer[1][ci + 7] * 2;

   TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTEven64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : Array [0..3] of Single;
  TempBuffer    : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @TimeDomain[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := fOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci          ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH-1 do
     begin
      TempBuffer[1][ci + i           ] := TempBuffer[0][ci           + i] + TempBuffer[0][ci + NbrCoef           - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i * PI / NbrCoef);
      s := TrigoLUT[tof + NbrCoefH - i]; // sin (i * PI / NbrCoef);

      vr := TempBuffer[0][ci           + i] - TempBuffer[0][ci + NbrCoef           - i];
      vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < fOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @FBuffer[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;
  repeat
   TempBuffer[1][ci    ] := TempBuffer[0][ci] + TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 4] := TempBuffer[0][ci] - TempBuffer[0][ci + 4];
   TempBuffer[1][ci + 2] := TempBuffer[0][ci + 2] * 2;
   TempBuffer[1][ci + 6] := TempBuffer[0][ci + 6] * 2;

   TempBuffer[1][ci + 1] := TempBuffer[0][ci + 1] + TempBuffer[0][ci + 3];
   TempBuffer[1][ci + 3] := TempBuffer[0][ci + 5] - TempBuffer[0][ci + 7];

   vr := TempBuffer[0][ci + 1] - TempBuffer[0][ci + 3];
   vi := TempBuffer[0][ci + 5] + TempBuffer[0][ci + 7];

   TempBuffer[1][ci + 5] := (vr + vi) * CSQRT2Div2;
   TempBuffer[1][ci + 7] := (vi - vr) * CSQRT2Div2;

   inc(ci, 8);
  until (ci >= fFFTSize);


  // Penultimate and last Pass at once
  ci := 0;
  repeat
   Tmp[0] := TempBuffer[1][ci    ] + TempBuffer[1][ci + 2];
   Tmp[2] := TempBuffer[1][ci    ] - TempBuffer[1][ci + 2];
   Tmp[1] := TempBuffer[1][ci + 1] * 2;
   Tmp[3] := TempBuffer[1][ci + 3] * 2;

   TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := TempBuffer[1][ci + 4] + TempBuffer[1][ci + 6];
   Tmp[2] := TempBuffer[1][ci + 4] - TempBuffer[1][ci + 6];
   Tmp[1] := TempBuffer[1][ci + 5] * 2;
   Tmp[3] := TempBuffer[1][ci + 7] * 2;

   TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOdd64(
  const FreqDomain: PDAVComplexDoubleFixedArray;
  const TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : Array [0..3] of Single;
  TempBuffer    : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer[0];

 // do the transformation in several pass

 // first pass
 for Pass := fOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef  := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci          ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci           + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TempBuffer[1][ci + i] := TempBuffer[0][ci + i] + TempBuffer[0][ci + NbrCoef - i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci + i + NbrCoefH] := TempBuffer[0][ci + NbrCoef + i] - TempBuffer[0][ci + NbrCoef + NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof + NbrCoefH - i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci + i] - TempBuffer[0][ci + NbrCoef - i];   
      vi := TempBuffer[0][ci + NbrCoef + i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef + i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH + i] := vi * c - vr * s;
     end;

    inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < fOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @TimeDomain[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;

  repeat
   FBuffer[ci    ] := TimeDomain[ci    ] + TimeDomain[ci + 4];
   FBuffer[ci + 4] := TimeDomain[ci    ] - TimeDomain[ci + 4];
   FBuffer[ci + 2] := TimeDomain[ci + 2] * 2;
   FBuffer[ci + 6] := TimeDomain[ci + 6] * 2;

   FBuffer[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
   FBuffer[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

   vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
   vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

   FBuffer[ci + 5] := (vr + vi) * CSQRT2Div2;
   FBuffer[ci + 7] := (vi - vr) * CSQRT2Div2;

   inc(ci, 8);
  until (ci >= fFFTSize);

  // Penultimate and last Pass at once
  ci := 0;

  repeat
   Tmp[0] := FBuffer[ci    ] + FBuffer[ci+2];
   Tmp[2] := FBuffer[ci    ] - FBuffer[ci+2];
   Tmp[1] := FBuffer[ci + 1] * 2;
   Tmp[3] := FBuffer[ci + 3] * 2;

   TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := FBuffer[ci + 4] + FBuffer[ci + 6];
   Tmp[2] := FBuffer[ci + 4] - FBuffer[ci + 6];
   Tmp[1] := FBuffer[ci + 5] * 2;
   Tmp[3] := FBuffer[ci + 7] * 2;

   TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   inc(ci, 8);
 until (ci >= fFFTSize);
end;

procedure TFftReal2ComplexNativeFloat64.PerformIFFTOdd64(const FreqDomain, TimeDomain: PDAVDoubleFixedArray);
var
  Pass          : Integer;
  NbrCoef       : Integer;
  NbrCoefH      : Integer;
  NbrCoefD      : Integer;
  tof, i, ci    : Integer;
  c, s, vr, vi  : Double;
  Tmp           : Array [0..3] of Single;
  TempBuffer    : Array [0..2] of PDAVSingleFixedArray;
begin
 TempBuffer[0] := @FreqDomain[0];
 TempBuffer[1] := @FBuffer[0];

 // Do the transformation in several Pass

 // First Pass
 for Pass := fOrder - 1 downto 3 do
  begin
   ci := 0;
   NbrCoef := 1 shl Pass;
   NbrCoefH := NbrCoef shr 1;
   NbrCoefD := NbrCoef shl 1;

   tof := NbrCoefH - 4;

   repeat
    // Extreme coefficients are always real
    TempBuffer[1][ci           ] := TempBuffer[0][ci] + TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci + NbrCoef] := TempBuffer[0][ci] - TempBuffer[0][ci + NbrCoef];
    TempBuffer[1][ci            + NbrCoefH] := TempBuffer[0][ci + NbrCoefH] * 2;
    TempBuffer[1][ci + NbrCoef + NbrCoefH] := TempBuffer[0][ci + NbrCoefH + NbrCoef] * 2;

    // Others are conjugate complex numbers

    for i := 1 to NbrCoefH - 1 do
     begin
      TempBuffer[1][ci+i] := TempBuffer[0][ci+i] + TempBuffer[0][ci + NbrCoef-i];    // + sfr [NbrCoef - i]
      TempBuffer[1][ci+i+NbrCoefH] := TempBuffer[0][ci + NbrCoef+i] - TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      c := TrigoLUT[tof            + i]; // cos (i*PI/NbrCoef);
      s := TrigoLUT[tof+NbrCoefH - i]; // sin (i*PI/NbrCoef);

      vr := TempBuffer[0][ci + i] - TempBuffer[0][ci + NbrCoef-i];    // - sfr [NbrCoef - i]
      vi := TempBuffer[0][ci + NbrCoef+i] + TempBuffer[0][ci + NbrCoef+NbrCoef - i];

      TempBuffer[1][ci + NbrCoef+i] := vr * c + vi * s;
      TempBuffer[1][ci + NbrCoef + NbrCoefH+i] := vi * c - vr * s;
     end;

    inc(ci, NbrCoefD);
   until (ci >= fFFTSize);

   // Prepare to the next Pass
   if (Pass < fOrder - 1) then
    begin
     TempBuffer[2] := TempBuffer[0];
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := TempBuffer[2];
    end
   else
    begin
     TempBuffer[0] := TempBuffer[1];
     TempBuffer[1] := @TimeDomain[0];
    end
  end;

  // Antepenultimate Pass
  ci := 0;

  repeat
   FBuffer[ci    ] := TimeDomain[ci    ] + TimeDomain[ci + 4];
   FBuffer[ci + 4] := TimeDomain[ci    ] - TimeDomain[ci + 4];
   FBuffer[ci + 2] := TimeDomain[ci + 2] * 2;
   FBuffer[ci + 6] := TimeDomain[ci + 6] * 2;

   FBuffer[ci + 1] := TimeDomain[ci + 1] + TimeDomain[ci + 3];
   FBuffer[ci + 3] := TimeDomain[ci + 5] - TimeDomain[ci + 7];

   vr := TimeDomain[ci + 1] - TimeDomain[ci + 3];
   vi := TimeDomain[ci + 5] + TimeDomain[ci + 7];

   FBuffer[ci + 5] := (vr + vi) * CSQRT2Div2;
   FBuffer[ci + 7] := (vi - vr) * CSQRT2Div2;

   inc(ci, 8);
  until (ci >= fFFTSize);

  // Penultimate and last Pass at once
  ci := 0;

  repeat
   Tmp[0] := FBuffer[ci    ] + FBuffer[ci+2];
   Tmp[2] := FBuffer[ci    ] - FBuffer[ci+2];
   Tmp[1] := FBuffer[ci + 1] * 2;
   Tmp[3] := FBuffer[ci + 3] * 2;

   TimeDomain[fBitRevLUT.LUT[ci    ]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 1]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 2]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 3]] := Tmp[2] - Tmp[3];

   Tmp[0] := FBuffer[ci + 4] + FBuffer[ci + 6];
   Tmp[2] := FBuffer[ci + 4] - FBuffer[ci + 6];
   Tmp[1] := FBuffer[ci + 5] * 2;
   Tmp[3] := FBuffer[ci + 7] * 2;

   TimeDomain[fBitRevLUT.LUT[ci + 4]] := Tmp[0] + Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 5]] := Tmp[0] - Tmp[1];
   TimeDomain[fBitRevLUT.LUT[ci + 6]] := Tmp[2] + Tmp[3];
   TimeDomain[fBitRevLUT.LUT[ci + 7]] := Tmp[2] - Tmp[3];

   inc(ci, 8);
 until (ci >= fFFTSize);
end;

initialization
  CSQRT2Div2 := Sqrt(2) * 0.5;
  TrigoLvl := 3;
  TrigoLUT := nil;
  DoTrigoLUT(5);
  InitLUTList;

finalization
  DestroyLUTList;

end.
