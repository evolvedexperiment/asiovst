unit DAV_DspFftReal2ComplexCuda;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_CudaRuntime, DAV_CudaFFT,
  DAV_DspFftReal2Complex;

type
  TFftReal2ComplexCuda32 = class(TFftReal2Complex)
  private
    FGPUTimeDomain : Pointer;
    FGPUFreqDomain : Pointer;
    procedure Rescale(const Data: PDAVSingleFixedArray);
    procedure RescaleSqrt(const Data: PDAVSingleFixedArray);
  protected
    procedure FFTOrderChanged; override;
  public
    constructor Create; override;
    constructor Create(const Order: Byte); override;
    destructor Destroy; override;
    procedure PerformFFTCCS(const FrequencyDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray);
    procedure PerformiFFTCCS(const FrequencyDomain: PDAVComplexSingleFixedArray; const TimeDomain: PDAVSingleFixedArray);
    procedure PerformFFT(const FrequencyDomain, TimeDomain: Pointer); override;
    procedure PerformIFFT(const FrequencyDomain, TimeDomain: Pointer); override;
  end;

implementation

uses
  SysUtils;

{ TFftReal2ComplexCuda32 }

procedure TFftReal2ComplexCuda32.PerformFFTCCS(
  const FrequencyDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  CudaFftHandle : TCudaFftHandle;
  CudaFftResult : TCudaFftResult;
  CudaError     : TCudaError;
begin
 CudaFftHandle := 0;

 // create FFT plan
 CudaFftResult := CudaFftPlan1d(CudaFftHandle, FFftSize, CCudaFftR2C, 1);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);

 // copy memory from host to device
 CudaError := CudaMemcpy(FGPUTimeDomain, TimeDomain, FFftSize * SizeOf(Single),
   cmkHostToDevice);
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // perform FFT
 CudaFftResult := CudaFftExecR2C(CudaFftHandle, FGPUTimeDomain, FGPUFreqDomain);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);

 // copy memory from device to host
 CudaError := CudaMemcpy(FrequencyDomain, FGPUFreqDomain, FFftSize * SizeOf(Single),
   cmkDeviceToHost);
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // free FFT plan
 CudaFftResult := CudaFftDestroy(CudaFftHandle);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);

end;

procedure TFftReal2ComplexCuda32.PerformiFFTCCS(
  const FrequencyDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  CudaFftHandle : TCudaFftHandle;
  CudaFftResult : TCudaFftResult;
  CudaError     : TCudaError;
begin
 CudaFftHandle := 0;

 // create FFT plan
 CudaFftResult := CudaFftPlan1d(CudaFftHandle, FFftSize, CCudaFftC2R, 1);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);

 // copy memory from host to device
 CudaError := CudaMemcpy(FGPUFreqDomain, FrequencyDomain, FFftSize * SizeOf(Single),
   cmkHostToDevice);
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // perform IFFT
 CudaFftResult := CudaFftExecR2C(CudaFftHandle, FGPUFreqDomain, FGPUTimeDomain);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);

 // copy memory from device to host
 CudaError := CudaMemcpy(TimeDomain, FGPUTimeDomain, FFftSize * SizeOf(Single),
   cmkDeviceToHost);
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // free FFT plan
 CudaFftResult := CudaFftDestroy(CudaFftHandle);
 if CudaFftResult <> cfrSuccess
  then raise Exception.Create(CCudaFftResultStrings[CudaFftResult]);
end;

constructor TFftReal2ComplexCuda32.Create;
begin
 inherited;
 FGPUTimeDomain := nil;
 FGPUFreqDomain := nil;
 FFTOrderChanged;
end;

constructor TFftReal2ComplexCuda32.Create(const Order: Byte);
begin
 inherited Create(Order);
 FGPUTimeDomain := nil;
 FGPUFreqDomain := nil;
 FFTOrderChanged;
end;

destructor TFftReal2ComplexCuda32.Destroy;
begin
 if assigned(FGPUTimeDomain) then CudaFree(FGPUTimeDomain);
 if assigned(FGPUFreqDomain) then CudaFree(FGPUFreqDomain);
 inherited;
end;

procedure TFftReal2ComplexCuda32.FFTOrderChanged;
var
  OldPtr    : Pointer;
  CudaError : TCudaError;
begin
 inherited;
 // Time Domain
 ///////////////////
 OldPtr := FGPUTimeDomain;

 // Allocate new memory
 CudaError := CudaMalloc(FGPUTimeDomain, FFftSize * SizeOf(Single));
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // Clear memory
 CudaError := CudaMemset(FGPUTimeDomain, 0, FFftSize * SizeOf(Single));
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // Free old memory
 if assigned(OldPtr) then CudaFree(OldPtr);


 // Frequency Domain
 OldPtr := FGPUFreqDomain;

 // Allocate new memory
 CudaError := CudaMalloc(FGPUFreqDomain, (FFftSize div 2 + 1) * SizeOf(TComplexSingle));
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // Clear memory
 CudaError := CudaMemset(FGPUFreqDomain, 0, FFftSize * SizeOf(Single));
 if CudaError <> ceSuccess
  then raise Exception.Create(CudaErrorToString(CudaError));

 // Free old memory
 if assigned(OldPtr) then CudaFree(OldPtr);
end;

procedure TFftReal2ComplexCuda32.PerformFFT(const FrequencyDomain,
  TimeDomain: Pointer);
begin
 PerformFFTCCS(FrequencyDomain, TimeDomain);
 case AutoScaleType of
   astDivideFwdByN : Rescale(FrequencyDomain);
  astDivideBySqrtN : RescaleSqrt(FrequencyDomain);
 end;
end;

procedure TFftReal2ComplexCuda32.PerformIFFT(const FrequencyDomain,
  TimeDomain: Pointer);
begin
 PerformIFFTCCS(FrequencyDomain, TimeDomain);
 case AutoScaleType of
   astDivideInvByN : Rescale(TimeDomain);
  astDivideBySqrtN : RescaleSqrt(TimeDomain);
 end;
end;

procedure TFftReal2ComplexCuda32.Rescale(const Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  1 / FFTSize;
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

procedure TFftReal2ComplexCuda32.RescaleSqrt(const Data: PDAVSingleFixedArray);
var
  i : Integer;
  s : Double;
begin
 s :=  sqrt(1 / FFTSize);
 for i := 0 to FFTSize - 1 do Data^[i] := s * Data^[i];
end;

end.
