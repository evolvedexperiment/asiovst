unit DAV_DspFftReal2ComplexCuda;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_CuFFT,
  DAV_DspFftReal2Complex;

type
  TFftReal2ComplexCuda32 = class(TFftReal2Complex)
  private
    procedure Rescale(const Data: PDAVSingleFixedArray);
    procedure RescaleSqrt(const Data: PDAVSingleFixedArray);
  public
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
  CuFftHandle : TCuFftHandle;
  CuFftResult : TCuFftResult;
begin
 CuFftHandle := 0;
 CuFftResult := CuFftPlan1d(CuFftHandle, FFftSize, CCuFftR2C, 1);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
 CuFftResult := CuFftExecR2C(CuFftHandle, TimeDomain, FrequencyDomain);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
 CuFftResult := CuFftDestroy(CuFftHandle);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
end;

procedure TFftReal2ComplexCuda32.PerformiFFTCCS(
  const FrequencyDomain: PDAVComplexSingleFixedArray;
  const TimeDomain: PDAVSingleFixedArray);
var
  CuFftHandle: TCuFftHandle;
  CuFftResult : TCuFftResult;
begin
 CuFftHandle := 0;
 CuFftResult := CuFftPlan1d(CuFftHandle, FFftSize, CCuFftC2R, 1);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
 CuFftResult := CuFftExecR2C(CuFftHandle, FrequencyDomain, TimeDomain);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
 CuFftResult := CuFftDestroy(CuFftHandle);
 if CuFftResult <> cfrSuccess
  then raise Exception.Create(CCuFftResultStrings[CuFftResult]);
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
