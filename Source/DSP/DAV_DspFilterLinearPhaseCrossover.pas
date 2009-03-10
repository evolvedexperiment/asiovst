unit DAV_DspFilterLinearPhaseCrossover;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspWindowing;

type
  TLinearPhaseCrossover = class(TDspSampleRateDependent)
  private
    FFilterKernel : PDAVSingleFixedArray;
    FStates       : array [0..1] of PDAVSingleFixedArray;
//    FBuffer       : TLinearPhaseLowpass;
    FSampleRate   : Single;
    FFrequency    : Single;
    FFilterLength : Integer;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetFilterLength(const Value: Integer);
    procedure CalculateFilterKernel;
  protected
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure FilterLengthChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessSample(const Input: Single; out Low, High: Single); overload;
    procedure ProcessSample(const Input: Double; out Low, High: Double); overload;
  published
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
    property FilterLength: Integer read FFilterLength write SetFilterLength;
  end;

implementation

uses
  SysUtils;

{ TLinearPhaseCrossover }

constructor TLinearPhaseCrossover.Create;
begin
 inherited;
 FSampleRate   := 44100;
 FFrequency    := 1000;
 FFilterKernel := nil;
end;

destructor TLinearPhaseCrossover.Destroy;
begin

  inherited;
end;

procedure TLinearPhaseCrossover.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TLinearPhaseCrossover.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TLinearPhaseCrossover.SetFilterLength(const Value: Integer);
begin
 if FFilterLength <> Value then
  begin
   FFilterLength := Value;
   FilterLengthChanged;
  end;
end;

procedure TLinearPhaseCrossover.FilterLengthChanged;
begin
 ReallocMem(FFilterKernel, FFilterLength * SizeOf(Single));
 FillChar(FFilterKernel^[0], FFilterLength * SizeOf(Single), 0);
 ReallocMem(FStates[0], FFilterLength * SizeOf(Single));
 FillChar(FStates[0]^[0], FFilterLength * SizeOf(Single), 0);
 ReallocMem(FStates[1], FFilterLength * SizeOf(Single));
 FillChar(FStates[1]^[0], FFilterLength * SizeOf(Single), 0);
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.FrequencyChanged;
begin
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.SampleRateChanged;
begin
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.CalculateFilterKernel;
var
  i, h   : Integer;
  n, d   : Double;
  CutOff : Double;
begin
 CutOff := FFrequency / SampleRate;

 if FFilterLength mod 2 = 0 then
  begin
   d := (FFilterLength - 1) * 0.5;
   // Generate sinc delayed by (N-1)/2
   for i := 0 to FFilterLength - 1 do
    begin
     n := PI * (i - d);
     FFilterKernel^[i] := sin(2.0 * Cutoff * n) / n;
    end;
  end
 else
  begin
   h := FFilterLength div 2;
   // Generate sinc delayed by (N-1)/2
   for i := 0 to FFilterLength - 1 do
    if (i = h)
     then FFilterKernel^[i] := 2.0 * CutOff
     else
      begin
       n := PI * (i - h);
       FFilterKernel^[i] := sin(2.0 * Cutoff * n) / n;
      end;
  end;
 ApplyHanningWindow(FFilterKernel, FFilterLength);
end;

procedure TLinearPhaseCrossover.ProcessSample(const Input: Single; out Low,
  High: Single);
{$IFDEF PUREPASCAL}
var
  Tap: Integer;
begin
 Low  := FStates[0]^[0] + Input * FFilterKernel^[0];
 High := FStates[1]^[0] - Input * FFilterKernel^[0];
 for Tap := 1 to FFilterLength - 1 do
  begin
   FStates[0]^[Tap - 1] := FStates[0]^[Tap] + Input * FFilterKernel^[Tap];
   FStates[1]^[Tap - 1] := FStates[1]^[Tap] - Input * FFilterKernel^[Tap];
   if Tap = FFilterLength div 2
    then FStates[1]^[Tap - 1] := FStates[1]^[Tap - 1] + Input
  end;
end;
{$ELSE}
asm
 push ebx
 push edi
 push esi

 mov ebx, [self.FFilterKernel]

 mov edi, [self.FStates    ].Integer
 mov esi, [self.FStates + 4].Integer

 // calculate first sample
 fld  Input               // Input
 fld  st(0)               // Input, Input
 fmul [ebx].Single        // Input * FFilterKernel^[0], Input
 fld  st(0)               // Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fadd [edi].Single        // FStates[0]^[0] + Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fstp [edx].Single        // Input * FFilterKernel^[0], Input
 fld  [esi].Single        // FStates[1]^[0], Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fsubrp                   // FStates[1]^[0] - Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fstp [ecx].Single        // Input

 mov ecx, [Self.FFilterLength]
 mov eax, ecx
 shr eax, 1              // eax = FFilterLength div 2
 sub ecx, 1

 @loop:
   add  ebx, 4           // increase FilterKernel Pointer
   fld st(0)             // Input, Input
   fmul [ebx].Single     // Input * FFilterKernel^[i], Input
   fld  st(0)            // Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
   fadd [edi + 4].Single // FStates[0]^[i] + Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   fstp [edi].Single     // Input * FFilterKernel^[0], Input

   fld  [esi + 4].Single // FStates[1]^[i], Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   fsubrp                // FStates[1]^[i] - Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   cmp ecx, eax
   jnz @norm
   fadd st(0), st(1)     // add Input if necessary
   @norm:
   fstp [esi].Single     // Input

   add  edi, 4           // increase State[0] Pointer
   add  esi, 4           // increase State[1] Pointer
 loop @loop

 fstp st(0)              // remove Input

 pop esi
 pop edi
 pop ebx
end;
{$ENDIF}

procedure TLinearPhaseCrossover.ProcessSample(const Input: Double; out Low,
  High: Double);
var
  Tap: Integer;
begin
 Low  := FStates[0]^[0] + Input * FFilterKernel^[0];
 High := FStates[1]^[0] - Input * FFilterKernel^[0];
 for Tap := 1 to FFilterLength - 1 do
  begin
   FStates[0]^[Tap - 1] := FStates[0]^[Tap] + Input * FFilterKernel^[Tap];
   FStates[1]^[Tap - 1] := FStates[1]^[Tap] - Input * FFilterKernel^[Tap];
  end;
end;

end.
