unit DAV_DspLFO;

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TCustomLFO = class(TDspObject)
  end;

  TCustomLFOSine = class(TCustomLFO)
  private
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    FFrequency  : Single;
    FAmplitude  : Single;
    FSampleRate : Single;
    procedure SetAmplitude(const Value: Single); virtual; abstract;
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure CalculateNextSample; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Amplitude: Single read FAmplitude write SetAmplitude; //  0..1
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TCustomLFOSine32 = class(TCustomLFOSine)
  private
    function GetPhase: Single;
    procedure SetPhase(const Value: Single);
  protected
    FAngle    : TComplexSingle;
    FPosition : TComplexSingle;
    procedure FrequencyChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Single); override;
  public
    procedure CalculateNextSample; override;
    procedure Reset; override;

    property Sine: Single read FPosition.Re;
    property Cosine: Single read FPosition.Im;
    property Phase: Single read GetPhase write SetPhase; //  0..2*Pi;
  end;

  TCustomLFOSine64 = class(TCustomLFOSine)
  private
    function GetPhase: Double;
    procedure SetPhase(const Value: Double);
  protected
    FAngle    : TComplexDouble;
    FPosition : TComplexDouble;
    procedure FrequencyChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Single); override;
  public
    procedure CalculateNextSample; override;
    procedure Reset; override;

    property Sine: Double read FPosition.Re;
    property Cosine: Double read FPosition.Im;
    property Phase: Double read GetPhase write SetPhase; //  0..2*Pi;
  end;

  TLFOSine32 = class(TCustomLFOSine32)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TLFOSine64 = class(TCustomLFOSine64)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TLFOSine = TLFOSine64;

  TCustomLFOSineLike = class(TDspObject)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetSpeed(const Value: Single);
  public
    constructor Create;
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TCustomLFOTriangleLike = class(TDspObject)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetSpeed(const Value: Single);
  public
    constructor Create;
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TLFOSineLike = class(TCustomLFOSineLike)
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TLFOTriangleLike = class(TCustomLFOTriangleLike)
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

implementation

uses
  Math;

{ TLFOSine }

constructor TCustomLFOSine.Create;
begin
  FFrequency   := 440;
  FSampleRate  := 44100;
  FAmplitude   := 1;
  FrequencyChanged;
  Reset;
end;

procedure TCustomLFOSine.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLFOSine then
  with TCustomLFOSine(Dest) do
   begin
    FFrequency  := Self.FFrequency;
    FAmplitude  := Self.FAmplitude;
    FSampleRate := Self.FSampleRate;
   end
 else inherited;
end;

procedure TCustomLFOSine.SampleRateChanged;
begin
 FrequencyChanged;
end;

procedure TCustomLFOSine.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TCustomLFOSine.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TCustomLFOSine32 }

procedure TCustomLFOSine32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLFOSine32 then
  with TCustomLFOSine32(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomLFOSine64 then
  with TCustomLFOSine64(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Im;
    FPosition.Re := Self.FPosition.Im;
   end
 else inherited;
end;

procedure TCustomLFOSine32.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TCustomLFOSine32.SetAmplitude(const Value: Single);
begin
 if FAmplitude <> Value then
  begin
   if FAmplitude = 0 then
    begin
     FPosition.Re := 0;
     FPosition.Im := Value;
    end
   else
    begin
     FPosition.Re := FPosition.Re / FAmplitude * Value;
     FPosition.Im := FPosition.Im / FAmplitude * Value;
    end;
   FAmplitude := Value;
  end;
end;

procedure TCustomLFOSine32.SetPhase(const Value: Single);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomLFOSine32.GetPhase: Single;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomLFOSine32.Reset;
begin
 Phase := 0;
end;

procedure TCustomLFOSine32.CalculateNextSample;
{$IFDEF PUREPASCAL}
var
  temp : Single;
begin
  temp := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := temp;
end;
{$ELSE}
asm
 fld [Self.FPosition.Re].Single  // FPosition.Re
 fmul [Self.FAngle.Re].Single    // FPosition.Re * FAngle.Re
 fld [Self.FPosition.Im].Single  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [Self.FAngle.Im].Single    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                           // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [Self.FPosition.Im].Single  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Single    // FPosition.Im * FAngle.Re, New.Re
 fld [Self.FPosition.Re].Single  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [Self.FAngle.Im].Single    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                           // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fstp [Self.FPosition.Im].Single // FPosition.Im := New.Im, New.Re
 fstp [Self.FPosition.Re].Single // FPosition.Re := New.Re
end;
{$ENDIF}

{ TCustomLFOSine64 }

procedure TCustomLFOSine64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLFOSine64 then
  with TCustomLFOSine64(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomLFOSine32 then
  with TCustomLFOSine32(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Re;
    FPosition.Im := Self.FPosition.Im;
   end
 else inherited;
end;

procedure TCustomLFOSine64.SetAmplitude(const Value: Single);
begin
 if FAmplitude <> Value then
  begin
   if FAmplitude = 0 then
    begin
     FPosition.Re := 0;
     FPosition.Im := Value;
    end
   else
    begin
     FPosition.Re := FPosition.Re / FAmplitude * Value;
     FPosition.Im := FPosition.Im / FAmplitude * Value;
    end;
   FAmplitude := Value;
  end;
end;

procedure TCustomLFOSine64.SetPhase(const Value: Double);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomLFOSine64.GetPhase: Double;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomLFOSine64.Reset;
begin
 Phase := 0;
end;

procedure TCustomLFOSine64.CalculateNextSample;
{$IFDEF PUREPASCAL}
var
  temp : Double;
begin
  temp := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := temp;
end;
{$ELSE}
asm
 fld [Self.FPosition.Re].Double  // FPosition.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Re * FAngle.Re
 fld [Self.FPosition.Im].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                           // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [Self.FPosition.Im].Double  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Im * FAngle.Re, New.Re
 fld [Self.FPosition.Re].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                           // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fstp [Self.FPosition.Im].Double // FPosition.Im := New.Im, New.Re
 fstp [Self.FPosition.Re].Double // FPosition.Re := New.Re
end;
{$ENDIF}

procedure TCustomLFOSine64.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

{ TCustomLFOSineLike }

constructor TCustomLFOSineLike.Create;
begin
  inherited;
  FMax   := 1;
  FMin   := 0;
  FValue := 1;
  FPos   := 0;
  Speed  := 100;
  FScale := FMax - ((FMin + FMax) * 0.5);
  FPosMul := Sqrt(FScale * 2) / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TCustomLFOSineLike.SetMin(const Value: Single);
begin
 if FMin <> Value then
  begin
   FMin := Value;
   FScale := FMax - ((FMin + FMax) * 0.5);
  end;
end;

procedure TCustomLFOSineLike.SetMax(const Value: Single);
begin
 if FMax <> Value then
  begin
   FMax := Value;
   FScale := FMax - ((FMin + FMax) * 0.5);
  end;
end;

procedure TCustomLFOSineLike.SetSpeed(const Value: Single);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   FIntSpeed := Round($100000000 / FSpeed);
  end;
end;

function TCustomLFOSineLike.GetValue: Single;
begin
  Result := Abs(FPos * FPosMul) - FHalfScale;
  Result := Result * (FHalfScale * 2 - Abs(Result)) * 2;
  Result := Result + (FMin + FMax) * 0.5;
  FPos := FPos + FIntSpeed;
end;


{ TCustomLFOTriangleLike }

constructor TCustomLFOTriangleLike.Create;
begin
  inherited;
  FMax := 1;
  FMin := 0;
  FValue := 1;
  FPos := 0;
  Speed := 100;
  FScale := FMax - (FMin + FMax) * 0.5;
  FPosMul := FScale / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TCustomLFOTriangleLike.SetMin(const Value: Single);
begin
 if FMin <> Value then
  begin
   FMin := Value;
   FScale := FMax - (FMin + FMax) * 0.5;
  end;
end;

procedure TCustomLFOTriangleLike.SetMax(const Value: Single);
begin
 if FMax <> Value then
  begin
   FMax := Value;
   FScale := FMax - (FMin + FMax) * 0.5;
  end;
end;

procedure TCustomLFOTriangleLike.SetSpeed(const Value: Single);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   FIntSpeed := Round($100000000 / FSpeed);
  end;
end;

function TCustomLFOTriangleLike.GetValue: Single;
begin
  Result := Abs(FPos * (2 * FPosMul)) + FMin;
  FPos := FPos + FIntSpeed;
end;

end.
