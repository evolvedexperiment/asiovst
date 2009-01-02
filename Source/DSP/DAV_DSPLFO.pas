unit DAV_DSPLFO;

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
    function GetPhase: Single;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetPhase(const Value: Single);
  protected
    FFrequency  : Single;
    FAmplitude  : Single;
    FSampleRate : Single;
    FAngle      : TComplexDouble;
    FPosition   : TComplexDouble;
    procedure SetAmplitude(const Value: Single); virtual;
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure CalculateNextSample; virtual;
    procedure Reset; virtual;

    property Sine: Double read FPosition.Re;
    property Cosine: Double read FPosition.Im;

    property Amplitude: Single read FAmplitude write SetAmplitude; //  0..1
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
    property Phase: Single read GetPhase write SetPhase; //  0..2*Pi;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TLFOSine = class(TCustomLFOSine)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

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
    FAngle      := Self.FAngle;
    FPosition   := Self.FPosition;
   end
 else inherited;
end;

procedure TCustomLFOSine.CalculateNextSample;
{$IFDEF PUREPASCAL}
var
  temp : Double;
begin
  temp := FPosition.Re * fAngle.Re - FPosition.Im * fAngle.Im;
  FPosition.Im := FPosition.Im * fAngle.Re + FPosition.Re * fAngle.Im;
  FPosition.Re := temp;
end;
{$ELSE}
asm
 fld [Self.FPosition.Re].Double  // FPosition.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Re * fAngle.Re
 fld [Self.FPosition.Im].Double  // FPosition.Im, FPosition.Re * fAngle.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Im * fAngle.Im, FPosition.Re * fAngle.Re
 fsubp                           // FPosition.Re * fAngle.Re - FPosition.Im * fAngle.Im = New.Re

 fld [Self.FPosition.Im].Double  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Im * fAngle.Re, New.Re
 fld [Self.FPosition.Re].Double  // FPosition.Re, FPosition.Re * fAngle.Re, New.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Re * fAngle.Im, FPosition.Re * fAngle.Re, New.Re
 faddp                           // FPosition.Re * fAngle.Re + FPosition.Im * fAngle.Im = New.Im, New.Re
 fstp [Self.FPosition.Im].Double // FPosition.Im := New.Im, New.Re
 fstp [Self.FPosition.Re].Double // FPosition.Re := New.Re
end;
{$ENDIF}

procedure TCustomLFOSine.Reset;
begin
  Phase := 0;
end;

procedure TCustomLFOSine.SampleRateChanged;
begin
  FrequencyChanged;
end;

procedure TCustomLFOSine.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

function TCustomLFOSine.GetPhase: Single;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomLFOSine.SetAmplitude(const Value: Single);
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
     FPosition.Im := FPosition.Re / FAmplitude * Value;
    end;
   FAmplitude := Value;
  end;
end;

procedure TCustomLFOSine.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TCustomLFOSine.SetPhase(const Value: Single);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

procedure TCustomLFOSine.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
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
