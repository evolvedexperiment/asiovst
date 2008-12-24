unit DAV_DspSineLFO;

interface

{$I ..\ASIOVST.INC}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TCustomSineLFO = class(TDspObject)
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

  TSineLFO = class(TCustomSineLFO)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

implementation

uses
  Math;

{ TSineLFO }

constructor TCustomSineLFO.Create;
begin
  FFrequency   := 440;
  FSampleRate  := 44100;
  FAmplitude   := 1;
  FrequencyChanged;
  Reset;
end;

procedure TCustomSineLFO.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSineLFO then
  with TCustomSineLFO(Dest) do
   begin
    FFrequency  := Self.FFrequency;
    FAmplitude  := Self.FAmplitude;
    FSampleRate := Self.FSampleRate;
    FAngle      := Self.FAngle;
    FPosition   := Self.FPosition;
   end
 else inherited;
end;

procedure TCustomSineLFO.CalculateNextSample;
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

procedure TCustomSineLFO.Reset;
begin
  Phase := 0;
end;

procedure TCustomSineLFO.SampleRateChanged;
begin
  FrequencyChanged;
end;

procedure TCustomSineLFO.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

function TCustomSineLFO.GetPhase: Single;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSineLFO.SetAmplitude(const Value: Single);
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

procedure TCustomSineLFO.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TCustomSineLFO.SetPhase(const Value: Single);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

procedure TCustomSineLFO.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

end.
