unit DAV_DSPSineLFO;

interface

{$I ..\ASIOVST.INC}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  DAV_Common, DAV_Complex;

type
  TSineLFO = class(TObject)
  private
    procedure SetSampleRate(const Value: Single);
  protected
    FFrequency  : Single;
    FAmplitude  : Single;
    FSampleRate : Single;
    FAngle      : TComplexDouble;
    FPosition   : TComplexDouble;

    procedure SetAmplitude(const Value: Single); virtual;
    procedure SetFrequency(const Value: Single); virtual;

    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;

    procedure Reset; virtual;
  public
    constructor Create; virtual;
    procedure CalculateNextSample; virtual;

    property Sine: Double read FPosition.Re;
    property Cosine: Double read FPosition.Im;
  published
    property Amplitude: Single read FAmplitude write SetAmplitude; //  0..1
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

{ TSineLFO }

procedure TSineLFO.CalculateNextSample;
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

constructor TSineLFO.Create;
begin
  FFrequency   := 440;
  FSampleRate  := 44100;
  FAmplitude   := 1;
  FrequencyChanged;
  Reset;
end;

procedure TSineLFO.Reset;
begin
  FPosition.Re := 0;
  FPosition.Im := -FAmplitude;
end;

procedure TSineLFO.SampleRateChanged;
begin
  FrequencyChanged;
end;

procedure TSineLFO.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TSineLFO.SetAmplitude(const Value: Single);
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

procedure TSineLFO.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TSineLFO.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

end.
