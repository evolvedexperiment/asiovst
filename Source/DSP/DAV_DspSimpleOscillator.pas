unit DAV_DspSimpleOscillator;

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TCustomOscillator = class(TDspObject)
  end;

  TCustomSimpleOscillator = class(TCustomOscillator)
  private
    procedure SetSampleRate(const Value: Single);
  protected
    FAmplitude  : Single;
    FSampleRate : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Single); virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
  public
    constructor Create; virtual;
    procedure CalculateNextSample; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Amplitude: Single read FAmplitude write SetAmplitude; //  0..1
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TCustomSimpleFrequencyOscillator = class(TCustomSimpleOscillator)
  private
    procedure SetFrequency(const Value: Single);
  protected
    FFrequency  : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; virtual; abstract;
  public
    constructor Create; virtual;
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
  end;

  TCustomSimpleOscillator32 = class(TCustomSimpleFrequencyOscillator)
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

  TCustomSimpleOscillator64 = class(TCustomSimpleFrequencyOscillator)
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

  TSimpleOscillator32 = class(TCustomSimpleOscillator32)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TSimpleOscillator64 = class(TCustomSimpleOscillator64)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TSimpleOscillator = TSimpleOscillator64;

implementation

uses
  Math;

{ TCustomSimpleOscillator }

constructor TCustomSimpleOscillator.Create;
begin
  FSampleRate  := 44100;
  FAmplitude   := 1;
  Reset;
end;

procedure TCustomSimpleOscillator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSimpleOscillator then
  with TCustomSimpleOscillator(Dest) do
   begin
    FAmplitude  := Self.FAmplitude;
    FSampleRate := Self.FSampleRate;
   end
 else inherited;
end;

procedure TCustomSimpleOscillator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;


{ TCustomSimpleOscillator }

constructor TCustomSimpleFrequencyOscillator.Create;
begin
 inherited;
 FFrequency := 440;
 FrequencyChanged;
end;

procedure TCustomSimpleFrequencyOscillator.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomSimpleFrequencyOscillator then
  with TCustomSimpleFrequencyOscillator(Dest)
   do FFrequency := Self.FFrequency;
end;

procedure TCustomSimpleFrequencyOscillator.SampleRateChanged;
begin
 FrequencyChanged;
end;

procedure TCustomSimpleFrequencyOscillator.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;


{ TCustomSimpleOscillator32 }

procedure TCustomSimpleOscillator32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSimpleOscillator32 then
  with TCustomSimpleOscillator32(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomSimpleOscillator64 then
  with TCustomSimpleOscillator64(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Im;
    FPosition.Re := Self.FPosition.Im;
   end
 else inherited;
end;

procedure TCustomSimpleOscillator32.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TCustomSimpleOscillator32.SetAmplitude(const Value: Single);
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

procedure TCustomSimpleOscillator32.SetPhase(const Value: Single);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomSimpleOscillator32.GetPhase: Single;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSimpleOscillator32.Reset;
begin
 Phase := 0;
end;

procedure TCustomSimpleOscillator32.CalculateNextSample;
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

{ TCustomSimpleOscillator64 }

procedure TCustomSimpleOscillator64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomSimpleOscillator64 then
  with TCustomSimpleOscillator64(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomSimpleOscillator32 then
  with TCustomSimpleOscillator32(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Re;
    FPosition.Im := Self.FPosition.Im;
   end;
end;

procedure TCustomSimpleOscillator64.SetAmplitude(const Value: Single);
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

procedure TCustomSimpleOscillator64.SetPhase(const Value: Double);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomSimpleOscillator64.GetPhase: Double;
begin
 result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSimpleOscillator64.Reset;
begin
 Phase := 0;
end;

procedure TCustomSimpleOscillator64.CalculateNextSample;
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

procedure TCustomSimpleOscillator64.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

end.
