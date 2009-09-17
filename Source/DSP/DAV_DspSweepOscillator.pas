unit DAV_DspSweepOscillator;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspSimpleOscillator;

type
  TCustomSweepOscillator64 = class(TCustomSimpleOscillator)
  private
    function GetPhase: Double;
    procedure SetPhase(const Value: Double);
    procedure SetModFreq(const Value: Double);
  protected
    FModFreq  : Double;
    FAngle    : array [0..1] of TComplexDouble;
    FPosition : TComplexDouble;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateModulationFrequency; virtual;
    procedure SetAmplitude(const Value: Single); override;
    procedure ModulationFrequencyChanged; virtual;
  public
    constructor Create; override;
    procedure Reset; override;
    property ModulationFrequency: Double read FModFreq write SetModFreq;

    property Sine: Double read FPosition.Re;
    property Cosine: Double read FPosition.Im;
    property Phase: Double read GetPhase write SetPhase; //  0..2*Pi;
  end;

  TCustomFullSweepOscillator64 = class(TCustomSweepOscillator64)
  private
    procedure SetIniFreq(const Value: Double);
    procedure CalculateInitialFrequency;
  protected
    FIniFreq  : Double;
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitialFrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    procedure CalculateNextSample; override;
    property InitialFrequency: Double read FIniFreq write SetIniFreq;
  end;

  TCustomRangeSweepOscillator64 = class(TCustomSweepOscillator64)
  private
    procedure SetStartFreq(const Value: Double);
    procedure SetStopFreq(const Value: Double);
    procedure CalculateSampleCount;
    procedure CalculateStartAngle;
  protected
    FSampleCount : Integer;
    FSamplePos   : Integer;
    FStartFreq   : Double;
    FStopFreq    : Double;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ModulationFrequencyChanged; override;
    procedure SampleRateChanged; override;
    procedure StartFrequencyChanged; virtual;
    procedure StopFrequencyChanged; virtual;
  public
    constructor Create; override;
    procedure CalculateNextSample; override;
    property StartFrequency: Double read FStartFreq write SetStartFreq;
    property StopFrequency: Double read FStopFreq write SetStopFreq;
  end;

  TFullSweepOscillator64 = class(TCustomFullSweepOscillator64)
  published
    property Amplitude;
    property Phase;
    property SampleRate;
  end;

  TRangeSweepOscillator64 = class(TCustomRangeSweepOscillator64)
  published
    property Amplitude;
    property Phase;
    property SampleRate;
  end;

implementation

uses
  Math, DAV_Approximations;

{ TCustomSweepOscillator64 }

constructor TCustomSweepOscillator64.Create;
begin
 inherited;
 FModFreq := 0.01;
 ModulationFrequencyChanged;
end;

procedure TCustomSweepOscillator64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSweepOscillator64 then
  with TCustomSweepOscillator64(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end
 else inherited;
end;

function TCustomSweepOscillator64.GetPhase: Double;
begin
 Result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSweepOscillator64.ModulationFrequencyChanged;
begin
 CalculateModulationFrequency;
end;

procedure TCustomSweepOscillator64.Reset;
begin
 Phase := 0;
end;

procedure TCustomSweepOscillator64.SetAmplitude(const Value: Single);
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

procedure TCustomSweepOscillator64.SetModFreq(const Value: Double);
begin
 if FModFreq <> Value then
  begin
   FModFreq := Value;
   ModulationFrequencyChanged;
  end;
end;

procedure TCustomSweepOscillator64.SetPhase(const Value: Double);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

procedure TCustomSweepOscillator64.CalculateModulationFrequency;
begin
 GetSinCos(2 * Pi * FModFreq / FSampleRate, FAngle[1].Im, FAngle[1].Re);
end;


{ TCustomFullSweepOscillator64 }

constructor TCustomFullSweepOscillator64.Create;
begin
 inherited;
 FIniFreq := 1000;
 CalculateInitialFrequency;
end;

procedure TCustomFullSweepOscillator64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomFullSweepOscillator64 then
  with TCustomFullSweepOscillator64(Dest)
   do FIniFreq := Self.FIniFreq;
end;

procedure TCustomFullSweepOscillator64.SampleRateChanged;
begin
 inherited;
 CalculateModulationFrequency;
 CalculateInitialFrequency;
end;

procedure TCustomFullSweepOscillator64.SetIniFreq(const Value: Double);
begin
 if FIniFreq <> Value then
  begin
   FIniFreq := Value;
   InitialFrequencyChanged;
  end;
end;

procedure TCustomFullSweepOscillator64.InitialFrequencyChanged;
begin
 CalculateInitialFrequency;
end;

procedure TCustomFullSweepOscillator64.CalculateInitialFrequency;
begin
 GetSinCos(2 * Pi * FIniFreq / FSampleRate, FAngle[0].Im, FAngle[0].Re);
end;

procedure TCustomFullSweepOscillator64.CalculateNextSample;
var
  Temp : Double;
begin
  Temp := FAngle[0].Re * FAngle[1].Re - FAngle[0].Im * FAngle[1].Im;
  FAngle[0].Im := FAngle[0].Im * FAngle[1].Re + FAngle[0].Re * FAngle[1].Im;
  FAngle[0].Re := Temp;

  Temp := FPosition.Re * FAngle[0].Re - FPosition.Im * FAngle[0].Im;
  FPosition.Im := FPosition.Im * FAngle[0].Re + FPosition.Re * FAngle[0].Im;
  FPosition.Re := Temp;
end;

{ TCustomRangeSweepOscillator64 }

constructor TCustomRangeSweepOscillator64.Create;
begin
 FStartFreq := 20;
 FStopFreq := 20000;
 inherited;
 CalculateStartAngle;
 CalculateSampleCount;
 FSamplePos := FSampleCount;
end;

procedure TCustomRangeSweepOscillator64.ModulationFrequencyChanged;
begin
 inherited;
 CalculateSampleCount;
end;

procedure TCustomRangeSweepOscillator64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomFullSweepOscillator64 then
  with TCustomFullSweepOscillator64(Dest) do
   begin
    FStartFreq := Self.FStartFreq;
    FStopFreq := Self.FStopFreq;
   end;
end;

procedure TCustomRangeSweepOscillator64.SampleRateChanged;
begin
 inherited;
 CalculateModulationFrequency;
 CalculateStartAngle;
 CalculateSampleCount;
end;

procedure TCustomRangeSweepOscillator64.SetStartFreq(const Value: Double);
begin
 if FStartFreq <> Value then
  begin
   FStartFreq := Value;
   StartFrequencyChanged;
  end;
end;

procedure TCustomRangeSweepOscillator64.SetStopFreq(const Value: Double);
begin
 if FStopFreq <> Value then
  begin
   FStopFreq := Value;
   StopFrequencyChanged;
  end;
end;

procedure TCustomRangeSweepOscillator64.StartFrequencyChanged;
begin
 CalculateStartAngle;
 CalculateSampleCount;
end;

procedure TCustomRangeSweepOscillator64.StopFrequencyChanged;
begin
 CalculateSampleCount;
end;

procedure TCustomRangeSweepOscillator64.CalculateStartAngle;
begin
 GetSinCos(2 * Pi * FStartFreq / FSampleRate, FAngle[0].Im, FAngle[0].Re);
end;

procedure TCustomRangeSweepOscillator64.CalculateSampleCount;
begin
 FSampleCount := Round((FStopFreq - FStartFreq) / FModFreq);
 assert(FSampleCount > 0);
end;

procedure TCustomRangeSweepOscillator64.CalculateNextSample;
var
  Temp : Double;
begin
 Temp := FAngle[0].Re * FAngle[1].Re - FAngle[0].Im * FAngle[1].Im;
 FAngle[0].Im := FAngle[0].Im * FAngle[1].Re + FAngle[0].Re * FAngle[1].Im;
 FAngle[0].Re := Temp;

 Temp := FPosition.Re * FAngle[0].Re - FPosition.Im * FAngle[0].Im;
 FPosition.Im := FPosition.Im * FAngle[0].Re + FPosition.Re * FAngle[0].Im;
 FPosition.Re := Temp;

 if FSamplePos = 0 then
  begin
   CalculateStartAngle;
   FSamplePos := FSampleCount;
  end
 else Dec(FSamplePos);
end;

end.
