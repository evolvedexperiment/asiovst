unit DAV_DspFilterMoog;

interface

uses
  DAV_Common, DAV_Complex, DAV_DspFilter;

type
  TCustomMoogFilter = class(TCustomGainFrequencyFilter)
  private
    FCoefficient       : Double;
    FThermalVoltage    : Single;
    FThermalVoltageInv : Single;
    FResonance         : Single;
    FGainCorrection    : Single;
    FScaleFactor       : Single;
    procedure CalculateInvertedThermalVoltage;
    procedure SetThermalVoltage(const Value: Single);
    procedure SetResonance(const Value: Single);
  protected
    procedure ResonanceChanged; virtual;
    procedure ThermalVoltageChanged; virtual;
  public
    constructor Create; override;

    property ThermalVoltage: Single read FThermalVoltage write SetThermalVoltage;
    property Resonance: Single read FResonance write SetResonance;
  end;

  TCustomClassicMoogFilter = class(TCustomMoogFilter)
  protected
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
  end;

  TCustomSimpleClassicMoogFilter = class(TCustomClassicMoogFilter)
  private
    FLastSample     : array [0..3] of Double;
    FTanhLastSample : array [0..2] of Double;
  protected
    procedure CalculateCoefficients; override;
  public
    procedure Reset; override;
  end;

  TCustomImprovedClassicMoogFilter = class(TCustomClassicMoogFilter)
  private
    FLastSample     : array [0..7] of Double;
    FTanhLastSample : array [0..5] of Double;
  protected
    procedure CalculateCoefficients; override;
  public
    procedure Reset; override;
  end;

  TCustomModernMoogFilter = class(TCustomMoogFilter)
  private
    FOrder          : Integer;
    FLastSample     : array of Double;
    FTanhLastSample : array of Double;
  protected
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
  public
    procedure Reset; override;
  end;

  TSimpleClassicMoogFilter = class(TCustomSimpleClassicMoogFilter)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property ThermalVoltage;
    property Resonance;
  end;

  TLightweightSimpleClassicMoogFilter = class(TCustomSimpleClassicMoogFilter)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property ThermalVoltage;
    property Resonance;
  end;

  TImprovedClassicMoogFilter = class(TCustomSimpleClassicMoogFilter)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property ThermalVoltage;
    property Resonance;
  end;

  TLightweightImprovedClassicMoogFilter = class(TCustomSimpleClassicMoogFilter)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property ThermalVoltage;
    property Resonance;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

{ TCustomMoogFilter }

constructor TCustomMoogFilter.Create;
begin
 inherited;
 FThermalVoltage := 5;
 CalculateInvertedThermalVoltage;
end;

procedure TCustomMoogFilter.SetResonance(const Value: Single);
begin
 if FResonance <> Value then
  begin
   FResonance := Value;
   ResonanceChanged;
  end;
end;

procedure TCustomMoogFilter.ResonanceChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomMoogFilter.SetThermalVoltage(const Value: Single);
begin
 if FThermalVoltage <> Value then
  begin
   FThermalVoltage := Value;
   ThermalVoltageChanged;
  end;
end;

procedure TCustomMoogFilter.ThermalVoltageChanged;
begin
 CalculateInvertedThermalVoltage;
 CalculateCoefficients;
end;

procedure TCustomMoogFilter.CalculateInvertedThermalVoltage;
begin
 FThermalVoltageInv := 1 / FThermalVoltage;
end;


{ TCustomClassicMoogFilter }

function TCustomClassicMoogFilter.GetOrder: Cardinal;
begin
 Result := 4;
end;

procedure TCustomClassicMoogFilter.SetOrder(const Value: Cardinal);
begin
 if Value <> 4
  then raise Exception.Create('not implemented yet');
 inherited;
end;


{ TCustomSimpleClassicMoogFilter }

procedure TCustomSimpleClassicMoogFilter.CalculateCoefficients;
begin
 inherited;
 FCoefficient := 2 * FThermalVoltage *
   (1 - Exp(-2 * Pi * Frequency * SampleRateReciprocal));
 FScaleFactor := FGainFactor * sqr(dB_to_Amp(FResonance));
end;

procedure TCustomSimpleClassicMoogFilter.Reset;
begin
 inherited;
 FillChar(FLastSample[0], Length(FLastSample) * SizeOf(Double), 0);
 FillChar(FTanhLastSample[0], Length(FTanhLastSample) * SizeOf(Double), 0);
end;


{ TCustomImprovedClassicMoogFilter }

procedure TCustomImprovedClassicMoogFilter.CalculateCoefficients;
var
  Fc  : Double;
  Fcr : Double;
begin
 inherited;
 // FExpW0.Re

(*
 Fc := Frequency * SampleRateReciprocal;

 // frequency & amplitude correction
 Fcr := 1.8730 * sqr(Fc) * Fc + 0.4955 * sqr(Fc) - 0.6490 * Fc + 0.9988;
 FGainCorrection := -3.9364 * sqr(Fc) + 1.8409 * Fc + 0.9968;
 FCoefficient := 2 * FThermalVoltage * (1 - exp(-Pi * Fcr * Fc)); // Filter Tuning
*)

 FCoefficient := 2 * FThermalVoltage *
   (1 - Exp(-2 * Pi * Frequency * SampleRateReciprocal));
 FScaleFactor := FGainFactor * sqr(dB_to_Amp(FResonance));
end;


procedure TCustomImprovedClassicMoogFilter.Reset;
begin
 inherited;
 FillChar(FLastSample[0], Length(FLastSample) * SizeOf(Double), 0);
 FillChar(FTanhLastSample[0], Length(FTanhLastSample) * SizeOf(Double), 0);
end;

{ TCustomModernMoogFilter }

function TCustomModernMoogFilter.GetOrder: Cardinal;
begin
 Result := FOrder;
end;

procedure TCustomModernMoogFilter.Reset;
begin
 inherited;
 FillChar(FLastSample[0], Length(FLastSample) * SizeOf(Double), 0);
 FillChar(FTanhLastSample[0], Length(FTanhLastSample) * SizeOf(Double), 0);
end;

procedure TCustomModernMoogFilter.SetOrder(const Value: Cardinal);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   SetLength(FLastSample, FOrder);
   if FOrder - 1 >= 0
    then SetLength(FTanhLastSample, FOrder - 1)
    else SetLength(FTanhLastSample, 0);
   Reset;
  end;
 inherited;
end;


{ TSimpleClassicMoogFilter }

function TSimpleClassicMoogFilter.ProcessSample64(Input: Double): Double;
var
  NewInput : Double;
begin
 NewInput := Input - FResonance * FLastSample[3];

 // first stage
 FLastSample[0] := FLastSample[0] + FCoefficient *
   (Tanh(0.5 * NewInput * FThermalVoltageInv) - FTanhLastSample[0]);
 FTanhLastSample[0] := Tanh(0.5 * FLastSample[0] * FThermalVoltageInv);

 // second stage
 FLastSample[1] := FLastSample[1] + FCoefficient * (FTanhLastSample[0] - FTanhLastSample[1]);
 FTanhLastSample[1] := Tanh(0.5 * FLastSample[1] * FThermalVoltageInv);

 // third stage
 FLastSample[2] := FLastSample[2] + FCoefficient * (FTanhLastSample[1] - FTanhLastSample[2]);
 FTanhLastSample[2] := Tanh(0.5 * FLastSample[2] * FThermalVoltageInv);

 // last stage
 FLastSample[3] := FLastSample[3] + FCoefficient *
   (FTanhLastSample[2] - Tanh(0.5 * FLastSample[3] * FThermalVoltageInv));

 Result := FScaleFactor * FLastSample[3];
end;

{ TLightweightSimpleClassicMoogFilter }

function TLightweightSimpleClassicMoogFilter.ProcessSample64(Input: Double): Double;
var
  NewInput : Double;
begin
 NewInput := Input - FResonance * FLastSample[3];

 // first stage
 FLastSample[0] := FLastSample[0] + FCoefficient *
   (FastTanhContinousError4(0.5 * NewInput * FThermalVoltageInv) - FTanhLastSample[0]);
 FTanhLastSample[0] := FastTanhContinousError4(0.5 * FLastSample[0] * FThermalVoltageInv);

 // second stage
 FLastSample[1] := FLastSample[1] + FCoefficient * (FTanhLastSample[0] - FTanhLastSample[1]);
 FTanhLastSample[1] := FastTanhContinousError4(0.5 * FLastSample[1] * FThermalVoltageInv);

 // third stage
 FLastSample[2] := FLastSample[2] + FCoefficient * (FTanhLastSample[1] - FTanhLastSample[2]);
 FTanhLastSample[2] := FastTanhContinousError4(0.5 * FLastSample[2] * FThermalVoltageInv);

 // last stage
 FLastSample[3] := FLastSample[3] + FCoefficient *
   (FTanhLastSample[2] - FastTanhContinousError4(0.5 * FLastSample[3] * FThermalVoltageInv));

 Result := FScaleFactor * FLastSample[3];
end;


{ TImprovedClassicMoogFilter }

function TImprovedClassicMoogFilter.ProcessSample64(Input: Double): Double;
var
  NewInput : Double;
begin
 NewInput := Input - FResonance * FLastSample[3];

 // first stage
 FLastSample[0] := FLastSample[0] + 2 * FThermalVoltage * FCoefficient *
   (Tanh(0.5 * NewInput * FThermalVoltageInv) - FTanhLastSample[0]);
 FTanhLastSample[0] := Tanh(0.5 * FLastSample[0] * FThermalVoltageInv);

 // second stage
 FLastSample[1] := FLastSample[1] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[0] - FTanhLastSample[1]);
 FTanhLastSample[1] := Tanh(0.5 * FLastSample[1] * FThermalVoltageInv);

 // third stage
 FLastSample[2] := FLastSample[2] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[1] - FTanhLastSample[2]);
 FTanhLastSample[2] := Tanh(0.5 * FLastSample[2] * FThermalVoltageInv);

 // last stage
 FLastSample[3] := FLastSample[3] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[2] - Tanh(0.5 * FLastSample[3] * FThermalVoltageInv));

 Result := FScaleFactor * FLastSample[3];
end;


{ TLightweightImprovedClassicMoogFilter }

function TLightweightImprovedClassicMoogFilter.ProcessSample64(Input: Double): Double;
var
  NewInput : Double;
begin
 NewInput := Input - FResonance * FLastSample[3];

 // first stage
 FLastSample[0] := FLastSample[0] + 2 * FThermalVoltage * FCoefficient *
   (FastTanhContinousError4(0.5 * NewInput * FThermalVoltageInv) - FTanhLastSample[0]);
 FTanhLastSample[0] := FastTanhContinousError4(0.5 * FLastSample[0] * FThermalVoltageInv);

 // second stage
 FLastSample[1] := FLastSample[1] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[0] - FTanhLastSample[1]);
 FTanhLastSample[1] := FastTanhContinousError4(0.5 * FLastSample[1] * FThermalVoltageInv);

 // third stage
 FLastSample[2] := FLastSample[2] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[1] - FTanhLastSample[2]);
 FTanhLastSample[2] := FastTanhContinousError4(0.5 * FLastSample[2] * FThermalVoltageInv);

 // last stage
 FLastSample[3] := FLastSample[3] + 2 * FThermalVoltage * FCoefficient *
   (FTanhLastSample[2] - FastTanhContinousError4(0.5 * FLastSample[3] * FThermalVoltageInv));

 Result := FScaleFactor * FLastSample[3];
end;

end.
