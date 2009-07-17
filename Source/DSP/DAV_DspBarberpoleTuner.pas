unit DAV_DspBarberpoleTuner;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspLfo, DAV_DspTuner,
  DAV_DspFilterButterworth;

type
  TCustomBarberpoleFilter = class(TDspObject)
  private
    FLFO        : TLFOSine32;
    FLowpass    : TButterworthLowPassFilter;
    FSampleRate : Single;
    function GetFrequency: Single;
    function GetOrder: Integer;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Process(Input: Single): Single; virtual;

    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read GetFrequency write SetFrequency;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleFilter = class(TCustomBarberpoleFilter)
  published
    property Frequency;
    property Order;
    property SampleRate;
  end;

  TCustomBarberpoleTuner = class(TCustomTuner)
  private
    FBarberpoleFilter : TBarberpoleFilter;
    FZCTuner          : TZeroCrossingTuner;
    function GetFrequency: Single;
    function GetFrequencyDifference: Single;
    function GetOrder: Integer;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; override;
    function GetCurrentFrequency: Single; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Process(Input: Single); override;

    property Frequency: Single read GetFrequency write SetFrequency;
    property FrequencyDifference: Single read GetFrequencyDifference;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleTuner = class(TCustomBarberpoleTuner)
  published
    property Frequency;
    property Order;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

resourcestring
  RCStrOrderMustBeLarger0 = 'Order must be larger than 0!';

{ TCustomBarberpoleFilter }

constructor TCustomBarberpoleFilter.Create;
begin
 inherited;
 FSampleRate := 44100;

 FLFO := TLFOSine32.Create;
 with FLFO do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FLowpass := TButterworthLowPassFilter.Create(4);
 with FLowpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
  end;
end;

destructor TCustomBarberpoleFilter.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FLowpass);
 inherited;
end;

function TCustomBarberpoleFilter.GetFrequency: Single;
begin
 result := FLFO.Frequency;
end;

function TCustomBarberpoleFilter.GetOrder: Integer;
begin
 result := FLowpass.Order;
end;

function TCustomBarberpoleFilter.Process(Input: Single): Single;
begin
 inherited;
 FLFO.CalculateNextSample;
 result := FLowpass.ProcessSample(FLFO.Sine * Input);
end;

procedure TCustomBarberpoleFilter.SampleRateChanged;
begin
 inherited;
 if assigned(FLFO) then FLFO.SampleRate := SampleRate;
 if assigned(FLowpass) then FLowpass.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleFilter.SetFrequency(const Value: Single);
begin
 if FLFO.Frequency <> Value then
  begin
   FLFO.Frequency := Value;
   FLowpass.Frequency := Value * FastPower2MinError4(2 * COneTwelfth32) - Value;
  end;
end;

procedure TCustomBarberpoleFilter.SetOrder(const Value: Integer);
begin
 if Value > 0
  then FLowpass.Order := Value
  else raise Exception.Create(RCStrOrderMustBeLarger0);
end;

procedure TCustomBarberpoleFilter.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TCustomBarberpoleTuner }

constructor TCustomBarberpoleTuner.Create;
begin
 inherited;
 FBarberpoleFilter := TBarberpoleFilter.Create;
 with FBarberpoleFilter do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FZCTuner := TZeroCrossingTuner.Create;
 with FZCTuner do
  begin
   MinimumFrequency := 0;
   MaximumFrequency := 2 * Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
   OneCrossingOnly := True;
   SampleRate := Self.SampleRate;
  end;
end;

destructor TCustomBarberpoleTuner.Destroy;
begin
 FreeAndNil(FBarberpoleFilter);
 FreeAndNil(FZCTuner);
 inherited;
end;

procedure TCustomBarberpoleTuner.SampleRateChanged;
begin
 inherited;
 if assigned(FBarberpoleFilter) then FBarberpoleFilter.SampleRate := SampleRate;
 if assigned(FZCTuner) then FZCTuner.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleTuner.SetFrequency(const Value: Single);
begin
 FBarberpoleFilter.Frequency := Value;
 FZCTuner.MaximumFrequency := 2 * Frequency * FastPower2MinError4(2 * COneTwelfth32) - Frequency;
end;

procedure TCustomBarberpoleTuner.SetOrder(const Value: Integer);
begin
 FBarberpoleFilter.Order := Value;
end;

function TCustomBarberpoleTuner.GetCurrentFrequency: Single;
begin
 result := Frequency + FrequencyDifference;
end;

function TCustomBarberpoleTuner.GetFrequency: Single;
begin
 result := FBarberpoleFilter.Frequency;
end;

function TCustomBarberpoleTuner.GetFrequencyDifference: Single;
begin
 result := FZCTuner.CurrentFrequency;
end;

function TCustomBarberpoleTuner.GetOrder: Integer;
begin
 result := FBarberpoleFilter.Order;
end;

procedure TCustomBarberpoleTuner.Process(Input: Single);
begin
 inherited;
 FZCTuner.Process(FBarberpoleFilter.Process(Input));
end;

end.
