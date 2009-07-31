unit DAV_DspRingModulator;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspLfo;

type
  TCustomRingModulator = class(TDspObject)
  end;

  TRingModulator = class(TCustomRingModulator)
  public
    function ProcessSample(Input: Single; Carrier: Single): Single; virtual;
  end;

  TCustomAutoRingModulator = class(TCustomRingModulator)
  private
    FSampleRate : Single;
    FFrequency  : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure FrequencyChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
  public
    constructor Create; virtual;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TCustomAutoRingModulator32 = class(TCustomAutoRingModulator)
  private
    FLfo : TLFOSine32;
  protected
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(Input: Single): Single; virtual;
  end;

  TAutoRingModulator32 = class(TCustomAutoRingModulator32)
  published
    property Frequency;
    property SampleRate;
  end;

implementation

uses
  SysUtils;

{ TRingModulator }

function TRingModulator.ProcessSample(Input, Carrier: Single): Single;
begin
 result := Input * Carrier;
end;

{ TCustomAutoRingModulator }

constructor TCustomAutoRingModulator.Create;
begin
 FSampleRate := 44100;
 FFrequency := 1000;
end;

procedure TCustomAutoRingModulator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomAutoRingModulator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TCustomAutoRingModulator32 }

constructor TCustomAutoRingModulator32.Create;
begin
 inherited;
 FLfo := TLFOSine32.Create;
 FLfo.SampleRate := SampleRate;
 FLfo.Frequency := Frequency; 
end;

destructor TCustomAutoRingModulator32.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomAutoRingModulator32.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
end;

procedure TCustomAutoRingModulator32.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
end;

function TCustomAutoRingModulator32.ProcessSample(Input: Single): Single;
begin
 result := Input * FLfo.Sine;
 FLfo.CalculateNextSample;
end;

end.
