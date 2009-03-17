unit DAV_DspFilterLinkwitzRiley;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspButterworthFilter;

type
  TLinkwitzRiley = class(TDspSampleRateDependent)
  private
    FLowpass    : TButterworthLowpassFilter;
    FHighpass   : TButterworthHighpassFilter;
    FSplit      : TButterworthSplitBandFilter;
    FSampleRate : Single;
    FFrequency  : Single;
    FOrder      : Integer;
    FSign       : Single;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure OrderChanged; virtual;
  public
    constructor Create(const Order: Integer = 4); virtual;
    destructor Destroy; override;
    procedure ProcessSample(const Input: Single; out Low, High: Single); overload;
    procedure ProcessSample(const Input: Double; out Low, High: Double); overload;
  published
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
    property Order: Integer read FOrder write SetOrder;
  end;

implementation

uses
  SysUtils;

{ TLinkwitzRiley }

constructor TLinkwitzRiley.Create(const Order: Integer = 4);
begin
 inherited Create;
 FLowpass    := TButterworthLowpassFilter.Create;
 FHighpass   := TButterworthHighpassFilter.Create;
 FSplit      := TButterworthSplitBandFilter.Create;
 FSampleRate := 44100;
 FOrder      := Order;
 FSign       := 1;
 FFrequency  := 1000;
 FrequencyChanged;
 OrderChanged;
end;

destructor TLinkwitzRiley.Destroy;
begin
 FreeAndNil(FLowpass);
 FreeAndNil(FHighpass);
 FreeAndNil(FSplit);
 inherited;
end;

procedure TLinkwitzRiley.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TLinkwitzRiley.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TLinkwitzRiley.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TLinkwitzRiley.FrequencyChanged;
begin
 FLowpass.Frequency  := FFrequency;
 FHighpass.Frequency := FFrequency;
 FSplit.Frequency    := FFrequency;
end;

procedure TLinkwitzRiley.OrderChanged;
begin
 FLowpass.Order  := FOrder;
 FHighpass.Order := FOrder;
 FSplit.Order    := FOrder;
 FSign := 1 - 2 * (FOrder mod 2);
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Single; out Low,
  High: Single);
begin
 FSplit.ProcessSample(Input, Low, High);
 Low  := FLowpass.ProcessSample(Low - CDenorm32);
 High := FHighpass.ProcessSample(FSign * High - CDenorm32);
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Double; out Low,
  High: Double);
begin
 FSplit.ProcessSample(Input, Low, High);
 Low  := FLowpass.ProcessSample(Low);
 High := FHighpass.ProcessSample(FSign * High);
end;

procedure TLinkwitzRiley.SampleRateChanged;
begin
 FLowpass.SampleRate  := FSampleRate;
 FHighpass.SampleRate := FSampleRate;
 FSplit.SampleRate    := FSampleRate;
end;

end.
