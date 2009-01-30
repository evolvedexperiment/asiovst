unit DAV_DspFilterLinkwitzRiley;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspFilter, DAV_DspButterworthFilter;

type
  TLinkwitzRiley = class(TObject)
  private
    FLowpass    : Array [0..1] of TButterworthLP;
    FHighpass   : Array [0..1] of TButterworthHP;
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
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessSample(const Input: Single; var Low, High: Single); overload;
    procedure ProcessSample(const Input: Double; var Low, High: Double); overload;
  published
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
    property Order: Integer read FOrder write SetOrder;
  end;

implementation

uses
  SysUtils;

{ TLinkwitzRiley }

constructor TLinkwitzRiley.Create;
begin
 inherited;
 FLowpass[0]  := TButterworthLP.Create;
 FLowpass[1]  := TButterworthLP.Create;
 FHighpass[0] := TButterworthHP.Create;
 FHighpass[1] := TButterworthHP.Create;
 FSampleRate  := 44100;
 FOrder       := 4;
 FSign        := 1;
 FFrequency   := 1000;
 FrequencyChanged;
 OrderChanged;
end;

destructor TLinkwitzRiley.Destroy;
begin
 FreeAndNil(FLowpass[0]);
 FreeAndNil(FLowpass[1]);
 FreeAndNil(FHighpass[0]);
 FreeAndNil(FHighpass[1]);
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
 FLowpass[0].Frequency  := FFrequency;
 FLowpass[1].Frequency  := FFrequency;
 FHighpass[0].Frequency := FFrequency;
 FHighpass[1].Frequency := FFrequency;
end;

procedure TLinkwitzRiley.OrderChanged;
begin
 FLowpass[0].Order  := FOrder;
 FLowpass[1].Order  := FOrder;
 FHighpass[0].Order := FOrder;
 FHighpass[1].Order := FOrder;
 FSign := 1 - 2 * (FOrder mod 2);
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Single; var Low,
  High: Single);
begin
 Low  := FLowpass[0].ProcessSample(
         FLowpass[1].ProcessSample(Input));
 High := FHighpass[0].ProcessSample(
         FHighpass[1].ProcessSample(FSign * Input));
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Double; var Low,
  High: Double);
begin
 Low  := FLowpass[0].ProcessSample(
         FLowpass[1].ProcessSample(Input));
 High := FHighpass[0].ProcessSample(
         FHighpass[1].ProcessSample(Input));
end;

procedure TLinkwitzRiley.SampleRateChanged;
begin
 FLowpass[0].SampleRate  := FSampleRate;
 FLowpass[1].SampleRate  := FSampleRate;
 FHighpass[0].SampleRate := FSampleRate;
 FHighpass[1].SampleRate := FSampleRate;
end;

end.
