unit DAV_DspFilterLinkwitzRiley;

interface

uses
  Classes, DAV_Common, DAV_DspFilter, DAV_DspButterworthFilter;

type
  TLinkwitzRiley = class(TObject)
  private
    fLowpass    : Array [0..1] of TButterworthLP;
    fHighpass   : Array [0..1] of TButterworthHP;
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
 fLowpass[0]  := TButterworthLP.Create;
 fLowpass[1]  := TButterworthLP.Create;
 fHighpass[0] := TButterworthHP.Create;
 fHighpass[1] := TButterworthHP.Create;
 FSampleRate  := 44100;
 FOrder       := 4;
 FSign        := 1;
 FFrequency   := 1000;
 FrequencyChanged;
 OrderChanged;
end;

destructor TLinkwitzRiley.Destroy;
begin
 FreeAndNil(fLowpass[0]);
 FreeAndNil(fLowpass[1]);
 FreeAndNil(fHighpass[0]);
 FreeAndNil(fHighpass[1]);
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
 fLowpass[0].Frequency  := FFrequency;
 fLowpass[1].Frequency  := FFrequency;
 fHighpass[0].Frequency := FFrequency;
 fHighpass[1].Frequency := FFrequency;
end;

procedure TLinkwitzRiley.OrderChanged;
begin
 fLowpass[0].Order  := FOrder;
 fLowpass[1].Order  := FOrder;
 fHighpass[0].Order := FOrder;
 fHighpass[1].Order := FOrder;
 FSign := 1 - 2 * (FOrder mod 2);
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Single; var Low,
  High: Single);
begin
 Low  := fLowpass[0].ProcessSample(
         fLowpass[1].ProcessSample(Input));
 High := fHighpass[0].ProcessSample(
         fHighpass[1].ProcessSample(FSign * Input));
end;

procedure TLinkwitzRiley.ProcessSample(const Input: Double; var Low,
  High: Double);
begin
 Low  := fLowpass[0].ProcessSample(
         fLowpass[1].ProcessSample(Input));
 High := fHighpass[0].ProcessSample(
         fHighpass[1].ProcessSample(Input));
end;

procedure TLinkwitzRiley.SampleRateChanged;
begin
 fLowpass[0].SampleRate  := FSampleRate;
 fLowpass[1].SampleRate  := FSampleRate;
 fHighpass[0].SampleRate := FSampleRate;
 fHighpass[1].SampleRate := FSampleRate;
end;

end.
