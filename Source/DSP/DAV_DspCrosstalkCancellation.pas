unit DAV_DspCrosstalkCancellation;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspDelayLines, DAV_DspFilterBasics;

type
  TCrosstalkFilterType = (cftHighshelf);

  TCustomCrosstalkCancellation = class(TDspSampleRateDependent)
  private
    FSpeakerDistance  : Single;
    FListenerDistance : Single;
    FAttenuation      : Single;
    FHeadRadius       : Single;
    FStageCount       : Integer;
    FSampleRate       : Single;
    FCrosstalkFilterType: TCrosstalkFilterType;
    FCrosstalkFilterFrequency: Single;
    FCrosstalkFilterGain: Single;
    procedure SetListenerDistance(const Value: Single);
    procedure SetSpeakerDistance(const Value: Single);
    procedure SetStageCount(const Value: Integer);
    procedure SetSampleRate(const Value: Single);
    procedure SetCrosstalkFilterType(const Value: TCrosstalkFilterType);
    procedure SetCrosstalkFilterFrequency(const Value: Single);
    procedure SetCrosstalkFilterGain(const Value: Single);
  protected
    procedure SampleRateChanged; virtual; abstract;
    procedure StageCountChanged; virtual; abstract;
    procedure CrosstalkFilterFrequencyChanged; virtual; abstract;
    procedure CrosstalkFilterGainChanged; virtual; abstract;
    procedure ListenerDistanceChanged; virtual;
    procedure SpeakerDistanceChanged; virtual;
  public
    constructor Create; virtual;
    procedure ProcessStereo(var Left, Right: Single); overload; virtual; abstract;

    property Attenuation: Single read FAttenuation write FAttenuation;
    property CrosstalkFilterType: TCrosstalkFilterType read FCrosstalkFilterType write SetCrosstalkFilterType;
    property CrosstalkFilterFrequency: Single read FCrosstalkFilterFrequency write SetCrosstalkFilterFrequency;
    property CrosstalkFilterGain: Single read FCrosstalkFilterGain write SetCrosstalkFilterGain;
    property HeadRadius: Single read FHeadRadius;
    property ListenerDistance: Single read FListenerDistance write SetListenerDistance;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property SpeakerDistance: Single read FSpeakerDistance write SetSpeakerDistance;
    property StageCount: Integer read FStageCount write SetStageCount;
  end;

  TCrosstalkCancellation32 = class(TCustomCrosstalkCancellation)
  protected
    FDelayLine       : array [0..1] of array of TDelayLineTime32;
    FCrosstalkFilter : array [0..1] of array of TBasicHighShelfFilter;

    procedure CalculateCoefficients; virtual;
    procedure CalculateCrosstalkFilter; virtual;
    procedure SampleRateChanged; override;
    procedure ListenerDistanceChanged; override;
    procedure SpeakerDistanceChanged; override;
    procedure CrosstalkFilterFrequencyChanged; override;
    procedure CrosstalkFilterGainChanged; override;
    procedure StageCountChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessStereo(var Left, Right: Single); overload; override;
  end;

implementation

uses
  SysUtils, Math, DAV_DspFilter;

{ TCustomCrosstalkCancellation }

constructor TCustomCrosstalkCancellation.Create;
begin
 inherited;
 FHeadRadius               :=   8;
 FSpeakerDistance          := 100;
 FListenerDistance         := 100;
 FSampleRate               := 44100;
 FAttenuation              := 0.5;
 FCrosstalkFilterFrequency := 1000;
 FCrosstalkFilterGain      := -10;
 SampleRateChanged;
end;

procedure TCustomCrosstalkCancellation.ListenerDistanceChanged;
begin
 if FSpeakerDistance > 2 * FListenerDistance
  then SpeakerDistance := 2 * FListenerDistance;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterFrequency(
  const Value: Single);
begin
 if FCrosstalkFilterFrequency <> Value then
  begin
   FCrosstalkFilterFrequency := Value;
   CrosstalkFilterFrequencyChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterGain(
  const Value: Single);
begin
 if FCrosstalkFilterGain <> Value then
  begin
   FCrosstalkFilterGain := Value;
   CrosstalkFilterGainChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterType(
  const Value: TCrosstalkFilterType);
begin
 if FCrosstalkFilterType <> Value then
  begin
   FCrosstalkFilterType := Value;
  end;
end;

procedure TCustomCrosstalkCancellation.SetListenerDistance(const Value: Single);
begin
 if FListenerDistance <> Value then
  begin
   FListenerDistance := Value;
   ListenerDistanceChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetSpeakerDistance(const Value: Single);
begin
 if FSpeakerDistance <> Value then
  begin
   FSpeakerDistance := Value;
   SpeakerDistanceChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetStageCount(const Value: Integer);
begin
 if FStageCount <> Value then
  begin
   FStageCount := Value;
   StageCountChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SpeakerDistanceChanged;
begin
 if 2 * FListenerDistance < FSpeakerDistance
  then FListenerDistance := 0.5 * SpeakerDistance;
end;

{ TCrosstalkCancellation32 }

constructor TCrosstalkCancellation32.Create;
begin
 inherited;
 FStageCount := 2;
 StageCountChanged;
 CalculateCrosstalkFilter;
end;

destructor TCrosstalkCancellation32.Destroy;
var
  Channel : Cardinal;
  Stage   : Cardinal;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  for Stage := 0 to Length(FDelayLine[Channel]) - 1
   do FreeAndNil(FDelayLine[Channel, Stage]);
  inherited;
end;

procedure TCrosstalkCancellation32.CrosstalkFilterFrequencyChanged;
begin
 CalculateCrosstalkFilter;
end;

procedure TCrosstalkCancellation32.CrosstalkFilterGainChanged;
begin
 CalculateCrosstalkFilter;
end;

procedure TCrosstalkCancellation32.CalculateCrosstalkFilter;
var
  Channel    : Integer;
  Stage      : Integer;
begin
 for Channel := 0 to Length(FCrosstalkFilter) - 1 do
  for Stage := 0 to Length(FCrosstalkFilter[Channel]) - 1 do
   with FCrosstalkFilter[Channel, Stage] do
    begin
     Frequency := FCrosstalkFilterFrequency;
     Gain      := FCrosstalkFilterGain;
     Bandwidth := 3.3;
    end;
end;

procedure TCrosstalkCancellation32.CalculateCoefficients;
var
  Channel    : Integer;
  Stage      : Integer;
  Alpha      : Single;
  DirectDist : Single;
  CTDistance : Single;
begin
 if FListenerDistance < 0.5 * FSpeakerDistance
  then exit;
 assert(FListenerDistance >= 0.5 * FSpeakerDistance);

 Alpha := arcsin(FSpeakerDistance * 0.5 / FListenerDistance);
 DirectDist := sqrt(sqr(FHeadRadius) + sqr(FListenerDistance) -
   2 * FHeadRadius * FListenerDistance * cos(0.5 * Pi - Alpha));

 CTDistance := sqrt(sqr(FHeadRadius) + sqr(FListenerDistance) -
   2 * FHeadRadius * FListenerDistance * cos(0.5 * Pi + Alpha));

 assert(CTDistance - DirectDist > 0);

 for Channel := 0 to Length(FDelayLine) - 1 do
  for Stage := 0 to Length(FDelayLine[Channel]) - 1 do
   begin
    FDelayLine[Channel, Stage].Time := (CTDistance - DirectDist) * 2.94E-5;
   end;
end;

procedure TCrosstalkCancellation32.ListenerDistanceChanged;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TCrosstalkCancellation32.SpeakerDistanceChanged;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TCrosstalkCancellation32.SampleRateChanged;
var
  Channel : Cardinal;
  Stage   : Cardinal;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  if assigned(FDelayLine[Channel]) then
   for Stage := 0 to Length(FDelayLine[Channel]) - 1
    do FDelayLine[Channel, Stage].Samplerate := SampleRate;
 for Channel := 0 to Length(FDelayLine) - 1 do
  if assigned(FCrosstalkFilter[Channel]) then
   for Stage := 0 to Length(FCrosstalkFilter[Channel]) - 1
    do FCrosstalkFilter[Channel, Stage].Samplerate := SampleRate;
end;

procedure TCrosstalkCancellation32.StageCountChanged;
var
  Channel       : Integer;
  Stage         : Integer;
  OldStageCount : Integer;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  begin
   if FStageCount > Length(FDelayLine[Channel]) then
    begin
     OldStageCount := Length(FDelayLine[Channel]);
     SetLength(FDelayLine[Channel], FStageCount);
     SetLength(FCrosstalkFilter[Channel], FStageCount);
     for Stage := OldStageCount to FStageCount - 1 do
      begin
       FDelayLine[Channel, Stage] := TDelayLineTime32.Create;
       FCrosstalkFilter[Channel, Stage] := TBasicHighShelfFilter.Create;
      end;
    end
   else
    begin
     for Stage := Length(FDelayLine[Channel]) - 1 downto FStageCount do
      begin
       FreeAndNil(FDelayLine[Channel, Stage]);
       FreeAndNil(FCrosstalkFilter[Channel, Stage]);
      end;
     SetLength(FDelayLine[Channel], FStageCount);
     SetLength(FCrosstalkFilter[Channel], FStageCount);
    end;
  end;
 CalculateCoefficients;
end;

procedure TCrosstalkCancellation32.ProcessStereo(var Left, Right: Single);
var
  Stage   : Integer;
  TempIn  : TDAV2SingleArray;
  Temp    : Single;
begin
(*
 TempIn[0] := InputLeft;
 TempIn[1] := InputRight;
 for Stage := 0 to FStageCount - 1 do
  begin
   TempIn[1] := FDelayLine[0, Stage].ProcessSample(TempIn[1]) - FAttenuation * FCrosstalkFilter[0, Stage].ProcessSample(TempIn[0]);
   TempIn[0] := FDelayLine[1, Stage].ProcessSample(TempIn[0]) - FAttenuation * FCrosstalkFilter[1, Stage].ProcessSample(TempIn[1]);
  end;
 OutputLeft  := TempIn[0];
 OutputRight := TempIn[1];
*)

 TempIn[0]   := Left;
 TempIn[1]   := Right;

 for Stage := 0 to FStageCount - 1 do
  begin
   Temp      := FDelayLine[0, Stage].ProcessSample(FAttenuation * FCrosstalkFilter[0, Stage].ProcessSample(-TempIn[1]));
   TempIn[1] := FDelayLine[1, Stage].ProcessSample(FAttenuation * FCrosstalkFilter[1, Stage].ProcessSample(-TempIn[0]));
   TempIn[0] := Temp;

   Left  := Left  + TempIn[0];
   Right := Right + TempIn[1];
  end;
end;

end.
