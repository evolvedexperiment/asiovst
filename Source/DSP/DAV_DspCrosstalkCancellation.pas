unit DAV_DspCrosstalkCancellation;

interface

{$I DAV_Compiler.inc}

(*
        Loudspeaker
  +---+            +---+
  | L |\          /| R |
  +-+-+  x      x  +-+-+
     \     \  /       /
      \     /\      /
       \  x    x  /
      +---+    +---+
      | L | :) | R |
      +---+    +---+
       Listener Head
*)

uses
  DAV_Common, DAV_DspCommon, DAV_DspDelayLines, DAV_DspFilterBasics;

type
  TCustomCrosstalkCancellation = class(TDspSampleRateDependent)
  private
    FSpeakerDistance  : Single;
    FListenerDistance : Single;
    FAttenuation      : Single;
    FHeadRadius       : Single;
    FStageCount       : Cardinal;
    FSampleRate       : Single;
    procedure SetListenerDistance(const Value: Single);
    procedure SetSpeakerDistance(const Value: Single);
    procedure SetStageCount(const Value: Cardinal);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SampleRateChanged; virtual; abstract;
    procedure StageCountChanged; virtual; abstract;
    procedure ListenerDistanceChanged; virtual;
    procedure SpeakerDistanceChanged; virtual;
  public
    constructor Create; virtual;
    procedure ProcessStereo(const Left, Right: Single); overload; virtual; abstract;
    procedure ProcessStereo(const InputLeft, InputRight: Single; var OutputLeft,
      OutputRight: Single); overload; virtual; abstract;

    property SpeakerDistance: Single read FSpeakerDistance write SetSpeakerDistance;
    property ListenerDistance: Single read FListenerDistance write SetListenerDistance;
    property HeadRadius: Single read FHeadRadius;
    property Attenuation: Single read FAttenuation write FAttenuation;
    property StageCount: Cardinal read FStageCount write SetStageCount;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TCrosstalkCancellation32 = class(TCustomCrosstalkCancellation)
  protected
    FDelayLine       : array [0..1] of array of TDelayLineTime32;
    FCrosstalkFilter : array [0..1] of array of TBasicHighShelfFilter;

    procedure CalculateCoefficients; virtual;
    procedure SampleRateChanged; override;
    procedure ListenerDistanceChanged; override;
    procedure SpeakerDistanceChanged; override;
    procedure StageCountChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessStereo(const Left, Right: Single); overload; override;
    procedure ProcessStereo(const InputLeft, InputRight: Single; var OutputLeft, OutputRight: Single); overload; override;
  end;

implementation

uses
  SysUtils, Math, DAV_DspFilter;

{ TCustomCrosstalkCancellation }

constructor TCustomCrosstalkCancellation.Create;
begin
 inherited;
 FHeadRadius       :=   8;
 FSpeakerDistance  := 100;
 FListenerDistance := 100;
 FSampleRate       := 44100;
 FAttenuation      := 0.5;
 SampleRateChanged;
end;

procedure TCustomCrosstalkCancellation.ListenerDistanceChanged;
begin
 if FSpeakerDistance > 2 * FListenerDistance
  then SpeakerDistance := 2 * FListenerDistance;
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

procedure TCustomCrosstalkCancellation.SetStageCount(const Value: Cardinal);
begin
 if FStageCount <> Value then
  begin
   FStageCount := Value;
   StageCountChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SpeakerDistanceChanged;
begin
 if 2 * FListenerDistance > FSpeakerDistance
  then FListenerDistance := 0.5 * SpeakerDistance;
end;

{ TCrosstalkCancellation32 }

constructor TCrosstalkCancellation32.Create;
begin
 inherited;
 FStageCount := 2;
 StageCountChanged;
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

procedure TCrosstalkCancellation32.CalculateCoefficients;
var
  Channel    : Integer;
  Stage      : Integer;
  Alpha      : Single;
  DirectDist : Single;
  CTDistance : Single;
begin
 for Channel := 0 to Length(FCrosstalkFilter) - 1 do
  for Stage := 0 to Length(FCrosstalkFilter[Channel]) - 1 do
   with FCrosstalkFilter[Channel, Stage] do
    begin
     Frequency := 1400;
     Bandwidth := 2.775;
     Gain      := -10;
    end;

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

procedure TCrosstalkCancellation32.ProcessStereo(const Left, Right: Single);
begin
 // not implemented yet!
(*
 Outputs[0, Sample] := FGains[0, 0] * Inputs[0, Sample] +
   FGains[0, 1] * FDelayLine[0, 0].ProcessSample(Inputs[1, Sample]);
 Outputs[1, Sample] := FGains[1, 0] * Inputs[1, Sample] +
   FGains[1, 1] * FDelayLine[1, 0].ProcessSample(Inputs[0, Sample]);
*)

end;

procedure TCrosstalkCancellation32.ProcessStereo(const InputLeft,
  InputRight: Single; var OutputLeft, OutputRight: Single);
var
  Stage   : Integer;
  TempIn  : TDAV2SingleArray;
  TempOut : TDAV2SingleArray;
begin
 TempIn[0] := InputLeft;
 TempIn[1] := InputRight;
 TempOut   := TempIn;
 for Stage := 0 to FStageCount - 1 do
  begin
   TempOut[1] := FDelayLine[0, Stage].ProcessSample(TempIn[1]) - FAttenuation * FCrosstalkFilter[0, Stage].ProcessSample(TempOut[0]);
   TempOut[0] := FDelayLine[1, Stage].ProcessSample(TempIn[0]) - FAttenuation * FCrosstalkFilter[1, Stage].ProcessSample(TempOut[1]);
  end;
 OutputLeft  := TempOut[0];
 OutputRight := TempOut[1];
end;

end.
