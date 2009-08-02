unit AbxTestSetup;

interface

uses
  Classes, SysUtils, DAV_Common, DAV_DspFilterBasics, DAV_DspLightweightDynamics;

type
  TXAssignment = (xaXisA = 1, xaXisB = 2);
  TGuess = (gNone = 0, gXisA = 1, gXisB = 2);

  TLogNotifyEvent = procedure(Sender: TObject; LogText: string) of object;

  TAbxTestSetupClass = class of TCustomAbxTestSetup;
  TCustomAbxTestSetup = class(TObject)
  private
    FChannelCount: Integer;
    FOnLogMessage: TLogNotifyEvent;
    FOnTestDone: TNotifyEvent;
    procedure SetProcessed(const Value: Boolean);
    procedure SetChannelCount(const Value: Integer);
    procedure ChannelCountChanged;
    procedure SetSampleRate(const Value: Single);
  protected
    FProcessed  : Boolean;
    FStartTrial : TDateTime;
    FStepCount  : Integer;
    FTestInfo   : string;
    FSampleRate : Single;
    procedure ProcessedChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure LogMessage(LogText: string); virtual;
  public
    constructor Create; virtual;
    procedure NextTrial(GuessWasCorrect: Boolean); virtual;
    procedure StartTest; virtual;
    procedure TestDone; virtual;

    function ProcessedAudio(Channel: Integer; Data: Double): Double; virtual; abstract;
    function UnprocessedAudio(Channel: Integer; Data: Double): Double; virtual; abstract;

    property ChannelCount: Integer read FChannelCount write SetChannelCount;
    property Processed: Boolean read FProcessed write SetProcessed;
    property TestInfo: string read FTestInfo;
    property SampleRate: Single read FSampleRate write SetSampleRate;

    property OnLogMessage: TLogNotifyEvent read FOnLogMessage write FOnLogMessage;
    property OnTestDone: TNotifyEvent read FOnTestDone write FOnTestDone;
  end;

  TAbxTestPeakFilterGainSetup = class(TCustomAbxTestSetup)
  protected
    FPeakFilter : array of TBasicPeakFilter;
    FGain_dB    : Single;
    FGainFactor : Single;
    FGainScale  : Single;
    FHysteresis : Integer;

    procedure CalculateGainFactors;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessedAudio(Channel: Integer; Data: Double): Double; override;
    function UnprocessedAudio(Channel: Integer; Data: Double): Double; override;
    procedure NextTrial(GuessWasCorrect: Boolean); override;
    procedure SampleRateChanged; override;
  end;

  TCustomLimiterAbxTestSetup = class(TCustomAbxTestSetup)
  protected
    FLimiter    : array [0..1] of array of TLightweightSoftKneeLimiter;
    FHysteresis : Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessedAudio(Channel: Integer; Data: Double): Double; override;
    function UnprocessedAudio(Channel: Integer; Data: Double): Double; override;
    procedure NextTrial(GuessWasCorrect: Boolean); override;
    procedure SampleRateChanged; override;
  end;

  TLimiterThresholdAbxTestSetup = class(TCustomLimiterAbxTestSetup)
  protected
    FThreshold_dB : Single;
  public
    constructor Create; override;

    procedure NextTrial(GuessWasCorrect: Boolean); override;
  end;

  TLimiterKneeAbxTestSetup = class(TCustomLimiterAbxTestSetup)
  protected
    FKnee_dB : Single;
  public
    constructor Create; override;

    procedure NextTrial(GuessWasCorrect: Boolean); override;
  end;

implementation

uses DAV_DspDynamics;

(*
uses
  ZLib;
*)

{ TCustomAbxTestSetup }

constructor TCustomAbxTestSetup.Create;
begin
 inherited;
 FChannelCount := 2;
 ChannelCountChanged;
 FSampleRate := 44100;
 SampleRateChanged;
end;

procedure TCustomAbxTestSetup.LogMessage(LogText: string);
begin
 if assigned(FOnLogMessage)
  then FOnLogMessage(Self, TimeToStr(Now) + ' - ' + LogText);
end;

procedure TCustomAbxTestSetup.SetChannelCount(const Value: Integer);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;

procedure TCustomAbxTestSetup.SetProcessed(const Value: Boolean);
begin
 if FProcessed <> Value then
  begin
   FProcessed := Value;
   ProcessedChanged;
  end;
end;

procedure TCustomAbxTestSetup.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomAbxTestSetup.StartTest;
begin
 FStartTrial := Now;
 FStepCount := 0;
 LogMessage('Start Test:' + FTestInfo);
end;

procedure TCustomAbxTestSetup.NextTrial(GuessWasCorrect: Boolean);
begin
 inc(FStepCount);
end;

procedure TCustomAbxTestSetup.SampleRateChanged;
begin
 LogMessage('Samplerate changed');
end;

procedure TCustomAbxTestSetup.ProcessedChanged;
begin
 // nothing in here yet
// LogMessage('Processed changed');
end;

procedure TCustomAbxTestSetup.ChannelCountChanged;
begin
 LogMessage('Channel count changed');
end;

procedure TCustomAbxTestSetup.TestDone;
begin
 LogMessage('Test done!');
 if assigned(FOnTestDone)
  then FOnTestDone(Self);
end;

{ TAbxTestPeakFilterGainSetup }

constructor TAbxTestPeakFilterGainSetup.Create;
var
  Channel : Integer;
begin
 inherited;

 FTestInfo   := 'Peak Filter Gain Test';
 FGain_dB    := 10;
 FHysteresis := 0;
 CalculateGainFactors;

 SetLength(FPeakFilter, FChannelCount);
 for Channel := 0 to Length(FPeakFilter) - 1 do
  begin
   FPeakFilter[Channel] := TBasicPeakFilter.Create;
   with FPeakFilter[Channel] do
    begin
     SampleRate := Self.SampleRate;
     Gain       := FGain_dB;
     Frequency  := 1000;
     Bandwidth  := 1;
    end;
  end;
end;

procedure TAbxTestPeakFilterGainSetup.CalculateGainFactors;
begin
 FGainFactor := dB_to_Amp(FGain_dB);
 FGainScale := 0.9 / sqrt(FGainFactor);
end;

destructor TAbxTestPeakFilterGainSetup.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPeakFilter) - 1
  do FreeAndNil(FPeakFilter[Channel]);
 inherited;
end;

procedure TAbxTestPeakFilterGainSetup.NextTrial(GuessWasCorrect: Boolean);
var
  Channel : Integer;
begin
 if GuessWasCorrect
  then LogMessage('Gain perceived: ' + FloatToStr(FGain_dB))
  else LogMessage('Gain NOT perceived: ' + FloatToStr(FGain_dB));

 if FGain_dB > 1 then
  if GuessWasCorrect
   then FGain_dB := FGain_dB * 0.75
   else FGain_dB := FGain_dB * 3.1
 else if FGain_dB > 0.5 then
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 2 then
     begin
      FHysteresis := 0;
      FGain_dB := FGain_dB * 0.8;
     end;
   end
  else FGain_dB := FGain_dB * 1.25
 else
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 3 then
     begin
      FHysteresis := 0;
      FGain_dB := FGain_dB * 0.9;
     end;
   end
  else FGain_dB := FGain_dB * 1.1;

 CalculateGainFactors;

 for Channel := 0 to Length(FPeakFilter) - 1 do
  with FPeakFilter[Channel] do Gain := FGain_dB;

 inherited;

 if FStepCount = 20
  then TestDone;
end;

procedure TAbxTestPeakFilterGainSetup.SampleRateChanged;
var
  Channel : Integer;
begin
 inherited;
 for Channel := 0 to Length(FPeakFilter) - 1
  do FPeakFilter[Channel].SampleRate := SampleRate;
end;

function TAbxTestPeakFilterGainSetup.ProcessedAudio(Channel: Integer;
  Data: Double): Double;
begin
 result := FGainScale * FPeakFilter[Channel].ProcessSample(Data);
end;

function TAbxTestPeakFilterGainSetup.UnprocessedAudio(Channel: Integer;
  Data: Double): Double;
begin
 result := FGainScale * Data;
end;

{ TCustomLimiterAbxTestSetup }

constructor TCustomLimiterAbxTestSetup.Create;
var
  Band, Channel : Integer;
begin
 inherited;
 FHysteresis := 0;
 for Band := 0 to Length(FLimiter) - 1 do
  begin
   SetLength(FLimiter[Band], FChannelCount);
   for Channel := 0 to Length(FLimiter[Band]) - 1 do
    begin
     FLimiter[Band, Channel] := TLightweightSoftKneeLimiter.Create;
     FLimiter[Band, Channel].SampleRate := Self.SampleRate;
    end;
  end;
end;

destructor TCustomLimiterAbxTestSetup.Destroy;
var
  Band, Channel : Integer;
begin
 for Band := 0 to Length(FLimiter) - 1 do
  for Channel := 0 to Length(FLimiter[Band]) - 1
   do FreeAndNil(FLimiter[Band, Channel]);
 inherited;
end;

procedure TCustomLimiterAbxTestSetup.NextTrial(GuessWasCorrect: Boolean);
begin
 inherited;

 if FStepCount = 20
  then TestDone;
end;

procedure TCustomLimiterAbxTestSetup.SampleRateChanged;
var
  Band, Channel : Integer;
begin
 inherited;
 for Band := 0 to Length(FLimiter) - 1 do
  for Channel := 0 to Length(FLimiter[Band]) - 1
   do FLimiter[Band, Channel].SampleRate := SampleRate;
end;

function TCustomLimiterAbxTestSetup.UnprocessedAudio(Channel: Integer;
  Data: Double): Double;
begin
 result := FLimiter[0, Channel].ProcessSample(Data);
end;

function TCustomLimiterAbxTestSetup.ProcessedAudio(Channel: Integer;
  Data: Double): Double;
begin
 result := FLimiter[1, Channel].ProcessSample(Data);
end;

{ TLimiterThresholdAbxTestSetup }

constructor TLimiterThresholdAbxTestSetup.Create;
var
  Band, Channel : Integer;
begin
 inherited;

 FThreshold_dB := -10;

 for Band := 0 to Length(FLimiter) - 1 do
  for Channel := 0 to Length(FLimiter[Band]) - 1 do
   with FLimiter[Band, Channel] do
    begin
     if Band = 0 then
      begin
       Threshold_dB := 0;
       MakeUpGain_dB := -6;
      end
     else
      begin
       Threshold_dB := FThreshold_dB;
       MakeUpGain_dB := -6 - 0.5 * FThreshold_dB ;
      end;
     Attack := 1;
     Release := 10;
     Knee_dB := 0;
    end;
end;

procedure TLimiterThresholdAbxTestSetup.NextTrial(GuessWasCorrect: Boolean);
var
  Channel : Integer;
begin
 if GuessWasCorrect
  then LogMessage('Threshold perceived: ' + FloatToStr(FThreshold_dB))
  else LogMessage('Threshold NOT perceived: ' + FloatToStr(FThreshold_dB));

 if abs(FThreshold_dB) > 1 then
  if GuessWasCorrect
   then FThreshold_dB := FThreshold_dB * 0.75
   else FThreshold_dB := FThreshold_dB * 3.1
 else if abs(FThreshold_dB) > 0.5 then
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 2 then
     begin
      FHysteresis := 0;
      FThreshold_dB := FThreshold_dB * 0.8;
     end;
   end
  else FThreshold_dB := FThreshold_dB * 1.25
 else
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 3 then
     begin
      FHysteresis := 0;
      FThreshold_dB := FThreshold_dB * 0.9;
     end;
   end
  else FThreshold_dB := FThreshold_dB * 1.1;

 for Channel := 0 to Length(FLimiter[1]) - 1 do
  with FLimiter[1, Channel] do
   begin
    Threshold_dB := FThreshold_dB;
    MakeUpGain_dB := -6 - 0.5 * FThreshold_dB;
   end;

 inherited;

 if FStepCount = 20
  then TestDone;
end;

{ TLimiterKneeAbxTestSetup }

constructor TLimiterKneeAbxTestSetup.Create;
var
  Band, Channel : Integer;
begin
 inherited;

 FKnee_dB := 10;

 for Band := 0 to Length(FLimiter) - 1 do
  for Channel := 0 to Length(FLimiter[Band]) - 1 do
   with FLimiter[Band, Channel] do
    begin
     Threshold_dB := -20;
     Attack       := 1;
     Release      := 10;
     case Band of
      0: Knee_dB := 0;
      1: Knee_dB := FKnee_dB;
      else raise Exception.Create('Band must be 0 or 1');
     end;
    end;
end;

procedure TLimiterKneeAbxTestSetup.NextTrial(GuessWasCorrect: Boolean);
var
  Channel : Integer;
begin
 if GuessWasCorrect
  then LogMessage('Threshold perceived: ' + FloatToStr(FKnee_dB))
  else LogMessage('Threshold NOT perceived: ' + FloatToStr(FKnee_dB));

 if abs(FKnee_dB) > 1 then
  if GuessWasCorrect
   then FKnee_dB := FKnee_dB * 0.75
   else FKnee_dB := FKnee_dB * 3.1
 else if abs(FKnee_dB) > 0.5 then
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 2 then
     begin
      FHysteresis := 0;
      FKnee_dB := FKnee_dB * 0.8;
     end;
   end
  else FKnee_dB := FKnee_dB * 1.25
 else
  if GuessWasCorrect then
   begin
    inc(FHysteresis);
    if FHysteresis >= 3 then
     begin
      FHysteresis := 0;
      FKnee_dB := FKnee_dB * 0.9;
     end;
   end
  else FKnee_dB := FKnee_dB * 1.1;

 for Channel := 0 to Length(FLimiter[1]) - 1
  do FLimiter[1, Channel].Knee_dB := FKnee_dB;

 inherited;

 if FStepCount = 20
  then TestDone;
end;

end.