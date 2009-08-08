unit VocInputDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule,
  DAV_DspVoiceInput;

type
  TVocInputDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterTrackingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMaxFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterPitchDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTrackingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPitchChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBreathChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaxFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterVoicedUnvoicedDetectorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLowpassState     : Array [0..1] of Single;
    FLowBuffer        : Array [0..1] of Single;
    FPitch            : Single;
    FPitchStep        : Single;
    FSawbuf           : Single;
    FNoise            : Single;
    FLowEnv           : Single;
    FHighEnv          : Single;
    FLowFreq          : Single;
    FVUv              : Single;
    FRoot             : Single;
    FMinPitch         : Single;
    FMaxPitch         : Single;
    FTempMinPitch     : Single;
    FPitchMult        : Single;
    FInvSampleRate    : Single;
    FMaximumFrequency : Single;
    FTrack            : TTrackingType;
    function Midi2String(const n : Single): string;
    procedure CalculateLowFrequency;
    procedure CalculatePitchStep;
    procedure PitchChanged;
    procedure CalculateFrequencyBounds;
    procedure ResetStates;
  protected  
    procedure TrackingChanged; virtual;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TVocInputDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 0.5; // Tracking Off / On / Quant
 Parameter[1] := 0.5; // Pitch
 Parameter[2] := 20;  // Breath FNoise
 Parameter[3] := 50;  // Voiced / Unvoiced Thresh
 Parameter[4] := 69;  // Max Freq

 FInvSampleRate := 1 / SampleRate;
 CalculateLowFrequency;
end;

procedure TVocInputDataModule.ParameterTrackingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FTrack <> TTrackingType(round(Value)) then
  begin
   FTrack := TTrackingType(round(Value));
   TrackingChanged;
  end;
end;

procedure TVocInputDataModule.TrackingChanged;
begin
 CalculatePitchStep;
end;

procedure TVocInputDataModule.CalculatePitchStep;
begin
 if (FTrack = ttOff)
  then FPitchStep := 110.0 * FPitchMult * FInvSampleRate
  else FPitchStep := 0;
end;

procedure TVocInputDataModule.ParameterTrackingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[index]) of
  0: PreDefined := 'OFF';
  1: PreDefined := 'FREE';
  2: PreDefined := 'QUANT';
 end;
end;

procedure TVocInputDataModule.ParameterPitchChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TVocInputDataModule.PitchChanged;
begin
  FPitchMult := Power(1.0594631, Round(48 * FPitch - 24));
  CalculatePitchStep;
end;

procedure TVocInputDataModule.ParameterPitchDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if FTrack = ttOff
  then PreDefined := Midi2string(round(48.0 * Parameter[Index] + 21.0))
  else PreDefined := IntToStr(Round(48.0 * Parameter[Index] - 24.0));
end;

procedure TVocInputDataModule.ParameterBreathChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FNoise := 6 * 0.01 * Value;
end;

procedure TVocInputDataModule.ParameterVoicedUnvoicedDetectorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVUv := sqr(0.01 * Parameter[3]);
end;

procedure TVocInputDataModule.ParameterMaxFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FMaximumFrequency <> Value then
  begin
   FMaximumFrequency := Value;
   CalculateFrequencyBounds;
  end;
end;

procedure TVocInputDataModule.CalculateFrequencyBounds;
begin
 FMinPitch := Power(16, 0.5 - (FMaximumFrequency - 45) / 48) * SampleRate / 440;
 FMaxPitch := 0.03 * SampleRate;
 FTempMinPitch := FMinPitch;
end;

procedure TVocInputDataModule.ParameterMaxFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := Midi2string(Parameter[4]);
end;

function TVocInputDataModule.Midi2String(const n : Single) : string; //show name of MIDI note number (60=C3)
var
  o, s : Integer;
begin
 result := '   ';
 o      := round(n / 12 - 0.49999);
 s      := round(n - (12 * o) - 0.49999);
 o      := o - 2;

 case s of
    0: result := result + 'C';
    1: result := result + 'C#';
    2: result := result + 'D';
    3: result := result + 'Eb';
    4: result := result + 'E';
    5: result := result + 'F';
    6: result := result + 'F#';
    7: result := result + 'G';
    8: result := result + 'G#';
    9: result := result + 'A';
   10: result := result + 'Bb';
  else result := result + 'B';
 end;

 result := result + ' ';

 if (o < 0) then result := result + '-';
 result := result + char(48 + (abs(o) mod 10));
end;

procedure TVocInputDataModule.CalculateLowFrequency;
begin
 FLowFreq := 660 * FInvSampleRate;
end;

procedure TVocInputDataModule.ResetStates;
begin
 FLowBuffer[0] := 0;
 FLowBuffer[1] := 0;
 FLowBuffer[0] := 0;
 FLowBuffer[1] := 0;
 FPitchStep    := 0;
 FSawbuf       := 0;
 FLowEnv       := 0;
end;

procedure TVocInputDataModule.VSTModuleSuspend(Sender: TObject);
begin
 ResetStates;
end;

procedure TVocInputDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FInvSampleRate := 1 / SampleRate;
 FRoot := log10(8.1757989 * FInvSampleRate);
 CalculatePitchStep;
 CalculateLowFrequency;
 CalculateFrequencyBounds;
end;

procedure TVocInputDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
const
  CRootM : Single = 39.863137;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // fundamental filter (peaking 2nd-order 100Hz lpf)
   FLowpassState[0] := FLowpassState[0] - FLowFreq * (FLowpassState[1] + Inputs[0, Sample]);
   FLowpassState[1] := FLowpassState[1] - FLowFreq * (FLowpassState[1] - FLowpassState[0]);

   // fundamental level
   FLowEnv := FLowEnv - FLowFreq * 0.1 * (FLowEnv - abs(FLowpassState[0]));

   Outputs[1, Sample] := abs((Inputs[0, Sample] + 0.03) * FVUv);

   // overall level (+ constant so >f0 when quiet)
   FHighEnv := FHighEnv - FLowFreq * 0.1 * (FHighEnv - Outputs[1, Sample]);

   FLowBuffer[1] := FLowBuffer[1] + 1;
   if FTrack > ttOff then                                                         // pitch tracking
    begin
     if ((FLowpassState[1] > 0) and (FLowBuffer[0] <= 0)) then                    // found +ve zero crossing
      begin
       if ((FLowBuffer[1] > FTempMinPitch) and (FLowBuffer[1] < FMaxPitch)) then  // ...in allowed range
        begin
         FTempMinPitch := 0.6 * FLowBuffer[1];                                    // new max pitch to discourage octave jumps!
         FLowBuffer[0] := FLowpassState[1] / (FLowpassState[1] - FLowBuffer[0]);  // fractional period...
         FPitchStep := FPitchMult / (FLowBuffer[1] - FLowBuffer[0]);              // new period

         if (FTrack = ttQuantized) then                                           // quantize pitch
          begin
           FPitchStep := CRootM * (log10(FPitchStep) - FRoot);
           FPitchStep := Power(1.0594631, trunc(FPitchStep + 0.5) + CRootM * FRoot);
          end;
        end;
       FLowBuffer[1] := FLowBuffer[0];                                            // restart period measurement
       FTempMinPitch := 0.9 * FTempMinPitch + 0.1 * FMinPitch;
      end;
     FLowBuffer[0] := FLowpassState[1];                                           // remember previous sample
    end;

//   Outputs[1, Sample] := 0.16384 * (2 * random - 1); // sibilance, but not used here


   // ...or modulated breath noise
   if (FLowEnv > FHighEnv)
    then Outputs[1, Sample] := Outputs[1, Sample] * FSawbuf * FNoise;
   Outputs[1, Sample] := Outputs[1, Sample] + FSawbuf;
   FSawbuf := FSawbuf + FPitchStep;

   // badly aliased sawtooth!
   if (FSawbuf > 0.5)
    then FSawbuf := FSawbuf - 1;

   Outputs[0, Sample] := Inputs[0, Sample];
  end;

 if (abs(FHighEnv) < 1E-10)
  then FHighEnv := 0;

 // catch denormals
 if (abs(FLowpassState[1]) < 1E-10) then
  begin
   FLowBuffer[0] := 0;
   FLowBuffer[1] := 0;
   FLowEnv       := 0;
  end;
end;

end.
