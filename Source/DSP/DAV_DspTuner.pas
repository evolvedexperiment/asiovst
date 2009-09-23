unit DAV_DspTuner;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$DEFINE OnlineFreqCalc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_Classes, DAV_DSPFilterButterworth,
  DAV_DspCorrelation, DAV_DspCepstrum;

type
  TCustomTuner = class(TDspSampleRatePersistent, IDspSink32)
  protected
    function GetCurrentFrequency: Single; virtual; abstract;
  public
    procedure ProcessSample32(Input: Single); virtual; abstract;
    property CurrentFrequency: Single read GetCurrentFrequency;
  end;

  TCustomDownsampledTuner = class(TCustomTuner)
  private
    FMaximumFrequency : Single;
    FMinimumFrequency : Single;
    FDownsampleBW     : Single;
    function GetDSFilterOrder: Cardinal;
    procedure SetDSFilterOrder(const Value: Cardinal);
    procedure SetMaximumFrequency(const Value: Single);
    procedure SetMinimumFrequency(const Value: Single);
    procedure SetupMaximumFrequency;
    procedure SetDownsampleBW(Value: Single);
  protected
    FLowpass           : TButterworthLowPassFilter;
    FHighpass          : TButterworthHighPassFilter;
    FDownSampleFactor  : Integer;
    FDownSampleCounter : Integer;
    procedure ProcessDownsampled(DownSampled: Single); virtual; abstract;
    procedure SampleRateChanged; override;
    procedure CalculateDownsampleFactor; virtual;
    procedure MaximumFrequencyChanged; virtual;
    procedure MinimumFrequencyChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure ProcessSample32(Input: Single); override;

    property DownSampleFilterOrder: Cardinal read GetDSFilterOrder write SetDSFilterOrder;
    property DownSampleBandwidth: Single read FDownsampleBW write SetDownsampleBW;
    property MaximumFrequency: Single read FMaximumFrequency write SetMaximumFrequency;
    property MinimumFrequency: Single read FMinimumFrequency write SetMinimumFrequency;
  end;

  TCustomZeroCrossingTuner = class(TCustomDownsampledTuner)
  private
    FSmoothFactor   : Single;
    procedure SetSmoothFactor(const Value: Single);
    procedure SetOneCrossingOnly(const Value: Boolean);
  protected
    FIsAbove         : Boolean;
    FSamples         : Integer;
    FAverageSamples  : Single;
    FOneCrossingOnly : Boolean;
    FFrequencyFactor : Single;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq     : Single;
    {$ENDIF}
    function GetCurrentFrequency: Single; override;
    procedure SmoothFactorChanged; virtual;
    procedure ProcessDownsampled(Downsampled: Single); override;
    function CalculateCurrentFrequency: Single; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    property SmoothFactor: Single read FSmoothFactor write SetSmoothFactor;
    property OneCrossingOnly: Boolean read FOneCrossingOnly write SetOneCrossingOnly;
  end;

  TCustomLinearZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  protected
    FLastSample : Single;
    FLastOffset : Single;
    procedure ProcessDownsampled(Downsampled: Single); override;
  end;

  TTunerNoteString = array [0..1] of AnsiChar;
  TCustomAdvancedTuner = class(TCustomLinearZeroCrossingTuner)
  private
    FCurrentNote             : TTunerNoteString;
    FCurrentDetune           : Single;
    FAttack, FAttackFactor   : Single;
    FRelease, FReleaseFactor : Single;
    FLevel, FThreshold       : Single;

    procedure SetAttack(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetThreshold(const Value: Single);
  protected
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure CalculateDownsampleFactor; override;
    function CalculateCurrentFrequency: Single; override;
    procedure ProcessDownsampled(Downsampled: Single); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    property Attack: Single read FAttack write SetAttack;
    property Release: Single read FRelease write SetRelease;
    property Threshold: Single read FThreshold write SetThreshold;
    property CurrentNote: TTunerNoteString read FCurrentNote;
    property CurrentDetune: Single read FCurrentDetune;
  end;

  TCustomCepstrumTuner = class(TCustomDownsampledTuner)
  private
    procedure CalculateBufferLength;
  protected
    FBuffer              : PDAVSingleFixedArray;
    FCepstrum            : PDAVComplexSingleFixedArray;
    FCepstrumCalculation : TPowerCepstrum32;
    FBufferLength        : Integer;
    FBufferPos           : Integer;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq         : Single;
    {$ENDIF}
    function GetCurrentFrequency: Single; override;
    procedure ProcessDownsampled(Downsampled: Single); override;
    function CalculateCurrentFrequency: Single; virtual;
    procedure CalculateDownsampleFactor; override;
    procedure SampleRateChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property OneCrossingOnly;
    property SmoothFactor;
  end;

  TLinearZeroCrossingTuner = class(TCustomLinearZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property OneCrossingOnly;
    property SmoothFactor;
  end;

  TAdvancedTuner = class(TCustomAdvancedTuner)
  published
    property CurrentDetune;
  end;

  TCepstrumTuner = class(TCustomCepstrumTuner);

  TTuner = class(TLinearZeroCrossingTuner);

implementation

uses
  SysUtils, Math, DAV_Approximations, DAV_DspWindowing;

{ TCustomDownsampledTuner }

constructor TCustomDownsampledTuner.Create;
begin
 inherited;

 FLowpass  := TButterworthLowPassFilter.Create(4);
 FHighpass := TButterworthHighPassFilter.Create(2);
 FMaximumFrequency := 4000;
 FMinimumFrequency := 100;
 FDownsampleBW     := 0.7;

 FDownSampleCounter := 1;

 MinimumFrequencyChanged;
 SetupMaximumFrequency;
 SampleRateChanged;
end;

procedure TCustomDownsampledTuner.SampleRateChanged;
begin
 inherited;
 CalculateDownsampleFactor;
 FLowpass.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate / FDownSampleFactor;
end;

procedure TCustomDownsampledTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDownsampledTuner then
  with TCustomDownsampledTuner(Dest) do
   begin
    inherited;
    FLowpass.Assign(Self.FLowpass);
    FHighpass.Assign(Self.FHighpass);
    FDownSampleFactor  := Self.FDownSampleFactor;
    FDownSampleCounter := Self.FDownSampleCounter;
   end
 else inherited;
end;

procedure TCustomDownsampledTuner.CalculateDownsampleFactor;
var
  NewFactor      : Integer;
  CurrentNyquist : Single;
begin
 CurrentNyquist := 0.5 * SampleRate;
 NewFactor := 1;
 while FDownsampleBW * 0.5 * CurrentNyquist > MaximumFrequency do
  begin
   CurrentNyquist := 0.5 * CurrentNyquist;
   NewFactor := NewFactor shl 1;
  end;
 FDownSampleFactor := NewFactor;
end;

function TCustomDownsampledTuner.GetDSFilterOrder: Cardinal;
begin
 result := FLowpass.Order;
end;

procedure TCustomDownsampledTuner.SetupMaximumFrequency;
begin
 FLowpass.Frequency := FMaximumFrequency;
end; 

procedure TCustomDownsampledTuner.MaximumFrequencyChanged;
begin
 SetupMaximumFrequency;
 CalculateDownsampleFactor;
 FHighpass.SampleRate := SampleRate / FDownSampleFactor;
end;

procedure TCustomDownsampledTuner.MinimumFrequencyChanged;
begin
 FHighpass.Frequency := FMinimumFrequency;
end;

procedure TCustomDownsampledTuner.ProcessSample32(Input: Single);
var
  LowpassedSignal : Double;
begin
 LowpassedSignal := FLowpass.ProcessSample64(Input);
 Dec(FDownSampleCounter);
 if FDownSampleCounter = 0 then
  begin
   FDownSampleCounter := FDownSampleFactor;
   ProcessDownsampled(FHighpass.ProcessSample64(LowpassedSignal));
  end;
end;

procedure TCustomDownsampledTuner.SetDownsampleBW(Value: Single);
begin
 Value := Limit(Value, 0, 1);
 if Value <> FDownsampleBW then
  begin
   FDownsampleBW := Value;
   CalculateDownsampleFactor;
  end;
end;

procedure TCustomDownsampledTuner.SetDSFilterOrder(const Value: Cardinal);
begin
 if FLowpass.Order <> Value then
  begin
   FLowpass.Order := Value;
   CalculateDownsampleFactor;
  end;
end;

procedure TCustomDownsampledTuner.SetMaximumFrequency(const Value: Single);
begin
 if FMaximumFrequency <> Value then
  begin
   FMaximumFrequency := Value;
   MaximumFrequencyChanged;
  end;
end;

procedure TCustomDownsampledTuner.SetMinimumFrequency(const Value: Single);
begin
 if FMinimumFrequency <> Value then
  begin
   FMinimumFrequency := Value;
   MinimumFrequencyChanged;
  end;
end;

{ TCustomZeroCrossingTuner }

constructor TCustomZeroCrossingTuner.Create;
begin
 inherited;
 FOneCrossingOnly := True;
 FFrequencyFactor := 1;
 FSmoothFactor    := 0.99;
 FAverageSamples  := FFrequencyFactor * SampleRate / (DownSampleFilterOrder * 440);
end;

function TCustomZeroCrossingTuner.GetCurrentFrequency: Single;
begin
 {$IFDEF OnlineFreqCalc}
 result := FCurrentFreq;
 {$ELSE}
 result := CalculateCurrentFrequency;
 {$ENDIF}
end;

procedure TCustomZeroCrossingTuner.ProcessDownsampled(Downsampled: Single);
begin
 if (Downsampled < 0) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * FSamples;
   FSamples := 1;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
end;

procedure TCustomZeroCrossingTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomZeroCrossingTuner then
  with TCustomZeroCrossingTuner(Dest) do
   begin
    inherited;
    FIsAbove         := Self.FIsAbove;
    FSamples         := Self.FSamples;
    FAverageSamples  := Self.FAverageSamples;
    FOneCrossingOnly := Self.FOneCrossingOnly;
    FFrequencyFactor := Self.FFrequencyFactor;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq     := Self.FCurrentFreq;
    {$ENDIF}
   end
 else inherited;
end;

function TCustomZeroCrossingTuner.CalculateCurrentFrequency: Single;
begin
 result := FFrequencyFactor * SampleRate / (FDownSampleFactor * FAverageSamples);
end;

procedure TCustomZeroCrossingTuner.SetOneCrossingOnly(const Value: Boolean);
begin
 if FOneCrossingOnly <> Value then
  begin
   FOneCrossingOnly := Value;
   if FOneCrossingOnly
    then FFrequencyFactor := 1
    else FFrequencyFactor := 0.5;
  end;
end;

procedure TCustomZeroCrossingTuner.SetSmoothFactor(const Value: Single);
begin
 if FSmoothFactor <> Value then
  begin
   FSmoothFactor := Value;
   SmoothFactorChanged;
  end;
end;

procedure TCustomZeroCrossingTuner.SmoothFactorChanged;
begin
// FSmoothFactor := exp( -ln2 / (FSmooth * 0.001 * SampleRate));
end;


{ TCustomLinearZeroCrossingTuner }

procedure TCustomLinearZeroCrossingTuner.ProcessDownsampled(
  Downsampled: Single);
var
  Offset : Single;
begin
 if (Downsampled < 0) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
 FLastSample := Downsampled;
end;

{ TCustomAdvancedTuner }

function TCustomAdvancedTuner.CalculateCurrentFrequency: Single;
var
  CurrentNote : Single;
begin
 result := inherited CalculateCurrentFrequency;

 CurrentNote := 12 * FastLog2ContinousError4(FCurrentFreq / 440);
 while CurrentNote < -6 do CurrentNote := CurrentNote + 12;
 while CurrentNote >  6 do CurrentNote := CurrentNote - 12;

 case round(CurrentNote) of
  -6, 6 : FCurrentNote := 'Eb';
  -5    : FCurrentNote := 'E';
  -4    : FCurrentNote := 'F';
  -3    : FCurrentNote := 'F#';
  -2    : FCurrentNote := 'G';
  -1    : FCurrentNote := 'G#';
   0    : FCurrentNote := 'A';
   1    : FCurrentNote := 'Bb';
   2    : FCurrentNote := 'B';
   3    : FCurrentNote := 'C';
   4    : FCurrentNote := 'C#';
   5    : FCurrentNote := 'D';
 end;

 FCurrentDetune := 100 * CurrentNote - round(CurrentNote);

 FCurrentFreq := 440 * FastPower2ContinousError3(CurrentNote * COneTwelfth32);
end;

procedure TCustomAdvancedTuner.CalculateDownsampleFactor;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomAdvancedTuner.SetAttack(const Value: Single);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomAdvancedTuner.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomAdvancedTuner.SetThreshold(const Value: Single);
begin
 FThreshold := Limit(Value, -1, 1);
end;

procedure TCustomAdvancedTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAdvancedTuner then
  with TCustomAdvancedTuner(Dest) do
   begin
    inherited;
    FCurrentNote   := Self.FCurrentNote;
    FCurrentDetune := Self.FCurrentDetune;
    FAttack        := Self.FAttack;
    FAttackFactor  := Self.FAttackFactor;
    FRelease       := Self.FRelease;
    FReleaseFactor := Self.FReleaseFactor;
    FLevel         := Self.FLevel;
    FThreshold     := Self.FThreshold;
   end
 else inherited;
end;

procedure TCustomAdvancedTuner.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomAdvancedTuner.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomAdvancedTuner.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate / FDownSampleFactor));
end;

procedure TCustomAdvancedTuner.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate / FDownSampleFactor));
end;

constructor TCustomAdvancedTuner.Create;
begin
 inherited;
 FAttack := 1;
 FRelease := 10;
 FThreshold := 0;
end;

procedure TCustomAdvancedTuner.ProcessDownsampled(Downsampled: Single);
var
  Offset : Single;
begin
 if abs(Downsampled) > FLevel
  then FLevel := FLevel + (abs(Downsampled) - FLevel) * FAttackFactor
  else FLevel := abs(Downsampled) + (FLevel - abs(Downsampled)) * FReleaseFactor;

 if (Downsampled < FThreshold * FLevel) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
 FLastSample := Downsampled;
end;

{ TCustomCepstrumTuner }

constructor TCustomCepstrumTuner.Create;
begin
 inherited;
 FCepstrumCalculation := TPowerCepstrum32.Create;
 FBufferLength := 0;
 CalculateBufferLength;
end;

destructor TCustomCepstrumTuner.Destroy;
begin
 FreeAndNil(FCepstrumCalculation);
 inherited;
end;

procedure TCustomCepstrumTuner.SampleRateChanged;
begin
 inherited;
 CalculateBufferLength;
end;

procedure TCustomCepstrumTuner.CalculateDownsampleFactor;
begin
 inherited;
 CalculateBufferLength;
end;

procedure TCustomCepstrumTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCepstrumTuner then
  with TCustomCepstrumTuner(Dest) do
   begin
    inherited;
    FBuffer              := Self.FBuffer;
    FCepstrum            := Self.FCepstrum;
    FCepstrumCalculation := Self.FCepstrumCalculation;
    FBufferLength        := Self.FBufferLength;
    FBufferPos           := Self.FBufferPos;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq         := Self.FCurrentFreq;
    {$ENDIF}
   end
 else inherited;
end;

procedure TCustomCepstrumTuner.CalculateBufferLength;
var
  NewBufferLength : Integer;
begin
 // buffer of about 25ms
 NewBufferLength := Round(0.025 * SampleRate / FDownSampleFactor);

 // round to nearest power of 2
 NewBufferLength := RoundToPowerOf2(NewBufferLength);

 if NewBufferLength <> FBufferLength then
  begin
   FBufferLength := NewBufferLength;

   // set cepstrum length
   FCepstrumCalculation.FFTOrder := round(Log2(NewBufferLength));

   // reallocate buffer memory
   ReallocMem(FBuffer, NewBufferLength * SizeOf(Single));
   FillChar(FBuffer^, NewBufferLength * SizeOf(Single), 0);

   // reset buffer position
   FBufferPos := 0;
  end;
end;

function TCustomCepstrumTuner.CalculateCurrentFrequency: Single;
var
  i   : Integer;
  max : Single;
  mps : Integer;
begin
 mps := 0;
 max := FCepstrum[0].Re;
 for i := 1 to FBufferLength - 1 do
  if FCepstrum[i].Re > max then
   begin
    max := FCepstrum[i].Re;
    mps := i;
   end;

 Result := SampleRate / (FDownSampleFactor * mps);
end;

function TCustomCepstrumTuner.GetCurrentFrequency: Single;
begin
 {$IFDEF OnlineFreqCalc}
 result := FCurrentFreq;
 {$ELSE}
 result := CalculateCurrentFrequency;
 {$ENDIF}
end;

procedure TCustomCepstrumTuner.ProcessDownsampled(Downsampled: Single);
begin
 FBuffer[FBufferPos] := DownSampled;
 inc(FBufferPos);
 if FBufferPos = FBufferLength then
  begin
   FBufferPos := 0;
   ApplyHammingWindow(FBuffer, FBufferLength);
   FCepstrumCalculation.CalculateCepstrum(FBuffer, FCepstrum);
  end;
end;

end.
