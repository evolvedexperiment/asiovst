unit DAV_DspSpectralNoiseReduction;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.INC}

uses
  Classes, DAV_Types, DAV_Classes, DAV_Complex, DAV_DspSpectralEffects,
  DAV_DspDynamics;

// TODO: check and implement all assignto functions!!!

type
  TSpectralNoiseCut32 = class(TCustomSpectralEffect32)
  private
    FThresholdFactor : Single;
    FThreshold       : Double;
    FSaveBuffer      : PDAVSingleFixedArray;
    procedure SetThreshold(const Value: Double);
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray); overload; override;
    procedure ThresholdChanged; virtual;
    procedure FFTOrderChanged; override;
  public
    constructor Create; override;
  published
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
  end;

  TSpectralNoiseCut64 = class(TCustomSpectralEffect64)
  private
    FThresholdFactor : Double;
    FThreshold       : Double;
    FSaveBuffer      : PDAVDoubleFixedArray;
    procedure SetThreshold(const Value: Double);
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVDoubleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectum: PDAVComplexDoubleFixedArray); overload; override;
    procedure ThresholdChanged; virtual;
    procedure FFTOrderChanged; override;
  public
    constructor Create; override;
  published
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
  end;

  TSpectralNoiseGate32 = class(TCustomSpectralEffect32)
  private
    FGates      : array of TCustomClassicGate;
    FThreshold  : Double;
    FSaveBuffer : PDAVSingleFixedArray;
    FRelease: Double;
    FAttack: Double;
    procedure SetThreshold(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray); overload; override;

    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure ThresholdChanged; virtual;
  published
    property Attack: Double read FAttack write SetAttack;
    property Release: Double read FRelease write SetRelease;
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
  end;

  TSpectralNoiseGate64 = class(TCustomSpectralEffect64)
  private
    FGates      : array of TCustomClassicGate;
    FThreshold  : Double;
    FSaveBuffer : PDAVDoubleFixedArray;
    procedure SetThreshold(const Value: Double);
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVDoubleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectum: PDAVComplexDoubleFixedArray); overload; override;
    procedure ThresholdChanged; virtual;
    procedure FFTOrderChanged; override;
  published
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
  end;

implementation

uses
  SysUtils, DAV_Common;

{ TSpectralNoiseCut32 }

constructor TSpectralNoiseCut32.Create;
begin
 inherited;
 FThresholdFactor := 1E-3;
end;

procedure TSpectralNoiseCut32.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FSaveBuffer, FFFTSizeHalf * SizeOf(Single));
end;

procedure TSpectralNoiseCut32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);

 Reci := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   Scale := sqr(Sample * Reci);
   SignalOut^[Sample] := Scale * SignalOut^[Sample] +
     (1 - Scale) * FSaveBuffer^[Sample];
  end;

 Move(SignalOut^[FFFTSizeHalf], FSaveBuffer^, FFFTSizeHalf * SizeOf(Single));
end;

procedure TSpectralNoiseCut32.PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 Half := FFTSize div 2;

 // DC & Nyquist
 if abs(Spectum^[0].Re) < FThresholdFactor
  then Spectum^[0].Re := 0;

 if abs(Spectum^[Half].Re) < FThresholdFactor
  then Spectum^[Half].Re := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  if (Sqr(Spectum^[Bin].Re) + Sqr(Spectum^[Bin].Im)) < Sqr(FThresholdFactor) then
   begin
    Spectum^[Bin].Re := 0;
    Spectum^[Bin].Im := 0;
   end;
end;

procedure TSpectralNoiseCut32.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseCut32.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;


{ TSpectralNoiseCut64 }

constructor TSpectralNoiseCut64.Create;
begin
 inherited;
 FThresholdFactor := 1E-3;
end;

procedure TSpectralNoiseCut64.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FSaveBuffer, FFFTSizeHalf * SizeOf(Double));
end;

procedure TSpectralNoiseCut64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Sample : Integer;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);

 Scale := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   SignalOut^[Sample] := (Sample * Scale) * SignalOut^[Sample] +
     (1 - (Sample * Scale)) * FSaveBuffer^[Sample];
  end;

 Move(SignalOut^[FFFTSizeHalf], FSaveBuffer^, FFFTSizeHalf * SizeOf(Double));
end;

procedure TSpectralNoiseCut64.PerformSpectralEffect(Spectum: PDAVComplexDoubleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 Half := FFTSize div 2;

 // DC & Nyquist
 if abs(Spectum^[0].Re) < FThresholdFactor
  then Spectum^[0].Re := 0;

 if abs(Spectum^[Half].Re) < FThresholdFactor
  then Spectum^[Half].Re := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  if (Sqr(Spectum^[Bin].Re) + Sqr(Spectum^[Bin].Im)) < Sqr(FThresholdFactor) then
   begin
    Spectum^[Bin].Re := 0;
    Spectum^[Bin].Im := 0;
   end;
end;

procedure TSpectralNoiseCut64.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseCut64.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;


{ TSpectralNoiseGate32 }

procedure TSpectralNoiseGate32.FFTOrderChanged;
var
  Bin : Integer;
begin
 inherited;
 ReallocMem(FSaveBuffer, FFFTSizeHalf * SizeOf(Single));

 // dispose unused gates
 for Bin := Fft.BinCount to Length(FGates) - 1 do
  if assigned(FGates[Bin]) then FreeAndNil(FGates[Bin]);

 SetLength(FGates, Fft.BinCount);

 for Bin := 0 to Length(FGates) - 1 do
  if not Assigned(FGates[Bin]) then
   begin
    FGates[Bin] := TClassicGate.Create;
    with FGates[Bin] do
     begin
      SampleRate := Self.SampleRate * 2 / FFft.FFTSize;
      Attack := 1;
      Release := 1;
     end;
   end;
end;

procedure TSpectralNoiseGate32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);

 Reci := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   Scale := sqr(Sample * Reci);
   SignalOut^[Sample] := Scale * SignalOut^[Sample] +
     (1 - Scale) * FSaveBuffer^[Sample];
  end;

 Move(SignalOut^[FFFTSizeHalf], FSaveBuffer^, FFFTSizeHalf * SizeOf(Single));
end;

procedure TSpectralNoiseGate32.PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 Half := FFTSize div 2;

 // DC bin
 FGates[0].InputSample(Sqr(Spectum^[0].Re));
 Spectum^[0].Re := FGates[0].GainSample(Spectum^[0].Re);

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FGates[Bin].InputSample(Sqr(Spectum^[Bin].Re) + Sqr(Spectum^[Bin].Im));
   Spectum^[Bin].Re := FGates[Bin].GainSample(Spectum^[Bin].Re);
   Spectum^[Bin].Im := FGates[Bin].GainSample(Spectum^[Bin].Im);
  end;

 // Nyquist bin
 FGates[Half].InputSample(Sqr(Spectum^[Half].Re));
 Spectum^[Half].Re := FGates[Half].GainSample(Spectum^[Half].Re);
end;

procedure TSpectralNoiseGate32.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseGate32.ThresholdChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Threshold_dB := 2 * FThreshold;
end;

procedure TSpectralNoiseGate32.AttackChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Attack := FAttack;
end;

procedure TSpectralNoiseGate32.ReleaseChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Release := FRelease;
end;

{ TSpectralNoiseGate64 }

procedure TSpectralNoiseGate64.FFTOrderChanged;
var
  Bin : Integer;
begin
 inherited;
 ReallocMem(FSaveBuffer, FFFTSizeHalf * SizeOf(Double));

 // dispose unused gates
 for Bin := Fft.BinCount to Length(FGates) - 1 do
  if assigned(FGates[Bin]) then FreeAndNil(FGates[Bin]);

 SetLength(FGates, Fft.BinCount);

 for Bin := 0 to Length(FGates) - 1 do
  if not assigned(FGates[Bin]) then
   begin
    FGates[Bin] := TClassicGate.Create;
    with FGates[Bin] do
     begin
      SampleRate := Self.SampleRate * 2 / FFft.FFTSize;
      Attack := 1;
      Release := 1;
     end;
   end;
end;

procedure TSpectralNoiseGate64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Sample : Integer;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);

 Scale := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   SignalOut^[Sample] := (Sample * Scale) * SignalOut^[Sample] +
     (1 - (Sample * Scale)) * FSaveBuffer^[Sample];
  end;

 Move(SignalOut^[FFFTSizeHalf], FSaveBuffer^, FFFTSizeHalf * SizeOf(Double));
end;

procedure TSpectralNoiseGate64.PerformSpectralEffect(Spectum: PDAVComplexDoubleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 Half := FFTSize div 2;

 // DC bin
 FGates[0].InputSample(Sqr(Spectum^[0].Re));
 Spectum^[0].Re := FGates[0].GainSample(Spectum^[0].Re);

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FGates[Bin].InputSample(Sqr(Spectum^[Bin].Re) + Sqr(Spectum^[Bin].Im));
   Spectum^[Bin].Re := FGates[Bin].GainSample(Spectum^[Bin].Re);
   Spectum^[Bin].Im := FGates[Bin].GainSample(Spectum^[Bin].Im);
  end;

 // Nyquist bin
 FGates[Half].InputSample(Sqr(Spectum^[Half].Re));
 Spectum^[Half].Re := FGates[Half].GainSample(Spectum^[Half].Re);
end;

procedure TSpectralNoiseGate64.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseGate64.ThresholdChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Threshold_dB := 2 * FThreshold;
end;

end.
