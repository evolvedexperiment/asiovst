unit DAV_DspTransformerSimulation;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspFilter,
  DAV_DspFilterButterworth, DAV_DspWaveshaper, DAV_DspPolyphaseUpsampler,
  DAV_DspPolyphaseDownsampler;

type
  TCustomTransformatorSimulation = class(TDspSampleRatePersistent,
    IDspProcessor32, IDspProcessor64)
  private
    FLowpassState : Double;
    FLpCoeffs     : array [0..1] of Double;
    FHpFreq       : Single;
    FInputGain    : Single;
    FHighpass     : TButterworthHighPassFilter;
    FWaveshaper   : TChebyshevWaveshaper;
    FUpsampler    : TPolyphaseUpsampler64;
    FDownsampler  : TPolyphaseDownsampler64;
    procedure SetHPFreq(const Value: Single);
    procedure CalculateLowpassCoefficients;
  protected
    procedure SampleRateChanged; override;
    procedure HighpassFrequencyChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);

    property HighpassFrequency: Single read FHpFreq write SetHPFreq;
  end;

  TTransformatorSimulation = class(TCustomTransformatorSimulation)
  published
    property HighpassFrequency;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_DspPolyphaseFilter;

{ TCustomTransformatorSimulation }

constructor TCustomTransformatorSimulation.Create;
begin
 inherited;

 // create highpass
 FHighpass := TButterworthHighPassFilter.Create(1);
 with FHighpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := 10;
  end;

 // create waveshaper
 FWaveshaper := TChebyshevWaveshaper.Create;
 with FWaveshaper do
  begin
   Order := 3;
   Gain[0] := 1;
   Gain[1] := 1E-2;
   Gain[2] := 1E-4;
   Inverted[1] := False;
  end;

 // create upsampler
 FUpsampler := TPolyphaseUpsampler64.Create;
 with FUpsampler do
  begin
   SampleRate := Self.SampleRate;
   NumberOfCoefficients := 6;
   Transition := 0.49;
  end;

 // create downsampler
 FDownsampler := TPolyphaseDownsampler64.Create;
 with FDownsampler do
  begin
   SampleRate := Self.SampleRate;
   NumberOfCoefficients := 6;
   Transition := 0.24;
  end;

 FInputGain := 1 / FWaveshaper.ProcessSample64(1);

 CalculateLowpassCoefficients; 
end;

destructor TCustomTransformatorSimulation.Destroy;
begin
 FreeAndNil(FHighpass);
 FreeAndNil(FWaveshaper);
 FreeAndNil(FUpsampler);
 FreeAndNil(FDownsampler);
 inherited;
end;

procedure TCustomTransformatorSimulation.SetHPFreq(const Value: Single);
begin
 if FHpFreq <> Value then
  begin
   FHpFreq := Value;
   HighpassFrequencyChanged;
  end;
end;

procedure TCustomTransformatorSimulation.HighpassFrequencyChanged;
begin
 if Assigned(FHighpass)
  then FHighpass.Frequency := FHpFreq;
end;

procedure TCustomTransformatorSimulation.SampleRateChanged;
begin
 inherited;
 FHighpass.SampleRate := SampleRate;
 CalculateLowpassCoefficients;
end;

procedure TCustomTransformatorSimulation.CalculateLowpassCoefficients;
begin
 FLpCoeffs[0] := 0.03;
 FLpCoeffs[1] := 0.97;
end;

procedure TCustomTransformatorSimulation.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32(Data[SampleIndex]);
end;

procedure TCustomTransformatorSimulation.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample64(Data[SampleIndex]);
end;

function TCustomTransformatorSimulation.ProcessSample32(Input: Single): Single;
var
  Data : TDAV2DoubleArray;
begin
 Result := FLpCoeffs[0] * FLowpassState + FLpCoeffs[1] * FHighpass.ProcessSample32(Input);
 FLowpassState := Result;
 FUpsampler.ProcessSample64(FInputGain * Result, Data);
 Data[0] := FWaveshaper.ProcessSample64(Data[0]);
 Data[1] := FWaveshaper.ProcessSample64(Data[1]);
 Result := FDownsampler.ProcessSample64(Data)
end;

function TCustomTransformatorSimulation.ProcessSample64(Input: Double): Double;
var
  Data : TDAV2DoubleArray;
begin
 Result := FLpCoeffs[0] * FLowpassState + FLpCoeffs[1] * FHighpass.ProcessSample64(Input);
 FLowpassState := Result;
 FUpsampler.ProcessSample64(FInputGain * Result, Data);
 Data[0] := FWaveshaper.ProcessSample64(Data[0]);
 Data[1] := FWaveshaper.ProcessSample64(Data[1]);
 Result := FDownsampler.ProcessSample64(Data);
end;

end.
