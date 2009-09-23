unit DAV_DspFrequencyShifter;

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_Classes, DAV_DspLfo, DAV_DspPolyphaseHilbert;

type
  TCustomBodeFrequencyShifter = class(TDspSampleRatePersistent)
  private
    FFrequency           : Single;
    FCoefficientCount    : Integer;
    FTransitionBandwidth : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetCoefficientCount(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Single);
  protected
    procedure CoefficientCountChanged; virtual; abstract;
    procedure FrequencyChanged; virtual; abstract;
    procedure TransitionBandwidthChanged; virtual; abstract;
  public
    constructor Create; override;
    property Frequency: Single read FFrequency write SetFrequency;
    property CoefficientCount: Integer read FCoefficientCount write SetCoefficientCount;
    property TransitionBandwidth: Single read FTransitionBandwidth write SetTransitionBandwidth;
  end;

  TCustomBodeFrequencyShifter32 = class(TCustomBodeFrequencyShifter)
  private
    FLfo     : TLFOSine32;
    FHilbert : TPhaseHalfPi32;
  protected
    procedure CoefficientCountChanged; override;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
    procedure TransitionBandwidthChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSample(Input: Single; var Upshift, Downshift: Single); virtual;
  end;

  TBodeFrequencyShifter32 = class(TCustomBodeFrequencyShifter32)
  published
    property SampleRate;
    property Frequency;
  end;

implementation

uses
  SysUtils;

{ TCustomBodeFrequencyShifter }

constructor TCustomBodeFrequencyShifter.Create;
begin
 inherited;
 FFrequency := 1000;
end;

procedure TCustomBodeFrequencyShifter.SetCoefficientCount(const Value: Integer);
begin
 if FCoefficientCount <> Value then
  begin
   FCoefficientCount := Value;
   CoefficientCountChanged;
  end;
end;

procedure TCustomBodeFrequencyShifter.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomBodeFrequencyShifter.SetTransitionBandwidth(
  const Value: Single);
begin
 if FTransitionBandwidth <> Value then
  begin
   FTransitionBandwidth := Value;
   TransitionBandwidthChanged;
  end;
end;


{ TCustomBodeFrequencyShifter32 }

constructor TCustomBodeFrequencyShifter32.Create;
begin
 inherited;
 FLfo := TLFOSine32.Create;
 with FLfo do
  begin
   SampleRate := Self.SampleRate;
   Frequency := FFrequency;
  end;
 FHilbert := TPhaseHalfPi32.Create;
 FHilbert.SetCoefficients(8, 0.1);
end;

destructor TCustomBodeFrequencyShifter32.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TCustomBodeFrequencyShifter32.CoefficientCountChanged;
begin
 assert(FCoefficientCount >= 1);
 assert(FCoefficientCount <= 32);
 FHilbert.NumberOfCoefficients := FCoefficientCount;
end;

procedure TCustomBodeFrequencyShifter32.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
end;

procedure TCustomBodeFrequencyShifter32.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
end;

procedure TCustomBodeFrequencyShifter32.TransitionBandwidthChanged;
begin
 FHilbert.Transition := FTransitionBandwidth;
end;

procedure TCustomBodeFrequencyShifter32.ProcessSample(Input: Single; var Upshift, Downshift: Single);
var
  Cmplx : TComplexSingle;
const
  CSqrtHalf32 : Single = 0.70710678118654752440084436210485;
begin
 FHilbert.ProcessHilbertSample(Input, Cmplx.Re, Cmplx.Im);
 Cmplx.Im := FLfo.Sine * Cmplx.Im;
 Cmplx.Re := FLfo.Cosine * Cmplx.Re;
 FLfo.CalculateNextSample;
 Upshift   := (Cmplx.Re - Cmplx.Im) * CSqrtHalf32;
 Downshift := (Cmplx.Re + Cmplx.Im) * CSqrtHalf32;
end;

end.
