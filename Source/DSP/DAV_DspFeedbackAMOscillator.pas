unit DAV_DspFeedbackAMOscillator;

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
  DAV_Common, DAV_DspCommon, DAV_DspLFO, DAV_DspFilter, DAV_DspDelayLines;

type
  TCustomFeedbackAMOscillator = class(TDspSampleRatePersistent)
  private
    FAmplitude  : Single;
    FFrequency  : Single;
    FFeedback   : Single;
    FState      : Single;
    procedure SetAmplitude(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetFeedback(const Value: Single);
    procedure FeedbackChanged;
  protected
    FOscillator : TLFOSine32;
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Amplitude: Single read FAmplitude write SetAmplitude;
    property Frequency: Single read FFrequency write SetFrequency;
    property Feedback: Single read FFeedback write SetFeedback;
  end;

  TFeedbackAMOscillator = class(TCustomFeedbackAMOscillator)
  public
    constructor Create; override;
    function ProcessSample: Single;
  published
    property Amplitude;
    property Frequency;
    property Feedback;
    property SampleRate;
  end;

  TDelayedFeedbackAMOscillator = class(TCustomFeedbackAMOscillator)
  private
    FDelayLine : TCustomDelayLineSamples32;
  public
    constructor Create; override;
    function ProcessSample: Single;
  published
    property Amplitude;
    property Frequency;
    property Feedback;
    property SampleRate;
  end;

implementation

uses
  SysUtils;

{ TCustomFeedbackAMOscillator }

constructor TCustomFeedbackAMOscillator.Create;
begin
 inherited;
 FOscillator := TLFOSine32.Create;
 with FOscillator do
  begin
   Amplitude := 0.5;
   SampleRate := Self.SampleRate;
  end;
 FFeedback  := 1;
 FAmplitude := 1;
end;

destructor TCustomFeedbackAMOscillator.Destroy;
begin
 FreeAndNil(FOscillator);
 inherited;
end;

procedure TCustomFeedbackAMOscillator.SetAmplitude(const Value: Single);
begin
 if FAmplitude <> Value then
  begin
   FAmplitude := Value;
   AmplitudeChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.SetFeedback(const Value: Single);
begin
 if FFeedback <> Value then
  begin
   FFeedback := Value;
   FeedbackChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.AmplitudeChanged;
begin
// nothing here yet!
end;

procedure TCustomFeedbackAMOscillator.FeedbackChanged;
begin
// nothing here yet!
end;

procedure TCustomFeedbackAMOscillator.FrequencyChanged;
begin
 FOscillator.Frequency := Frequency;
end;

procedure TCustomFeedbackAMOscillator.SampleRateChanged;
begin
 FOscillator.SampleRate := SampleRate;
end;


{ TFeedbackAMOscillator }

constructor TFeedbackAMOscillator.Create;
begin
 inherited;
 FState := 0;
end;

function TFeedbackAMOscillator.ProcessSample: Single;
begin
(*
 Result := FState * FOscillator.Sine;
 FState := CHalf32 + FFeedback * Result;
 FOscillator.CalculateNextSample;
 Result := FScale * Result;
*)
 Result := FOscillator.Sine * (1 + FFeedback * FState);
 FState := Result;
 FOscillator.CalculateNextSample;
 Result := FAmplitude * Result;
end;

{ TDelayedFeedbackAMOscillator }

constructor TDelayedFeedbackAMOscillator.Create;
begin
 inherited;
 FDelayLine := TCustomDelayLineSamples32.Create(128);
end;

function TDelayedFeedbackAMOscillator.ProcessSample: Single;
begin
 Result := FOscillator.Sine * (1 + FFeedback * FState);
 FState := FDelayLine.ProcessSample(Result);
 FOscillator.CalculateNextSample;
 Result := FAmplitude * Result;
end;

end.