unit XSynthVoice;

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

{$I DAV_Compiler.inc}

uses
  DAV_VSTModule, DAV_Complex;

{$i Consts.inc}

type
  TOscilatorType = (otNone, otSine, otHalfSine, otSquare, otNoise);
  TADSRStage = (adsrAttack, adsrDecay, adsrSustain, adsrRelease);
  TOsc = record
           OType   : TOscilatorType;
           Attack  : Single;
           Decay   : Single;
           Release : Single;
           Sustain : Single;
           Level   : Single;
  end;

  TOscilator = class(TObject)
  private
    FSampleRate : Single;
    FSampleReci : Single;
    FFrequency  : Single;
    FAmplitude  : Single;
    FAttack,
    FDecay,
    FSustain,
    FRelease    : Single;
    FADSRStage  : TADSRStage;
    FADSRGain   : Single;
    FLevel      : Single;
    FReleased   : Boolean;
    procedure SetAmplitude(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
    procedure SetOsc(Osc : TOsc); virtual;
    procedure SetLevel(const Value: Single);
  protected
    function ProcessADSR: Single; virtual;
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const SampleRate: Single); overload; virtual;
    destructor Destroy; override;
    function Process: Single; virtual;
    procedure ReleaseOsc; virtual;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Level: Single read FLevel write SetLevel;
    property Amplitude: Single read FAmplitude write SetAmplitude;
    property Attack: Single read FAttack write FAttack;
    property Decay: Single read FDecay write FDecay;
    property Sustain: Single read FSustain write FSustain;
    property Release: Single read FRelease write FRelease;
  end;

  TSineOscilator = class(TOscilator)
  private
    FAngle,
    FPosition   : TComplexDouble;
  protected
    procedure FrequencyChanged; override;
  public
    constructor Create; override;
    function Process: Single; override;
  end;

  TSquareOscilator = class(TOscilator)
  private
    FAngle,
    FPosition   : TComplexDouble;
  protected
    procedure FrequencyChanged; override;
  public
    constructor Create; override;
    function Process: Single; override;
  end;

  THalfSineOscilator = class(TSineOscilator)
  private
  public
    function Process: Single; override;
  end;

  TNoiseOscilator = class(TOscilator)
  private
  public
    constructor Create; override;
    function Process: Single; override;
  end;

  TXSynthVoice = class(TObject)
  private
    FMidiKeyNr  : Integer;
    FVelocity   : Integer;
    FSampleRate : Single;
    FSampleReci : Single;
    FFrequency  : Single;
    FAmplitude  : Single;
    FReleased   : Boolean;
    FOscilators : Array[0..1] of TOscilator;
    FVSTModule  : TVSTModule;
    procedure SetSampleRate(const Value: Single); virtual;
  protected
    procedure SampleRateChanged; virtual;
  public
    constructor Create(theModule: TVSTModule);
    destructor Destroy; override;
    procedure SetFrequency(Frequency: Single); virtual;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Released: Boolean read FReleased;
    property MidiKeyNr: Integer read FMidiKeyNr write FMidiKeyNr;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Types, DAV_Math, XSynthModule;

{ TOscilator }

constructor TOscilator.Create;
begin
 FSampleRate := 44100;
 FFrequency  := 1000;
 FAmplitude  := 1;
 FADSRStage  := adsrAttack;
 FReleased   := False;
end;

constructor TOscilator.Create(const SampleRate: Single);
begin
 inherited Create;
 Self.SampleRate := SampleRate;
 SampleRateChanged;
end;

destructor TOscilator.Destroy;
begin
 inherited;
end;

procedure TOscilator.AmplitudeChanged;
begin
 // nothing to do yet
end;

procedure TOscilator.FrequencyChanged;
begin
 // nothing to do yet
end;

function TOscilator.ProcessADSR: Single;
begin
 case FADSRStage of
  adsrAttack  : begin
                 FADSRGain := FADSRGain + FAttack*(1 - FADSRGain);
                 if FADSRGain > 0.999
                  then FADSRStage := adsrDecay;
                end;
  adsrDecay   : begin
                 FADSRGain := FADSRGain - FDecay * (FADSRGain - FSustain);
                 if FADSRGain < 1.001 * FSustain
                  then FADSRStage := adsrSustain;
                end;
  adsrSustain : if FReleased then FADSRStage := adsrRelease;
  adsrRelease : begin
                 FADSRGain := FADSRGain - FRelease * FADSRGain;
                 if FADSRGain < 0.001
                  then FADSRGain := 0;
                end;
 end;
 Result := FADSRGain;
end;

procedure TOscilator.ReleaseOsc;
begin
 FReleased := True;
 FADSRStage := adsrRelease;
end;

function TOscilator.Process: Single;
begin
 result := 0;
end;

procedure TOscilator.SampleRateChanged;
begin
 FSampleReci := 1 / FSampleRate;
end;

procedure TOscilator.SetAmplitude(const Value: Single);
begin
 if FAmplitude <> Value then
  begin
   FAmplitude := Value * FLevel;
   AmplitudeChanged;
  end;
end;

procedure TOscilator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TOscilator.SetLevel(const Value: Single);
begin
 if FLevel <> Value then
  begin
   FLevel := Value;
  end;
end;

procedure TOscilator.SetOsc(Osc: TOsc);
begin
 Attack  := Power(10, -8 * Osc.Attack);
 Decay   := Power(10, -8 * Osc.Decay);
 Release := Power(10, -8 * Osc.Release);
 Sustain := Osc.Sustain;
 Level   := Osc.Level;
end;

procedure TOscilator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TSineOscilator }

constructor TSineOscilator.Create;
begin
 inherited;
 FPosition.Re := 0;
 FPosition.Im := -1;
 FADSRGain := 0;
end;

procedure TSineOscilator.FrequencyChanged;
begin
 inherited;
 GetSinCos(2 * Pi * FFrequency * FSampleReci, FAngle.Im, FAngle.Re);
end;

function TSineOscilator.Process: Single;
begin
 result := FPosition.Re*FAngle.Re-FPosition.Im*FAngle.Im;
 FPosition.Im := FPosition.Im*FAngle.Re+FPosition.Re*FAngle.Im;
 FPosition.Re := result; result := result * FAmplitude * ProcessADSR;
end;

{ THalfSineOscilator }

function THalfSineOscilator.Process: Single;
begin
 Result := abs(inherited Process);
end;

{ TSquareOscilator }

constructor TSquareOscilator.Create;
begin
 inherited;
 FPosition.Re :=  0;
 FPosition.Im := -1;
end;

procedure TSquareOscilator.FrequencyChanged;
begin
 inherited;
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

function TSquareOscilator.Process: Single;
begin
 result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
 FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
 FPosition.Re := result;
 if Result > 0
  then Result :=  FAmplitude * ProcessADSR
  else Result := -FAmplitude * ProcessADSR;
end;

{ TNoiseOscilator }

constructor TNoiseOscilator.Create;
begin
 inherited;
 Randomize;
end;

function TNoiseOscilator.Process: Single;
begin
 result := (2 * random - 1) * FAmplitude * ProcessADSR;
end;

{ TXSynthVoice }

constructor TXSynthVoice.Create(theModule: TVSTModule);
var i : Integer;
begin
 FVSTModule := theModule;
 FReleased := False;
 if theModule.SampleRate=0
  then SampleRate := 44100
  else SampleRate := theModule.SampleRate;
 for i := 0 to 1 do
  with (FVSTModule as TVSTSSModule) do
   begin
    case Oscilators[i].OType of
     otNone     : FOscilators[i] := TOscilator.Create;
     otSine     : FOscilators[i] := TSineOscilator.Create;
     otHalfSine : FOscilators[i] := THalfSineOscilator.Create;
     otSquare   : FOscilators[i] := TSquareOscilator.Create;
     otNoise    : FOscilators[i] := TNoiseOscilator.Create;
    end;
    FOscilators[i].SampleRate := SampleRate;
    FOscilators[i].SetOsc(Oscilators[i]);
   end;
end;

destructor TXSynthVoice.Destroy;
begin
 FOscilators[0].Free;
 FOscilators[1].Free;
 inherited;
end;

procedure TXSynthVoice.SetSampleRate(const Value: Single);
begin
 if Value <= 0
  then raise Exception.Create('Samplerate must be above 0!');
 if Value <> FSampleRate then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TXSynthVoice.SampleRateChanged;
begin
 FSampleReci := 1 / FSampleRate;
end;

function TXSynthVoice.Process: Single;
var
  i : Integer;
begin
 result := FOscilators[0].Process + FOscilators[1].Process;
 if (FOscilators[0].FADSRGain = 0) and
    (FOscilators[1].FADSRGain = 0) then
  with (FVSTModule as TVSTSSModule) do
   for i := 0 to Voices.Count - 1 do
    if Voices.Items[i] = Self then
     begin
      Voices.Delete(i);
      Exit;
     end;
end;

procedure TXSynthVoice.SetFrequency(Frequency: Single);
begin
 FFrequency := Frequency;
 FOscilators[0].Frequency := FFrequency;
 FOscilators[1].Frequency := FFrequency;
end;

procedure TXSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
 FFrequency := Frequency;
 SetFrequency(Frequency);
 FAmplitude := Amplitude;

 FOscilators[0].Frequency := Frequency;
 FOscilators[1].Frequency := Frequency;
 FOscilators[0].Amplitude := FAmplitude;
 FOscilators[1].Amplitude := FAmplitude;
end;

procedure TXSynthVoice.NoteOff;
begin
 FOscilators[0].ReleaseOsc;
 FOscilators[1].ReleaseOsc;
 FReleased := True;
end;

end.
