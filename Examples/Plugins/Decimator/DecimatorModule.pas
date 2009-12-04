unit DecimatorModule;

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
  Windows, Forms, DAV_Types, DAV_VSTEffect, DAV_VSTModule;

type
  TDecimatorFilterType = (dftLowpass, dftHighpass);
  TVSTDecimator = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParameterSampleRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBitsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCutoffChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterResonanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetDryMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FCutoffFreqNorm  : Double;
    FResonance       : Double;
    FFC              : Double;
    FSHrate          : Double;
    FYL, FYR         : Double;
    FOutVol, FMixWet : Double;
    FState           : array [0..1, 0..1] of Double;
    FSHCounter       : Double;
    FBitDepth        : Double;
    FBitMul, FBitDiv : Double;
    FFilterType      : TDecimatorFilterType;
  public
    property CutoffNormalizedFrequency: Double read FCutoffFreqNorm;
    property BitDepth: Double read FBitDepth;
    property Resonance: Double read FResonance;
    property SampleHoldRate: Double read FSHrate;
    property WetMix: Double read FMixWet;
    property OutputVolume: Double read FOutVol;
    property FilterType: TDecimatorFilterType read FFilterType;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Common, DecimatorGUI;

////////////////////////////////////////////////////////////////////////////////
// Open / Close
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleOpen(Sender: TObject);
begin
 // default parameters
 Parameter[0] := 44100;
 Parameter[1] := 24;
 Parameter[2] := 1000;
 Parameter[3] := 1;
 Parameter[4] := 1;
 Parameter[5] := 100;
 Parameter[6] := 0;

 // default preset
 with Programs[0] do
  begin
   Parameter[0] := 44100;
   Parameter[1] := 24;
   Parameter[2] := 1000;
   Parameter[3] := 1;
   Parameter[4] := 1;
   Parameter[5] := 100;
   Parameter[6] := 0;
  end;

 // preset 1
 with Programs[1] do
  begin
   Parameter[0] := 20000;
   Parameter[1] := 16;
   Parameter[2] := 2000;
   Parameter[3] := 1;
   Parameter[4] := 1;
   Parameter[5] := 80;
   Parameter[6] := 0;
  end;

 // preset 2
 with Programs[2] do
  begin
   Parameter[0] := 44100;
   Parameter[1] := 24;
   Parameter[2] := 400;
   Parameter[3] := 4;
   Parameter[4] := 1;
   Parameter[5] := 100;
   Parameter[6] := 0;
  end;
end;

procedure TVSTDecimator.ParameterFilterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case FFilterType of
  dftLowpass  : PreDefined := 'Lowpass';
  dftHighpass : PreDefined := 'Highpass';
 end;
end;

procedure TVSTDecimator.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;


////////////////////////////////////////////////////////////////////////////////
// Parameter Changed
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.ParameterCutoffChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCutoffFreqNorm := Limit(0.5 * FreqLogToLinear(Value), 0.01, 1);
 FFC := FResonance * (1 + 1 / (1 - FCutoffFreqNorm));

 // update GUI
 if EditorForm is TVSTGUI then
  with TVSTGUI(EditorForm)
   do UpdateFrequency;
end;

procedure TVSTDecimator.ParameterResonanceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FResonance := Value * 0.1;
 if FResonance > 0.8 then FResonance := 0.8;
 FFC := FResonance * (1 + 1 / (1 - FCutoffFreqNorm));

 // update GUI
 if EditorForm is TVSTGUI then
  with TVSTGUI(EditorForm)
   do UpdateResonance;
end;

procedure TVSTDecimator.ParameterWetDryMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMixWet := Value * 0.01;

 // update GUI
 if EditorForm is TVSTGUI then
  with TVSTGUI(EditorForm)
   do UpdateMix;
end;

procedure TVSTDecimator.ParameterOutputVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutVol := dB_to_Amp(Value);

 // update GUI
 if EditorForm is TVSTGUI then
  with TVSTGUI(EditorForm)
   do UpdateOutput;
end;

procedure TVSTDecimator.ParameterSampleRateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
const
  CMul = 200/441;
begin
 FSHrate := FreqLogToLinear(Value * cMul);

 // update GUI
 if EditorForm is TVSTGUI then
  with TVSTGUI(EditorForm)
   do UpdateSampleRate;
end;

procedure TVSTDecimator.ParameterBitsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FBitDepth := Value;
 FBitMul := Power(2, FBitDepth + 1) - 1;
 FBitDiv := 1 / FBitMul;

 // update GUI
 if Assigned(EditorForm) then
  with TVSTGUI(EditorForm)
   do UpdateBits;
end;

procedure TVSTDecimator.ParameterFilterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFilterType := TDecimatorFilterType(round(Value));

 // update GUI
 if Assigned(EditorForm) then
  with TVSTGUI(EditorForm)
   do UpdateFilterType;
end;


////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i :Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter>1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * Inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * Inputs[1, i]) * FBitDiv;
      end;

     FState[0, 0] := FState[0, 0] + FCutoffFreqNorm * (FYL - FState[0, 0] + FFC * (FState[0, 0] - FState[0, 1])) + CDenorm32;
     FState[0, 1] := FState[0, 1] + FCutoffFreqNorm * (FState[0, 0] - FState[0, 1]) + CDenorm32;
     FState[1, 0] := FState[1, 0] + FCutoffFreqNorm * (FYR - FState[1, 0] + FFC * (FState[1, 0] - FState[1, 1])) + CDenorm32;
     FState[1, 1] := FState[1, 1] + FCutoffFreqNorm * (FState[1, 0] - FState[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FState[0, 0] > 1 then FState[0, 0] := 1 else if FState[0, 0] < -1 then FState[0, 0] := -1;
     if FState[0, 1] > 1 then FState[0, 1] := 1 else if FState[0, 1] < -1 then FState[0, 1] := -1;
     if FState[1, 0] > 1 then FState[1, 0] := 1 else if FState[1, 0] < -1 then FState[1, 0] := -1;
     if FState[1, 1] > 1 then FState[1, 1] := 1 else if FState[1, 1] < -1 then FState[1, 1] := -1;

     Outputs[0,i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[0, i] + FMixWet * FState[0, 1])));
     Outputs[1,i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[1, i] + FMixWet * FState[1, 1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * Inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * Inputs[1, i]) * FBitDiv;
      end;

     FState[0, 0] := FState[0, 0] + FCutoffFreqNorm * (FYL - FState[0, 0] + FFC * (FState[0, 0] - FState[0, 1])) + CDenorm32;
     FState[0, 1] := FState[0, 1] + FCutoffFreqNorm * (FState[0, 0] - FState[0, 1]) + CDenorm32;
     FState[1, 0] := FState[1, 0] + FCutoffFreqNorm * (FYR - FState[1, 0] + FFC * (FState[1, 0] - FState[1, 1])) + CDenorm32;
     FState[1, 1] := FState[1, 1] + FCutoffFreqNorm * (FState[1, 0] - FState[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FState[0, 0] > 1 then FState[0, 0] := 1 else if FState[0, 0] < -1 then FState[0, 0] := -1;
     if FState[0, 1] > 1 then FState[0, 1] := 1 else if FState[0, 1] < -1 then FState[0, 1] := -1;
     if FState[1, 0] > 1 then FState[1, 0] := 1 else if FState[1, 0] < -1 then FState[1, 0] := -1;
     if FState[1, 1] > 1 then FState[1, 1] := 1 else if FState[1, 1] < -1 then FState[1, 1] := -1;

     Outputs[0, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[0, i] + FMixWet * (FYL - FState[0, 1]))));
     Outputs[1, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[1, i] + FMixWet * (FYR - FState[0, 1]))));
    end;
  end;
end;



////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * Inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * Inputs[1, i]) * FBitDiv;
      end;

     FState[0, 0] := FState[0, 0] + FCutoffFreqNorm * (FYL - FState[0, 0] + FFC * (FState[0, 0] - FState[0, 1])) + CDenorm32;
     FState[0, 1] := FState[0, 1] + FCutoffFreqNorm * (FState[0, 0]-FState[0, 1]) + CDenorm32;
     FState[1, 0] := FState[1, 0] + FCutoffFreqNorm * (FYR - FState[1, 0] + FFC * (FState[1, 0] - FState[1, 1])) + CDenorm32;
     FState[1, 1] := FState[1, 1] + FCutoffFreqNorm * (FState[1, 0]-FState[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FState[0, 0] > 1 then FState[0, 0] := 1 else if FState[0, 0] < -1 then FState[0, 0] := -1;
     if FState[0, 1] > 1 then FState[0, 1] := 1 else if FState[0, 1] < -1 then FState[0, 1] := -1;
     if FState[1, 0] > 1 then FState[1, 0] := 1 else if FState[1, 0] < -1 then FState[1, 0] := -1;
     if FState[1, 1] > 1 then FState[1, 1] := 1 else if FState[1, 1] < -1 then FState[1, 1] := -1;

     Outputs[0, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[0, i] + FMixWet * FState[0, 1])));
     Outputs[1, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[1, i] + FMixWet * FState[1, 1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter-1;
       FYL := round(FBitMul * Inputs[0,i]) * FBitDiv;
       FYR := round(FBitMul * Inputs[1,i]) * FBitDiv;
      end;

     FState[0, 0] := FState[0, 0] + FCutoffFreqNorm * (FYL - FState[0, 0] + FFC * (FState[0, 0] - FState[0, 1])) + CDenorm32;
     FState[0, 1] := FState[0, 1] + FCutoffFreqNorm * (FState[0, 0] - FState[0, 1]) + CDenorm32;
     FState[1, 0] := FState[1, 0] + FCutoffFreqNorm * (FYR - FState[1, 0] + FFC*(FState[1, 0] - FState[1, 1])) + CDenorm32;
     FState[1, 1] := FState[1, 1] + FCutoffFreqNorm * (FState[1, 0] - FState[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FState[0, 0] > 1 then FState[0, 0] := 1 else if FState[0, 0] < -1 then FState[0, 0] := -1;
     if FState[0, 1] > 1 then FState[0, 1] := 1 else if FState[0, 1] < -1 then FState[0, 1] := -1;
     if FState[1, 0] > 1 then FState[1, 0] := 1 else if FState[1, 0] < -1 then FState[1, 0] := -1;
     if FState[1, 1] > 1 then FState[1, 1] := 1 else if FState[1, 1] < -1 then FState[1, 1] := -1;

     Outputs[0, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[0, i] + FMixWet*(FYL - FState[0, 1]))));
     Outputs[1, i] := Limit((FOutVol * ((1 - FMixWet) * Inputs[1, i] + FMixWet*(FYR - FState[0, 1]))));
    end;
  end;
end;

procedure TVSTDecimator.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status : Integer;
  CCData : Single;
begin
 Status := MidiEvent.midiData[0] and $F0; // channel information is removed

 if (Status = $B0) then // midi CC ?
  begin
   CCData := MidiEvent.midiData[2] / 127; // CC data
   case MidiEvent.midiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData) * 2.205;
    71: Parameter[1] := 24 * CCData;
    72: Parameter[2] := FreqLinearToLog(CCData);
    73: Parameter[3] := 10 * CCData;
    74: Parameter[5] := 100 * CCData;
    75: Parameter[6] := 6 - 30 * CCData;
   end;
  end;
end;

end.
