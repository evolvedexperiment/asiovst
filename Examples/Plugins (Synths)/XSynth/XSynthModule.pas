unit XSynthModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTEffect,
  DAV_VSTModule, XSynthVoice, VoiceList;

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTSSModuleLevelParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1TypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2TypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleDriveParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleCutoffParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleResonanceParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1DecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1SustainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1LevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2DecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2SustainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2LevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
  private
    FLevel: Single;
    FDrive: Single;
    FCutoff: array [0..1] of Single;
    FRes: array [0..1] of Single;
    FOld: array [0..1] of Single;
    FOscs: array [0..1] of TOsc;
    FVoices: TVoiceList;
    function GetOscilators(index: integer): TOsc;
  public
    property Voices: TVoiceList read FVoices;
    property Oscilators[Index: Integer]: TOsc read GetOscilators;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, XSynthGUI;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
  FVoices := TVoiceList.Create(True);
  FLevel := 1; FDrive := 1;
  ParameterProperties[0].Max := Integer(otNoise);
  ParameterProperties[1].Max := Integer(otNoise);
  with FOscs[0] do
  begin
    OType    := otSine;
    Attack   := 0.5;
    Decay    := 0.5;
    Sustain  := 0.5;
    Release  := 0.5;
    Level    := 1;
  end;
  with FOscs[1] do
  begin
    OType    := otNone;
    Attack   := 0.5;
    Decay    := 0.5;
    Sustain  := 0.5;
    Release  := 0.5;
    Level    := 1;
  end;
  FCutoff[0] := 0.5;
  FCutoff[1] := 0.5;
  FRes[0]    := 0.1;
  FRes[1]    := 0.1;

  // Default Preset
  with Programs[0] do
  begin
    Parameter[ 0] := 1;
    Parameter[ 1] := 50;
    Parameter[ 2] := 50;
    Parameter[ 3] := 50;
    Parameter[ 4] := 50;
    Parameter[ 5] := 100;
    Parameter[ 6] := 0;
    Parameter[ 7] := 50;
    Parameter[ 8] := 50;
    Parameter[ 9] := 50;
    Parameter[10] := 50;
    Parameter[11] := 100;
    Parameter[12] := 1;
    Parameter[13] := 20000;
    Parameter[14] := 1;
    Parameter[15] := 100;
  end;

  // Preset 1
  with Programs[1] do
  begin
    Parameter[ 0] := 4;
    Parameter[ 1] := 50;
    Parameter[ 2] := 50;
    Parameter[ 3] := 50;
    Parameter[ 4] := 50;
    Parameter[ 5] := 20;
    Parameter[ 6] := 2;
    Parameter[ 7] := 15;
    Parameter[ 8] := 50;
    Parameter[ 9] := 50;
    Parameter[10] := 50;
    Parameter[11] := 100;
    Parameter[12] := 8;
    Parameter[13] := 8000;
    Parameter[14] := 8;
    Parameter[15] := 90;
  end;

  // set editor form class
  EditorFormClass := TVSTGUI;
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
  FreeAndNil(FVoices);
end;

procedure TVSTSSModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  VoiceIndex: Integer;
  SampleIndex: Integer;
  fb: Single;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := 0; VoiceIndex := 0;
    while VoiceIndex < Voices.Count do
    begin
      Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[VoiceIndex].Process;
      Inc(VoiceIndex);
    end;
  end;

  if FDrive > 1 then
   for SampleIndex := 0 to SampleFrames - 1 do
     Outputs[0, SampleIndex] := FastTanhOpt5TermFPU(FDrive * Outputs[0, SampleIndex]);

  FCutoff[1] := 0.9 * FCutoff[1] + 0.1 * FCutoff[0];
  FRes[1] := 0.9 * FRes[1] + 0.1 * FRes[0];

  fb := FRes[1] + FRes[1] / (1 - FCutoff[1] * 0.9);
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    FOld[0] := FOld[0] + FCutoff[1] * (Outputs[0,SampleIndex] - FOld[0] + fb * (FOld[0] - FOld[1])) + CDenorm32;
    FOld[1] := FOld[1] + FCutoff[1] * (FOld[0] - FOld[1]);
    Outputs[0, SampleIndex] := FLevel * FOld[1];
  end;

  for VoiceIndex := 1 to numOutputs - 1 do
    Move(Outputs[0, 0], Outputs[VoiceIndex, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcess64Replacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  VoiceIndex: Integer;
  SampleIndex: Integer;
  fb: Single;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := 0; VoiceIndex := 0;
    while VoiceIndex < Voices.Count do
    begin
      Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[VoiceIndex].Process;
      Inc(VoiceIndex);
    end;
  end;

  if FDrive > 1 then
   for SampleIndex := 0 to SampleFrames - 1 do
     Outputs[0, SampleIndex] := FastTanhOpt5TermFPU(FDrive * Outputs[0, SampleIndex]);

  FCutoff[1] := 0.9 * FCutoff[1] + 0.1 * FCutoff[0];
  FRes[1] := 0.9 * FRes[1] + 0.1 * FRes[0];

  fb := FRes[1] + FRes[1] / (1 - FCutoff[1] * 0.9);
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    FOld[0] := FOld[0] + FCutoff[1] * (Outputs[0,SampleIndex] - FOld[0] + fb * (FOld[0] - FOld[1])) + CDenorm32;
    FOld[1] := FOld[1] + FCutoff[1] * (FOld[0] - FOld[1]);
    Outputs[0, SampleIndex] := FLevel * FOld[1];
  end;

  for VoiceIndex := 1 to numOutputs - 1 do
    Move(Outputs[0, 0], Outputs[VoiceIndex, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status: Byte;
  i: Integer;
  newNote: TXSynthVoice;
const
  CVeloDiv: Single = 1 / 128;
begin
  Status := MidiEvent.midiData[0] and $F0; // channel information is removed
  if (Status = $90) and (MidiEvent.mididata[2] > 0) then // "note on" ?
  begin
    if Voices.Count > 7 then
      Voices.Remove(Voices.Items[0]);
    newNote := TXSynthVoice.Create(Self);
    with newNote do
    begin
      MidiKey := MidiEvent.midiData[1];
      Velocity := MidiEvent.midiData[2];
      NoteOn(Midi2Pitch[MidiKey], Velocity * CVeloDiv);
    end;
    Voices.Add(newNote);
  end
  else
  if ((Status = $90) and (MidiEvent.mididata[2] = 0)) or (Status = $80) then
  begin
    // "note off" ?
    for i := 0 to Voices.Count - 1 do
    begin
      if (Voices.Items[i].MidiKey = MidiEvent.midiData[1]) then
      begin
        Voices.Items[i].NoteOff;
        Break;
      end;
    end;
  end
  else
  if (Status = $B0) and (MidiEvent.midiData[1] = $7E) then
    Voices.Clear; // all notes off
end;

procedure TVSTSSModule.VSTSSModuleLevelParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FLevel := Value * 0.01;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
    begin
      if SBLevel.Position <> Round(FLevel * 100) then
        SBLevel.Position := Round(FLevel * 100);
    end;
end;

procedure TVSTSSModule.VSTSSModuleOsc1TypeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].OType := TOscilatorType(Round(Value));
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if CBOsc1Type.ItemIndex <> Round(Value) then
        CBOsc1Type.ItemIndex := Round(Value);
end;

procedure TVSTSSModule.VSTSSModuleOsc2TypeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].OType := TOscilatorType(Round(Value));
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if CBOsc2Type.ItemIndex <> Round(Value) then
        CBOsc2Type.ItemIndex := Round(Value);
end;

procedure TVSTSSModule.VSTSSModuleOsc1AttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].Attack := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc1ADSR.Attack <> 0.01 * Value then
        Osc1ADSR.Attack := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1DecayChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].Decay := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc1ADSR.Decay <> 0.01 * Value then
        Osc1ADSR.Decay := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1ReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].Release := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc1ADSR.Release <> 0.01 * Value then
        Osc1ADSR.Release := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1SustainChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].Sustain := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc1ADSR.Sustain <> 0.01 * Value then
        Osc1ADSR.Sustain := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1LevelChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[0].Level := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc1Level.Position <> Round(100 * FOscs[0].Level) then
        Osc1Level.Position := Round(100 * FOscs[0].Level);
end;

procedure TVSTSSModule.VSTSSModuleOsc2AttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].Attack := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc2ADSR.Attack <> 0.01 * Value then
        Osc2ADSR.Attack := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2DecayChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].Decay := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc2ADSR.Decay <> 0.01 * Value then
        Osc2ADSR.Decay := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2ReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].Release := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc2ADSR.Release <> 0.01 * Value then
        Osc2ADSR.Release := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2SustainChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].Sustain := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Osc2ADSR.Sustain <> 0.01 * Value then
        Osc2ADSR.Sustain := 0.01 * Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2LevelChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FOscs[1].Level := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
     if Osc2Level.Position <> Round(100 * FOscs[1].Level) then
       Osc2Level.Position := Round(100 * FOscs[1].Level);
end;

procedure TVSTSSModule.VSTSSModuleCutoffParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FCutoff[0] := 0.01 + Value / 20000;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Round(100 * FreqLogToLinear(((FCutoff[0] - 0.01) * 20000))) <> SBCutoff.Position then
        SBCutoff.Position := Round(100 * FreqLogToLinear((FCutoff[0] - 0.01) * 20000));
end;

procedure TVSTSSModule.VSTSSModuleResonanceParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FRes[0] := 0.01 * Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
      if Round(100 * FRes[0]) <> SBResonance.Position then
        SBResonance.Position := Round(100 * FRes[0]);
end;

procedure TVSTSSModule.VSTSSModuleDriveParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FDrive := Value;
  if EditorForm is TVSTGUI then
    with TVSTGUI(EditorForm) do
    begin
      if SBDrive.Position<>Round(FDrive * 10) then
        SBDrive.Position := Round(FDrive * 10);
    end;
end;

function TVSTSSModule.GetOscilators(index: integer): TOsc;
begin
  Result := FOscs[index];
end;

end.
