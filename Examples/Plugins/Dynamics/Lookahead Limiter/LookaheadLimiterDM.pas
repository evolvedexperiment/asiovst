unit LookaheadLimiterDM;

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
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Types,
  DAV_VSTModule, DAV_DspDynamicLookaheadLimiter, DAV_DspDelayLines;

type
  TLookaheadLimiterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDualMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessPeakMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterProcessingModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterProcessingModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLookaheadDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLookaheadChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FCriticalSection : TCriticalSection;
    FLimiter         : array [0..1] of TDspLookaheadLimiter32;
    FDelayLine       : array [0..1] of TDelayLineSamples32;
  public
    property Limiter: TDspLookaheadLimiter32 read FLimiter[0];
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, DAV_Common, DAV_VSTModuleWithPrograms,
  LookaheadLimiterGUI;

procedure TLookaheadLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLookaheadLimiterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLookaheadLimiterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..9, 0..5] of Single = (
    ( 0.0, -0.01, 0,  75, 1, 64),
    ( 0.5, -0.01, 0,  80, 1, 64),
    ( 1.0, -0.01, 1, 500, 1, 64),
    ( 1.5, -0.01, 1, 250, 1, 64),
    (10.0, -0.01, 0, 100, 1, 64),
    (20.0, -0.01, 2, 500, 1, 64),
    ( 4.0, -0.01, 0,  80, 1, 64),
    ( 6.0, -0.01, 1,  60, 1, 64),
    ( 3.0, -0.01, 0,  20, 1, 64));
begin
 // create limiter
 for Channel := 0 to Length(FLimiter) - 1 do
  begin
   FLimiter[Channel] := TDspLookaheadLimiter32.Create;
   FLimiter[Channel].SampleRate := SampleRate;
  end;

 // create delay lines
 for Channel := 0 to Length(FDelayLine) - 1
  do FDelayLine[Channel] := TDelayLineSamples32.Create(FLimiter[0].LookAhead);

 // initialize parameters
 Parameter[0] :=  0.00;
 Parameter[1] := -0.01;
 Parameter[2] :=  0.00;
 Parameter[3] := 75.00;
 Parameter[4] :=  1.00;
 Parameter[5] := 64.00;

 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TLookaheadLimiterDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 // free delay line
 for Channel := 0 to Length(FDelayLine) - 1
  do FreeAndNil(FDelayLine[Channel]);

 // free limiter line
 for Channel := 0 to Length(FLimiter) - 1
  do FreeAndNil(FLimiter[Channel]);
end;

procedure TLookaheadLimiterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLookaheadLimiter.Create(Self);
end;

procedure TLookaheadLimiterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLookaheadLimiterDataModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := 'µs' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TLookaheadLimiterDataModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3) else
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TLookaheadLimiterDataModule.ParameterOutputDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLookaheadLimiterDataModule.ParameterInputDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLookaheadLimiterDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLookaheadLimiterDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLookaheadLimiterDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLookaheadLimiterDataModule.ParameterProcessingModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
  0 : OnProcess := VSTModuleProcessStereo;
  1 : OnProcess := VSTModuleProcessPeakMono;
  2 : OnProcess := VSTModuleProcessDualMono;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TLookaheadLimiterDataModule.ParameterProcessingModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Stereo';
  1 : PreDefined := 'PeakMono';
  2 : PreDefined := 'DualMono';
 end;
end;

procedure TLookaheadLimiterDataModule.ParameterLookaheadDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(2 * Round(0.5 * Parameter[Index]));
end;

procedure TLookaheadLimiterDataModule.ParameterLookaheadChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  LookaheadSamples : Integer;
begin
 LookaheadSamples := 2 * Round(0.5 * Value);
 FCriticalSection.Enter;
 try
  if Assigned(FLimiter[0])
   then FLimiter[0].LookAhead := LookaheadSamples;
  if Assigned(FLimiter[1])
   then FLimiter[1].LookAhead := LookaheadSamples;
  if Assigned(FDelayLine[0])
   then FDelayLine[0].BufferSize := LookaheadSamples;
  if Assigned(FDelayLine[1])
   then FDelayLine[1].BufferSize := LookaheadSamples;
  InitialDelay := LookaheadSamples;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLookaheadLimiterDataModule.ParameterAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case TAttackShape(Round(Limit(Parameter[Index], 0, 1))) of
     asLinear : PreDefined := 'Linear';
  asParabolic : Predefined := 'Parabolic';
 end;
end;

procedure TLookaheadLimiterDataModule.ParameterAttackShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter[0])
  then FLimiter[0].AttackShape := TAttackShape(Round(Limit(Value, 0, 1)));
 if Assigned(FLimiter[1])
  then FLimiter[1].AttackShape := TAttackShape(Round(Limit(Value, 0, 1)));
end;

procedure TLookaheadLimiterDataModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter[0])
  then FLimiter[0].Input_dB := Value;
 if Assigned(FLimiter[1])
  then FLimiter[1].Input_dB := Value;

 if EditorForm is TFmLookaheadLimiter
  then TFmLookaheadLimiter(EditorForm).UpdateInput;
end;

procedure TLookaheadLimiterDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter[0])
  then FLimiter[0].Output_dB := Value;
 if Assigned(FLimiter[1])
  then FLimiter[1].Output_dB := Value;

 if EditorForm is TFmLookaheadLimiter
  then TFmLookaheadLimiter(EditorForm).UpdateOutput;
end;

procedure TLookaheadLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter[0])
  then FLimiter[0].Release := Value;
 if Assigned(FLimiter[1])
  then FLimiter[1].Release := Value;

 if EditorForm is TFmLookaheadLimiter
  then TFmLookaheadLimiter(EditorForm).UpdateRelease;
end;

procedure TLookaheadLimiterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FLimiter[0].InputSample(Inputs[0, Sample]);
   FLimiter[1].InputSample(Inputs[1, Sample]);
   Temp := Min(FLimiter[0].GainReductionFactor, FLimiter[1].GainReductionFactor);

   Outputs[0, Sample] := Temp * FDelayLine[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := Temp * FDelayLine[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TLookaheadLimiterDataModule.VSTModuleProcessDualMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLimiter[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := FLimiter[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TLookaheadLimiterDataModule.VSTModuleProcessPeakMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Temp := abs(Inputs[0, Sample]);
   if abs(Inputs[1, Sample]) > Temp
    then Temp := abs(Inputs[1, Sample]);

   FLimiter[0].InputSample(Temp);
   Temp := FLimiter[0].GainReductionFactor;
   Outputs[0, Sample] := Temp * FDelayLine[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := Temp * FDelayLine[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TLookaheadLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if abs(SampleRate) > 0 then
  begin
   if Assigned(FLimiter[0])
    then FLimiter[0].SampleRate := abs(SampleRate);
   if Assigned(FLimiter[1])
    then FLimiter[1].SampleRate := abs(SampleRate);
  end;
end;

end.
