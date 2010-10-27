unit NoiseReductionDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, Classes, Forms, SyncObjs, DAV_Types, DAV_Complex,
  DAV_DspDelayLines, DAV_DspSpectralNoiseReduction, DAV_DspWindowFunctions,
  DAV_VSTModule {$IFDEF Use_IPPS}, DAV_DspWindowFunctionsAdvanced{$ENDIF};

type
  TNoiseReductionModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure Parameter2DigitDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterThresholdOffsetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCriticalSection : TCriticalSection;
    FSamplesCaptured : Integer;
    FIsMatching      : Boolean;
    FAdditionalDelay : array of TDelayLineSamples32;
    FNoiseReduction  : array of TNoiseReduction32;
    function GetTimeCaptured: Single;
  public
    property TimeCaptured: Single read GetTimeCaptured;
  end;

implementation

{$R *.DFM}

uses
  SysUtils, Math, NoiseReductionGui, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule;

procedure TNoiseReductionModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TNoiseReductionModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TNoiseReductionModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 InitialDelay := 1 shl ParameterProperties[1].MaxInteger +
   1 shl (ParameterProperties[1].MaxInteger - 1);

 SetLength(FNoiseReduction, numInputs);
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel] := TNoiseReduction32.Create;

 SetLength(FAdditionalDelay, numInputs);
 for Channel := 0 to Length(FAdditionalDelay) - 1
  do FAdditionalDelay[Channel] := TDelayLineSamples32.Create(512);

 with ParameterProperties[2] do
  begin
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;

 FEditorRect.Bottom := 147;
 FEditorRect.Right := 348;

 Parameter[0] := 8;
 Parameter[1] := 9;
 Parameter[2] := 4;
 Parameter[3] := 10;
 Parameter[4] := 1;
 Parameter[5] := 0.5;
 Parameter[6] := 25;
 Parameter[7] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 8;
   Parameter[1] := 9;
   Parameter[2] := 4;
   Parameter[3] := 10;
   Parameter[4] := 1;
   Parameter[5] := 0.5;
   Parameter[6] := 25;
   Parameter[7] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 10;
   Parameter[1] := 9;
   Parameter[2] := 8;
   Parameter[3] := 50;
   Parameter[4] := 2;
   Parameter[5] := 0.1;
   Parameter[6] := 10;
   Parameter[7] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 12;
   Parameter[1] := 9;
   Parameter[2] := 1;
   Parameter[3] := 80;
   Parameter[4] := 0.5;
   Parameter[5] := 0.02;
   Parameter[6] := 1;
   Parameter[7] := 0;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 13;
   Parameter[1] := 9;
   Parameter[2] := 11;
   Parameter[3] := 100;
   Parameter[4] := 0;
   Parameter[5] := 0.01;
   Parameter[6] := 0.5;
   Parameter[7] := 0;
  end;
end;

procedure TNoiseReductionModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FreeAndNil(FNoiseReduction[Channel]);

 for Channel := 0 to Length(FAdditionalDelay) - 1
  do FreeAndNil(FAdditionalDelay[Channel]);
end;

procedure TNoiseReductionModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmNoiseReduction.Create(Self);
end;

procedure TNoiseReductionModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FIsMatching := Value > 0.5;

 // reset sample counter
 if FIsMatching
  then FSamplesCaptured := 0;

 // mark noise reduction for matching
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].Match := FIsMatching;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateMatchThreshold;
end;

procedure TNoiseReductionModule.ParameterWindowFunctionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FNoiseReduction) - 1
   do FNoiseReduction[Channel].WindowFunctionClass := GWindowFunctions[Round(Parameter[Index])];
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateWindowFunction;
end;

procedure TNoiseReductionModule.ParameterWindowFunctionDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TNoiseReductionModule.ParameterThresholdOffsetChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].ThresholdOffset := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateThresholdOffset;
end;

procedure TNoiseReductionModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1
  then PreDefined := FloatToStrF(Parameter[Index] * 1E3, ffGeneral, 3, 1)
  else PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 3, 1);
end;

procedure TNoiseReductionModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
if Parameter[Index] < 1
  then PreDefined := 'µs'
  else PreDefined := 'ms';
end;

function TNoiseReductionModule.GetTimeCaptured: Single;
begin
 Result := FSamplesCaptured / FSampleRate;
end;

procedure TNoiseReductionModule.Parameter2DigitDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 3, 1);
end;

procedure TNoiseReductionModule.ParameterFftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
  Delay   : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FNoiseReduction) - 1
   do FNoiseReduction[Channel].FFTOrder := Round(Value);

  Delay := InitialDelay - (1 shl Round(Value - 1) + 1 shl (Round(Value) - 2));

  for Channel := 0 to Length(FAdditionalDelay) - 1
   do FAdditionalDelay[Channel].BufferSize := Delay;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateFftOrder;
end;

procedure TNoiseReductionModule.ParameterFftOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TNoiseReductionModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateRatio;
end;

procedure TNoiseReductionModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].Knee := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateKnee;
end;

procedure TNoiseReductionModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].Attack := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateAttack;
end;

procedure TNoiseReductionModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[Channel].Release := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateRelease;
end;

procedure TNoiseReductionModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FNoiseReduction) - 1
   do FNoiseReduction[Channel].SampleRate := Abs(SampleRate);
end;

procedure TNoiseReductionModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FNoiseReduction) - 1 do
   begin
    FNoiseReduction[Channel].ProcessBlock(@Inputs[Channel, 0],
      @Outputs[Channel, 0], SampleFrames);
    FAdditionalDelay[Channel].ProcessBlock32(@Outputs[Channel, 0],
      SampleFrames);

   end;
 finally
  FCriticalSection.Leave;
 end;

 if FIsMatching
  then FSamplesCaptured := FSamplesCaptured + SampleFrames;
end;

end.
