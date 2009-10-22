unit BarberpoleShifterDM;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFrequencyShifter;

type
  TBarberpoleShifterDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterMixChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCoeffsChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTransitionBWChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFreqShifter : array of TBodeFrequencyShifter32;
    FMix         : array [0..2] of Single;
    procedure ChooseProcess;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, BarberpoleShifterGUI;

procedure TBarberpoleShifterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFreqShifter, numInputs);

 ChooseProcess;

 for Channel := 0 to Length(FFreqShifter) - 1
  do FFreqShifter[Channel] := TBodeFrequencyShifter32.Create;

 Parameter[0] := 0.2;
 Parameter[1] := 40;
 Parameter[2] := 16;
 Parameter[3] := 0.02; 

(*
 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
*)
end;

procedure TBarberpoleShifterDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1
  do FreeAndNil(FFreqShifter[Channel]);
end;

procedure TBarberpoleShifterDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].Frequency := Value;

 if EditorForm is TFmBarberpoleShifter
  then TFmBarberpoleShifter(EditorForm).UpdateFrequency;
end;

procedure TBarberpoleShifterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := max(0, -(0.01 * Value));
 FMix[2] := max(0, (0.01 * Value));
 FMix[1] := 1 - max(FMix[0], FMix[2]);

 if EditorForm is TFmBarberpoleShifter
  then TFmBarberpoleShifter(EditorForm).UpdateMix;
end;

procedure TBarberpoleShifterDataModule.ParameterCoeffsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].CoefficientCount := round(Value);
end;

procedure TBarberpoleShifterDataModule.ParameterTransitionBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].TransitionBandwidth := Value;
end;

procedure TBarberpoleShifterDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TBarberpoleShifterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmBarberpoleShifter.Create(Self);
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FMix[1] * Inputs[0, Sample] +
     FMix[0] * Down + FMix[2] * Up;
  end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FMix[1] * Inputs[0, Sample] +
     FMix[0] * Down + FMix[2] * Up;

   FFreqShifter[1].ProcessSample(Inputs[1, Sample], Up, Down);
   Outputs[1, Sample] := FMix[1] * Inputs[1, Sample] +
     FMix[0] * Down + FMix[2] * Up;
  end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessMultiChannel(
  const Inputs, Outputs: TDAVArrayOfSingleDynArray;
  const SampleFrames: Integer);
var
  Channel  : Integer;
  Sample   : Integer;
  Up, Down : Single;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FFreqShifter[Channel].ProcessSample(Inputs[Channel, Sample], Up, Down);
    Outputs[Channel, Sample] := FMix[1] * Inputs[Channel, Sample] +
      FMix[0] * Down + FMix[2] * Up;
   end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].SampleRate := SampleRate;
end;

end.
