unit RingModulatorDM;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspRingModulator;

type
  TRingModulatorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FRingMod : array of TAutoRingModulator32;
    procedure ChooseProcess;
  public
  end;

implementation

{$R *.DFM}

uses
  RingModulatorGUI;

procedure TRingModulatorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FRingMod, numInputs);

 ChooseProcess;

 for Channel := 0 to Length(FRingMod) - 1
  do FRingMod[Channel] := TAutoRingModulator32.Create;

 Parameter[0] := 10;

 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
end;

procedure TRingModulatorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1
  do FreeAndNil(FRingMod[Channel]);
end;

procedure TRingModulatorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmRingModulator.Create(Self);
end;

procedure TRingModulatorDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TRingModulatorDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  if assigned(FRingMod[Channel])
   then FRingMod[Channel].Frequency := Value;

 if EditorForm is TFmRingModulator
  then TFmRingModulator(EditorForm).UpdateFrequency;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
end;

procedure TRingModulatorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := FRingMod[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMultiChannel(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FRingMod[Channel].ProcessSample32(Inputs[Channel, Sample]);
end;

procedure TRingModulatorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  if assigned(FRingMod[Channel])
   then FRingMod[Channel].SampleRate := SampleRate;
end;

end.