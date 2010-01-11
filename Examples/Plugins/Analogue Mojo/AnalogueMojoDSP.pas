unit AnalogueMojoDSP;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilter, DAV_DspTransformerSimulation;

type
  TAnalogueMojoDM = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMulti(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FTransformator : array of TTransformatorSimulation;
    procedure ChooseProcess;
  public

  end;

implementation

{$R *.DFM}

procedure TAnalogueMojoDM.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FTransformator, numInputs);

 ChooseProcess;

 for ChannelIndex := 0 to Length(FTransformator) - 1
  do FTransformator[ChannelIndex] := TTransformatorSimulation.Create;

 // initialize parameter
 Parameter[0] := 12;
end;

procedure TAnalogueMojoDM.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FTransformator) - 1
  do FreeAndNil(FTransformator[ChannelIndex]);
end;

procedure TAnalogueMojoDM.ChooseProcess;
begin
 case Length(FTransformator) of
  1 : OnProcess := VSTModuleProcessMono;
  2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMulti;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TAnalogueMojoDM.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FTransformator) - 1 do
  if Assigned(FTransformator[ChannelIndex])
   then FTransformator[ChannelIndex].HighpassFrequency := (1 + 0.02 * ChannelIndex) * Value;
end;

procedure TAnalogueMojoDM.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FTransformator) - 1 do
   if Assigned(FTransformator[ChannelIndex])
    then FTransformator[ChannelIndex].SampleRate := SampleRate;
end;

procedure TAnalogueMojoDM.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1
  do Outputs[0, SampleIndex] := FTransformator[0].ProcessSample64(Inputs[0, SampleIndex])
end;

procedure TAnalogueMojoDM.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FTransformator[0].ProcessSample64(0.033 * Inputs[1, SampleIndex] + 0.967 * Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FTransformator[1].ProcessSample64(0.034 * Inputs[0, SampleIndex] + 0.967 * Inputs[1, SampleIndex]);
  end;
end;

procedure TAnalogueMojoDM.VSTModuleProcessMulti(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 for ChannelIndex := 0 to Length(FTransformator) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[ChannelIndex, SampleIndex] := FTransformator[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex])
end;

end.
