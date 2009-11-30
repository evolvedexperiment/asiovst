unit ButterworthDM;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilterButterworth, DAV_VstWindowSizer;

type
  TButterworthLPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter: array of TCustomButterworthFilter;
  public  
    function Magnitude_dB(Frequency: Single): Single;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Approximations, DAV_DspFilter, ButterworthGUI;

procedure TButterworthLPModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FFilter[ChannelIndex] := TButterworthLowPassFilter.Create;
   FFilter[ChannelIndex].SetFilterValues(1000, 0);
  end;

 Parameter[0] := 1000;
 Parameter[1] := 4;
end;

procedure TButterworthLPModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[ChannelIndex]);
end;

procedure TButterworthLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthLPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  begin
   if Assigned(FFilter[ChannelIndex])
    then FFilter[ChannelIndex].Order := round(Value);
  end;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateOrder;
end;

function TButterworthLPModule.Magnitude_dB(Frequency: Single): Single;
begin
 if Assigned(FFilter[0])
  then Result := 10 * FastLog10MinError5(FFilter[0].MagnitudeSquared(Frequency));
end;

procedure TButterworthLPModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].Frequency := Value;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateFrequency;
end;

procedure TButterworthLPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FFilter[ChannelIndex].SampleRate := SampleRate;
end;

procedure TButterworthLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[ChannelIndex, SampleIndex] := FFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
end;

procedure TButterworthLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[ChannelIndex, SampleIndex] := FFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
end;

{$IFDEF FPC}
initialization
  {$I ButterworthDM.lrs}
{$ENDIF}

end.
