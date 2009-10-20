unit DitherNoiseshaperDM;

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

uses 
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspDitherNoiseshaper;

type
  TDitherNoiseshaperModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBitDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDitherTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterNoiseshaperFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDitherChangeAmplitude(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDitherTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FDitherNoiseshaper : array of TDitherHighShelfNoiseShaper32;
    FCriticalSection   : TCriticalSection;
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperGUI;

procedure TDitherNoiseshaperModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TDitherNoiseshaperModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TDitherNoiseshaperModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FDitherNoiseshaper, numInputs);
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FDitherNoiseshaper[Channel] := TDitherHighShelfNoiseShaper32.Create;
 Parameter[0] := 16;
 Parameter[1] := 1;
 Parameter[2] := 2;
 Parameter[3] := 1;
 Parameter[4] := 14000;
end;

procedure TDitherNoiseshaperModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FreeAndNil(FDitherNoiseshaper[Channel]);
end;

procedure TDitherNoiseshaperModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmDitherNoiseshaper.Create(Self);
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherType := TDitherType(round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherType;
end;

procedure TDitherNoiseshaperModule.ParameterDitherChangeAmplitude(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherAmplitude := Value;

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherAmplitude;
end;

procedure TDitherNoiseshaperModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].Limit := Boolean(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateLimit;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].BitDepth := Round(Value);

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateBitDepth;
end;

procedure TDitherNoiseshaperModule.ParameterNoiseshaperFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].Frequency := Value;

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateNoiseShaperFrequency;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TDitherNoiseshaperModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  COnOff :array [0..1] of string = ('Off', 'On');
begin
 PreDefined := COnOff[round(Parameter[Index])];
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 case TDitherType(Round(Parameter[Index])) of
        dtNone : PreDefined := 'None';
       dtEqual : PreDefined := 'Rectangular';
  dtTriangular : PreDefined := 'Triangular';
       dtGauss : PreDefined := 'Gauss';
   dtFastGauss : PreDefined := 'Gauss (fast)';
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].SampleRate := SampleRate;
end;

end.
