unit TwoBandDistortionDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Classes, Messages, SysUtils, Forms, SyncObjs, DAV_Types, DAV_VSTModule,
  DAV_DSPFilterLinkwitzRiley;

type
  TTwoBandDistortionDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHighDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FLinkwitzRiley   : array [0..1] of TLinkwitzRiley;
    FLowMix          : array [0..1] of Single;
    FHighMix         : array [0..1] of Single;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  TwoBandDistortionGUI, DAV_Approximations, DAV_VSTCustomModule;

procedure TTwoBandDistortionDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1
  do FLinkwitzRiley[ChannelIndex] := TLinkwitzRiley.Create;

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := 2;
 Parameter[2] := 10;
 Parameter[3] := 10;

 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);
end;

procedure TTwoBandDistortionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLinkwitzRiley[0]);
 FreeAndNil(FLinkwitzRiley[1]);
end;

procedure TTwoBandDistortionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create; 
end;

procedure TTwoBandDistortionDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TTwoBandDistortionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmTwoBandDistortion.Create(Self);
end;

procedure TTwoBandDistortionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  Low, High    : Double;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   for ChannelIndex := 0 to 1 do
    begin
     // using Linkwitz-Riley TwoBand filters
     FLinkwitzRiley[ChannelIndex].ProcessSample(Inputs[ChannelIndex, SampleIndex], Low, High);

     Outputs[ChannelIndex, SampleIndex] := FLowMix[0]  * Low  + FastTanhOpt5TermFPU(FLowMix[1]  * Low) +
                                 FHighMix[0] * High + FastTanhOpt5TermFPU(FHighMix[1] * High);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTwoBandDistortionDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  Low, High    : Double;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   for ChannelIndex := 0 to 1 do
    begin
     // using Linkwitz-Riley TwoBand filters
     FLinkwitzRiley[ChannelIndex].ProcessSample(Inputs[ChannelIndex, SampleIndex], Low, High);

     Outputs[ChannelIndex, SampleIndex] := FLowMix[0]  * Low  + FastTanhOpt5TermFPU(FLowMix[1]  * Low) +
                                 FHighMix[0] * High + FastTanhOpt5TermFPU(FHighMix[1] * High);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TTwoBandDistortionDataModule.ParamLowDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLowMix[1] := 0.01 * Value;
 FLowMix[0] := 1 - FLowMix[1];

 // update GUI
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateLowDistortion;
end;

procedure TTwoBandDistortionDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TTwoBandDistortionDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4)
  else PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 4, 4);
end;

procedure TTwoBandDistortionDataModule.ParamHighDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHighMix[1] := 0.01 * Value;
 FHighMix[0] := 1 - FHighMix[1];

 // update GUI
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateHighDistortion;
end;

procedure TTwoBandDistortionDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to numInputs - 1 do
   if Assigned(FLinkwitzRiley[ChannelIndex])
    then FLinkwitzRiley[ChannelIndex].Order := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateOrder;
end;

procedure TTwoBandDistortionDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to numInputs - 1 do
   if Assigned(FLinkwitzRiley[ChannelIndex])
    then FLinkwitzRiley[ChannelIndex].Frequency := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateFrequency;
end;

{$IFDEF FPC}
initialization
  {$i TwoBandDistortionDM.lrs}
{$ENDIF}

end.
