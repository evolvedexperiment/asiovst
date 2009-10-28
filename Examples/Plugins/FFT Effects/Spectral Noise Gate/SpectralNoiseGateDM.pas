unit SpectralNoiseGateDM;

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

{$I DAV_Compiler.INC}

uses
  Windows, Messages, Classes, Forms, SyncObjs, DAV_Types, DAV_Complex,
  DAV_VSTModule, DAV_DspSpectralNoiseReduction;

type
  TSpectralNoiseGateModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection   : TCriticalSection;
    FSpectralNoiseGate : array of TSpectralNoiseGate32;
  public
  end;

implementation

{$R *.DFM}

uses
  SysUtils, Math, SpectralNoiseGateGui, DAV_VSTModuleWithPrograms;

procedure TSpectralNoiseGateModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralNoiseGateModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSpectralNoiseGateModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FSpectralNoiseGate, numInputs);
 for Channel := 0 to Length(FSpectralNoiseGate) - 1 do
  begin
   FSpectralNoiseGate[Channel] := TSpectralNoiseGate32.Create;
   with FSpectralNoiseGate[Channel] do
    begin
     FftOrder := Round(Log2(BlockModeSize));
     Assert(FftSize = BlockModeSize);
    end;
  end;

 Parameter[0] := -30;
 Parameter[1] := 0.5;
 Parameter[2] := 100;
 Parameter[3] := 10;
 Parameter[4] := 1;
end;

procedure TSpectralNoiseGateModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FreeAndNil(FSpectralNoiseGate[Channel]);
end;

procedure TSpectralNoiseGateModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSpectralNoiseGate.Create(Self);
end;

procedure TSpectralNoiseGateModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Threshold := Value;
end;

procedure TSpectralNoiseGateModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Ratio := 1 / Value;
end;

procedure TSpectralNoiseGateModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Knee := Value;
end;

procedure TSpectralNoiseGateModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Attack := Value;
end;

procedure TSpectralNoiseGateModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Release := Value;
end;

procedure TSpectralNoiseGateModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].SampleRate := SampleRate;
end;

procedure TSpectralNoiseGateModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralNoiseGate) - 1 do
   begin
    FSpectralNoiseGate[Channel].ProcessBlock(@Inputs[Channel, 0],
      @Outputs[Channel, 0], SampleFrames);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
