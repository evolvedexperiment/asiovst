unit PhaserDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPhaser;

type
  TPhaserModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure PMDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMinimumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMaximumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection: TCriticalSection;
    FPhaser: array [0 .. 1] of TPhaser;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

procedure TPhaserModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure TPhaserModule.VSTModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FCriticalSection);
end;

procedure TPhaserModule.VSTModuleOpen(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to Length(FPhaser) - 1 do
  begin
    FPhaser[Index] := TPhaser.Create;
    FPhaser[Index].SampleRate := SampleRate;
  end;

  // initialize parameters
  Parameter[0] := 30;
  Parameter[1] := 30;
  Parameter[2] := 300;
  Parameter[3] := 1000;
  Parameter[4] := 0.1;
  Parameter[5] := 5;
end;

procedure TPhaserModule.VSTModuleClose(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to Length(FPhaser) - 1 do
    FreeAndNil(FPhaser[Index]);
end;

procedure TPhaserModule.PMDepthChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Depth := 0.01 * Value;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.PMFeedbackChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Feedback := 0.01 * Value;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.PMMinimumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Minimum := Limit(Value, 20, 20000);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.PMMaximumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Maximum := Limit(Value, 20, 20000);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.PMRateChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Rate := Value;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.PMStagesChange(Sender: TObject; const Index: Integer;
  var Value: Single);
var
  Channel: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to 1 do
      if Assigned(FPhaser[Channel]) then
        FPhaser[Channel].Stages := Round(Value);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.VSTModuleProcess(const inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample: Integer;
begin
  FCriticalSection.Enter;
  try
    for Sample := 0 to SampleFrames - 1 do
    begin
      Outputs[0, Sample] := FPhaser[0].ProcessSample32(inputs[0, Sample]);
      Outputs[1, Sample] := FPhaser[1].ProcessSample32(inputs[1, Sample]);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPhaserModule.VSTModuleProcessDoubleReplacing(const inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample: Integer;
begin
  FCriticalSection.Enter;
  try
    for Sample := 0 to SampleFrames - 1 do
    begin
      Outputs[0, Sample] := FPhaser[0].ProcessSample32(inputs[0, Sample]);
      Outputs[1, Sample] := FPhaser[1].ProcessSample32(inputs[1, Sample]);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
