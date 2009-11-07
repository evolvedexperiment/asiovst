unit StkEchoDM;

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
  DAV_StkEcho;

type
  TStkEchoModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FEcho      : array [0..1] of TStkEcho;
    FSemaphore : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  StkEchoGUI;

procedure TStkEchoModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TStkEchoModule.VSTModuleOpen(Sender: TObject);
var
  Params : TDAVSingleDynArray;
begin
 FEcho[0] := TStkEcho.Create(SampleRate, SampleRate);
 FEcho[1] := TStkEcho.Create(SampleRate, SampleRate);
 Parameter[0] := 500;
 Parameter[1] :=  30;
 SetLength(Params, numParams);
 Params[0] := Parameter[0];
 Params[1] := Parameter[1];
 Programs[0].SetParameters(Params);
 Params[0] := 400;
 Params[1] :=  25;
 Programs[1].SetParameters(Params);
 Params[0] := 450;
 Params[1] :=  28;
 Programs[2].SetParameters(Params);
 Params[0] := 200;
 Params[1] :=  33;
 Programs[3].SetParameters(Params);
 Params[0] := 600;
 Params[1] :=  60;
 Programs[4].SetParameters(Params);
end;

procedure TStkEchoModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FEcho);
end;

procedure TStkEchoModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmStkEcho.Create(Self);
end;

procedure TStkEchoModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FEcho[0].Tick(Inputs[0, Sample]);
    Outputs[1, Sample] := FEcho[1].Tick(Inputs[1, Sample]);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkEchoModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FEcho[0].Tick(Inputs[0, Sample]);
    Outputs[1, Sample] := FEcho[1].Tick(Inputs[1, Sample]);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkEchoModule.ParamDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 inc(FSemaphore);
 try
  FEcho[0].Delay := 0.99 * Value;
  FEcho[0].Delay := 1.01 * Value;
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmStkEcho
  then TFmStkEcho(EditorForm).UpdateDelay;
end;

procedure TStkEchoModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FEcho[0].EffectMix := 0.01 * Value;
 FEcho[1].EffectMix := 0.01 * Value;

 if EditorForm is TFmStkEcho
  then TFmStkEcho(EditorForm).UpdateEffectMix;
end;

procedure TStkEchoModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FEcho[0].SampleRate := SampleRate;
 FEcho[1].SampleRate := SampleRate;
end;

end.
