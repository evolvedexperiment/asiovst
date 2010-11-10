unit StkPitchShiftDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_StkPitchShift;

type
  TStkPitchShiftModule = class(TVSTModule)
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
    FPitchShift : array [0..1] of TStkPitchShifter;
    FSemaphore  : Integer;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, StkPitchShiftGUI;

procedure TStkPitchShiftModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TStkPitchShiftModule.VSTModuleOpen(Sender: TObject);
begin
 FPitchShift[0] := TStkPitchShifter.Create(SampleRate);
 FPitchShift[1] := TStkPitchShifter.Create(SampleRate);
 Parameter[0] := 0;
 Parameter[1] := 100;
(*
 Programs[0].SetParameters(FParameter);
 Programs[1].SetParameters(FParameter);
 Programs[2].SetParameters(FParameter);
 Programs[3].SetParameters(FParameter);
 Programs[4].SetParameters(FParameter);
*)
end;

procedure TStkPitchShiftModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FPitchShift);
end;

procedure TStkPitchShiftModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmStkPitchShift.Create(Self);
end;

procedure TStkPitchShiftModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FPitchShift[0].Tick(Inputs[0, Sample]);
    Outputs[1, Sample] := FPitchShift[1].Tick(Inputs[1, Sample]);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkPitchShiftModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FPitchShift[0].Tick(Inputs[0, Sample]);
    Outputs[1, Sample] := FPitchShift[1].Tick(Inputs[1, Sample]);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkPitchShiftModule.ParamDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 inc(FSemaphore);
 try
  FPitchShift[0].Shift := Power(2, Value / 12);
  FPitchShift[1].Shift := FPitchShift[0].Shift;
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmStkPitchShift
  then TFmStkPitchShift(EditorForm).UpdateDelay;
end;

procedure TStkPitchShiftModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FPitchShift[0]) then
  with FPitchShift[0] do
   begin
    EffectMix := 0.01 * Value;
    FPitchShift[1].EffectMix := EffectMix;
   end;

 if EditorForm is TFmStkPitchShift
  then TFmStkPitchShift(EditorForm).UpdateEffectMix;
end;

procedure TStkPitchShiftModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FPitchShift[0].SampleRate := SampleRate;
 FPitchShift[1].SampleRate := SampleRate;
end;

end.
