unit OversampledTanhModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPolyphaseDownsampler, 
  DAV_DspPolyphaseUpSampler;

type
  TOversampledTanhModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTEditOpen(Sender: TObject; var GUI: TForm; const ParentWindow: Cardinal);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamCoeffsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamTransitionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FDownsampler2x   : array [0..1] of TPolyphaseDownsampler32;
    FUpSampler2x     : array [0..1] of TPolyphaseUpsampler32;
    FBuffer          : PDAVSingleFixedArray;
    FCriticalSection : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_Approximations, DAV_VSTModuleWithDsp, OversampledTanhGUI;

procedure TOversampledTanhModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 ProcessingMode := pmBlockSave;
end;

procedure TOversampledTanhModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TOversampledTanhModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FDownsampler2x[Channel] := TPolyphaseDownsampler32.Create;
   FUpSampler2x[Channel] := TPolyphaseUpsampler32.Create;
  end;
 ReallocMem(FBuffer, 2 * BlockModeSize * SizeOf(Single));

 // initial parameters
 Parameter[0] := 16;
 Parameter[1] := 0.01;
end;

procedure TOversampledTanhModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FreeAndNil(FDownsampler2x[Channel]);
   FreeAndNil(FUpSampler2x[Channel]);
  end;
end;

procedure TOversampledTanhModule.VSTEditOpen(Sender: TObject;
  var GUI: TForm; const ParentWindow: Cardinal);
begin
  GUI := TFmOversampledTanh.Create(Self);
end;

procedure TOversampledTanhModule.ParamTransitionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
  for Channel := 0 to 1 do
   begin
    if assigned(FDownsampler2x[Channel]) then FDownsampler2x[Channel].Transition := 0.001 + 0.498 * Value;
    if assigned(FUpSampler2x[Channel]) then FUpSampler2x[Channel].Transition := 0.001 + 0.498 * Value;
   end;

 // update GUI
 if EditorForm is TFmOversampledTanh then
  with TFmOversampledTanh(EditorForm)
   do UpdateTransition;
end;

procedure TOversampledTanhModule.ParamCoeffsChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if (Value < 1) or (Value > 32) then Exit;

  for Channel := 0 to 1 do
   begin
    if assigned(FDownsampler2x[Channel]) then
     begin
      FDownsampler2x[Channel].NumberOfCoefficients := round(Value); //round(Value);
      FDownsampler2x[Channel].Transition := 0.01;
     end;
    if assigned(FUpSampler2x[Channel]) then
     begin
      FUpSampler2x[Channel].NumberOfCoefficients := round(Value);
      FUpSampler2x[Channel].Transition := 0.01;
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmOversampledTanh then
  with TFmOversampledTanh(EditorForm)
   do UpdateCoeffs;
end;

procedure TOversampledTanhModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample: Integer;
begin
  FCriticalSection.Enter;
  try
   for Channel := 0 to 1 do
    begin
     FUpSampler2x[Channel].ProcessBlock(@Inputs[Channel][0], @FBuffer[0], 16);
     for Sample := 0 to 2 * SampleFrames - 1
      do FBuffer[Sample] := CDenorm32 + FastTanhOpt5Term(FBuffer[Sample]);
     FDownsampler2x[Channel].ProcessBlock(@FBuffer[0], @Outputs[Channel][0], 16);
    end;
  finally
   FCriticalSection.Leave;
  end;
end;

procedure TOversampledTanhModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Sample: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   begin
    // nothing todo
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
