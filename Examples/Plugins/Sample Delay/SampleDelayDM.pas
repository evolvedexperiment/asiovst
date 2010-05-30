unit SampleDelayDM;

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

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Types,
  DAV_VSTModule, DAV_DspDelayLines;

type
  TSampleDelayDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterSamplesLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSamplesRightChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FDelayLine       : array of TDelayLineSamples32;
  public
  end;

implementation

{$R *.DFM}

uses
  SampleDelayGui, DAV_VSTCustomModule;

procedure TSampleDelayDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSampleDelayDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSampleDelayDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = numInputs);
 SetLength(FDelayLine, numInputs);
 for Channel := 0 to Length(FDelayLine) - 1
  do FDelayLine[Channel] := TDelayLineSamples32.Create(1025);
end;

procedure TSampleDelayDataModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FDelayLine) - 1
  do FreeAndNil(FDelayLine[Channel]);
end;

procedure TSampleDelayDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmSampleDelay.Create(Self);
end;

procedure TSampleDelayDataModule.ParameterSamplesLeftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDelayLine) - 1 do
   if (Channel mod 2 = 0) and Assigned(FDelayLine[Channel])
    then FDelayLine[Channel].BufferSize := Round(1025 + Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSampleDelay
  then TFmSampleDelay(EditorForm).UpdateSamplesLeft;
end;

procedure TSampleDelayDataModule.ParameterSamplesRightChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDelayLine) - 1 do
   if (Channel mod 2 = 1) and Assigned(FDelayLine[Channel])
    then FDelayLine[Channel].BufferSize := Round(1025 + Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSampleDelay
  then TFmSampleDelay(EditorForm).UpdateSamplesRight;
end;

procedure TSampleDelayDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDelayLine) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDelayLine[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
