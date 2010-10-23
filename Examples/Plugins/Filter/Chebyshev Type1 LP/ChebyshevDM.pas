unit ChebyshevDM;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_DspFilterChebyshev,
  DAV_DspFilterChebyshevType1, DAV_VstWindowSizer;

type
  TChebyshevLPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRippleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array of TCustomChebyshev1LowpassFilter;
    FResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read FResizer;
  end;

implementation

{$R *.DFM}

uses
  Math, ChebyshevGUI;

procedure TChebyshevLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for ch := 0 to Length(FFilter) - 1 do
  begin
   FFilter[ch] := TChebyshev1LowpassFilter.Create(4);
   FFilter[ch].SetFilterValues(1000, 0, 1);
  end;
(*
 FResizer := TVstWindowSizer.Create;
 FResizer.Effect := Self;
*)

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := 1;
 Parameter[2] := 4;

 with Programs[0] do
  begin
   Parameter[0] := 1000;
   Parameter[1] := 1;
   Parameter[2] := 4;
  end;
end;

procedure TChebyshevLPModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[Channel]);
// FreeAndNil(FResizer);
end;

procedure TChebyshevLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev.Create(Self);
end;

procedure TChebyshevLPModule.ParamRippleChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel]) then FFilter[Channel].Ripple := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateRipple;
end;

procedure TChebyshevLPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Order := round(Value); // max(2, 2 * round(0.5 * Value));

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateOrder;
end;

procedure TChebyshevLPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateFrequency;
end;

procedure TChebyshevLPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].SampleRate := SampleRate;
end;

procedure TChebyshevLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);
end;

procedure TChebyshevLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);
end;

end.
