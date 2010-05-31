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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
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
  TChebyshevHPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRippleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure StringToFrequencyParameter(Sender: TObject; const Index: Integer; const ParameterString: string; var Value: Single);
    procedure StringToOrderParameter(Sender: TObject; const Index: Integer; const ParameterString: string; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterRippleDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFilter  : array of TCustomChebyshev1HighpassFilter;
    FResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read FResizer;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Common, ChebyshevGUI;
  
procedure TChebyshevHPModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for Channel := 0 to Length(FFilter) - 1 do
  begin
   FFilter[Channel] := TChebyshev1HighpassFilter.Create(4);
   FFilter[Channel].SetFilterValues(1000, 0, 1);
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

procedure TChebyshevHPModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[Channel]);
// FreeAndNil(FResizer);
end;

procedure TChebyshevHPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev.Create(Self);
end;


// Parameter Change

procedure TChebyshevHPModule.ParamRippleChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel]) then FFilter[Channel].Ripple := Limit(Value, 1E-5, 10);

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateRipple;
end;

procedure TChebyshevHPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Order := Round(Value); //max(2, 2 * Round(0.5 * Value)); //

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateOrder;
end;

procedure TChebyshevHPModule.ParamFrequencyChange(Sender: TObject;
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


// parameter display

procedure TChebyshevHPModule.ParameterRippleDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 4, 4);
end;

procedure TChebyshevHPModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1000
  then PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 4, 4)
  else PreDefined := FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4)
end;

procedure TChebyshevHPModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz';
end;

procedure TChebyshevHPModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;


// string to parameter conversion

procedure TChebyshevHPModule.StringToFrequencyParameter(
  Sender: TObject; const Index: Integer; const ParameterString: string; var Value: Single);
var
  Str    : string;
  Indxes : array [0..1] of Integer;
  Mult   : Single;
begin
 Str := Trim(ParameterString);
 if Str = '' then Exit;

 // process unit extensions
 if Pos('k', Str) > 0 then Mult := 1E3 else
 if Pos('m', Str) > 0 then Mult := 1E-3
  else Mult := 1;

 Indxes[0] := 1;
 while (Indxes[0] <= Length(Str)) and
  (not (Str[Indxes[0]] in ['0'..'9', ',', '.'])) do Inc(Indxes[0]);

 if (Indxes[0] >= Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  (Str[Indxes[1]] in ['0'..'9', ',', '.']) do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Mult * StrToFloat(Str);
 except
 end;
end;

procedure TChebyshevHPModule.StringToOrderParameter(Sender: TObject;
  const Index: Integer; const ParameterString: string; var Value: Single);
var
  Str    : string;
  Indxes : array [0..1] of Integer;
begin
 Str := Trim(ParameterString);
 if Str = '' then Exit;

 Indxes[0] := 1;
 while (Indxes[0] <= Length(Str)) and
  (not (Str[Indxes[0]] in ['0'..'9'])) do Inc(Indxes[0]);

 if (Indxes[0] > Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  (Str[Indxes[1]] in ['0'..'9']) do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Round(StrToFloat(Str));
 except
 end;
end;


// samplerate change

procedure TChebyshevHPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FFilter) - 1 do
   if Assigned(FFilter[ChannelIndex])
    then FFilter[ChannelIndex].SampleRate := Abs(SampleRate);
end;


// process

procedure TChebyshevHPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);
end;

procedure TChebyshevHPModule.VSTModuleProcessDoubleReplacing(const Inputs,
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