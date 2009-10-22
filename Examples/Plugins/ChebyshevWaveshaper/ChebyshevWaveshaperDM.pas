unit ChebyshevWaveshaperDM;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspWaveshaper;

const
  CHarmCount : Integer = 24;
  CdBMin : Single = -140;

type
  TChebyshevWaveshaperDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDouble(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHarmDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHarmLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FChebysheWaveshaper : TChebyshevWaveshaper;
    FVolume             : Single;
    procedure ParamHarmonicChange(Sender: TObject; const Index: Integer; var Value: Single);
  public
  end;

implementation

{$R *.DFM}

uses
  ChebyshevWaveshaperGUI, DAV_Common, DAV_VSTParameters;

procedure TChebyshevWaveshaperDataModule.VSTModuleCreate(Sender: TObject);
var
  i : Integer;
begin
 FVolume := 1;
 for i := CHarmCount - 1 downto 0 do
  with ParameterProperties.Insert(0) do
   begin
    DisplayName       := 'Harmonic ' + IntToStr(i + 1);
    Min               := -1;
    Max               := 1;
    StepFloat         := 1;
    StepInteger       := 1;
    SmallStepFloat    := 0.1;
    LargeStepFloat    := 10;
    LargeStepInteger  := 10;
    ShortLabel        := 'H' + IntToStr(i + 1);
    Units             := 'dB';
    OnParameterChange := ParamHarmonicChange;
    OnCustomParameterDisplay := ParamHarmDisplay;
    OnCustomParameterLabel := ParamHarmLabel;
   end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmChebyshevWaveshaper.Create(Self);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 FChebysheWaveshaper := TChebyshevWaveshaper.Create;
 FChebysheWaveshaper.Order := CHarmCount;

 // initial parameters
 Parameter[0] := 1;
 for i := 1 to CHarmCount - 1
  do Parameter[i] := 0;
 Parameter[CHarmCount] := 0;

 // programs
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChebysheWaveshaper);
end;

procedure TChebyshevWaveshaperDataModule.ParamVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVolume := dB_to_Amp(Value);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0 then PreDefined := 'dB (+)' else
 if Parameter[Index] < 0
  then PreDefined := 'dB (-)'
  else PreDefined := 'dB';
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := abs(140 * Parameter[Index]) - 140;
 if abs(Parameter[Index]) < 1E-3
  then PreDefined := '-oo'
  else PreDefined := FloatToStrF(Val, ffGeneral, 3, 3);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmonicChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FChebysheWaveshaper)then
  begin
   if abs(Value) < 1E-3 then
    begin
     FChebysheWaveshaper.Gain[Index] := 0;
    end
   else
    begin
     FChebysheWaveshaper.Level[Index]    := abs(Value * 140) - 140;
     FChebysheWaveshaper.Inverted[Index] := Value < 0;
    end;
  end;
 if EditorForm is TFmChebyshevWaveshaper then
  with TFmChebyshevWaveshaper(EditorForm) do
   begin
    UpdateHarmonic(Index);
   end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 for Channel := 0 to 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FVolume * FChebysheWaveshaper.ProcessSample64(Inputs[Channel, Sample]);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcessDouble(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 for Channel := 0 to 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FVolume * FChebysheWaveshaper.ProcessSample64(Inputs[Channel, Sample]);
end;

end.