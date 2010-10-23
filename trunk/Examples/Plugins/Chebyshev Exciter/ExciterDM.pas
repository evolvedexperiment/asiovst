unit ExciterDM;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilterButterworth, DAV_DspWaveshaper;

type
  TExciterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSourceLowpassFilter    : array [0..1, 0..1] of TButterworthLowPassFilter;
    FSourceHighpassFilter   : array [0..1, 0..1] of TButterworthHighPassFilter;
    FSplitterHighpassFilter : array [0..1, 0..1] of TButterworthHighPassFilter;
    FMix                    : array [0..1] of Single;
    FOverdriveGain          : Single;
    FChebyshevWaveshaper    : TChebyshevWaveshaperSquarelShape;
    procedure InvertMix;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_VSTCustomModule, ExciterGUI, DAV_VSTModuleWithPrograms;

procedure TExciterDataModule.VSTModuleOpen(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FSourceLowpassFilter[ch, i]    := TButterworthLowPassFilter.Create;
    FSourceHighpassFilter[ch, i]   := TButterworthHighPassFilter.Create;
    FSplitterHighpassFilter[ch, i] := TButterworthHighPassFilter.Create;
   end;
 FChebyshevWaveshaper := TChebyshevWaveshaperSquarelShape.Create;

 Parameter[0] := 8000;
 Parameter[1] := 4;
 Parameter[2] := 50;
 Parameter[3] := 50;

 with Programs[0] do
  begin
   Parameter[0] := 8000;
   Parameter[1] := 4;
   Parameter[2] := 50;
   Parameter[3] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 10000;
   Parameter[1] := 2;
   Parameter[2] := 80;
   Parameter[3] := 70;
  end;
end;

procedure TExciterDataModule.VSTModuleClose(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FreeAndNil(FSourceLowpassFilter[ch, i]);
    FreeAndNil(FSourceHighpassFilter[ch, i]);
    FreeAndNil(FSplitterHighpassFilter[ch, i]);
   end;
 FreeAndNil(FChebyshevWaveshaper);
end;

procedure TExciterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmExciter.Create(Self);
end;

procedure TExciterDataModule.InvertMix;
begin
 if Round(ParameterByName['Order']) mod 2 = 1
  then FMix[0] := -abs(FMix[0])
  else FMix[0] := abs(FMix[0]);
end;

procedure TExciterDataModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;
 FMix[0] := 1 - FMix[1];
 FMix[1] := 2 * FMix[1];
 InvertMix;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateMix;
end;

procedure TExciterDataModule.ParamShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FChebyshevWaveshaper)
  then FChebyshevWaveshaper.Shape := 2 - (0.01 * Value);
 FOverdriveGain := 1.4 - 0.4 * (0.01 * Value);

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateShape;
end;

procedure TExciterDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  for BandIndex := 0 to 1 do
   begin
    if Assigned(FSourceLowpassFilter[ChannelIndex, BandIndex])
     then FSourceLowpassFilter[ChannelIndex, BandIndex].Order    := Round(Value);
    if Assigned(FSourceHighpassFilter[ChannelIndex, BandIndex])
     then FSourceHighpassFilter[ChannelIndex, BandIndex].Order   := Round(Value);
    if Assigned(FSplitterHighpassFilter[ChannelIndex, BandIndex])
     then FSplitterHighpassFilter[ChannelIndex, BandIndex].Order := Round(Value);
   end;

 InvertMix;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateOrder;
end;

procedure TExciterDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 Assert(Value > 0);
 if Assigned(FChebyshevWaveshaper) then
  begin
   FChebyshevWaveshaper.Order := Round(min(22000, 0.48 * SampleRate) / Value + 0.5);
   for ChannelIndex := 0 to numInputs - 1 do
    for BandIndex := 0 to 1 do
     begin
      FSourceLowpassFilter[ChannelIndex, BandIndex].Frequency    := Value;
      FSourceHighpassFilter[ChannelIndex, BandIndex].Frequency   := Value;
      FSplitterHighpassFilter[ChannelIndex, BandIndex].Frequency := Value;
     end;
  end;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateTune;
end;

procedure TExciterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 if Abs(SampleRate) > 0 then
  begin
   for ChannelIndex := 0 to numInputs - 1 do
    for BandIndex := 0 to 1 do
     begin
      if Assigned(FSourceLowpassFilter[ChannelIndex, BandIndex])
       then FSourceLowpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
      if Assigned(FSourceHighpassFilter[ChannelIndex, BandIndex])
       then FSourceHighpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
      if Assigned(FSplitterHighpassFilter[ChannelIndex, BandIndex])
       then FSplitterHighpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
     end;
   if Assigned(FChebyshevWaveshaper)
    then FChebyshevWaveshaper.Order := Round(Min(22000, 0.48 * Abs(SampleRate)) / ParameterByName['Tune'] + 0.5);
  end;
end;

procedure TExciterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample    : Integer;
  Channel   : Integer;
  Input     : Double;
  Source    : Double;
  Low, High : Double;
const
  cDenorm = 1E-31;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to 1 do
   begin
    Input  := cDenorm + Inputs[Channel, Sample];
    Low    := FSourceLowpassFilter[Channel, 1].ProcessSample64(
              FSourceLowpassFilter[Channel, 0].ProcessSample64(Input));
    Source := FChebyshevWaveshaper.ProcessSample64(FOverdriveGain * Low);
    Source := FSourceHighpassFilter[Channel, 1].ProcessSample64(
              FSourceHighpassFilter[Channel, 0].ProcessSample64(Source));

    High  := FSplitterHighpassFilter[Channel, 1].ProcessSample64(
             FSplitterHighpassFilter[Channel, 0].ProcessSample64(Input));

    Outputs[Channel, Sample] := Low + FMix[0] * High + FMix[1] * Source;
  end;
end;

procedure TExciterDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  Sample    : Integer;
  Channel   : Integer;
  Input      : Double;
  Source     : Double;
  Low, High  : Double;
const
  cDenorm = 1E-31;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to 1 do
   begin
    Input  := cDenorm + Inputs[Channel, Sample];
    Low    := FSourceLowpassFilter[Channel, 1].ProcessSample64(
              FSourceLowpassFilter[Channel, 0].ProcessSample64(Input));
    Source := FChebyshevWaveshaper.ProcessSample64(FOverdriveGain * Low);
    Source := FSourceHighpassFilter[Channel, 1].ProcessSample64(
              FSourceHighpassFilter[Channel, 0].ProcessSample64(cDenorm + Source));

    High  := FSplitterHighpassFilter[Channel, 1].ProcessSample64(
             FSplitterHighpassFilter[Channel, 0].ProcessSample64(Input));

    Outputs[Channel, Sample] := Low + FMix[0] * High + FMix[1] * Source;
  end;
end;

end.
