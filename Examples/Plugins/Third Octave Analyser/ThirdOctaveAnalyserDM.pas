unit ThirdOctaveAnalyserDM;

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

{$I DAV_Compiler.Inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilterChebyshevType1;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);
  CDS = 8;
  CBW = 0.4;

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    RMS          : Double;
  end;

  TThirdOctaveAnalyserModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessNormal(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDownsampled(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterSmoothChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFullscaleGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDownsamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDownsamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FUseDownsampling: Boolean;
    function GetBandReserve: Single;
    function GetBandRMS(Index: Integer): Single;
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
    procedure CalculateSmoothingFactor;
  protected
    FMaxDSStages     : Integer;
    FDownSampleCount : Integer;
    FDownSampleMax   : Integer;
    FBandReserve     : Double;

    FFilterArray     : array [0..cNumFrequencies - 1] of TDownsampleFilterRecord;
    FFSGain          : Single;
    FSpeedConst      : array [0..1] of Single;
//    FDSSpeedConsts   : array [0..cNumFrequencies - 1] of Single;
    procedure UpdateFilters; virtual;
    procedure DownsamplingChanged; virtual;
  public
    property BandReserve: Single read GetBandReserve write SetBandReserve;
    property UseDownsampling: Boolean read FUseDownsampling write SetUseDownsampling default True;
    property BandRMS[Index: Integer]: Single read GetBandRMS;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Common, DAV_VSTCustomModule, ThirdOctaveAnalyserGUI;

procedure TThirdOctaveAnalyserModule.VSTModuleOpen(Sender: TObject);
begin
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
 FFSGain := 0;
 FBandReserve := 0.25;
 UpdateFilters;

 UseDownsampling := True;
 DownsamplingChanged;

 // initialize parameters
 Parameter[0] := 0.99;
 Parameter[1] := 90;
 Parameter[2] := 1;

 with Programs[0] do
  begin
   Parameter[0] := 0.99;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 0.999;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 0.9999;
   Parameter[1] := 90;
   Parameter[2] := 1;
  end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleClose(Sender: TObject);
var
  Band : Integer;
begin
 // free filters
 for Band := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FFilterArray[Band].Lowpass);
   FreeAndNil(FFilterArray[Band].Highpass);
  end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmThirdOctaveAnalyser.Create(Self);
end;

function TThirdOctaveAnalyserModule.GetBandReserve: Single;
begin
 Result := 100 * FBandReserve;
end;

function TThirdOctaveAnalyserModule.GetBandRMS(Index: Integer): Single;
begin
 if Index in [0..CNumFrequencies - 1]
  then Result := FFilterArray[Index].RMS
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TThirdOctaveAnalyserModule.SetBandReserve(const Value: Single);
begin
 FBandReserve := 0.01 * Value;
end;

procedure TThirdOctaveAnalyserModule.SetUseDownsampling(const Value: Boolean);
begin
 if FUseDownsampling <> Value then
  begin
   FUseDownsampling := Value;
   DownsamplingChanged;
  end;
end;

procedure TThirdOctaveAnalyserModule.ParameterDownsamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 UseDownsampling := Value > 0.5;
end;

procedure TThirdOctaveAnalyserModule.ParameterDownsamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TThirdOctaveAnalyserModule.ParameterFullscaleGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFSGain := Value;

 // update GUI
 if EditorForm is TFmThirdOctaveAnalyser
  then TFmThirdOctaveAnalyser(EditorForm).UpdateFullscaleGain;
end;

procedure TThirdOctaveAnalyserModule.ParameterSmoothChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSpeedConst[0] := 0.01 * Value;
 CalculateSmoothingFactor;
end;

procedure TThirdOctaveAnalyserModule.CalculateSmoothingFactor;
begin
 FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TThirdOctaveAnalyserModule.UpdateFilters;
var
  Band         : Integer;
  Downsampling : Integer;
  DesiredFreq  : Double;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 Downsampling := 0;

 for Band := 0 to Length(FFilterArray) - 1 do
  begin
   // Lowpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * SampleRate then DesiredFreq := 0.499 * SampleRate;

   if UseDownsampling then
    while ((2 * DesiredFreq / Self.SampleRate) * (1 shl Downsampling)) < FBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(10);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(12);
    
   with FFilterArray[Band].Highpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
  end;
 FDownSampleMax := 1 shl Downsampling;
end;

procedure TThirdOctaveAnalyserModule.DownsamplingChanged;
begin
 if FUseDownsampling
  then FDownSampleCount := 0
  else FDownSampleCount := -1;

 if FDownSampleCount = -1
  then OnProcess := VSTModuleProcessNormal
  else OnProcess := VSTModuleProcessDownSampled;

 OnProcessReplacing := OnProcess;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleProcessNormal(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Band   : Integer;
  Temp, Filtered : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Temp := Inputs[0, Sample];
   for Band := 0 to CNumFrequencies - 1 do
    begin
     Temp := FFilterArray[Band].Lowpass.ProcessSample64(Temp + CDenorm32);
     Filtered := FFilterArray[Band].Highpass.ProcessSample64(Temp + CDenorm32);
     FFilterArray[Band].RMS := FSpeedConst[0] * FFilterArray[Band].RMS + FSpeedConst[1] * Amp_to_dB(abs(Filtered));
    end;

   if numOutputs > 0 then Outputs[0, Sample] := Inputs[0, Sample];
  end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to CNumFrequencies - 1 do
  begin
   if Assigned(FFilterArray[BandIndex].Lowpass)
    then FFilterArray[BandIndex].Lowpass.SampleRate := SampleRate;
   if Assigned(FFilterArray[BandIndex].Highpass)
    then FFilterArray[BandIndex].Highpass.SampleRate := SampleRate;
  end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleProcessDownsampled(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Band      : Integer;
  Temp, Filtered, s : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Temp := Inputs[0, Sample];
   for Band := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[Band].Downsampling) <> 0
      then Break;

     Temp := FFilterArray[Band].Lowpass.ProcessSample64(Temp + CDenorm32);
     Filtered := FFilterArray[Band].Highpass.ProcessSample64(Temp + CDenorm32);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[Band].Downsampling + 1);
     FFilterArray[Band].RMS := s * FFilterArray[Band].RMS + (1 - s) * Amp_to_dB(abs(Filtered));
    end;
   Inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;

   if numOutputs > 0 then Outputs[0, Sample] := Inputs[0, Sample];
  end;
end;

end.