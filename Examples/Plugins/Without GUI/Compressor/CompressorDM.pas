unit CompressorDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCompressors : array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FCompressors[0] := TSimpleRMSCompressor.Create;
 FCompressors[1] := TSimpleRMSCompressor.Create;
end;

procedure TCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCompressors[0]);
 FreeAndNil(FCompressors[1]);
end;

procedure TCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressors[0])
  then FCompressors[0].Threshold_dB := Value;
 if Assigned(FCompressors[1])
  then FCompressors[1].Threshold_dB := Value;
end;

procedure TCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressors[0])
  then FCompressors[0].Ratio := 1 / Value;
 if Assigned(FCompressors[1])
  then FCompressors[1].Ratio := 1 / Value;
end;

procedure TCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressors[0])
  then FCompressors[0].Release := Value;
 if Assigned(FCompressors[1])
  then FCompressors[1].Release := Value;
end;

procedure TCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressors[0])
  then FCompressors[0].Attack := Value;
 if Assigned(FCompressors[1])
  then FCompressors[1].Attack := Value;
end;

procedure TCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FCompressors[0].ProcessSample64(Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FCompressors[1].ProcessSample64(Inputs[1, SampleIndex]);
  end;
end;

procedure TCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FCompressors[0])
    then FCompressors[0].SampleRate := Abs(SampleRate);
   if Assigned(FCompressors[1])
    then FCompressors[1].SampleRate := Abs(SampleRate);
  end;
end;

end.
