unit SimpleCompressorDM;

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
  DAV_DspDynamics;

type
  TSimpleCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FSimpleCompressors : Array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TSimpleCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleCompressors[0] := TSimpleRMSCompressor.Create;
 FSimpleCompressors[1] := TSimpleRMSCompressor.Create;
end;

procedure TSimpleCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleCompressors[0]);
 FreeAndNil(FSimpleCompressors[1]);
end;

procedure TSimpleCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Threshold_dB := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSimpleCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Ratio := 1 / Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Ratio := 1 / Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSimpleCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Release := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSimpleCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Attack := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
    Outputs[0, i] := FSimpleCompressors[0].ProcessSample64(Inputs[0, i]);
    Outputs[1, i] := FSimpleCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSimpleCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].SampleRate := SampleRate;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].SampleRate := SampleRate;
end;

end.
