unit GateDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TGateDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SGDMThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FGates : array [0..1] of TClassicGate;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TGateDataModule.VSTModuleOpen(Sender: TObject);
begin
 FGates[0] := TClassicGate.Create;
 FGates[1] := TClassicGate.Create;

 Parameter[0] := -10;
end;

procedure TGateDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FGates[0]);
 FreeAndNil(FGates[1]);
end;

procedure TGateDataModule.SGDMThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGates[0]) then FGates[0].Threshold_dB := Value;
 if Assigned(FGates[1]) then FGates[1].Threshold_dB := Value;
end;

procedure TGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FGates[0].ProcessSample64(Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FGates[1].ProcessSample64(Inputs[1, SampleIndex]);
  end;
end;

procedure TGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FGates[0]) then FGates[0].SampleRate := SampleRate;
 if Assigned(FGates[1]) then FGates[1].SampleRate := SampleRate;
end;

end.
