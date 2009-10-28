unit BugpassLiteDM;

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

{$I DAV_Compiler.INC}
{.$DEFINE Use_IPPS}
{.$DEFINE Use_CUDA}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_Complex,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TBugpassLiteDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFreqLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqHighChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilterKernel : PDAVSingleFixedArray;
    FSignalPadded : PDAVSingleFixedArray;
    FFilterFreq   : PDAVComplexSingleFixedArray;
    FSignalFreq   : PDAVComplexSingleFixedArray;
    FSemaphore    : Integer;
    {$IFDEF Use_IPPS}
    FFft          : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft          : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft          : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure CalculateFilterKernel;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_DspWindowing, BugpassLiteGUI;

procedure TBugpassLiteDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore       := 0;
 FFilterKernel    := nil;
 FSignalPadded    := nil;
 FFilterFreq      := nil;
 FSignalFreq      := nil;
 BlockModeOverlap := BlockModeSize div 2;
end;

procedure TBugpassLiteDataModule.VSTModuleOpen(Sender: TObject);
begin
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 ReallocMem(FSignalFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 FillChar(FFilterFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FSignalFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq, BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq, BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq, BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq, BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 ReallocMem(FFilterKernel, BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalPadded, BlockModeSize * SizeOf(Single));
 FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
 FFft.DataOrder := doPackedComplex;

 // Parameters and Programs
 Parameter[0] := 100;
 Parameter[1] := 16000;
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);

 // finally calculate the first filter kernel
 CalculateFilterKernel;
end;

procedure TBugpassLiteDataModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);
 FreeAndNil(FFft);
end;

procedure TBugpassLiteDataModule.ParamFreqLowChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;
 if EditorForm is TFmBugpassLite then
  with TFmBugpassLite(EditorForm)
   do UpdateFrequencyBar;
end;

procedure TBugpassLiteDataModule.ParamFreqHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;
 if EditorForm is TFmBugpassLite then
  with TFmBugpassLite(EditorForm)
   do UpdateFrequencyBar;
end;

procedure TBugpassLiteDataModule.CalculateFilterKernel;
var
  i, h, q : Integer;
  n       : Double;
  CutOff  : array [0..1] of Double;
begin
 if assigned(FFilterKernel) and assigned(FFilterFreq) and assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    CutOff[0] := Parameter[0] / SampleRate;
    CutOff[1] := Parameter[1] / SampleRate;
    h := BlockModeSize div 2;
    q := BlockModeSize div 4;

    // Generate sinc delayed by (N-1)/2
    for i := 0 to h - 1 do
     if (i = q)
      then FFilterKernel^[i] := 2.0 * (CutOff[0] - CutOff[1])
      else
       begin
        n := PI * (i - q);
        FFilterKernel^[i] := (sin(2.0 * CutOff[0] * n) - sin(2.0 * CutOff[1] * n)) / n;
       end;
    ApplyBlackmanWindow(FFilterKernel, h);
    FillChar(FFilterKernel^[h], h * SizeOf(Single), 0);

    // calculate frequency
    {$IFDEF Use_IPPS}
    FFft.PerformFFTCCS(FFilterFreq, FFilterKernel);
    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFT(FFilterFreq, FFilterKernel);
    {$ELSE}
    case FFft.DataOrder of
     doPackedRealImaginary : FFft.PerformFFTPackedReIm(PDAVSingleFixedArray(FFilterFreq), FFilterKernel);
           doPackedComplex : FFft.PerformFFTPackedComplex(FFilterFreq, FFilterKernel);
     else raise Exception.Create('not supported');
    end;
    {$ENDIF}{$ENDIF}
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TBugpassLiteDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmBugpassLite.Create(Self);
end;

procedure TBugpassLiteDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Bin     : Integer;
  Half    : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  Half := BlockModeSize div 2;
  for Channel := 0 to numOutputs - 1 do
   begin
    {$IFDEF Use_IPPS}
    FFft.PerformFFT(PDAVComplexSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

    // DC & Nyquist
    FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
    FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

    for Bin := 1 to Half - 1
     do ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

    FFft.PerformIFFT(PDAVComplexSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);

    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFT(FSignalFreq, @Inputs[Channel, 0]);

    // DC & Nyquist
    FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
    FSignalFreq^[0].Im := FFilterFreq^[0].Im * FSignalFreq^[0].Im;
    FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

    for Bin := 1 to Half - 1
     do ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

    FFft.PerformIFFT(FSignalFreq, @Outputs[Channel, 0]);
    {$ELSE}
    case FFft.DataOrder of
     doPackedRealImaginary :
      begin
       FFft.PerformFFTPackedReIm(PDAVSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

       // DC
       Bin := 0;
       PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
         PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];
       Inc(Bin);

       // inbetween...
       while Bin < Half do
        begin
         ComplexMultiplyInplace(
           PDAVSingleFixedArray(FSignalFreq)^[Bin],
           PDAVSingleFixedArray(FSignalFreq)^[Bin + Half],
           PDAVSingleFixedArray(FFilterFreq)^[Bin],
           PDAVSingleFixedArray(FFilterFreq)^[Bin + Half]);
         Inc(Bin);
        end;

       // Nyquist
       PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
         PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];

       FFft.PerformIFFTPackedReIm(PDAVSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);
      end;
      doPackedComplex :
       begin
        FFft.PerformFFTPackedComplex(FSignalFreq, @Inputs[Channel, 0]);

        // DC & Nyquist
        FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
        FSignalFreq^[0].Im := FFilterFreq^[Half].Im * FSignalFreq^[Half].Im;

        for Bin := 1 to Half - 1
         do ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

        FFft.PerformIFFTPackedComplex(FSignalFreq, @Outputs[Channel, 0]);
       end
     else raise Exception.Create('not supported');
    end;
    {$ENDIF}{$ENDIF}
   end;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TBugpassLiteDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

end.
