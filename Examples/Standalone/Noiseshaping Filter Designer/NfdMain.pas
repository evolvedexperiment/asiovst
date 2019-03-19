{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit NfdMain;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, DAV_Types, DAV_Complex,
  DAV_DifferentialEvolution, DAV_DspDitherNoiseshaper, DAV_DspFftReal2Complex
{$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TFormNoiseshapingFilterDesigner = class(TForm)
    ButtonCalculate: TButton;
    LabelCoefficients: TLabel;
    LabelFrequency: TLabel;
    LabelFrequencyUnit: TLabel;
    LabelSampleRate: TLabel;
    LabelSampleRateUnit: TLabel;
    Memo: TMemo;
    SpinEditCoefficientCount: TSpinEdit;
    SpinEditFrequency: TSpinEdit;
    SpinEditSampleRate: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCalculateClick(Sender: TObject);
    procedure SpinEditCoefficientCountChange(Sender: TObject);
  private
    FDiffEvol: TDifferentialEvolution;
    FFFT: TFftReal2Complex;
    FImpResp: PDAVSingleFixedArray;
    FFreqResp: PDAVComplexSingleFixedArray;
    FMagnitude: PDAVSingleFixedArray;
    function DiffEvolCalcSharpCosts(Sender: TObject;
      const Population: TDifferentialEvolutionPopulation): Double;
    function DiffEvolCalcSoftCosts(Sender: TObject;
      var Population: TDifferentialEvolutionPopulation): Double;
    procedure RenderImpulseResponse(const Population
      : TDifferentialEvolutionPopulation);
    procedure CalculateMagnitude;
  public
    function DiffEvolInitPopulation(Sender: TObject;
      const Population: TDifferentialEvolutionPopulation): Double;
  end;

var
  FormNoiseshapingFilterDesigner: TFormNoiseshapingFilterDesigner;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  Math, DAV_Common;

procedure TFormNoiseshapingFilterDesigner.FormCreate(Sender: TObject);
begin
{$IFDEF Use_IPPS}
  FFFT := TFftReal2ComplexIPPSFloat32.Create(9);
{$ELSE} {$IFDEF Use_CUDA}
  FFFT := TFftReal2ComplexCUDA32.Create(9);
{$ELSE}
  FFFT := TFftReal2ComplexNativeFloat32.Create(9);
  FFFT.DataOrder := doPackedComplex;
{$ENDIF}{$ENDIF}
  // impulse response buffer
  GetMem(FImpResp, FFFT.FFTSize * SizeOf(Single));
  FillChar(FImpResp^[0], FFFT.FFTSize * SizeOf(Single), 0);

  // frequency response buffer
  GetMem(FFreqResp, FFFT.BinCount * SizeOf(TComplex32));
  FillChar(FFreqResp^[0], FFFT.BinCount * SizeOf(TComplex32), 0);

  // magnitude buffer
  GetMem(FMagnitude, FFFT.BinCount * SizeOf(Single));
  FillChar(FMagnitude^[0], FFFT.BinCount * SizeOf(Single), 0);

  FDiffEvol := TDifferentialEvolution.Create(Self);
  with FDiffEvol do
  begin
    PopulationCount := 500;
    VariableCount := SpinEditCoefficientCount.Value;
    GainBest := 0.3;
    GainR1 := -0.6;
    GainR2 := 0.6;
    GainR3 := 1;
    CrossOver := 1;
    AutoInitialize := False;
    OnCalculateCosts := DiffEvolCalcSoftCosts;
  end;
end;

procedure TFormNoiseshapingFilterDesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDiffEvol);
  Dispose(FImpResp);
  Dispose(FFreqResp);
  FreeAndNil(FFFT);
end;

procedure TFormNoiseshapingFilterDesigner.SpinEditCoefficientCountChange
  (Sender: TObject);
begin
  FDiffEvol.VariableCount := SpinEditCoefficientCount.Value;
end;

procedure TFormNoiseshapingFilterDesigner.ButtonCalculateClick(Sender: TObject);
var
  VariableNo: Integer;
  EvolveCounter: Integer;
  BestPopulation: TDifferentialEvolutionPopulation;
begin
  ButtonCalculate.Tag := 1 - ButtonCalculate.Tag;
  SpinEditCoefficientCount.Enabled := ButtonCalculate.Tag = 0;
  SpinEditFrequency.Enabled := ButtonCalculate.Tag = 0;
  SpinEditSampleRate.Enabled := ButtonCalculate.Tag = 0;

  if ButtonCalculate.Tag = 1 then
  begin
    ButtonCalculate.Caption := 'Stop!';
    for VariableNo := 0 to FDiffEvol.VariableCount - 1 do
    begin
      FDiffEvol.MinConstraints[VariableNo] := -4;
      FDiffEvol.MaxConstraints[VariableNo] := 4;
    end;

    EvolveCounter := 0;
    FDiffEvol.Initialize;

    while (ButtonCalculate.Tag = 1) and not Application.Terminated do
    begin
      FDiffEvol.Evolve;
      if EvolveCounter mod 8 = 0 then
      begin
        Memo.Clear;
        Memo.Lines.Add('Costs: ' + FloatToStr(FDiffEvol.GetBestCost));
        Memo.Lines.Add('Coefficients:');
        BestPopulation := FDiffEvol.GetBestPopulation;
        for VariableNo := 0 to FDiffEvol.VariableCount - 1 do
          Memo.Lines.Add(FloatToStr(BestPopulation[VariableNo]));
      end;
      inc(EvolveCounter);
      // sleep(10);
      Application.ProcessMessages;
    end;
  end
  else
    ButtonCalculate.Caption := 'Calculate';
end;

function TFormNoiseshapingFilterDesigner.DiffEvolInitPopulation(Sender: TObject;
  const Population: TDifferentialEvolutionPopulation): Double;
var
  i: Integer;
begin
  if Length(Population) = FDiffEvol.VariableCount then
    for i := 0 to FDiffEvol.VariableCount - 1 do
      Population[i] := 8 * random - 4;
  Result := 1000;
end;

function AbsoluteThresholdOfHearing(Frequency: Single;
  HighFrequencyModification: Single): Single;
(* from Painter & Spanias
  modified by Gabriel Bouvigne to better fit the reality

  ath =    3.640 * pow(f,-0.8)
  - 6.800 * exp(-0.6*pow(f-3.4,2.0))
  + 6.000 * exp(-0.15*pow(f-8.7,2.0))
  + 0.6 * 0.001 * pow(f,4.0);

  In the past LAME was using the Painter & Spanias formula. But we had some
  recurrent problems with HF content. We measured real ATH values, and found
  the older formula to be inacurate in the higher part. So we made this new
  formula and this solved most of HF problematic testcases. The tradeoff is
  that in VBR mode it increases a lot the bitrate. *)
begin
  (*
    This curve can be adjusted according to the VBR scale:
    it adjusts from something close to Painter & Spanias
    on V9 up to Bouvigne's formula for V0. This way the VBR
    bitrate is more balanced according to the -V value.
  *)

  (* the following hack allows to ask for the lowest value *)
  if (Frequency < -0.3) then
    Frequency := 3410;

  Frequency := Frequency / 1000; // convert to kHz
  Frequency := Max(0.1, Frequency); // clip value for lower frequency (100 Hz)
  // Frequency := Min(21.0, Frequency);

  Result := 3.640 * Power(Frequency, -0.8) - 6.800 *
    Exp(-0.6 * Power(Frequency - 3.4, 2.0)) + 6.000 *
    Exp(-0.15 * Power(Frequency - 8.7, 2.0)) +
    (0.6 + 0.04 * HighFrequencyModification) * 0.001 * Power(Frequency, 4.0);
end;

procedure TFormNoiseshapingFilterDesigner.RenderImpulseResponse(const Population
  : TDifferentialEvolutionPopulation);
var
  VarNo: Integer;
begin
  // render FIR filter
  FImpResp[0] := 1;
  for VarNo := 0 to Length(Population) - 1 do
    FImpResp[1 + VarNo] := -Population[VarNo];
  FFFT.PerformFFT(FFreqResp, FImpResp);
end;

procedure TFormNoiseshapingFilterDesigner.CalculateMagnitude;
var
  BinNo: Integer;
begin
  // calculate magnitude
  FMagnitude[0] := Amp_to_dB(CDenorm32 + abs(FFreqResp[0].Re));
  for BinNo := 1 to FFFT.BinCount - 2 do
    FMagnitude[BinNo] := SqrAmp2dB(CDenorm32 + sqr(FFreqResp[BinNo].Re) +
      sqr(FFreqResp[BinNo].Im));
  FMagnitude[FFFT.BinCount - 1] :=
    Amp_to_dB(CDenorm32 + abs(FFreqResp[FFFT.BinCount - 1].Re));
end;

function TFormNoiseshapingFilterDesigner.DiffEvolCalcSharpCosts(Sender: TObject;
  const Population: TDifferentialEvolutionPopulation): Double;
var
  BinNo: Integer;
  Above, Below: Single;
  RelFrq, Avg: Single;
  Min, Max: Single;
begin
  // render FIR filter
  RenderImpulseResponse(Population);

  // calculate magnitude
  CalculateMagnitude;

  // evaluate magnitude
  Above := 0;
  Below := 0;
  Result := 0;
  Min := 1000;
  Max := -1000;
  RelFrq := SpinEditFrequency.Value * FFFT.FFTSize / SpinEditSampleRate.Value;
  for BinNo := 0 to FFFT.BinCount - 1 do
  begin
    if FMagnitude[BinNo] > 0 then
      Above := Above + FMagnitude[BinNo]
    else if FMagnitude[BinNo] < 0 then
      Below := Below - FMagnitude[BinNo];
    if BinNo < RelFrq then
    begin
      Result := Result + FMagnitude[BinNo];
      if FMagnitude[BinNo] > Max then
        Max := FMagnitude[BinNo];
      if FMagnitude[BinNo] < Min then
        Min := FMagnitude[BinNo];
    end;
  end;
  Avg := Result / Round(RelFrq);
  Result := 2 * Avg + 2 * Max - Min + abs(Above - Below);
end;

function TFormNoiseshapingFilterDesigner.DiffEvolCalcSoftCosts(Sender: TObject;
  var Population: TDifferentialEvolutionPopulation): Double;
var
  BinNo, Coeff: Integer;
  Above, Below: Single;
  RelFrq, Avg: Single;
  Min, Max: Single;
  Weights: Single;
  MaxCoeff: Single;
begin
  // render FIR filter
  RenderImpulseResponse(Population);

  // calculate magnitude
  CalculateMagnitude;

  // evaluate magnitude
  Above := 0;
  Below := 0;
  Result := 0;
  Min := 1000;
  Max := -1000;
  Weights := 0;
  RelFrq := SpinEditFrequency.Value * FFFT.FFTSize / SpinEditSampleRate.Value;
  for BinNo := 0 to FFFT.BinCount - 1 do
  begin
    if FMagnitude[BinNo] > 0 then
      Above := Above + FMagnitude[BinNo]
    else if FMagnitude[BinNo] < 0 then
      Below := Below - FMagnitude[BinNo];
    if BinNo < RelFrq then
    begin
      if BinNo > 0.8 * RelFrq then
      begin
        Result := Result + 0.7 * FMagnitude[BinNo];
        Weights := Weights + 0.7;
        if (FMagnitude[BinNo] > Max) and (BinNo < 0.9 * RelFrq) then
          Max := FMagnitude[BinNo];
      end
      else
      begin
        Result := Result + FMagnitude[BinNo];
        Weights := Weights + 1;
        if FMagnitude[BinNo] > Max then
          Max := FMagnitude[BinNo];
      end;
      if FMagnitude[BinNo] < Min then
        Min := FMagnitude[BinNo];
    end;
  end;

  Avg := Result / Weights;
  Result := 2 * Avg + 1.7 * Max - Min + abs(Above - Below);

  // penalties
  MaxCoeff := 0;
  for Coeff := 0 to Length(Population) - 1 do
    if abs(Population[Coeff]) > MaxCoeff then
      MaxCoeff := abs(Population[Coeff]);
  if MaxCoeff > 6 then
    Result := 1000 + Result;

  for Coeff := 1 to Length(Population) - 1 do
    if abs(Population[Coeff - 1]) < abs(Population[Coeff]) then
      Result := Result + 10 * abs(abs(Population[Coeff - 1]) -
        abs(Population[Coeff]));
end;

end.
