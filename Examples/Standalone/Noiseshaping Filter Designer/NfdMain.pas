unit NfdMain;

interface

{$I DAV_Compiler.inc}
{$DEFINE Use_IPPS}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, DAV_Common, DAV_Complex, DAV_DifferentialEvolution,
  DAV_DspDitherNoiseshaper, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TFmNoiseshapingFilterDesigner = class(TForm)
    SECoefficientCount: TSpinEdit;
    LbCoefficients: TLabel;
    Memo: TMemo;
    BtCalculate: TButton;
    SeSampleRate: TSpinEdit;
    LbSampleRate: TLabel;
    LbFrequency: TLabel;
    SeFrequency: TSpinEdit;
    LbSampleRateUnit: TLabel;
    LbFrequencyUnit: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtCalculateClick(Sender: TObject);
    procedure SECoefficientCountChange(Sender: TObject);
  private
    FDiffEvol  : TDifferentialEvolution;
    FFFT       : TFftReal2Complex;
    FImpResp   : PDAVSingleFixedArray;
    FFreqResp  : PDAVComplexSingleFixedArray;
    FMagnitude : PDAVSingleFixedArray;
    function DiffEvolCalcCosts(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
  public
    function DiffEvolInitPopulation(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
  end;

var
  FmNoiseshapingFilterDesigner: TFmNoiseshapingFilterDesigner;

implementation

{$R *.dfm}

uses
  Math;

procedure TFmNoiseshapingFilterDesigner.FormCreate(Sender: TObject);
begin
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(9);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(9);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(9);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}

 // impulse response buffer
 GetMem(FImpResp, FFft.FFTSize * SizeOf(Single));
 FillChar(FImpResp^[0], FFft.FFTSize * SizeOf(Single), 0);

 // frequency response buffer
 GetMem(FFreqResp, FFft.BinCount * SizeOf(TComplexSingle));
 FillChar(FFreqResp^[0], FFft.BinCount * SizeOf(TComplexSingle), 0);

 // magnitude buffer
 GetMem(FMagnitude, FFft.BinCount * SizeOf(Single));
 FillChar(FMagnitude^[0], FFft.BinCount * SizeOf(Single), 0);

 FDiffEvol := TDifferentialEvolution.Create(Self);
 with FDiffEvol do
  begin
   PopulationCount := 500;
   VariableCount := SECoefficientCount.Value;
   GainBest := 0.3;
   GainR1 := -0.6;
   GainR2 := 0.6;
   GainR3 := 1;
   CrossOver := 1;
   AutoInitialize := False;
   OnCalcCosts := DiffEvolCalcCosts;
  end;
end;

procedure TFmNoiseshapingFilterDesigner.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FDiffEvol);
 Dispose(FImpResp);
 Dispose(FFreqResp);
 FreeAndNil(FFft);
end;

procedure TFmNoiseshapingFilterDesigner.SECoefficientCountChange(
  Sender: TObject);
begin
 FDiffEvol.VariableCount := SECoefficientCount.Value;
end;

procedure TFmNoiseshapingFilterDesigner.BtCalculateClick(Sender: TObject);
var
  VariableNo     : Integer;
  EvolveCounter  : Integer;
  BestPopulation : TDifferentialEvolutionPopulation;
begin
 BtCalculate.Tag := 1 - BtCalculate.Tag;
 SECoefficientCount.Enabled := BtCalculate.Tag = 0;
 SeFrequency.Enabled := BtCalculate.Tag = 0;
 SeSampleRate.Enabled := BtCalculate.Tag = 0;

 if BtCalculate.Tag = 1 then
  begin
   BtCalculate.Caption := 'Stop!';
   for VariableNo := 0 to FDiffEvol.VariableCount - 1 do
    begin
     FDiffEvol.MinArr[VariableNo] := -4;
     FDiffEvol.MaxArr[VariableNo] :=  4;
    end;

   EvolveCounter := 0;
   FDiffEvol.Initialize;

   while (BtCalculate.Tag = 1) and not Application.Terminated do
    begin
     FDiffEvol.Evolve;
     if EvolveCounter mod 8 = 0 then
      begin
       Memo.Clear;
       Memo.Lines.Add('Costs: ' + FloatToStr(FDiffEvol.GetBestCost));
       Memo.Lines.Add('Coefficients:');
       BestPopulation := FDiffEvol.GetBestPopulation;
       for VariableNo := 0 to FDiffEvol.VariableCount - 1
        do Memo.Lines.Add(FloatToStr(BestPopulation[VariableNo]));
      end;
     inc(EvolveCounter);
     sleep(10);
     Application.ProcessMessages;
    end;
  end else BtCalculate.Caption := 'Calculate';
end;

function TFmNoiseshapingFilterDesigner.DiffEvolInitPopulation(Sender: TObject;
  const Population: TDifferentialEvolutionPopulation): Double;
var
  i : Integer;
begin
 if Length(Population) = FDiffEvol.VariableCount then
  for i := 0 to FDiffEvol.VariableCount - 1
   do Population[i] := 8 * random - 4;
 result := 1000;  
end;

function TFmNoiseshapingFilterDesigner.DiffEvolCalcCosts(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
var
  VarNo        : Integer;
  BinNo        : Integer;
  Above, Below : Single;
  BinBandWidth : Single;
  Freq, Max    : Single;
begin
 // render FIR filter
 FImpResp[0] := 1;
 for VarNo := 0 to Length(Population) - 1
  do FImpResp[1 + VarNo] := -Population[VarNo];
 FFFT.PerformFFT(FFreqResp, FImpResp);

 // calculate magnitude
 FMagnitude[0] := Amp_to_dB(CDenorm32 + abs(FFreqResp[0].Re));
 for BinNo := 1 to FFFT.BinCount - 2 do
  begin
(*
   assert(FFreqResp[BinNo].Re <> 0);
   assert(FFreqResp[BinNo].Im <> 0);
*)
   FMagnitude[BinNo] := SqrAmp2dB(CDenorm32 + sqr(FFreqResp[BinNo].Re) + sqr(FFreqResp[BinNo].Im));
  end;
 FMagnitude[FFFT.BinCount - 1] := Amp_to_dB(CDenorm32 + abs(FFreqResp[FFFT.BinCount - 1].Re));

 // evaluate magnitude
 Above := 0;
 Below := 0;
 Result := 0;
 Max := -1000;
 Freq := SeFrequency.Value;
 BinBandWidth := SeSampleRate.Value / FFFT.FFTSize;
 for BinNo := 0 to FFFT.BinCount - 1 do
  begin
   if FMagnitude[BinNo] > 0 then Above := Above + FMagnitude[BinNo] else
   if FMagnitude[BinNo] < 0 then Below := Below - FMagnitude[BinNo];
   if BinNo * BinBandWidth < Freq then
    begin
     Result := Result + FMagnitude[BinNo];
     if FMagnitude[BinNo] > Max then Max := FMagnitude[BinNo];
    end;
  end;

 Result := ((Result / round(Freq / BinBandWidth)) + 2 * Max) + 10 * abs(Above - Below);
end;

end.
