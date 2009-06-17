unit ThirdOctaveAnalyserGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  ExtCtrls, TeEngine, Series, Controls, TeeProcs, Chart, StdCtrls, Spin;

type
  TFmThirdOctaveAnalyser = class(TForm)
    LbSpeed: TLabel;
    LbFullscale: TLabel;
    Lb_dB: TLabel;
    RB_Fast: TRadioButton;
    RB_Medium: TRadioButton;
    RB_Slow: TRadioButton;
    SEFullscaleGain: TSpinEdit;
    AnalyserChart: TChart;
    BarSeries: TBarSeries;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFSGain : Single;  
  public
    procedure UpdateFullscaleGain;
  end;

implementation

{$R *.DFM}

uses
  ThirdOctaveAnalyserDM;

procedure TFmThirdOctaveAnalyser.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 // add bands
 for Band := 0 to CNumFrequencies - 1 do
  begin
   {$IFNDEF FPC}
   if CThirdOctaveFrequencies[Band] < 1000
    then BarSeries.Add(0,FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else BarSeries.Add(0,FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
   {$ELSE}
   if Frequency < 1000
    then AnalyserChart.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
    else AnalyserChart.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;
end;

procedure TFmThirdOctaveAnalyser.RB_FastClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.99;
  end;
end;

procedure TFmThirdOctaveAnalyser.RB_MediumClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.999;
  end;
end;

procedure TFmThirdOctaveAnalyser.RB_SlowClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.9999;
  end;
end;

procedure TFmThirdOctaveAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[1] := SEFullscaleGain.Value;
  end;
end;

procedure TFmThirdOctaveAnalyser.UpdateFullscaleGain;
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   if SEFullscaleGain.Value <> round(Parameter[1])
    then SEFullscaleGain.Value := round(Parameter[1]);
   FFSGain := Parameter[1];
  end;
end;

procedure TFmThirdOctaveAnalyser.TimerTimer(Sender: TObject);
var
  Band : Integer;
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   {$IFNDEF FPC}
   for Band := 0 to cNumFrequencies - 1
    do BarSeries.YValue[Band] := BandRMS[cNumFrequencies - Band - 1] + FFSGain;
   AnalyserChart.Invalidate;
   {$ELSE}
   for j := 0 to cNumFrequencies - 1
    do TBar(AnalyserChart.Bars.Items[Band]).Value := round(FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain);
   AnalyserChart.Invalidate;
   {$ENDIF}
  end;
end;

end.