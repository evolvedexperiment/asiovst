unit ThirdOctaveAnalyserDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
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
    procedure ParameterSmoothChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFullscaleGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessNormal(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDownsampled(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
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

    FFilterArray     : Array [0..cNumFrequencies - 1] of TDownsampleFilterRecord;
    FChannelNr       : Integer;
    FFSGain          : Single;
    FSpeedConst      : array [0..1] of Single;
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
  Math, DAV_VSTCustomModule, ThirdOctaveAnalyserGUI;

procedure TThirdOctaveAnalyserModule.VSTModuleOpen(Sender: TObject);
var
  Band : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
 FFSGain := 0;
 FBandReserve := 0.25;
 UpdateFilters;

 UseDownsampling := True;
 DownsamplingChanged;

 if FDownSampleCount = -1
  then OnProcess := VSTModuleProcessNormal
  else OnProcess := VSTModuleProcessDownSampled;

 OnProcessReplacing := OnProcess; 
end;

procedure TThirdOctaveAnalyserModule.VSTModuleProcessNormal(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i,j : Integer;
  d,z : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[FChannelNr,i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     d := FFilterArray[j].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample(d + cDenorm);
     FFilterArray[j].RMS := FSpeedConst[0] * FFilterArray[j].RMS + FSpeedConst[1] * Amp_to_dB(abs(z));
    end;
  end;
end;

procedure TThirdOctaveAnalyserModule.VSTModuleProcessDownsampled(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j, r : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[FChannelNr, i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[j].Downsampling) <> 0
      then Break;

     d := FFilterArray[j].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample(d + cDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[j].Downsampling + 1);
     FFilterArray[j].RMS := s * FFilterArray[j].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
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
 result := 100 * FBandReserve;
end;

function TThirdOctaveAnalyserModule.GetBandRMS(Index: Integer): Single;
begin
 if Index in [0..CNumFrequencies - 1]
  then result := FFilterArray[Index].RMS
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

procedure TThirdOctaveAnalyserModule.ParameterFullscaleGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFSGain := Value;
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
  else FDownSampleCount := -1
end;


end.