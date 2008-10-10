unit ChebyshevWaveshaperDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspWaveshaper;

const
  HarmCount : Integer = 24;
  dBMin : Single = -140;

type
  TChebyshevWaveshaperDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDouble(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHarmDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHarmLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FChebysheWaveshaper : TChebyshevWaveshaper;
    FVolume             : Single;
    procedure ParamHarmonicChange(Sender: TObject; const Index: Integer; var Value: Single);
  public
  end;

implementation

{$R *.DFM}

uses
  ChebyshevWaveshaperGUI, DAV_VSTParameters;

procedure TChebyshevWaveshaperDataModule.ParamVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVolume := dB_to_Amp(Value);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0 then PreDefined := 'dB (+)' else
 if Parameter[Index] < 0
  then PreDefined := 'dB (-)'
  else PreDefined := 'dB';
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := abs(140 * Parameter[Index]) - 140;
 if abs(Parameter[Index]) < 1E-3
  then PreDefined := '-oo'
  else PreDefined := FloatToStrF(Val, ffGeneral, 3, 3);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmonicChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FChebysheWaveshaper)then
  begin
   if abs(Value) < 1E-3 then
    begin
     FChebysheWaveshaper.Gain[Index] := 0;
    end
   else
    begin
     FChebysheWaveshaper.Level[Index]    := abs(Value * 140) - 140;
     FChebysheWaveshaper.Inverted[Index] := Value < 0;
    end;
  end;
 if EditorForm is TFmChebyshevWaveshaper then
  with TFmChebyshevWaveshaper(EditorForm) do
   begin
    UpdateHarmonic(Index);
   end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChebysheWaveshaper);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleCreate(Sender: TObject);
var
  i : Integer;
begin
 FVolume := 1;
 for i := HarmCount - 1 downto 0 do
  with ParameterProperties.Insert(0) do
   begin
    DisplayName       := 'Harmonic ' + IntToStr(i + 1);
    Min               := -1;
    Max               := 1;
    StepFloat         := 1;
    StepInteger       := 1;
    SmallStepFloat    := 0.1;
    LargeStepFloat    := 10;
    LargeStepInteger  := 10;
    ShortLabel        := 'H' + IntToStr(i + 1);
    Units             := 'dB';
    OnParameterChange := ParamHarmonicChange;
    OnCustomParameterDisplay := ParamHarmDisplay;
    OnCustomParameterLabel := ParamHarmLabel;
   end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmChebyshevWaveshaper.Create(Self);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 FChebysheWaveshaper := TChebyshevWaveshaper.Create;
 FChebysheWaveshaper.Order := HarmCount;
 Parameter[0] := 1;
 for i := 1 to HarmCount - 1
  do Parameter[i] := 0;
 Parameter[HarmCount] := 0;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i : Integer;
begin
 for ch := 0 to 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[ch, i] := FVolume * FChebysheWaveshaper.ProcessSample(Inputs[ch, i]);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcessDouble(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  ch, i : Integer;
begin
 for ch := 0 to 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[ch, i] := FVolume * FChebysheWaveshaper.ProcessSample(Inputs[ch, i]);
end;

end.