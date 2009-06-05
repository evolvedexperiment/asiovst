unit UniQuEDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilter, DAV_DSPFilterBasics;

type
  TUniQuEDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamPowerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamPhaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamPadDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPadChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPresChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFade   : array [0..1] of Single;
    FVolume : Single;
    FLow    : array [0..1] of TBasicLowShelfFilter;
    FMid    : array [0..1] of TBasicPeakFilter;
    FPres   : array [0..1] of TBasicPeakFilter;
    FHigh   : array [0..1] of TBasicHighShelfFilter;
    procedure UpdateVolume;
  public
  end;

implementation

{$R *.DFM}

uses
  UniQuEGUI, DAV_VSTCustomModule;

procedure TUniQuEDataModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   FLow[ch]  := TBasicLowShelfFilter.Create;
   with FLow[ch] do
    begin
     Frequency := 777;
     Gain      := 0;
     Bandwidth := 3.2;
    end;
   FMid[ch]  := TBasicPeakFilter.Create;
   with FMid[ch] do
    begin
     Frequency := 1700;
     Gain      := 0;
     Bandwidth := 3.6;
    end;
   FPres[ch] := TBasicPeakFilter.Create;
   with FPres[ch] do
    begin
     Frequency := 7280;
     Gain      := 0;
     Bandwidth := 1.0;
    end;
   FHigh[ch] := TBasicHighShelfFilter.Create;
   with FHigh[ch] do
    begin
     Frequency := 4340;
     Gain      := 0;
     Bandwidth := 2.55;
    end;
  end;

 // Initial Parameters
 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0;
end;

procedure TUniQuEDataModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   FreeAndNil(FLow[ch]);
   FreeAndNil(FMid[ch]);
   FreeAndNil(FPres[ch]);
   FreeAndNil(FHigh[ch]);
  end;
end;

procedure TUniQuEDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmUniQuE.Create(Self);
end;

procedure TUniQuEDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, sample : Integer;
begin
 for ch := 0 to 1 do
  for sample := 0 to SampleFrames - 1 do
   begin
    Outputs[ch, sample] := FFade[0] * FVolume *
                           FLow[ch].ProcessSample(
                           FMid[ch].ProcessSample(
                           FPres[ch].ProcessSample(
                           FHigh[ch].ProcessSample(Inputs[ch, sample])))) +
                           FFade[1] * Inputs[ch, sample];
   end;
end;

procedure TUniQuEDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   if assigned(FLow[Ch]) then FLow[Ch].SampleRate  := SampleRate;
   if assigned(FMid[Ch]) then FMid[Ch].SampleRate  := SampleRate;
   if assigned(FPres[Ch]) then FPres[Ch].SampleRate := SampleRate;
   if assigned(FHigh[Ch]) then FHigh[Ch].SampleRate := SampleRate;
  end;
end;

procedure TUniQuEDataModule.ParamPowerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TUniQuEDataModule.ParamPresChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain,
  Freq  : Single;
begin
 if Value > 0
  then Gain := Value * 11.46 / 15
  else Gain := Value * 12.96 / 15;

 if assigned(FPres[0]) then FPres[0].Gain := Gain;
 if assigned(FPres[1]) then FPres[1].Gain := Gain;

 Freq := 7278 + 108 * Value / 15;

 if assigned(FPres[0]) then FPres[0].Frequency := Freq;
 if assigned(FPres[1]) then FPres[1].Frequency := Freq;

 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdatePres;
end;

procedure TUniQuEDataModule.ParamPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 UpdateVolume;
 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateInvert;
end;

procedure TUniQuEDataModule.ParamPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFade[0] := Value;
 FFade[1] := 1 - Value;
 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateOnOff;
end;

procedure TUniQuEDataModule.ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain,
  Freq  : Single;
begin
 if Value > 0
  then Gain := Value * 15 / 15
  else Gain := Value * 13.5 / 15;

 if assigned(FHigh[0]) then FHigh[0].Gain := Gain;
 if assigned(FHigh[1]) then FHigh[1].Gain := Gain;

 Freq := 4340 - 300 * Value / 15;

 if assigned(FHigh[0]) then FHigh[0].Frequency := Freq;
 if assigned(FHigh[1]) then FHigh[1].Frequency := Freq;

 UpdateVolume;

 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateHigh;
end;

procedure TUniQuEDataModule.ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain : Single;
begin
 if Value > 0
  then Gain := Value * 11.9 / 15
  else Gain := Value * 12.3 / 15;

 if assigned(FLow[0]) then FLow[0].Gain := Gain;
 if assigned(FLow[1]) then FLow[1].Gain := Gain;

 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateLow;
end;

procedure TUniQuEDataModule.ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain, BW : Single;
begin
 if Value > 0
  then Gain := Value * 11.42 / 15
  else Gain := Value * 13.35 / 15;

 if assigned(FMid[0]) then FMid[0].Gain := Gain;
 if assigned(FMid[1]) then FMid[1].Gain := Gain;

 BW := 3.6 + 0.1 * Value / 15;

 if assigned(FMid[0]) then FMid[0].Bandwidth := BW;
 if assigned(FMid[1]) then FMid[1].Bandwidth := BW;

 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdateMid;
end;

procedure TUniQuEDataModule.UpdateVolume;
var
  HighAtt : Single;
begin
 if Parameter[6] > 0
  then HighAtt := 4.05 * Parameter[6] / 15
  else HighAtt := 1.28 * Parameter[6] / 15;

 if Parameter[2] > 0.5
  then FVolume := -dB_to_Amp(-Parameter[1] - HighAtt)
  else FVolume :=  dB_to_Amp(-Parameter[1] - HighAtt);
end;

procedure TUniQuEDataModule.ParamPadChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 UpdateVolume;
 if assigned(EditorForm) then
  with TFmUniQuE(EditorForm)
   do UpdatePad;
end;

procedure TUniQuEDataModule.ParamPadDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TUniQuEDataModule.ParamPhaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := '-'
  else PreDefined := '+';
end;

end.
