unit UniQuEDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  DDSPFilter;

type
  TUniQuEDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamPowerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamPhaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamPadDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPhaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPadChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPresChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
  private
    fFade   : array [0..1] of Single;
    fVolume : Single;
    fLow    : array [0..1] of TSimpleLowShelfFilter;
    fMid    : array [0..1] of TSimplePeakFilter;
    fPres   : array [0..1] of TSimplePeakFilter;
    fHigh   : array [0..1] of TSimpleHighShelfFilter;
    procedure UpdateVolume;
  public
  end;

implementation

{$R *.DFM}

uses
  UniQuEGUI, DVSTCustomModule;

procedure TUniQuEDataModule.VSTModuleCreate(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   fLow[ch]  := TSimpleLowShelfFilter.Create;
   with fLow[ch] do
    begin
     Frequency := 777;
     Gain      := 0;
     Bandwidth := 3.2;
    end;
   fMid[ch]  := TSimplePeakFilter.Create;
   with fMid[ch] do
    begin
     Frequency := 1700;
     Gain      := 0;
     Bandwidth := 3.6;
    end;
   fPres[ch] := TSimplePeakFilter.Create;
   with fPres[ch] do
    begin
     Frequency := 7280;
     Gain      := 0;
     Bandwidth := 1.0;
    end;
   fHigh[ch] := TSimpleHighShelfFilter.Create;
   with fHigh[ch] do
    begin
     Frequency := 4340;
     Gain      := 0;
     Bandwidth := 2.55;
    end;
  end;
end;

procedure TUniQuEDataModule.VSTModuleDestroy(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   FreeAndNil(fLow[ch]);
   FreeAndNil(fMid[ch]);
   FreeAndNil(fPres[ch]);
   FreeAndNil(fHigh[ch]);
  end;
end;

procedure TUniQuEDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmUniQuE.Create(Self);
end;

procedure TUniQuEDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0;
end;

procedure TUniQuEDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, sample : Integer;
begin
 for ch := 0 to 1 do
  for sample := 0 to SampleFrames - 1 do
   begin
    Outputs[ch, sample] := fFade[0] * fVolume *
                           fLow[ch].ProcessSample(
                           fMid[ch].ProcessSample(
                           fPres[ch].ProcessSample(
                           fHigh[ch].ProcessSample(Inputs[ch, sample])))) +
                           fFade[1] * Inputs[ch, sample];
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

 fPres[0].Gain := Gain;
 fPres[1].Gain := Gain;

 Freq := 7278 + 108 * Value / 15;

 fPres[0].Frequency := Freq;
 fPres[1].Frequency := Freq;

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
 fFade[0] := Value;
 fFade[1] := 1 - Value;
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

 fHigh[0].Gain := Gain;
 fHigh[1].Gain := Gain;

 Freq := 4340 - 300 * Value / 15;

 fHigh[0].Frequency := Freq;
 fHigh[1].Frequency := Freq;

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

 fLow[0].Gain := Gain;
 fLow[1].Gain := Gain;

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

 fMid[0].Gain := Gain;
 fMid[1].Gain := Gain;

 BW := 3.6 + 0.1 * Value / 15;

 fMid[0].Bandwidth := BW;
 fMid[1].Bandwidth := BW;

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
  then fVolume := -dB_to_Amp(-Parameter[1] - HighAtt)
  else fVolume :=  dB_to_Amp(-Parameter[1] - HighAtt);
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
