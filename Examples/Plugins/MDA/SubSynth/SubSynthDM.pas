unit SubSynthDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTModule;

type
  TProcessType = (ptDistort, ptDivide, ptInvert, ptKeyOsc);

  TSubSynthDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fFilt  : array [0..3] of Single;
    fFilti : Single;
    fFilto : Single;

    fPhi   : Single;
    fEnv   : Single;
    fDvd   : Single;
    fPhs   : Single;
    fOsc   : Single;
    fTyp   : TProcessType;
    fWet   : Single;
    fDry   : Single;
    fThr   : Single;
    fRls   : Single;
    fDPhi  : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TSubSynthDataModule.ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fWet := 0.01 * Value;
end;

procedure TSubSynthDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fThr := dB_to_Amp(Parameter[4]);
end;

procedure TSubSynthDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRls := 1 - Power(10, -2 - (3 * Parameter[5]));
end;

procedure TSubSynthDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of 
  0: PreDefined := 'Distort';
  1: PreDefined := 'Divide';
  2: PreDefined := 'Invert';
  3: PreDefined := 'Key Osc.';
 end;
end;

procedure TSubSynthDataModule.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDry := 0.01 * Value;
end;

procedure TSubSynthDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] :=  0;    // Type
 Parameter[1] := 30;    // Level
 Parameter[2] := 0.6;   // Tune
 Parameter[3] := 10;    // Dry Mix
 Parameter[4] := -24;   // Threshold
 Parameter[5] := 0.65;  // Release

 VSTModuleResume(Sender);
end;

procedure TSubSynthDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  fDvd   := 1;
  fPhs   := 1;
  fOsc   := 0; // Oscillator phase
  fTyp   := TProcessType(round(Parameter[0]));
  if fTyp = ptKeyOsc
   then fFilti := 0.018
   else fFilti := Power(10, -3 + (2 * Parameter[2]));
  fFilto := 1 - fFilti;
  fDPhi  := 0.456159 * Power(10, -2.5 + (1.5 * Parameter[2]));
end;

procedure TSubSynthDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  fi, fo       : Single;
  f1, f2       : Single;
  f3, f4, sub  : Single;
  rl, th, dv   : Single;
  ph, phii     : Single;
  dph, os, en  : Single;

  Sample       : Integer;
begin
 dph  := fDPhi;
 rl   := fRls;
 phii := fPhi;
 en   := fEnv;
 os   := fOsc;
 th   := fThr;
 dv   := fDvd;
 ph   := fPhs;
 f1   := fFilt[0];
 f2   := fFilt[1];
 f3   := fFilt[2];
 f4   := fFilt[3];

 fi   := fFilti;
 fo   := fFilto;

 for Sample := 0 to SampleFrames - 1 do
  begin
   f1 := fo * f1 + fi * (Inputs[0, Sample] + Inputs[1, Sample]);
   f2 := fo * f2 + fi * f1;

   sub := f2;
   if sub > th then sub := 1 else
    if sub < -th then sub := -1 else sub := 0;

   if sub * dv < 0 then     // Octave Divider
    begin
     dv := -dv;
     if dv < 0 then ph := -ph;
    end;

   case fTyp of
    ptDivide : sub := ph * sub;     // Divide
    ptInvert : sub := ph * f2 * 2;  // Invert
    ptKeyOsc : begin                // Osc
                if (f2 > th)
                 then en := 1
                 else en := en * rl;
                sub  := (en * sin(phii));
                phii := f_mod(phii + dph, 6.283185);
               end;
   end;

   f3 := (fo * f3) + (fi * sub);
   f4 := (fo * f4) + (fi * f3);

  Outputs[0, Sample] := Inputs[0, Sample] * fDry + f4 * fWet; // output
  Outputs[1, Sample] := Inputs[1, Sample] * fDry + f4 * fWet;
  end;

 if (abs(f1) < 1E-10) then fFilt[0] := 0 else fFilt[0] := f1;
 if (abs(f2) < 1E-10) then fFilt[1] := 0 else fFilt[1] := f2;
 if (abs(f3) < 1E-10) then fFilt[2] := 0 else fFilt[2] := f3;
 if (abs(f4) < 1E-10) then fFilt[3] := 0 else fFilt[3] := f4;

 fDvd := dv;
 fPhs := ph;
 fOsc := os;
 fPhi := phii;
 fEnv := en;
end;

procedure TSubSynthDataModule.VSTModuleResume(Sender: TObject);
begin
 fPhi     := 0;
 fEnv     := 0;
 fFilt[0] := 0;
 fFilt[1] := 0;
 fFilt[2] := 0;
 fFilt[3] := 0;
 fFilti   := 0;
 fFilto   := 0;
end;

end.

(*
void mdaSubSynth::getParameterDisplay(VstInt32 index, char *text)
{
 {
  case 2: sprintf(string, "%ld", (long)(0.0726 * SampleRate * Power(10.0,-2.5 + (1.5 * fParam3)))); break;
  case 5: sprintf(string, "%ld", (long)(-301.03 / (SampleRate * log10(rls)))); break;
 }
}
*)