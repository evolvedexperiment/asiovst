unit DeessDM;

interface

uses 
  Windows, Messages, SysUtils, DAVDCommon, DVSTModule;

type
  TDeessDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHFDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fThreshold : Single;
    fEnvelope  : Single;
    fAttack    : Single;
    fRelease   : Single;
    fFilter    : Single;
    fGain      : Single;
    fBuffer    : Array [0..1] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDeessDataModule.ParamHFDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fGain := dB_to_Amp(Value);
end;

procedure TDeessDataModule.ParamEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fThreshold := dB_to_Amp(Value);
end;

procedure TDeessDataModule.ParamFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  RangeRatio : Double;
  Exp2Lin    : Double;
begin
 with ParameterProperties[Index] do
  begin
   RangeRatio := Max / Min;
   Exp2Lin := (exp(((Value - Min) / (Max - Min)) * ln(RangeRatio + 1)) - 1);
  end;
 fFilter := 0.05 + 0.94 * sqr(Exp2Lin);
end;

procedure TDeessDataModule.VSTModuleCreate(Sender: TObject);
begin
 fAttack   := 0.01;
 fRelease  := 0.992;
 fEnvelope := 0;

 Parameter[0] :=  -30; // Threshold [dB]
 Parameter[1] := 1000; // Frequency [Hz]
 Parameter[0] :=    0; // HF-Drive  [dB]
end;

procedure TDeessDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
  f           : Array [0..1] of Single;
  tmp, fi, fo : Single;
  en, g       : Single;
begin
 f[0] := fBuffer[0];
 f[1] := fBuffer[1];
 fi   := fFilter;
 fo   := (1 - fFilter);
 en   := fEnvelope;
 for Sample := 0 to SampleFrames - 1 do
  begin
    tmp  := 0.5 * (Inputs[0, Sample] + Inputs[1, Sample]);   // 2nd order crossover
    f[0] := fo * f[0] + fi * tmp;
    tmp  := tmp - f[0];
    f[1] := fo * f[1] + fi * tmp;
    tmp  := fGain * (tmp - f[1]);                            // extra HF gain
    // brackets for full-band!!!

    if (tmp > en)                                            // envelope
     then en := en + fAttack * (tmp - en)
     else en := en * fRelease;
    if (en > fThreshold)
     then g := f[0] + f[1] + tmp * (fThreshold / en)
     else g := f[0] + f[1] + tmp; //limit
    Outputs[0, Sample] := g;
    Outputs[1, Sample] := g;
  end;
 if abs(f[0]) < 1E-10 then fBuffer[0] := 0 else fBuffer[0] := f[0];
 if abs(f[1]) < 1E-10 then fBuffer[1] := 0 else fBuffer[1] := f[1];
 if abs(en)   < 1E-10 then fEnvelope  := 0 else fEnvelope  := en;
end;

procedure TDeessDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
  f           : Array [0..1] of Double;
  tmp, fi, fo : Double;
  en, g       : Double;
begin
 f[0] := fBuffer[0];
 f[1] := fBuffer[1];
 fi   := fFilter;
 fo   := (1 - fFilter);
 en   := fEnvelope;
 for Sample := 0 to SampleFrames - 1 do
  begin
    tmp  := 0.5 * (Inputs[0, Sample] + Inputs[1, Sample]);   // 2nd order crossover
    f[0] := fo * f[0] + fi * tmp;
    tmp  := tmp - f[0];
    f[1] := fo * f[1] + fi * tmp;
    tmp  := fGain * (tmp - f[1]);                            // extra HF gain

    if (tmp > en)                                            // envelope
     then en := en + fAttack * (tmp - en)
     else en := en * fRelease;
    if (en > fThreshold)
     then g := f[0] + f[1] + tmp * (fThreshold / en)
     else g := f[0] + f[1] + tmp; //limit
    Outputs[0, Sample] := g;
    Outputs[1, Sample] := g;
  end;
 if abs(f[0]) < 1E-10 then fBuffer[0] := 0 else fBuffer[0] := f[0];
 if abs(f[1]) < 1E-10 then fBuffer[1] := 0 else fBuffer[1] := f[1];
 if abs(en)   < 1E-10 then fEnvelope  := 0 else fEnvelope  := en;
end;

end.