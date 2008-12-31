unit DeessDM;

interface

uses 
  Windows, Messages, SysUtils, DAV_Common, DAV_VSTModule;

type
  TDeessDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHFDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FThreshold : Single;
    FEnvelope  : Single;
    FAttack    : Single;
    FRelease   : Single;
    FFilter    : Single;
    FGain      : Single;
    FBuffer    : Array [0..1] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDeessDataModule.ParamHFDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := dB_to_Amp(Value);
end;

procedure TDeessDataModule.ParamEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Value);
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
 FFilter := 0.05 + 0.94 * sqr(Exp2Lin);
end;

procedure TDeessDataModule.VSTModuleOpen(Sender: TObject);
begin
 FAttack   := 0.01;
 FRelease  := 0.992;
 FEnvelope := 0;

 // Initial Parameters
 Parameter[0] :=  -30; // Threshold [dB]
 Parameter[1] := 1000; // Frequency [Hz]
 Parameter[0] :=    0; // HF-Drive  [dB]
end;

procedure TDeessDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
  f           : Array [0..1] of Single;
  tmp, fi, fo : Single;
  en, g       : Single;
begin
 f[0] := FBuffer[0];
 f[1] := FBuffer[1];
 fi   := FFilter;
 fo   := (1 - FFilter);
 en   := FEnvelope;
 for Sample := 0 to SampleFrames - 1 do
  begin
    tmp  := 0.5 * (Inputs[0, Sample] + Inputs[1, Sample]);   // 2nd order crossover
    f[0] := fo * f[0] + fi * tmp;
    tmp  := tmp - f[0];
    f[1] := fo * f[1] + fi * tmp;
    tmp  := FGain * (tmp - f[1]);                            // extra HF gain
    // brackets for full-band!!!

    if (tmp > en)                                            // envelope
     then en := en + FAttack * (tmp - en)
     else en := en * FRelease;
    if (en > FThreshold)
     then g := f[0] + f[1] + tmp * (FThreshold / en)
     else g := f[0] + f[1] + tmp; //limit
    Outputs[0, Sample] := g;
    Outputs[1, Sample] := g;
  end;
 if abs(f[0]) < 1E-10 then FBuffer[0] := 0 else FBuffer[0] := f[0];
 if abs(f[1]) < 1E-10 then FBuffer[1] := 0 else FBuffer[1] := f[1];
 if abs(en)   < 1E-10 then FEnvelope  := 0 else FEnvelope  := en;
end;

procedure TDeessDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
  f           : Array [0..1] of Double;
  tmp, fi, fo : Double;
  en, g       : Double;
begin
 f[0] := FBuffer[0];
 f[1] := FBuffer[1];
 fi   := FFilter;
 fo   := (1 - FFilter);
 en   := FEnvelope;
 for Sample := 0 to SampleFrames - 1 do
  begin
    tmp  := 0.5 * (Inputs[0, Sample] + Inputs[1, Sample]);   // 2nd order crossover
    f[0] := fo * f[0] + fi * tmp;
    tmp  := tmp - f[0];
    f[1] := fo * f[1] + fi * tmp;
    tmp  := FGain * (tmp - f[1]);                            // extra HF gain

    if (tmp > en)                                            // envelope
     then en := en + FAttack * (tmp - en)
     else en := en * FRelease;
    if (en > FThreshold)
     then g := f[0] + f[1] + tmp * (FThreshold / en)
     else g := f[0] + f[1] + tmp; //limit
    Outputs[0, Sample] := g;
    Outputs[1, Sample] := g;
  end;
 if abs(f[0]) < 1E-10 then FBuffer[0] := 0 else FBuffer[0] := f[0];
 if abs(f[1]) < 1E-10 then FBuffer[1] := 0 else FBuffer[1] := f[1];
 if abs(en)   < 1E-10 then FEnvelope  := 0 else FEnvelope  := en;
end;

end.