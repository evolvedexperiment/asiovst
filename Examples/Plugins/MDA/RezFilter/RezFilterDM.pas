unit RezFilterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TRezFilterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLFORateDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTriggerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fBuffer           : Array [0..2] of Single;
    fFrequency        : Single;
    fQuality, fGain   : Single;
    fFreqMax          : Single;
    fFreqEnv          : Single;
    fEnv, fEnv2       : Single;
    fAtt, fRel, fPhi  : Single;
    fLFOMode          : Integer;
    fState, fFreqLFO  : Single;
    fDeltaPhi         : Single;
    fTrigger          : Integer;
    fTriggerAttack    : Integer;
    fTriggerThreshold : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TRezFilterDataModule.ParameterGainChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fGain  := 0.5 * dB_to_Amp(Value);
end;

procedure TRezFilterDataModule.ParameterReleaseDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF((-301.0301 / (SampleRate * log10(fRel))), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.ParameterTriggerDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if (fTriggerThreshold = 0)
  then PreDefined := 'FREE RUN'
  else PreDefined := IntToStr(round(20 * log10(0.5 * fTriggerThreshold)));
end;

procedure TRezFilterDataModule.ParameterLFORateDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(Power(10, 4 * Parameter[Index] - 2), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.ParameterAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(-301.0301 / (SampleRate * log10(1 - fAtt)), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.VSTModuleCreate(Sender: TObject);
begin
 // inits here!
 Parameter[0] := 33;   // Frequency [%]
 Parameter[1] := 70;   // Quality [%]
 Parameter[2] := 0;    // Amplification [dB]
 Parameter[3] := 85;   // Frequency Envelope [%]
 Parameter[4] := 0.00; // Attack
 Parameter[5] := 0.50; // Release
 Parameter[6] := 70;   // LFO  [%]
 Parameter[7] := 0.40; // Rate
 Parameter[8] := 0.00; // Trigger
 Parameter[9] := 75;   // Max. Frequency

 VSTModuleSuspend(Sender);
end;

procedure TRezFilterDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fFrequency := 1.5 * sqr(0.01 * Parameter[0]) - 0.15;
 fQuality  := 0.99 * Power(0.01 * Parameter[1], 0.3); // was 0.99 *

 fFreqMax := 0.99 + 0.3 * Parameter[1];
 if fFreqMax > (0.013 * Parameter[9])
  then fFreqMax := 0.013 * Parameter[9];

 fFreqEnv := abs(2 * sqr(0.005 * Parameter[3]));
 fAtt     := Power(10, -0.01 - 4.0 * Parameter[4]);
 fRel     := 1 - Power(10, -2.00 - 4.0 * Parameter[5]);

 fLFOMode := 0;
 fFreqLFO := 2 * sqr(0.005 * Parameter[6]);
 fDeltaPhi := (6.2832 * Power(10, 3 * Parameter[7] - 1.5) / SampleRate);
 if (Parameter[6] < 0) then
  begin
   fLFOMode  := 1;
   fDeltaPhi := 0.15915 * fDeltaPhi;
   fFreqLFO  := fFreqLFO * 0.001;
  end; //S&H

 if (Parameter[8] < 0.1)
  then fTriggerThreshold := 0
  else fTriggerThreshold := 3 * sqr(Parameter[8]);
end;

procedure TRezFilterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
  a, f, i, ff, fe     : Single;
  q, g, e, tmp        : Single;
  b0, b1, b2          : Single;
  at, re, fm, fl      : Single;
  dph, ph, bl, th, e2 : Single;
  lm, ta, tt          : Integer;
begin
  ff  := fFrequency;
  fe  := fFreqEnv;
  q   := fQuality;
  g   := fGain;
  e   := fEnv;
  at  := fAtt;
  re  := fRel;
  fm  := fFreqMax;
  fl  := fFreqLFO;
  dph := fDeltaPhi;
  th  := fTriggerThreshold;
  e2  := fEnv2;
  lm  := fLFOMode;
  ta  := fTriggerAttack;
  tt  := fTrigger;
  b0  := fBuffer[0];
  b1  := fBuffer[1];
  b2  := fBuffer[2];
  ph  := fPhi;
  bl  := fState;

  if th = 0 then
   begin
    for Sample := 0 to SampleFrames - 1 do
     begin
      a := Inputs[0, Sample] + Inputs[1, Sample];

      i := abs(a); //envelope
      if i > e
       then e := e + at * (i - e)
       else e := e * re;

      if lm = 0 // LFO
       then bl := fl * sin(ph) else
      if (ph > 1) then
       begin
        bl := fl * (random * 2000 - 1000);
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl; //freq
      if (f < 0)
       then i := 0 else
      if f > fm
       then i := fm
       else i := f;

      tmp := q + q * (1 + i * (1 + 1.1 * i));

      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);
      Outputs[0, Sample] := b1;
      Outputs[1, Sample] := b1;
    end;
   end
  else
   begin
    for Sample := 0 to SampleFrames - 1 do
     begin
      a := Inputs[0, Sample] + Inputs[1, Sample];

      i := abs(a);  // Envelope

      if i > e
       then e := i
       else e := e * re;

      if (e > th) then
       begin
        if tt = 0 then
         begin
          ta :=1;
          if lm = 1
           then ph := 2
           else tt := 1;
         end
        else tt := 0;
       end;
      if ta = 1 then
       begin
        e2 := e2 + at * (1 - e2);
        if (e2 > 0.999) then ta := 0;
       end
      else e2 := e2 * re;

      if (lm = 0) then bl := fl * sin(ph) // LFO
      else if (ph > 1) then
       begin
        bl := fl * (random * 2000 - 1000);
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl;              // Freq
      if (f < 0) then i := 0 else
      if f > fm then i := fm else i := f;

      tmp := q + q * (1.0 + i * (1.0 + 1.1 * i));
      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);

      Outputs[0, Sample] := b1;
      Outputs[1, Sample] := b1;
    end;
  end;

  if (abs(b0) < 1E-10) then
   begin
    fBuffer[0] := 0;
    fBuffer[1] := 0;
    fBuffer[2] := 0;
   end
  else
   begin
    fBuffer[0] := b0;
    fBuffer[1] := b1;
    fBuffer[2] := b2;
   end;

 fState         := bl;
 fPhi           := f_mod(ph, 2 * Pi);
 fEnv           := e;
 fEnv2          := e2;
 fTriggerAttack := ta;
 fTrigger       := tt;
end;

procedure TRezFilterDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fBuffer[0] := 0;
 fBuffer[1] := 0;
 fBuffer[2] := 0;
end;

end.
