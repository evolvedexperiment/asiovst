unit RezFilterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTModule;

type
  TRezFilterDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fBuffer           : Array [0..2] of Single;
    fFrequency        : Single;
    fQuality          : Single;
    fGain             : Single;
    fFreqMax          : Single;
    fFreqEnv          : Single;
    fEnv, fEnv2       : Single;
    fAtt              : Single;
    fRel              : Single;
    fPhi              : Single;
    fState            : Single;
    fLFOMode          : Integer;
    fFreqLFO          : Single;
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

procedure TRezFilterDataModule.VSTModuleCreate(Sender: TObject);
begin
{
 //inits here!
 Parameter[0] := 0.33; // Frequency
 Parameter[1] := 0.70; // Quality
 Parameter[2] := 0.50; // Amplification
 Parameter[3] := 0.85; // Frequency Envelope
 Parameter[4] := 0.00; // Attack
 Parameter[5] := 0.50; // Release
 Parameter[6] := 0.70; // LFO
 Parameter[7] := 0.40; // Rate
 Parameter[8] := 0.00; // Trigger
 Parameter[9] := 0.75; // Max. Frequency
}

 VSTModuleSuspend(Sender);
end;

procedure TRezFilterDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  //calcs here
  fFrequency := 1.5 * sqr(Parameter[0]) - 0.15;
  fQuality  := 0.99 * Power(Parameter[1], 0.3); // was 0.99 *
  fGain  := 0.5 * Power(10, 2 * Parameter[2] - 1);

  fFreqMax := 0.99 + 0.3 * Parameter[1];
  if (fFreqMax > (1.3 * Parameter[9]))
   then fFreqMax := 1.3 * Parameter[9];
  // fFreqMax := 1;
  // fQuality := fQuality * (1 + 0.2 * Parameter[9]);

  fFreqEnv := 2 * sqr(0.5 - Parameter[3]);
  if (Parameter[3] > 0.5)
   then fFreqEnv := fFreqEnv
   else fFreqEnv := -fFreqEnv;
  fAtt  := Power(10.0, -0.01 - 4.0 * Parameter[4]);
  fRel  := 1 - Power(10.0, -2.00 - 4.0 * Parameter[5]);

  fLFOMode := 0;
  fFreqLFO := 2 * sqr(Parameter[6] - 0.5);
  fDeltaPhi := (6.2832 * Power(10, 3 * Parameter[7] - 1.5) / SampleRate);
  if (Parameter[6] < 0.5) then
   begin
    fLFOMode := 1;
    fDeltaPhi    := 0.15915 * fDeltaPhi;
    fFreqLFO    := fFreqLFO * 0.001;
   end; //S&H

  if (Parameter[8] < 0.1)
   then fTriggerThreshold := 0
   else fTriggerThreshold := 3 * sqr(Parameter[8]);
end;

procedure TRezFilterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
  a     : Single;
  f, i, ff, fe, q, g, e, tmp : Single;
  b0, b1, b2, at, re, fm     : Single;
  fl, dph, ph, bl, th, e2    : Single;
  lm, ta, tt                 : Integer;
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
(*
        bl := fl * (random mod 2000 - 1000);
*)
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl; //freq
      if (f < 0)
       then i := 0 else
      if f > fm
       then i := fm
       else i := f;
 //      o := 1 - i;

 //     tmp := g * a + q * (1 + (1/o)) * (b0 - b1);
 //     b0 := o * (b0 - tmp) + tmp;
 //     b1 := o * (b1 - b0) + b0;

      tmp := q + q * (1 + i * (1 + 1.1 * i));
      //tmp := q + q / (1.0008 - i);

      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);


  //    b2 = o * (b2 - b1) + b1;

      Outputs[0, Sample] := b1;
      Outputs[1, Sample] := b1;
    end;
   end
  else
   begin
    for Sample := 0 to SampleFrames - 1 do
     begin
      a := Inputs[0, Sample] + Inputs[1, Sample];

      i := abs(a);  //envelope

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

      if (lm = 0) then bl := fl * sin(ph) //lfo
      else if (ph > 1) then
       begin
(*
        bl := fl * (random mod 2000 - 1000);
*)
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl; //freq
      if (f < 0) then i := 0 else
      if f > fm then i := fm else i := f;

//      o = 1. - i;

      tmp := q + q * (1.0 + i * (1.0 + 1.1 * i));
//      tmp := q + q/(1.0008 - i);
      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);


//      tmp := g*a + q*(1. + (1./o)) * (b0-b1);  //what about (q + q/)*
//      b0 := o * (b0 - tmp) + tmp;                // ^ what about div0 ?
//      b1 := o * (b1 - b0) + b0;
//      b2 := o * (b2 - b1) + b1;


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
// fPhi           := fmod(ph, 2 * Pi);
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

(*
void mdaRezFilter::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: long2string((long)(100 * Parameter[0]), text); break;
    case 1: long2string((long)(100 * Parameter[1]), text); break;
    case 2: long2string((long)(40 * Parameter[2] - 20),text); break;
    case 3: long2string((long)(200 * Parameter[3] - 100), text); break;
    case 4: float2strng((float)(-301.0301 / (SampleRate * log10(1.0 - fAtt))),text); break;
    case 5: float2strng((float)(-301.0301 / (SampleRate * log10(fRel))),text); break;
    case 6: long2string((long)(200 * Parameter[6] - 100), text); break;
    case 7: float2strng(Power(10, 4 * Parameter[7] - 2), text); break;
    case 8: if (fTriggerThreshold = 0)
             then strcpy(text, "FREE RUN");
             else long2string((long) (20 * log10(0.5 * fTriggerThreshold)), text); break;
    case 9: long2string((long)(100 * Parameter[9]), text); break;
  end;
end;
*)
