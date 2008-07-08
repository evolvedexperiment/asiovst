unit VocInputDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TVocInputDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterTrackingDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VocInputDataModuleParameterProperties4CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fLowBuffer : Array [0..3] of Single;
    fPStep     : Single;
    fSawbuf    : Single;
    fNoise     : Single;
    fLowEnv    : Single;
    fHighEnv   : Single;
    fLowFreq   : Single;
    fVUv       : Single;
    fRoot      : Single;
    fMinp      : Single;
    fMaxp      : Single;
    fPMult     : Single;
    fTrack     : Integer;
    function midi2string(const n : Single): string;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TVocInputDataModule.ParameterTrackingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[index]) of
  0: PreDefined := 'OFF';
  1: PreDefined := 'FREE';
  2: PreDefined := 'QUANT';
 end;
end;

procedure TVocInputDataModule.VocInputDataModuleParameterProperties4CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := midi2string(Parameter[4]);
end;

procedure TVocInputDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 0.5;  // Tracking Off / On / Quant

 Parameter[2] := 20;   // Breath fNoise
 Parameter[3] := 50;   // Voiced / Unvoiced Thresh
(*
 Parameter[1] := 0.50;  //Pitch
 Parameter[4] := 0.35;  //Max Freq
*)
end;

function TVocInputDataModule.midi2string(const n : Single) : string; //show name of MIDI note number (60=C3)
var
  o, s, p : Integer;
begin
 p      := 0;
 result := '   ';
 o      := round(n / 12);
 s      := round(n - (12 * o));
 o      := o - 2;

 case s of
    0: result := result + 'C';
    1: result := result + 'C#';
    2: result := result + 'D';
    3: result := result + 'Eb';
    4: result := result + 'E';
    5: result := result + 'F';
    6: result := result + 'F#';
    7: result := result + 'G';
    8: result := result + 'G#';
    9: result := result + 'A';
   10: result := result + 'Bb';
  else result := result + 'B';
 end;

 result := result + ' ';

 if (o < 0) then result := result + '-';
 result := result + char(48 + (abs(o) mod 10));
end;

procedure TVocInputDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  ds     : Single;
  s      : Single;
  n      : Single;
  l0     : Single;
  l1     : Single;
  l2     : Single;
  l3     : Single;
  le     : Single;
  he     : Single;
  et     : Single;
  lf     : Single;
  v      : Single;
  mn     : Single;
  mx     : Single;
  rootm  : Single;
  tr     : Integer;
begin
  ds    := fPStep;
  s     := fSawbuf;
  n     := fNoise;
  l0    := fLowBuffer[0];
  l1    := fLowBuffer[1];
  l2    := fLowBuffer[2];
  l3    := fLowBuffer[3];
  le    := fLowEnv;
  he    := fHighEnv;
  et    := fLowFreq * 0.1;
  lf    := fLowFreq;
  v     := fVUv;
  mn    := fMinp;
  mx    := fMaxp;
  rootm := 39.863137;
  tr    := fTrack;

 for Sample := 0 to SampleFrames - 1 do
  begin
   l0 := l0 - lf * (l1 + Inputs[0, Sample]);   // fundamental filter (peaking 2nd-order 100Hz lpf)
   l1 := l1 - lf * (l1 - l0);

   Outputs[1, Sample] := abs(l0);
   le := le - et * (le - Outputs[1, Sample]);  // fundamental level

   Outputs[1, Sample]  := abs((Inputs[0, Sample] + 0.03) * v);
   he := he - et * (he - Outputs[1, Sample]);  // overall level (+ constant so >f0 when quiet)

   l3 := l3 + 1;
   if tr > 0 then                         // pitch tracking
    begin
     if ((l1 > 0) and (l2 <= 0)) then     // found +ve zero crossing
      begin
       if ((l3 > mn) and (l3 < mx)) then  // ...in allowed range
        begin
         mn := 0.6 * l3;                  // new max pitch to discourage octave jumps!
         l2 := l1 / (l1 - l2);            // fractional period...
         ds := fPMult / (l3 - l2);        // new period

         if (tr = 2) then                 // quantize pitch
          begin
           ds := rootm * (log10(ds) - fRoot);
           ds := Power(1.0594631, trunc(ds + 0.5) + rootm * fRoot);
          end;
        end;
       l3 := l2;                          // restart period measurement
      end;
     l2 := l1;                            // remember previous sample
    end;

(*
   Outputs[1, Sample] := 0.00001 * ((random & 32767) - 16384); // sibilance
*)
   if (le > he)
    then Outputs[1, Sample] := Outputs[1, Sample] * s * n;     // ...or modulated breath fNoise
   Outputs[1, Sample] := Outputs[1, Sample] + s;
   s := s + ds;
   if (s > 0.5)
    then s := s - 1;              // badly aliased sawtooth!

   Outputs[0, Sample] := Inputs[0, Sample];
  end;
  fSawbuf := s;

  if (abs(he) > 1E-10)
   then fHighEnv := he
   else fHighEnv := 0; //catch denormals
  if (abs(l1) > 1E-10) then
   begin
    fLowBuffer[0] := l0;
    fLowBuffer[1] := l1;
    fLowEnv       := le;
   end
  else
   begin
    fLowBuffer[0] := 0;
    fLowBuffer[1] := 0;
    fLowEnv := 0;
   end; 
  fLowBuffer[2] := l2;
  fLowBuffer[3] := l3;
  if (tr > 0) then fPStep := ds; 
end;

procedure TVocInputDataModule.VSTModuleResume(Sender: TObject);
var
  fs, ifs : Single;
begin
  fs  := SampleRate;
  ifs := 1 / fs;
  fTrack := round(2.99 * Parameter[0]);
  fPMult := Power(1.0594631, Trunc(48 * Parameter[1] - 24));
  if (fTrack = 0)
   then fPStep := 110.0 * fPMult * ifs;

  fNoise   := 6 * 0.01 * Parameter[2];
  fLowFreq := 660 * ifs;

  fMinp    := Power(16, 0.5 - (Parameter[4] - 45) / 48) * fs / 440;
  fMaxp    := 0.03 * fs;
  fRoot    := log10(8.1757989 * ifs);
  fVUv     := sqr(0.01 * Parameter[3]);
end;

procedure TVocInputDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fLowBuffer[0] := 0;
 fLowBuffer[1] := 0;
 fLowBuffer[2] := 0;
 fLowBuffer[3] := 0;
 fPStep        := 0;
 fSawbuf       := 0;
 fLowEnv       := 0;
end;

end.

(*
void mdaVocInput::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case  1: if (fTrack)
              then sprintf(string, "%ld", (long)(48.0f * Parameter[1] - 24.0f));
              else midi2string((long)(48.0f * Parameter[1] + 21.0f), string); break;
  }
}
*)