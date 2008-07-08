unit BeatBoxDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TBeatBoxDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure ParameterDynamicsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fHihatBufferLength : Integer;
    fKickBufferLength  : Integer;
    fSnareBufferLength : Integer;
    fHihatThreshold    : Single;
    fHihatDelay        : Integer;
    fSnareThreshold    : Single;
    fSnareDelay        : Integer;
    fKickThreshold     : Single;
    fKickDelay         : Integer;
    fSnareBufferPos    : Integer;
    fRec               : Integer;
    fRecx              : Integer;
    fRecPos            : Integer;
    fMix               : Single;
    fDyna              : Single;
    fDynr              : Single;
    fDyne              : Single;
    fDynm              : Single;
    fKWW, fKWWx        : Single;
    fKSF1              : Single;
    fKSF2              : Single;
    fWW, fWWx          : Single;
    fHfil              : Single;
    fHBufPos           : Integer;
    fSBufPos           : Integer;
    fSb1               : Single;
    fSb2               : Single;
    fKBufPos           : Integer;
    fKsb1              : Single;
    fKsb2              : Single;
    fSf1               : Single;
    fSf2               : Single;
    fSf3               : Single;
    fSnareFX           : Single;
    fKSFX              : Single;
    fHihatLevel        : Single;
    fKickLevel         : Single;
    fSnareLevel        : Single;
    fHihatBuffer       : PAVDSingleFixedArray;
    fKickBuffer        : PAVDSingleFixedArray;
    fSnareBuffer       : array [0..1] of PAVDSingleFixedArray;
    procedure Synth;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TBeatBoxDataModule.VSTModuleCreate(Sender: TObject);
var
  Oversampling : Integer;
begin
(*
 Parameter[ 0] := 0.3;  // Hat Thresh
 Parameter[ 1] := 0.45; // Hat rate
 Parameter[ 2] := 0.5;  // Hat Mix
 Parameter[ 3] := 0.46; // Kick Thresh
 Parameter[ 4] := 0.15; // Kick Key
 Parameter[ 5] := 0.5;  // Kick Mix
 Parameter[ 6] := 0.5;  // Snare Thresh
 Parameter[ 7] := 0.7;  // Snare Key
 Parameter[ 8] := 0.5;  // Snare Mix
 Parameter[ 9] := 0;    // Dynamics
 Parameter[10] := 0;    // Record
 Parameter[11] := 0;    // Thru Mix
*)

 Oversampling := round(SampleRate / 49000 + 0.5);
 if Oversampling < 1 then Oversampling := 1; 

 fHihatBufferLength := Oversampling * 20000;
 fKickBufferLength  := Oversampling * 20000;
 fSnareBufferLength := Oversampling * 60000;

 GetMem(fKickBuffer,     fKickBufferLength  * SizeOf(Single));
 GetMem(fSnareBuffer[0], fSnareBufferLength * SizeOf(Single));
 GetMem(fSnareBuffer[1], fSnareBufferLength * SizeOf(Single));
 GetMem(fHihatBuffer,    fHihatBufferLength * SizeOf(Single));

 //calcs here
 fHihatThreshold := Power(10, 2 * Parameter[0] - 2);
 fHihatDelay     := Round((0.04 + 0.2 * Parameter[1]) * SampleRate);
 fSnareThreshold := 40 * Power(10, 2 * Parameter[6] - 2);
 fSnareDelay     := Round(0.12 * SampleRate);
 fKickThreshold  := 220 * Power(10, 2 * Parameter[3] - 2);
 fKickDelay      := Round(0.1 * SampleRate);

 fHihatLevel := (1E-4 + sqr(Parameter[2]) * 4);
 fKickLevel  := (1E-4 + sqr(Parameter[5]) * 4);
 fSnareLevel := (1E-4 + sqr(Parameter[8]) * 4);

 fKWW     := Power(10, -3 + 2.2 * Parameter[4]);
 fKSF1    := cos(Pi * fKWW);    // p
 fKSF2    := sin(Pi * fKWW);    // q

 fWW      := Power(10, -3 + 2.2 * Parameter[7]);
 fSf1     := cos(Pi * fWW);     // p
 fSf2     := sin(Pi * fWW);     // q
 fSf3     := 0.991;             // r
 fSnareFX := 0;
 fKSFX    := 0;

 fRec     := 0;
 fRecx    := 0;
 fRecPos  := 0;

 fDyna    := Power(10, -1E4 / SampleRate);
 fDynr    := Power(10, -6   / SampleRate);
 fDyne    := 0;

 Synth;
end;

procedure TBeatBoxDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fHihatBuffer)    then Dispose(fHihatBuffer);
 if assigned(fKickBuffer)     then Dispose(fKickBuffer);
 if assigned(fSnareBuffer[0]) then Dispose(fSnareBuffer[0]);
 if assigned(fSnareBuffer[1]) then Dispose(fSnareBuffer[1]);
end;

procedure TBeatBoxDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHihatThreshold := Power(10, 2 * Parameter[0] - 2);
 fHihatDelay     := Round((0.04 + 0.2 * Parameter[1]) * SampleRate);
 fSnareThreshold := 40 * Power(10, 2 * Parameter[6] - 2);
 fKickThreshold  := 220 * Power(10, 2 * Parameter[3] - 2);

 fHihatLevel     := 1E-4 + sqr(Parameter[2]) * 4;
 fKickLevel      := 1E-4 + sqr(Parameter[5]) * 4;
 fSnareLevel     := 1E-4 + sqr(Parameter[8]) * 4;

 fWWx            := fWW;
 fWW             := Power(10, -3 + 2.2 * Parameter[7]);
 fSf1            := cos(3.1415927 * fWW);     //p
 fSf2            := sin(3.1415927 * fWW);     //q
 fSnareFX        := 0;
 fKSFX           := 0;

 fKWWx           := fKWW;
 fKWW            := Power(10, -3 + 2.2 * Parameter[4]);
 fKSF1           := cos(3.1415927 * fKWW);     //p
 fKSF2           := sin(3.1415927 * fKWW);     //q

 if (fWWx  <> fWW)  then fSnareFX := round(2 * SampleRate);
 if (fKWWx <> fKWW) then fKSFX    := round(2 * SampleRate);

 fRec := round(4.9 * Parameter[10]);
 if (fRec <> fRecx) and (fRecPos > 0) then //finish sample
  begin
   case fRec of
    2: while (fRecPos < fHihatBufferLength) do begin fHihatBuffer[fRecPos] := 0; inc(fRecPos); end;
    3: while (fRecPos < fKickBufferLength)  do begin fKickBuffer[fRecPos] := 0;  inc(fRecPos); end;
    4: while (fRecPos < fSnareBufferLength) do
        begin
         fSnareBuffer[0, fRecPos] := 0;
         fSnareBuffer[1, fRecPos] := 0;
         inc(fRecPos);
        end;
   end;
 end;

 fRecPos := 0;
 fRecx   := fRec;
end;

procedure TBeatBoxDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample           : Integer;
  a, b, e, o       : Single;
  hf, ht, mx3, mx1 : Single;
  hp, hl, hd       : Integer;
  kt               : Single;
  kp, kl, kd       : Integer;
  st, s, f1, f2,
  b1, b2, b3       : Single;
  k, kf1, kf2,
  kb1, kb2, mx4    : Single;
  hlv, klv, slv    : Single;
  sp, sl, sd       : Integer;
  ya, yr, ye, ym   : Single;
begin
 ht  := fHihatThreshold;
 hl  := fHihatBufferLength - 2;
 hd  := fHihatDelay;
 kt  := fKickThreshold;
 kl  := fKickBufferLength - 2;
 kd  := fKickDelay;
 st  := fSnareThreshold;
 mx3 := 0;
 hf  := fHfil;
 mx1 := fMix;
 hp  := fHBufPos;
 kp  := fKBufPos;
 f1  := fSb1;
 f2  := fSb2;
 kf1 := fKsb1;
 kf2 := fKsb2;
 b1  := fSf1;
 b2  := fSf2;
 b3  := fSf3;
 kb1 := fKSF1;
 kb2 := fKSF2;
 hlv := fHihatLevel;
 klv := fKickLevel;
 slv := fSnareLevel;
 sp  := fSnareBufferPos;
 sl  := fSnareBufferLength - 2;
 sd  := fSnareDelay;
 ya  := fDyna;
 yr  := fDynr;
 ye  := fDyne;
 ym  := fDynm;

 if (fSnareFX > 0) then
  begin
   mx3 := 0.08;
   slv := 0;
   klv := 0;
   hlv := 0;
   mx1 := 0;
   fSnareFX := fSnareFX - sampleFrames;
  end; //key listen (snare)

 if (fKSFX > 0) then
  begin
   mx3   := 0.03;
   slv   := 0;
   klv   := 0;
   hlv   := 0;
   mx1   := 0;
   fKSFX := fKSFX - sampleFrames;
   b1    := fKSF1;
   b2    := fKSF2;
  end; //key listen (kick)

if (fRec = 0) then
 for Sample := 0 to SampleFrames - 1 do
  begin
   a    := Inputs[0, Sample];
   b    := Inputs[1, Sample];
   e    := a + b;

   if e < ye
    then ye := ye * yr
    else ye := e - ya * (e - ye);  // dynamics envelope

   hf := e - hf;                   // high filter
   if ((hp > hd) and (hf > ht))
    then hp := 0
    else
     begin
      inc(hp);
      if (hp > hl)
       then hp := hl;
     end;
   o := hlv * fHihatBuffer[hp]; //hat

   k   := e + (kf1 * kb1) - (kf2 * kb2); //low filter
   kf2 := b3 * ((kf1 * kb2) + (kf2 * kb1));
   kf1 := b3 * k;
   if ((kp > kd) and (k > kt))
    then kp := 0
    else
     begin
      inc(kp);
      if (kp > kl) then kp := kl;
     end;

   o := o + klv * fKickBuffer[kp]; //kick

   s  := hf + (0.3 * e) + (f1 * b1) - (f2 * b2); //mid filter
   f2 := b3 * ((f1 * b2) + (f2 * b1));
   f1 := b3 * s;

   if ((sp > sd) and (s > st))
    then sp := 0
    else
     begin
      inc(sp);
      if (sp > sl) then sp := sl;
     end;

   mx4 := 1 + ym * (ye + ye - 1); //dynamics

   Outputs[0, Sample] := mx1 * a + mx3 * s + mx4 * (o + slv * fSnareBuffer[0, sp]);
   Outputs[1, Sample] := mx1 * a + mx3 * s + mx4 * (o + slv * fSnareBuffer[1, sp]);

   hf := e;
  end
else //record
 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample];
   e := 0.5 * (a + b);

   if ((fRecPos = 0) and (abs(e) < 0.004))
    then e := 0
    else
     case fRec of
       2: begin
           if (fRecPos < hl)
            then fHihatBuffer[fRecPos] := e
            else e := 0;
           inc(fRecPos);
          end;
       3: begin
           if (fRecPos < kl)
            then fKickBuffer[fRecPos] := e
            else e := 0;
           inc(fRecPos);
          end;
       4: if (fRecPos < sl) then
           begin
            fSnareBuffer[0, fRecPos] := a;
            fSnareBuffer[1, fRecPos] := b;
            inc(fRecPos);
           end
          else e := 0;
     end;

   Outputs[0, Sample] := e;
   Outputs[1, Sample] := e;
  end;

 fHfil    := hf;
 fHBufPos := hp;
 fSBufPos := sp;
 fSb1     := f1;
 fSb2     := f2;
 fKBufPos := kp;
 fKsb1    := kf1;
 fKsb2    := kf2;
 fDyne    := ye;
end;

function fmod(Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
 Norm := Arg1 / Arg2;
 result := (Norm - round(Norm - 0.5)) * Arg2
end;

procedure TBeatBoxDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMix := Value;
end;

procedure TBeatBoxDataModule.ParameterDynamicsChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDynm := Value;
end;

procedure TBeatBoxDataModule.Synth;
var
  t : Integer;
  e : Single;
  de, o, o1, o2, p, dp : Single;
begin
 e   := 0.00012;
 o1  := 0;
 o2  := 0;
 p   := 0.2;

 //generate hi-hat
 FillChar(fHihatBuffer, fHihatBufferLength * SizeOf(Single), 0);
 de := Power(10, -36 / SampleRate);
 for t := 0 to fHihatBufferLength - 1 do
  begin
   o := 1000 * (random * 2 - 1);
   fHihatBuffer[t] :=  e * (2 * o1 - o2 - o);
   e  := e * de;
   o2 := o1;
   o1 := o;
  end;

 FillChar(fKickBuffer, fKickBufferLength * SizeOf(Single), 0); //generate kick
 de := Power(10, -3.8 / SampleRate);
 e  := 0.5;
 dp := 1588 / SampleRate;
 for t := 0 to fKickBufferLength - 1 do
  begin
   fKickBuffer[t] :=  e * sin(p);
   e := e * de;
   p := fmod(p + dp * e, 2 * Pi);
  end;

 FillChar(fSnareBuffer[0], fSnareBufferLength * SizeOf(Single), 0); //generate snare
 FillChar(fSnareBuffer[1], fSnareBufferLength * SizeOf(Single), 0); //generate snare
 de := Power(10, -15.0 / SampleRate);
 e  := 0.38;
 dp := 1103 / SampleRate;
 for t := 0 to fSnareBufferLength - 1 do
  begin
   o := (0.3 * o) + 1000 * (2 * random - 1);
   fSnareBuffer[0, t] := (e * (sin(p) + 0.0004 * o));
   fSnareBuffer[1, t] := fSnareBuffer[0, t];
   e := e * de;
   p := fmod(p + 0.025, 2 * Pi);
  end;
end;

end.

(*
void mdaBeatBox::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: float2strng((40.0*Parameter[0 - 40.0),text); break;
    case 1: long2string((long)(1000. * fHihatDelay / SampleRate),text); break;
    case 2: long2string((long)(20. * log10(fHihatLevel)),text); break;
    case 3: float2strng((40.0*Parameter[3 - 40.0),text); break;
    case 4: long2string((long)(0.5 * fKWW * SampleRate), text); break;
    case 5: long2string((long)(20. * log10(fKickLevel)),text); break;
    case 6: float2strng((40.0*Parameter[6 - 40.0),text); break;
    case 7: long2string((long)(0.5 * fWW * SampleRate), text); break;
    case 8: long2string((long)(20. * log10(fSnareLevel)),text); break;
    case 9: long2string((long)(100. * Parameter[9),text); break; 
    case 11: long2string((long)(20. * log10(Parameter[11)),text); break;

    case 10: switch(fRec) 
            begin case 0: strcpy(text, "-"); break;
              case 1: strcpy(text, "MONITOR"); break;
              case 2: strcpy(text, "-> HAT"); break;
              case 3: strcpy(text, "-> KIK"); break;
              case 4: strcpy(text, "-> SNR"); break; end; break;
  end;
end;

*)
