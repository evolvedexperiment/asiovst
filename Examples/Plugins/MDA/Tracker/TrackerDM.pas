unit TrackerDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTModule;

type
  TTrackerDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fMode      : Integer;
    fThreshold : Single;
    fMin, fMax : Single;
    fTrans     : Single;
    fDry, fWet : Single;
    fDyn, fPhi : Single;
    fRel, fEnv : Single;
    fDeltaPhi  : Single;
    fBuffer    : array [0..3] of Single; 
    function filterFreq(Hz: Double): Double;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TTrackerDataModule.ParameterModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: PreDefined := 'SINE';
  1: PreDefined := 'SQUARE';
  2: PreDefined := 'SAW';
  3: PreDefined := 'RING';
  4: PreDefined := 'EQ';
 end;
end;

procedure TTrackerDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 //inits here!
 Parameter[0] := 0; //fMode
 Parameter[1] := 1; //Dynamics
 Parameter[2] := 1; //Mix
 Parameter[3] := 0.97; //Tracking
 Parameter[4] := 0.5; //Trnspose
 Parameter[5] := 0.8; //Maximum Hz
 Parameter[6] := 0.5; //Trigger dB
 Parameter[7] := 0.5; //Output

 res1 := cos(0.01); //p
 res2 := sin(0.01); //q
*)

 fMin := SampleRate / 30; //lower limit
 fDeltaPhi := 100 / SampleRate;  // Initial Pitch
end;

function TTrackerDataModule.filterFreq(Hz: Double) : Double;
var
  j, k, r : Double;
begin
 r := 0.999;
 j := r * r - 1;
 k := 2 - 2 * sqr(r) * cos(0.647 * Hz / SampleRate);
 result := (sqrt(k * k - 4 * j * j) - k) / (2 * j);
end;

procedure TTrackerDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 fMode := round(Parameter[0] * 4.9);
(*
 fo := filterFreq(50);
 fi := sqr(1 - fo);
 ddphi := sqr(Parameter[3]);
*)
 fThreshold := Power(10, 3 * Parameter[6] - 3.8);
 fWet := Power(10, 2 * Parameter[7] - 1);
 fMax := round(SampleRate / Power(10, 1.6 + 2.2 * Parameter[5]));
 fTrans := Power(1.0594631, round(72 * Parameter[4] - 36));

 if (fMode < 4) then
  begin
   fDyn := fWet * 0.6 * Parameter[2] * Parameter[1];
   fDry := fWet * sqrt(1 - Parameter[2]);
   fWet := fWet * 0.3 * Parameter[2] * (1 - Parameter[1]);
  end
 else
  begin
   fDry := fWet * (1 - Parameter[2]);
   fWet := fWet * (0.02 * Parameter[2] - 0.004);
   fDyn := 0;
  end;
 fRel := Power(10, -10 / SampleRate);
end;

procedure TTrackerDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample                 : Integer;
  a, b, x, t, p, dp,
  mn, ddp, tmp, tmp2     : Single;
  o, i, b1, b2, b3, b4   : Single;
  m, we, dr, bo, r1, r2  : Single;
  sw, dsw, dy, e, re     : Single;
  n, s, mo               : Integer;
const
  TwoPi : Single = 6.2831853;
begin
 t   := fThreshold;
 p   := fPhi;
 dp  := fDeltaPhi;
(*
 ddp := ddphi;
 o   := fo;
 i   := fi;
 b1  := fBuffer[0];
 b2  := fBuffer[1]
 bo  := fBold;
 r1  := res1;
 r2  := res2;
 b3  := fBuffer[2];
 b4  := fBuffer[3];
 sw  := saw;
 dsw := dsaw;
 re  := rel;
 n   := num;
 s   := sig;
*)
 we  := fWet;
 dr  := fDry;
 mn  := fMin;
 m   := fMax;
 dy  := fDyn;
 e   := fEnv;
 mo  := fMode;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample];
   x := a; // + b;

   if (x > 0)                                   // Dynamics Envelope
    then tmp :=  x
    else tmp := -x;
   if (tmp > e)
    then e := 0.5 * (tmp + e)
    else e := e * re;

   b1 := o * b1 + i * x;
   b2 := o * b2 + b1;                              // low-pass filter

   if b2 > t then                                  // if >thresh
    begin
     if s < 1 then                                 // and was <thresh
      begin
       if n < mn then                              // not long ago
        begin
(*
         tmp2 := b2 / (b2 - bo);                   // update period
         tmp  := fTrans * TwoPi / (n + dn - tmp2);
         dp   := dp + ddp * (tmp - dp);
         dn   := tmp2;
*)
         dsw  := 0.3183098 * dp;
         if fMode = 4 then
          begin
           r1 := cos(4 * dp); //resonator
           r2 := sin(4 * dp);
          end;
        end;
        n := 0; //restart period measurement
      end;
     s := 1;
    end else
   if n > m then s := 0; //now <thresh
   inc(n);
   bo := b2;

   p := f_mod(p + dp, 2 * Pi);
   case mo of
     0 : x := sin(p);                 // Sine
     1 : if (sin(p) > 0)              // Square
          then x :=  0.5
          else x := -0.5;

     2 : begin                        // Saw
          sw := f_mod(sw + dsw, 2);
          x  := sw - 1;
         end;
     3 : x := x * sin(p);             // Ring

       //filt
     4: begin
         x  := x + (b3 * r1) - (b4 * r2);
         b4 := 0.996 * ((b3 * r2) + (b4 * r1));
         b3 := 0.996 * x;
        end; 
    end;
    x := x * (we + dy * e);
    Outputs[0, Sample] := a;           // dr * a + x;
    Outputs[1, Sample] := dr * b + x;
  end;

  if abs(b1) < 1E-10 then
   begin
    fBuffer[0] := 0;
    fBuffer[1] := 0;
    fBuffer[2] := 0;
    fBuffer[3] := 0;
   end
  else
   begin
    fBuffer[0] := b1;
    fBuffer[1] := b2;
    fBuffer[2] := b3;
    fBuffer[3] := b4;
   end;

(*
  sig   := s;
  fBold := bo;

  if n > 100000
   then num := 100000
   else num := n;

  saw   := sw;
  dsaw  := dsw;
  res1  := r1;
  res2  := r2;
*)

 fPhi      := p;
 fDeltaPhi := dp;
 fEnv      := e;
end;

end.

(*
void mdaTracker::getParameterDisplay(VstInt32 index, char *text)
begin
 case index of
  1: long2string(round(100 * Parameter[1]), text); break;
  2: long2string(round(100 * Parameter[2]), text); break;
  3: long2string(round(100 * Parameter[3]), text); break;
  4: long2string(round( 72 * Parameter[4] - 36), text); break;
  5: long2string(round(SampleRate / fMax), text); break;
  6: long2string(round( 60 * Parameter[6] - 60), text); break;
  7: long2string(round( 40 * Parameter[7] - 20), text); break;
 end;
end;
*)
