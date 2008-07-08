unit DitherDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TDitherDataModule = class(TVSTModule)
    procedure ParamDitherDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fWordLength : Integer;
    fBits       : Single;
    fGain       : Single;
    fOffset     : Single;
    fDither     : Single;
    fShaper     : Single;
    fShapeState : Array [0..3] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDitherDataModule.ParamDitherDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
   0 : PreDefined := 'OFF';
   1 : PreDefined := 'TRI';
   2 : PreDefined := 'HP-TRI';
  else PreDefined := 'N.SHAPE';
 end;
end;

procedure TDitherDataModule.VSTModuleCreate(Sender: TObject);
begin
 fShapeState[0] := 0;
 fShapeState[1] := 0;
 fShapeState[2] := 0;
 fShapeState[3] := 0;
(*
 rnd1 := 0;
 rnd3 := 0;
*) 
end;

procedure TDitherDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 fGain := 1;
 fBits := 8 + 2 * trunc(8.9 * Parameter[0]);

 if (Parameter[4] > 0.1) then //zoom to 6 bit & fade out audio
  begin
   fWordLength := 32;
   fGain := sqr(1 - Parameter[4]);
  end
 else fWordLength := round(Power(2, fBits - 1)); //word length in quanta

 //Using WaveLab 2.01 (unity gain) as a reference:
 //  16-bit output is floor(floating_point_value * 32768)

 fOffset := (4 * Parameter[3] - 1.5) / fWordLength;   //DC offset (plus 0.5 to round dither not truncate)
 fDither := 2 * Parameter[2] / (fWordLength * 32767);
 fShaper := 0;

 case round(Parameter[0]) of //dither mode
  0: fDither := 0;     //off
  3: fShaper := 0.5;   //noise shaping
 end;
end;

procedure TDitherDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample             : Integer;
  a, b, aa, bb       : Single;
  sl, s1, s2, s3, s4 : Single;  // shaping level, buffers
  dl                 : Single;  // dither level
  o, w, wi           : Single;  // DC offset, word length & inverse
  g                  : Single;  // gain for Zoom mode
  r1, r2, r3, r4     : Integer; // random numbers for dither
  m                  : Integer; // dither mode
begin
(*
  sl := shap;
  s1 := fShapeState[0];
  s2 := fShapeState[1];
  s3 := fShapeState[2];
  s4 := fShapeState[3];
  dl := dith;
  o  := offs,
  w  := wlen;
  wi := 1 / wlen;
  g  := gain;
  r1 := rnd1;
  r3 := rnd3;
  m := 1;
  if (round(fParam1 * 3.9f) == 1)
   then m=0;
*)

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
   (*
    a = *++in1;
    b = *++in2;

    r2 := r1;
    r4 := r3; //HP-TRI dither (also used when noise shaping)
    if(m==0) { r4=rand() & 0x7FFF; r2=(r4 & 0x7F)<<8; } //TRI dither
               r1=rand() & 0x7FFF; r3=(r1 & 0x7F)<<8;   //Assumes RAND_MAX=32767?

    a  = g * a + sl * (s1 + s1 - s2);    //target level + error feedback
    aa = a + o + dl * (float)(r1 - r2);  //             + offset + dither
    if(aa<0.0f) aa-=wi;                 //(long) truncates towards zero!
    aa = wi * (float)(long)(w * aa);    //truncate
    s2 = s1;
    s1 = a - aa;                        //error feedback: 2nd order noise shaping

    b  = g * b + sl * (s3 + s3 - s4);
    bb = b + o + dl * (float)(r3 - r4);
    if(bb<0.0f) bb-=wi;
    bb = wi * (float)(long)(w * bb);
    s4 = s3;
    s3 = b - bb;

    *++out1 = aa;
    *++out2 = bb;
   *)
  end;

(*
  fShapeState[0]  := s1;
  fShapeState[1]  := s2;
  fShapeState[2]  := s3;
  fShapeState[3]  := s4;                     //doesn't actually matter if these are
  rnd1 := r1;
  rnd3 := r3;                     //saved or not as effect is so small !
*)
end;

end.

(*
void mdaDither::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)bits, text); break;
    case 2: float2strng(4.0f * fParam2, text); break;
    case 3: float2strng(4.0f * fParam3 - 2.0f, text); break;
    case 4: if(fParam4>0.1f) 
            if(gain<0.0001f) strcpy(text, "-80");
                        else long2string((long)(20.0 * log10(gain)), text);
                        else strcpy(text, "OFF"); break;
  }
}
*)
