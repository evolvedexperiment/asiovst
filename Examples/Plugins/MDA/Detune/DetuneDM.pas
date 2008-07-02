unit DetuneDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

const
  BUFMAX = 512;

type
  TDetuneDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
  private
    fBuffer : Array [0..1] of PAVDSingleFixedArray;
    fBufLen : Integer;
    fBufRes : Double;
    fPos    : Array [0..2] of Integer;
    fWin    : PAVDSingleFixedArray;
    fSemi   : Double;
    fDPs1   : Double;
    fDPs2   : Double;
  public
  end;

implementation

{$R *.DFM}

procedure TDetuneDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer; 
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, c, d;
  float x, w=wet, y=dry, p1=pos1, p1f, d1=dpos1;
  float                  p2=pos2,      d2=dpos2;
  long  p0=pos0, p1i, p2i;
  long  l=buflen-1, lh=buflen>>1;
  float lf = (float)buflen;

  for Sample := 0 to SampleFrames - 1 do
   begin
    a = *++in1;
    b = *++in2;

    c = y * a;
    d = y * b;

    --p0 &= l;     
    *(buf + p0) = w * (a + b);      //input

    p1 -= d1;
    if(p1<0.0f) p1 += lf;           //output
    p1i = (long)p1;
    p1f = p1 - (float)p1i;
    a = *(buf + p1i);
    ++p1i &= l;
    a += p1f * (*(buf + p1i) - a);  //linear interpolation

    p2i = (p1i + lh) & l;           //180-degree ouptut
    b = *(buf + p2i);
    ++p2i &= l;
    b += p1f * (*(buf + p2i) - b);  //linear interpolation

    p2i = (p1i - p0) & l;           //crossfade
    x = *(win + p2i);
    //++p2i &= l;
    //x += p1f * (*(win + p2i) - x); //linear interpolation (doesn't do much)
    c += b + x * (a - b);

    p2 -= d2;  //repeat for downwards shift - can't see a more efficient way?
    if(p2<0.0f) p2 += lf;           //output
    p1i = (long)p2;
    p1f = p2 - (float)p1i;
    a = *(buf + p1i);
    ++p1i &= l;
    a += p1f * (*(buf + p1i) - a);  //linear interpolation

    p2i = (p1i + lh) & l;           //180-degree ouptut
    b = *(buf + p2i);
    ++p2i &= l;
    b += p1f * (*(buf + p2i) - b);  //linear interpolation

    p2i = (p1i - p0) & l;           //crossfade
    x = *(win + p2i);
    //++p2i &= l;
    //x += p1f * (*(win + p2i) - x); //linear interpolation (doesn't do much)
    d += b + x * (a - b);

    *++out1 = c;
    *++out2 = d;
   end;

 fPos[0] := p0;
 fPos[1] := p1;
 fPos[2] := p2;
*)
end;

procedure TDetuneDataModule.VSTModuleResume(Sender: TObject);
var
  i, tmp : Integer;
  p, dp  : Double;
begin
  fSemi := 3.0 * sqr(Parameter[0]) * Parameter[0];
  fDPs2 := Power(1.0594631, semi);
  fDPs1 := 1 / dpos2;

  fWet := dB_to_Amp(Parameter[2]);
  fDry := fWet * (1 - Parameter[1]) * Parameter[1];
  fWet := fWet * (2 - Parameter[1]) * Parameter[1];

  tmp = 1 shl (8 + round(4.9 * Parameter[3]));

  if (tmp <> fBufLen) then //recalculate crossfade window
   begin
    fBufLen := tmp;
    fBufRes := 1000 * fBufLen / SampleRate;

    //hanning half-overlap-and-add
    p  := 0,
    dp := 2 * Pi / fBufLen;
    for i:=0 to fBufLen - 1 do
     begin
      win[i] := (0.5 - 0.5 * cos(p));
      p := p + dp;
     end;
   end;
end;

procedure TDetuneDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(fBuffer, BUFMAX * SizeOf(Single), 0);
 fPos[0] := 0;
 fPos[1] := 0;
 fPos[2] := 0;
end;

end.