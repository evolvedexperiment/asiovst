unit BandistoDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TBandistoDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TBandistoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, c, d, g, l=fb3, m, h, s, sl=slev;
  float f1i=fi1, f1o=fo1, f2i=fi2, f2o=fo2, b1=fb1, b2=fb2;
  float g1, d1=driv1, t1=trim1;
  float g2, d2=driv2, t2=trim2;
  float g3, d3=driv3, t3=trim3;
  int v=valve;

 for i := 0 to SampleFrames - 1 do
  begin
   a := *++in1;
   b := *++in2; //process from here...

   s  := (a - b) * sl; //keep stereo component for later
   a  := a + (float)(b + 0.00002); //dope filter at low level
   b2 := (f2i * a) + (f2o * b2); //crossovers
   b1 := (f1i * b2) + (f1o * b1);
   l  := (f1i * b1) + (f1o * l);
   m  := b2 - l;
   h  := a - b2;

   g  := (l>0)? l : -l;
   g  := (float)(1.0 / (1.0 + d1 * g) ); //distort
   g1 := g;

   g  := (m>0)? m : -m;
   g  := (float)(1.0 / (1.0 + d2 * g) );
   g2 := g;

   g  := (h>0)? h : -h;
   g  := (float)(1.0 / (1.0 + d3 * g) );
   g3 := g;

   if (v) then
    begin
     if (l > 0) then
      begin
       g1 = 1.0;
       if (m > 0) then g2 := 1.0;
       if (h > 0) then g3 := 1.0;
      end; 

   a  := (l*g1*t1) + (m*g2*t2) + (h*g3*t3);
   c  := a + s; // output
   d  := a - s;

   *++out1 = c;
   *++out2 = d;
 end;
 fb1 := b1;
 fb2 := b2;
 fb3 := l;
*)
end;

end.
