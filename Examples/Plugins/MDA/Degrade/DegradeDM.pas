unit DegradeDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TDegradeDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDegradeDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
(*
  //calcs here
  if (fParam3 > 0.5) then
   begin
    f = fParam3 - 0.5f;
    mode = 1.0f;
   end
  else
   begin
    f = 0.5f - fParam3;
    mode = 0.0f;
   end; 

  tn = (int)exp(18.0f * f);

  //tn = (int)(18.0 * fParam3 - 8.0); mode=1.f; end;
  //         else begin tn = (int)(10.0 - 18.0 * fParam3); mode=0.f; end;

  tcount = 1;
  clp  := Power(10, (-1.5 + 1.5 * fParam1));
  fo2  := filterFreq(Power(10, 2.30104f + 2.f*fParam4));
  fi2  := sqr(sqr(1.f-fo2));
  float _g1 = Power(2.0,2.0 + int(fParam2*12.0));
  g2   := 1 / (2 * _g1));
  if (fParam3 > 0.5)
   then g1 := -_g1 / tn
   else g1 := -_g1;
  g3 = Power(10, 2 * fParam6 - 1.0);
  if (fParam5 > 0.5) then
   begin
    lin  := Power(10, 0.3 * (0.5 - fParam5)));
    lin2 := lin;
   end
  else
   begin
    lin  := Power(10, 0.3 * (fParam5 - 0.5));
    lin2 := 1;
   end; 
*)
end;

procedure TDegradeDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
 float *in1 = inputs[0];
 float *in2 = inputs[1];
 float *out1 = outputs[0];
 float *out2 = outputs[1];
 float b0=buf0, l=lin, l2=lin2;
 float cl=clp, i2=fi2, o2=fo2;
 float b1=buf1, b2=buf2, b3=buf3, b4=buf4, b5=buf5;
 float b6=buf6, b7=buf7, b8=buf8, b9=buf9;
 float gi=g1, go=g2, ga=g3, m=mode;
 int n=tn, t=tcount;

 --in1;
 --in2;
 --out1;
 --out2;
 while(--sampleFrames >= 0)
 begin
    b0 = (*++in1 + *++in2) + m * b0;

    if(t==n)
    begin
      t=0;
      b5 = (float)(go * int(b0 * gi));
      if(b5>0)
      begin b5=Power(b5,l2); if(b5>cl) b5=cl;end;
      else
      begin b5=-Power(-b5, l); if(b5<-cl) b5=-cl; end;
      b0=0;
    end; 
    t=t+1;

    b1 = i2 * (b5 * ga) + o2 * b1;
    b2 =      b1 + o2 * b2;
    b3 =      b2 + o2 * b3;
    b4 =      b3 + o2 * b4;
    b6 = i2 * b4 + o2 * b6;
    b7 =      b6 + o2 * b7;
    b8 =      b7 + o2 * b8;
    b9 =      b8 + o2 * b9;

  *++out1 = b9;
  *++out2 = b9;
 end;
  if (abs(b1) < 1E-10) 
  begin buf1=0.f; buf2=0.f; buf3=0.f; buf4=0.f; buf6=0.f; buf7=0.f;
    buf8=0.f; buf9=0.f; buf0=0.f; buf5=0.f; end;
  else 
  begin buf1=b1; buf2=b2; buf3=b3; buf4=b4; buf6=b6; buf7=b7; 
    buf8=b8, buf9=b9; buf0=b0; buf5=b5; tcount=t; end;
*)
end;

end.