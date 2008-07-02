unit DitherDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TDitherDataModule = class(TVSTModule)
    procedure ParamDitherDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

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
(*
 sh1 = sh2 = sh3 = sh4 = 0.0f;
 rnd1 = rnd3 = 0;
*) 
end;

procedure TDitherDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
(*
  //calcs here
  fGain := 1;
  fBits := 8 + 2 * (float)floor(8.9f * fParam0);

  if (fParam4 > 0.1f) then //zoom to 6 bit & fade out audio
   begin
    wlen := 32;
    gain := sqr(1 - fParam4);
   end
  else wlen := Power(2, bits - 1); //word length in quanta

  //Using WaveLab 2.01 (unity gain) as a reference:
  //  16-bit output is floor(floating_point_value * 32768)

  offs := (4 * fParam3 - 1.5) / wlen; //DC offset (plus 0.5 to round dither not truncate)
  dith := 2 * fParam2 / (wlen * 32767);
  shap := 0;

  case round(Parameter[0]) of //dither mode
   0: dith := 0;   //off
   3: shap := 0.5; //noise shaping
  end;
*)
end;

procedure TDitherDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, aa, bb;
  float sl=shap, s1=sh1, s2=sh2, s3=sh3, s4=sh4; //shaping level, buffers
  float dl=dith;                                 //dither level
  float o=offs, w=wlen, wi=1.0f/wlen;            //DC offset, word length & inverse
  float g=gain;                                  //gain for Zoom mode
  long  r1=rnd1, r2, r3=rnd3, r4;                //random numbers for dither
  long  m=1;                                     //dither mode
  if((long)(fParam1 * 3.9f)==1) m=0;             //what is the fastest if(?)

  --in1;
  --in2;
  --out1;
  --out2;

  while(--sampleFrames >= 0)
  {
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
  }
  
  sh1=s1; sh2=s2; sh3=s3; sh4=s4; //doesn't actually matter if these are
  rnd1=r1; rnd3=r3;               //saved or not as effect is so small !
*)
end;

end.