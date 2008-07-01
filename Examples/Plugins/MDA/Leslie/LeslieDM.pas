unit LeslieDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TLeslieDataModule = class(TVSTModule)
    procedure ParamSpeedDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    fGain : Single;
    fFilo : Single;
    fLWid : Single;
    fLLev : Single;
    fHWid : Single;
    fHDep : Single;
    fHLev : Single;
    fHMom : Single;
    fLMom : Single;
    fHSet : Single;
    fLSet : Single;
    fLSpd : Single;
    fHSpd : Single;
    fLPhi : Single;
    fHPhi : Single;
    fHPos : Integer;
    fHBuf : PAVDSingleFixedArray;
    fBuf  : Array [0..1] of Single;
    fHBufferSize : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TLeslieDataModule.ParamSpeedDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.1 then PreDefined := 'STOP' else
 if Parameter[Index] < 0.5
  then PreDefined := 'SLOW'
  else PreDefined := 'FAST';
end;

procedure TLeslieDataModule.VSTModuleCreate(Sender: TObject);
begin
 fLSpd := 0;
 fHSpd := 0;
 fLPhi := 0;
 fHPhi := 1.6;

 fHBufferSize := 256 * SizeOf(Single);
 fHPos := 0;
 GetMem(fHBuf, fHBufferSize);
end;

procedure TLeslieDataModule.VSTModuleDestroy(Sender: TObject);
begin
 Dispose(fHBuf);
end;

procedure TLeslieDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ifs, spd : Single;
begin
 //calcs here!
 fFilo := 1 - Power(10, 0.01 * Parameter[2] * (2.27 - 0.54 * 0.01 * Parameter[2]) - 1.92);

 if (Parameter[0] < 0.5) then
  begin
   if (Parameter[0] < 0.1) then //stop
    begin
     fLSet := 0.00; fHSet := 0.00;
     fLMom := 0.12; fHMom := 0.10;
    end
   else //low speed
    begin
     fLSet := 0.49; fHSet := 0.66;
     fLMom := 0.27; fHMom := 0.18;
    end;
  end
 else //high speed
  begin
    fLSet := 5.31; fHSet := 6.40;
    fLMom := 0.14; fHMom := 0.09;
  end;

 ifs   := 1 / SampleRate;
 spd   := 4 * Pi * ifs * Parameter[7];

 fHMom := Power(10, -ifs / fHMom);
 fLMom := Power(10, -ifs / fLMom);
 fHSet := fHSet * spd;
 fLSet := fLSet * spd;

 fGain := 0.4 * Power(10, 2 * Parameter[1] - 1);
 fLWid := sqr(Parameter[6]);
 fLLev := fGain * 0.9 * sqr(Parameter[8]);
 fHWid := sqr(Parameter[3]);
 fHDep := sqr(Parameter[4]) * SampleRate / 760;
 fHLev := fGain * 0.9 * sqr(Parameter[5]);
end;

procedure TLeslieDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  a, c, d, g, h, l           : Single;
  fo, fb1, fb2               : Single;
  hl, hs, ht, hm, hp, hw, hd : Single;
  ll, ls, lt, lm, lp, lw     : Single;
  hint, k0, k1               : Single;
  hdd, hdd2, k, hps          : Integer;
  dchp, dclp, dshp, dslp     : Single;
  chp, clp, shp, slp         : Single;
  Sample                     : Integer;

begin
 g   := fGain;
 fo  := fFilo;
 fb1 := fBuf[0];
 fb2 := fBuf[1];
 hl  := fHLev;
 hs  := fHSpd;
 hm  := fHMom;
 hp  := fHPhi;
 hw  := fHWid;
 hd  := fHDep;
 ll  := fLLev;
 ls  := fLSpd;
 lm  := fLMom;
 lp  := fLPhi;
 lw  := fLWid;
 k0  := 0.03125;
 k1  := 32;
 k   := 0;
 hps := fHPos;

 ht := fHSet * (1 - hm); //target speeds
 lt := fLSet * (1 - lm);

 chp := cos(hp);
 chp := chp * chp * chp; //set LFO values
 clp := cos(lp);
 shp := sin(hp);
 slp := sin(lp);

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := inputs[0, Sample] + inputs[1, Sample]; //mono input

   if k > 0 then dec(k) else //linear piecewise approx to LFO waveforms
    begin
      ls := (lm * ls) + lt; //tend to required speed
      hs := (hm * hs) + ht;
      lp := lp + k1 * ls;
      hp := hp + k1 * hs;

      dchp := cos(hp + k1 * hs);
      dchp := k0 * (sqr(dchp) * dchp - chp);        //sin^3 level mod
      dclp := k0 * (cos(lp + k1 * ls) - clp);
      dshp := k0 * (sin(hp + k1 * hs) - shp);
      dslp := k0 * (sin(lp + k1 * ls) - slp);

      k := round(k1);
    end;

   fb1 := fo * (fb1 - a) + a; //crossover
   fb2 := fo * (fb2 - fb1) + fb1;
   h   := (g - hl * chp) * (a - fb2); //volume
   l   := (g - ll * clp) * fb2;

   if hps > 0
    then dec(hps)
    else hps := 200;              // delay input pos
   hint := hps + hd * (1 + chp);  // delay output pos
   hdd  := round(hint);
   hint := hint - hdd;            // linear intrpolation
   hdd2 := hdd + 1;
   if (hdd > 199) then
    begin
     if (hdd > 200)
      then hdd := hdd - 201;
     hdd2 := hdd2 - 201;
    end;

    fHBuf[hps] := h; //delay input
    a := fHBuf[hdd];
    h := h + a + hint * (fHBuf[hdd2] - a); //delay output

    c := l + h;
    d := l + h;
    h := h * hw * shp;
    l := l * lw * slp;
    d := d + l - h;
    c := c + h - l;

   outputs[0, Sample] := c; //output
   outputs[1, Sample] := d;

   chp := chp + dchp;
   clp := clp + dclp;
   shp := shp + dshp;
   slp := slp + dslp;
  end;

 fLSpd := ls;
 fHSpd := hs;
 fHPos := hps;
 fLPhi := (lp + (k1 - k) * ls);
 while fLPhi > 2 * Pi do fLPhi := fLPhi - 2 * Pi;
 fHPhi := (hp + (k1 - k) * hs);
 while fHPhi > 2 * Pi do fHPhi := fHPhi - 2 * Pi;


 if (abs(fb1) > 1E-10) then fBuf[0] := fb1 else fBuf[0] := 0; //catch denormals
 if (abs(fb2) > 1E-10) then fBuf[1] := fb2 else fBuf[1] := 0;
end;

end.