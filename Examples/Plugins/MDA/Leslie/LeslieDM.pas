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
  private
    fGain : Single;
    fFilo : Single;
    fLWid : Single;
    fLLev : Single;
    fHWid : Single;
    fHDep : Single;
    fHLev : Single;
    fBuf  : Array [0..1] of Single;
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

procedure TLeslieDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here!
 fFilo := 1 - Power(10, 0.01 * Parameter[2] * (2.27 - 0.54 * 0.01 * Parameter[2]) - 1.92);

(*
 if (Parameter[0] < 0.5) then
  begin
   if (Parameter[0] < 0.1) then //stop
    begin
     lset := 0.00; hset := 0.00;
     lmom := 0.12; hmom := 0.10;
    end
   else //low speed
    begin
     lset := 0.49; hset := 0.66;
     lmom := 0.27; hmom := 0.18:;
    end;
  end
 else //high speed
  begin
    lset := 5.31; hset := 6.40;
    lmom := 0.14; hmom := 0.09:;
  end;

 hmom := Power(10, -ifs / hmom);
 lmom := Power(10, -ifs / lmom);
 hset := hset * spd;
 lset := lset * spd;
*)

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

begin
 g   := fGain;
 fo  := fFilo;
(*
 fb1 := fBuf[0];
 fb2 := fBuf[1];
 hl  := fHLev;
 hs  := fHSpd;
 hm  := hmom;
 hp  := hphi;
 hw  := fHWid;
 hd  := fHDep;
 ll  := fLLev;
 ls  := fLSpd;
 lm  := lmom;
 lp  := lphi;
 lw  := fLWid;
 k0  := 0.03125;
 k1  := 32;
 k   := 0;
 hps := hpos;

 ht := hset*(1.f-hm); //target speeds
 lt := lset*(1.f-lm);

 chp := (float)cos(hp);
 chp := chp * chp * chp; //set LFO values
 clp := (float)cos(lp);
 shp := (float)sin(hp);
 slp := (float)sin(lp);

 for Sample := 0 to SampleFrames - 1 do
  begin
   a = inputs[0, Sample] + inputs[1, Sample]; //mono input

   if(k) k--; else //linear piecewise approx to LFO waveforms
    {
      ls := (lm * ls) + lt; //tend to required speed
      hs := (hm * hs) + ht;
      lp := lp + k1 * ls;
      hp := hp + k1 * hs;

      dchp := (float)cos(hp + k1*hs);
      dchp := k0 * (dchp * dchp * dchp - chp); //sin^3 level mod
      dclp := k0 * ((float)cos(lp + k1*ls) - clp);
      dshp := k0 * ((float)sin(hp + k1*hs) - shp);
      dslp := k0 * ((float)sin(lp + k1*ls) - slp);

      k := (long)k1;
    }

    fb1 := fo * (fb1 - a) + a; //crossover
    fb2 := fo * (fb2 - fb1) + fb1;
    h   := (g - hl * chp) * (a - fb2); //volume
    l   := (g - ll * clp) * fb2;

    if(hps>0) hps--; else hps=200;  //delay input pos
    hint := hps + hd * (1 + chp);  //delay output pos
    hdd  := (int)hint;
    hint := hint - hdd; //linear intrpolation
    hdd2 := hdd + 1;
    if (hdd > 199) then
     begin
      if (hdd > 200)
       then hdd := hdd - 201;
      hdd2 := hdd2 - 201;
     end;

    *(hbuf + hps) = h; //delay input
    a = *(hbuf + hdd);
    h := h + a + hint * ( *(hbuf + hdd2) - a); //delay output

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

 lspd := ls;
 hspd := hs;
 hpos := hps;
 lphi := (float)fmod(lp+(k1-k)*ls,twopi);
 hphi := (float)fmod(hp+(k1-k)*hs,twopi);

 if (abs(fb1) > 1E-10) then fBuf[0] := fb1 else fBuf[0] := 0; //catch denormals
 if (abs(fb2) > 1E-10) then fBuf[1] := fb2 else fBuf[1] := 0;
*)
end;

end.