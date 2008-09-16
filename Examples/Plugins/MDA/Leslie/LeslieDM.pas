unit LeslieDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TLeslieDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamSpeedDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterXOverChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
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
    fHBuf : PDAVSingleFixedArray;
    fBuf  : Array [0..1] of Single;
    fHBufferSize : Integer;
    procedure GainChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TLeslieDataModule.ParameterHighWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHWid := sqr(Parameter[3]);
end;

procedure TLeslieDataModule.ParameterXOverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fGain := 0.4 * Power(10, 2 * Parameter[6] - 1);
 GainChanged;
end;

procedure TLeslieDataModule.ParameterLowWidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLWid := sqr(Parameter[6]);
end;

procedure TLeslieDataModule.GainChanged;
begin
 fLLev := fGain * 0.9 * sqr(Parameter[7]);
end;

procedure TLeslieDataModule.ParameterSpeedChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 GainChanged;
end;

procedure TLeslieDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 GainChanged;
end;

procedure TLeslieDataModule.ParameterHighDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHDep := sqr(Parameter[4]) * SampleRate / 760;
end;

procedure TLeslieDataModule.ParameterHighThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHLev := fGain * 0.9 * sqr(Parameter[5]);
end;

procedure TLeslieDataModule.ParameterLowThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fFilo := 1 - Power(10, Value * (0.0227 - 0.000054 * Value) - 1.92);
end;

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
  spd : Single;
begin
 //calcs here!
 if (Parameter[0] < 0.5) then
  begin
   if (Parameter[0] < 0.1) then //stop
    begin
     fLSet := 0.00; fHSet := 0.0;
     fLMom := 0.12; fHMom := 0.1;
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

 spd   := 4 * Pi * Parameter[8] / SampleRate;

 fHMom := Power(10, -1 / (SampleRate * fHMom));
 fLMom := Power(10, -1 / (SampleRate * fLMom));
 fHSet := fHSet * spd;
 fLSet := fLSet * spd;
end;

procedure TLeslieDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
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