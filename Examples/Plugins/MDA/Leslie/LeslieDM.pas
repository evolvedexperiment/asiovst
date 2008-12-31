unit LeslieDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TLeslieDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
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
    FGain : Single;
    FFilo : Single;
    FLWid : Single;
    FLLev : Single;
    FHWid : Single;
    FHDep : Single;
    FHLev : Single;
    FHMom : Single;
    FLMom : Single;
    FHSet : Single;
    FLSet : Single;
    FLSpd : Single;
    FHSpd : Single;
    FLPhi : Single;
    FHPhi : Single;
    FHPos : Integer;
    FHBuf : PDAVSingleFixedArray;
    FBuf  : Array [0..1] of Single;
    FHBufferSize : Integer;
    procedure GainChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TLeslieDataModule.ParameterHighWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHWid := sqr(Parameter[3]);
end;

procedure TLeslieDataModule.ParameterXOverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := 0.4 * Power(10, 2 * Parameter[6] - 1);
 GainChanged;
end;

procedure TLeslieDataModule.ParameterLowWidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLWid := sqr(Parameter[6]);
end;

procedure TLeslieDataModule.GainChanged;
begin
 FLLev := FGain * 0.9 * sqr(Parameter[7]);
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
 FHDep := sqr(Parameter[4]) * SampleRate / 760;
end;

procedure TLeslieDataModule.ParameterHighThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHLev := FGain * 0.9 * sqr(Parameter[5]);
end;

procedure TLeslieDataModule.ParameterLowThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFilo := 1 - Power(10, Value * (0.0227 - 0.000054 * Value) - 1.92);
end;

procedure TLeslieDataModule.ParamSpeedDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.1 then PreDefined := 'STOP' else
 if Parameter[Index] < 0.5
  then PreDefined := 'SLOW'
  else PreDefined := 'FAST';
end;

procedure TLeslieDataModule.VSTModuleOpen(Sender: TObject);
begin
 FLSpd := 0;
 FHSpd := 0;
 FLPhi := 0;
 FHPhi := 1.6;

 FHBufferSize := 256 * SizeOf(Single);
 FHPos := 0;
 GetMem(FHBuf, FHBufferSize);
end;

procedure TLeslieDataModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FHBuf);
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
     FLSet := 0.00; FHSet := 0.0;
     FLMom := 0.12; FHMom := 0.1;
    end
   else //low speed
    begin
     FLSet := 0.49; FHSet := 0.66;
     FLMom := 0.27; FHMom := 0.18;
    end;
  end
 else //high speed
  begin
    FLSet := 5.31; FHSet := 6.40;
    FLMom := 0.14; FHMom := 0.09;
  end;

 spd   := 4 * Pi * Parameter[8] / SampleRate;

 FHMom := Power(10, -1 / (SampleRate * FHMom));
 FLMom := Power(10, -1 / (SampleRate * FLMom));
 FHSet := FHSet * spd;
 FLSet := FLSet * spd;
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
 g   := FGain;
 fo  := FFilo;
 fb1 := FBuf[0];
 fb2 := FBuf[1];
 hl  := FHLev;
 hs  := FHSpd;
 hm  := FHMom;
 hp  := FHPhi;
 hw  := FHWid;
 hd  := FHDep;
 ll  := FLLev;
 ls  := FLSpd;
 lm  := FLMom;
 lp  := FLPhi;
 lw  := FLWid;
 k0  := 0.03125;
 k1  := 32;
 k   := 0;
 hps := FHPos;

 ht := FHSet * (1 - hm); //target speeds
 lt := FLSet * (1 - lm);

 chp := cos(hp);
 chp := chp * chp * chp; //set LFO values
 clp := cos(lp);
 shp := sin(hp);
 slp := sin(lp);
 dchp := 0;
 dclp := 0;

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

    FHBuf[hps] := h; //delay input
    a := FHBuf[hdd];
    h := h + a + hint * (FHBuf[hdd2] - a); //delay output

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

 FLSpd := ls;
 FHSpd := hs;
 FHPos := hps;
 FLPhi := (lp + (k1 - k) * ls);
 while FLPhi > 2 * Pi do FLPhi := FLPhi - 2 * Pi;
 FHPhi := (hp + (k1 - k) * hs);
 while FHPhi > 2 * Pi do FHPhi := FHPhi - 2 * Pi;


 if (abs(fb1) > 1E-10) then FBuf[0] := fb1 else FBuf[0] := 0; //catch denormals
 if (abs(fb2) > 1E-10) then FBuf[1] := fb2 else FBuf[1] := 0;
end;

end.