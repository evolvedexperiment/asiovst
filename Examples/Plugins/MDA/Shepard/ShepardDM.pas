unit ShepardDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TShepardDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fMax    : Integer;
    fBuffer : Array [0..1] of PDAVSingleFixedArray;
    fOut    : Single;
    fPos    : Single;
    fRate   : Single;
    fDRate  : Single;
    fMode   : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TShepardDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[0]) of
  0: PreDefined := 'TONES';
  1: PreDefined := 'RING MOD';
  2: PreDefined := 'TONES + IN';
 end;
end;

procedure TShepardDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOut := 0.4842 * dB_to_Amp(Value - 1);
end;

procedure TShepardDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDRate := 1 + 10 * Power(0.01 * Value - 0.5, 3) / SampleRate;
end;

procedure TShepardDataModule.ParameterModeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMode := round(Value);
end;

procedure TShepardDataModule.VSTModuleCreate(Sender: TObject);
var
  i, j : Integer;
  x, a : Single;
const
  Twopi = 6.2831853;
begin
(*
 //inits here!
 Parameter[0] := 0.2; // Mode
 Parameter[1] := 0.7; // Rate
 Parameter[2] := 0.5; // Level
*)

 fMax := 512 * SizeOf(Single);
 GetMem(fBuffer[0], fMax);
 GetMem(fBuffer[1], fMax);
 for i := 0 to fMax - 1 do
  begin
   fPos := (2 * Pi * i / (fMax - 1)); //generate wavetables
   x    := 0;
   a    := 1;
   fBuffer[1, fMax] := sin(fPos);
   for j := 0 to 7 do
    begin
     x   := x + a * sin(f_mod(fPos, twopi));
     a   := a * 0.5;
     fPos := fPos * 2;
    end;
    fBuffer[0, fMax] := x;
  end;
 i := 511;
 fBuffer[0, i] := 0;
 fBuffer[1, i] := 0; // wrap end for interpolation
 fPos  := 0;
 fRate := 1;
end;

procedure TShepardDataModule.VSTModuleDestroy(Sender: TObject);
begin
 Dispose(fBuffer[0]);
 Dispose(fBuffer[1]);
end;

procedure TShepardDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample       : Integer;
  a, b         : Single;
  r, p, di     : Single;
  x, m, i1, i2 : Integer;
begin
 r := fRate;
 p := fPos;
 x := fMax;
 m := fMode;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample] + Inputs[1, Sample];

   r := r * fDRate;
   if r > 2 then
    begin
     r := r * 0.5;
     p := p * 0.5;
    end
   else if r < 1 then
    begin
     r := r * 2;
     p := p * 2;
     if (p > x)
      then p := p - x;
    end;

   p := p + r;
   if (p > x)
    then p := p - x;

   i1 := round(p); //interpolate position
   i2 := i1 + 1;
   di := i2 - p;

   b :=          di  * (fBuffer[0, i1] + (r - 2) * fBuffer[1, i1]);
   b := b + (1 - di) * (fBuffer[0, i2] + (r - 2) * fBuffer[1, i2]);
   b := b * fOut / r;

   if (m > 0) then
    if (m = 2)
     then b := b + 0.5 * a
     else b := b * a; //ring mod or add

   Outputs[0, Sample] := b;
   Outputs[1, Sample] := b;
  end;

 fPos  := p;
 fRate := r;
end;

end.
