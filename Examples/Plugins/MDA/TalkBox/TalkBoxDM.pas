unit TalkBoxDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

const
  CMaxBufferSize = 1600;
  CMaxOrder      =   50;

type
  TTalkBoxDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure ParameterCarrierDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fBuf       : array [0..1] of PDAVSingleFixedArray;
    fCar       : array [0..1] of PDAVSingleFixedArray;
    fWindow    : PDAVSingleFixedArray;
    fWinSize   : Integer;
    fEmphasis  : Single;
    fFX        : Single;
    fWet, fDry : Single;
    fPos       : Integer;
    fSwap      : Integer;
    fK, fO     : Integer;
    fD, fU     : Array [0..4] of Single;
    procedure LPC(buf, car: PDAVSingleFixedArray; n, o: Integer);
    procedure LPCDurbin(r : PDAVSingleFixedArray; p : Integer; k : PDAVSingleFixedArray; var g: Single);
  public
  end;

implementation

{$R *.DFM}

procedure TTalkBoxDataModule.ParameterCarrierDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Left'
  else PreDefined := 'Right';
end;

procedure TTalkBoxDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 100; // Wet  [%]
 Parameter[1] := 0;   // Dry  [%]
 Parameter[2] := 0;   // Swap
(*
 Parameter[3] := 1;   // Quality
*)

 ///initialise...
 GetMem(fBuf[0], CMaxBufferSize * SizeOf(Single));
 GetMem(fBuf[1], CMaxBufferSize * SizeOf(Single));
 GetMem(fWindow, CMaxBufferSize * SizeOf(Single));
 GetMem(fCar[0], CMaxBufferSize * SizeOf(Single));
 GetMem(fCar[1], CMaxBufferSize * SizeOf(Single));
 fWinSize := 1; //trigger window recalc
 fK := 0;
 VSTModuleSuspend(Sender);
end;

procedure TTalkBoxDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuf[0]) then Dispose(fBuf[0]);
 if assigned(fBuf[1]) then Dispose(fBuf[1]);
 if assigned(fWindow) then Dispose(fWindow);
 if assigned(fCar[0]) then Dispose(fCar[0]);
 if assigned(fCar[1]) then Dispose(fCar[1]);
end;

procedure TTalkBoxDataModule.LPC(buf, car : PDAVSingleFixedArray; n, o : Integer);
var
  z, r, k  : array [0..CMaxOrder - 1] of Single;
  G, x     : Single;
  i, j, nn : Integer;
  min      : Single;
begin
 nn := n;

 for j := 0 to o do  //buf[] is already emphasized and windowed
  begin
   z[j] := 0;
   r[j] := 0;
   for i := 0 to nn - 1
    do r[j] := r[j] + Buf[i] * Buf[i + j]; //autocorrelation
   dec(nn);
  end;
 r[0] := 1.001 * r[0];  //stability fix

 min := 0.00001;
 if r[0] < min then
  begin
   for i := 0 to n - 1
    do buf[i] := 0.0;
   exit;
  end;

 LPCDurbin(@r, o, @k, G);  //calc reflection coeffs

 for i := 0 to o do
  begin
   if (k[i] > 0.995)
    then k[i] := 0.995 else
   if (k[i] < -0.995) then k[i] := -0.995;
  end;

 for i := 0 to n - 1 do
  begin
   x := G * car[i];
   for j := o downto 1 do  //lattice filter
    begin
     x    := x - k[j] * z[j - 1];
     z[j] := z[j - 1] + k[j] * x;
    end;
   buf[i] := x;
   z[0]   := x;  //output buf[] will be windowed elsewhere
  end;
end;


procedure TTalkBoxDataModule.LPCDurbin(r : PDAVSingleFixedArray; p : Integer; k : PDAVSingleFixedArray; var g: Single);
var
  i, j  : Integer;
  a, at : array [0..CMaxOrder - 1] of Single;
  e     : Single;
begin
 e := r[0];

 for i := 0 to p do
  begin
   a[i]  := 0;
   at[i] := 0;
  end;  //probably don't need to clear at[] or k[]

 for i := 1 to p do
  begin
   k[i] := -r[i];

   for j := 1 to i - 1 do
    begin
     at[j] := a[j];
     k[i] := k[i] - (a[j] * r[i-j]);
    end;
   if (abs(e) < 1E-20) then
    begin
     e := 0;
     break;
    end;

   k[i] := k[i] / e;

   a[i] := k[i];
   for j := 1 to i - 1
    do a[j] := at[j] + k[i] * at[i-j];

    e := e * (1 - sqr(k[i]));
  end;

 if (e < 1E-20) then e := 0;
 g := sqrt(e);
end;

procedure TTalkBoxDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample        : Integer;
  p, q, dr, FX  : Single;
  e, w, o, x    : Single;
  p0, p1        : Integer;
const
  den : Single = 1E-10;
  h0  : Single = 0.3;
  h1  : Single = 0.77;
begin
 p0 := fPos;
 p1 := (fPos + fWinSize div 2) mod fWinSize;
 e  := fEmphasis;
 FX := fFX;

 for Sample := 0 to SampleFrames - 1 do
  begin
   o  := Inputs[    fSwap, Sample];
   x  := Inputs[1 - fSwap, Sample];
   dr := o;

   p := fD[0] + h0 *  x;    fD[0] := fD[1];  fD[1] := x  - h0 * p;
   q := fD[2] + h1 * fD[4]; fD[2] := fD[3];  fD[3] := fD[4] - h1 * q;
   fD[4] := x;
   x := p + q;

   if fK > 0 then
    begin
     fK := 0;

     fCar[0][p0] := x;
     fCar[1][p1] := x;              // carrier input

     x := o - e;
     e := o;                        // 6dB / oct pre-emphasis

     w := fWindow[p0];
     fFX := fBuf[0, p0] * w;
     fBuf[0, p0] := x * w;          // 50% overlapping hanning windows

     inc(p0);
     if p0 >= fWinSize then
      begin
       LPC(fBuf[0], fCar[0], fWinSize, fO);
       p0 := 0;
      end;

     w := 1 - w;
     fFX := fFX + fBuf[1][p1] * w;
     fBuf[1][p1] := x * w;

     inc(p1);
     if p1 >= fWinSize then
      begin
       lpc(fBuf[1], fCar[1], fWinSize, fO);
       p1 := 0;
      end;
    end;
   inc(fK);

   p := fU[0] + h0 * fFX;   fU[0] := fU[1];  fU[1] := fFX - h0 * p;
   q := fU[2] + h1 * fU[4]; fU[2] := fU[3];  fU[3] := fU[4] - h1 * q;
   fU[4] := fFX;
   x := p + q;

   o := fWet * x + fDry * dr;

   Outputs[0, Sample] := o;
   Outputs[1, Sample] := o;
  end;

  fEmphasis := e;
  fPos := p0;
  fFX := FX;

  if (abs(fD[0]) < den) then fD[0] := 0; //anti-denormal (doesn't seem necessary but P4?)
  if (abs(fD[1]) < den) then fD[1] := 0;
  if (abs(fD[2]) < den) then fD[2] := 0;
  if (abs(fD[3]) < den) then fD[3] := 0;
  if (abs(fU[0]) < den) then fU[0] := 0;
  if (abs(fU[1]) < den) then fU[1] := 0;
  if (abs(fU[2]) < den) then fU[2] := 0;
  if (abs(fU[3]) < den) then fU[3] := 0;
end;

procedure TTalkBoxDataModule.VSTModuleResume(Sender: TObject);
var
  fs    : Single;
  n     : Integer;
  p, dp : Single;
begin
 fs := SampleRate;
 if (fs <  8000) then fs :=  8000.0;
 if (fs > 96000) then fs := 96000.0;

 if Parameter[2] > 0.5
  then fSwap := 1
  else fSwap := 0;

 n := round(0.01633 * fs);
 if (n > CMaxBufferSize)
  then n := CMaxBufferSize;

 //fO = round(0.0005 * fs);
 fO := round((0.0001 + 0.0004 * Parameter[3]) * fs);

 if (n <> fWinSize) then //recalc hanning window
  begin
   fWinSize := n;
   dp := 2 * Pi / fWinSize;
   p  := 0;
   for n := 0 to N - 1 do
    begin
     fWindow[n] := 0.5 - 0.5 * cos(p);
     p := p + dp;
    end;
  end;

 fWet := 0.5 * Sqr(0.01 * Parameter[0]);
 fDry := 2   * Sqr(0.01 * Parameter[1]);
end;

procedure TTalkBoxDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fPos := 0;
 fK   := 0;
 fU[0] := 0;
 fU[1] := 0;
 fU[2] := 0;
 fU[3] := 0;
 fU[4] := 0;
 fD[0] := 0;
 fD[1] := 0;
 fD[2] := 0;
 fD[3] := 0;
 fD[4] := 0;

 fEmphasis := 0;
 fFX := 0;

 FillChar(fBuf[0]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(fBuf[1]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(fCar[0]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(fCar[1]^, CMaxBufferSize * SizeOf(Single), 0);
end;

end.

(*
void mdaTalkBox::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
   begin
    case 3: sprintf(string, "%4.0f", 5.0 + 95.0 * Parameter[index] * Parameter[index]); break;
    default: sprintf(string, "%4.0f %%", 200.0 * Parameter[index]);
   end;
end;
*)
