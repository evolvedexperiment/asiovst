unit DetuneDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

const
  BUFMAX = 512;

type
  TDetuneDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure DetuneDataModulePrograms1Initialize(Sender: TObject);
    procedure DetuneDataModulePrograms2Initialize(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fBuffer : PDAVSingleFixedArray;
    fBufLen : Integer;
    fBufRes : Double;
    fPos    : Array [0..2] of Integer;
    fDPos   : Array [0..1] of Integer;
    fWin    : PDAVSingleFixedArray;
    fSemi   : Double;
    fDPs1   : Double;
    fDPs2   : Double;
    fWet    : Single;
    fDry    : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDetuneDataModule.DetuneDataModulePrograms1Initialize(Sender: TObject);
begin
 Programs[1].Parameter[0] := 0.20
end;

procedure TDetuneDataModule.DetuneDataModulePrograms2Initialize(Sender: TObject);
begin
 programs[2].Parameter[0] := 0.8;
 programs[2].Parameter[1] := 0.7;
end;

procedure TDetuneDataModule.VSTModuleCreate(Sender: TObject);
begin
 GetMem(fBuffer, BUFMAX * SizeOf(Single));
 GetMem(fWin   , BUFMAX * SizeOf(Single));
 fBufLen := 0;

(*
 Parameter[0] := 0.4;  // Fine
 Parameter[1] := 0.4;  // Mix
 Parameter[2] := 0.5;  // Output
 Parameter[3] := 0.5;  // ChunkSize

 ///differences from default program...
 Programs[3].Parameter[0] = 0.90f;
*)

 VSTModuleSuspend(Sender);
end;

procedure TDetuneDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuffer) then Dispose(fBuffer);
 if assigned(fWin) then Dispose(fWin);
end;

procedure TDetuneDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, p0, p1i, p2i  : Integer;
  x, w, y, p1, p1f, d1  : Single;
  p2, d2, lf            : Single;
  lh, l                 : Integer;
  a, b, c, d            : Single;
begin
 w  := fWet;
 p0 := fPos[0];
 p1 := fPos[1];
 p2 := fPos[2];
 d1 := fDPos[0];
 d2 := fDPos[1];
 l  := fBuflen - 1;
 lh := fBuflen shr 1;
 lf := fBuflen;

 for Sample := 0 to SampleFrames - 1 do
  begin
   c := fDry * Inputs[0, Sample];
   d := fDry * Inputs[1, Sample];

   dec(p0);
   p0 := p0 and l;

   fBuffer[p0] := w * (Inputs[0, Sample] + Inputs[1, Sample]);      //input

   p1 := p1 - d1;
   if p1 < 0 then p1 := p1 + lf;          // output
   p1i := round(p1);
   p1f := p1 - round(p1i);
   a := fBuffer[p1i];
   inc(p1i);
   p1i := p1i and l;
   a := a + p1f * (fBuffer[p1i] - a);  // linear interpolation

   p2i := (p1i + lh) and l;                 // 180-degree ouptut
   b := fBuffer[p2i];
   inc(p2i);
   p2i := p2i and l;
   b := b + p1f * (fBuffer[p2i] - b);    // Linear interpolation

   p2i := (p1i - p0) and l;                 // Crossfade
   x   := fWin[p2i];

(*
   //++p2i &= l;
   //x += p1f * (*(win + p2i) - x); //linear interpolation (doesn't do much)
*)

   c := c + b + x * (a - b);

   p2 := p2 - d2;  //repeat for downwards shift - can't see a more efficient way?
   if (p2 < 0) then p2 := p2 + lf;       // output

   p1i := round(p2);
   p1f := p2 - p1i;
   a   := fBuffer[p1i];
   inc(p1i);
   p1i := p1i and l;
   a := a + p1f * (fBuffer[p1i] - a); // linear interpolation

   p2i := (p1i + lh) and l;              // 180-degree Output
   b   := fBuffer[p2i];
   Inc(p2i);
   p2i := p2i + l;
   b := b + p1f * (fBuffer[p2i] - b); // Linear Interpolation

   p2i := (p1i - p0) and l;              // Crossfade

   x := fWin[p2i];
   //++p2i &= l;
   //x += p1f * (*(win + p2i) - x); //linear interpolation (doesn't do much)
   d := d + b + x * (a - b);

   Outputs[0, Sample] := c;
   Outputs[1, Sample] := d;
  end;

(*
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
  fDPs2 := Power(1.0594631, fSemi);
  fDPs1 := 1 / fDPs2;

  fWet := dB_to_Amp(Parameter[2]);
  fDry := fWet * (1 - Parameter[1]) * Parameter[1];
  fWet := fWet * (2 - Parameter[1]) * Parameter[1];

  tmp  := 1 shl (8 + round(4.9 * Parameter[3]));

  if (tmp <> fBufLen) then //recalculate crossfade window
   begin
    fBufLen := tmp;
    fBufRes := 1000 * fBufLen / SampleRate;

    //hanning half-overlap-and-add
    p  := 0;
    dp := 2 * Pi / fBufLen;
    for i:=0 to fBufLen - 1 do
     begin
      fWin[i] := (0.5 - 0.5 * cos(p));
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

(*
void mdaDetune::getParameterDisplay(VstInt32 index, char *text)
begin
   char string[16];

  switch(index)
  begin
    case  1: sprintf(string, "%.0f", 99 * Parameter[index]); break;
    case  2: sprintf(string, "%.1f", 40 * Parameter[index] - 20); break;
    case  3: sprintf(string, "%.1f", bufres); break;
    default: sprintf(string, "%.1f", 100 * semi);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;
*)
