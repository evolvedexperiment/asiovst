unit DelayDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TDelayDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    fBuffer    : PAVDSingleFixedArray;
    fSize      : Integer;
    fWet       : Single;
    fDry       : Single;
    fFeedback  : Single;
    fLowMix    : Single;
    fHighMix   : Single;
    fFilter    : Single;
    fFilter0   : Single;
    ipos       : Integer;
    ldel, rdel : Integer;
  public
  end;

implementation

{$R *.DFM}

procedure TDelayDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSize   := 32766;  //set max delay time at max sample rate
 GetMem(fBuffer, fSize * SizeOf(Single));

(*
 ipos = 0;
 fil0 = 0.0;

 // inits here!
 Parameter[0] := 0.50; // Left Delay
 Parameter[1] := 0.27; // Right Ratio
 Parameter[2] := 0.70; // Feedback
 Parameter[3] := 0.50; // Tone
 Parameter[4] := 0.33; // Wet mix
 Parameter[5] := 0.50; // Output

 suspend();    //flush fBuffer
 setParameter(0, 0.5);
*)
end;

procedure TDelayDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuffer)
  then Dispose(fBuffer);
end;

procedure TDelayDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  tmp : Single;
begin
 //calcs here
 ldel := round(fSize * sqr(Parameter[0]));
 if (ldel < 4) then ldel := 4;

 case round(Parameter[1] * 17.9) of //fixed left/right ratios
   17: tmp := 0.5;
   16: tmp := 0.6667;
   15: tmp := 0.75;
   14: tmp := 0.8333;
   13: tmp := 1;
   12: tmp := 1.2;
   11: tmp := 1.3333;
   10: tmp := 1.5;
    9: tmp := 2;
  else tmp := 4 * Parameter[1]; //variable ratio
 end;

 rdel := round(fSize * sqr(Parameter[0]) * tmp);
 if (rdel > fSize) then rdel := fSize;
 if (rdel < 4) then rdel := 4;

(*
 fil := Parameter[3];

 if (Parameter[3] > 0.5)  //simultaneously change crossover frequency & high/low mix
  begin
   fil  := 0.5 * fil - 0.25;
   lmix := -2.0 * fil;
   hmix := 1.0;
  end;
 else
  begin
   hmix := 2 * fil;
   lmix := 1 - hmix;
  end;
 fil := exp(-6.2831853 * Power(10, 2.2 + 4.5 * fil) / SampleRate);
*)

 fFeedback := 0.495 * Parameter[2];
 fWet      := 1 - Parameter[4];
 fWet      := Parameter[5] * (1 - sqr(fWet)); // -3dB at 50% mix
 fDry      := Parameter[5] * 2 * (1 - sqr(Parameter[4]));

 //if(Parameter[2] > 0.99) { fbk=0.5; fWet=0.0; } //freeze
end;

procedure TDelayDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
  i, l, r, s  : Integer;
  fb, lx, hx,
  f, f0, tmp  : Single;
begin
 fb := fFeedback;
 lx := fLowMix;
 hx := fHighMix;
 f  := fFilter;
 f0 := fFilter0;

 i  := ipos;

 s  := fSize;
 l  := (i + ldel) mod (s + 1);
 r  := (i + rdel) mod (s + 1);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := fBuffer[l]; //delay outputs
   Outputs[1, Sample] := fBuffer[r];

   tmp := fWet * ( Inputs[0, Sample] +  Inputs[1, Sample]) +
          fb   * (Outputs[0, Sample] + Outputs[1, Sample]);   // mix input & feedback
   f0  := f * (f0 - tmp) + tmp;                             // low-pass filter
   fBuffer[i] := lx * f0 + hx * tmp;                        // delay input

   dec(i); if (i < 0) then i := s;
   dec(l); if (l < 0) then l := s;
   dec(r); if (r < 0) then r := s;

   Outputs[0, Sample] := fDry * Inputs[0, Sample] + Outputs[0, Sample]; //mix fWet & fDry
   Outputs[1, Sample] := fDry * Inputs[1, Sample] + Outputs[1, Sample];
  end;

 ipos := i;
 if abs(f0) < 1E-10
  then fFilter0 := 0
  else fFilter0 := f0; //trap denormals
end;

procedure TDelayDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(fBuffer, fSize * SizeOf(Single), 0);
end;

end.

(*
void mdaDelay::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(ldel * 1000.0 / SampleRate), text); break;
    case 1: long2string((long)(100 * rdel / ldel), text); break;
    case 2: long2string((long)(99 * Parameter[2]), text); break;
    case 3: long2string((long)(200 * Parameter[3] - 100), text); break;
    case 4: long2string((long)(100 * Parameter[4]), text); break;
    case 5: long2string((long)(20 * log10(2.0 * Parameter[5])), text); break;
  }
}

*)
