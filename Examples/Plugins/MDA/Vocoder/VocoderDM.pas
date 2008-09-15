unit VocoderDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TVocoderDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterModInDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterModInChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHiBandChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fSwap     : Integer;
    fGain     : Single;
    fThru     : Single;
    fHigh     : Single;
    fNumBands : Integer;
    fKOut     : Single;
    fKVal     : Integer;
    fFreq     : Array [0..15, 0..12] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TVocoderDataModule.ParameterModInDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'RIGHT'
  else PreDefined := 'LEFT';
end;

procedure TVocoderDataModule.ParameterModInChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSwap := Integer(Parameter[0] > 0.5);
end;

procedure TVocoderDataModule.ParameterHiBandChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHigh := sqr(Value) * Value * Power(10, 0.5 + 2 * Parameter[1]);
end;

procedure TVocoderDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 Parameter[0] := 0.33;  // Input Select
 Parameter[1] := 0.50;  // Output dB
 Parameter[2] := 0.40;  // Hi Thru
 Parameter[3] := 0.40;  // Hi Band
 Parameter[4] := 0.16;  // Envelope
 Parameter[5] := 0.55;  // Filter Q
 Parameter[6] := 0.6667;// fFreq Range
 Parameter[7] := 0.33;  // Num Bands

 ///differences from default program...
 programs[1].Parameter[7] = 0.66;
 strcpy(programs[1].name,"16 Band Vocoder");

 programs[2].Parameter[2] = 0.00;
 programs[2].Parameter[3] = 0.00;
 programs[2].Parameter[6] = 0.50;
 strcpy(programs[2].name,"Old Vocoder");

 programs[3].Parameter[3] = 0.00;
 programs[3].Parameter[5] = 0.70;
 programs[3].Parameter[6] = 0.50;
 strcpy(programs[3].name,"Choral Vocoder");

 programs[4].Parameter[4] = 0.78;
 programs[4].Parameter[6] = 0.30;
 strcpy(programs[4].name,"Pad Vocoder");

 suspend();
*)
end;

procedure TVocoderDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample              : Integer;
  a, b, o, aa, bb     : Single;
  oo, g, ht, hh, tmp  : Single;
  i, k, sw, nb        : Integer;
begin
 o  := 0;
 oo := fKOut;
 g  := fGain;
 ht := fThru;
 hh := fHigh;
 k  := fKVal;
 sw := fSwap;
 nb := fNumBands;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[    sw, Sample]; //speech
   b := Inputs[1 - sw, Sample]; //synth

   tmp := a - fFreq[0][7]; //integrate modulator for HF band and filter bank pre-emphasis
   fFreq[0][7] := a;
   a := tmp;

   tmp := abs(tmp);
   fFreq[0][11] := fFreq[0][11] - fFreq[0][12] * (fFreq[0][11] - tmp); // High band envelope
   o := fFreq[0][11] * (ht * a + hh * (b - fFreq[0][3]));             // High band + High Thru

   fFreq[0][3] := b; //integrate carrier for HF band

   inc(k);
   if (k and 1) > 0 then   // this block runs at half sample rate
    begin
     oo := 0.0;
     aa := a + fFreq[0][9] - fFreq[0][8] - fFreq[0][8];  //apply zeros here instead of in each reson
     fFreq[0][9] := fFreq[0][8];
     fFreq[0][8] := a;
     bb := b + fFreq[0][5] - fFreq[0][4] - fFreq[0][4];
     fFreq[0][5] := fFreq[0][4];
     fFreq[0][4] := b;

     for i := 1 to nb - 1 do //filter bank: 4th-order band pass
      begin
       tmp := fFreq[i][0] * fFreq[i][3] + fFreq[i][1] * fFreq[i][4] + bb;
       fFreq[i][4] := fFreq[i][3];
       fFreq[i][3] := tmp;
       tmp := tmp + fFreq[i][2] * fFreq[i][5] + fFreq[i][1] * fFreq[i][6];
       fFreq[i][6] := fFreq[i][5];
       fFreq[i][5] := tmp;

       tmp := fFreq[i][0] * fFreq[i][7] + fFreq[i][1] * fFreq[i][8] + aa;
       fFreq[i][8] := fFreq[i][7];
       fFreq[i][7] := tmp;
       tmp := tmp + fFreq[i][2] * fFreq[i][9] + fFreq[i][1] * fFreq[i][10];
       fFreq[i][10] := fFreq[i][ 9];
       fFreq[i][ 9] := tmp;

       tmp := abs(tmp);
       fFreq[i][11] := fFreq[i][11] - fFreq[i][12] * (fFreq[i][11] - tmp);
       oo := oo + fFreq[i][5] * fFreq[i][11];
      end;
    end;
   o := o + oo * g; //effect of interpolating back up to Fs would be minimal (aliasing >16kHz)

   Outputs[0, Sample] := o;
   Outputs[1, Sample] := o;
  end;

  fKOut := oo;
  fKVal := k and $1;
  if abs(fFreq[0][11]) < 1E-10
   then fFreq[0][11] := 0; //catch HF envelope denormal

  for i:=1 to nb - 1 do
   if (abs(fFreq[i][3]) < 1E-10) or (abs(fFreq[i][7]) < 1E-10) then
    for k := 3 to 11 do fFreq[i][k] := 0; //catch reson & envelope denormals

(*
  if (abs(o) > 10) then suspend; //catch instability
*)
end;

procedure TVocoderDataModule.VSTModuleResume(Sender: TObject);
var
  tpofs      : Double;
  rr, th, re : Double;
  sh         : Single;
  i          : Integer;
begin
 tpofs := 2 * Pi / SampleRate;

 fGain := Power(10, 2 * Parameter[1] - 3 * Parameter[5] - 2);

 fThru := Power(10, 0.5 + 2 * Parameter[1]);
 fHigh := sqr(Parameter[3]) * Parameter[3] * fThru;
 fThru := fThru * sqr(Parameter[2]) * Parameter[2];

 if (Parameter[7] < 0.5) then
  begin
   fNumBands := 8;
   re   := 0.003;
   fFreq[1][2] := 3000.0;
   fFreq[2][2] := 2200.0;
   fFreq[3][2] := 1500.0;
   fFreq[4][2] := 1080.0;
   fFreq[5][2] := 700.0;
   fFreq[6][2] := 390.0;
   fFreq[7][2] := 190.0;
  end
 else
  begin
   fNumBands := 16;
   re   := 0.0015;
   fFreq[ 1][2] := 5000.0;  // +1000
   fFreq[ 2][2] := 4000.0;  //  +750
   fFreq[ 3][2] := 3250.0;  //  +500
   fFreq[ 4][2] := 2750.0;  //  +450
   fFreq[ 5][2] := 2300.0;  //  +300
   fFreq[ 6][2] := 2000.0;  //  +250
   fFreq[ 7][2] := 1750.0;  //  +250
   fFreq[ 8][2] := 1500.0;  //  +250
   fFreq[ 9][2] := 1250.0;  //  +250
   fFreq[10][2] := 1000.0;  //  +250
   fFreq[11][2] :=  750.0;  //  +210
   fFreq[12][2] :=  540.0;  //  +190
   fFreq[13][2] :=  350.0;  //  +155
   fFreq[14][2] :=  195.0;  //  +100
   fFreq[15][2] :=   95.0;
  end;

 if (Parameter[4] < 0.05) //freeze
  then  for i := 0 to fNumBands - 1 do fFreq[i][12] := 0
  else
   begin
    fFreq[0][12] := Power(10.0, -1.7 - 2.7 * Parameter[4]); //envelope speed

    rr := 0.022 / fNumBands; //minimum proportional to frequency to stop distortion
    for i := 1 to fNumBands - 1 do
     begin
      fFreq[i][12] := (0.025 - rr * i);
      if (fFreq[0][12] < fFreq[i][12])
       then fFreq[i][12] := fFreq[0][12];
     end;
    fFreq[0][12] := 0.5 * fFreq[0][12]; //only top band is at full rate
   end;

 rr := 1 - Power(10, -1 - 1.2 * Parameter[5]);
 sh := Power(2, 3 * Parameter[6] - 1); //filter bank range shift

 for i := 1 to fNumBands - 1 do
  begin
   fFreq[i][2] := fFreq[i][2] * sh;
   th          := arccos((2.0 * rr * cos(tpofs * fFreq[i][2])) / (1.0 + rr * rr));
   fFreq[i][0] := (2.0 * rr * cos(th)); //a0
   fFreq[i][1] := (-rr * rr);           //a1
                        //was 0.98
   fFreq[i][2] := fFreq[i][2] * 0.96; //shift 2nd stage slightly to stop high resonance peaks
   th          := arccos((2 * rr * cos(tpofs * fFreq[i][2])) / (1.0 + rr * rr));
   fFreq[i][2] := (2 * rr * cos(th));
  end;
end;

procedure TVocoderDataModule.VSTModuleSuspend(Sender: TObject);
var
  i, j : Integer;
begin
 for i := 0 to fNumBands - 1 do
  for j := 3 to 11 do
   fFreq[i][j] := 0; //zero band filters and envelopes
 fKOut := 0;
 fKVal := 0;
end;

end.

(*
void mdaVocoder::getParameterDisplay(VstInt32 index, char *text)
begin
   char string[16];

  switch(index)
  begin
    case  1: sprintf(string, "%.1f", 40.0 * Parameter[index] - 20.0); break;
    case  4: if (Parameter[index] < 0.05)
             then strcpy(string, "FREEZE");
             else sprintf(string, "%.1f", Power(10.0, 1.0 + 3.0 * Parameter[index])); break;
    case  6: sprintf(string, "%.0f", 800.0 * Power(2.0, 3.0 * Parameter[index] - 2.0)); break;
    case  7: if (fNumBands = 8)
              then strcpy(string, "8 BAND");
              else strcpy(string, "16 BAND"); break;

    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;
*)
