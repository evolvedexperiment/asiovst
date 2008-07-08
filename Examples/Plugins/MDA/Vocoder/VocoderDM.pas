unit VocoderDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TVocoderDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TVocoderDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
  Parameter[0] = 0.33;  //input select
  Parameter[1] = 0.50;  //output dB
  Parameter[2] = 0.40;  //hi fThru
  Parameter[3] = 0.40;  //hi band
  Parameter[4] = 0.16;  //envelope
  Parameter[5] = 0.55;  //filter q
  Parameter[6] = 0.6667;//freq range       
  Parameter[7] = 0.33;  //num bands       
*)
end;

procedure TVocoderDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;
(*
  float a, b, o=0.0, aa, bb, oo=fKOut, g=fGain, ht=fThru, hh=fHigh, tmp;
  long i, k=fKVal, sw=fSwap, nb=nbnd;

  while(--sampleFrames >= 0)
   begin
    a = *++in1; //speech  
    b = *++in2; //synth
    if sw = 0 then
     begin
      tmp := a;
      a   := b;
      b   := tmp;
     end; //fSwap channels
 
    tmp := a - freq[0][7]; //integrate modulator for HF band and filter bank pre-emphasis
    freq[0][7] := a;
    a := tmp;

    tmp := abs(tmp);
    freq[0][11] := freq[0][11] - freq[0][12] * (freq[0][11] - tmp); // High band envelope
    o = freq[0][11] * (ht * a + hh * (b - freq[0][3]));             // High band + High Thru
    
    freq[0][3] := b; //integrate carrier for HF band

    if (++k & 0x1)   // this block runs at half sample rate
     begin
      oo := 0.0;
      aa := a + freq[0][9] - freq[0][8] - freq[0][8];  //apply zeros here instead of in each reson
                freq[0][9] = freq[0][8];  freq[0][8] = a;
      bb := b + freq[0][5] - freq[0][4] - freq[0][4];
                freq[0][5] = freq[0][4];  freq[0][4] = b;

      for i := 1 to nb - 1 do //filter bank: 4th-order band pass
       begin
        tmp := freq[i][0] * freq[i][3] + freq[i][1] * freq[i][4] + bb;
        freq[i][4] := freq[i][3];
        freq[i][3] := tmp;
        tmp := tmp + freq[i][2] * freq[i][5] + freq[i][1] * freq[i][6];
        freq[i][6] := freq[i][5];
        freq[i][5] := tmp;

        tmp := freq[i][0] * freq[i][7] + freq[i][1] * freq[i][8] + aa;
        freq[i][8] := freq[i][7];
        freq[i][7] := tmp;
        tmp := tmp + freq[i][2] * freq[i][9] + freq[i][1] * freq[i][10];
        freq[i][10] := freq[i][ 9];
        freq[i][ 9] := tmp;

        tmp := abs(tmp);
        freq[i][11] := freq[i][11] - freq[i][12] * (freq[i][11] - tmp);
        oo := oo + freq[i][5] * freq[i][11];
       end;
     end;
    o += oo * g; //effect of interpolating back up to Fs would be minimal (aliasing >16kHz)

    *++out1 = o;
    *++out2 = o;
   end;

  fKOut = oo;  
  fKVal = k & 0x1;
  if abs(freq[0][11]) < 1E-10)
   then freq[0][11] = 0; //catch HF envelope denormal

  for i:=1 to nb - 1 do
   if (abs(freq[i][3]) < 1E-10) or (abs(freq[i][7]) < 1E-10) then
    for k := 3 to 11 do freq[i][k] := 0; //catch reson & envelope denormals

  if (abs(o) > 10) then suspend; //catch instability
*)
end;

procedure TVocoderDataModule.VSTModuleResume(Sender: TObject);
begin
(*
  double tpofs = 6.2831853 / SampleRate;
  double rr, th, re;
  float sh;
  long i;

  fSwap := 1;
  if (Parameter[0] > 0.5) then fSwap := 0;
  fGain := Power(10, 2 * Parameter[1] - 3 * Parameter[5] - 2);
  
  fThru := Power(10, 0.5 + 2 * Parameter[1]);
  fHigh :=  sqr(Parameter[3]) * Parameter[3] * fThru;
  fThru := fThru * sqr(Parameter[2]) * Parameter[2];
  
  if (Parameter[7] < 0.5) 
  begin
    nbnd := 8;
    re   := 0.003;
    freq[1][2] := 3000.0;
    freq[2][2] := 2200.0;
    freq[3][2] := 1500.0;
    freq[4][2] := 1080.0;
    freq[5][2] := 700.0;
    freq[6][2] := 390.0;
    freq[7][2] := 190.0;
  end;
  else
  begin
    nbnd := 16;
    re   := 0.0015;
    freq[ 1][2] := 5000.0; //+1000
    freq[ 2][2] := 4000.0; //+750
    freq[ 3][2] := 3250.0; //+500
    freq[ 4][2] := 2750.0; //+450
    freq[ 5][2] := 2300.0; //+300
    freq[ 6][2] := 2000.0; //+250
    freq[ 7][2] := 1750.0; //+250
    freq[ 8][2] := 1500.0; //+250
    freq[ 9][2] := 1250.0; //+250
    freq[10][2] := 1000.0; //+250
    freq[11][2] :=  750.0; //+210
    freq[12][2] :=  540.0; //+190
    freq[13][2] :=  350.0; //+155
    freq[14][2] :=  195.0; //+100
    freq[15][2] :=   95.0;
  end;

  if (Parameter[4] < 0.05) //freeze
   then  for i := 0 to nbnd - 1 do freq[i][12] := 0
   else
    begin
     freq[0][12] := Power(10.0, -1.7 - 2.7 * Parameter[4]); //envelope speed

     rr := 0.022 / nbnd; //minimum proportional to frequency to stop distortion
     for i := 1 to nbnd - 1 do
      begin
       freq[i][12] := (0.025 - rr * (double)i);
       if (freq[0][12] < freq[i][12])
        then freq[i][12] := freq[0][12];
      end;
     freq[0][12] := 0.5 * freq[0][12]; //only top band is at full rate
    end;

  rr = 1.0 - Power(10.0, -1.0 - 1.2 * Parameter[5]);
  sh = Power(2.0, 3.0 * Parameter[6] - 1.0); //filter bank range shift

  for i:=1 to nbnd - 1 do
  begin
    freq[i][2] := freq[i][2] * sh;
    th         := acos((2.0 * rr * cos(tpofs * freq[i][2])) / (1.0 + rr * rr));
    freq[i][0] := (2.0 * rr * cos(th)); //a0
    freq[i][1] := (-rr * rr);           //a1
                         //was 0.98
    freq[i][2] := freq[i][2] * 0.96; //shift 2nd stage slightly to stop fHigh resonance peaks
    th         := acos((2 * rr * cos(tpofs * freq[i][2])) / (1.0 + rr * rr));
    freq[i][2] := (2 * rr * cos(th));
  end;
*)
end;

procedure TVocoderDataModule.VSTModuleSuspend(Sender: TObject);
var
  i, j : Integer;
begin
(*
 for i := 0 to nbnd - 1 do
  for j := 3 to 11 do
   freq[i][j] := 0; //zero band filters and envelopes
 fKOut := 0;
 fKVal := 0;
*)
end;

end.

(*
mdaVocoder::mdaVocoder(audioMasterCallback audioMaster): AudioEffectX(audioMaster, NPROGS, NPARAMS)
begin
  programs = new mdaVocoderProgram[numPrograms];
  setProgram(0);
  
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
end;

void mdaVocoder::setProgram(VstInt32 program)
begin
  int i=0;

  mdaVocoderProgram *p = &programs[program];
  curProgram = program;
  setProgramName(p->name);
  for(i=0; i<NPARAMS; i++) Parameter[i] = p->Parameter[i];
  resume();
end;

void mdaVocoder::getParameterDisplay(VstInt32 index, char *text)
begin
   char string[16];

  switch(index)
  begin
    case  0: if(fSwap) strcpy(string, "RIGHT"); else strcpy(string, "LEFT"); break;
    case  1: sprintf(string, "%.1f", 40.0 * Parameter[index] - 20.0); break;
    case  4: if(Parameter[index]<0.05) strcpy(string, "FREEZE");
             else sprintf(string, "%.1f", Power(10.0, 1.0 + 3.0 * Parameter[index])); break;
    case  6: sprintf(string, "%.0f", 800.0 * Power(2.0, 3.0 * Parameter[index] - 2.0)); break;
    case  7: if(nbnd==8) strcpy(string, "8 BAND"); else strcpy(string, "16 BAND"); break;

    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;
*)
