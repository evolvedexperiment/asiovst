unit TestToneDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TTestToneDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    function Midi2String(const n : Single): string;
    function ISO2String(b: Single): string;
  public
  end;

implementation

{$R *.DFM}

function TTestToneDataModule.Midi2String(const n: Single): string;
var
  o, s, p : Integer;
begin
 p      := 0;
 result := '   ';
 o      := round(n / 12);
 s      := round(n - (12 * o));
 o      := o - 2;

 case s of
    0: result := result + 'C';
    1: result := result + 'C#';
    2: result := result + 'D';
    3: result := result + 'Eb';
    4: result := result + 'E';
    5: result := result + 'F';
    6: result := result + 'F#';
    7: result := result + 'G';
    8: result := result + 'G#';
    9: result := result + 'A';
   10: result := result + 'Bb';
  else result := result + 'B';
 end;

 result := result + ' ';

 if (o < 0) then result := result + '-';
 result := result + char(48 + (abs(o) mod 10));
end;

procedure TTestToneDataModule.ParameterModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: PreDefined := 'MIDI #';
  1: PreDefined := 'IMPULSE';
  2: PreDefined := 'WHITE';
  3: PreDefined := 'PINK';
  4: PreDefined := '---';
  5: PreDefined := 'SINE';
  6: PreDefined := 'LOG SWP.';
  7: PreDefined := 'LOG STEP';
  8: PreDefined := 'LIN SWP.';
 end;
end;

procedure TTestToneDataModule.ParameterChannelDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[2]) of
  0 : PreDefined := 'LEFT';
  1 : PreDefined := 'CENTRE';
  2 : PreDefined := 'RIGHT';
 end;
end;

function TTestToneDataModule.ISO2String(b : Single): string;
begin
 case round(b) of
   13: PreDefined := '20 Hz';
   14: PreDefined := '25 Hz';
   15: PreDefined := '31 Hz';
   16: PreDefined := '40 Hz';
   17: PreDefined := '50 Hz';
   18: PreDefined := '63 Hz';
   19: PreDefined := '80 Hz';
   20: PreDefined := '100 Hz';
   21: PreDefined := '125 Hz';
   22: PreDefined := '160 Hz';
   23: PreDefined := '200 Hz';
   24: PreDefined := '250 Hz';
   25: PreDefined := '310 Hz';
   26: PreDefined := '400 Hz';
   27: PreDefined := '500 Hz';
   28: PreDefined := '630 Hz';
   29: PreDefined := '800 Hz';
   30: PreDefined := '1 kHz';
   31: PreDefined := '1.25 kHz';
   32: PreDefined := '1.6 kHz';
   33: PreDefined := '2.0 kHz';
   34: PreDefined := '2.5 kHz';
   35: PreDefined := '3.1 kHz';
   36: PreDefined := '4 kHz';
   37: PreDefined := '5 kHz';
   38: PreDefined := '6.3 kHz';
   39: PreDefined := '8 kHz';
   40: PreDefined := '10 kHz';
   41: PreDefined := '12.5 kHz';
   42: PreDefined := '16 kHz';
   43: PreDefined := '20 kHz';
  else PreDefined := '--';
 end;
end;

procedure TTestToneDataModule.VSTModuleProcess(const Inputs,
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
 if(updateRx != updateTx) update();

 float a, b, x=0.0, twopi=6.2831853;
 float z0=zz0, z1=zz1, z2=zz2, z3=zz3, z4=zz4, z5=zz5;
 float ph=phi, dph=dphi, l=fLeft, r=right, t=thru;
 float s=sw, sx=swx, ds=swd, fsc=fscale;
 long st=swt;
 int m=fMode;

 while(--sampleFrames >= 0)
 begin
  a = *++in1; 
  b = *++in2;
  
  switch(m)
  begin
   case 1: if(st>0) begin st--; x=0.; end; else //impulse
       begin 
        x=1.;
        st=(long)(fLengh*SampleRate());
       end;
       break;
   
   case 2: //noise
  #if WIN32 
   case 3: x = (Random() - 16384); //for RAND_MAX = 32767
  #else //mac/gcc
   case 3: x = ((Random() & 0x7FFF) - 16384);
  #endif
       if(m==3)
       begin
         z0 = 0.997 * z0 + 0.029591 * x; //pink filter
         z1 = 0.985 * z1 + 0.032534 * x;
         z2 = 0.950 * z2 + 0.048056 * x;
         z3 = 0.850 * z3 + 0.090579 * x;
         z4 = 0.620 * z4 + 0.108990 * x;
         z5 = 0.250 * z5 + 0.255784 * x;
         x = z0 + z1 + z2 + z3 + z4 + z5;
       end; 
       break;

   case 4: x=0.; break; //mute
  
   case 0: //tones
   case 5:
   case 9: ph = fmod(ph+dph,twopi); 
       x = sin(ph); 
       break;

   case 6: //log sweep & step
   case 7: if(st>0) begin st--; ph=0.; end; else
       begin
        s += ds;
        if(m==7) dph = fsc * Power(10.0, 0.1 * int(s));
        else dph = fsc * Power(10.0, 0.1 * s);
        x = sin(ph);
        ph = fmod(ph+dph,twopi);
        if(s>sx) begin l=0.; r=0.; end; 
       end;
       break; 

   case 8: //lin sweep
       if(st>0) begin st--; ph=0.; end; else
       begin
        s += ds;
        x = sin(ph);
        ph = fmod(ph+s,twopi);
        if(s>sx) begin l=0.; r=0.; end; 
       end; 
       break; 
  end;

  *++out1 = t*a + l*x;
  *++out2 = t*b + r*x;
 end;
 zz0 := z0;
 zz1 := z1;
 zz2 := z2;
 zz3 := z3;
 zz4 := z4;
 zz5 := z5;
 phi := ph;
 sw  := s;
 swt := st;
 if (s > sx)
  then setParameter(0, Parameter[0]); //retrigger sweep
*)
end;

end.

(*
mdaTestTone::mdaTestTone(audioMasterCallback audioMaster)  : AudioEffectX(audioMaster, 1, 8)  
begin
 Parameter[0] := 0.47; //fMode
 Parameter[1] := 0.71; //level dB
 Parameter[2] := 0.50; //pan dB
 Parameter[3] := 0.57; //freq1 B
 Parameter[4] := 0.50; //freq2 Hz
 Parameter[5] := 0.00; //thru dB
 Parameter[6] := 0.30; //sweep ms
 Parameter[7] := 1.00; //cal dBFS

 updateTx := updateRx;
 
 suspend();
 setParameter(6, 0);
end;

void mdaTestTone::suspend()
begin
 zz0 := 0;
 zz1 := 0;
 zz2 := 0;
 zz3 := 0;
 zz4 := 0;
 zz5 := 0;
 phi := 0;
end;

void mdaTestTone::setParameter(VstInt32 index, float value)
begin
 //just update display text...
 int fMode := int(8.9 * Parameter[0]);
 float , df=0.0;
 if (Parameter[4] > 0.6) then df := 1.25 * Parameter[4] - 0.75;
 if (Parameter[4] < 0.4) then df := 1.25 * Parameter[4] - 0.50;
 switch(fMode)
 begin
  case 0: //MIDI note
       = Trunc(128.*Parameter[3]);
      //long2string((long), disp1); //Semi
      midi2string(, disp1); //Semitones
      long2string((long)(100.*df), disp2); //Cents
      break;

  case 1: //no frequency display
  case 2: 
  case 3:
  case 4: strcpy(disp1, "--");
          strcpy(disp2, "--"); break;
  
  case 5: //sine
       = 13. + Trunc(30.*Parameter[3]);
      iso2string(, disp1); //iso band freq
      =Power(10.0, 0.1*(+df));
      float2strng(, disp2); //Hz
      break;
  
  case 6: //log sweep & step    
  case 7: sw = 13. + Trunc(30.*Parameter[3]);
      swx = 13. + Trunc(30.*Parameter[4]);
      iso2string(sw, disp1); //start freq
      iso2string(swx, disp2); //end freq
      break; 
  
  case 8: //lin sweep
      sw = 200. * Trunc(100.*Parameter[3]);
      swx = 200. * Trunc(100.*Parameter[4]);
      long2string((long)sw, disp1); //start freq
      long2string((long)swx, disp2); //end freq
      break; 
 end;

 updateTx++;
end;

void mdaTestTone::update()
begin
 updateRx = updateTx;
 
 float , df, twopi=6.2831853;

 //calcs here! 
 fMode = int(8.9 * Parameter[0]);
 fLeft = 0.05 * int(60.*Parameter[1]);
 fLeft = Power(10.0, fLeft - 3.);
 if(fMode==2) fLeft*=0.0000610; //scale white for RAND_MAX = 32767
 if(fMode==3) fLeft*=0.0000243; //scale pink for RAND_MAX = 32767
 if(Parameter[2]<0.3) right=0.; else right=fLeft;
 if(Parameter[2]>0.6) fLeft=0.;
 fLengh = 1. + 0.5*int(62*Parameter[6]);
 swt=(long)(fLengh*SampleRate();

 if (Parameter[7] > 0.8) //output level trim
  begin
   if (Parameter[7] > 0.96) cal = 0.;
   else if(Parameter[7] > 0.92) cal = -0.01000001;
   else if(Parameter[7] > 0.88) cal = -0.02000001;
   else if(Parameter[7] > 0.84) cal = -0.1;
   else cal = -0.2;
  
   calx = Power(10.0, 0.05*cal); 
   fLeft := fLeft * calx;
   right := right * calx; 
   calx = 0.;
  end;
 else //output level calibrate
  begin
   cal = int(25.*Parameter[7] - 21.1);
   calx = cal;
  end;

 df := 0;
 if Parameter[4] > 0.6 then df := 1.25 * Parameter[4] - 0.75;
 if Parameter[4] < 0.4 then df := 1.25 * Parameter[4] - 0.50;

 switch(fMode)
 begin
    case 0: //MIDI note
         = Trunc(128.*Parameter[3]);
        //long2string((long), disp1); //Semi
        midi2string(, disp1); //Semitones
        long2string((long)(100.*df), disp2); //Cents
        dphi = 51.37006*Power(1.0594631,+df)/SampleRate();
        break;

    case 1: //no frequency display
    case 2: 
    case 3: 
    case 4: strcpy(disp1, "--");
        strcpy(disp2, "--"); break;
    
    case 5: //sine
         = 13. + Trunc(30.*Parameter[3]);
        iso2string(, disp1); //iso band freq
        =Power(10.0, 0.1*(+df));
        float2strng(, disp2); //Hz
        dphi=twopi*/SampleRate();
        break;
    
    case 6: //log sweep & step    
    case 7: sw = 13. + Trunc(30.*Parameter[3]);
        swx = 13. + Trunc(30.*Parameter[4]);
        iso2string(sw, disp1); //start freq
        iso2string(swx, disp2); //end freq
        if(sw>swx) begin swd=swx; swx=sw; sw=swd; end; //only sweep up
        if(fMode==7) swx += 1.;
        swd = (swx-sw) / (fLengh*SampleRate());
        swt= 2 * (long)SampleRate();
        break; 
    
    case 8: //lin sweep
        sw = 200. * Trunc(100.*Parameter[3]);
        swx = 200. * Trunc(100.*Parameter[4]);
        long2string((long)sw, disp1); //start freq
        long2string((long)swx, disp2); //end freq
        if(sw>swx) begin swd=swx; swx=sw; sw=swd; end; //only sweep up
        sw = twopi*sw/SampleRate();
        swx = twopi*swx/SampleRate();
        swd = (swx-sw) / (fLengh*SampleRate());
        swt= 2 * (long)SampleRate();
        break; 
 end;
 thru = Power(10.0, (0.05 * int(40.*Parameter[5])) - 2.);
 if (Parameter[5] =0) then thru := 0;
 fscale = twopi / SampleRate;
end;

void mdaTestTone::getParameterDisplay(VstInt32 index, char *text)
begin
 switch(index)
 begin
  case 1: long2string((long)(int(60. * Parameter[1]) - 60.0 - calx), text); break;
  case 3: strcpy(text, disp1); break;
  case 4: strcpy(text, disp2); break;
  case 6: if(Parameter[5]==0) strcpy(text, "OFF");
      else long2string((long)(40 * Parameter[5] - 40), text); break;
  case 5: long2string(1000 + 500*int(62*Parameter[6]), text); break;
  case 7: float2strng(cal, text); break;
 end;
end;
*)
