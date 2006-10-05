unit uPlugin;

interface
uses Windows, DVstUtils, uEditor, DAEffect, DAEffectX, JclMath, DSPUtils,
     DAudioEffect, DAudioEffectX, DVstTemplate, Forms;

const p4=1.0e-24;
      coeff:array[0..4,0..10] of double=(
(8.11044e-06, 8.943665402, -36.83889529, 92.01697887, -154.337906,
181.6233289, -151.8651235,  89.09614114, -35.10298511, 8.388101016,
-0.923313471),

(4.36215e-06,8.90438318, -36.55179099, 91.05750846, -152.422234,
179.1170248,-149.6496211,87.78352223, -34.60687431, 8.282228154,
-0.914150747),

(3.33819e-06,8.893102966, -36.49532826, 90.96543286, -152.4545478,
179.4835618,-150.315433, 88.43409371, -34.98612086, 8.407803364,
-0.932568035),

(1.13572e-06,8.994734087, -37.2084849, 93.22900521, -156.6929844,
184.596544,-154.3755513, 90.49663749, -35.58964535, 8.478996281,
-0.929252233),

(4.09431e-07,8.997322763, -37.20218544, 93.11385476, -156.2530937,
183.7080141,-153.2631681, 89.59539726, -35.12454591, 8.338655623,
-0.910251753));
{$i freq.inc}

type
 APlugin = class(TVSTTemplate)
 private
  memory:array[0..4,0..9] of double;
  synth_active:boolean;
  envstage:integer;
  envvalue:single;
  freq,dfreq,cnt:single;
  notesplaying,mtimer,mtimermax:longint;
  mix_src,mix_dst,mix_cur:array[0..4] of single;
  fwave:single;
  done:boolean;
  function DoFormant(i:single;n:integer):single;
 public
  procedure setParameter(index:Longint; value: Single); override;
  procedure processMIDI(ev: VstMidiEvent); override;
  procedure processAudio(const inputs,outputs: TArrayOfSingleArray; sampleframes: Integer); override;
  procedure initializeParameters; override;
  constructor Create(audioMaster: TAudioMasterCallbackFunc);
  destructor Destroy; override;
 end;

implementation

uses sysutils;

constructor APlugin.Create(audioMaster: TAudioMasterCallbackFunc);
var j, i, numPrograms, numParameters: integer;
begin
 canDos := [receiveVstEvents, receiveVstMidiEvent, sendVstEvents, sendVstMidiEvent];
 numInputs := 2;
 numOutputs := 2;
 numPrograms := 8;
 numParameters := 8;
 {$IFDEF Synth}
 UniqueID := 'TBVS';
 EffectName := 'VowelSynth';
 ProductName := 'VowelSynth';
 Properties := [prCanMono, prCanReplacing, prSynth];
 {$ELSE}
 UniqueID := 'TBVF';
 EffectName := 'VowelFilter';
 ProductName := 'VowelFilter';
 Properties := [prCanMono, prCanReplacing];
 {$ENDIF}
 VendorName := 'Tobybear';
 VendorVersion := 1;
 initialDelay := 0;
 tailSize := 0;
 inherited Create(audiomaster, numPrograms, numParameters, TPluginEditorWindow);

 Programs[0].fPar[0]:=1-(1/1.5);;
 Programs[0].fPar[1]:=0;
 Programs[0].fPar[2]:=0;
 Programs[0].fPar[3]:=0.9;
 Programs[0].fPar[4]:=0.9;
 Programs[0].fPar[5]:=0.9;
 Programs[0].fPar[6]:=0.3;
 Programs[0].fPar[7]:=0;

 StrCopy(programs[0].name, 'Init Preset 01');
 StrCopy(programs[1].name, 'Init Preset 02');
 StrCopy(programs[2].name, 'Init Preset 03');
 StrCopy(programs[3].name, 'Init Preset 04');
 StrCopy(programs[4].name, 'Init Preset 05');
 StrCopy(programs[5].name, 'Init Preset 06');
 StrCopy(programs[6].name, 'Init Preset 07');
 StrCopy(programs[7].name, 'Init Preset 08');

 cnt:=0;
 synth_active:=false;
 notesplaying:=0;
 envstage:=0;
 envvalue:=0;

 for j:=0 to 4 do
 begin
  mix_src[j]:=0;
  mix_dst[j]:=0;
  mix_cur[j]:=0;
  for i:=0 to 9 do memory[j,i]:=0;
 end;

 mtimer:=0;
end;

destructor APlugin.destroy;
begin
 inherited;
end;

procedure APlugin.initializeParameters;
begin
 ParameterProperties[0].min := -90;
 ParameterProperties[0].max := 3;
 ParameterProperties[0].name := 'Volume';
 ParameterProperties[0].units := 'dB';
 ParameterProperties[0].curve := ctExponential;
 ParameterProperties[0].curveFactor := 1000;
 ParameterProperties[1].min := 0;
 ParameterProperties[1].max := 100;
 ParameterProperties[1].name := 'Dry/Wet';
 ParameterProperties[1].units := '%';
 ParameterProperties[1].curve := ctLinear;
 ParameterProperties[1].curveFactor := 1000;

 ParameterProperties[3].min := 0;
 ParameterProperties[3].max := 400;
 ParameterProperties[3].name := 'Attack';
 ParameterProperties[3].units := 'ms';
 ParameterProperties[3].curve := ctLinear;
 ParameterProperties[3].curveFactor := 5000;
 ParameterProperties[4].min := 0;
 ParameterProperties[4].max := 400;
 ParameterProperties[4].name := 'Release';
 ParameterProperties[4].units := 'ms';
 ParameterProperties[4].curve := ctLinear;
 ParameterProperties[4].curveFactor := 5000;
 ParameterProperties[5].min := 0;
 ParameterProperties[5].max := 400;
 ParameterProperties[5].name := 'Glide';
 ParameterProperties[5].units := 'ms';
 ParameterProperties[5].curve := ctLinear;
 ParameterProperties[5].curveFactor := 1000;
 ParameterProperties[6].min := 0;
 ParameterProperties[6].max := 400;
 ParameterProperties[6].name := 'Morph';
 ParameterProperties[6].units := 'ms';
 ParameterProperties[6].curve := ctLinear;
 ParameterProperties[6].curveFactor := 4000;
 ParameterProperties[7].min := 0;
 ParameterProperties[7].max := 5;
 ParameterProperties[7].name := 'Vowel';
 ParameterProperties[7].units := 'X';
 ParameterProperties[7].curve := ctLinear;
 ParameterProperties[7].curveFactor := 1000;
end;

function i_limit(v,l,u:Integer):Integer;
begin
 if v<l then result:=l
 else if v>u then result:=u else result:=v;
end;


procedure APlugin.processMIDI(ev: VstMidiEvent);
var time, data1, data2, status, ch: integer;
begin
 ch := ev.midiData[0] and $0F;
 status := ev.midiData[0] and $F0;
 data1 := ev.midiData[1] and $7F;
 data2 := ev.midiData[2] and $7F;
 time := ev.deltaFrames;
 if (status = $90) and (data2 > 0) then // "Note On" ?
 begin
  // data1 contains note number
  // data2 contains note velocity
  MIDI_NoteOn(ch, data1, data2, time);

  data1:=i_limit(data1,0,119);
  dfreq:=freqtable[data1];
  inc(notesplaying);
  cnt:=0;
  synth_active:=true;
  if notesplaying=1 then envstage:=1;
 end
 else if ((status = $90) and (data2 = 0)) or (status = $80) then // "Note Off" ?
 begin
  // data1 contains note number
  // data2 contains note off velocity
  MIDI_NoteOff(ch, data1, data2, time);
  dec(notesplaying);
  if (notesplaying<1)and(envstage<>0) then envstage:=3;
 end
 else if (status = $B0) then // "MIDI Controller" ?
 begin
  // data1 contains CC number
  // data2 contains data value
  if data1=$7e then
   begin
    notesplaying:=0;
    envstage:=3;
   end;
 end
 else if (status = $C0) then // "Program Change" ?
 begin
  // data1 contains program number
 end
end;

procedure APlugin.setParameter(index:Longint; value: Single);
var j,jj: integer;
begin
 inherited;
 case index of
  4: ;//sc1:=round(value*4000);
  5: ;//sc2:=round(value*5000);
  6: begin
      mtimermax:=round(value*samplerate*4);
      move(mix_cur[0],mix_src[0],5*sizeof(Single));
      move(mix_cur[0],mix_dst[0],5*sizeof(Single));
      while mtimer>mtimer do mtimer:=mtimer-mtimermax;
     end;
  7: begin
      jj:=round(value*10) mod 5;
      mtimer:=0;
      move(mix_cur[0],mix_src[0],5*sizeof(Single));
      FillChar(mix_dst[0],5*sizeof(Single),0);
      mix_dst[jj]:=1;
     end;
  end;
end;

function APlugin.DoFormant(i:single;n:integer):single;
var res:single;
    j:integer;
begin
 res:=(coeff[n][0]*i+
 coeff[n][1]*memory[n,0]+
 coeff[n][2]*memory[n,1]+
 coeff[n][3]*memory[n,2]+
 coeff[n][4]*memory[n,3]+
 coeff[n][5]*memory[n,4]+
 coeff[n][6]*memory[n,5]+
 coeff[n][7]*memory[n,6]+
 coeff[n][8]*memory[n,7]+
 coeff[n][9]*memory[n,8]+
 coeff[n][10]*memory[n,9])+p4;
 for j:=9 downto 1 do memory[n,j]:=memory[n,j-1];
 memory[n,0]:=res;
 result:=res;
end;

procedure APlugin.processAudio(const inputs,outputs: TArrayOfSingleArray; sampleframes: Integer);
var d:single;
    i,j:integer;
    jj:single;
    phase,yy:single;
begin
 for i:=0 to sampleframes-1 do
  begin
   if freq<dfreq then freq:=freq+0.0001+programs[curProgram].fPar[5]*0.3 else
   if freq>dfreq then freq:=freq-0.0001-programs[curProgram].fPar[5]*0.3;

   //envelope control
   if envstage=1 then
    begin
     synth_active:=true;
     envvalue:=envvalue+(programs[curProgram].fPar[3]/10000)+p4;
     if envvalue>=1 then envstage:=2;
    end
   else if envstage=2 then
    begin
     synth_active:=true;
     envvalue:=1;
    end
   else if envstage=3 then
    begin
     synth_active:=true;
     envvalue:=envvalue-(programs[curProgram].fPar[4]/10000)+p4;
     if envvalue<=0 then
      begin
       envstage:=0;
       envvalue:=0;
       synth_active:=false;
      end;
    end
   else
     begin
      envvalue:=0;
      synth_active:=false;
     end;

   if synth_active then
    begin //phase control
     jj:=samplerate/freq;
     phase:=(cnt/jj)+p4;
     cnt:=cnt+1;while (cnt>=jj) do cnt:=cnt-jj;

     //waveform generation (NOT bandlimited!)
     if (fwave>0.6) then yy:=2*random-1 else
     if (fwave>0.3) then if phase<0.5 then yy:=1 else yy:=-1
     else if phase<0.5 then yy:=2*phase else yy:=2*phase-2;

     yy:=yy*envvalue+p4;
     Outputs[0][i]:=yy;
     Outputs[1][i]:=Outputs[0][i];
    end else yy:=(Inputs[0][i]+Inputs[1][i])*0.5;

   //morph timer
   inc(mtimer);
   if mtimer>=mtimermax then
    begin
     mtimer:=mtimermax;
     move(mix_dst[0],mix_cur[0],5*sizeof(Single));
    end
   else
    begin
     d:=mtimer/mtimermax;
     for j:=0 to 4 do mix_cur[j]:=(1-d)*mix_src[j]+d*mix_dst[j];
    end;

   //formant filter call
   yy:=0.5*DoFormant(yy,0)*mix_cur[0]*0.19+
   DoFormant(yy,1)*mix_cur[1]*0.58+
   DoFormant(yy,2)*mix_cur[2]*0.48+
   DoFormant(yy,3)*mix_cur[3]*0.20+
   DoFormant(yy,4)*mix_cur[4]*0.41;

   //dry/wet mix
   Outputs[0][i]:=1.5*(1-programs[curProgram].fPar[0])*(Outputs[0][i]*(programs[curProgram].fPar[1])+(1-programs[curProgram].fPar[1])*yy);
   Outputs[1][i]:=1.5*(1-programs[curProgram].fPar[0])*(Outputs[1][i]*(programs[curProgram].fPar[1])+(1-programs[curProgram].fPar[1])*yy);

   //hard clipping
   f_limit(Outputs[0][i]);
   f_limit(Outputs[1][i]);
  end;
end;

end.
