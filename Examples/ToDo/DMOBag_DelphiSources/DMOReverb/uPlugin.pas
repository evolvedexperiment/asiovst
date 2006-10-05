{
This is merely a wrapper for the DirectX9 DMOs

Original C++ VST SDK by Steinberg (www.steinberg.net)
Delphi SDK translation by Frederic Vanmol (www.axiworld.be)
Template code by Tobias Fleischer/Tobybear (www.tobybear.de)

I AM NOT RESPONSIBLE FOR ANYTHING! USE AT YOUR OWN RISK!

VST is a registered trademark of Steinberg GmbH
}

unit uPlugin;

interface
uses Windows, uEditor, DAEffect, DAEffectX, ActiveX,
     DAudioEffect, DAudioEffectX, DVSTExtra,
     dmoI3DL2Reverb, DVSTUtils, dmoconst;

{$I config.inc}

const numEventsMax=250;
      
 type APluginEditor=class;

 APluginProgram=class
 private
  fpar:array[0..kNumParams-1] of single;
  name:array[0..50] of char;
 public
  constructor Create;
 end;

 //this is the plugin class
 APlugin=class(AudioEffectX)
 private
  // here are some internal variables:
  pxevent:pvstevents;
  xevent:array[0..numEventsMax] of pvstmidievent;
  numChannels,numEvents:integer;
  host1,host2:string;
  fpar:array[0..kNumParams-1] of single;
  done:boolean;
  _rv:array [0..15] of tdcdmoi3dl2reverb;
 public
  programs:array[0..kNumPrograms-1] of APluginProgram;
  vu:single;
  function GetPluginEditor:APluginEditor;
  constructor CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);virtual;

  destructor Destroy;override;
  procedure process(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure processReplacing(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure processMIDI(pos:longint);

  procedure MIDI_NoteOn(ch,note,val:integer);
  procedure MIDI_NoteOff(ch,note,val:integer);
  procedure MIDI_PolyAftertouch(ch,note,val:integer);
  procedure MIDI_CC(ch,num,val:integer);
  procedure MIDI_ProgramChange(ch,val:integer);
  procedure MIDI_ChannelAftertouch(ch,val:integer);
  procedure MIDI_PitchBend(ch,val:integer);
  procedure MIDI_PitchBend2(ch,x1,x2:integer);

  procedure setProgram(aProgram:Longint);override;
  function canDo(text:pchar):longint;override;
  procedure setProgramName(name:PChar);override;
  procedure getProgramName(name:PChar);override;
  procedure setParameter(index:Longint;value:Single);override;
  function getParameter(index:Longint):Single;override;
  function getVu:Single;override;
  function processEvents(ev:PVSTEvents):longint;override;
  procedure resume;override;
  procedure suspend;override;
  procedure getParameterLabel(index:longint;text:pchar);override;
  procedure getParameterDisplay(index:longint;text:pchar);override;
  procedure getParameterName(index:longint;text:pchar);override;
  function getInputProperties(index:longint;properties:PVstPinProperties):boolean;override;
  function getOutputProperties(index:longint;properties:PVstPinProperties):boolean;override;
  function getProgramNameIndexed(category,index:longint;text:pchar):boolean;override;
  function getEffectName(name:pchar):boolean;override;
  function getVendorString(text:pchar):boolean;override;
  function getProductString(text:pchar):boolean;override;
  function getVendorVersion:longint;override;
  property PluginEditor:APluginEditor read GetPluginEditor;
 end;

 //this is the editor class
 APluginEditor=class(AEffEditor)
 private
  r:ERect;
  useCount:Longint;
  Editor:TPluginEditorWindow; 
  systemWindow:HWnd;
  function GetPlugin:APlugin;
 public
  constructor Create(effect:AudioEffect);override;
  destructor Destroy;override;
  function getRect(var rect: PERect): longint; override;
  function open(ptr:Pointer):Longint;override;
  procedure close;override;
  procedure idle;override;
  procedure update;override;
  property Plugin:APlugin read GetPlugin;
 end;

implementation
uses SysUtils;

function APlugin.canDo(text:pchar):longint;
begin
 Result:=-1;
 // there are way more of these so called "CanDo"s, check DAudioEffectX!

 // can plugin receive MIDI?
 if kHasMidiIn then
 begin
  if StrComp(text,'receiveVstEvents')=0 then Result:=1;
  if StrComp(text,'receiveVstMidiEvent')=0 then Result:=1;
 end;

 // can plugin send MIDI?
 if kHasMidiOut then
 begin
  if StrComp(text,'sendVstEvents')=0 then Result:=1;
  if StrComp(text,'sendVstMidiEvent')=0 then Result:=1;
 end;

 // can plugin sync to host?
 if kSyncToHost then
  if StrComp(text,'receiveVstTimeInfo')=0 then Result:=1;
end;

constructor APluginProgram.Create;
var i:integer;
begin
 // Here the parameters for a program are inititialized
 inherited Create;
 for i:=0 to kNumParams-1 do fpar[i]:=0;
 
 fpar[0]:=0.9;
 fpar[1]:=0.9;
 fpar[2]:=0.7;
 fpar[3]:=0.8;
 fpar[4]:=0.2;
 fpar[6]:=0.5;
 fpar[12]:=1;

 StrCopy(name,'Init'); // set program name
end;

constructor APlugin.CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);
var i:integer;
    t:array[0..50] of char;
    s:string;
begin
 inherited Create(audioMaster,knumprograms,kNumParams);
 if samplerate<8000 then samplerate:=44100;

 s:=uppercase(getdllfilename);
 numChannels:=1;
 if (pos('STEREO',s)>0) then numChannels:=2;
 for i:=16 downto 1 do
  if (pos(inttostr(i)+'CH',s)>0)
  or (pos(inttostr(i)+'-CH',s)>0) then
  begin
   numChannels:=i;
   break;
  end;
 CoInitialize(nil);

 for i:=0 to numChannels-1 do
 begin
  _rv[i]:=tdcdmoi3dl2reverb.create(nil);
  _rv[i].Init(round(samplerate),32,1,true);
  _rv[i].Enabled:=true;
 end;

 // create and initialize the MIDI handler
 for i:=0 to numeventsmax do
 begin
  new(xevent[i]);
  xevent[i]^.vType:=1;
  xevent[i]^.mididata[3]:=0;
  xevent[i]^.reserved1:=99;
  xevent[i]^.deltaFrames:=-99;
  xevent[i]^.noteLength:=0;
  xevent[i]^.noteOffset:=0;
 end;
 new(pxevent);
 pxevent^.numEvents:=1;pxevent^.events[0]:=pvstevent(xevent[0]);
 numevents:=0;

 // host1=name of host vendor, host2=name of host
 gethostvendorstring(t);host1:=strpas(t);
 gethostproductstring(t);host2:=strpas(t);

 // create the programs
 for i:=kNumPrograms-1 downto 0 do programs[i]:=APluginProgram.Create;

 // create the editor window
 editor:=APluginEditor.Create(Self);

 randomize; // important if you use the random() function!
 suspend;

 // set some variables, according to the constants in config.inc
 hasVu(false);
 setNumInputs(numchannels);
 setNumOutputs(numchannels);
 canMono(kCanMono);
 canProcessReplacing(kCanReplacing);
 isSynth(kIsSynth);
 s:=inttostr(kPlugID)+inttostr(numchannels-1);
 setUniqueID(FourCharToLong('D','M',s[1],s[2]));

 // initial program names
 for i:=0 to kNumPrograms-1 do StrPCopy(programs[i].name,'Preset '+inttostr(i));

 // initialize with program 0
 curprogram:=0;
 setProgram(0);
 setparameter(kpreset,0);
end;

destructor APlugin.Destroy;
var i:integer;
begin
 inherited Destroy;
 for i:=0 to numchannels-1 do _rv[i].free;

 // destroy the created programs
 for i:=0 to kNumPrograms-1 do
 begin
  programs[i].Free;
  programs[i]:=nil;
 end;

 // destroy MIDI handler
 for i:=0 to numeventsmax do dispose(xevent[i]);
 dispose(pxevent);
 CoUnInitialize;
end;

procedure APlugin.setProgram(aProgram: Longint);
var i:integer;
begin
 if (aprogram<0)or(aprogram>knumprograms-1) then exit;
 curProgram:=aProgram;
 for i:=0 to kNumParams-1 do SetParameter(i,programs[curprogram].fpar[i]);
 // security check, "done" is only true when there is no editor window open
 if done then exit;
 if assigned(editor) then editor.update;
end;

procedure APlugin.setProgramName(name:PChar);
begin
 StrCopy(programs[curProgram].name, name);
 if done then exit;if assigned(editor) then editor.update;
end;

procedure APlugin.getProgramName(name: PChar);
begin StrCopy(name, programs[curProgram].name) end;

procedure APlugin.suspend;
begin
 // buffers are normally cleared/initialized here
end;

function APlugin.getVu:Single;
var cvu:Single;
begin
 cvu:=vu;
 vu:=0;
 Result:=cvu;
end;

procedure APlugin.setParameter(index:Longint;value:Single);
var i,j:integer;
begin
 if index=kpreset then
 begin
  i:=round(value*50);
  for j:=0 to numchannels-1 do
   _rv[j].Preset:=TDCDMOI3DL2ReverbPreset(i);
  fpar[0]:=(_rv[0].room+10000)/10000;
  fpar[1]:=(_rv[0].roomhf+10000)/10000;
  fpar[2]:=_rv[0].roomrollofffactor/10;
  fpar[3]:=(_rv[0].decaytime-0.1)/19.9;
  fpar[4]:=(_rv[0].decayhfratio-0.1)/1.9;
  fpar[5]:=-(_rv[0].reflections-10000)/11000;
  fpar[6]:=_rv[0].reflectionsdelay/0.3;
  fpar[7]:=(_rv[0].reverb+10000)/12000;
  fpar[8]:=_rv[0].reverbdelay*10;
  fpar[9]:=_rv[0].diffusion/100;
  fpar[10]:=_rv[0].density/100;
  fpar[11]:=(_rv[0].hfreference-20)/(20000-20);
  fpar[12]:=_rv[0].quality/3;
  if assigned(editor) then editor.update;
  exit;
 end;
 if (value>1) then value:=1 else if (value<0) then value:=0;
 if (index>=kNumParams) or (index<0) then exit;
 fpar[index]:=value;
 programs[curprogram].fpar[index]:=value;
{
    property Room : Longint -10000 0
    property RoomHF : Longint  -10000 0
    property RoomRolloffFactor : Single 0 10
    property DecayTime : Single 0.1 20
    property DecayHFRatio : Single 0.1 2.0
    property Reflections : Longint 10000 -1000
    property ReflectionsDelay : Single  0 0.3
    property Reverb : Longint -10000 2000
    property ReverbDelay : Single 0.0 0.1
    property Diffusion : Single 0 100
    property Density : Single 0 100
    property HFReference : Single 20 20000
    property Quality : Longint 0 3
 }
 for j:=0 to numchannels-1 do
 case index of
 0:_rv[j].room:=round(fpar[0]*10000-10000);
 1:_rv[j].roomhf:=round(fpar[1]*10000-10000);
 2:_rv[j].roomrollofffactor:=fpar[2]*10;
 3:_rv[j].decaytime:=fpar[3]*19.9+0.1;
 4:_rv[j].decayhfratio:=fpar[4]*1.9+0.1;
 5:_rv[j].reflections:=round(10000-fpar[5]*11000);
 6:_rv[j].reflectionsdelay:=fpar[6]*0.3;
 7:_rv[j].reverb:=round(fpar[7]*12000-10000);
 8:_rv[j].reverbdelay:=fpar[8]*0.1;
 9:_rv[j].diffusion:=fpar[9]*100;
 10:_rv[j].density:=fpar[10]*100;
 11:_rv[j].hfreference:=fpar[11]*(20000-20)+20;
 12:_rv[j].quality:=round(fpar[12]*3);
 else
 end;

 if assigned(editor) then editor.update;
end;

function APlugin.getParameter(index: Longint): Single;
begin
 if (index>=kNumParams) then result:=0
 else result:=fpar[index];
end;

type TSingleArray=array of single;
     TArrayOfSingleArray=array of TSingleArray;

procedure APlugin.process(inputs,outputs:PPSingle;sampleframes:Longint);
var Ins:TArrayOfSingleArray absolute inputs;
    Outs:TArrayOfSingleArray absolute outputs;
    i,j:Integer;
begin
 for i:=0 to numchannels-1 do
  _rv[i].Process(Ins[i],sampleFrames*4);

 for i:=0 to sampleFrames-1 do
  for j:=0 to numchannels-1 do
   Outs[j,i]:=Outs[j,i]+Ins[j,i];
end;

procedure APlugin.processReplacing(inputs, outputs: PPSingle; sampleframes: Longint);
var Ins:TArrayOfSingleArray absolute inputs;
    Outs:TArrayOfSingleArray absolute outputs;
    i,j:Integer;
begin
 for i:=0 to numchannels-1 do
  _rv[i].Process(Ins[i],sampleFrames*4);

 for i:=0 to sampleFrames-1 do
  for j:=0 to numchannels-1 do
   Outs[j,i]:=Ins[j,i];
end;

function APlugin.getInputProperties(index:longint;properties:PVstPinProperties):boolean;
begin
 Result:=false;
 if (index<NumChannels) then
 begin
  // set name of input channel:
  StrPCopy(properties^.vLabel,'Input #'+inttostr(index+1));

  // activate input channel:
  properties^.flags:=kVstPinIsActive;

  // make channel stereo bound (2-channel signal):
  if numchannels=2 then
   properties^.flags:=properties^.flags or kVstPinIsStereo;

  Result:=true;
 end;
end;

function APlugin.getOutputProperties(index:longint;properties:PVstPinProperties):boolean;
begin
 Result:=false;
 if (index<numchannels) then
 begin
  // set name of output channel:
  StrPCopy(properties^.vLabel,'Output #'+inttostr(index+1));

  // activate output channel:
  properties^.flags:=kVstPinIsActive;

  // make channel stereo bound (2-channel signal):
  if numchannels=2 then
   properties^.flags:=properties^.flags or kVstPinIsStereo;

  Result:=true;
 end;
end;

function APlugin.getProgramNameIndexed(category,index:longint;text:pchar):boolean;
begin
 Result:=false;
 if (index<kNumPrograms) then
 begin
  StrCopy(text,programs[index].name);
  Result:=true;
 end;
end;

procedure APlugin.getParameterName(index:longint;text:pchar);
begin
 if (index>kNumParams-1) then StrPCopy(text,'undefined') else
 case index of
 0:StrPCopy(text,'Room');
 1:StrPCopy(text,'Room HF');
 2:StrPCopy(text,'Room Rolloff');
 3:StrPCopy(text,'Decay Time');
 4:StrPCopy(text,'Decay HF Ratio');
 5:StrPCopy(text,'Reflections');
 6:StrPCopy(text,'Reflections Delay');
 7:StrPCopy(text,'Reverb');
 8:StrPCopy(text,'Reverb Delay');
 9:StrPCopy(text,'Diffusion');
 10:StrPCopy(text,'Density');
 11:StrPCopy(text,'HF Reference');
 12:StrPCopy(text,'Quality');
 else
  StrPCopy(text,'undefined');
 end;
end;

procedure APlugin.getParameterDisplay(index:longint;text:pchar);
begin
 if (index>kNumParams-1) then StrPCopy(text,'undefined') else
 float2string(fpar[index],text);
end;

procedure APlugin.getParameterLabel(index:longint;text:pchar);
begin StrCopy(text,'%') end;
function APlugin.GetPluginEditor:APluginEditor;
begin Result:=(editor as APluginEditor) end;
function APlugin.getEffectName(name:pchar): boolean;
begin StrCopy(name,PChar(kEffectName+' '+inttostr(numchannels)));result:=true end;
function APlugin.getVendorString(text:pchar):boolean;
begin StrCopy(text,kVendor);result:=true end;
function APlugin.getProductString(text:pchar):boolean;
begin StrPCopy(text,kEffectName);result:=true end;
function APlugin.getVendorVersion:longint;
begin result:=1 end;

function APlugin.processEvents(ev:PVstEvents):longint;
var i,j,k:integer;
    event:PVstMidiEvent;
begin
 for i:=0 to ev^.numEvents-1 do
 if (ev.events[i].vtype=kVstMidiType) then
 begin
  event:=PVstMidiEvent(ev^.events[i]);
  j:=-1;
  for k:=1 to numeventsmax do
   if xevent[k]^.deltaFrames=-99 then
   begin
    j:=k;
    break;
   end;
  if (j>0)and(numevents<numeventsmax) then
  begin
   inc(numevents);
   xevent[j]^.midiData[0]:=event.midiData[0];
   xevent[j]^.midiData[1]:=event.midiData[1];
   xevent[j]^.midiData[2]:=event.midiData[2];
   xevent[j]^.midiData[3]:=event.midiData[3];
   xevent[j]^.vType:=event.vType;
   xevent[j]^.byteSize:=event.byteSize;
   xevent[j]^.deltaFrames:=event.deltaframes;
   xevent[j]^.flags:=event.flags;
   xevent[j]^.noteLength:=event.notelength;
   xevent[j]^.noteOffset:=event.noteoffset;
   xevent[j]^.detune:=event.detune;
   xevent[j]^.noteOffVelocity:=event.noteOffVelocity;
   xevent[j]^.reserved1:=99;
   xevent[j]^.reserved2:=event.reserved2;
  end;
 end;
 Result:=1;
end;

procedure APlugin.resume;
begin
 wantEvents(1); // important for all plugins that receive MIDI!
end;

// APluginEditor

constructor APluginEditor.Create(effect:AudioEffect);
begin
 inherited Create(effect);
 useCount:=0;
end;

destructor APluginEditor.Destroy;
begin
 if assigned(editor) then
 begin
  Plugin.done:=true;
  Editor.Free;
  Editor:=nil;
  systemWindow:=0;
 end;
 inherited Destroy;
end;

function APluginEditor.getRect(var rect: PERect): longint;
//function APluginEditor.getRect(rect:PPERect):Longint;
begin
 r.top:=0;
 r.left:=0;
 r.right:=466;
 r.bottom:=401;
 rect:=@r;
 Result:=1;
end;

function APluginEditor.open(ptr:Pointer):Longint;
var i:integer;
begin
 systemWindow:=HWnd(ptr);
 Inc(useCount);
 if (useCount=1)or (not assigned(editor)) then
 begin
  Editor:=TPluginEditorWindow.CreateParented(systemWindow);
  Editor.SetBounds(0,0,Editor.Width,Editor.Height);
  for i:=0 to kNumParams-1 do Editor.fpar[i]:=Plugin.fpar[i];
  Editor.Effect:=Self.Plugin;
 end;

 editor.label14.caption:='channels: '+inttostr(plugin.numChannels);
 Plugin.done:=false;
 Editor.Show;
 Editor.Update;
 Editor.Updater.Enabled:=true;
 Editor.UpdaterTimer(nil);
 Editor.par0Change(Editor.par0);
 Editor.par0Change(Editor.par1);
 Editor.par0Change(Editor.par2);
 Editor.par0Change(Editor.par3);
 Editor.par0Change(Editor.par4);
 Editor.par0Change(Editor.par5);
 Editor.par0Change(Editor.par6);
 Editor.par0Change(Editor.par7);
 Editor.par0Change(Editor.par8);
 Editor.par0Change(Editor.par9);
 Editor.par0Change(Editor.par10);
 Editor.par0Change(Editor.par11);
 for i := 0 to plugin._rv[0].PresetCount-1 do
  Editor.presets.Items[i].Caption:=inttostr(i)+': '+plugin._rv[0].PresetName[i];
 Result:=1;
end;

procedure APluginEditor.close;
begin
 dec(useCount);if useCount<0 then useCount:=0;
 if useCount=0 then
 begin
  Plugin.done:=true;
  Editor.Free;Editor:=nil;
  systemWindow:=0;
 end;
end;

procedure APluginEditor.idle;
begin
 if (Plugin.done) or (not Assigned(Editor)) then exit;
end;

procedure APluginEditor.update;
var i:integer;
begin
 if (Plugin.done) or (not Assigned(Editor)) then exit;
 for i:=0 to kNumParams-1 do Editor.fpar[i]:=Plugin.fpar[i];
end;

function APluginEditor.GetPlugin: APlugin;
begin Result:=(effect as APlugin) end;

{$I MidiHandler.inc}


// MIDI and audio processing functions:

procedure APlugin.processMIDI(pos:longint);
// Here you can define the MIDI handling
var data1,data2,status,ch,i:integer;
begin
 for i:=1 to numEventsMax do
 with xevent[i]^ do
 begin
 if deltaframes>0 then dec(deltaframes);
 if deltaFrames=0 then
 begin
  dec(numevents);
  if numevents<0 then begin numevents:=0;exit; end;
  ch:=midiData[0] and $0f;
  status:=midiData[0] and $f0;
  data1:=(midiData[1] and $7F); // midi data 1
  data2:=(midiData[2] and $7F); // midi data 2
  if (status=$90)and(data2>0) then // "Note On" ?
  begin
   // data1 contains note number
   // data2 contains note velocity
//   MIDI_NoteOn(ch,data1,data2);
  end
  else if ((status=$90)and(data2=0))or(status=$80) then // "Note Off" ?
  begin
   // data1 contains note number
   // data2 contains note off velocity
//   MIDI_NoteOff(ch,data1,data2);
  end
  else if (status=$A0) then // "Polyphonic Aftertouch" ?
  begin
   // data1 contains note number
   // data2 contains aftertouch value
//   MIDI_PolyAftertouch(ch,data1,data2);
  end
  else if (status=$B0) then // "MIDI Controller" ?
  begin
   // data1 contains CC number
   // data2 contains data value
   if (data1>=70)and(data1<=70+knumparams) then
    setparameter(data1-70,data2/127);
//   MIDI_CC(ch,data1,data2);
  end
  else if (status=$C0) then // "Program Change" ?
  begin
   // data1 contains program number
//   MIDI_ProgramChange(ch,data1);
  end
  else if (status=$D0) then // "Channel Aftertouch" ?
  begin
   // data1 contains channel aftertouch value
//   MIDI_ChannelAftertouch(ch,data1);
  end
  else if (status=$E0) then // "Pitchbend" ?
  begin
   // data1 and data2 make up the 12 bit pitchbend value
//   MIDI_PitchBend2(ch,data1,data2);
  end;
  deltaframes:=-99;
 end;
 end;
end;


end.


