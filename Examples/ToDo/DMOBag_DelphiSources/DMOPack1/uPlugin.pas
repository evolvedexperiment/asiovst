{
This is merely a wrapper for the call to the DirectX9 DMO
chorus/flanger/distortion objects.

Original C++ VST SDK by Steinberg (www.steinberg.net)
Delphi SDK translation by Frederic Vanmol (www.axiworld.be)
Template code by Tobias Fleischer/Tobybear (www.tobybear.de)

I AM NOT RESPONSIBLE FOR ANYTHING! USE AT YOUR OWN RISK!

VST is a registered trademark of Steinberg GmbH
}

unit uPlugin;

interface
uses Windows, uEditor, DAEffect, DAEffectX,
     DAudioEffect, DAudioEffectX,
     ActiveX, dmoChorus, dmoFlanger,
     dmoDistortion, dmoCompressor,
     DVSTUtils, DVSTExtra;

{$I config.inc}

const numEventsMax=250;
// maximal number of MIDI events per processing block,
// you might experiment to get better values for special
// MIDI only plugins, but generally this should be quite okay

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
  fgain:single;
  done:boolean;
  {$IFDEF Distortion}
  _dist:array[0..15] of tdcdmodistortion;
  {$ENDIF}
  {$IFDEF Compressor}
  _comp:array[0..15] of TDCDMOCompressor;
  {$ENDIF}

  {$IFDEF Chorus}
  _chorus:array[0..15] of tdcdmochorus;
  {$ENDIF}
  {$IFDEF Flanger}
  _flanger:array[0..15] of tdcdmoflanger;
  {$ENDIF}
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

 fpar[0]:=0.5;
 fpar[1]:=0.9;
 fpar[2]:=0.7;
 fpar[3]:=0.8;
 fpar[4]:=0.2;
 fpar[6]:=0.5;

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
 
 {$IFDEF Compressor}
 for i:=0 to numChannels-1 do
 begin
  _comp[i]:=tdcdmocompressor.create(nil);
  _comp[i].Init(round(samplerate),32,1,true);
  _comp[i].Enabled:=true;
 end;
 {$ENDIF}

 {$IFDEF Distortion}
 for i:=0 to numChannels-1 do
 begin
  _dist[i]:=tdcdmodistortion.create(nil);
  _dist[i].Init(round(samplerate),32,1,true);
  _dist[i].Enabled:=true;
 end;
 {$ENDIF}

 {$IFDEF Chorus}
 for i:=0 to numChannels-1 do
 begin
  _chorus[i]:=tdcdmochorus.create(nil);
  _chorus[i].Init(round(samplerate),32,1,true);
  _chorus[i].Enabled:=true;
 end;
 {$ENDIF}

 {$IFDEF Flanger}
 for i:=0 to numChannels-1 do
 begin
  _flanger[i]:=tdcdmoflanger.create(nil);
  _flanger[i].Init(round(samplerate),32,1,true);
  _flanger[i].Enabled:=true;
 end;
 {$ENDIF}

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
 s:=inttostr(PlugID)+inttostr(numchannels-1);
 setUniqueID(FourCharToLong('D','M',s[1],s[2]));

 // initial program names
 for i:=0 to kNumPrograms-1 do StrPCopy(programs[i].name,'Preset '+inttostr(i));

 // initialize with program 0
 curprogram:=0;
 setProgram(0);
end;

destructor APlugin.Destroy;
var i:integer;
begin
 inherited Destroy;
 {$IFDEF Distortion}
 for i:=0 to numchannels-1 do _dist[i].free;
 {$ENDIF}

 {$IFDEF Chorus}
 for i:=0 to numchannels-1 do _chorus[i].free;
 {$ENDIF}

 {$IFDEF Compressor}
 for i:=0 to numchannels-1 do _comp[i].free;
 {$ENDIF}

 {$IFDEF Flanger}
 for i:=0 to numchannels-1 do _flanger[i].free;
 {$ENDIF}

 // destroy the created programs
 for i:=0 to kNumPrograms-1 do
 begin
  programs[i].Free;
  programs[i]:=nil;
 end;

 // destroy MIDI handler
 for i:=0 to numeventsmax do dispose(xevent[i]);
 dispose(pxevent);
 CoUninitialize; 
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
var i:integer;
begin
 if (value>1) then value:=1 else if (value<0) then value:=0;
 if (index>=kNumParams) or (index<0) then exit;
 fpar[index]:=value;
 programs[curprogram].fpar[index]:=value;

 {$IFDEF Compressor}
 for i:=0 to numchannels-1 do
 case index of
 0:_comp[i].Gain:=(fpar[0]*2-1)*60;
 1:_comp[i].attack:=fpar[1]*(500-0.01)+0.01;
 2:_comp[i].release:=fpar[2]*(3000-50)+50;
 3:_comp[i].threshold:=fpar[3]*60-60;
 4:_comp[i].ratio:=fpar[4]*99+1;
 5:_comp[i].predelay:=fpar[5]*4;
 else
 end;
 {$ENDIF}

 {$IFDEF Distortion}
 for i:=0 to numchannels-1 do
 case index of
 0:begin
  _dist[i].gain:=fpar[0]*60-60;
  fgain:=fpar[0];//power(10,(_dist.gain/10));
 end;
 1:_dist[i].edge:=fpar[1]*100;
 2:_dist[i].PostEQCenterFrequency:=(fpar[2]*7900)+100;
 3:_dist[i].PostEQBandwidth:=(fpar[3]*7900)+100;
 4:_dist[i].PreLowpassCutoff:=(fpar[4]*7900)+100;
 else
 end;
 {$ENDIF}

 {$IFDEF Chorus}
 for i:=0 to numchannels-1 do
 case index of
 0:_chorus[i].WetDryMix:=fpar[0]*100;
 1:_chorus[i].Depth:=fpar[1]*100;
 2:_chorus[i].feedback:=(fpar[2]*2-1)*99;
 3:_chorus[i].frequency:=fpar[3]*10;
 4:_chorus[i].delay:=fpar[4]*20;
 5:_chorus[i].Waveform:=round(fpar[5]);
 6:_chorus[i].Phase:=round(fpar[6]*4);
 else
 end;
 {$ENDIF}

 {$IFDEF Flanger}
 for i:=0 to numchannels-1 do
 case index of
 0:_flanger[i].WetDryMix:=fpar[0]*100;
 1:_flanger[i].Depth:=fpar[1]*100;
 2:_flanger[i].feedback:=(fpar[2]*2-1)*99;
 3:_flanger[i].frequency:=fpar[3]*10;
 4:_flanger[i].delay:=fpar[4]*20;
 5:_flanger[i].Waveform:=round(fpar[5]);
 6:_flanger[i].Phase:=round(fpar[6]*4);
 else
 end;
 {$ENDIF}

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
 {$IFDEF Compressor}
 for i:=0 to numchannels-1 do
  _comp[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Distortion}
 for i:=0 to numchannels-1 do
  _dist[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Chorus}
 for i:=0 to numchannels-1 do
  _chorus[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Flanger}
 for i:=0 to numchannels-1 do
  _flanger[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 for i:=0 to sampleFrames-1 do
  for j:=0 to numchannels-1 do
  begin
   Outs[j,i]:=Outs[j,i]+Ins[j,i];
   {$IFDEF Distortion}
   if Outs[j,i]>1 then Outs[j,i]:=1 else
   if Outs[j,i]<-1 then Outs[j,i]:=-1;
   Outs[j,i]:=Outs[j,i]*fgain;
   {$ENDIF}
  end;
end;

procedure APlugin.processReplacing(inputs, outputs: PPSingle; sampleframes: Longint);
var Ins:TArrayOfSingleArray absolute inputs;
    Outs:TArrayOfSingleArray absolute outputs;
    i,j:Integer;
begin
 {$IFDEF Compressor}
 for i:=0 to numchannels-1 do
  _comp[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Distortion}
 for i:=0 to numchannels-1 do
  _dist[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Chorus}
 for i:=0 to numchannels-1 do
  _chorus[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 {$IFDEF Flanger}
 for i:=0 to numchannels-1 do
  _flanger[i].Process(Ins[i],sampleFrames*4);
 {$ENDIF}

 for i:=0 to sampleFrames-1 do
  for j:=0 to numchannels-1 do
  begin
   Outs[j,i]:=Ins[j,i];
   {$IFDEF Distortion}
   if Outs[j,i]>1 then Outs[j,i]:=1 else
   if Outs[j,i]<-1 then Outs[j,i]:=-1;
   Outs[j,i]:=Outs[j,i]*fgain;
   {$ENDIF}
  end;
end;

function APlugin.getInputProperties(index:longint;properties:PVstPinProperties):boolean;
begin
 Result:=false;
 if (index<numchannels) then
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
 {$IFDEF Distortion}
 0:StrPCopy(text,'Gain');
 1:StrPCopy(text,'Edge');
 2:StrPCopy(text,'PostEQ Frequency');
 3:StrPCopy(text,'PostEQ Bandwidth');
 4:StrPCopy(text,'PreLowpass Cutoff');
 {$ENDIF}

 {$IFDEF Compressor}
 0:StrPCopy(text,'Gain');
 1:StrPCopy(text,'Attack');
 2:StrPCopy(text,'Release');
 3:StrPCopy(text,'Threshold');
 4:StrPCopy(text,'Ratio');
 5:StrPCopy(text,'PreDelay');
 {$ENDIF}

 {$IFDEF Flanger}
 0:StrPCopy(text,'Wet/Dry Mix');
 1:StrPCopy(text,'Depth');
 2:StrPCopy(text,'Feedback');
 3:StrPCopy(text,'Frequency');
 4:StrPCopy(text,'Delay');
 5:StrPCopy(text,'LFO Waveform');
 6:StrPCopy(text,'LFO Phase');
 {$ENDIF}

 {$IFDEF Chorus}
 0:StrPCopy(text,'Wet/Dry Mix');
 1:StrPCopy(text,'Depth');
 2:StrPCopy(text,'Feedback');
 3:StrPCopy(text,'Frequency');
 4:StrPCopy(text,'Delay');
 5:StrPCopy(text,'LFO Waveform');
 6:StrPCopy(text,'LFO Phase');
 {$ENDIF}
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
begin
 r.top:=0;
 r.left:=0;
 r.bottom:=375;
 r.right:=465;
 if assigned(editor) then
 begin
  r.bottom:=editor.ClientHeight;
  r.right:=editor.ClientWidth;
 end;
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

 {$IFDEF Compressor}
 editor.label3.caption:='DX9 DMO Compressor';
 editor.label4.caption:='DX9 DMO Compressor';
 editor.Label9.Visible:=false;
 editor.Label10.Visible:=false;
 editor.Label11.Visible:=false;
 editor.Label12.Visible:=false;
 editor.Label15.Visible:=true;
 editor.par5.Visible:=true;
 {$ENDIF}

 {$IFDEF Distortion}
 editor.label3.caption:='DX9 DMO Distortion';
 editor.label4.caption:='DX9 DMO Distortion';
 editor.Label9.Visible:=false;
 editor.Label10.Visible:=false;
 editor.Label11.Visible:=false;
 editor.Label12.Visible:=false;
 {$ENDIF}
 {$IFDEF Chorus}
 editor.label3.caption:='DX9 DMO Chorus';
 editor.label4.caption:='DX9 DMO Chorus';
 {$ENDIF}
 {$IFDEF Flanger}
 editor.label3.caption:='DX9 DMO Flanger';
 editor.label4.caption:='DX9 DMO Flanger';
 {$ENDIF}

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


