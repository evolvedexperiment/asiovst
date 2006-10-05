unit uPlugin;
interface
uses Windows, DVSTUtils, uEditor, DAEffect, DAEffectX, DAudioEffect, DAudioEffectX;

const
 kID = 'TBCW'; // unique plugin identifier, has to be different for every plugin
 kChannelID='TBCW'; // string displayed in the VSTi channel mixer
 kEffectName = 'CowDelay'; // effect name
 kProduct = 'CowDelay'; // product name
 kVendor = 'Tobybear'; // vendor name
 kIsSynth = false; //false=audio effect, true=synth
 kNumInputs = 2; // number of inputs
 kNumOutputs = 2; // number of outputs
 kCanMono = true; // can be fed with mono signals?
 kCanReplacing = true; //processreplacing() is called instead of process()

 // now follow the 16 constants that allow you to access the 16 parameters
 // you can of course add more :-)
 kWet=0;
 kDelayPan=1;
 kDelayLen=2;
 kDelayFB=3;
 kCutoff=4;
 kResonance=5;
 kFType=6;
 kOutVol=7;
 kThresh=8;
 kLength=9;
 kDir=10;

 kNumParams = 16;
 kNumPrograms = 16; // 16 programs per fxb bank

 type APluginEditor=class;
 APluginProgram=class
 private
  fWet,fDelayPan,fDelayLen,fDelayFB,fThresh,fLength,fDir,
  fCutoff,fResonance,fFType,fOutVol:single;
  name:array[0..50] of char;
 public
  constructor Create;
 end;

 APlugin=class(AudioEffectX)
 private
  buffer:array[0..44100] of single; //x
  tcnt,buffer_cnt:longint;

  Cut,Reso,old1,old2:single;

  fWet,fDelayPan,fDelayLen,fDelayFB,fThresh,fLength,fDir,fWWet,
  fCutoff,fResonance,fFType,fOutVol:single;
  done:boolean;
 public
  programs:array[0..kNumPrograms-1] of APluginProgram;
  vu:single;
  function GetPluginEditor:APluginEditor;
  constructor CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);virtual;
  destructor Destroy;override;
  procedure DoProcess(var i1:single;var i2:single);
  procedure process(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure processReplacing(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure setProgram(aProgram:Longint);override;
  function canDo(text:pchar):longint;override;
  procedure setProgramName(name:PChar);override;
  procedure getProgramName(name:PChar);override;
  procedure setParameter(index:Longint;value:Single);override;
  function getParameter(index:Longint):Single;override;
  function getVu:Single;override;
  function DoFilter(i:single;cutoff,res:single):Single;
  function processEvents(ev:PVSTEvents):longint;override;
  procedure resume;override;
  procedure suspend;override;
  procedure getParameterLabel(index:longint;text:pchar);override;
  procedure getParameterDisplay(index:longint;text:pchar);override;
  procedure getParameterName(index:longint;text:pchar);override;
  function getOutputProperties(index:longint;properties:PVstPinProperties):boolean;override;
  function getProgramNameIndexed(category,index:longint;text:pchar):boolean;override;
  function getEffectName(name:pchar):boolean;override;
  function getVendorString(text:pchar):boolean;override;
  function getProductString(text:pchar):boolean;override;
  function getVendorVersion:longint;override;
  property PluginEditor:APluginEditor read GetPluginEditor;
 end;

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
  function getRect(var rect:PERect):Longint;override;
  function open(ptr:Pointer):Longint;override;
  procedure close;override;
  procedure idle;override;
  procedure update;override;
  property Plugin:APlugin read GetPlugin;
 end;

implementation
uses SysUtils;
var memory:array[0..9] of double;

function APlugin.canDo(text:pchar):longint;
begin
 Result:=-1;

 // The following two lines are needed so that host allow sendin
 // MIDI data to your plugin!
 // There are way more of these so called "CanDo"s, check DAudioEffectX!

 if StrComp(text, 'receiveVstEvents') = 0 then Result := 1;
 if StrComp(text, 'receiveVstMidiEvent') = 0 then Result := 1;
end;

constructor APluginProgram.Create;
begin
 inherited Create;
 // Here the parameters for a program are inititialized
 fWet:=0.5;
 fDir:=0;
 fThresh:=1;
 fLength:=1;
 fDelayPan:=0.5;
 fDelayLen:=0.5;
 fDelayFB:=0.4;
 fCutoff:=1;
 fResonance:=0;
 fFType:=0;
 fOutVol:=1;
 StrCopy(name,'Init'); // set program name
end;

constructor APlugin.CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);
var i:integer;
begin
 inherited Create(audioMaster,knumprograms,kNumParams);
 tcnt:=0;
 for i:=0 to 9 do memory[i]:=0;
 // create the programs
 for i:=kNumPrograms-1 downto 0 do programs[i] := APluginProgram.Create;

 editor:=APluginEditor.Create(Self);
 randomize; // important if you use the random() function!
 suspend;

 // set some important variables, according to the constants
 // you specified at the beginning of this file
 hasVu(false);
 setNumInputs(KNumInputs);
 setNumOutputs(KNumOutputs);
 canMono(KCanMono);
 canProcessReplacing(KCanReplacing);
 isSynth(KIsSynth);
 setUniqueID(FourCharToLong(kID[1], kID[2], kID[3], kID[4]));

 // initial program names
 StrCopy(programs[0].name, 'Init Preset 01');
 StrCopy(programs[1].name, 'Init Preset 02');
 StrCopy(programs[2].name, 'Init Preset 03');
 StrCopy(programs[3].name, 'Init Preset 04');
 StrCopy(programs[4].name, 'Init Preset 05');
 StrCopy(programs[5].name, 'Init Preset 06');
 StrCopy(programs[6].name, 'Init Preset 07');
 StrCopy(programs[7].name, 'Init Preset 08');
 StrCopy(programs[8].name, 'Init Preset 09');
 StrCopy(programs[9].name, 'Init Preset 10');
 StrCopy(programs[10].name, 'Init Preset 11');
 StrCopy(programs[11].name, 'Init Preset 12');
 StrCopy(programs[12].name, 'Init Preset 13');
 StrCopy(programs[13].name, 'Init Preset 14');
 StrCopy(programs[14].name, 'Init Preset 15');
 StrCopy(programs[15].name, 'Init Preset 16');
 curprogram:=0;
 setProgram(0);
end;

destructor APlugin.Destroy;
var i:integer;
begin
 inherited Destroy;

 // destroy the created programs
 for i:=0 to kNumPrograms-1 do
 begin
  programs[i].Free;
  programs[i]:=nil;
 end;
end;

procedure APlugin.setProgram(aProgram: Longint);
begin
 if (aprogram<0)or(aprogram>knumprograms-1) then exit;

 curProgram := aProgram;
 // all parameters have to be set here for a program change
 SetParameter(kThresh,programs[curprogram].fThresh);
 SetParameter(kLength,programs[curprogram].fLength);
 SetParameter(kDir,programs[curprogram].fDir);
 SetParameter(kWet,programs[curprogram].fWet);
 SetParameter(kDelayPan,programs[curprogram].fDelayPan);
 SetParameter(kDelayLen,programs[curprogram].fDelayLen);
 SetParameter(kDelayFB,programs[curprogram].fDelayFB);
 SetParameter(kCutoff,programs[curprogram].fCutoff);
 SetParameter(kResonance,programs[curprogram].fResonance);
 SetParameter(kFType,programs[curprogram].fFType);
 SetParameter(kOutVol,programs[curprogram].fOutVol);
 if done then exit;

 if assigned(editor) then editor.update;
end;

procedure APlugin.setProgramName(name:PChar);
begin
 // nothing to change here
 StrCopy(programs[curProgram].name, name);
 if done then exit;
 if assigned(editor) then editor.update;
end;

procedure APlugin.getProgramName(name: PChar);
begin
 StrCopy(name, programs[curProgram].name);
end;

procedure APlugin.suspend;
begin
 // buffers are normally cleared/initialized here
 fillchar(buffer,sizeof(buffer),0);
 buffer_cnt:=0;
end;

function APlugin.getVu:Single;
var cvu:Single;
begin
 cvu:=vu;
 vu:=0;
 Result:=cvu;
end;

procedure APlugin.setParameter(index:Longint;value:Single);
begin
 // value HAS to be between 0 and 1 !!!
 if (value>1) then value:=1;
 if (value<0) then value:=0;

 case index of
  kLength:begin fLength:=value;programs[curprogram].fLength:=value end;
  kThresh:begin fThresh:=value;programs[curprogram].fThresh:=value end;
  kDir:begin fDir:=value;programs[curprogram].fDir:=value end;
  kWet:begin fWet:=value;programs[curprogram].fWet:=value end;
  kDelayPan:begin fDelayPan:=value;programs[curprogram].fDelayPan:=value end;
  kDelayLen:begin fDelayLen:=value;programs[curprogram].fDelayLen:=value end;
  kDelayFB:begin fDelayFB:=value;programs[curprogram].fDelayFB:=value end;
  kCutoff:begin
   Cut:=value;
   if value>0.99 then Cut:=0.99 else if value<0.01 then Cut:=0.01;
   fCutoff:=value;programs[curprogram].fCutoff:=value;
  end;
  kResonance:begin
   Reso:=value;if value>0.98 then Reso:=0.98;
   fResonance:=value;programs[curprogram].fResonance:=value;
  end;
  kFType:begin fFType:=value;programs[curprogram].fFType:=value end;
  kOutVol:begin fOutVol:=value;programs[curprogram].fOutVol:=value end;
 end;
 if assigned(editor) then editor.update;
end;

function APlugin.getParameter(index: Longint): Single;
var j:single;
begin
 case index of
  kWet:j:=fWet;
  kDelayPan:j:=fDelayPan;
  kDelayLen:j:=fDelayLen;
  kDelayFB:j:=fDelayFB;
  kCutoff:j:=fCutoff;
  kResonance:j:=fResonance;
  kFType:j:=fFType;
  kOutVol:j:=fOutVol;
  kLength:j:=fLength;
  kThresh:j:=fThresh;
  kDir:j:=fDir;
 else
  j:=0;
 end;
 Result:=j;
end;

function APlugin.DoFilter(i:single;cutoff,res:single):Single;
var fb:single;
begin
 fb:=res+res/(1-cutoff);
 old1:=old1+cutoff*(i-old1+fb*(old1-old2));
 old2:=old2+cutoff*(old1-old2);
 if fFType<0.5 then
  DoFilter:=old2  //return lowpass filtered signal
 else
  DoFilter:=i-old2 // return highpass filtered signal
end;

procedure APlugin.DoProcess(var i1:single;var i2:single);
var i,j,k:single;
begin
 inc(buffer_cnt);
 if buffer_cnt>round(fDelayLen*44100) then buffer_cnt:=0;

 i:=(i1+i2)*0.5; // mix left and right into one channel

 if fDir=0 then
 begin
  if abs(i)<=fThresh then inc(tcnt) else tcnt:=0;
  if tcnt>round(fLength*100) then fWWet:=fWet else fWWet:=0;
 end else
 begin
  if abs(i)>=fThresh then inc(tcnt) else tcnt:=0;
  if tcnt>round(fLength*100) then fWWet:=fWet else fWWet:=0;
 end;

 j:=buffer[buffer_cnt];
 k:=fWWet*DoFilter(j,Cut,Reso);

 buffer[buffer_cnt]:=i+fDelayFB*buffer[buffer_cnt];


 i1:=fOutVol*((1-fWWet)*i1+k*fDelayPan);
 i2:=fOutVol*((1-fWWet)*i2+k*(1-fDelayPan));

 if i1>1 then i1:=1 else if i1<-1 then i1:=-1; // check for clipping
 if i2>1 then i2:=1 else if i2<-1 then i2:=-1;
end;


procedure APlugin.process(inputs,outputs:PPSingle;sampleframes:Longint);
var In1,In2,Out1,Out2:PSingle;
    i1,i2:Single;
    i:Integer;
begin
 In1:=inputs^;
 In2:=PPSingle(Longint(inputs)+SizeOf(PSingle))^;
 Out1:=outputs^;
 Out2:=PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 for i:=0 to sampleFrames-1 do
 begin
  i1:=In1^;
  i2:=In2^;
  DoProcess(i1,i2);
  Out1^:=Out1^+i1;
  Out2^:=Out2^+i2;
  Inc(In1);
  Inc(In2);
  Inc(Out1);
  Inc(Out2);
 end;
end;

procedure APlugin.processReplacing(inputs, outputs: PPSingle; sampleframes: Longint);
var In1,In2,Out1,Out2:PSingle;
    i1,i2:Single;
    i:Integer;
begin
 In1:=inputs^;
 In2:=PPSingle(Longint(inputs)+SizeOf(PSingle))^;
 Out1:=outputs^;
 Out2:=PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 for i:=0 to sampleFrames-1 do
 begin
  i1:=In1^;
  i2:=In2^;
  DoProcess(i1,i2);
  Out1^:=i1;
  Out2^:=i2;
  Inc(In1);
  Inc(In2);
  Inc(Out1);
  Inc(Out2);
 end;
end;

function APlugin.GetPluginEditor:APluginEditor;
begin
 Result:=(editor as APluginEditor);
end;

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

function APluginEditor.getRect(var rect:PERect):Longint;
begin
 r.top:=0;
 r.left:=0;
 r.bottom:=230;
 r.right:=460;
 rect:=@r;
 Result := 1;
end;

function APluginEditor.open(ptr:Pointer):Longint;
begin
 systemWindow := HWnd(ptr);
 Inc(useCount);
 if (useCount=1)or (not assigned(editor)) then
 begin
  Editor:=TPluginEditorWindow.CreateParented(systemWindow);
  Editor.SetBounds(0,0,Editor.Width,Editor.Height);
  Editor.fWet:=Plugin.fWet;
  Editor.fDir:=Plugin.fDir;
  Editor.fLength:=Plugin.fLength;
  Editor.fThresh:=Plugin.fThresh;
  Editor.fDelayPan:=Plugin.fDelayPan;
  Editor.fDelayLen:=Plugin.fDelayLen;
  Editor.fDelayFB:=Plugin.fDelayFB;
  Editor.fCutoff:=Plugin.fCutoff;
  Editor.fResonance:=Plugin.fResonance;
  Editor.fFType:=Plugin.fFType;
  Editor.fOutVol:=Plugin.fOutVol;
  Editor.Effect:=Self.Plugin;
 end;
 Plugin.done:=false;
 Editor.Update;
 Editor.Show;
 editor.UpdaterTimer(nil);
 Result:=1;
end;

procedure APluginEditor.close;
begin
 if assigned(editor) then editor.visible:=false;
 Dec(useCount);
 if useCount=0 then
 begin
  Plugin.done:=true;
  Editor.Free;
  Editor:=nil;
  systemWindow:=0;
 end;
end;

procedure APluginEditor.idle;
begin
 if (Plugin.done) or (not Assigned(Editor)) then exit;
end;

procedure APluginEditor.update;
begin
 if (Plugin.done) or (not Assigned(Editor)) then exit;
 Editor.fDir:=Plugin.fDir;
 Editor.fLength:=Plugin.fLength;
 Editor.fThresh:=Plugin.fThresh;
 Editor.fWet:=Plugin.fWet;
 Editor.fDelayPan:=Plugin.fDelayPan;
 Editor.fDelayLen:=Plugin.fDelayLen;
 Editor.fDelayFB:=Plugin.fDelayFB;
 Editor.fCutoff:=Plugin.fCutoff;
 Editor.fResonance:=Plugin.fResonance;
 Editor.fFType:=Plugin.fFType;
 Editor.fOutVol:=Plugin.fOutVol;
end;

function APluginEditor.GetPlugin: APlugin;
begin
 Result:=(effect as APlugin);
end;

function APlugin.getEffectName(name:pchar): boolean;
begin
 StrCopy(name,kEffectName);
 Result:=TRUE;
end;

function APlugin.getVendorString(text:pchar):boolean;
begin
 StrCopy(text, kVendor);
 Result:=TRUE;
end;

function APlugin.getProductString(text:pchar):boolean;
begin
 StrCopy(text,kProduct);
 Result:=TRUE;
end;

function APlugin.getVendorVersion:longint;
begin
 Result:=1; // return version number
end;

function APlugin.getOutputProperties(index:longint;properties:PVstPinProperties):boolean;
begin
 Result:=false;
 if (index<kNumOutputs) then
 begin
  StrCopy(properties^.vLabel, pchar(Format(kChannelID+' %d', [index+1])));
  properties^.flags:=kVstPinIsActive;
  if (index<2) then
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
 case index of
  kDir:StrCopy(text, 'direction');
  kLength:StrCopy(text, 'tresh.length');
  kThresh:StrCopy(text, 'threshold');
  kWet:StrCopy(text, 'wet');
  kDelayPan:StrCopy(text, 'delay-pan');
  kDelayLen:StrCopy(text, 'delay-length');
  kDelayFB:StrCopy(text, 'delay-fb');
  kCutoff:StrCopy(text, 'filter-cutoff');
  kResonance:StrCopy(text, 'filter-reso');
  kfType:StrCopy(text, 'filter-type');
  kOutVol:StrCopy(text, 'outvol');
  else StrCopy(text, 'reserved');
  end;
end;

procedure APlugin.getParameterDisplay(index:longint;text:pchar);
begin
 case index of
  kDir:float2string(fDir, text);
  kThresh:float2string(fThresh, text);
  kLength:float2string(fLength, text);
  kWet:float2string(fWet, text);
  kDelayPan:float2string(fDelayPan, text);
  kDelayLen:float2string(fDelayLen, text);
  kDelayFB:float2string(fDelayFB, text);
  kCutoff:float2string(fCutoff, text);
  kResonance:float2string(fResonance, text);
  kFType:float2string(fFType, text);
  kOutVol:float2string(fOutVol, text);
  else float2string(0, text);
 end;
end;

procedure APlugin.getParameterLabel(index:longint;text:pchar);
begin
 StrCopy(text, '%');
end;

function APlugin.processEvents(ev:PVstEvents):longint;
var k,i{,status}:longint;
    event:PVstMidiEvent;
    midiData:array[0..3] of byte;
begin
 for i:=0 to ev^.numEvents-1 do
 if (ev.events[i].vtype=kVstMidiType) then
 begin
  event:=PVstMidiEvent(ev^.events[i]);
  for k:=0 to 3 do midiData[k]:=event.midiData[k];
{  status:=midiData[0] AND $f0; // channel information is removed

  if (status=$90)and(mididata[2]>0) then // "note on" ?
  begin
   note:=(midiData[1] and $7F); // midi note
   nvol:=(midiData[2] and $7F)/127; // velocity
  end
  else if ((status=$90)and(mididata[2]=0))or(status=$80) then // "note off" ?
  begin
   note:=(midiData[1] and $7F); // midi note
  end
  else if (status=$B0) then // midi CC ?
  begin
   note:=event^.mididata[1]; // midi CC#
   nvol:=event^.mididata[2]/127; // CC data
   if assigned(editor) then editor.update;
  end
  else if (status=$E0) then // pitchbend ?
  begin
   nvol:=event^.mididata[1]/127; // pitchbend data
   if assigned(editor) then editor.update;
  end;}
 end;
 
 Result:=1;
end;

procedure APlugin.resume;
begin
 wantEvents(1); // important for all plugins that receive MIDI!
end;

end.

