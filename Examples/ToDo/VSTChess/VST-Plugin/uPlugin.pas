unit uPlugin;

interface
uses Windows, DVSTUtils, uEditor, DAEffect, DAEffectX, DAudioEffect, DAudioEffectX,dialogs,extctrls;

const
 kID = 'TBCH'; // unique plugin identifier, has to be different for every plugin
 kChannelID='CHSS'; // string displayed in the VSTi channel mixer
 kEffectName = 'ChessVST'; // effect name
 kProduct = 'ChessVST'; // product name
 kVendor = 'Tobybear'; // vendor name
 kIsSynth = false; //false=audio effect, true=synth
 kNumInputs = 2; // number of inputs
 kNumOutputs = 2; // number of outputs
 kCanMono = true; // can be fed with mono signals?
 kCanReplacing = true;

 kNumParams = 100;
 kNumPrograms = 8;


 type APluginEditor=class;
 APluginProgram=class
 private
  pars:array[0..100] of single;
  name:array[0..50] of char;
 public
  constructor Create;
 end;

 APlugin=class(AudioEffectX)
 private
     pars:array[0..100] of single;
     vu: Single;
     host1,host2:string;
     done:boolean;
  programs:array[0..kNumPrograms-1] of APluginProgram;
 public
  function GetPluginEditor:APluginEditor;
  constructor CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);virtual;
  destructor Destroy;override;
  procedure process(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure processReplacing(inputs,outputs:PPSingle;sampleframes:Longint);override;
  procedure setProgram(aProgram:Longint);override;
  function canDo(text:pchar):longint;override;
  procedure setProgramName(name:PChar);override;
  procedure getProgramName(name:PChar);override;
  procedure setParameter(index:Longint;value:Single);override;
  function getParameter(index:Longint):Single;override;
  function getVu:Single;override;
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

function APlugin.canDo(text:pchar):longint;
begin
 Result:=-1;
 if StrComp(text, 'receiveVstEvents') = 0 then Result := 1;
 if StrComp(text, 'receiveVstMidiEvent') = 0 then Result := 1;
end;

constructor APluginProgram.Create;
var s:string;
    i:integer;
begin
 inherited Create;
 s:='rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR';
 for i:=1 to 64 do pars[i]:=ord(s[i])/1000;
 pars[73]:=0; //white to move

 pars[65]:=0;
 pars[66]:=1;

 pars[67]:=2/10;
 pars[68]:=0;
 pars[69]:=0;
 pars[70]:=0;
 pars[71]:=0;
 pars[72]:=0;
 StrCopy(name,'Init');
end;

constructor APlugin.CreateAPlugin(audioMaster:TAudioMasterCallbackFunc);
var i:integer;
    t:array[0..50] of char;
begin
 inherited Create(audioMaster, kNumPrograms, kNumParams);
 for i:=kNumPrograms-1 downto 0 do programs[i] := APluginProgram.Create;
 editor:=APluginEditor.Create(Self);
 randomize;
 suspend;
 hasVu(false);
 setNumInputs(KNumInputs);
 setNumOutputs(KNumOutputs);
 canMono(KCanMono);
 canProcessReplacing(KCanReplacing);
 isSynth(KIsSynth);
 setUniqueID(FourCharToLong(kID[1], kID[2], kID[3], kID[4]));

 StrCopy(programs[0].name, 'Chessboard 1');
 StrCopy(programs[1].name, 'Chessboard 2');
 StrCopy(programs[2].name, 'Chessboard 3');
 StrCopy(programs[3].name, 'Chessboard 4');
 StrCopy(programs[4].name, 'Chessboard 5');
 StrCopy(programs[5].name, 'Chessboard 6');
 StrCopy(programs[6].name, 'Chessboard 7');
 StrCopy(programs[7].name, 'Chessboard 8');
 curprogram:=0;
 setProgram(0);

 gethostvendorstring(t);host1:=strpas(t);
 gethostproductstring(t);host2:=strpas(t);
end;

destructor APlugin.Destroy;
var i:integer;
begin
 inherited Destroy;
 for i:=kNumPrograms-1 downto 0 do
  programs[i].Free;
end;

procedure APlugin.setProgramName(name:PChar);
begin
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
end;

function APlugin.getVu:Single;
var cvu:Single;
begin
 cvu:=vu;
 vu:=0;
 Result:=cvu;
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
 r.bottom:=550;
 r.right:=430;
 rect:=@r;
 Result := 1;
end;

function APluginEditor.open(ptr:Pointer):Longint;
var i:integer;
    s:string;
begin
 systemWindow := HWnd(ptr);
 Inc(useCount);
 if (useCount=1)or (not assigned(editor)) then
 begin
  Editor:=TPluginEditorWindow.CreateParented(systemWindow);
  Editor.SetBounds(0,0,Editor.Width,Editor.Height);

  for i:=1 to 8 do editor.pars[i]:=plugin.pars[64+i];
  s:='';
  for i:=1 to 64 do s:=s+chr(round(plugin.pars[i]*1000));
  editor.ChessBrd1.position:=s;
  if plugin.pars[73]>0.5 then editor.chessbrd1.whitetomove:=false
  else editor.chessbrd1.whitetomove:=true;
  Editor.Effect:=Self.Plugin;

 with editor do
 with chessbrd1 do
 begin
 case round(pars[4]*10) of
 0:if not set11.checked then set11click(nil);
 1:if not set21.checked then set21click(nil);
 2:if not set31.checked then set31click(nil);
 3:if not set41.checked then set41click(nil);
 4:if not set51.checked then set51click(nil);
 end;

 if (pars[1]>0.5) then begin
 if not machine1.checked then machine1click(nil) end
 else begin if not human1.checked then human1click(nil) end;

 if pars[2]>0.5 then begin
 if not machine2.checked then machine2click(nil);end
 else begin if not human2.checked then human2click(nil) end;

 case round(10*pars[3]) of
 0:if not N11.checked then N11click(nil);
 1:if not N21.checked then N21click(nil);
 2:if not N31.checked then N31click(nil);
 3:if not N41.checked then N41click(nil);
 4:if not N51.checked then N51click(nil);
 5:if not N61.checked then N61click(nil);
 6:if not N71.checked then N71click(nil);
 7:if not N81.checked then N81click(nil);
 end;

 case round(pars[5]*10) of
 0:if not set12.checked then set12click(nil);
 1:if not set22.checked then set22click(nil);
 2:if not set32.checked then set32click(nil);
 3:if not set42.checked then set42click(nil);
 4:if not set52.checked then set52click(nil);
 5:if not set61.checked then set61click(nil);
 end;

 if pars[6]>0.5 then begin
 if not boardlines then boardlines:=true end
 else begin if boardlines then boardlines:=false;
 end;
 if pars[7]>0.5 then
 begin
  if not showcoordinates1.checked then
  begin
   showcoordinates1.checked:=true;
   showcoordinates1click(nil);
  end;
 end
 else
 begin
  if showcoordinates1.checked then
  begin
   showcoordinates1.checked:=false;
   showcoordinates1click(nil);
  end;
 end;
 if (pars[8]>0.5) then
 begin if not whiteontop then whiteontop:=true end
 else begin if whiteontop then whiteontop:=false; end;
 end;

 end;
 Plugin.done:=false;
 editor.updater.enabled:=true;
 Editor.Update;
 Editor.Show;
 Result:=1;
end;

procedure APluginEditor.close;
begin
 if assigned(editor) then
 begin
  editor.visible:=false;
  editor.chessbrd1.CancelThinking;
 end;
 Dec(useCount);
 if usecount<0 then usecount:=0;
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
var i:integer;
begin
 if (Plugin.done) or (not Assigned(Editor)) then exit;
 for i:=1 to 8 do editor.pars[i]:=plugin.pars[64+i];
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
 Result:=1; 
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

procedure APlugin.setProgram(aProgram: longint);
var i:integer;
    s:string;
begin
  curProgram := aProgram;
  for i:=1 to 100 do
   setParameter(i, programs[curProgram].pars[i]);

  if assigned(editor) then
   if assigned(plugineditor) then
   if assigned(plugineditor.editor) then
   begin
    s:='';
    for i:=1 to 64 do
     s:=s+chr(round(pars[i]*1000));
    plugineditor.editor.ChessBrd1.position:=s;
    if pars[73]>0.5 then plugineditor.editor.chessbrd1.whitetomove:=false
    else plugineditor.editor.chessbrd1.whitetomove:=true;
  end;
  if done then exit;
  if assigned(editor) then editor.update;
end;

procedure APlugin.setParameter(index: longint; value: Single);
begin
 if (value>1) then value:=1;
 if (value<0) then value:=0;
 if (index>=1)and(index<=100) then
 begin
  pars[index]:=value;
  programs[curProgram].pars[index]:=value;
 end;
 if done then exit;
 if assigned(editor) then editor.update;
end;

function APlugin.getParameter(index: longint): Single;
begin
  Result := 0;
  if index>=1 then result:=pars[index];
end;

procedure APlugin.getParameterName(index: longint; text: pchar);
begin
 StrCopy(text, 'No');
end;

procedure APlugin.getParameterDisplay(index: longint; text: pchar);
begin
 float2string(0, text);
end;

procedure APlugin.getParameterLabel(index: longint; text: pchar);
begin
 StrCopy(text, '0');
end;

procedure APlugin.processReplacing(inputs, outputs: PPSingle; sampleframes: longint);
var In1,In2,Out1, Out2: PSingle;
    i:longint;
begin
 In1 := inputs^;
 In2 := PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 Out1 := outputs^;
 Out2 := PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 for i:=sampleFrames-1 downto 0 do
 begin
  Out1^:=In1^;
  Out2^:=In2^;
  Inc(In1);
  Inc(In2);
  Inc(Out1);
  Inc(Out2);
 end;
end;

procedure APlugin.process(inputs, outputs: PPSingle; sampleframes: longint);
var In1,In2,Out1, Out2: PSingle;
    i:longint;
begin
 In1 := inputs^;
 In2 := PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 Out1:= outputs^;
 Out2 := PPSingle(Longint(outputs)+SizeOf(PSingle))^;
 for i:=sampleFrames-1 downto 0 do
 begin
  Out1^:=Out1^+In1^;
  Out2^:=Out2^+In2^;
  Inc(In1);
  Inc(In2);
  Inc(Out1);
  Inc(Out2);
 end;
end;


end.


