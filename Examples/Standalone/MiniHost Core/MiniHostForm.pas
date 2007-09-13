unit MiniHostForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Types,
  Forms, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  DDspBase, DVstEffect, WaveIOX, MIDIFile, MIDI,
  ExtCtrls, ComCtrls, Menus, DVSTHost, DASIOHost;

type
  shortstr = string[255];
  pshortstr = ^shortstr;

const
  appversion = '1.0';
  appname = 'MiniHost Core';
    
type
  TWavPlayer = class
    pbuf        : Pointer;
    pf          : PSingle;
    looped,
    interpolate : Boolean;
    cnt2, size,
    sr, ch      : Integer;
    speed,
    vol, pan,
    samplerate  : Single;
    cnt         : Double;
    pmode       : Integer;
    filename    : string;
    procedure Process(var o1, o2: single);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Load(s: string);
    procedure Unload;
    constructor Create;
    destructor Destroy; override;
    procedure SetPos(i: Integer);
  end;

  MIDIData = record
    d1, d2, d3: byte;
    pos: Integer;
  end;

  { TFmMiniHost }

  TFmMiniHost = class(TForm)
    VSTHost: TVSTHost;
    MainMenu1: TMainMenu;
    VST1: TMenuItem;
    ASIO1: TMenuItem;
    MIDI1: TMenuItem;
    Help1: TMenuItem;
    LoadPlugin1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    Panic1: TMenuItem;
    MIDIIn1: TMenuItem;
    MIDIOut1: TMenuItem;
    Timer1: TTimer;
    Preset1: TMenuItem;
    LoadPresetfxp1: TMenuItem;
    LoadBankfxb1: TMenuItem;
    SavePresetfxp1: TMenuItem;
    SaveBankfxb1: TMenuItem;
    N2: TMenuItem;
    Driver1: TMenuItem;
    ControlPanel1: TMenuItem;
    OutputChannel1: TMenuItem;
    RenamePreset1: TMenuItem;
    N6: TMenuItem;
    Settings1: TMenuItem;
    ASIOHost: TASIOHost;
    InputChannel1: TMenuItem;
    ClosePlugin1: TMenuItem;
    DownmixToStereo1: TMenuItem;
    N14: TMenuItem;
    MidiThru1: TMenuItem;
    showpr: TMenuItem;
    Alwaysontop1: TMenuItem;
    UseMouseWheel1: TMenuItem;
    Main1: TMenuItem;
    Exit1: TMenuItem;
    N15: TMenuItem;
    status: TPanel;
    prbox: TComboBox;
    Panel1: TPanel;
    bg: TImage;
    onoff: TImage;
    Image2: TImage;
    dropdown: TImage;
    quicksettings: TImage;
    quickmidplay: TImage;
    quickwavplay: TImage;
    quickwavrec: TImage;
    bord0: TImage;
    bord2: TImage;
    bord3: TImage;
    bord4: TImage;
    bord1: TImage;
    Shape1: TShape;
    Shape2: TShape;
    N16: TMenuItem;
    ShowMIDIWAVPlayerrecorderWindow1: TMenuItem;
    IdleTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VSTHostAudioMasterIdle(Sender: TObject);
    procedure VSTHostAudioMasterNeedIdle(Sender: TObject);
    procedure ASIOHostLatencyChanged(Sender: TObject);
    procedure ASIOHostUpdateSamplePos(Sender: TObject;
      SamplePosition: Int64);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure Panic1Click(Sender: TObject);
    procedure LoadPresetFXP1Click(Sender: TObject);
    procedure SavePresetFXP1Click(Sender: TObject);
    procedure LoadBankFXB1Click(Sender: TObject);
    procedure SaveBankFXB1Click(Sender: TObject);
    procedure ClosePlugin1Click(Sender: TObject);
    procedure LoadPlugin1Click(Sender: TObject);
    procedure WAVFile1Click(Sender: TObject);
    procedure StartPlayback2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StartPlayback1Click(Sender: TObject);
    procedure RecordWAVfile1Click(Sender: TObject);
    procedure StartRecording1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ControlPanel1Click(Sender: TObject);
    procedure N81Click(Sender: TObject);
    procedure RenamePreset1Click(Sender: TObject);
    procedure LoadMIDIFile1Click(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostDestroy(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure showprClick(Sender: TObject);
    procedure StopPlayback1Click(Sender: TObject);
    procedure StopPlayback2Click(Sender: TObject);
    procedure StopRecording1Click(Sender: TObject);
    procedure F1ASIOControlPanel1Click(Sender: TObject);
    procedure RenameF1Click(Sender: TObject);
    procedure F3PlayStopMIDI1Click(Sender: TObject);
    procedure F4PlayStopWAV1Click(Sender: TObject);
    procedure F5RecStopWAV1Click(Sender: TObject);
    procedure F11MIDIPanic1Click(Sender: TObject);
    procedure Alwaysontop1Click(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure onoffMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure prboxClick(Sender: TObject);
    procedure prboxKeyPress(Sender: TObject; var Key: Char);
    procedure wpClick(Sender: TObject);
    procedure MPClick(Sender: TObject);
    procedure WRClick(Sender: TObject);
    procedure prboxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dropdownMouseDown(Sender: TObject; Button: TMouseButton;      Shift: TShiftState; X, Y: Integer);
    procedure prboxChange(Sender: TObject);
    procedure quicksettingsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure quickmidplayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure quickwavplayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure quickwavrecMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure bord2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowMIDIWAVPlayerrecorderWindow1Click(Sender: TObject);
    procedure DownmixToStereo1Click(Sender: TObject);
    procedure MidiThru1Click(Sender: TObject);
    procedure UseMouseWheel1Click(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
  private
    downmix: boolean;
    totalframes: Integer;
    dir_plugin, dir_preset, dir_wave, dir_midi: string;
    loadprog: Integer;
    loaded, pluginloaded: boolean;
    WavBufL, InBufL, WavBufR, InBufR: TSingleDynArray;
    VSTBufIn, VSTBufOut: TArrayOfSingleDynArray;
    CurrentASIO: Integer;
    CurrentMIDIIn: Integer;
    CurrentMIDIOut: Integer;
    CurrentOutputChannel, CurrentInputChannel: Integer;
    fPanel: TPanel;
    title: string;
    maxtimeinv: double;
    mdatacnt: Integer;
    evproc: boolean;
    Oct: Integer;
    lastdir: string;
    NotePlaying: array[0..127] of boolean;
    procedure MyMidiEvent(event: PMidiEvent);
    procedure MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: byte);
    procedure ClosePlugin;
    procedure ASIOChange(Sender: TObject);
    procedure SetChannel(Sender: TObject);
    procedure SetChannelI(Sender: TObject);
    procedure MIDIInChange(Sender: TObject);
    procedure MIDIOutChange(Sender: TObject);
    procedure SetPreset(Sender: TObject);
    procedure PluginResize(Sender: TObject);
    procedure ProcessEvents(Sender: TObject; ev: PVstEvents);
    procedure ProcessNoteOnOff(ch, n, v: byte);
  public
    MIDIPlaying         : boolean;
    MyEvents            : TVstEvents;
    Processing          : boolean;
    recording           : Integer;
    mdown               : boolean;
    CurProg             : Integer;
    CurProgName         : string;
    FileLength          : Cardinal;
    srate               : Cardinal;
    WaveFile            : TWavPlayer;
    MidiFile            : TMidiFile;
    WavWriter           : TWavWriter;
    OverallVol          : single;
    VSTVol              : single;
    InputVol            : single;
    numin, numout       : Integer;
    vpp                 : array of TVstPinProperties;
    procedure AddMID(s: string);
    procedure AddWAV(s: string);
    procedure LoadWAV(fn: string);
    procedure NoteOn(ch, note, v: byte);
    procedure NoteOff(ch, note: byte);
    procedure StartAudio;
    procedure StopAudio;
    procedure BuildPresetList;
    procedure LoadPresets(Files: TStrings);
    procedure LoadPlugin(s: string; prog: Integer = 0);
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
  end;

var
  FmMiniHost : TFmMiniHost;
  ININame    : string;
  allowed   : boolean = false;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses Math, Inifiles, Dialogs, ShellAPI,
     OptionsForm, AboutForm, PlayerForm;

procedure TFmMiniHost.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
  tlist: TStringList;
  slist: TStrings;
  s: string;
  Settings: TIniFile;
  p: PVstMidiEvent;
  mi: Integer;
begin
 bg.picture.Bitmap.TransparentColor := $A8A8A8;
 bg.picture.Bitmap.Transparent := true;
 dropdown.picture.Bitmap.TransparentColor := $A8A8A8;
 dropdown.picture.Bitmap.Transparent := true;
 bord0.picture.Bitmap.TransparentColor := clblack;
 bord0.picture.Bitmap.Transparent := true;
 bord1.Picture.Assign(bord0.Picture);
 bord2.Picture.Assign(bord0.Picture);
 bord3.Picture.Assign(bord0.Picture);
 bord4.Picture.Assign(bord0.Picture);

 for i := 0 to 2047 do
  begin
   GetMem(MyEvents.Events[i], sizeof(TVSTMidiEvent));
   FillChar(MyEvents.Events[i]^, sizeof(TVSTMidiEvent), 0);
   p := PVstMidiEvent(MyEvents.events[i]);
   p^.EventType := etMidi;
   p^.byteSize := 24;
  end;

 wavefile := TWavPlayer.create;
 pluginloaded := false;
 loaded := false;

 Player := TPlayer.Create(self);

 fPanel := TPanel.Create(self);
 with fPanel do
  begin
   Parent := self;
   Left := 0;
   Top := status.height;
   Width := 700;
   Height := 1;
   Tag := -1;
   OnResize := PluginResize;
  end;

 About := TAbout.Create(self);
 Options := TOptions.Create(self);
 Options.Host := self;

 DragAcceptFiles(self.handle, true);
 
{$IFNDEF FPC}
 ININame := GetApplicationDirectory + ChangeFileExt(GetApplicationFilename, '.ini');
{$ENDIF}

 MidiFile := TMidiFile.create(nil);
 MidiFile.OnMidiEvent := MyMidiEvent;
 MidiFile.ManualCall := true;
 recording := 0;
 Oct  :=  4;
 for i := 0 to 127 do noteplaying[i] := false;

 Settings := TIniFile.Create(ININame);

 try
  m := TMenuItem.Create(self);
  m.RadioItem := true;
  m.tag := 0;
  m.Caption := 'None';
  m.OnClick := MIDIInChange;
  MIDIIn1.Add(m);
  for mi := 0 to MidiInput.Devices.Count - 1 do
  begin
   m := TMenuItem.Create(self);
   m.RadioItem := true;
   m.tag := mi + 1;
   m.Caption := MidiInput.Devices[mi];
   m.OnClick := MIDIInChange;
   MIDIIn1.Add(m);
  end;
 except
  MessageDlg('ERROR: A serious problem occured with MIDI-In drivers!', mtError, [mbOK], 0);
 end;

 try
  m := TMenuItem.Create(self);
  m.RadioItem := true;
  m.tag := 0;
  m.Caption := 'None';
  m.OnClick := MIDIOutChange;
  MIDIOut1.Add(m);
  for mi := 0 to MidiOutput.Devices.Count - 1 do
  begin
   m := TMenuItem.Create(self);
   m.RadioItem := true;
   m.tag := mi + 1;
   m.Caption := MidiOutput.Devices[mi];
   m.OnClick := MIDIOutChange;
   MIDIOut1.Add(m);
  end;
 except
  MessageDlg('ERROR: A serious problem occured with MIDI-Out drivers', mtError, [mbOK], 0);
 end;

 try
  slist := ASIOHost.DriverList;
 except
  slist := nil;
  MessageDlg('ASIO driver list could not be received! Application Terminated!', mtError, [mbOK], 0);
  Application.Terminate;
 end;

 if slist <> nil then
  for i := 0 to slist.Count - 1 do
  begin
   m := TMenuItem.Create(self);
   m.RadioItem := true;
   m.tag := i;
   m.Caption := slist.Strings[i];
   m.OnClick := ASIOChange;
   Driver1.Add(m);
  end;
 if slist.Count = 0 then
 begin
  MessageDlg('No ASIO Driver present! Application Terminated!', mtError, [mbOK], 0);
  Application.Terminate;
 end;

 MidiInput.OnMidiData := MidiData;

 i := Settings.ReadInteger('Audio', 'ASIO Driver', 0);
 if (i < 0) or (i >= slist.count) then i := 0;
 Driver1.Items[i].Checked := true;
 try
  ASIOChange(Driver1.Items[i]);
 except
 end;
 i := Settings.ReadInteger('Audio', 'Output Channel', 0);
 if (i < 0) or (i >= OutputChannel1.Count) or
  (OutputChannel1.Count = 0) then
 begin
 end else
 begin
  try
   OutputChannel1.Items[i].checked := true;
   OutputChannel1.Items[i].Click;
  except
  end;
 end;

 i := Settings.ReadInteger('Audio', 'Input Channel', 0);
 if (i < 0) or (i >= InputChannel1.Count) or
  (InputChannel1.Count = 0) then
 begin
 end else
 begin
  try
   InputChannel1.Items[i].checked := true;
   InputChannel1.Items[i].Click;
  except
  end;
 end;

 WaveFile.filename := Settings.ReadString('Audio', 'File', '');

 i := Settings.ReadInteger('Audio', 'Record Bits', 16);
 case i of
 16: Player.rformat.ItemIndex := 0;
 else Player.rformat.ItemIndex := 1;
 end;

 showpr.Checked := Settings.ReadBool('Layout', 'ShowPresetInTitleBar', true);
 dir_plugin := Settings.ReadString('General', 'Plugin Directory', '');
 dir_preset := Settings.ReadString('General', 'Preset Directory', '');
 dir_wave := Settings.ReadString('General', 'Wave Directory', '');
 dir_midi := Settings.ReadString('General', 'Midi Directory', '');

 Player.midibox.Clear;
 tlist := TStringList.Create;
 Settings.ReadSection('Playlist MIDI', tlist);
 for i := 0 to tlist.Count - 1 do AddMID(tlist[i]);
 Player.WavBox.Clear;
 Settings.ReadSection('Playlist WAV', tlist);
 for i := 0 to tlist.Count - 1 do AddWAV(tlist[i]);
 tlist.Free;

 Player.mode1.itemindex := Settings.ReadInteger('MIDI', 'LoopMode', 1);
 Player.mode2.itemindex := Settings.ReadInteger('Audio', 'LoopMode', 1);
 WaveFile.looped := player.mode2.itemindex = 1;
 MidiFile.Filename := Settings.ReadString('MIDI', 'LastFile', '');
 if (MidiFile.filename <> '') and fileexists(MidiFile.filename)
  and (uppercase(extractfileext(MidiFile.filename))='.MID') then MidiFile.ReadFile;
 MidiPlaying := false;

 DownmixToStereo1.checked := Settings.ReadBool('VST', 'DownmixStereo', false);
 MIDIThru1.checked := Settings.ReadBool('VST', 'MIDIThru', false);

 s := Settings.ReadString('VST', 'LastPlugin', '');
 i := Settings.ReadInteger('VST', 'LastProgram', 0);
 Settings.Free;

 LoadWAV(WaveFile.filename);

 if (ParamCount > 0) and (FileExists(Paramstr(1))) then
 begin
  LoadPlugin(Paramstr(1));
  VSTHost.VSTPlugIns[0].OnProcessEvents := ProcessEvents;
 end else if FileExists(s) then
 begin
  loadprog := i;
  LoadPlugin(s, i);
  VSTHost.VSTPlugIns[0].OnProcessEvents := ProcessEvents;
 end;
end;

procedure TFmMiniHost.FormDestroy(Sender: TObject);
var i: Integer;
    Settings: TIniFile;
begin
 Processing := false;
 try
 
 allowed := false;
 try
  StopAudio;
 except
 end;

 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 try
 Settings := TIniFile.Create(ININame);
 Settings.EraseSection('Playlist MIDI');
 Settings.EraseSection('Playlist WAV');
 for i := 0 to Player.midibox.Items.Count - 1 do
  Settings.WriteString('Playlist MIDI', PShortStr(Player.midibox.Items.Objects[i])^, '');
 for i := 0 to Player.WavBox.Items.Count - 1 do
  Settings.WriteString('Playlist WAV', PShortStr(Player.WavBox.Items.Objects[i])^, '');
 Settings.WriteInteger('General', 'Timer', Timer1.Interval);
 Settings.WriteInteger('Layout', 'MainWindow X', Left);
 Settings.WriteInteger('Layout', 'MainWindow Y', Top);
 Settings.WriteInteger('Layout', 'SettingsWindow X', Options.Left);
 Settings.WriteInteger('Layout', 'SettingsWindow Y', Options.Top);
 Settings.WriteBool('Layout', 'SettingsWindow Visible', Options.Showing);
 Settings.WriteString('General', 'Plugin Directory', dir_plugin);
 Settings.WriteString('General', 'Preset Directory', dir_preset);
 Settings.WriteString('General', 'Wave Directory', dir_wave);
 Settings.WriteString('General', 'Midi Directory', dir_midi);
 Settings.WriteInteger('Layout', 'PlayerWindow X', Player.Left);
 Settings.WriteInteger('Layout', 'PlayerWindow Y', Player.Top);
 Settings.WriteBool('Layout', 'PlayerWindow Visible', Player.Showing);
 Settings.WriteBool('VST', 'DownmixStereo', DownmixToStereo1.checked);
 Settings.WriteBool('VST', 'MIDIThru', MIDIThru1.checked);
 Settings.WriteBool('VST', 'UseMouseWheel', UseMouseWheel1.Checked);
 Settings.WriteBool('MIDI', 'MidiFileOnlyChannel1', Player.onlych1.checked);
 Settings.WriteInteger('Audio', 'ASIO Driver', CurrentASIO);
 Settings.WriteInteger('Audio', 'Output Channel', CurrentOutputChannel);
 Settings.WriteInteger('Audio', 'Input Channel', CurrentInputChannel);
 if player.WavBox.Items.Count = 0 then WAVEFile.Filename := '';//c
 Settings.WriteString('Audio', 'File', WAVEFile.filename);
 Settings.WriteInteger('Audio', 'VST Volume', options.Scrollbar6.position);
 Settings.WriteInteger('Audio', 'Overall Volume', options.Scrollbar2.position);
 Settings.WriteInteger('Audio', 'Input Volume', options.Scrollbar5.position);
 Settings.WriteInteger('Audio', 'WAV Volume', options.Scrollbar1.position);
 Settings.WriteInteger('VST', 'Tempo', options.Scrollbar3.position);
 Settings.WriteString('VST', 'LastPlugin', VSTHost.VSTPlugIns[0].DLLFilename);
 Settings.WriteInteger('VST', 'LastProgram', CurProg);
 if player.midibox.Items.Count = 0 then MidiFile.Filename := '';//c
 Settings.WriteString('MIDI', 'LastFile', MidiFile.Filename);
 Settings.WriteInteger('MIDI', 'LoopMode', Player.mode1.itemindex);
 Settings.WriteInteger('Audio', 'LoopMode', player.mode2.itemindex);
 Settings.WriteBool('Layout', 'ShowPresetInTitleBar', showpr.Checked);
 Settings.WriteInteger('MIDI', 'MIDI-In Driver', CurrentMidiIn);
 Settings.WriteInteger('MIDI', 'MIDI-Out Driver', CurrentMidiOut);
 case Player.rformat.ItemIndex of
 0: i := 16;
 else i := 32;
 end;
 Settings.WriteInteger('Audio', 'Record Bits', i);
 Settings.Free;
 except
 end;
 
 if pluginloaded then ClosePlugin;

 try
  MidiInput.CloseAll;
 except
 end;
 try
  MidiOutput.CloseAll;
 except
 end;
 MidiFile.Free;

 wavefile.Free;
 numin := 0;
 numout := 0;
 for i := 0 to length(VSTBufOut) - 1 do SetLength(VSTBufOut[i], 0);
 for i := 0 to length(VSTBufIn) - 1 do SetLength(VSTBufIn[i], 0);
 SetLength(VSTBufOut, 0);
 SetLength(VSTBufIn, 0);
 SetLength(vpp, 0);
 for i := 0 to 2047 do FreeMem(MyEvents.Events[i]);

 finally
  AsioHost.free;
  AsioHost := nil;
 end;
end;

procedure TFmMiniHost.StartAudio;
var i: Integer;
begin
 if ASIOHost.Active then exit;
 ASIOHost.Active := False;
 VSTHost.BlockSize := ASIOHost.BufferSize;
 for i := 0 to VSTHost.VSTPlugIns.Count - 1 do
 begin
  VSTHost.VSTPlugIns[i].CanDo('sendVstTimeInfo');
  VSTHost.VSTPlugIns[i].CanDo('receiveVstTimeInfo');
 end;
 VSTHost.VstTimeInfo.Flags := VSTHost.VstTimeInfo.Flags + [vtiTransportPlaying];
 ASIOHost.Active := True;
end;

procedure TFmMiniHost.StopAudio;
begin
 if assigned(VSTHost) then
  VSTHost.VstTimeInfo.Flags := VSTHost.VstTimeInfo.Flags - [vtiTransportPlaying];
 if not ASIOHost.Active then exit;
 ASIOHost.Active := False;
end;

procedure TFmMiniHost.LoadWAV(fn: string);
begin
 WaveFile.unload;
 if FileExists(fn) then
 begin
  WaveFile.load(fn);
  filelength := WaveFile.size;
  srate := WaveFile.sr;
  WaveFile.samplerate := ASIOHost.SampleRate;
 end;
end;

procedure TFmMiniHost.ClosePlugin;
var i: Integer;
begin
 MidiFile.StopPlaying;
 MidiPlaying := false;
 timer1.enabled := false;
 Processing := false;
 
 StopAudio;
 panic1click(nil);
 recording := 0;

 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 if (VSTHost.VSTPlugIns[0].DLLFileName <> '') then
 begin
  VSTHost.VSTPlugIns[0].CloseEdit;
  VSTHost.VSTPlugIns[0].Close;
  VSTHost.VSTPlugIns[0].Unload;
  VSTHost.VSTPlugIns[0].DLLFileName := '';
 end;

 numin := 0;
 numout := 0;
 for i := 0 to length(VSTBufOut) - 1 do SetLength(VSTBufOut[i], 0);
 for i := 0 to length(VSTBufIn) - 1 do SetLength(VSTBufIn[i], 0);
 SetLength(VSTBufOut, 0);
 SetLength(VSTBufIn, 0);
 SetLength(vpp, 0);

 LoadPresetFXP1.Enabled := false;
 SavePresetFXP1.Enabled := false;
 LoadBankFXB1.Enabled := false;
 SaveBankFXB1.Enabled := false;
 pluginloaded := false;
 prbox.clear;
 timer1.enabled := true;
end;

procedure TFmMiniHost.BuildPresetList;
var m: TMenuItem;
    n, i: Integer;
    p: Array[0..100] of char;
    s: string;
begin
 prbox.clear;
 n := VSTHost.VSTPlugIns[0].numPrograms;

 for i := 0 to n - 1 do
 begin
  VSTHost.VSTPlugIns[0].GetProgramNameIndexed(-1, i, p);

  m := TMenuItem.Create(self);
  m.Caption := StrPas(p);
  m.OnClick := SetPreset;
  m.tag := i;
{$IFNDEF FPC}
  if (i > 0) and (i mod 256 <> 0) and (i mod 32 = 0) then
   m.break := mbBarBreak;
{$ENDIF}
  s := inttostr(i);
  if i < 10 then s := '00' + s else
  if i < 100 then s := '0' + s;
  prbox.AddItem(s + ': ' + m.caption, nil);
 end;

 if n >= 0 then
  prbox.ItemIndex := curprog;
end;

procedure TFmMiniHost.LoadPlugin(s: string; prog: Integer = 0);
var r: ERect;
    i: Integer;
begin
 if not FileExists(s) then exit;
 timer1.enabled := false;
 Processing := false;
 StopAudio;
 sleep(10);
 ClosePlugin;
 sleep(10);
 LastDir := ExtractFilePath(s);

 if assigned(fPanel) and (fPanel.tag = -1) then
 begin
  fPanel.Free;
  fPanel := nil;
 end;

 fPanel := TPanel.Create(self);
 with fPanel do
  begin
   Parent := self;
   Left := 0;
   Top := 0;
   tag := 0;
   Width := 700;
   Height := 0;
   OnResize := PluginResize;
  end;
  
 VSTHost.BlockSize := ASIOHost.BufferSize;
 VSTHost.VSTPlugIns[0].DLLFilename := s;

 try
  VSTHost.VSTPlugIns[0].Active := true;
 except
{$IFNDEF FPC}
  msg(s + ' is not a valid VST plugin!');
{$ENDIF}
  VSTHost.VSTPlugIns[0].Active := false;
  VSTHost.VSTPlugIns[0].DLLFilename := '';
  exit;
 end;

 SetLength(VSTBufIn, max(VSTHost.VSTPlugIns[0].numInputs, 2));
 SetLength(VSTBufOut, max(VSTHost.VSTPlugIns[0].numOutputs, 2));
 for i := 0 to length(VSTBufOut) - 1 do
  SetLength(VSTBufOut[i], ASIOHost.BufferSize);
 for i := 0 to length(VSTBufIn) - 1 do
  SetLength(VSTBufIn[i], ASIOHost.BufferSize);
 numin := VSTHost.VSTPlugIns[0].numInputs;
 numout := VSTHost.VSTPlugIns[0].numOutputs;
 SetLength(vpp, numout);
 for i := 0 to numout - 1 do
  vpp[i] := VSTHost.VSTPlugIns[0].GetOutputProperties(i);

 VSTHost.VSTPlugIns[0].GUIForm := TForm(fPanel);
 VSTHost.VSTPlugIns[0].ShowEdit(TForm(fPanel));

 title := VSTHost.VSTPlugIns[0].GetVendorString + ' ' +
  VSTHost.VSTPlugIns[0].GetEffectName;
 BuildPresetList;
 r := VSTHost.VSTPlugIns[0].EditGetRect;
 fPanel.width := r.right - r.left;
 fPanel.height := r.bottom - r.top;
 fPanel.top := status.height;

 LoadPresetFXP1.Enabled := true;
 LoadBankFXB1.Enabled := true;
 SavePresetFXP1.Enabled := true;
 SaveBankFXB1.Enabled := true;
 Processing := true;
 StartAudio;
 timer1.enabled := true;
 renamepreset1.Enabled := VSTHost.VSTPlugIns[0].numPrograms >= 1;

 pluginloaded := true;
 allowed := true;
 caption := title;
 left := screen.Width div 2 - width div 2;
 top := screen.height div 2 - height div 2;
 VSTHost.VSTPlugIns[0].SetProgram(prog);
 Options.scrollbar3change(nil);
 sleep(100);
end;

procedure TFmMiniHost.VSTHostAudioMasterIdle(Sender: TObject);
begin
 (Sender As TVSTPlugin).Idle;
end;

procedure TFmMiniHost.VSTHostAudioMasterNeedIdle(Sender: TObject);
begin
 (Sender As TVSTPlugin).EditIdle;
end;

procedure TFmMiniHost.MIDIInChange(Sender: TObject);
begin
 MidiInput.OnMidiData := MidiData;
 (sender as TMenuItem).checked := true;
 try
  MidiInput.Close(CurrentMidiIn);
 except
 end;
 CurrentMidiIn := (sender as TMenuItem).tag;
 MidiIn1.Items[CurrentMidiIn].Checked := true;
 try
  if CurrentMidiIn > 0 then
   MidiInput.Open(CurrentMidiIn - 1);
 except
 end;
end;

procedure TFmMiniHost.MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: byte);
begin
 if aStatus = $FE then exit; // ignore active sensing
 if (not Player.onlych1.checked) or ((aStatus and $0F) = 0) then
 begin
  if (aStatus and $F0) = $90 then //ok
   NoteOn(aStatus, aData1, aData2)
  else if (aStatus and $F0) = $80 then
   NoteOff(aStatus, aData1)
  else
   AddMidiData(aStatus, aData1, aData2);
 end;
end;

procedure TFmMiniHost.ASIOHostLatencyChanged(Sender: TObject);
begin
 VSTHost.LatencyInput := ASIOHost.InputLatency;
 VSTHost.LatencyOutput := ASIOHost.OutputLatency;
end;

procedure TFmMiniHost.ASIOHostUpdateSamplePos(Sender: TObject;
  SamplePosition: Int64);
begin
 VSTHost.VstTimeInfo.SamplePos := SamplePosition;
end;

procedure TFmMiniHost.ASIOHostSampleRateChanged(Sender: TObject);
begin
 StopRecording1Click(nil);
 VSTHost.VSTPlugIns[0].SetSampleRate(ASIOHost.SampleRate);
 VSTHost.VstTimeInfo.SampleRate := ASIOHost.SampleRate;
 WaveFile.samplerate := ASIOHost.SampleRate;
 maxtimeinv := ASIOHost.samplerate / ASIOHost.BufferSize;
end;

procedure TFmMiniHost.SetChannel(Sender: TObject);
begin
 (Sender as TMenuItem).checked := true;
 CurrentOutputChannel := (Sender as TMenuItem).tag;
 if ASIOHost.Active then
 begin
  StopAudio;
  ASIOHost.OutputChannelOffset := CurrentOutputChannel * 2;
  StartAudio;
 end else
 begin
  Processing := false;
  ASIOHost.OutputChannelOffset := CurrentOutputChannel * 2;
 end;
 Options.Label2.Caption := 'Outputs: ' + OutputChannel1.Items[CurrentOutputChannel].Caption;
end;

procedure TFmMiniHost.ASIOChange(Sender: TObject);
var i, j: Integer;
    m: TMenuItem;
begin
 (Sender as TMenuItem).checked := true;
 Processing := false;
 Panic1Click(nil);
 MidiFile.StopPlaying;
 MidiPlaying := false;
 StopPlayback2Click(nil);
 StopAudio;
 CurrentASIO := (Sender as TMenuItem).tag;
 if CurrentASIO >= 0 then
 begin
  ASIOHost.DriverIndex := CurrentASIO;
  for i := 0 to OutputChannel1.Count - 1 do OutputChannel1.Delete(0);
  for i := 0 to InputChannel1.Count - 1 do InputChannel1.Delete(0);
  j := 0;
  for i := 0 to length(ASIOHost.OutputChannelInfos) - 1 do
   if not odd(i) then
   begin
    m := TMenuItem.Create(self);
    m.RadioItem := true;
    m.tag := j;
    inc(j);
    m.OnClick := SetChannel;
    if i < length(ASIOHost.OutputChannelInfos) - 1 then
     m.Caption :=
      ASIOHost.OutputChannelInfos[i].name + ' / ' +
      ASIOHost.OutputChannelInfos[i + 1].name
    else
     m.Caption :=
      ASIOHost.OutputChannelInfos[i].name;
    OutputChannel1.Add(m);
   end;

  m := TMenuItem.Create(self);
  m.RadioItem := true;
  m.tag := 0;
  m.OnClick := SetChannelI;
  m.Caption := 'None';
  InputChannel1.Add(m);
  j := 1;
  for i := 0 to length(ASIOHost.InputChannelInfos) - 1 do
   if not odd(i) then
   begin
    m := TMenuItem.Create(self);
    m.RadioItem := true;
    m.tag := j;
    inc(j);
    m.OnClick := SetChannelI;
    if i < length(ASIOHost.InputChannelInfos) - 1 then
     m.Caption :=
      ASIOHost.InputChannelInfos[i].name
       + ' / ' + ASIOHost.InputChannelInfos[i + 1].name
    else
     m.Caption :=
      ASIOHost.InputChannelInfos[i].name;
    InputChannel1.Add(m);
   end;

  InputChannel1.Items[0].Click;
  OutputChannel1.Items[0].Click;
 end;
 Options.Label1.Caption := 'ASIO Driver: ' + ASIOHost.DriverName;
 if OutputChannel1.Count > 0 then
  Options.Label2.Caption := 'Outputs: ' + OutputChannel1.Items[0].Caption
 else
  Options.Label2.Caption := 'Outputs: None';
 if InputChannel1.Count > 0 then
  Options.Label8.Caption := 'Inputs: ' + InputChannel1.Items[0].Caption
 else
  Options.Label8.Caption := 'Inputs: None';
 if length(ASIOHost.OutputChannelInfos) > 0 then
  Options.Label9.Caption := 'Format: ' + inttostr(ASIOHost.OutputChannelInfos[0].vType) + ' ' + ChannelTypeToString(ASIOHost.OutputChannelInfos[0].vType)
 else
  Options.Label9.Caption := 'Format: None';
 Options.Label3.Caption := 'Buffersize: ' + inttostr(ASIOHost.BufferSize);
 Options.Label4.Caption := 'Samplerate: ' + inttostr(round(ASIOHost.SampleRate));

 ASIOHostReset(Sender);
 StartAudio;
 Processing := true;
end;

procedure TFmMiniHost.Panic1Click(Sender: TObject);
var ch, note: word;
begin
 mdatacnt := 0;
 for note := 0 to 127 do
 begin
  noteplaying[note] := false;
  AddMidiData($80, note, 0);
 end;
 for ch := 0 to 15 do AddMidiData($B0 + ch, 123, 0);
end;

procedure TFmMiniHost.LoadPresetFXP1Click(Sender: TObject);
var OD: TOpenDialog;
begin
 OD := TOpenDialog.Create(Self);
 try
 sleep(10);
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  Filename := '*.fxp';
  InitialDir := dir_preset;
  DefaultExt := '.fxp';
  Options := [ofAllowMultiSelect, ofFileMustExist,
   ofForceShowHidden];
  Ctl3D := false;
  Filter := 'preset files (*.fxp)|*.fxp';
  Title := 'Select a preset';
  if Execute then
  begin
   dir_preset := extractfiledir(filename);
   LoadPresets(Files);
  end;
 end;
 finally
 OD.Free;
 end;
end;

procedure TFmMiniHost.LoadPresets(Files: TStrings);
var i, j, k: Integer;
    s: string;
begin
 panic1click(nil);
 timer1.Enabled := false;
 j := CurProg;
 for i := 0 to Files.Count - 1 do
 begin
  if i > 0 then VSTHost.VSTPlugIns[0].SetProgram(j + i);
  try
   VSTHost.VSTPlugIns[0].LoadPreset(Files[i]);
  except
{$IFNDEF FPC}
   msg('Preset file not for this plugin (or file is corrupted)!');
{$ENDIF}
   timer1.Enabled := true;
   exit;
  end;
  k := VSTHost.VSTPlugIns[0].GetProgram;
  s := inttostr(k);
  if k < 10 then s := '00' + s else
  if k < 100 then s := '0' + s;
 end;
 timer1.Enabled := true;
end;

procedure TFmMiniHost.SavePresetFXP1Click(Sender: TObject);
var OD: TSaveDialog;
    s2: string;
begin
 panic1click(nil);
 OD := TSaveDialog.Create(Self);
 try
 sleep(10);
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  DefaultExt := '.fxp';
  filename := '*.fxp';
  Filter := 'preset files (*.fxp)|*.fxp';
  Title := 'Select a preset';
  InitialDir := dir_preset;
  Options := [ofForceShowHidden];
  Ctl3D := false;

  s2 := prbox.Items[prbox.ItemIndex];
  s2 := copy(s2, 6, length(s2) - 5);
{$IFNDEF FPC}
  Filename := MakeGoodFileName(s2) + '.fxp';
{$ENDIF}

  if Execute then
  begin
   VSTHost.VSTPlugIns[0].SavePreset(FileName);
   dir_preset := extractfiledir(filename);
  end;
 end;
 finally
 OD.Free;
 end;
end;

procedure TFmMiniHost.LoadBankFXB1Click(Sender: TObject);
var OD: TOpenDialog;
begin
 timer1.Enabled := false;
 OD := TOpenDialog.Create(Self);
 try
 sleep(10);
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  DefaultExt := '.fxb';
  filename := '*.fxb';
  Filter := 'bank files (*.fxb)|*.fxb';
  Title := 'Select a bank';
  InitialDir := dir_preset;

  Options := [ofFileMustExist, ofForceShowHidden];
  Ctl3D := false;

  if Execute then
  begin
   dir_preset := extractfiledir(od.filename);
   try
    VSTHost.VSTPlugIns[0].LoadBank(Filename);
   except
{$IFNDEF FPC}
    msg('Bank file not for this plugin (or file is corrupted)!');
{$ENDIF}
    timer1.Enabled := true;
   end;
   BuildPresetList;
  end;
 end;
 finally
  OD.Free;
  curprog := 0;
  VSTHost.VSTPlugIns[0].SetProgram(0);
  prbox.ItemIndex := 0;
  timer1.Enabled := true;
 end;
end;

procedure TFmMiniHost.SaveBankFXB1Click(Sender: TObject);
var OD: TSaveDialog;
begin
 OD := TSaveDialog.Create(Self);
 try
  sleep(10);
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  filename := '*.fxb';
  DefaultExt := '.fxb';
  Filter := 'bank files (*.fxb)|*.fxb';
  Title := 'Select a bank';
  InitialDir := dir_preset;
  Options := [ofForceShowHidden];
  Ctl3D := false;
  if Execute then
  begin
   dir_preset := extractfiledir(filename);
   VSTHost.VSTPlugIns[0].SaveBank(FileName);
  end;
 end;
 finally
  OD.Free;
 end;
end;

procedure TFmMiniHost.ClosePlugin1Click(Sender: TObject);
begin
 WaveFile.stop;
 ClosePlugin;
end;

procedure TFmMiniHost.LoadPlugin1Click(Sender: TObject);
var OD  : TOpenDialog;
begin
 OD  :=  TOpenDialog.Create(Self);
 sleep(10);
 with OD do
  begin
   Name := 'OD' + inttostr(random(25555));
   DefaultExt := '.dll';
   filename := '*.dll';
   Filter := 'VST Plugins (*.dll)|*.dll';
   Options := [ofFileMustExist, ofForceShowHidden];
   Ctl3D := false;
   Title := 'Select a VST plugin';
   initialdir := dir_plugin;
   if Execute then
   begin
    dir_plugin := extractfiledir(filename);
    LoadPlugin(FileName);
   end;
  end;
 OD.Free;
end;

procedure TFmMiniHost.WAVFile1Click(Sender: TObject);
var OD: TOpenDialog;
begin
 OD := TOpenDialog.Create(Self);
 try
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  DefaultExt := '.wav';
  filename := '*.wav;*.wpl';
  Filter := 'WAV files and playlists (*.wav;*.wpl)|*.wav;*.wpl|WAV files (*.wav)|*.wav|WAV playlists (*.wpl)|*.wpl';
  FilterIndex := 0;
  InitialDir := dir_wave;
  Options := [ofFileMustExist, ofForceShowHidden];
  Ctl3D := false;
  Title := 'Select a WAV file';
  if Execute then
  begin
   dir_wave := extractfiledir(filename);
   AddWAV(FileName);
  end;
 end;
 finally
 OD.Free;
 end;
end;

procedure TFmMiniHost.StartPlayback2Click(Sender: TObject);
begin
 if not fileexists(Wavefile.filename) then exit;
 WaveFile.play;
end;

procedure TFmMiniHost.SetPreset(Sender: TObject);
begin
 panic1click(nil);
 VSTHost.VSTPlugIns[0].SetProgram((sender as TMenuItem).tag);
end;

procedure TFmMiniHost.FormShow(Sender: TObject);
var i: Integer;
    Settings: TIniFile;
begin
 Settings := TIniFile.Create(ININame);
 UseMouseWheel1.checked := Settings.ReadBool('VST', 'UseMouseWheel', true);
 Player.onlych1.checked := Settings.ReadBool('MIDI', 'MidiFileOnlyChannel1', false);
 Options.scrollbar2.position := settings.ReadInteger('Audio', 'Overall Volume', 100);
 Options.scrollbar6.position := settings.ReadInteger('Audio', 'VST Volume', 100);
 Options.scrollbar5.position := settings.ReadInteger('Audio', 'Input Volume', 100);
 Options.scrollbar1.position := settings.ReadInteger('Audio', 'WAV Volume', 100);
 Options.scrollbar3.position := settings.ReadInteger('VST', 'Tempo', 120);

 Options.Left := Settings.ReadInteger('Layout', 'SettingsWindow X', Left - 100);
 Options.Top := Settings.ReadInteger('Layout', 'SettingsWindow Y', Top);
 if options.Left < 0 then options.Left := 0;
 if options.Top < 0 then options.Top := 0;
 if options.Left > screen.width - 20 then options.Left := screen.width - 20;
 if options.Top > screen.height - 20 then options.Top := screen.height - 20;

 Player.Left := Settings.ReadInteger('Layout', 'PlayerWindow X', Left - 100);
 Player.Top := Settings.ReadInteger('Layout', 'PlayerWindow Y', Top);
 if Player.Left < 0 then Player.Left := 0;
 if Player.Top < 0 then Player.Top := 0;
 if Player.Left > screen.width - 20 then Player.Left := screen.width - 20;
 if Player.Top > screen.height - 20 then Player.Top := screen.height - 20;
 Options.scrollbar3change(nil);
 if Settings.ReadBool('Layout', 'SettingsWindow Visible', false) then
 begin
  Options.Show;
  options.setfocus;
 end;
 if Settings.ReadBool('Layout', 'PlayerWindow Visible', false) then
 begin
  Player.Show;
  Player.SetFocus;
 end;

 alwaysontop1.checked := not Settings.ReadBool('Layout', 'AlwaysOnTop', false);
 alwaysontop1click(sender);
 i := Settings.ReadInteger('MIDI', 'MIDI-In Driver', 0);
 if (i < 0) or (i > MidiInput.Devices.Count) then i := 0;
 CurrentMidiIn := i;
 MIDIIn1.Items[i].Click;
 i := Settings.ReadInteger('MIDI', 'MIDI-Out Driver', 0);
 if (i < 0) or (i > MidiOutput.Devices.Count) then i := 0;
 CurrentMidiOut := i;
 MIDIOut1.Items[i].Click;
 loaded := true;
 Settings.Free;
 Timer1.Enabled := true;
 if loadprog >=0 then
 begin
  VSTHost.VSTPlugIns[0].SetProgram(loadprog);
  loadprog := -1;
 end;
 if status.visible then status.setfocus;
end;

procedure TFmMiniHost.PluginResize(Sender: TObject);
begin
 if not (effFlagsHasEditor in VSTHost.VSTPlugIns[0].EffectOptions) then
 begin
  fPanel.Width := 700;
  if (VSTHost.VSTPlugIns[0].DLLFileName = '')
   then fPanel.Height := 0
   else fPanel.Height := 90;
 end;
 if fPanel.width < 560 then
  begin
   ClientWidth := 560;
   fPanel.left := (560 - fPanel.width) div 2;
  end
 else
  begin
   ClientWidth := fPanel.width;
   fPanel.left := 0;
  end;
 ClientHeight := fPanel.height + status.height;
end;

procedure TFmMiniHost.MyMidiEvent(event: PMidiEvent);
begin
 if (event^.event and $F0) = $90 then
  NoteOn(event^.event, event^.data1, event^.data2)
 else if (event^.event and $F0) = $80 then 
  NoteOff(event^.event, event^.data1)
 else
  AddMidiData(event^.event, event^.data1, event^.data2);
end;

procedure TFmMiniHost.StartPlayback1Click(Sender: TObject);
begin
 Panic1Click(nil);
 MidiFile.StartPlaying;
 MidiPlaying := true;
end;

procedure TFmMiniHost.RecordWAVfile1Click(Sender: TObject);
var OD: TSaveDialog;
begin
 OD := TSaveDialog.Create(Self);
 try
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  DefaultExt := '.wav';
  InitialDir := dir_wave;
  filename := '*.wav';
  Filter := 'WAV files (*.wav)|*.wav';
  Title := 'Select a WAV file';
  Options := [ofForceShowHidden];
  Ctl3D := false;
  if Execute then
  begin
   dir_wave := extractfiledir(filename);
   Player.WavFile.caption := filename;
  end;
 end;
 finally
 OD.Free;
 end;
end;

procedure TFmMiniHost.StartRecording1Click(Sender: TObject);
var s: string;
    i: Integer;
begin
 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 case Player.rformat.ItemIndex of
 0: i := 16;
 else i := 32;
 end;
 s := Player.wavfile.caption;
 if s = '<none>' then
 begin
  recordwavfile1click(sender);
  s := Player.wavfile.caption;
  if (s = '<none>') or (s = '') then exit;
 end;
 totalframes := 0;
 if Player.recinmono.checked then
  WavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 1, i)
 else
  WavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 2, i);
 recording := 1;
end;

procedure TFmMiniHost.MIDIOutChange(Sender: TObject);
begin
 (sender as TMenuItem).checked := true;
 try
  MidiOutput.Close(CurrentMidiOut);
 except
 end;
 CurrentMidiOut := (sender as TMenuItem).tag;
 MidiOut1.Items[currentmidiOut].Checked := true;
 try
  if CurrentMidiOut > 0 then
   MidiOutput.Open(CurrentMidiOut - 1);
 except
 end;
end;

procedure TFmMiniHost.Timer1Timer(Sender: TObject);
var s2, s: string;
    i: Integer;
    e: single;
begin
 if wavefile.pmode > 0 then
 begin
  i := round(100 * wavefile.cnt2 / (wavefile.size-2));
  Player.label7.caption := 'position: ' + inttostr(i) +' %';
  Player.s_pos2.position := i;
 end;

 bord0.visible := Processing;
 bord1.visible := options.Showing;
 bord2.visible := midiplaying;
 bord3.visible := not (wavefile.pmode = 0);
 bord4.visible := (recording = 1);

 case recording of
 1: Player.rstatus.caption := 'status: recording';
 2: Player.rstatus.caption := 'status: paused';
 else Player.rstatus.caption := 'status: stopped';
 end;

 if recording > 0 then
 begin
  e := totalframes / ASIOHost.samplerate;
  Player.rstatus.caption :=
   player.rstatus.caption + ' (time: '
   + floattostrf(e, fffixed, 4, 2) + ' sec, size: ' + inttostr(
    round(e * wavwriter.Format.nAvgBytesPerSec / 1000))
   + ' kbytes)';
 end;

 downmix := DownmixToStereo1.Checked;

 if (MIDIPlaying) then
 begin
  i := round(100 * MidiFile.GetCurrentPos / MidiFile.GetTrackLength2);
  if i > 100 then i := 100 else if i < 0 then i := 0;
  Player.s_pos.position := i;

  if (MidiFile.Ready) then
  begin
   player.s_pos.position := 0;
   if Player.mode1.itemindex = 1 then
   begin
    Panic1Click(nil);
    MidiFile.StartPlaying;
   end else
   if (player.mode1.ItemIndex = 2) and (player.midibox.Items.Count > 0) then
   begin
    player.midibox.itemindex := (Player.midibox.itemindex + 1) mod player.midibox.Items.Count;
    player.Button4click(nil);
   end else
   if (player.mode1.ItemIndex = 3) and (player.midibox.Items.Count > 0) then
   begin
    player.midibox.itemindex := random(player.midibox.Items.Count);
    player.Button4click(nil);
   end else
    MIDIPlaying := false;
  end;
 end;

 if prbox.Items.Count = 0 then
 begin
  caption := 'Tobybear MiniHost (www.tobybear.de)';
  exit;
 end;

 s := VSTHost.VSTPlugIns[0].GetProgramName;
 i := VSTHost.VSTPlugIns[0].GetProgram;
 if (CurProg <> i) or (CurProgName <> s) then
  begin
   CurProg := i;
   CurProgName := s;
   s := inttostr(CurProg);
   if CurProg < 10 then s := '00' + s else
   if CurProg < 100 then s := '0' + s;
 if (prbox.items.Count > 0) and (CurProg>=0) then
  begin
   prbox.Items[CurProg] := s + ': ' + CurProgName;
   prbox.ItemIndex := i;
  end;
  s2 := title;
  if showpr.Checked then
   s2 := s2 + ' - ' + s + ': ' + CurProgName;
  if caption <> s2 then caption := s2;
 end;
end;

procedure TFmMiniHost.ControlPanel1Click(Sender: TObject);
begin
 StopAudio;
 ASIOHost.ControlPanel;
 ASIOHost.Reset;
 StartAudio;
end;

procedure TFmMiniHost.N81Click(Sender: TObject);
begin
 Panic1Click(nil);
 (Sender as TMenuItem).Checked := true;
 Oct := (Sender as TMenuItem).Tag;
end;

procedure TFmMiniHost.ProcessEvents(Sender: TObject;
 ev: PVstEvents);
var i: Integer;
    event: PVstMidiEvent;
    Sysex : PVstMidiSysexEvent;
    aStream: TMemoryStream;
begin
 if CurrentMidiOut = 0 then exit;
 for i := 0 to ev^.numEvents - 1 do
  if (ev.events[i].EventType = etMidi) then
   begin
    event := PVstMidiEvent(ev^.events[i]);
    MidiOutput.Send(CurrentMidiOut - 1, event^.mididata[0],
      event^.mididata[1], event^.mididata[2]);
   end else
  if ev.events[i].EventType = etSysex then
   begin
    Sysex := PVstMidiSysexEvent(ev^.events[i]);
    if Sysex.dumpBytes > 0 then
     begin
      AStream := TMemoryStream.Create;
      aStream.Size := Sysex.dumpBytes;
      aStream.Position := 0;
      Move(Sysex.SysexDump^, pchar(aStream.Memory)[0], Sysex.dumpBytes);
      MidiOutput.SendSysEx(CurrentMidiOut - 1,aStream);
      aStream.Free;
     end;
   end;
end;

procedure TFmMiniHost.wmdropfiles(var msg: tmessage);
var size: Integer;
    name: pchar;
    fn,s :string;
begin
 inherited;
 size := DragQueryFile(msg.wparam, 0, nil, 0) + 1;
 name := StrAlloc(size);
 DragQueryFile(msg.wparam, 0, name, size);
 s := StrPas(name);
 StrDispose(name);
 DragFinish(msg.wparam);

 fn := UpperCase(ExtractFileExt(s));
 if (fn = '.FXP') then
 begin
  try
   VSTHost.VSTPlugIns[0].LoadPreset(s);
  except
{$IFNDEF FPC}
   DDspBase.msg('Preset file not for this plugin (or file is corrupted)!');
{$ENDIF}
   exit;
  end;
 end else
 if (fn = '.FXB') then
 begin
  try
   VSTHost.VSTPlugIns[0].LoadBank(s);
  except
{$IFNDEF FPC}
   DDspBase.msg('Bank file not for this plugin (or file is corrupted)!');
{$ENDIF}
   exit;
  end;
 end else
  if (fn = '.DLL') then LoadPlugin(s)
 else
  if (fn = '.WAV') then
  begin
   AddWAV(s);
   LoadWAV(s);
  end else
   if (fn = '.MID') then AddMid(s);
end;

procedure TFmMiniHost.RenamePreset1Click(Sender: TObject);
var s2, s: string;
begin
 s := inputbox('Rename Preset', 'New name:', VSTHost.VSTPlugIns[0].GetProgramName);
 VSTHost.VSTPlugIns[0].SetProgramName(s);
 VSTHost.VSTPlugIns[0].Idle;
 VSTHost.VSTPlugIns[0].EditIdle;

 s2 := inttostr(CurProg);
 if CurProg < 10 then s2 := '00' + s2 else
 if CurProg < 100 then s2 := '0' + s2;

 prbox.Items[CurProg] := s2 + ': ' + s;
end;

procedure TFmMiniHost.LoadMIDIFile1Click(Sender: TObject);
var OD: TOpenDialog;
begin
 OD := TOpenDialog.Create(Self);
 try
 with OD do
 begin
  Name := 'OD' + inttostr(random(25555));
  DefaultExt := '.mid';
  InitialDir := dir_midi;
  Options := [ofFileMustExist, ofForceShowHidden];
  Ctl3D := false;
  //c
  filename := '*.mid;*.mpl';
  Filter := 'MIDI files and playlists (*.mid;*.mpl)|*.mid;*.mpl|MIDI files (*.mid)|*.mid|MIDI playlists (*.mpl)|*.mpl';
  FilterIndex := 0;
  Title := 'Select a MIDI file';
  if Execute then
  begin
   dir_midi := extractfiledir(filename);
   AddMID(filename);
  end;
 end;
 finally
 OD.Free;
 end;
end;

procedure TFmMiniHost.Settings1Click(Sender: TObject);
begin
 Options.Show;
end;

procedure TFmMiniHost.SetChannelI(Sender: TObject);
var f: boolean;
begin
 (Sender as TMenuItem).checked := true;
 CurrentInputChannel := (Sender as TMenuItem).tag;
 f := ASIOHost.Active;
 if CurrentInputChannel = 0 then
  ASIOHost.InputChannelOffset := 0
 else
  ASIOHost.InputChannelOffset := (CurrentInputChannel - 1) * 2;
 if f then StartAudio;
 Options.Label8.Caption := 'Inputs: ' + InputChannel1.Items[CurrentInputChannel].Caption;
end;

procedure TFmMiniHost.ASIOHostReset(Sender: TObject);
var i: Integer;
begin
 for i := 0 to length(VSTBufOut) - 1 do
  SetLength(VSTBufOut[i], ASIOHost.BufferSize);
 for i := 0 to length(VSTBufIn) - 1 do
  SetLength(VSTBufIn[i], ASIOHost.BufferSize);
 SetLength(InBufL, ASIOHost.BufferSize);
 SetLength(InBufR, ASIOHost.BufferSize);
 SetLength(WavBufL, ASIOHost.BufferSize);
 SetLength(WavBufR, ASIOHost.BufferSize);
 ASIOHostSampleRateChanged(Sender);
 maxtimeinv := ASIOHost.samplerate / ASIOHost.BufferSize;
end;

procedure TFmMiniHost.ASIOHostDestroy(Sender: TObject);
var i: Integer;
begin
 Processing := false;
 SetLength(InBufL, 0);
 SetLength(InBufR, 0);
 SetLength(WavBufL, 0);
 SetLength(WavBufR, 0);
 numin := 0;
 numout := 0;
 for i := 0 to length(VSTBufOut) - 1 do SetLength(VSTBufOut[i], 0);
 for i := 0 to length(VSTBufIn) - 1 do SetLength(VSTBufIn[i], 0);
 SetLength(VSTBufOut, 0);
 SetLength(VSTBufIn, 0);
 SetLength(vpp, 0);
end;

procedure TFmMiniHost.AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
begin
 if mdatacnt > 2046 then exit;
 inc(mdatacnt);
 PVstMidiEvent(MyEvents.events[mdatacnt - 1])^.deltaFrames := pos;
 PVstMidiEvent(MyEvents.events[mdatacnt - 1])^.midiData[0] := d1;
 PVstMidiEvent(MyEvents.events[mdatacnt - 1])^.midiData[1] := d2;
 PVstMidiEvent(MyEvents.events[mdatacnt - 1])^.midiData[2] := d3;
end;

procedure TFmMiniHost.NoteOn(ch, note, v: byte);
begin
 if v = 0 then
 begin
  ch := ch - $10;
  NoteOff(ch, note);
  exit;
 end;
 begin
  if (note <= 127) then
   ProcessNoteOnOff(ch, note, v);
 end;
end;

procedure TFmMiniHost.NoteOff(ch, note: byte);
begin
 if (note <= 127) then ProcessNoteOnOff(ch, note, 0);
end;

procedure TFmMiniHost.ProcessNoteOnOff(ch, n, v: byte);
begin
 if v = 0 then
 begin // Note Off
  if ch >= $90 then ch := ch - $10;
  AddMidiData(ch, n, 0);
 end else
 begin // Note On
  if ch < $90 then ch := ch + $10;
  AddMidiData(ch, n, v);
 end;
end;

procedure TFmMiniHost.About1Click(Sender: TObject);
begin
 about.showmodal;
end;

procedure TFmMiniHost.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Panic1Click(nil);

 allowed := false;
 WaveFile.stop;
 WaveFile.Unload;
 MidiFile.StopPlaying;
 MidiPlaying := false;

 recording := 0;
 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;
end;

procedure TFmMiniHost.showprClick(Sender: TObject);
var s: string;
begin
 showpr.Checked := not showpr.Checked;
 s := inttostr(CurProg);
 if CurProg < 10 then s := '00' + s else
 if CurProg < 100 then s := '0' + s;
 if showpr.Checked then
  caption := title + ' - ' + s + ': ' + CurProgName
 else
  caption := title;
end;

procedure TFmMiniHost.StopPlayback1Click(Sender: TObject);
begin
 MidiFile.StopPlaying;
 MidiPlaying := false;
 Panic1Click(nil);
end;

procedure TFmMiniHost.StopPlayback2Click(Sender: TObject);
begin
 WaveFile.stop;
end;

procedure TFmMiniHost.StopRecording1Click(Sender: TObject);
begin
 recording := 0;
 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;
end;

procedure TFmMiniHost.F1ASIOControlPanel1Click(Sender: TObject);
begin
 if Options.Showing then Options.Hide else Options.Show;
end;

procedure TFmMiniHost.RenameF1Click(Sender: TObject);
begin
 RenamePreset1Click(nil);
end;

procedure TFmMiniHost.F3PlayStopMIDI1Click(Sender: TObject);
begin
 with Player do
 if midibox.ItemIndex >= 0 then
  Player.label2.Caption := midibox.items[midibox.itemindex];
  
 if MidiPlaying then
 begin
  MidiFile.StopPlaying;
  MidiPlaying := false;
  Panic1Click(nil);
 end else
 begin
  Panic1Click(nil);
  MIDIFile.StartPlaying;
  MidiPlaying := true;
 end;
end;

procedure TFmMiniHost.F4PlayStopWAV1Click(Sender: TObject);
begin
 with Player do
 if WavBox.Items.Count > 0 then
 if WavBox.ItemIndex >= 0 then
 begin
  Player.label4.Caption := WavBox.items[WavBox.itemindex];
  if Wavefile.pmode = 1 then
   StopPlayback2Click(nil)
  else
   StartPlayback2Click(nil);
 end;
end;

procedure TFmMiniHost.F5RecStopWAV1Click(Sender: TObject);
begin
 if recording >= 1 then StopRecording1Click(nil)
 else if recording = 0 then StartRecording1Click(nil);
end;

procedure TFmMiniHost.F11MIDIPanic1Click(Sender: TObject);
begin
 Panic1Click(nil);
end;

{ TWavPlayer }

constructor TWavPlayer.create;
begin
 pbuf := nil;
 pf := nil;
 cnt := 0;
 cnt2 := 0;
 pmode := 0;
 vol := 1;
 pan := 0.5;
 speed := 1;
 interpolate := false;
end;

destructor TWavPlayer.destroy;
begin
 if assigned(pbuf) then freemem(pbuf);
 inherited;
end;

procedure TWavPlayer.load(s: string);
begin
 pmode:=0;
 if assigned(pbuf) then
 begin
  freemem(pbuf);
  pbuf := nil;
 end;
 filemode := 0;
 if s <> '' then
 begin
//  pbuf := LoadWAVFile(s,sr,ch,size);
 end;
 pf := pbuf;
 cnt := 0;
 cnt2 := 0;
 filename := s;
end;

procedure TWavPlayer.pause;
begin
 pmode := 0;
end;

procedure TWavPlayer.play;
begin
 pmode := 1;
end;

procedure TWavPlayer.process(var o1, o2: single);
var next, next2, pp: psingle;
begin
 if (not assigned(pf)) // if buffer is empty (no file loaded)
  or (pmode = 0)       // or "play" not activated
 then begin            // then output silence
  o1 := 0;
  o2 := 0;
 end else
 begin
  o1 := pf^;
  if ch = 2 then // stereo?
  begin
   pp := psingle(longint(pf) + 4);
   o2 := pp^;
   next := psingle(longint(pf) + 8);
   next2 := psingle(longint(pf) + 12);
   o2 := o2 * (1 - cnt) + cnt * next2^;
  end else
  begin
   next := psingle(longint(pf) + 4);
   o2 := o1;
  end;
  if (cnt <1 ) and (interpolate) then // interpolation?
   o1 := o1 * (1 - cnt) + cnt * next^; // get next sample

  cnt := cnt + speed * (sr / samplerate);
  while (cnt >= 1) do
  begin
   inc(pf, ch);
   cnt := cnt - 1;
   inc(cnt2, ch);
  end;
  if (cnt2 >= size - 1) then
  begin
   if not looped then
   begin
    pmode := 0;
    player.s_pos2.position := 0;
    if (player.mode2.ItemIndex = 2) and (player.WavBox.Items.Count > 0) then
    begin
     player.WavBox.itemindex := (Player.WavBox.itemindex + 1) mod player.WavBox.Items.Count;
     player.Button8click(nil);
    end else
    if (player.mode2.ItemIndex = 3) and (player.WavBox.Items.Count > 0) then
    begin
     player.WavBox.itemindex := random(player.WavBox.Items.Count);
     player.Button8click(nil);
    end;
   end;
   cnt2 := 0;
   cnt := 0;
   pf := pbuf;
  end;
 end;

 if ch = 2 then // stereo output
 begin
  o1 := vol * o1;
  o2 := vol * o2;
 end else
 begin // mono output
  o1 := vol * o1 * 2 * (1 - pan);
  o2 := vol * o1 * 2 * pan;
 end;
end;

procedure TWavPlayer.stop;
begin
 pmode := 0;
 cnt2 := 0;
 cnt := 0;
 pf := psingle(longint(pbuf) + cnt2 * sizeof(single));
end;

procedure TWavPlayer.SetPos(i: Integer);
begin
 cnt2 := i;
 cnt := 0;
 pf := psingle(longint(pbuf) + cnt2 * sizeof(single));
end;

procedure TWavPlayer.unload;
begin
 pmode := 0;
 if assigned(pbuf) then
 begin
  freemem(pbuf);
  pbuf := nil;
 end;
 pf := pbuf;
 cnt := 0;
 cnt2 := 0;
end;

procedure TFmMiniHost.Alwaysontop1Click(Sender: TObject);
begin
 alwaysontop1.checked := not alwaysontop1.checked;
{$IFNDEF FPC}
 if alwaysontop1.checked then
  SetWindowPos(self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
   SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE)
 else
  SetWindowPos(self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
   SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE);
{$ENDIF}
end;

procedure TFmMiniHost.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
 if not UseMouseWheel1.Checked then exit;
 panic1click(nil);
 if CurProg > 0 then
  VSTHost.VSTPlugIns[0].SetProgram(CurProg - 1);
end;

procedure TFmMiniHost.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if not UseMouseWheel1.Checked then exit;
 panic1click(nil);
 if CurProg + 1 < VSTHost.VSTPlugIns[0].numPrograms then
  VSTHost.VSTPlugIns[0].SetProgram(CurProg + 1);
end;

procedure TFmMiniHost.Exit1Click(Sender: TObject);
begin
 Close;
end;

procedure TFmMiniHost.onoffMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 Processing := not Processing;
 Panic1Click(nil);
end;

procedure TFmMiniHost.prboxClick(Sender: TObject);
begin
 Timer1.Enabled := false;
 panic1click(nil);
 VSTHost.VSTPlugIns[0].SetProgram(prbox.ItemIndex);
 CurProg := prbox.ItemIndex;
 Timer1.Enabled := true;
end;

procedure TFmMiniHost.prboxKeyPress(Sender: TObject; var Key: Char);
begin
 key := #0;
end;

procedure TFmMiniHost.wpClick(Sender: TObject);
begin
 F4PlayStopWAV1Click(Sender);
end;

procedure TFmMiniHost.MPClick(Sender: TObject);
begin
 F3PlayStopMIDI1Click(Sender);
end;

procedure TFmMiniHost.WRClick(Sender: TObject);
begin
 F5RecStopWAV1Click(Sender);
end;

procedure TFmMiniHost.prboxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 if Index < 0 then exit;
 prbox.Canvas.FillRect(Rect);
 prbox.Canvas.TextOut(rect.Left + 2, rect.top, prbox.items[index]);
end;

procedure TFmMiniHost.Image2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 panic1click(nil);
 if x < image2.width shr 1 then
 begin
  if CurProg > 0 then
   VSTHost.VSTPlugIns[0].SetProgram(CurProg - 1);
 end else
 begin
  if CurProg + 1 < VSTHost.VSTPlugIns[0].numPrograms then
   VSTHost.VSTPlugIns[0].SetProgram(CurProg + 1);
 end;
end;

procedure TFmMiniHost.dropdownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 prbox.DroppedDown := not prbox.DroppedDown;
end;

procedure TFmMiniHost.prboxChange(Sender: TObject);
begin
 status.SetFocus;
end;

procedure TFmMiniHost.quicksettingsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 F1ASIOControlPanel1Click(sender);
end;

procedure TFmMiniHost.quickmidplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if button = mbleft then F3PlayStopMIDI1Click(Sender);
end;

procedure TFmMiniHost.quickwavplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if button = mbleft then F4PlayStopWAV1Click(Sender);
end;

procedure TFmMiniHost.quickwavrecMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if button = mbleft then F5RecStopWAV1Click(Sender);
end;

procedure TFmMiniHost.AddMID(s:string);
var j, i: Integer;
    ms: pshortstr;
    t:tstringlist;
begin
 if uppercase(extractfileext(s))='.MPL' then
 begin
  {$IFDEF FPC}
  for I := 0 to player.WavBox.Items.Count - 1
   do player.WavBox.Selected[I] := True;
  {$ELSE}
  player.WavBox.SelectAll;
  {$ENDIF}
  player.button2click(nil);
  t:=tstringlist.Create;
  t.LoadFromFile(s);
  for i := 0 to t.Count - 1 do AddMID(t[i]);
  if (t.count > 0) and (uppercase(t[0]) = 'RANDOM') then
   player.mode1.ItemIndex := 3;
  t.free;
 end;
 if uppercase(extractfileext(s))<>'.MID' then exit;
 if not fileexists(s) then exit;
 j := -1;
 for i := 0 to Player.midibox.Items.Count - 1 do
  if PShortstr(Player.midibox.Items.Objects[i])^ = s then
   j := 0;
 if j = 0 then exit;
 getmem(ms, sizeof(shortstr));
 ms^ := s;
 Player.midibox.Items.AddObject(ExtractFilename(s), TObject(ms));
 Player.midibox.ItemIndex := Player.midibox.Items.Count - 1;
end;

procedure TFmMiniHost.AddWAV(s:string);
var j, i: Integer;
    ms: pshortstr;
    t:tstringlist;
begin
 if uppercase(extractfileext(s))='.WPL' then
 begin
  {$IFDEF FPC}
  for I := 0 to player.WavBox.Items.Count - 1
   do player.WavBox.Selected[I] := True;
  {$ELSE}
  player.WavBox.SelectAll;
  {$ENDIF}
  player.button6click(nil);
  t:=tstringlist.Create;
  t.LoadFromFile(s);
  for i := 0 to t.Count - 1 do AddWAV(t[i]);
  if (t.count > 0) and (uppercase(t[0]) = 'RANDOM')
   then player.mode2.ItemIndex := 3;
  t.free;
 end;
 if uppercase(extractfileext(s))<>'.WAV' then exit;
 if not fileexists(s) then exit;
 j := -1;
 for i := 0 to Player.WavBox.Items.Count - 1 do
  if PShortstr(Player.WavBox.Items.Objects[i])^ = s then
   j := 0;
 if j = 0 then exit;
 GetMem(ms, sizeof(shortstr));
 ms^ := s;
 Player.WavBox.Items.AddObject(ExtractFilename(s), TObject(ms));
 Player.WavBox.ItemIndex := Player.WavBox.Items.Count - 1;
end;

procedure TFmMiniHost.bord2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if shift = [ssright] then Player.Show;
end;

procedure TFmMiniHost.ShowMIDIWAVPlayerrecorderWindow1Click(
  Sender: TObject);
begin
 Player.Show;
end;

procedure TFmMiniHost.DownmixToStereo1Click(Sender: TObject);
begin
 DownmixToStereo1.checked := not DownmixToStereo1.checked;
end;

procedure TFmMiniHost.MidiThru1Click(Sender: TObject);
begin
 MidiThru1.checked := not MidiThru1.checked;
end;

procedure TFmMiniHost.UseMouseWheel1Click(Sender: TObject);
begin
 UseMouseWheel1.Checked := not UseMouseWheel1.Checked;
end;

procedure TFmMiniHost.IdleTimerTimer(Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].Idle;
 VSTHost.VSTPlugIns[0].EditIdle;
end;

procedure TFmMiniHost.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TArrayOfSingleDynArray);
var j, i: Integer;
    bs, ChOfs: Integer;
begin
 bs := ASIOHost.BufferSize;
 if (bs <= 0) or (not allowed) or (VSTHost = nil)
  then exit;

 VSTHost.UpdateVstTimeInfo(bs);
 MidiFile.MidiTimer(nil);

 evproc := true;
 if mdatacnt > 0 then
  begin
   MyEvents.numEvents := mdatacnt;
   if VSTHost.VSTPlugIns[0].CanDo('receiveVstMidiEvent') >= 0 then
    VSTHost.VSTPlugIns[0].ProcessEvents(@MyEvents);
   if (CurrentMidiOut > 0) and MidiThru1.checked then
    begin
     for i := 0 to mdatacnt - 1 do
      MidiOutput.Send(CurrentMidiOut - 1,
       PVstMidiEvent(MyEvents.events[i])^.midiData[0],
       PVstMidiEvent(MyEvents.events[i])^.midiData[1],
       PVstMidiEvent(MyEvents.events[i])^.midiData[2]);
    end;
  end;

 ChOfs := ASIOHost.OutputChannelOffset;
 if CurrentInputChannel = 0 then
  begin
   for i := 0 to bs - 1 do
    begin
     InBufL[i] := 0;
     InBufR[i] := 0;
    end;
  end
 else
  begin
   for i := 0 to bs - 1 do
    begin
     InBufL[i] := InputVol * InBuffer[ASIOHost.InputChannelOffset][i];
     InBufR[i] := InputVol * InBuffer[ASIOHost.InputChannelOffset + 1][i];
    end;
  end;

 // fill WavBufL and WavBufR
 if Wavefile.pmode = 1 then
  begin
   for i := 0 to bs - 1 do
    WaveFile.process(WavBufL[i], WavBufR[i]);
  end
 else
  begin
   FillChar(WavBufL[0], bs * SizeOf(Single), 0);
   FillChar(WavBufR[0], bs * SizeOf(Single), 0);
  end;

 if numout > 0 then
 begin
  // assign Input to VSTBufIn
  for i := 0 to numout - 1 do FillChar(VSTBufOut[i][0], bs * SizeOf(Single), 0);
  if effFlagsIsSynth in VSTHost.VSTPlugIns[0].EffectOptions then
   for i := 0 to numin - 1 do FillChar(VSTBufIn[i][0], bs * SizeOf(Single), 0)
  else
   for i := 0 to bs - 1 do
   begin
    VSTBufIn[0][i] := (WavBufL[i] * Wavefile.Vol) + InBufL[i];
    VSTBufIn[1][i] := (WavBufR[i] * Wavefile.Vol) + InBufR[i];
   end;

  // apply Processing
  if Processing then
  begin
   if effFlagsCanReplacing in VSTHost.VSTPlugIns[0].EffectOptions then
    VSTHost.VSTPlugIns[0].ProcessReplacing(@VSTBufIn[0], @VSTBufOut[0], bs)
   else
    VSTHost.VSTPlugIns[0].Process(@VSTBufIn[0], @VSTBufOut[0], bs);
   if downmix then
    for i := 0 to bs - 1 do
     for j := 2 to numout - 1 do
     begin
      if vpp[j].arrangementType = 0 then
      begin
       VSTBufOut[0][i] := VSTBufOut[0][i] + VSTBufOut[j][i];
       VSTBufOut[1][i] := VSTBufOut[1][i] + VSTBufOut[j][i];
      end else
       VSTBufOut[j mod 2][i] := VSTBufOut[j mod 2][i] + VSTBufOut[j][i];
     end;
  end;

  // assign Output from VSTBufOut
  if numout = 1 then j := 0 else j := 1;
  if effFlagsIsSynth in VSTHost.VSTPlugIns[0].EffectOptions then
   for i := 0 to bs - 1 do
   begin
    OutBuffer[ChOfs][i] := (VSTBufOut[0][i] * VSTVol + InBufL[i] + WavBufL[i] * Wavefile.Vol) * OverallVol;
    OutBuffer[ChOfs + 1][i] := (VSTBufOut[j][i] * VSTVol + InBufR[i] + WavBufR[i] * Wavefile.Vol) * OverallVol;
   end
  else
   for i := 0 to bs - 1 do
   begin
    OutBuffer[ChOfs][i] := (VSTBufOut[0][i] * VSTVol + (1 - VSTVol) * VSTBufIn[0][i]) * OverallVol;
    OutBuffer[ChOfs + 1][i] := (VSTBufOut[j][i] * VSTVol + (1 - VSTVol) * VSTBufIn[j][i]) * OverallVol;
   end;
 end else
  for i := 0 to bs - 1 do
  begin
   OutBuffer[ChOfs][i] := (InBufL[i] + WavBufL[i] * Wavefile.Vol) * OverallVol;
   OutBuffer[ChOfs + 1][i] := (InBufR[i] + WavBufR[i] * Wavefile.Vol) * OverallVol;
  end;

 if recording = 1 then
  begin
   totalframes := totalframes + ASIOHost.buffersize;
   if wavwriter.Format.nChannels = 1 then
    WavWriter.WriteFloatData(OutBuffer[ChOfs], bs)
   else
    WavWriter.WriteFloatDataSeparateStereo(OutBuffer[ChOfs],
     OutBuffer[ChOfs + 1], bs);
  end;

 mdatacnt := 0;
 evproc := false;
end;

{$IFDEF FPC}
initialization
  {$i MiniHostForm.lrs}
{$ENDIF}

end.
