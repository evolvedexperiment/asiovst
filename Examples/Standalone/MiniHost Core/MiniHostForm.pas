unit MiniHostForm;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Types, Messages,
  Forms, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, DAVDCommon, DVstEffect, WaveIOX, DMidiFile, DMidiIO, DVSTHost,
  DAVASIOHost;

type
  ShortStr = string[255];
  PShortStr = ^ShortStr;

const
  AppVersion = '1.0';
  AppName = 'MiniHost Core';

type
  TWavPlayerMode = (wpmPause, wpmPlay);
  TWavPlayer = class
  private
    pbuf        : Pointer;
    pf          : PSingle;
    fLooped,
    fInterpol   : Boolean;
    fCnt2,
    fSize,
    fSR, fCh    : Integer;
    fSpeed,
    fVol, fPan,
    fSamplerate : Single;
    fCnt        : Double;
    fPMode      : TWavPlayerMode;
    fFilename   : TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPos(i: Integer);
    procedure Process(var o1, o2: single);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Load(s: TFileName);
    procedure Unload;
  published
    property SampleRate: Single read fSampleRate write fSampleRate;
    property Speed: Single read fSpeed write fSpeed;
    property Size: Integer read fSize;
    property Volume: Single read fVol write fVol;
    property Looped: Boolean read fLooped write fLooped;
    property Interpolate: Boolean read fInterpol write fInterpol;
    property Filename: TFileName read fFilename write fFilename;
  end;

  MIDIData = record
    d1, d2, d3: byte;
    pos: Integer;
  end;

  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TRGB24 = packed record
    B, G, R: Byte;
  end;
  TRGB24Array = packed array[0..MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  { TFmMiniHost }

  TRecordState = (rsStop, rsRecord, rsPause);
  TFmMiniHost = class(TForm)
    VSTHost: TVSTHost;
    ASIOHost: TASIOHost;
    WaveTimer: TTimer;
    IdleTimer: TTimer;
    MainMenu: TMainMenu;

    MIVST: TMenuItem;
    MIASIO: TMenuItem;
    MIMIDI: TMenuItem;
    MIHelp: TMenuItem;
    MIVSTLoadPlugin: TMenuItem;
    MIAbout: TMenuItem;
    MIPanic: TMenuItem;
    MIMIDIIn: TMenuItem;
    MIMIDIOut: TMenuItem;
    MIPreset: TMenuItem;
    MILoadPreset: TMenuItem;
    MILoadBank: TMenuItem;
    MISavePreset: TMenuItem;
    MISaveBank: TMenuItem;
    MIAsioDriver: TMenuItem;
    MIASIOControlPanel: TMenuItem;
    MIASIOOutputChannel: TMenuItem;
    MIRenamePreset: TMenuItem;
    MISettings: TMenuItem;
    MIASIOInputChannel: TMenuItem;
    MIVSTClosePlugin: TMenuItem;
    MIDownMixToStereo: TMenuItem;
    MIMidiThru: TMenuItem;
    MIShowPreset: TMenuItem;
    MIAlwaysOnTop: TMenuItem;
    MIUseMouseWheel: TMenuItem;
    MIMain: TMenuItem;
    MIExit: TMenuItem;
    MIShowMIDIWAVWindow: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    PnStatus: TPanel;
    PresetBox: TComboBox;
    ToolBarBackground: TImage;
    IBtLeftRight: TImage;
    IBtDropDown: TImage;
    IQuickSettings: TImage;
    IQuickMidPlay: TImage;
    IQuickWavPlay: TImage;
    IQuickWavRec: TImage;
    BorderPlayMIDI: TImage;
    BorderPlayWave: TImage;
    BorderRecordWave: TImage;
    BorderOnOff: TImage;
    BorderOptions: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure VSTHostAudioMasterIdle(Sender: TVSTPlugin);
    procedure VSTHostAudioMasterNeedIdle(Sender: TVSTPlugin);
    procedure ASIOHostLatencyChanged(Sender: TObject);
    procedure ASIOHostUpdateSamplePos(Sender: TObject; SamplePosition: Int64);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
    procedure MIPanicClick(Sender: TObject);
    procedure MILoadPresetClick(Sender: TObject);
    procedure MISavePresetClick(Sender: TObject);
    procedure MILoadBankClick(Sender: TObject);
    procedure MISaveBankClick(Sender: TObject);
    procedure MIVSTClosePluginClick(Sender: TObject);
    procedure MIVSTLoadPluginClick(Sender: TObject);
    procedure MIASIOControlPanelClick(Sender: TObject);
    procedure MIRenamePresetClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MISettingsClick(Sender: TObject);
    procedure MIShowPresetClick(Sender: TObject);
    procedure MIStartRecordingClick(Sender: TObject);
    procedure MIStopRecordingClick(Sender: TObject);
    procedure MIAlwaysOnTopClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIShowMIDIWAVWindowClick(Sender: TObject);
    procedure MIDownMixToStereoClick(Sender: TObject);
    procedure MIMidiThruClick(Sender: TObject);
    procedure MIUseMouseWheelClick(Sender: TObject);
    procedure StartPlayback2Click(Sender: TObject);
    procedure StartPlayback1Click(Sender: TObject);
    procedure StopPlayback1Click(Sender: TObject);
    procedure StopPlayback2Click(Sender: TObject);
    procedure WaveTimerTimer(Sender: TObject);
    procedure LoadMIDIFile1Click(Sender: TObject);
    procedure RenameF1Click(Sender: TObject);
    procedure F3PlayStopMIDI1Click(Sender: TObject);
    procedure F4PlayStopWAV1Click(Sender: TObject);
    procedure F5RecStopWAV1Click(Sender: TObject);
    procedure F11MIDIPanic1Click(Sender: TObject);
    procedure PresetBoxClick(Sender: TObject);
    procedure PresetBoxChange(Sender: TObject);
    procedure PresetBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PresetBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IdleTimerTimer(Sender: TObject);
    procedure IBtLeftRightMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IBtDropDownMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IOnOffMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickMidPlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    fRecordState    : TRecordState;
    fDownMix        : Boolean;
    fTotalFrames    : Integer;
    fPanel          : TPanel;
    fTitle          : string;
    fWaveFile       : TWavPlayer;
    fProcessing     : Boolean;
    fAllowed        : Boolean;
    fLoadProg       : Integer;
    fDirPlugin,
    fDirPreset,
    fDirWave,
    fDirMidi        : string;
    fPluginLoaded   : Boolean;
    fWavBufL,
    fWavBufR,
    fInBufL,
    fInBufR         : TAVDSingleDynArray;
    fVSTBufIn,
    fVSTBufOut      : TAVDArrayOfSingleDynArray;
    fMIDIPlaying    : Boolean;
    fMyEvents       : TVstEvents;
    fOverallVol     : Single;
    fVSTVol         : Single;
    fInputVol       : Single;
    fCurProg        : Integer;
    fCurProgName    : string;
    fVSTPinProps    : array of TVstPinProperties;
    fNumIn,
    fNumOut         : Integer;

    fCurrentASIO    : Integer;
    fCurrentMIDIIn  : Integer;
    fCurrentMIDIOut : Integer;
    fCurrentOutputChannel,
    fCurrentInputChannel: Integer;

    fMDataCnt       : Integer;
    procedure MyMidiEvent(event: PMidiEvent);
    procedure MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: byte);
    procedure SysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
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
    MidiFile      : TMidiFile;
    WavWriter     : TWavWriter;
    procedure AddMID(s: string);
    procedure AddWAV(s: string);
    procedure LoadWAVFile;
    procedure LoadWAV(fn: string);
    procedure NoteOn(ch, note, v: byte);
    procedure NoteOff(ch, note: byte);
    procedure StartAudio;
    procedure StopAudio;
    procedure RecordWAVFileSelect;
    procedure BuildPresetList;
    procedure LoadPresets(Files: TStrings);
    procedure LoadPlugin(s: string; prog: Integer = 0);
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
  published
    property RecordState: TRecordState read fRecordState write fRecordState;
    property WaveFile: TWavPlayer read fWaveFile;
    property CurrentProgram: Integer read fCurProg;
    property CurrentProgramName: string read fCurProgName;
    property OverallVolume: Single read fOverallVol write fOverallVol;
    property VSTVol: Single read fVSTVol write fVSTVol;
    property InputVol: Single read fInputVol write fInputVol;
    property MidiPlaying: Boolean read fMidiPlaying write fMidiPlaying;
  end;

var
  FmMiniHost : TFmMiniHost;
  ININame    : string;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, Inifiles, Dialogs, ShellAPI, OptionsForm, AboutForm, PlayerForm;

procedure TFmMiniHost.FormCreate(Sender: TObject);
var
  i        : Integer;
  m        : TMenuItem;
  tlist    : TStringList;
  slist    : TStrings;
  s        : string;
  Settings : TIniFile;
  p        : PVstMidiEvent;
  mi       : Integer;
(*
  b        : Byte;
  flt      : array [0..1] of Single;
  x, y     : Integer;
  Line24   : PRGB24Array;
  Line32   : PRGB32Array;
*)
begin
 fAllowed := False;
 with ToolBarBackground.Picture do
  begin
   Bitmap.TransparentColor := $A8A8A8;
(*
   // not working yet!!!
   case Bitmap.PixelFormat of
    pf24bit:
     for y := 0 to Bitmap.Height - 1 do
      begin
       Line24 := Bitmap.Scanline[y];
       for x := 0 to Bitmap.Width - 1 do
        begin
         flt[1] := 0.9 * flt[0] + 0.1 * (2 * random - 1);
         b := round($F * flt[1]);
         flt[0] := flt[1];
         Line24[x].B := $A8 + b;
         Line24[x].G := $A8 + b;
         Line24[x].R := $A8 + b;
        end;
      end;
    pf32bit:
     for y := 0 to Bitmap.Height - 1 do
      begin
       Line32 := Bitmap.Scanline[y];
       for x := 0 to Bitmap.Width - 1 do
        begin
         flt[1] := 0.9 * flt[0] + 0.1 * (2 * random - 1);
         b := round($F * flt[1]);
         flt[0] := flt[1];
         Line32[x].B := $A8 + b;
         Line32[x].G := $A8 + b;
         Line32[x].R := $A8 + b;
         Line32[x].A := $FF;
        end;
      end;
   end;
(*
*)
  end;
 IBtDropDown.picture.Bitmap.TransparentColor := $A8A8A8;
 IBtDropDown.picture.Bitmap.Transparent := True;
 BorderOnOff.picture.Bitmap.TransparentColor := clBlack;
 BorderOnOff.picture.Bitmap.Transparent := False;
 BorderOptions.Picture.Assign(BorderOnOff.Picture);
 BorderPlayMIDI.Picture.Assign(BorderOnOff.Picture);
 BorderPlayWave.Picture.Assign(BorderOnOff.Picture);
 BorderRecordWave.Picture.Assign(BorderOnOff.Picture);

 Assert(SizeOf(TVSTMidiEvent) = SizeOf(TVstMidiSysexEvent));

 for i := 0 to 2047 do
  begin
   GetMem(fMyEvents.Events[i], SizeOf(TVSTMidiEvent));
   FillChar(fMyEvents.Events[i]^, SizeOf(TVSTMidiEvent), 0);
   p := PVstMidiEvent(fMyEvents.events[i]);
   p^.EventType := etMidi;
   p^.byteSize := 24;
  end;

 fWaveFile := TWavPlayer.create;
 fPluginLoaded := False;

 Player := TPlayer.Create(self);

 fPanel := TPanel.Create(self);
 with fPanel do
  begin
   Parent := self;
   Left := 0;
   Top := PnStatus.height;
   Width := 700;
   Height := 1;
   Tag := -1;
   OnResize := PluginResize;
  end;

 FmAbout := TFmAbout.Create(self);
 FmOptions := TFmOptions.Create(self);
 FmOptions.Host := self;

 DragAcceptFiles(self.handle, True);

{$IFNDEF FPC}
 ININame := GetApplicationDirectory + '\' + ChangeFileExt(GetApplicationFilename, '.ini');
{$ENDIF}

 MidiFile := TMidiFile.create(nil);
 MidiFile.OnMidiEvent := MyMidiEvent;
 MidiFile.ManualCall := True;
 fRecordState := rsStop;

 try
  m := TMenuItem.Create(self);
  m.RadioItem := True;
  m.tag := 0;
  m.Caption := 'None';
  m.OnClick := MIDIInChange;
  MIMIDIIn.Add(m);
  for mi := 0 to MidiInput.Devices.Count - 1 do
   begin
    m := TMenuItem.Create(self);
    m.RadioItem := True;
    m.tag := mi + 1;
    m.Caption := MidiInput.Devices[mi];
    m.OnClick := MIDIInChange;
    MIMIDIIn.Add(m);
   end;
 except
  MessageDlg('ERROR: A serious problem occured with MIDI-In drivers!', mtError, [mbOK], 0);
 end;

 try
  m := TMenuItem.Create(self);
  m.RadioItem := True;
  m.tag := 0;
  m.Caption := 'None';
  m.OnClick := MIDIOutChange;
  MIMIDIOut.Add(m);
  for mi := 0 to MidiOutput.Devices.Count - 1 do
   begin
    m := TMenuItem.Create(self);
    m.RadioItem := True;
    m.tag := mi + 1;
    m.Caption := MidiOutput.Devices[mi];
    m.OnClick := MIDIOutChange;
    MIMIDIOut.Add(m);
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
    m.RadioItem := True;
    m.Tag := i;
    m.Caption := slist.Strings[i];
    m.OnClick := ASIOChange;
    MIAsioDriver.Add(m);
   end;
 if slist.Count = 0 then
 begin
  MessageDlg('No ASIO Driver present! Application Terminated!', mtError, [mbOK], 0);
  Application.Terminate;
 end;

 MidiInput.OnMidiData := MidiData;
 MidiInput.OnSysExData := SysExData;

 Settings := TIniFile.Create(ININame);
 i := Settings.ReadInteger('Audio', 'ASIO Driver', -1);
 if i = -1 then i := slist.IndexOf('ASIO4ALL v2');
 if i = -1 then i := slist.IndexOf('ASIO4ALL');
 if (i < 0) or (i >= slist.count) then i := 0;
 MIAsioDriver.Items[i].Checked := True;
 try
  ASIOChange(MIAsioDriver.Items[i]);
 except
 end;
 i := Settings.ReadInteger('Audio', 'Output Channel', 0);
 if (i >= 0) and (i < MIASIOOutputChannel.Count) and (MIASIOOutputChannel.Count <> 0)
  then
   try
    MIASIOOutputChannel.Items[i].checked := True;
    MIASIOOutputChannel.Items[i].Click;
   except
   end;

 i := Settings.ReadInteger('Audio', 'Input Channel', 0);
 if (i >= 0) and (i < MIASIOInputChannel.Count) and (MIASIOInputChannel.Count <> 0)
  then
   try
    MIASIOInputChannel.Items[i].checked := True;
    MIASIOInputChannel.Items[i].Click;
   except
   end;

 WaveFile.fFilename := Settings.ReadString('Audio', 'File', '');

 i := Settings.ReadInteger('Audio', 'Record Bits', 16);
 case i of
  16 : Player.CbRecordFormat.ItemIndex := 0;
  else Player.CbRecordFormat.ItemIndex := 1;
 end;

 MIShowPreset.Checked := Settings.ReadBool('Layout', 'ShowPresetInTitleBar', True);
 fDirPlugin := Settings.ReadString('General', 'Plugin Directory', '');
 fDirPreset := Settings.ReadString('General', 'Preset Directory', '');
 fDirWave   := Settings.ReadString('General', 'Wave Directory', '');
 fDirMidi   := Settings.ReadString('General', 'Midi Directory', '');

 Player.MidiBox.Clear;
 tlist := TStringList.Create;
 Settings.ReadSection('Playlist MIDI', tlist);
 for i := 0 to tlist.Count - 1 do AddMID(tlist[i]);
 Player.WavBox.Clear;
 Settings.ReadSection('Playlist WAV', tlist);
 for i := 0 to tlist.Count - 1 do AddWAV(tlist[i]);
 tlist.Free;

 Player.CBMidiPlayMode.ItemIndex := Settings.ReadInteger('MIDI', 'LoopMode', 1);
 Player.CBWavPlayMode.ItemIndex := Settings.ReadInteger('Audio', 'LoopMode', 1);
 WaveFile.looped := Player.CBWavPlayMode.itemindex = 1;
 MidiFile.Filename := Settings.ReadString('MIDI', 'LastFile', '');
 if (MidiFile.filename <> '') and fileexists(MidiFile.filename)
  and (uppercase(extractfileext(MidiFile.filename))='.MID') then MidiFile.ReadFile;
 MidiPlaying := False;

 MIDownMixToStereo.checked := Settings.ReadBool('VST', 'DownmixStereo', False);
 MIMidiThru.checked := Settings.ReadBool('VST', 'MIDIThru', False);

 s := Settings.ReadString('VST', 'LastPlugin', '');
 i := Settings.ReadInteger('VST', 'LastProgram', 0);
 Settings.Free;

 LoadWAV(WaveFile.Filename);

 if (ParamCount > 0) and (FileExists(Paramstr(1))) then
  begin
   LoadPlugin(Paramstr(1));
   VSTHost[0].OnProcessEvents := ProcessEvents;
  end
 else if FileExists(s) then
  begin
   fLoadProg := i;
   LoadPlugin(s, i);
   VSTHost[0].OnProcessEvents := ProcessEvents;
  end;
end;

procedure TFmMiniHost.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 fProcessing := False;
 fAllowed := False;
 try
  StopAudio;
 except
 end;

 with TIniFile.Create(ININame) do
  try
   EraseSection('Playlist MIDI');
   EraseSection('Playlist WAV');
   for i := 0 to Player.MidiBox.Items.Count - 1
    do WriteString('Playlist MIDI', PShortStr(Player.MidiBox.Items.Objects[i])^, '');
   for i := 0 to Player.WavBox.Items.Count - 1
    do WriteString('Playlist WAV', PShortStr(Player.WavBox.Items.Objects[i])^, '');
   WriteInteger('General', 'Timer', WaveTimer.Interval);
   WriteInteger('Layout', 'MainWindow X', Left);
   WriteInteger('Layout', 'MainWindow Y', Top);
   WriteInteger('Layout', 'SettingsWindow X', FmOptions.Left);
   WriteInteger('Layout', 'SettingsWindow Y', FmOptions.Top);
   WriteBool('Layout', 'SettingsWindow Visible', FmOptions.Showing);
   WriteString('General', 'Plugin Directory', fDirPlugin);
   WriteString('General', 'Preset Directory', fDirPreset);
   WriteString('General', 'Wave Directory', fDirWave);
   WriteString('General', 'Midi Directory', fDirMidi);
   WriteInteger('Layout', 'PlayerWindow X', Player.Left);
   WriteInteger('Layout', 'PlayerWindow Y', Player.Top);
   WriteBool('Layout', 'PlayerWindow Visible', Player.Showing);
   WriteBool('VST', 'DownmixStereo', MIDownMixToStereo.checked);
   WriteBool('VST', 'MIDIThru', MIMidiThru.checked);
   WriteBool('VST', 'UseMouseWheel', MIUseMouseWheel.Checked);
   WriteBool('MIDI', 'MidiFileOnlyChannel1', Player.CbOnlyChannel1.checked);
   WriteInteger('Audio', 'ASIO Driver', fCurrentASIO);
   WriteInteger('Audio', 'Output Channel', fCurrentOutputChannel);
   WriteInteger('Audio', 'Input Channel', fCurrentInputChannel);
   if Player.WavBox.Items.Count = 0 then WAVEFile.Filename := '';//c
   WriteString('Audio', 'File', WAVEFile.Filename);
   WriteInteger('Audio', 'VST Volume', FmOptions.SbVSTVolume.position);
   WriteInteger('Audio', 'Overall Volume', FmOptions.SbOverallVolume.position);
   WriteInteger('Audio', 'Input Volume', FmOptions.SbInputVolume.position);
   WriteInteger('Audio', 'WAV Volume', FmOptions.SbWavVolume.position);
   WriteInteger('VST', 'Tempo', FmOptions.SbTempo.position);
   WriteString('VST', 'LastPlugin', VSTHost[0].DLLFilename);
   WriteInteger('VST', 'LastProgram', fCurProg);
   if Player.MidiBox.Items.Count = 0 then MidiFile.Filename := '';//c
   WriteString('MIDI', 'LastFile', MidiFile.Filename);
   WriteInteger('MIDI', 'LoopMode', Player.CBMidiPlayMode.itemindex);
   WriteInteger('Audio', 'LoopMode', Player.CBWavPlayMode.itemindex);
   WriteBool('Layout', 'ShowPresetInTitleBar', MIShowPreset.Checked);
   WriteInteger('MIDI', 'MIDI-In Driver', fCurrentMidiIn);
   WriteInteger('MIDI', 'MIDI-Out Driver', fCurrentMidiOut);
   case Player.CbRecordFormat.ItemIndex of
     0: i := 16;
    else i := 32;
   end;
   WriteInteger('Audio', 'Record Bits', i);
 finally
  Free;
 end;

 if fPluginLoaded then ClosePlugin;

 if Assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 try
  MidiInput.CloseAll;
  MidiOutput.CloseAll;
 except
 end;

 MidiFile.Free;
 WaveFile.Free;
 fNumIn := 0;
 fNumOut := 0;
 for i := 0 to length(fVSTBufOut) - 1 do SetLength(fVSTBufOut[i], 0);
 for i := 0 to length(fVSTBufIn) - 1 do SetLength(fVSTBufIn[i], 0);
 SetLength(fVSTBufOut, 0);
 SetLength(fVSTBufIn, 0);
 SetLength(fVSTPinProps, 0);
 for i := 0 to 2047 do FreeMem(fMyEvents.Events[i]);
end;

procedure TFmMiniHost.StartAudio;
var
  i : Integer;
begin
 if ASIOHost.Active then exit;
 ASIOHost.Active := False;
 VSTHost.BlockSize := ASIOHost.BufferSize;
 for i := 0 to VSTHost.VSTPlugIns.Count - 1 do
  if VSTHost[i].Active then
   begin
    VSTHost[i].CanDo('sendVstTimeInfo');
    VSTHost[i].CanDo('receiveVstTimeInfo');
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
  WaveFile.SampleRate := ASIOHost.SampleRate;
 end;
end;

procedure TFmMiniHost.ClosePlugin;
var
  i : Integer;
begin
 MidiFile.StopPlaying;
 MidiPlaying := False;
 WaveTimer.enabled := False;
 fProcessing := False;
 
 StopAudio;
 MIPanicClick(nil);
 fRecordState := rsStop;

 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 if (VSTHost[0].DLLFileName <> '') and VSTHost[0].Active then
  with VSTHost[0] do
   begin
    CloseEdit;
    Close;
    Unload;
    DLLFileName := '';
   end;

 fNumIn := 0;
 fNumOut := 0;
 for i := 0 to length(fVSTBufOut) - 1 do SetLength(fVSTBufOut[i], 0);
 for i := 0 to length(fVSTBufIn) - 1 do SetLength(fVSTBufIn[i], 0);
 SetLength(fVSTBufOut, 0);
 SetLength(fVSTBufIn, 0);
 SetLength(fVSTPinProps, 0);

 MILoadPreset.Enabled := False;
 MISavePreset.Enabled := False;
 MILoadBank.Enabled := False;
 MISaveBank.Enabled := False;
 fPluginLoaded := False;
 PresetBox.clear;
 WaveTimer.enabled := True;
end;

procedure TFmMiniHost.BuildPresetList;
var
  m    : TMenuItem;
  n, i : Integer;
  p    : Array[0..100] of char;
  s    : string;
begin
 PresetBox.clear;
 n := VSTHost[0].numPrograms;

 for i := 0 to n - 1 do
 begin
  VSTHost[0].GetProgramNameIndexed(-1, i, p);

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
  PresetBox.AddItem(s + ': ' + m.caption, nil);
 end;

 if n >= 0 then PresetBox.ItemIndex := fCurProg;
end;

procedure TFmMiniHost.LoadPlugin(s: string; prog: Integer = 0);
var
  r : ERect;
  i : Integer;
begin
 if not FileExists(s) then exit;
 WaveTimer.enabled := False;
 fProcessing := False;
 StopAudio;
 sleep(2);
 ClosePlugin;
 sleep(2);

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
 VSTHost[0].DLLFilename := s;

 try
  VSTHost[0].Active := True;
 except
{$IFNDEF FPC}
  msg(s + ' is not a valid VST plugin!');
{$ENDIF}
  VSTHost[0].Active := False;
  VSTHost[0].DLLFilename := '';
  exit;
 end;

 SetLength(fVSTBufIn,  max(VSTHost[0].numInputs,  2), ASIOHost.BufferSize);
 SetLength(fVSTBufOut, max(VSTHost[0].numOutputs, 2), ASIOHost.BufferSize);
 fNumIn := VSTHost[0].numInputs;
 fNumOut := VSTHost[0].numOutputs;
 SetLength(fVSTPinProps, fNumOut);
 for i := 0 to fNumOut - 1
  do fVSTPinProps[i] := VSTHost[0].GetOutputProperties(i);

 with VSTHost[0] do
  begin
   GUIForm := TForm(fPanel);
   ShowEdit(TForm(fPanel));

   fTitle := GetVendorString + ' ' +  GetEffectName;
   BuildPresetList;
   r := EditGetRect;
  end;
 fPanel.width  := r.right - r.left;
 fPanel.height := r.bottom - r.top;
 fPanel.top    := PnStatus.height;

 MILoadPreset.Enabled := True;
 MILoadBank.Enabled   := True;
 MISavePreset.Enabled := True;
 MISaveBank.Enabled   := True;
 fProcessing          := True;
 StartAudio;
 WaveTimer.enabled    := True;
 MIRenamePreset.Enabled := VSTHost[0].numPrograms >= 1;

 fPluginLoaded := True;
 fAllowed := True;
 Caption := fTitle;
 Left := Screen.Width div 2 - Width div 2;
 Top := Screen.Height div 2 - Height div 2;
 VSTHost[0].SetProgram(prog);
 FmOptions.SbTempoChange(nil);
 Sleep(50);
end;

procedure TFmMiniHost.VSTHostAudioMasterIdle(Sender: TVSTPlugin);
begin
 Sender.Idle;
end;

procedure TFmMiniHost.VSTHostAudioMasterNeedIdle(Sender: TVSTPlugin);
begin
 Sender.EditIdle;
end;

procedure TFmMiniHost.MIDIInChange(Sender: TObject);
begin
 if MidiInput.Devices.Count = 0
  then exit;
 MidiInput.OnMidiData := MidiData;
 (Sender as TMenuItem).Checked := True;
 try
  MidiInput.Close(fCurrentMidiIn);
 except
 end;
 fCurrentMidiIn := (sender as TMenuItem).tag;
 MIMIDIIn.Items[fCurrentMidiIn].Checked := True;
 try
  if fCurrentMidiIn > 0 then
   MidiInput.Open(fCurrentMidiIn - 1);
 except
 end;
end;

procedure TFmMiniHost.MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: Byte);
begin
 if aStatus = $FE then exit; // ignore active sensing
 if (not Player.CbOnlyChannel1.checked) or ((aStatus and $0F) = 0) then
  begin
   if (aStatus and $F0) = $90
    then NoteOn(aStatus, aData1, aData2) //ok
    else
   if (aStatus and $F0) = $80
    then NoteOff(aStatus, aData1)
    else AddMidiData(aStatus, aData1, aData2);
  end;
end;

procedure TFmMiniHost.SysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
begin
 // yet ToDo...
 if fMDataCnt > 2046 then exit;
 inc(fMDataCnt);
 with PVstMidiSysexEvent(fMyEvents.events[fMDataCnt - 1])^ do
  begin
   EventType := etSysEx;
   ByteSize := 24;
   DeltaFrames := 0;
   Flags := 0;
   dumpBytes := aStream.Size;
   sysexDump := aStream.Memory;
   resvd1 := nil;
   resvd2 := nil;
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
 MIStopRecordingClick(nil);
 if VSTHost[0].Active
  then VSTHost[0].SetSampleRate(ASIOHost.SampleRate);
 VSTHost.VstTimeInfo.SampleRate := ASIOHost.SampleRate;
 WaveFile.samplerate := ASIOHost.SampleRate;
end;

procedure TFmMiniHost.SetChannel(Sender: TObject);
begin
 (Sender as TMenuItem).checked := True;
 fCurrentOutputChannel := (Sender as TMenuItem).tag;
 if ASIOHost.Active then
 begin
  StopAudio;
  ASIOHost.OutputChannelOffset := fCurrentOutputChannel * 2;
  StartAudio;
 end else
 begin
  fProcessing := False;
  ASIOHost.OutputChannelOffset := fCurrentOutputChannel * 2;
 end;
 FmOptions.LbOutputs.Caption := 'Outputs: ' + MIASIOOutputChannel.Items[fCurrentOutputChannel].Caption;
end;

procedure TFmMiniHost.ASIOChange(Sender: TObject);
var i, j: Integer;
    m: TMenuItem;
begin
 assert(Sender is TMenuItem);
 (Sender as TMenuItem).Checked := True;
 fProcessing := False;
 MIPanicClick(nil);
 MidiFile.StopPlaying;
 MidiPlaying := False;
 StopPlayback2Click(nil);
 StopAudio;
 fCurrentASIO := (Sender as TMenuItem).tag;
 if fCurrentASIO >= 0 then
  begin
   ASIOHost.DriverIndex := fCurrentASIO;
   for i := 0 to MIASIOOutputChannel.Count - 1 do MIASIOOutputChannel.Delete(0);
   for i := 0 to MIASIOInputChannel.Count - 1 do MIASIOInputChannel.Delete(0);
   j := 0;
   for i := 0 to ASIOHost.OutputChannelCount - 1 do
    if not odd(i) then
    begin
     m := TMenuItem.Create(self);
     m.RadioItem := True;
     m.tag := j;
     inc(j);
     m.OnClick := SetChannel;
     if i < ASIOHost.OutputChannelCount - 1 then
      m.Caption :=
       ASIOHost.OutputChannelInfos[i].name + ' / ' +
       ASIOHost.OutputChannelInfos[i + 1].name
     else
      m.Caption :=
       ASIOHost.OutputChannelInfos[i].name;
     MIASIOOutputChannel.Add(m);
    end;

   m := TMenuItem.Create(self);
   m.RadioItem := True;
   m.tag := 0;
   m.OnClick := SetChannelI;
   m.Caption := 'None';
   MIASIOInputChannel.Add(m);
   j := 1;
   for i := 0 to ASIOHost.InputChannelCount - 1 do
    if not odd(i) then
     begin
      m := TMenuItem.Create(self);
      m.RadioItem := True;
      m.tag := j;
      inc(j);
      m.OnClick := SetChannelI;
      if i < ASIOHost.InputChannelCount - 1 then
       m.Caption :=
        ASIOHost.InputChannelInfos[i].name
         + ' / ' + ASIOHost.InputChannelInfos[i + 1].name
      else
       m.Caption :=
        ASIOHost.InputChannelInfos[i].name;
      MIASIOInputChannel.Add(m);
     end;

   MIASIOInputChannel.Items[0].Click;
   if ASIOHost.OutputChannelCount > 0
    then MIASIOOutputChannel.Items[0].Click;
  end;
 FmOptions.LbASIODriver.Caption := 'ASIO Driver: ' + ASIOHost.DriverName;
 if MIASIOOutputChannel.Count > 0 then
  FmOptions.LbOutputs.Caption := 'Outputs: ' + MIASIOOutputChannel.Items[0].Caption
 else
  FmOptions.LbOutputs.Caption := 'Outputs: None';
 if MIASIOInputChannel.Count > 0 then
  FmOptions.LbInputs.Caption := 'Inputs: ' + MIASIOInputChannel.Items[0].Caption
 else
  FmOptions.LbInputs.Caption := 'Inputs: None';
 if ASIOHost.OutputChannelCount > 0 then
  FmOptions.LbFormat.Caption := 'Format: ' + inttostr(ASIOHost.OutputChannelInfos[0].vType) + ' ' + ChannelTypeToString(ASIOHost.OutputChannelInfos[0].vType)
 else
  FmOptions.LbFormat.Caption := 'Format: None';
 FmOptions.LbBufferSize.Caption := 'Buffersize: ' + inttostr(ASIOHost.BufferSize);
 FmOptions.LbSampleRate.Caption := 'Samplerate: ' + inttostr(round(ASIOHost.SampleRate));

 ASIOHostReset(Sender);
 StartAudio;
 fProcessing := True;
end;

procedure TFmMiniHost.MIPanicClick(Sender: TObject);
var
  Ch, Note: word;
begin
 fMDataCnt := 0;
 for Note := 0 to 127 do AddMidiData($80, Note, 0);
 for Ch := 0 to 15 do AddMidiData($B0 + Ch, 123, 0);
end;

procedure TFmMiniHost.MILoadPresetClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   sleep(2);
   Filename := '*.fxp';
   InitialDir := fDirPreset;
   DefaultExt := '.fxp';
   Options := [ofAllowMultiSelect, ofFileMustExist, ofForceShowHidden];
   Ctl3D := False;
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   if Execute then
    begin
     fDirPreset := extractfiledir(filename);
     LoadPresets(Files);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.LoadPresets(Files: TStrings);
var i, j, k: Integer;
    s: string;
begin
 MIPanicClick(nil);
 WaveTimer.Enabled := False;
 j := fCurProg;
 for i := 0 to Files.Count - 1 do
 begin
  if i > 0 then VSTHost[0].SetProgram(j + i);
  try
   VSTHost[0].LoadPreset(Files[i]);
  except
{$IFNDEF FPC}
   msg('Preset file not for this plugin (or file is corrupted)!');
{$ENDIF}
   WaveTimer.Enabled := True;
   exit;
  end;
  k := VSTHost[0].GetProgram;
  s := IntToStr(k);
  if k < 10 then s := '00' + s else
  if k < 100 then s := '0' + s;
 end;
 WaveTimer.Enabled := True;
end;

procedure TFmMiniHost.MISavePresetClick(Sender: TObject);
var s2: string;
begin
 MIPanicClick(nil);
 with TSaveDialog.Create(Self) do
  try
   sleep(2);
   DefaultExt := '.fxp';
   filename := '*.fxp';
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   InitialDir := fDirPreset;
   Options := [ofForceShowHidden];
   Ctl3D := False;

   s2 := PresetBox.Items[PresetBox.ItemIndex];
   s2 := copy(s2, 6, length(s2) - 5);
{$IFNDEF FPC}
   Filename := MakeGoodFileName(s2) + '.fxp';
{$ENDIF}

   if Execute then
    begin
     VSTHost[0].SavePreset(FileName);
     fDirPreset := extractfiledir(filename);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MILoadBankClick(Sender: TObject);
begin
 WaveTimer.Enabled := False;
 with TOpenDialog.Create(Self) do
  try
   sleep(2);
   DefaultExt := '.fxb';
   filename := '*.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := fDirPreset;

   Options := [ofFileMustExist, ofForceShowHidden];
   Ctl3D := False;

   if Execute then
    begin
     fDirPreset := ExtractFileDir(filename);
     try
      VSTHost[0].LoadBank(Filename);
     except
{$IFNDEF FPC}
      msg('Bank file not for this plugin (or file is corrupted)!');
{$ENDIF}
      WaveTimer.Enabled := True;
     end;
     BuildPresetList;
    end;
  finally
   Free;
   fCurProg := 0;
   VSTHost[0].SetProgram(0);
   PresetBox.ItemIndex := 0;
   WaveTimer.Enabled := True;
  end;
end;

procedure TFmMiniHost.MISaveBankClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   sleep(2);
   FileName := '*.fxb';
   DefaultExt := '.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := fDirPreset;
   Options := [ofForceShowHidden];
   Ctl3D := False;
   if Execute then
    begin
     fDirPreset := ExtractFileDir(filename);
     VSTHost[0].SaveBank(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MIVSTClosePluginClick(Sender: TObject);
begin
 WaveFile.stop;
 ClosePlugin;
end;

procedure TFmMiniHost.MIVSTLoadPluginClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   sleep(2);
   DefaultExt := '.dll';
   filename := '*.dll';
   Filter := 'VST Plugins (*.dll)|*.dll';
   Options := [ofFileMustExist, ofForceShowHidden];
   Ctl3D := False;
   Title := 'Select a VST plugin';
   InitialDir := fDirPlugin;
   if Execute then
   begin
    fDirPlugin := ExtractFileDir(Filename);
    LoadPlugin(FileName);
   end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.LoadWAVFile;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filename := '*.wav;*.wpl';
   Filter := 'WAV files and playlists (*.wav;*.wpl)|*.wav;*.wpl|WAV files (*.wav)|*.wav|WAV playlists (*.wpl)|*.wpl';
   FilterIndex := 0;
   InitialDir := fDirWave;
   Options := [ofFileMustExist, ofForceShowHidden];
   Ctl3D := False;
   Title := 'Select a WAV file';
   if Execute then
   begin
    fDirWave := ExtractFileDir(Filename);
    AddWAV(FileName);
   end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.StartPlayback2Click(Sender: TObject);
begin
 if not FileExists(Wavefile.filename) then exit;
 WaveFile.play;
end;

procedure TFmMiniHost.SetPreset(Sender: TObject);
begin
 MIPanicClick(nil);
 VSTHost[0].SetProgram((sender as TMenuItem).Tag);
end;

procedure TFmMiniHost.FormShow(Sender: TObject);
var i: Integer;
begin
 with TIniFile.Create(ININame) do
  try
   MIUseMouseWheel.checked := ReadBool('VST', 'UseMouseWheel', True);
   Player.CbOnlyChannel1.Checked := ReadBool('MIDI', 'MidiFileOnlyChannel1', False);
   FmOptions.SbOverallVolume.position := ReadInteger('Audio', 'Overall Volume', 100);
   FmOptions.SbVSTVolume.position := ReadInteger('Audio', 'VST Volume', 100);
   FmOptions.SbInputVolume.position := ReadInteger('Audio', 'Input Volume', 100);
   FmOptions.SbWavVolume.position := ReadInteger('Audio', 'WAV Volume', 100);
   FmOptions.SbTempo.Position := ReadInteger('VST', 'Tempo', 120);

   FmOptions.Left := ReadInteger('Layout', 'SettingsWindow X', Left - 100);
   FmOptions.Top := ReadInteger('Layout', 'SettingsWindow Y', Top);
   if FmOptions.Left < 0 then FmOptions.Left := 0;
   if FmOptions.Top < 0 then FmOptions.Top := 0;
   if FmOptions.Left > Screen.Width - 20 then FmOptions.Left := Screen.Width - 20;
   if FmOptions.Top > Screen.Height - 20 then FmOptions.Top := Screen.Height - 20;

   Player.Left := ReadInteger('Layout', 'PlayerWindow X', Left - 100);
   Player.Top := ReadInteger('Layout', 'PlayerWindow Y', Top);
   if Player.Left < 0 then Player.Left := 0;
   if Player.Top  < 0 then Player.Top := 0;
   if Player.Left > Screen.Width  - 20 then Player.Left := Screen.Width  - 20;
   if Player.Top  > Screen.Height - 20 then Player.Top  := Screen.Height - 20;
   FmOptions.SbTempoChange(nil);
   if ReadBool('Layout', 'SettingsWindow Visible', False) then
    begin
     FmOptions.Show;
     FmOptions.setfocus;
    end;
   if ReadBool('Layout', 'PlayerWindow Visible', False) then
    begin
     Player.Show;
     Player.SetFocus;
    end;

   MIAlwaysOnTop.Checked := not ReadBool('Layout', 'AlwaysOnTop', False);
   MIAlwaysOnTopClick(Sender);
   i := ReadInteger('MIDI', 'MIDI-In Driver', 0);
   if (i < 0) or (i > MidiInput.Devices.Count) then i := 0;
   fCurrentMidiIn := i;
   MIMIDIIn.Items[i].Click;
   i := ReadInteger('MIDI', 'MIDI-Out Driver', 0);
   if (i < 0) or (i > MidiOutput.Devices.Count) then i := 0;
   fCurrentMidiOut := i;
   MIMIDIOut.Items[i].Click;
  finally
   Free;
  end;
 WaveTimer.Enabled := True;
 if fLoadProg >=0 then
  begin
   VSTHost[0].SetProgram(fLoadProg);
   fLoadProg := -1;
  end;
 if PnStatus.Visible then PnStatus.SetFocus;
end;

procedure TFmMiniHost.PluginResize(Sender: TObject);
begin
 if not (effFlagsHasEditor in VSTHost[0].EffectOptions) then
 begin
  fPanel.Width := 700;
  if (VSTHost[0].DLLFileName = '')
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
 ClientHeight := fPanel.Height + PnStatus.Height;
end;

procedure TFmMiniHost.MyMidiEvent(event: PMidiEvent);
begin
 with event^ do
  if (event and $F0) = $90 then NoteOn(event, data1, data2) else
  if (event and $F0) = $80 then NoteOff(event, data1)
   else AddMidiData(event, data1, data2);
end;

procedure TFmMiniHost.StartPlayback1Click(Sender: TObject);
begin
 MIPanicClick(nil);
 MidiFile.StartPlaying;
 MidiPlaying := True;
end;

procedure TFmMiniHost.RecordWAVFileSelect;
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   InitialDir := fDirWave;
   filename := '*.wav';
   Filter := 'WAV files (*.wav)|*.wav';
   Title := 'Select a WAV file';
   Options := [ofForceShowHidden];
   Ctl3D := False;
   if Execute then
    begin
     fDirWave := extractfiledir(filename);
     Player.LbRecordFile.Caption := filename;
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MIStartRecordingClick(Sender: TObject);
var s: string;
    i: Integer;
begin
 if Assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;

 case Player.CbRecordFormat.ItemIndex of
    0 : i := 16;
   else i := 32;
 end;
 s := Player.LbRecordFile.Caption;
 if s = '<none>' then
 begin
  RecordWavFileSelect;
  s := Player.LbRecordFile.Caption;
  if (s = '<none>') or (s = '') then exit;
 end;
 fTotalFrames := 0;
 if Player.CbRecInMono.Checked
  then WavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 1, i)
  else WavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 2, i);
 fRecordState := rsRecord;
end;

procedure TFmMiniHost.MIDIOutChange(Sender: TObject);
begin
 (Sender as TMenuItem).checked := True;
 try
  MidiOutput.Close(fCurrentMidiOut);
 except
 end;
 fCurrentMidiOut := (sender as TMenuItem).tag;
 MIMidiOut.Items[fCurrentMidiOut].Checked := True;
 try
  if fCurrentMidiOut > 0 then
   MidiOutput.Open(fCurrentMidiOut - 1);
 except
 end;
end;

procedure TFmMiniHost.WaveTimerTimer(Sender: TObject);
var s2, s: string;
    i: Integer;
    e: single;
begin
 if WaveFile.fPMode > wpmPause then
 begin
  i := round(100 * WaveFile.fCnt2 / (WaveFile.Size-2));
  Player.LbWavPosition.caption := 'position: ' + IntToStr(i) +' %';
  Player.SbWavPosition.position := i;
 end;

 BorderOnOff.Visible := fProcessing;
 BorderOptions.Visible := FmOptions.Showing;
 BorderPlayMIDI.Visible := midiplaying;
 BorderPlayWave.Visible := not (WaveFile.fPMode = wpmPause);
 BorderRecordWave.Visible := (fRecordState = rsRecord);

 case fRecordState of
  rsRecord: Player.LbStatus.caption := 'status: recording';
   rsPause: Player.LbStatus.caption := 'status: paused';
       else Player.LbStatus.caption := 'status: stopped';
 end;

 if fRecordState > rsStop then
 begin
  e := fTotalFrames / ASIOHost.SampleRate;
  Player.LbStatus.Caption :=
   Player.LbStatus.Caption + ' (time: '
   + FloatToStrF(e, ffFixed, 4, 2) + ' sec, size: ' + IntToStr(
    round(e * WavWriter.Format.nAvgBytesPerSec / 1000))
   + ' kbytes)';
 end;

 fDownMix := MIDownMixToStereo.Checked;

 if (MIDIPlaying) then
 begin
  i := round(100 * MidiFile.GetCurrentPos / MidiFile.GetTrackLength2);
  if i > 100 then i := 100 else if i < 0 then i := 0;
  Player.SbMidiPosition.position := i;

  if (MidiFile.Ready) then
  begin
   Player.SbMidiPosition.Position := 0;
   if Player.CBMidiPlayMode.ItemIndex = 1 then
   begin
    MIPanicClick(nil);
    MidiFile.StartPlaying;
   end else
   if (Player.CBMidiPlayMode.ItemIndex = 2) and (Player.MidiBox.Items.Count > 0) then
   begin
    Player.MidiBox.itemindex := (Player.MidiBox.itemindex + 1) mod Player.MidiBox.Items.Count;
    Player.BtMidiPlayClick(nil);
   end else
   if (Player.CBMidiPlayMode.ItemIndex = 3) and (Player.MidiBox.Items.Count > 0) then
   begin
    Player.MidiBox.itemindex := random(Player.MidiBox.Items.Count);
    Player.BtMidiPlayClick(nil);
   end else
    MIDIPlaying := False;
  end;
 end;

 if PresetBox.Items.Count = 0 then
  begin
   Caption := 'Delphi ASIO & VST Project -  MiniHost';
   exit;
  end;

 s := VSTHost[0].GetProgramName;
 i := VSTHost[0].GetProgram;
 if (fCurProg <> i) or (fCurProgName <> s) then
  begin
   fCurProg := i;
   fCurProgName := s;
   s := inttostr(fCurProg);
   if fCurProg < 10 then s := '00' + s else
   if fCurProg < 100 then s := '0' + s;
 if (PresetBox.items.Count > 0) and (fCurProg>=0) then
  begin
   PresetBox.Items[fCurProg] := s + ': ' + fCurProgName;
   PresetBox.ItemIndex := i;
  end;
  s2 := fTitle;
  if MIShowPreset.Checked then
   s2 := s2 + ' - ' + s + ': ' + fCurProgName;
  if caption <> s2 then caption := s2;
 end;
end;

procedure TFmMiniHost.MIASIOControlPanelClick(Sender: TObject);
begin
 StopAudio;
 ASIOHost.ControlPanel;
 ASIOHost.Reset;
 StartAudio;
end;

procedure TFmMiniHost.ProcessEvents(Sender: TObject; ev: PVstEvents);
var
  i: Integer;
  event: PVstMidiEvent;
  Sysex : PVstMidiSysexEvent;
  aStream: TMemoryStream;
begin
 if fCurrentMidiOut = 0 then exit;
 for i := 0 to ev^.numEvents - 1 do
  if (ev.events[i].EventType = etMidi) then
   begin
    event := PVstMidiEvent(ev^.events[i]);
    MidiOutput.Send(fCurrentMidiOut - 1, event^.mididata[0],
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
      MidiOutput.SendSysEx(fCurrentMidiOut - 1, aStream);
      aStream.Free;
     end;
   end;
end;

procedure TFmMiniHost.WMDropFiles(var msg: TMessage);
var
  size  : Integer;
  name  : PChar;
  fn, s : string;
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
   VSTHost[0].LoadPreset(s);
  except
{$IFNDEF FPC}
   DAVDCommon.msg('Preset file not for this plugin (or file is corrupted)!');
{$ENDIF}
   exit;
  end;
 end else
 if (fn = '.FXB') then
 begin
  try
   VSTHost[0].LoadBank(s);
  except
{$IFNDEF FPC}
   DAVDCommon.msg('Bank file not for this plugin (or file is corrupted)!');
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

procedure TFmMiniHost.MIRenamePresetClick(Sender: TObject);
var
  s2, s: string;
begin
 s := inputbox('Rename Preset', 'New name:', VSTHost[0].GetProgramName);
 VSTHost[0].SetProgramName(s);
 VSTHost[0].Idle;
 VSTHost[0].EditIdle;

 s2 := inttostr(fCurProg);
 if fCurProg < 10 then s2 := '00' + s2 else
 if fCurProg < 100 then s2 := '0' + s2;

 PresetBox.Items[fCurProg] := s2 + ': ' + s;
end;

procedure TFmMiniHost.LoadMIDIFile1Click(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.mid';
   InitialDir := fDirMidi;
   Options := [ofFileMustExist, ofForceShowHidden];
   Ctl3D := False;

   Filename := '*.mid;*.mpl';
   Filter := 'MIDI files and playlists (*.mid;*.mpl)|*.mid;*.mpl|MIDI files (*.mid)|*.mid|MIDI playlists (*.mpl)|*.mpl';
   FilterIndex := 0;
   Title := 'Select a MIDI file';
   if Execute then
    begin
     fDirMidi := ExtractFileDir(Filename);
     AddMID(Filename);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MISettingsClick(Sender: TObject);
begin
 FmOptions.Show;
end;

procedure TFmMiniHost.SetChannelI(Sender: TObject);
var
  f : Boolean;
begin
 (Sender as TMenuItem).checked := True;
 fCurrentInputChannel := (Sender as TMenuItem).Tag;
 f := ASIOHost.Active;
 if fCurrentInputChannel = 0
  then ASIOHost.InputChannelOffset := 0
  else ASIOHost.InputChannelOffset := (fCurrentInputChannel - 1) * 2;
 if f then StartAudio;
 FmOptions.LbInputs.Caption := 'Inputs: ' + MIASIOInputChannel.Items[fCurrentInputChannel].Caption;
end;

procedure TFmMiniHost.ASIOHostReset(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to length(fVSTBufOut) - 1
  do SetLength(fVSTBufOut[i], ASIOHost.BufferSize);
 for i := 0 to length(fVSTBufIn) - 1
  do SetLength(fVSTBufIn[i], ASIOHost.BufferSize);
 SetLength(fInBufL, ASIOHost.BufferSize);
 SetLength(fInBufR, ASIOHost.BufferSize);
 SetLength(fWavBufL, ASIOHost.BufferSize);
 SetLength(fWavBufR, ASIOHost.BufferSize);
 ASIOHostSampleRateChanged(Sender);
end;

procedure TFmMiniHost.ASIOHostDestroy(Sender: TObject);
var
  i : Integer;
begin
 fProcessing := False;
 SetLength(fInBufL, 0);
 SetLength(fInBufR, 0);
 SetLength(fWavBufL, 0);
 SetLength(fWavBufR, 0);
 fNumIn := 0;
 fNumOut := 0;
 for i := 0 to length(fVSTBufOut) - 1 do SetLength(fVSTBufOut[i], 0);
 for i := 0 to length(fVSTBufIn) - 1 do SetLength(fVSTBufIn[i], 0);
 SetLength(fVSTBufOut, 0);
 SetLength(fVSTBufIn, 0);
 SetLength(fVSTPinProps, 0);
end;

procedure TFmMiniHost.AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
begin
 if fMDataCnt > 2046 then exit;
 inc(fMDataCnt);
 with PVstMidiEvent(fMyEvents.events[fMDataCnt - 1])^ do
  begin
   EventType := etMidi;
   deltaFrames := pos;
   midiData[0] := d1;
   midiData[1] := d2;
   midiData[2] := d3;
  end;
end;

procedure TFmMiniHost.NoteOn(ch, note, v: byte);
begin
 if v = 0 then
  begin
   ch := ch - $10;
   NoteOff(ch, note);
   exit;
  end;
 if (note <= 127)
  then ProcessNoteOnOff(ch, note, v);
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
  end
 else
  begin // Note On
   if ch < $90 then ch := ch + $10;
   AddMidiData(ch, n, v);
  end;
end;

procedure TFmMiniHost.MIAboutClick(Sender: TObject);
begin
 FmAbout.ShowModal;
end;

procedure TFmMiniHost.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 MIPanicClick(nil);

 fAllowed := False;
 WaveFile.stop;
 WaveFile.Unload;
 MidiFile.StopPlaying;
 MidiPlaying := False;

 fRecordState := rsStop;
 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;
end;

procedure TFmMiniHost.MIShowPresetClick(Sender: TObject);
var
  s: string;
begin
 MIShowPreset.Checked := not MIShowPreset.Checked;
 s := inttostr(fCurProg);
 if fCurProg < 10 then s := '00' + s else
 if fCurProg < 100 then s := '0' + s;
 if MIShowPreset.Checked
  then Caption := fTitle + ' - ' + s + ': ' + fCurProgName
  else Caption := fTitle;
end;

procedure TFmMiniHost.StopPlayback1Click(Sender: TObject);
begin
 MidiFile.StopPlaying;
 MidiPlaying := False;
 MIPanicClick(nil);
end;

procedure TFmMiniHost.StopPlayback2Click(Sender: TObject);
begin
 WaveFile.stop;
end;

procedure TFmMiniHost.MIStopRecordingClick(Sender: TObject);
begin
 fRecordState := rsStop;
 if assigned(WavWriter) then
 begin
  WavWriter.free;
  WavWriter := nil;
 end;
end;

procedure TFmMiniHost.RenameF1Click(Sender: TObject);
begin
 MIRenamePresetClick(nil);
end;

procedure TFmMiniHost.F3PlayStopMIDI1Click(Sender: TObject);
begin
 with Player do
 if MidiBox.ItemIndex >= 0
  then Player.LbMidiFile.Caption := MidiBox.items[MidiBox.itemindex]
  else Exit;
  
 if MidiPlaying then
  begin
   MidiFile.StopPlaying;
   MidiPlaying := False;
   MIPanicClick(nil);
  end
 else
  begin
   MIPanicClick(nil);
   MIDIFile.StartPlaying;
   MidiPlaying := True;
  end;
end;

procedure TFmMiniHost.F4PlayStopWAV1Click(Sender: TObject);
begin
 with Player do
 if WavBox.Items.Count > 0 then
 if WavBox.ItemIndex >= 0 then
  begin
   Player.LbWaveFile.Caption := WavBox.items[WavBox.itemindex];
   if Wavefile.fPMode = wpmPlay
    then StopPlayback2Click(nil)
    else StartPlayback2Click(nil);
  end;
end;

procedure TFmMiniHost.F5RecStopWAV1Click(Sender: TObject);
begin
 if fRecordState >= rsRecord then MIStopRecordingClick(nil)
 else if fRecordState = rsStop then MIStartRecordingClick(nil);
end;

procedure TFmMiniHost.F11MIDIPanic1Click(Sender: TObject);
begin
 MIPanicClick(nil);
end;

{ TWavPlayer }

constructor TWavPlayer.Create;
begin
 pbuf := nil;
 pf := nil;
 fCnt := 0;
 fCnt2 := 0;
 fPMode := wpmPause;
 fVol := 1;
 fPan := 0.5;
 fSpeed := 1;
 fInterpol := False;
end;

destructor TWavPlayer.Destroy;
begin
 if assigned(pbuf) then freemem(pbuf);
 inherited;
end;

procedure TWavPlayer.Load(s: TFileName);
begin
 fPMode := wpmPause;
 if assigned(pbuf) then
  begin
   freemem(pbuf);
   pbuf := nil;
  end;
 Filemode := 0;
 if s <> '' then
  begin
 //  pbuf := LoadWAVFile(s,sr,ch,size);
  end;
 pf := pbuf;
 fCnt := 0;
 fCnt2 := 0;
 filename := s;
end;

procedure TWavPlayer.pause;
begin
 fPMode := wpmPause;
end;

procedure TWavPlayer.play;
begin
 fPMode := wpmPlay;
end;

procedure TWavPlayer.process(var o1, o2: single);
var
  next, next2, pp: PSingle;
begin
 if (not assigned(pf))   // if buffer is empty (no file loaded)
  or (fPmode = wpmPause) // or "play" not activated
 then begin              // then output silence
  o1 := 0;
  o2 := 0;
 end else
 begin
  o1 := pf^;
  if fCh = 2 then // stereo?
   begin
    pp := psingle(longint(pf) + 4);
    o2 := pp^;
    next := psingle(longint(pf) + 8);
    next2 := psingle(longint(pf) + 12);
    o2 := o2 * (1 - fCnt) + fCnt * next2^;
   end
  else
   begin
    next := psingle(longint(pf) + 4);
    o2 := o1;
   end;
  if (fCnt <1 ) and (fInterpol) then // interpolation?
   o1 := o1 * (1 - fCnt) + fCnt * next^; // get next sample

  fCnt := fCnt + speed * (fSR / samplerate);
  while (fCnt >= 1) do
   begin
    inc(pf, fCh);
    fCnt := fCnt - 1;
    inc(fCnt2, fCh);
   end;
  if (fCnt2 >= fSize - 1) then
   begin
    if not looped then
     begin
      fPMode := wpmPause;
      Player.SbWavPosition.position := 0;
      if (Player.CBWavPlayMode.ItemIndex = 2) and (Player.WavBox.Items.Count > 0) then
       begin
        Player.WavBox.ItemIndex := (Player.WavBox.ItemIndex + 1) mod Player.WavBox.Items.Count;
        Player.BtWavPlayClick(nil);
       end else
      if (Player.CBWavPlayMode.ItemIndex = 3) and (Player.WavBox.Items.Count > 0) then
       begin
        Player.WavBox.itemindex := random(Player.WavBox.Items.Count);
        Player.BtWavPlayClick(nil);
       end;
     end;
    fCnt2 := 0;
    fCnt := 0;
    pf := pbuf;
   end;
 end;

 if fCh = 2 then // stereo output
  begin
   o1 := fVol * o1;
   o2 := fVol * o2;
  end
 else
  begin // mono output
   o1 := fVol * o1 * 2 * (1 - fPan);
   o2 := fVol * o1 * 2 * fPan;
  end;
end;

procedure TWavPlayer.stop;
begin
 fPMode := wpmPause;
 fCnt2 := 0;
 fCnt := 0;
 pf := psingle(longint(pbuf) + fCnt2 * SizeOf(single));
end;

procedure TWavPlayer.SetPos(i: Integer);
begin
 fCnt2 := i;
 fCnt := 0;
 pf := psingle(longint(pbuf) + fCnt2 * SizeOf(single));
end;

procedure TWavPlayer.unload;
begin
 fPMode := wpmPause;
 if assigned(pbuf) then
 begin
  freemem(pbuf);
  pbuf := nil;
 end;
 pf := pbuf;
 fCnt := 0;
 fCnt2 := 0;
end;

procedure TFmMiniHost.MIAlwaysOnTopClick(Sender: TObject);
begin
 MIAlwaysOnTop.checked := not MIAlwaysOnTop.checked;
{$IFNDEF FPC}
 if MIAlwaysOnTop.checked then
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
 if not MIUseMouseWheel.Checked then exit;
 MIPanicClick(nil);
 if fCurProg > 0 then
  VSTHost[0].SetProgram(fCurProg - 1);
end;

procedure TFmMiniHost.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if not MIUseMouseWheel.Checked then exit;
 MIPanicClick(nil);
 if fCurProg + 1 < VSTHost[0].numPrograms then
  VSTHost[0].SetProgram(fCurProg + 1);
end;

procedure TFmMiniHost.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmMiniHost.IOnOffMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fProcessing := not fProcessing;
 MIPanicClick(nil);
end;

procedure TFmMiniHost.PresetBoxClick(Sender: TObject);
begin
 WaveTimer.Enabled := False;
 MIPanicClick(nil);
 VSTHost[0].SetProgram(PresetBox.ItemIndex);
 fCurProg := PresetBox.ItemIndex;
 WaveTimer.Enabled := True;
end;

procedure TFmMiniHost.PresetBoxKeyPress(Sender: TObject; var Key: Char);
begin
 key := #0;
end;

procedure TFmMiniHost.PresetBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 if Index < 0 then exit;
 PresetBox.Canvas.FillRect(Rect);
 PresetBox.Canvas.TextOut(rect.Left + 2, rect.top, PresetBox.items[index]);
end;

procedure TFmMiniHost.IBtLeftRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 MIPanicClick(nil);
 if x < IBtLeftRight.width shr 1 then
  if fCurProg > 0 then
   VSTHost[0].SetProgram(fCurProg - 1) else else
 if fCurProg + 1 < VSTHost[0].numPrograms then
   VSTHost[0].SetProgram(fCurProg + 1);
end;

procedure TFmMiniHost.IBtDropDownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 PresetBox.DroppedDown := not PresetBox.DroppedDown;
end;

procedure TFmMiniHost.PresetBoxChange(Sender: TObject);
begin
 PnStatus.SetFocus;
end;

procedure TFmMiniHost.IQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 with FmOptions do if Showing then Hide else Show;
end;

procedure TFmMiniHost.IQuickMidPlayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F3PlayStopMIDI1Click(Sender);
end;

procedure TFmMiniHost.IQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F4PlayStopWAV1Click(Sender);
end;

procedure TFmMiniHost.IQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F5RecStopWAV1Click(Sender);
end;

procedure TFmMiniHost.AddMID(s:string);
var
  j, i : Integer;
  ms   : PShortStr;
begin
 if UpperCase(ExtractFileExt(s)) = '.MPL' then
  begin
   {$IFDEF FPC}
   for I := 0 to Player.MidiBox.Items.Count - 1
    do Player.MidiBox.Selected[I] := True;
   {$ELSE}
   Player.MidiBox.SelectAll;
   {$ENDIF}
   Player.BtMidiAddClick(nil);
   with TStringList.Create do
    try
     LoadFromFile(s);
     for i := 0 to Count - 1 do AddMID(Strings[i]);
     if (Count > 0) and (uppercase(Strings[0]) = 'RANDOM') then
      Player.CBMidiPlayMode.ItemIndex := 3;
    finally
     Free
    end;
  end;
 if UpperCase(ExtractFileExt(s)) <> '.MID' then Exit;
 if not FileExists(s) then Exit;
 j := -1;
 for i := 0 to Player.MidiBox.Items.Count - 1 do
  if PShortstr(Player.MidiBox.Items.Objects[i])^ = s
   then j := 0;
 if j = 0 then Exit;
 GetMem(ms, SizeOf(shortstr));
 ms^ := s;
 Player.MidiBox.Items.AddObject(ExtractFilename(s), TObject(ms));
 Player.MidiBox.ItemIndex := Player.MidiBox.Items.Count - 1;
end;

procedure TFmMiniHost.AddWAV(s:string);
var
  j, i : Integer;
  ms   : PShortStr;
begin
 if UpperCase(ExtractFileExt(s))='.WPL' then
  begin
   {$IFDEF FPC}
   for I := 0 to Player.WavBox.Items.Count - 1
    do Player.WavBox.Selected[I] := True;
   {$ELSE}
   Player.WavBox.SelectAll;
   {$ENDIF}
   Player.BtWavAddClick(nil);
   with TStringList.Create do
    try
     LoadFromFile(s);
     for i := 0 to Count - 1 do AddWAV(Strings[i]);
     if (Count > 0) and (UpperCase(Strings[0]) = 'RANDOM')
      then Player.CBWavPlayMode.ItemIndex := 3;
    finally
     Free;
    end;
   if UpperCase(ExtractFileExt(s))<>'.WAV' then exit;
   if not FileExists(s) then Exit;
   j := -1;
   for i := 0 to Player.WavBox.Items.Count - 1 do
    if PShortstr(Player.WavBox.Items.Objects[i])^ = s then
     j := 0;
   if j = 0 then exit;
   GetMem(ms, SizeOf(ShortStr));
   ms^ := s;
   Player.WavBox.Items.AddObject(ExtractFilename(s), TObject(ms));
   Player.WavBox.ItemIndex := Player.WavBox.Items.Count - 1;
  end;
end;

procedure TFmMiniHost.BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Shift = [ssRight] then Player.Show;
end;

procedure TFmMiniHost.MIShowMIDIWAVWindowClick(Sender: TObject);
begin
 Player.Show;
end;

procedure TFmMiniHost.MIDownMixToStereoClick(Sender: TObject);
begin
 MIDownMixToStereo.checked := not MIDownMixToStereo.checked;
end;

procedure TFmMiniHost.MIMidiThruClick(Sender: TObject);
begin
 MIMidiThru.checked := not MIMidiThru.checked;
end;

procedure TFmMiniHost.MIUseMouseWheelClick(Sender: TObject);
begin
 MIUseMouseWheel.Checked := not MIUseMouseWheel.Checked;
end;

procedure TFmMiniHost.IdleTimerTimer(Sender: TObject);
begin
 VSTHost[0].Idle;
 VSTHost[0].EditIdle;
end;

procedure TFmMiniHost.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TAVDArrayOfSingleDynArray);
var
  j, i      : Integer;
  bs, ChOfs : Integer;
begin
 bs := ASIOHost.BufferSize;
 if (bs <= 0) or (not fAllowed) or (VSTHost = nil)
  then exit;

 VSTHost.UpdateVstTimeInfo(bs);
 MidiFile.MidiTimer(nil);

 if fMDataCnt > 0 then
  begin
   fMyEvents.numEvents := fMDataCnt;
   if VSTHost[0].CanDo('receiveVstMidiEvent') >= 0 then
    VSTHost[0].ProcessEvents(@fMyEvents);
   if (fCurrentMidiOut > 0) and MIMidiThru.Checked then
    begin
     for i := 0 to fMDataCnt - 1 do
      MidiOutput.Send(fCurrentMidiOut - 1,
                      PVstMidiEvent(fMyEvents.events[i])^.midiData[0],
                      PVstMidiEvent(fMyEvents.events[i])^.midiData[1],
                      PVstMidiEvent(fMyEvents.events[i])^.midiData[2]);
    end;
  end;

 ChOfs := ASIOHost.OutputChannelOffset;
 if fCurrentInputChannel = 0 then
  begin
   for i := 0 to bs - 1 do
    begin
     fInBufL[i] := 0;
     fInBufR[i] := 0;
    end;
  end
 else
  begin
   for i := 0 to bs - 1 do
    begin
     fInBufL[i] := InputVol * InBuffer[ASIOHost.InputChannelOffset    , i];
     fInBufR[i] := InputVol * InBuffer[ASIOHost.InputChannelOffset + 1, i];
    end;
  end;

 // fill WavBufL and WavBufR
 if Wavefile.fPMode = wpmPlay then
  begin
   for i := 0 to bs - 1 do
    WaveFile.process(fWavBufL[i], fWavBufR[i]);
  end
 else
  begin
   assert(Length(fWavBufL) >= bs);
   assert(Length(fWavBufR) >= bs);
   FillChar(fWavBufL[0], bs * SizeOf(Single), 0);
   FillChar(fWavBufR[0], bs * SizeOf(Single), 0);
  end;

 if fNumOut > 0 then
 begin
  // assign Input to VSTBufIn
  for i := 0 to fNumOut - 1 do FillChar(fVSTBufOut[i][0], bs * SizeOf(Single), 0);
  if effFlagsIsSynth in VSTHost[0].EffectOptions then
   for i := 0 to fNumIn - 1 do FillChar(fVSTBufIn[i][0], bs * SizeOf(Single), 0)
  else
   for i := 0 to bs - 1 do
    begin
     fVSTBufIn[0][i] := (fWavBufL[i] * Wavefile.Volume) + fInBufL[i];
     fVSTBufIn[1][i] := (fWavBufR[i] * Wavefile.Volume) + fInBufR[i];
    end;

  // apply Processing
  if fProcessing then
  begin
   if effFlagsCanReplacing in VSTHost[0].EffectOptions then
    VSTHost[0].ProcessReplacing(@fVSTBufIn[0], @fVSTBufOut[0], bs)
   else
    VSTHost[0].Process(@fVSTBufIn[0], @fVSTBufOut[0], bs);
   if fDownMix then
    for i := 0 to bs - 1 do
     for j := 2 to fNumOut - 1 do
     begin
      if fVSTPinProps[j].arrangementType = 0 then
      begin
       fVSTBufOut[0][i] := fVSTBufOut[0][i] + fVSTBufOut[j][i];
       fVSTBufOut[1][i] := fVSTBufOut[1][i] + fVSTBufOut[j][i];
      end else
       fVSTBufOut[j mod 2][i] := fVSTBufOut[j mod 2][i] + fVSTBufOut[j][i];
     end;
  end;

  // assign Output from VSTBufOut
  if fNumOut = 1 then j := 0 else j := 1;
  if effFlagsIsSynth in VSTHost[0].EffectOptions then
   for i := 0 to bs - 1 do
   begin
    OutBuffer[ChOfs][i] := (fVSTBufOut[0][i] * VSTVol + fInBufL[i] + fWavBufL[i] * Wavefile.Volume) * fOverallVol;
    OutBuffer[ChOfs + 1][i] := (fVSTBufOut[j][i] * VSTVol + fInBufR[i] + fWavBufR[i] * Wavefile.Volume) * fOverallVol;
   end
  else
   for i := 0 to bs - 1 do
   begin
    OutBuffer[ChOfs][i] := (fVSTBufOut[0][i] * VSTVol + (1 - VSTVol) * fVSTBufIn[0][i]) * fOverallVol;
    OutBuffer[ChOfs + 1][i] := (fVSTBufOut[j][i] * VSTVol + (1 - VSTVol) * fVSTBufIn[j][i]) * fOverallVol;
   end;
 end else
  for i := 0 to bs - 1 do
  begin
   OutBuffer[ChOfs][i] := (fInBufL[i] + fWavBufL[i] * Wavefile.Volume) * fOverallVol;
   OutBuffer[ChOfs + 1][i] := (fInBufR[i] + fWavBufR[i] * Wavefile.Volume) * fOverallVol;
  end;

 if fRecordState = rsRecord then
  begin
   fTotalFrames := fTotalFrames + integer(ASIOHost.buffersize);
   if wavwriter.Format.nChannels = 1 then
    WavWriter.WriteFloatData(OutBuffer[ChOfs], bs)
   else
    WavWriter.WriteFloatDataSeparateStereo(OutBuffer[ChOfs],
     OutBuffer[ChOfs + 1], bs);
  end;

 fMDataCnt := 0;
end;

{$IFDEF FPC}
initialization
  {$i MiniHostForm.lrs}
{$ENDIF}

end.
