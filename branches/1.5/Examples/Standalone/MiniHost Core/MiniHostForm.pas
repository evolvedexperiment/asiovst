unit MiniHostForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Types, Messages,
  Forms, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, SyncObjs,
  DAV_Types, DAV_VstEffect, DAV_MidiIO, DAV_ASIOHost,
  DAV_VSTHost, DAV_GuiCommon,
  WaveIOX, AboutForm;

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
    FBuffer: PDAVSingleFixedArray;
    FBufferPntr: PSingle;
    FLooped: Boolean;
    FInterpol: Boolean;
    FCnt2: Integer;
    FSize: Integer;
    FSR, FCh: Integer;
    FSpeed: Single;
    FVol, FPan: Single;
    FSamplerate: Single;
    FCnt: Double;
    FPMode: TWavPlayerMode;
    FFilename: TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPos(const Value: Integer);
    procedure Process(var o1, o2: single);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Load(const FileName: TFileName);
    procedure Unload;

    property LabelSampleRate: Single read FSamplerate write FSamplerate;
    property Speed: Single read FSpeed write FSpeed;
    property Size: Integer read FSize;
    property Volume: Single read FVol write FVol;
    property Looped: Boolean read FLooped write FLooped;
    property Interpolate: Boolean read FInterpol write FInterpol;
    property FileName: TFileName read FFilename write FFilename;
  end;

  MIDIData = record
    d1, d2, d3: byte;
    pos: Integer;
  end;

  { TFormMiniHost }

  TRecordState = (rsStop, rsRecord, rsPause);

  TFormMiniHost = class(TForm)
    VSTHost: TVSTHost;
    ASIOHost: TASIOHost;
    TimerWaveFile: TTimer;
    TimerIdle: TTimer;
    MainMenu: TMainMenu;
    MenuItemVST: TMenuItem;
    MenuItemASIO: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemVSTLoadPlugin: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemPreset: TMenuItem;
    MenuItemLoadPreset: TMenuItem;
    MenuItemLoadBank: TMenuItem;
    MenuItemSavePreset: TMenuItem;
    MenuItemSaveBank: TMenuItem;
    MenuItemAsioDriver: TMenuItem;
    MenuItemASIOControlPanel: TMenuItem;
    MenuItemASIOOutputChannel: TMenuItem;
    MenuItemRenamePreset: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemASIOInputChannel: TMenuItem;
    MenuItemVSTClosePlugin: TMenuItem;
    MenuItemDownMixToStereo: TMenuItem;
    MenuItemShowPreset: TMenuItem;
    MenuItemAlwaysOnTop: TMenuItem;
    MenuItemUseMouseWheel: TMenuItem;
    MenuItemMain: TMenuItem;
    MenuItemIExit: TMenuItem;
    MenuItemShowMIDIWAVWindow: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    PanelStatus: TPanel;
    PresetBox: TComboBox;
    ToolBarBackground: TImage;
    ImageLeftRight: TImage;
    ImageDropDown: TImage;
    ImageQuickSettings: TImage;
    ImageQuickWavPlay: TImage;
    ImageQuickWavRec: TImage;
    BorderPlayWave: TImage;
    BorderRecordWave: TImage;
    BorderOnOff: TImage;
    BorderOptions: TImage;
    MenuItemMIDI: TMenuItem;
    MenuItemMidiPanic: TMenuItem;
    N4: TMenuItem;
    MenuItemMidiOut: TMenuItem;
    MenuItemMidiIn: TMenuItem;
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
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure MenuItemPanicClick(Sender: TObject);
    procedure MenuItemLoadPresetClick(Sender: TObject);
    procedure MenuItemSavePresetClick(Sender: TObject);
    procedure MenuItemLoadBankClick(Sender: TObject);
    procedure MenuItemSaveBankClick(Sender: TObject);
    procedure MenuItemVSTClosePluginClick(Sender: TObject);
    procedure MenuItemVSTLoadPluginClick(Sender: TObject);
    procedure MenuItemASIOControlPanelClick(Sender: TObject);
    procedure MenuItemRenamePresetClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure MenuItemShowPresetClick(Sender: TObject);
    procedure MenuItemStartRecordingClick(Sender: TObject);
    procedure MenuItemStopRecordingClick(Sender: TObject);
    procedure MenuItemAlwaysOnTopClick(Sender: TObject);
    procedure MenuItemIExitClick(Sender: TObject);
    procedure MenuItemShowMIDIWAVWindowClick(Sender: TObject);
    procedure MenuItemDownMixToStereoClick(Sender: TObject);
    procedure MenuItemUseMouseWheelClick(Sender: TObject);
    procedure StartPlayback2Click(Sender: TObject);
    procedure StopPlayback1Click(Sender: TObject);
    procedure StopPlayback2Click(Sender: TObject);
    procedure TimerWaveFileTimer(Sender: TObject);
    procedure RenameF1Click(Sender: TObject);
    procedure F4PlayStopWAV1Click(Sender: TObject);
    procedure F5RecStopWAV1Click(Sender: TObject);
    procedure F11MIDIPanic1Click(Sender: TObject);
    procedure PresetBoxClick(Sender: TObject);
    procedure PresetBoxChange(Sender: TObject);
    procedure PresetBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PresetBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerIdleTimer(Sender: TObject);
    procedure ImageLeftRightMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageDropDownMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageOnOffMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDataSection: TCriticalSection;
    FRecordState: TRecordState;
    FDownMix: Boolean;
    FTotalFrames: Integer;
    FPanel: TPanel;
    FTitle: AnsiString;
    FWaveFile: TWavPlayer;
    FProcessing: Boolean;
    FAllowed: Boolean;
    FLoadProg: Integer;
    FDirPlugin,
    FDirPreset,
    FDirWave,
    FDirMidi: string;
    FPluginLoaded: Boolean;
    FWavBufL,
    FWavBufR,
    FInBufL,
    FInBufR: TDAVSingleDynArray;
    FVSTBufIn,
    FVSTBufOut: TDAVArrayOfSingleDynArray;
    FMIDIPlaying: Boolean;
    FMyEvents: TVstEvents;
    FOverallVol: Single;
    FVSTVol: Single;
    FInputVol: Single;
    FCurProg: Integer;
    FCurProgName: AnsiString;
    FVSTPinProps: array of TVstPinProperties;
    FNumIn,
    FNumOut: Integer;
    FColBack: Boolean;
    FAboutForm: TFormAbout;

    FMIDIInput: TMidiInput;
    FMIDIOutput: TMidiOutput;
    FWavWriter: TWavWriter;

    FCurrentASIO: Integer;
    FCurrentMIDIIn: Integer;
    FCurrentMIDIOut: Integer;
    FCurrentOutputChannel,
    FCurrentInputChannel: Integer;

    FMDataCnt: Integer;
    function FindBackgroundColor: TColor;
    procedure ASIOChange(Sender: TObject);
    procedure BuildChannelBuffers;
    procedure ClosePlugin;
    procedure MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: byte);
    procedure MIDIInChange(Sender: TObject);
    procedure MIDIOutChange(Sender: TObject);
    procedure MyMidiEvent(const Event: TVstEvent);
    procedure ProcessEvents(Sender: TObject; ev: PVstEvents);
    procedure ProcessNoteOnOff(ch, n, v: byte);
    procedure SetChannel(Sender: TObject);
    procedure SetChannelI(Sender: TObject);
    procedure SetPreset(Sender: TObject);
    procedure StopProcessingAndClosePlugin;
    procedure SysExData(const aDeviceIndex: Integer; const aStream: TMemoryStream);
  protected
    procedure ShowVSTPlugin(const DefaultProgram: Integer = 0);
  public
    procedure AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
    procedure AddWAV(const FileName: string);
    procedure BuildPresetList;
    procedure LoadPlugin(const VSTDll: TFileName; const DefaultProgram: Integer = 0); overload;
    procedure LoadPresets(Files: TStrings);
    procedure LoadWAV(const FileName: string);
    procedure LoadWAVFile;
    procedure NoteOff(ch, note: Byte);
    procedure NoteOn(ch, note, v: Byte);
    procedure RecordWAVFileSelect;
    procedure StartAudio;
    procedure StopAudio;
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
  published
    property CurrentProgram: Integer read FCurProg;
    property CurrentProgramName: AnsiString read FCurProgName;
    property InputVol: Single read FInputVol write FInputVol;
    property MidiPlaying: Boolean read FMIDIPlaying write FMIDIPlaying;
    property OverallVolume: Single read FOverallVol write FOverallVol;
    property RecordState: TRecordState read FRecordState write FRecordState;
    property VSTVol: Single read FVSTVol write FVSTVol;
    property WaveFile: TWavPlayer read FWaveFile;
  end;

var
  FormMiniHost: TFormMiniHost;
  IniName: string;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, Inifiles, Dialogs, ShellAPI, DAV_Common, DAV_AudioData,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_XPlatform,
  OptionsForm, PlayerForm;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;


{ TWavPlayer }

constructor TWavPlayer.Create;
begin
  FBuffer := nil;
  FBufferPntr := nil;
  FCnt := 0;
  FCnt2 := 0;
  FPMode := wpmPause;
  FVol := 1;
  FPan := 0.5;
  FSpeed := 1;
  FInterpol := False;
end;

destructor TWavPlayer.Destroy;
begin
  if Assigned(FBuffer) then
    Dispose(FBuffer);

  inherited;
end;

procedure TWavPlayer.Load(const FileName: TFileName);
begin
  FPMode := wpmPause;
  if Assigned(FBuffer) then
  begin
    Dispose(FBuffer);
    FBuffer := nil;
  end;
  Filemode := 0;
  if FileName <> '' then
  begin
    // FBuffer := LoadWAVFile(s,sr,ch,size);
  end;
  FBufferPntr := @FBuffer^[0];
  FCnt := 0;
  FCnt2 := 0;
  FFilename := FileName;
end;

procedure TWavPlayer.Pause;
begin
  FPMode := wpmPause;
end;

procedure TWavPlayer.Play;
begin
  FPMode := wpmPlay;
end;

procedure TWavPlayer.Process(var o1, o2: single);
var
  next, next2, pp: PSingle;
begin
  if (not Assigned(FBufferPntr))   // if buffer is empty (no file loaded)
    or (FPMode = wpmPause) then// or "play" not activated
  begin              // then output silence
    o1 := 0;
    o2 := 0;
  end
  else
  begin
    o1 := FBufferPntr^;
    if FCh = 2 then // stereo?
    begin
      pp := PSingle(LongInt(FBufferPntr) + 4);
      o2 := pp^;
      next := PSingle(LongInt(FBufferPntr) + 8);
      next2 := PSingle(LongInt(FBufferPntr) + 12);
      o2 := o2 * (1 - FCnt) + FCnt * next2^;
    end
    else
    begin
      next := PSingle(longint(FBufferPntr) + 4);
      o2 := o1;
    end;
    if (FCnt <1 ) and (FInterpol) then // interpolation?
      o1 := o1 * (1 - FCnt) + FCnt * next^; // get next sample

    FCnt := FCnt + speed * (FSR / LabelSampleRate);
    while (FCnt >= 1) do
    begin
      Inc(FBufferPntr, FCh);
      FCnt := FCnt - 1;
      Inc(FCnt2, FCh);
    end;
    if (FCnt2 >= FSize - 1) then
    begin
      if not looped then
      begin
        FPMode := wpmPause;
        Player.ScrollBarWavPosition.Position := 0;
        if (Player.ComboBoxWavPlayMode.ItemIndex = 2) and (Player.ListBoxWavFiles.Items.Count > 0) then
        begin
          Player.ListBoxWavFiles.ItemIndex := (Player.ListBoxWavFiles.ItemIndex + 1) mod Player.ListBoxWavFiles.Items.Count;
          Player.ButtonWavPlayClick(nil);
        end
        else
        if (Player.ComboBoxWavPlayMode.ItemIndex = 3) and (Player.ListBoxWavFiles.Items.Count > 0) then
        begin
          Player.ListBoxWavFiles.ItemIndex := Random(Player.ListBoxWavFiles.Items.Count);
          Player.ButtonWavPlayClick(nil);
        end;
      end;

      FCnt2 := 0;
      FCnt := 0;
      FBufferPntr := @FBuffer^[0];
    end;
  end;

  if FCh = 2 then // stereo output
  begin
    o1 := FVol * o1;
    o2 := FVol * o2;
  end
  else
  begin // mono output
    o1 := FVol * o1 * 2 * (1 - FPan);
    o2 := FVol * o1 * 2 * FPan;
  end;
end;

procedure TWavPlayer.Stop;
begin
  FPMode := wpmPause;
  FCnt2 := 0;
  FCnt := 0;
  FBufferPntr := @FBuffer^[FCnt2];
end;

procedure TWavPlayer.SetPos(const Value: Integer);
begin
  FCnt2 := Value;
  FCnt := 0;
  FBufferPntr := @FBuffer^[FCnt2];
end;

procedure TWavPlayer.Unload;
begin
  FPMode := wpmPause;
  if Assigned(FBuffer) then
  begin
    Dispose(FBuffer);
    FBuffer := nil;
  end;
  FBufferPntr := @FBuffer^[0];
  FCnt := 0;
  FCnt2 := 0;
end;


{ TFormMiniHost }

procedure TFormMiniHost.FormCreate(Sender: TObject);
var
  i, mi: Integer;
  MenuItem: TMenuItem;
  PlayList: TStringList;
  AsioDriverList: TStrings;
  str: string;
  Settings: TIniFile;
  ContainedVSTPlugins: TStringList;
  RS: TResourceStream;
begin
  FDataSection := TCriticalSection.Create;

  FAllowed := False;
  with ToolBarBackground.Picture do
    Bitmap.TransparentColor := $A8A8A8;
  ImageDropDown.picture.Bitmap.TransparentColor := $A8A8A8;
  ImageDropDown.picture.Bitmap.Transparent := True;
  BorderOnOff.picture.Bitmap.TransparentColor := clBlack;
  BorderOnOff.picture.Bitmap.Transparent := False;
  BorderOptions.Picture.Assign(BorderOnOff.Picture);
  BorderPlayWave.Picture.Assign(BorderOnOff.Picture);
  BorderRecordWave.Picture.Assign(BorderOnOff.Picture);

  Assert(SizeOf(TVSTMidiEvent) = SizeOf(TVstMidiSysexEvent));

  for i := 0 to 2047 do
  begin
    GetMem(FMyEvents.Events[i], SizeOf(TVSTMidiEvent));
    FillChar(FMyEvents.Events[i]^, SizeOf(TVSTMidiEvent), 0);
    with PVstMidiEvent(FMyEvents.Events[i])^ do
    begin
      EventType := etMidi;
      ByteSize := 24;
    end;
  end;

  FMIDIInput  := TMidiInput.Create;
  FMIDIOutput := TMidiOutput.Create;

  FWaveFile := TWavPlayer.Create;
  FPluginLoaded := False;

  Player := TPlayer.Create(Self);

  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Parent := Self;
    Top := PanelStatus.Height;
  end;

  ClientHeight := PanelStatus.Height;
  FormOptions := TFormOptions.Create(Self);
  FormOptions.Host := Self;

  DragAcceptFiles(Self.Handle, True);

{$IFNDEF FPC}
  IniName := GetApplicationDirectory + '\' + ChangeFileExt(GetApplicationFilename, '.ini');
{$ENDIF}

  FRecordState := rsStop;

  try
    MenuItem := TMenuItem.Create(Self);
    MenuItem.RadioItem := True;
    MenuItem.Tag := 0;
    MenuItem.Caption := 'None';
    MenuItem.OnClick := MIDIInChange;
    MenuItemMidiIn.Add(MenuItem);
    for mi := 0 to FMidiInput.Devices.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.RadioItem := True;
      MenuItem.Tag := mi + 1;
      MenuItem.Caption := FMidiInput.Devices[mi];
      MenuItem.OnClick := MIDIInChange;
      MenuItemMidiIn.Add(MenuItem);
    end;
  except
    MessageDlg('ERROR: A serious problem occured with MIDI-In drivers!', mtError, [mbOK], 0);
  end;

  try
    MenuItem := TMenuItem.Create(Self);
    MenuItem.RadioItem := True;
    MenuItem.Tag := 0;
    MenuItem.Caption := 'None';
    MenuItem.OnClick := MIDIOutChange;
    MenuItemMidiOut.Add(MenuItem);
    for mi := 0 to FMidiOutput.Devices.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.RadioItem := True;
      MenuItem.Tag := mi + 1;
      MenuItem.Caption := FMidiOutput.Devices[mi];
      MenuItem.OnClick := MIDIOutChange;
      MenuItemMidiOut.Add(MenuItem);
    end;
  except
    MessageDlg('ERROR: A serious problem occured with MIDI-Out drivers', mtError, [mbOK], 0);
  end;

  try
    AsioDriverList := ASIOHost.DriverList;
  except
    AsioDriverList := nil;
    MessageDlg('ERROR: ASIO driver list could not be received! Application Terminated!', mtError, [mbOK], 0);
    Application.Terminate;
  end;

  if AsioDriverList <> nil then
    for i := 0 to AsioDriverList.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.RadioItem := True;
      MenuItem.Tag := i;
      MenuItem.Caption := AsioDriverList.Strings[i];
      MenuItem.OnClick := ASIOChange;
      MenuItemAsioDriver.Add(MenuItem);
    end;

  if AsioDriverList.Count = 0 then
  begin
    MessageDlg('ERROR: No ASIO Driver present! Application Terminated!', mtError, [mbOK], 0);
    Application.Terminate;
  end;

  FMidiInput.OnMidiData := MidiData;
  FMidiInput.OnSysExData := SysExData;

  Settings := TIniFile.Create(IniName);
  try
    i := Settings.ReadInteger('Audio', 'ASIO Driver', -1);
    if i = -1 then i := AsioDriverList.IndexOf('ASIO4ALL v2');
    if i = -1 then i := AsioDriverList.IndexOf('ASIO4ALL');
    if (i < 0) or (i >= AsioDriverList.count) then i := 0;
    MenuItemAsioDriver.Items[i].Checked := True;
    try
      ASIOChange(MenuItemAsioDriver.Items[i]);
    except
    end;

    i := Settings.ReadInteger('Audio', 'Output Channel', 0);
    if (i >= 0) and (i < MenuItemASIOOutputChannel.Count) and (MenuItemASIOOutputChannel.Count <> 0) then
      try
        MenuItemASIOOutputChannel.Items[i].Checked := True;
        MenuItemASIOOutputChannel.Items[i].Click;
      except
      end;

    i := Settings.ReadInteger('Audio', 'Input Channel', 0);
    if (i >= 0) and (i < MenuItemASIOInputChannel.Count) and (MenuItemASIOInputChannel.Count <> 0) then
      try
        MenuItemASIOInputChannel.Items[i].Checked := True;
        MenuItemASIOInputChannel.Items[i].Click;
      except
      end;

    WaveFile.FFilename := Settings.ReadString('Audio', 'File', '');

    i := Settings.ReadInteger('Audio', 'Record Bits', 16);
    case i of
      16:
        Player.ComboBoxRecordFormat.ItemIndex := 0;
      else
        Player.ComboBoxRecordFormat.ItemIndex := 1;
    end;

    MenuItemShowPreset.Checked := Settings.ReadBool('Layout', 'ShowPresetInTitleBar', True);
    FDirPlugin := Settings.ReadString('General', 'Plugin Directory', '');
    FDirPreset := Settings.ReadString('General', 'Preset Directory', '');
    FDirWave   := Settings.ReadString('General', 'Wave Directory', '');

    // clear playlists
    Player.ListBoxWavFiles.Clear;

    // load playlists
    PlayList := TStringList.Create;
    try
      Settings.ReadSection('Playlist WAV', PlayList);
      for i := 0 to PlayList.Count - 1 do
        AddWAV(PlayList[i]);
    finally
      PlayList.Free;
    end;

    Player.ComboBoxWavPlayMode.ItemIndex := Settings.ReadInteger('Audio', 'LoopMode', 1);
    WaveFile.looped := Player.ComboBoxWavPlayMode.ItemIndex = 1;

    MenuItemDownMixToStereo.Checked := Settings.ReadBool('VST', 'DownmixStereo', False);

    str := Settings.ReadString('VST', 'LastPlugin', '');
    i := Settings.ReadInteger('VST', 'LastProgram', 0);
  finally
    Settings.Free;
  end;

  LoadWAV(WaveFile.FileName);

{$IFNDEF FPC}
  if (ParamCount > 0) and (FileExists(Paramstr(1))) then
  begin
    LoadPlugin(Paramstr(1));
    VSTHost[0].OnProcessEvents := ProcessEvents;
  end
  else if FileExists(str) then
  begin
    FLoadProg := i;
    LoadPlugin(str, i);
    VSTHost[0].OnProcessEvents := ProcessEvents;
  end;
{$ENDIF}
end;

procedure TFormMiniHost.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FProcessing := False;
  FAllowed := False;
  try
    StopAudio;
  except
  end;

  // free about form if necessary
  if Assigned(FAboutForm) then
    FreeAndNil(FAboutForm);

  with TIniFile.Create(IniName) do
    try
      EraseSection('Playlist MIDI');
      EraseSection('Playlist WAV');
      for i := 0 to Player.ListBoxWavFiles.Items.Count - 1 do
        WriteString('Playlist WAV', PShortStr(Player.ListBoxWavFiles.Items.Objects[i])^, '');
      WriteInteger('General', 'Timer', TimerWaveFile.Interval);
      WriteInteger('Layout', 'MainWindow X', Left);
      WriteInteger('Layout', 'MainWindow Y', Top);
      WriteInteger('Layout', 'SettingsWindow X', FormOptions.Left);
      WriteInteger('Layout', 'SettingsWindow Y', FormOptions.Top);
      WriteBool('Layout', 'SettingsWindow Visible', FormOptions.Showing);
      WriteString('General', 'Plugin Directory', FDirPlugin);
      WriteString('General', 'Preset Directory', FDirPreset);
      WriteString('General', 'Wave Directory', FDirWave);
      WriteString('General', 'Midi Directory', FDirMidi);
      WriteInteger('Layout', 'PlayerWindow X', Player.Left);
      WriteInteger('Layout', 'PlayerWindow Y', Player.Top);
      WriteBool('Layout', 'PlayerWindow Visible', Player.Showing);
      WriteBool('VST', 'DownmixStereo', MenuItemDownMixToStereo.Checked);
      WriteBool('VST', 'UseMouseWheel', MenuItemUseMouseWheel.Checked);
      WriteInteger('Audio', 'ASIO Driver', FCurrentASIO);
      WriteInteger('Audio', 'Output Channel', FCurrentOutputChannel);
      WriteInteger('Audio', 'Input Channel', FCurrentInputChannel);
      if Player.ListBoxWavFiles.Items.Count = 0 then
        WAVEFile.FileName := '';//c
      WriteString('Audio', 'File', WAVEFile.FileName);
      WriteInteger('Audio', 'VST Volume', FormOptions.ScrollBarVSTVolume.Position);
      WriteInteger('Audio', 'Overall Volume', FormOptions.ScrollBarOverallVolume.Position);
      WriteInteger('Audio', 'Input Volume', FormOptions.ScrollBarInputVolume.Position);
      WriteInteger('Audio', 'WAV Volume', FormOptions.ScrollBarWavVolume.Position);
      WriteInteger('VST', 'Tempo', FormOptions.ScrollBarTempo.Position);
      WriteString('VST', 'LastPlugin', VSTHost[0].DLLFilename);
      WriteInteger('VST', 'LastProgram', FCurProg);
      WriteInteger('Audio', 'LoopMode', Player.ComboBoxWavPlayMode.ItemIndex);
      WriteBool('Layout', 'ShowPresetInTitleBar', MenuItemShowPreset.Checked);
      WriteInteger('MIDI', 'MIDI-In Driver', FCurrentMIDIIn);
      WriteInteger('MIDI', 'MIDI-Out Driver', FCurrentMIDIOut);
      case Player.ComboBoxRecordFormat.ItemIndex of
        0:
          i := 16;
        else
          i := 32;
      end;
     WriteInteger('Audio', 'Record Bits', i);
  finally
    Free;
  end;

  if FPluginLoaded then
    ClosePlugin;

  if Assigned(FWavWriter) then
    FreeAndNil(FWavWriter);

  try
    FMidiInput.CloseAll;
    FMidiOutput.CloseAll;
  except
  end;

  FreeAndNil(FMidiInput);
  FreeAndNil(FMidiOutput);
  FreeAndNil(FWaveFile);
  FNumIn := 0;
  FNumOut := 0;
  for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
  for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
  SetLength(FVSTBufOut, 0);
  SetLength(FVSTBufIn, 0);
  SetLength(FVSTPinProps, 0);
  for i := 0 to 2047 do
    FreeMem(FMyEvents.Events[i]);

  FreeAndNil(FDataSection);
end;

procedure TFormMiniHost.FormShow(Sender: TObject);
var
  i: Integer;
begin
  with TIniFile.Create(IniName) do
  try
    MenuItemUseMouseWheel.Checked := ReadBool('VST', 'UseMouseWheel', True);
    FormOptions.ScrollBarOverallVolume.Position := ReadInteger('Audio', 'Overall Volume', 100);
    FormOptions.ScrollBarVSTVolume.Position := ReadInteger('Audio', 'VST Volume', 100);
    FormOptions.ScrollBarInputVolume.Position := ReadInteger('Audio', 'Input Volume', 100);
    FormOptions.ScrollBarWavVolume.Position := ReadInteger('Audio', 'WAV Volume', 100);
    FormOptions.ScrollBarTempo.Position := ReadInteger('VST', 'Tempo', 120);

    FormOptions.Left := ReadInteger('Layout', 'SettingsWindow X', Left - 100);
    FormOptions.Top := ReadInteger('Layout', 'SettingsWindow Y', Top);
    if FormOptions.Left < 0 then FormOptions.Left := 0;
    if FormOptions.Top < 0 then FormOptions.Top := 0;
    if FormOptions.Left > Screen.Width - 20 then FormOptions.Left := Screen.Width - 20;
    if FormOptions.Top > Screen.Height - 20 then FormOptions.Top := Screen.Height - 20;

    Player.Left := ReadInteger('Layout', 'PlayerWindow X', Left - 100);
    Player.Top := ReadInteger('Layout', 'PlayerWindow Y', Top);
    if Player.Left < 0 then Player.Left := 0;
    if Player.Top  < 0 then Player.Top := 0;
    if Player.Left > Screen.Width  - 20 then Player.Left := Screen.Width  - 20;
    if Player.Top  > Screen.Height - 20 then Player.Top  := Screen.Height - 20;
    FormOptions.ScrollBarTempoChange(nil);
    if ReadBool('Layout', 'SettingsWindow Visible', False) then
    begin
      FormOptions.Show;
      FormOptions.setfocus;
    end;
    if ReadBool('Layout', 'PlayerWindow Visible', False) then
    begin
      Player.Show;
      Player.SetFocus;
    end;

    MenuItemAlwaysOnTop.Checked := not ReadBool('Layout', 'AlwaysOnTop', False);
    MenuItemAlwaysOnTopClick(Sender);
    i := ReadInteger('MIDI', 'MIDI-In Driver', 0);
    if (i < 0) or (i > FMidiInput.Devices.Count) then i := 0;
    FCurrentMIDIIn := i;
    MenuItemMidiIn.Items[i].Click;
    i := ReadInteger('MIDI', 'MIDI-Out Driver', 0);
    if (i < 0) or (i > FMidiOutput.Devices.Count) then i := 0;
    FCurrentMIDIOut := i;
    MenuItemMIDIOut.Items[i].Click;
  finally
    Free;
  end;
  TimerWaveFile.Enabled := True;

  if FLoadProg >= 0 then
  begin
    VSTHost[0].CurrentProgram := FLoadProg;
    FLoadProg := -1;
  end;

  if PanelStatus.Visible then
    PanelStatus.SetFocus;
end;

procedure TFormMiniHost.StartAudio;
var
  i: Integer;
begin
  if ASIOHost.Active then
    Exit;

  // deactivate ASIO host
  ASIOHost.Active := False;

  // update plugins
  VSTHost.BlockSize := ASIOHost.BufferSize;
  for i := 0 to VSTHost.VSTPlugIns.Count - 1 do
    with VSTHost[i] do
      if Active then
      begin
        VstCanDo('sendVstTimeInfo');
        VstCanDo('receiveVstTimeInfo');
      end;

  // activate ASIO host
  with VSTHost.VstTimeInfo do
    Flags := Flags + [vtiTransportPlaying];
  ASIOHost.Active := True;
end;

procedure TFormMiniHost.StopAudio;
begin
  // exclude transport playing flag
  if Assigned(VSTHost) then
    with VSTHost.VstTimeInfo do
      Flags := Flags - [vtiTransportPlaying];

  // deactivate ASIO host
  if not ASIOHost.Active then
    Exit;
  ASIOHost.Active := False;
end;

procedure TFormMiniHost.LoadWAV(const FileName: string);
begin
  WaveFile.Unload;

  if FileExists(FileName) then
  begin
    WaveFile.Load(FileName);
    WaveFile.LabelSampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFormMiniHost.ClosePlugin;
var
  i: Integer;
begin
  FPanel.Height := 0;
  MidiPlaying := False;
  TimerWaveFile.Enabled := False;
  FProcessing := False;

  StopAudio;
  MenuItemPanicClick(nil);
  FRecordState := rsStop;

  if Assigned(FWavWriter) then
    FreeAndNil(FWavWriter);

  if (VSTHost[0].DLLFileName <> '') and VSTHost[0].Active then
    with VSTHost[0] do
    begin
      CloseEdit;
      Close;
      Unload;
    end;

  FNumIn := 0;
  FNumOut := 0;
  for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
  for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
  SetLength(FVSTBufOut, 0);
  SetLength(FVSTBufIn, 0);
  SetLength(FVSTPinProps, 0);

  MenuItemLoadPreset.Enabled := False;
  MenuItemSavePreset.Enabled := False;
  MenuItemLoadBank.Enabled := False;
  MenuItemSaveBank.Enabled := False;
  FPluginLoaded := False;
  PresetBox.clear;
  TimerWaveFile.Enabled := True;
end;

procedure TFormMiniHost.BuildPresetList;
var
  m: TMenuItem;
  n, i: Integer;
  s: AnsiString;
begin
  PresetBox.Clear;
  n := VSTHost[0].numPrograms;

  for i := 0 to n - 1 do
  begin
    VSTHost[0].GetProgramNameIndexed(-1, i, s);
    m := TMenuItem.Create(Self);
    m.Caption := string(s);
    m.OnClick := SetPreset;
    m.Tag := i;
{$IFNDEF FPC}
    if (i > 0) and (i mod 256 <> 0) and (i mod 32 = 0) then
      m.break := mbBarBreak;
{$ENDIF}
    s := AnsiString(IntToStr(i));
    if i < 10 then
      s := '00' + s
    else if i < 100 then
      s := '0' + s;
    PresetBox.AddItem(string(s) + ': ' + m.Caption, nil);
  end;

  if n >= 0 then
    PresetBox.ItemIndex := FCurProg;
end;

procedure TFormMiniHost.StopProcessingAndClosePlugin;
begin
  TimerWaveFile.Enabled := False;
  FProcessing := False;
  StopAudio;
  Sleep(2);
  ClosePlugin;
  Sleep(2);
end;

procedure TFormMiniHost.LoadPlugin(const VSTDll: TFileName;
  const DefaultProgram: Integer = 0);
var
  PresetFileName: TFileName;
{$IFDEF LoadPluginFromStream}
  FileStream: TFileStream;
{$ENDIF}
begin
  if not FileExists(VSTDll) then
    Exit;

  StopProcessingAndClosePlugin;

  VSTHost.BlockSize := ASIOHost.BufferSize;
  VSTHost[0].LoadFromFile(VSTDll);

  try
    VSTHost[0].Active := True;
  except
    MessageDlg('ERROR: ' + VSTDll + ' is not a valid VST plugin!', mtError, [mbOK], 0);
    VSTHost[0].Active := False;
    VSTHost[0].DLLFilename := '';
    FPanel.Height := 0;
    Exit;
  end;

  // try loading possible default bank
  PresetFileName := ChangeFileExt(VSTDll, '.fxb');
  if FileExists(PresetFileName) then
    try
      VSTHost[0].LoadBank(PresetFileName);
    except
    end;

  // try loading possible default program
  PresetFileName := ChangeFileExt(VSTDll, '.fxp');
  if FileExists(PresetFileName) then
    try
      VSTHost[0].LoadPreset(PresetFileName);
    except
    end;

  BuildChannelBuffers;

  ShowVSTPlugin(DefaultProgram);
end;

procedure TFormMiniHost.BuildChannelBuffers;
var
  i: Integer;
begin
  SetLength(FVSTBufIn,  max(VSTHost[0].numInputs,  2), ASIOHost.BufferSize);
  SetLength(FVSTBufOut, max(VSTHost[0].numOutputs, 2), ASIOHost.BufferSize);
  FNumIn := VSTHost[0].numInputs;
  FNumOut := VSTHost[0].numOutputs;
  SetLength(FVSTPinProps, FNumOut);
  for i := 0 to FNumOut - 1 do
    FVSTPinProps[i] := VSTHost[0].GetOutputProperties(i);
end;

procedure TFormMiniHost.ShowVSTPlugin(const DefaultProgram: Integer = 0);
var
  rct: ERect;
begin
  with VSTHost[0] do
  try
    ShowEdit(FPanel);

    FTitle := GetVendorString + ' ' +  GetEffectName;
    BuildPresetList;

    if (effFlagsHasEditor in VSTHost[0].EffectOptions) then
    begin
      rct := EditGetRect;
      FPanel.Width  := Rct.Right - Rct.Left;
      FPanel.Height := Rct.Bottom - Rct.Top;
      FPanel.Top := PanelStatus.Height;

      // set client width
      if FPanel.Width < 560 then
      begin
        ClientWidth := 560;
        FPanel.Left := (560 - FPanel.Width) div 2;
      end
      else
      begin
        ClientWidth := FPanel.Width;
        FPanel.left := 0;
      end;
      ClientHeight := FPanel.Height + PanelStatus.Height;

      // find background color ASAP
      FColBack := False;
    end
    else
    begin
      FPanel.Width  := 560;
      FPanel.Height := 480;
    end;
  except
    raise;
  end;

  MenuItemLoadPreset.Enabled := True;
  MenuItemLoadBank.Enabled   := True;
  MenuItemSavePreset.Enabled := True;
  MenuItemSaveBank.Enabled   := True;
  FProcessing := True;
  StartAudio;
  TimerWaveFile.Enabled := True;
  MenuItemRenamePreset.Enabled := VSTHost[0].numPrograms >= 1;

  FPluginLoaded := True;
  FAllowed := True;
  Caption := string(FTitle);
  Left := Screen.Width div 2 - Width div 2;
  Top := Screen.Height div 2 - Height div 2;
  VSTHost[0].CurrentProgram := DefaultProgram;
  FormOptions.ScrollBarTempoChange(nil);
end;

function TFormMiniHost.FindBackgroundColor: TColor;
var
  BMP: TBitmap;
  SCL: PRGB24Array;
  R, G, B: Integer;
  x: Integer;
begin
 // fill background
 Application.ProcessMessages;

 {$IFNDEF FPC}
 BMP := TBitmap.Create;
 BMP.PixelFormat := pf24bit;
 with BMP do
  try
   VSTHost[0].RenderEditorToBitmap(BMP);
   SCL := BMP.ScanLine[0];
   R := 0; G := 0; B := 0;

   for x := 0 to BMP.Width - 1 do
    begin
     R := R + SCL[x].R;
     G := G + SCL[x].G;
     B := B + SCL[x].B;
    end;

   Result := RGB(R div BMP.Width, G div BMP.Width, B div BMP.Width);
  finally
   FreeAndNil(BMP);
  end;
 {$ELSE}
 Result := $808080;
 {$ENDIF}
end;

procedure TFormMiniHost.VSTHostAudioMasterIdle(Sender: TVSTPlugin);
begin
 Sender.Idle;
end;

procedure TFormMiniHost.VSTHostAudioMasterNeedIdle(Sender: TVSTPlugin);
begin
 Sender.EditIdle;
end;

procedure TFormMiniHost.MIDIInChange(Sender: TObject);
begin
 if FMidiInput.Devices.Count = 0
  then Exit;
 FMidiInput.OnMidiData := MidiData;
 (Sender as TMenuItem).Checked := True;
 try
  FMidiInput.Close(FCurrentMIDIIn);
 except
 end;
 FCurrentMIDIIn := (sender as TMenuItem).Tag;
 MenuItemMidiIn.Items[FCurrentMIDIIn].Checked := True;
 try
  if FCurrentMIDIIn > 0
   then FMidiInput.Open(FCurrentMIDIIn - 1);
 except
 end;
end;

// By Daniel:  Note that Dav_MidiIO midiInProc midiInCallback is called
// concurrently by different service threads
procedure TFormMiniHost.MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: Byte);
begin
  if aStatus = $FE then Exit; // ignore active sensing
  if ((aStatus and $0F) = 0) then
  begin
    if (aStatus and $F0) = $90 then
      NoteOn(aStatus, aData1, aData2) //ok
    else
    if (aStatus and $F0) = $80 then
      NoteOff(aStatus, aData1)
    else
      AddMidiData(aStatus, aData1, aData2);
  end;
end;

procedure TFormMiniHost.SysExData(const aDeviceIndex: Integer; const aStream: TMemoryStream);
begin
  FDataSection.Acquire;
  try
    if FMDataCnt > 2046 then
      Exit;

    Inc(FMDataCnt);
    with PVstMidiSysexEvent(FMyEvents.events[FMDataCnt - 1])^ do
    begin
      EventType := etSysEx;
      ByteSize := 24;
      DeltaFrames := 0;
      Flags := [];
      dumpBytes := aStream.Size;
      sysexDump := aStream.Memory;
      Reserved1 := 0;
      Reserved2 := 0;
    end;
  finally
    FDataSection.Release;
  end;
end;

procedure TFormMiniHost.ASIOHostLatencyChanged(Sender: TObject);
begin
  VSTHost.LatencyInput := ASIOHost.InputLatency;
  VSTHost.LatencyOutput := ASIOHost.OutputLatency;
end;

procedure TFormMiniHost.ASIOHostUpdateSamplePos(Sender: TObject;
  SamplePosition: Int64);
begin
  VSTHost.VstTimeInfo.SamplePos := SamplePosition;
end;

procedure TFormMiniHost.ASIOHostSampleRateChanged(Sender: TObject);
begin
  MenuItemStopRecordingClick(nil);
  if VSTHost[0].Active then
    VSTHost[0].SetSampleRate(ASIOHost.SampleRate);
  VSTHost.VstTimeInfo.SampleRate := ASIOHost.SampleRate;
  WaveFile.LabelSampleRate := ASIOHost.SampleRate;
end;

procedure TFormMiniHost.SetChannel(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  FCurrentOutputChannel := (Sender as TMenuItem).Tag;
  if ASIOHost.Active then
  begin
    StopAudio;
    StartAudio;
  end
  else
    FProcessing := False;
  FormOptions.LabelOutputs.Caption := 'Outputs: ' + MenuItemASIOOutputChannel.Items[FCurrentOutputChannel].Caption;
end;

procedure TFormMiniHost.ASIOChange(Sender: TObject);
var
  Channel, j: Integer;
  m: TMenuItem;
begin
  // make sure the sender was a TMenuItem, then check this item!
  Assert(Sender is TMenuItem);
  TMenuItem(Sender).Checked := True;

  // set flag to disable internal processing
  FProcessing := False;

  // do panic!
  MenuItemPanicClick(nil);

  // stop audio
  StopPlayback2Click(nil);
  StopAudio;

  // get new driver index
  FCurrentASIO := (Sender as TMenuItem).Tag;
  if FCurrentASIO >= 0 then
  begin
    // ensure the driver has some time to stop actually!
    Sleep(10);

    // change driver index
    ASIOHost.DriverIndex := FCurrentASIO;

    // delete all channels
    for Channel := 0 to MenuItemASIOOutputChannel.Count - 1 do MenuItemASIOOutputChannel.Delete(0);
    for Channel := 0 to MenuItemASIOInputChannel.Count - 1 do MenuItemASIOInputChannel.Delete(0);

    // add new output channel pairs
    j := 0;
    for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
      if not Odd(Channel) then
      begin
        m := TMenuItem.Create(Self);
        m.RadioItem := True;
        m.Tag := j;
        Inc(j);
        m.OnClick := SetChannel;
        if Channel < ASIOHost.OutputChannelCount - 1 then
          m.Caption := string(ASIOHost.OutputChannelInfos[Channel].Name +
            AnsiString(' / ') + ASIOHost.OutputChannelInfos[Channel + 1].Name)
        else
          m.Caption := string(ASIOHost.OutputChannelInfos[Channel].Name);
        MenuItemASIOOutputChannel.Add(m);
      end;

    // add new input channel pairs
    m := TMenuItem.Create(Self);
    m.RadioItem := True;
    m.Tag := 0;
    m.OnClick := SetChannelI;
    m.Caption := 'None';
    MenuItemASIOInputChannel.Add(m);
    j := 1;
    for Channel := 0 to ASIOHost.InputChannelCount - 1 do
      if not Odd(Channel) then
      begin
        m := TMenuItem.Create(Self);
        m.RadioItem := True;
        m.Tag := j;
        inc(j);
        m.OnClick := SetChannelI;
        if Channel < ASIOHost.InputChannelCount - 1
         then m.Caption := string(ASIOHost.InputChannelInfos[Channel].Name +
           AnsiString(' / ') + ASIOHost.InputChannelInfos[Channel + 1].Name)
         else m.Caption := string(ASIOHost.InputChannelInfos[Channel].Name);
        MenuItemASIOInputChannel.Add(m);
      end;

    MenuItemASIOInputChannel.Items[0].Click;
    if ASIOHost.OutputChannelCount > 0 then
      MenuItemASIOOutputChannel.Items[0].Click;
  end;

  // update options form
  with FormOptions do
  begin
    LabelASIODriver.Caption := 'ASIO Driver: ' + ASIOHost.DriverName;
    if MenuItemASIOOutputChannel.Count > 0 then
      LabelOutputs.Caption := 'Outputs: ' + MenuItemASIOOutputChannel.Items[0].Caption
    else
      LabelOutputs.Caption := 'Outputs: None';
    if MenuItemASIOInputChannel.Count > 0 then
      LabelInputs.Caption := 'Inputs: ' + MenuItemASIOInputChannel.Items[0].Caption
    else
      LabelInputs.Caption := 'Inputs: None';
    if ASIOHost.OutputChannelCount > 0 then
      LabelFormat.Caption := 'Format: ' + IntToStr(ASIOHost.OutputChannelInfos[0].SampleType) + ' ' + ChannelTypeToString(ASIOHost.OutputChannelInfos[0].SampleType)
    else
      LabelFormat.Caption := 'Format: None';
    LabelBufferSize.Caption := 'Buffersize: ' + IntToStr(ASIOHost.BufferSize);
    LabelSampleRate.Caption := 'Samplerate: ' + IntToStr(Round(ASIOHost.SampleRate));
  end;

  // reset ASIO driver and start processing...
  ASIOHostReset(Sender);
  StartAudio;
  FProcessing := True;
end;

procedure TFormMiniHost.MenuItemPanicClick(Sender: TObject);
var
  Ch, Note: word;
begin
 FDataSection.Acquire;
 try
  FMDataCnt := 0;
  for Note := 0 to 127 do AddMidiData($80, Note, 0);
  for Ch := 0 to 15 do AddMidiData($B0 + Ch, 123, 0);
 finally
  FDataSection.Release;
 end;
end;

procedure TFormMiniHost.MenuItemLoadPresetClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   FileName := '*.fxp';
   InitialDir := FDirPreset;
   DefaultExt := '.fxp';
   Options := [ofAllowMultiSelect, ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   if Execute then
    begin
     FDirPreset := ExtractFileDir(FileName);
     LoadPresets(Files);
    end;
  finally
   Free;
  end;
end;

procedure TFormMiniHost.LoadPresets(Files: TStrings);
var
  i, j, k: Integer;
  s: string;
begin
 MenuItemPanicClick(nil);
 TimerWaveFile.Enabled := False;
 j := FCurProg;
 for i := 0 to Files.Count - 1 do
  begin
   if i > 0 then VSTHost[0].CurrentProgram := j + i;
   try
    VSTHost[0].LoadPreset(Files[i]);
   except
    MessageDlg('ERROR: Preset file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
    TimerWaveFile.Enabled := True;
    Exit;
   end;
   k := VSTHost[0].CurrentProgram;
   s := IntToStr(k);
   if k < 10 then s := '00' + s else
   if k < 100 then s := '0' + s;
  end;
 TimerWaveFile.Enabled := True;
end;

procedure TFormMiniHost.MenuItemSavePresetClick(Sender: TObject);
var
  s2: string;
begin
  MenuItemPanicClick(nil);
  Sleep(2);
  with TSaveDialog.Create(Self) do
    try
      DefaultExt := '.fxp';
      FileName := '*.fxp';
      Filter := 'preset files (*.fxp)|*.fxp';
      Title := 'Select a preset';
      InitialDir := FDirPreset;
      Options := [ofForceShowHidden];
{$IFNDEF FPC}
      Ctl3D := False;
{$ENDIF}
      s2 := PresetBox.Items[PresetBox.ItemIndex];
      s2 := Copy(s2, 6, Length(s2) - 5);
{$IFNDEF FPC}
      FileName := MakeGoodFileName(s2) + '.fxp';
{$ENDIF}
      if Execute then
      begin
        VSTHost[0].SavePreset(FileName);
        FDirPreset := ExtractFileDir(FileName);
      end;
    finally
      Free;
    end;
end;

procedure TFormMiniHost.MenuItemLoadBankClick(Sender: TObject);
begin
  TimerWaveFile.Enabled := False;
  Sleep(2);
  with TOpenDialog.Create(Self) do
    try
      DefaultExt := '.fxb';
      FileName := '*.fxb';
      Filter := 'bank files (*.fxb)|*.fxb';
      Title := 'Select a bank';
      InitialDir := FDirPreset;

      Options := [ofFileMustExist, ofForceShowHidden];
{$IFNDEF FPC}
      Ctl3D := False;
{$ENDIF}
      if Execute then
      begin
        FDirPreset := ExtractFileDir(FileName);
        try
          VSTHost[0].LoadBank(FileName);
        except
          MessageDlg('ERROR: Bank file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
          TimerWaveFile.Enabled := True;
        end;
        BuildPresetList;
      end;
    finally
      Free;
      FCurProg := 0;
      VSTHost[0].CurrentProgram := 0;
      PresetBox.ItemIndex := 0;
      TimerWaveFile.Enabled := True;
    end;
end;

procedure TFormMiniHost.MenuItemSaveBankClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   FileName := '*.fxb';
   DefaultExt := '.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := FDirPreset;
   Options := [ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   if Execute then
    begin
     FDirPreset := ExtractFileDir(FileName);
     VSTHost[0].SaveBank(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFormMiniHost.MenuItemVSTClosePluginClick(Sender: TObject);
begin
 WaveFile.Stop;
 ClosePlugin;
end;

procedure TFormMiniHost.MenuItemVSTLoadPluginClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   FileName := '*.dll';
   Filter := 'VST Plugins (*.dll)|*.dll';
   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Title := 'Select a VST plugin';
   InitialDir := FDirPlugin;
   if Execute then
    begin
     FDirPlugin := ExtractFileDir(FileName);
     LoadPlugin(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFormMiniHost.LoadWAVFile;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   FileName := '*.wav;*.wpl';
   Filter := 'WAV files and playlists (*.wav;*.wpl)|*.wav;*.wpl|WAV files (*.wav)|*.wav|WAV playlists (*.wpl)|*.wpl';
   FilterIndex := 0;
   InitialDir := FDirWave;
   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Title := 'Select a WAV file';
   if Execute then
    begin
     FDirWave := ExtractFileDir(FileName);
     AddWAV(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFormMiniHost.StartPlayback2Click(Sender: TObject);
begin
 if not FileExists(Wavefile.FileName) then Exit;
 WaveFile.play;
end;

procedure TFormMiniHost.SetPreset(Sender: TObject);
begin
 MenuItemPanicClick(nil);
 VSTHost[0].CurrentProgram := (Sender as TMenuItem).Tag;
end;

procedure TFormMiniHost.MyMidiEvent(const Event: TVstEvent);
begin
  with TVstMidiEvent(Event) do
  if (MidiData[0] and $F0) = $90 then
    NoteOn(MidiData[0], MidiData[1], MidiData[2]) else
  if (MidiData[0] and $F0) = $80 then
    NoteOff(MidiData[0], MidiData[1])
  else
    AddMidiData(MidiData[0], MidiData[1], MidiData[2]);
end;

procedure TFormMiniHost.RecordWAVFileSelect;
begin
  with TSaveDialog.Create(Self) do
  try
    DefaultExt := '.wav';
    InitialDir := FDirWave;
    FileName := '*.wav';
    Filter := 'WAV files (*.wav)|*.wav';
    Title := 'Select a WAV file';
    Options := [ofForceShowHidden];
    {$IFNDEF FPC}
    Ctl3D := False;
    {$ENDIF}
    if Execute then
    begin
      FDirWave := ExtractFileDir(FileName);
      Player.LabelRecordFile.Caption := FileName;
    end;
  finally
    Free;
  end;
end;

procedure TFormMiniHost.MenuItemStartRecordingClick(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  if Assigned(FWavWriter) then
    FreeAndNil(FWavWriter);

  case Player.ComboBoxRecordFormat.ItemIndex of
    0:
      i := 16;
    else
      i := 32;
  end;

  s := Player.LabelRecordFile.Caption;
  if s = '<none>' then
  begin
    RecordWavFileSelect;
    s := Player.LabelRecordFile.Caption;
    if (s = '<none>') or (s = '') then
      Exit;
  end;

  FTotalFrames := 0;
  if Player.CheckBoxRecInMono.Checked then
    FWavWriter := TWavWriter.Create(s, Round(ASIOHost.SampleRate), 1, i)
  else
    FWavWriter := TWavWriter.Create(s, Round(ASIOHost.SampleRate), 2, i);
  FRecordState := rsRecord;
end;

procedure TFormMiniHost.MIDIOutChange(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  try
    FMidiOutput.Close(FCurrentMIDIOut);
  except
  end;

  FCurrentMIDIOut := (Sender as TMenuItem).Tag;
  MenuItemMidiOut.Items[FCurrentMIDIOut].Checked := True;
  try
    if FCurrentMIDIOut > 0 then
      FMidiOutput.Open(FCurrentMIDIOut - 1);
  except
  end;
end;

procedure TFormMiniHost.TimerWaveFileTimer(Sender: TObject);
var
  s2, s: AnsiString;
  i: Integer;
  e: Single;
begin
  if WaveFile.FPMode > wpmPause then
  begin
   i := Round(100 * WaveFile.FCnt2 / (WaveFile.Size - 2));
   Player.LabelWavPosition.Caption := 'position: ' + IntToStr(i) +' %';
   Player.ScrollBarWavPosition.Position := i;
  end;

  BorderOnOff.Visible := FProcessing;
  BorderOptions.Visible := FormOptions.Showing;
  BorderPlayWave.Visible := not (WaveFile.FPMode = wpmPause);
  BorderRecordWave.Visible := (FRecordState = rsRecord);

  case FRecordState of
    rsRecord:
      Player.LabelStatus.Caption := 'status: recording';
    rsPause:
      Player.LabelStatus.Caption := 'status: paused';
    else
      Player.LabelStatus.Caption := 'status: stopped';
  end;

  if FRecordState > rsStop then
  begin
    e := FTotalFrames / ASIOHost.SampleRate;
    Player.LabelStatus.Caption :=
      Player.LabelStatus.Caption + ' (time: '
      + FloatToStrF(e, ffFixed, 4, 2) + ' sec, size: ' + IntToStr(
        Round(e * FWavWriter.Format.nAvgBytesPerSec / 1000))
      + ' kbytes)';
  end;

  FDownMix := MenuItemDownMixToStereo.Checked;

  if PresetBox.Items.Count = 0 then
  begin
   Caption := 'Delphi ASIO & VST Project -  MiniHost';
   Exit;
  end;

  s := VSTHost[0].GetProgramName;
  i := VSTHost[0].CurrentProgram;
  if (FCurProg <> i) or (FCurProgName <> s) then
  begin
    FCurProg := i;
    FCurProgName := s;
    s := AnsiString(IntToStr(FCurProg));
    if FCurProg < 10 then s := '00' + s else
    if FCurProg < 100 then s := '0' + s;
    if (PresetBox.items.Count > 0) and (FCurProg>=0) then
    begin
      PresetBox.Items[FCurProg] := string(s + ': ' + FCurProgName);
      PresetBox.ItemIndex := i;
    end;
    s2 := FTitle;
    if MenuItemShowPreset.Checked then
      s2 := s2 + AnsiString(' - ') + s + AnsiString(': ') + FCurProgName;
    if Caption <> string(s2) then
      Caption := string(s2);
  end;
end;

procedure TFormMiniHost.MenuItemASIOControlPanelClick(Sender: TObject);
begin
  StopAudio;
  ASIOHost.ControlPanel;
  ASIOHost.Reset;
  StartAudio;
end;

procedure TFormMiniHost.ProcessEvents(Sender: TObject; ev: PVstEvents);
var
  i: Integer;
  Event: PVstMidiEvent;
  Sysex: PVstMidiSysexEvent;
  aStream: TMemoryStream;
begin
  if FCurrentMIDIOut = 0 then Exit;

  FDataSection.Acquire;
  try
    for i := 0 to ev^.numEvents - 1 do
      if (ev.events[i].EventType = etMidi) then
      begin
        Event := PVstMidiEvent(ev^.events[i]);
        FMidiOutput.Send(FCurrentMIDIOut - 1, Event^.mididata[0],
          Event^.mididata[1], Event^.mididata[2]);
      end
      else
      if ev.events[i].EventType = etSysex then
      begin
        Sysex := PVstMidiSysexEvent(ev^.events[i]);
        if Sysex.dumpBytes > 0 then
        begin
          AStream := TMemoryStream.Create;
          try
            aStream.Size := Sysex.dumpBytes;
            aStream.Position := 0;
            Move(Sysex.SysexDump^, pchar(aStream.Memory)[0], Sysex.dumpBytes);
            FMidiOutput.SendSysEx(FCurrentMIDIOut - 1, aStream);
          finally
            FreeAndNil(aStream);
          end;
        end;
      end;
  finally
    FDataSection.Release;
  end;
end;

procedure TFormMiniHost.WMDropFiles(var msg: TMessage);
var
  size: Integer;
  name: PChar;
  fn, s: string;
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
      MessageDlg('ERROR: Preset file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
     Exit;
    end;
  end
  else
  if (fn = '.FXB') then
  begin
    try
      VSTHost[0].LoadBank(s);
    except
      MessageDlg('ERROR: Bank file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
      Exit;
    end;
  end
  else
  if (fn = '.DLL') then
    LoadPlugin(s)
  else
  if (fn = '.WAV') then
  begin
    AddWAV(s);
    LoadWAV(s);
  end;
end;

procedure TFormMiniHost.MenuItemRenamePresetClick(Sender: TObject);
var
  s2, s: string;
begin
  s := InputBox('Rename Preset', 'New name:', string(VSTHost[0].GetProgramName));
  VSTHost[0].SetProgramName(AnsiString(s));
  VSTHost[0].Idle;
  VSTHost[0].EditIdle;

  s2 := IntToStr(FCurProg);
  if FCurProg < 10 then s2 := '00' + s2 else
  if FCurProg < 100 then s2 := '0' + s2;

  PresetBox.Items[FCurProg] := s2 + ': ' + s;
end;

procedure TFormMiniHost.MenuItemSettingsClick(Sender: TObject);
begin
  FormOptions.Show;
end;

procedure TFormMiniHost.SetChannelI(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  FCurrentInputChannel := (Sender as TMenuItem).Tag;
  FormOptions.LabelInputs.Caption := 'Inputs: ' + MenuItemASIOInputChannel.Items[FCurrentInputChannel].Caption;
end;

procedure TFormMiniHost.ASIOHostReset(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FVSTBufOut) - 1 do
    SetLength(FVSTBufOut[i], ASIOHost.BufferSize);
  for i := 0 to Length(FVSTBufIn) - 1 do
    SetLength(FVSTBufIn[i], ASIOHost.BufferSize);
  SetLength(FInBufL, ASIOHost.BufferSize);
  SetLength(FInBufR, ASIOHost.BufferSize);
  SetLength(FWavBufL, ASIOHost.BufferSize);
  SetLength(FWavBufR, ASIOHost.BufferSize);
  ASIOHostSampleRateChanged(Sender);
end;

procedure TFormMiniHost.ASIOHostDestroy(Sender: TObject);
var
  i: Integer;
begin
  FProcessing := False;
  SetLength(FInBufL, 0);
  SetLength(FInBufR, 0);
  SetLength(FWavBufL, 0);
  SetLength(FWavBufR, 0);
  FNumIn := 0;
  FNumOut := 0;
  for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
  for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
  SetLength(FVSTBufOut, 0);
  SetLength(FVSTBufIn, 0);
  SetLength(FVSTPinProps, 0);
end;


// By Daniel:  Dav_MidiIO midiInProc midiIncallback is called
// concurrently by different service threads
// we need to protect the midi Event arrary and the DataCnt
// against concurrent access
procedure TFormMiniHost.AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
begin
  FDataSection.Acquire;
  try
    if FMDataCnt > 2046 then
      Exit;

    Inc(FMDataCnt);
    with PVstMidiEvent(FMyEvents.events[FMDataCnt - 1])^ do
    begin
      EventType := etMidi;
      deltaFrames := pos;
      midiData[0] := d1;
      midiData[1] := d2;
      midiData[2] := d3;
    end;
  finally
    FDataSection.Release;
  end;
end;

procedure TFormMiniHost.NoteOn(ch, note, v: byte);
begin
  if v = 0 then
  begin
    ch := ch - $10;
    NoteOff(ch, note);
    Exit;
  end;
  if (note <= 127) then
    ProcessNoteOnOff(ch, note, v);
end;

procedure TFormMiniHost.NoteOff(ch, note: byte);
begin
  if (note <= 127) then
    ProcessNoteOnOff(ch, note, 0);
end;

procedure TFormMiniHost.ProcessNoteOnOff(ch, n, v: byte);
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

procedure TFormMiniHost.MenuItemAboutClick(Sender: TObject);
begin
  if not Assigned(FAboutForm) then
    FAboutForm := TFormAbout.Create(Self);
  FAboutForm.ShowModal;
end;

procedure TFormMiniHost.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MenuItemPanicClick(nil);

  FAllowed := False;
  WaveFile.stop;
  WaveFile.Unload;

  FRecordState := rsStop;
  if Assigned(FWavWriter) then
    FreeAndNil(FWavWriter);
end;

procedure TFormMiniHost.MenuItemShowPresetClick(Sender: TObject);
var
  s: AnsiString;
begin
 MenuItemShowPreset.Checked := not MenuItemShowPreset.Checked;
 s := AnsiString(IntToStr(FCurProg));
 if FCurProg < 10 then s := '00' + s else
 if FCurProg < 100 then s := '0' + s;
 if MenuItemShowPreset.Checked
  then Caption := string(FTitle + ' - ' + s + ': ' + FCurProgName)
  else Caption := string(FTitle);
end;

procedure TFormMiniHost.StopPlayback1Click(Sender: TObject);
begin
  MenuItemPanicClick(nil);
end;

procedure TFormMiniHost.StopPlayback2Click(Sender: TObject);
begin
  WaveFile.Stop;
end;

procedure TFormMiniHost.MenuItemStopRecordingClick(Sender: TObject);
begin
  FRecordState := rsStop;
  if Assigned(FWavWriter) then
    FreeAndNil(FWavWriter);
end;

procedure TFormMiniHost.RenameF1Click(Sender: TObject);
begin
  MenuItemRenamePresetClick(nil);
end;

procedure TFormMiniHost.F4PlayStopWAV1Click(Sender: TObject);
begin
  with Player do
    if ListBoxWavFiles.ItemIndex >= 0 then
    begin
      Player.LabelWaveFile.Caption := ListBoxWavFiles.items[ListBoxWavFiles.ItemIndex];
      if Wavefile.FPMode = wpmPlay then
        StopPlayback2Click(nil)
      else
      StartPlayback2Click(nil);
    end;
end;

procedure TFormMiniHost.F5RecStopWAV1Click(Sender: TObject);
begin
  if FRecordState >= rsRecord then
    MenuItemStopRecordingClick(nil)
  else
  if FRecordState = rsStop then
    MenuItemStartRecordingClick(nil);
end;

procedure TFormMiniHost.F11MIDIPanic1Click(Sender: TObject);
begin
  MenuItemPanicClick(nil);
end;

procedure TFormMiniHost.MenuItemAlwaysOnTopClick(Sender: TObject);
begin
  MenuItemAlwaysOnTop.Checked := not MenuItemAlwaysOnTop.Checked;
{$IFNDEF FPC}
  if MenuItemAlwaysOnTop.Checked then
    SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE)
  else
    SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE);
{$ENDIF}
end;

procedure TFormMiniHost.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not MenuItemUseMouseWheel.Checked then
    Exit;
  MenuItemPanicClick(nil);
  if FCurProg > 0 then
    VSTHost[0].CurrentProgram := FCurProg - 1;
end;

procedure TFormMiniHost.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if not MenuItemUseMouseWheel.Checked then
    Exit;
  MenuItemPanicClick(nil);
  if FCurProg + 1 < VSTHost[0].numPrograms then
    VSTHost[0].CurrentProgram := FCurProg + 1;
end;

procedure TFormMiniHost.MenuItemIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMiniHost.ImageOnOffMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FProcessing := not FProcessing;
  MenuItemPanicClick(nil);
end;

procedure TFormMiniHost.PresetBoxClick(Sender: TObject);
begin
  TimerWaveFile.Enabled := False;
  MenuItemPanicClick(nil);
  VSTHost[0].CurrentProgram := PresetBox.ItemIndex;
  FCurProg := PresetBox.ItemIndex;
  TimerWaveFile.Enabled := True;
end;

procedure TFormMiniHost.PresetBoxKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TFormMiniHost.PresetBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if Index < 0 then Exit;
  PresetBox.Canvas.FillRect(Rect);
  PresetBox.Canvas.TextOut(rect.Left + 2, rect.top, PresetBox.items[index]);
end;

procedure TFormMiniHost.ImageLeftRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MenuItemPanicClick(nil);
  if (x < ImageLeftRight.width shr 1) and (FCurProg > 0) then
    VSTHost[0].CurrentProgram := FCurProg - 1
  else
  if FCurProg + 1 < VSTHost[0].numPrograms then
    VSTHost[0].CurrentProgram := FCurProg + 1;
end;

procedure TFormMiniHost.ImageDropDownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PresetBox.DroppedDown := not PresetBox.DroppedDown;
end;

procedure TFormMiniHost.PresetBoxChange(Sender: TObject);
begin
  PanelStatus.SetFocus;
end;

procedure TFormMiniHost.ImageQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with FormOptions do
    if Showing then
      Hide
    else
      Show;
end;

procedure TFormMiniHost.ImageQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    F4PlayStopWAV1Click(Sender);
end;

procedure TFormMiniHost.ImageQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then F5RecStopWAV1Click(Sender);
end;

procedure TFormMiniHost.AddWAV(const FileName: string);
var
  j, i: Integer;
  ms: PShortStr;
begin
  if UpperCase(ExtractFileExt(FileName)) = '.WPL' then
  begin
    {$IFDEF FPC}
    for I := 0 to Player.ListBoxWavFiles.Items.Count - 1 do
      Player.ListBoxWavFiles.Selected[I] := True;
    {$ELSE}
    Player.ListBoxWavFiles.SelectAll;
    {$ENDIF}
    Player.ButtonWavAddClick(nil);
    with TStringList.Create do
    try
      LoadFromFile(FileName);
      for i := 0 to Count - 1 do
        AddWAV(Strings[i]);
      if (Count > 0) and (UpperCase(Strings[0]) = 'RANDOM') then
        Player.ComboBoxWavPlayMode.ItemIndex := 3;
    finally
      Free;
    end;
    if UpperCase(ExtractFileExt(FileName)) <> '.WAV' then
      Exit;
    if not FileExists(FileName) then
      Exit;
    j := -1;
    for i := 0 to Player.ListBoxWavFiles.Items.Count - 1 do
      if string(PShortStr(Player.ListBoxWavFiles.Items.Objects[i])^) = FileName then
        j := 0;
    if j = 0 then
      Exit;
    GetMem(ms, SizeOf(ShortStr));
    ms^ := ShortStr(FileName);
    Player.ListBoxWavFiles.Items.AddObject(ExtractFilename(FileName), TObject(ms));
    Player.ListBoxWavFiles.ItemIndex := Player.ListBoxWavFiles.Items.Count - 1;
  end;
end;

procedure TFormMiniHost.BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssRight] then Player.Show;
end;

procedure TFormMiniHost.MenuItemShowMIDIWAVWindowClick(Sender: TObject);
begin
  Player.Show;
end;

procedure TFormMiniHost.MenuItemDownMixToStereoClick(Sender: TObject);
begin
  MenuItemDownMixToStereo.Checked := not MenuItemDownMixToStereo.Checked;
end;

procedure TFormMiniHost.MenuItemUseMouseWheelClick(Sender: TObject);
begin
  MenuItemUseMouseWheel.Checked := not MenuItemUseMouseWheel.Checked;
end;

procedure TFormMiniHost.TimerIdleTimer(Sender: TObject);
begin
 VSTHost[0].Idle;
 VSTHost[0].EditIdle;
 if not FColBack then
  begin
   FColBack := True;
   if VSTHost[0].Active
    then Self.Color := FindBackgroundColor;
  end;
end;

procedure TFormMiniHost.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  j, i: Integer;
  bs, ChOfs: Integer;
begin
  bs := ASIOHost.BufferSize;
  if (bs <= 0) or (not FAllowed) or (VSTHost = nil) then
    Exit;

  VSTHost.UpdateVstTimeInfo(bs);

  FDataSection.Acquire;
  try
    if FMDataCnt > 0 then
    begin
      FMyEvents.numEvents := FMDataCnt;

      // by Daniel:  this is a critical path, better save this cando in a value
      // instead of calling VstDispatch here
      VSTHost[0].ProcessEvents(FMyEvents);

      if (FCurrentMIDIOut > 0) then
      begin
        for i := 0 to FMDataCnt - 1 do
          FMidiOutput.Send(FCurrentMIDIOut - 1,
            PVstMidiEvent(FMyEvents.events[i])^.midiData[0],
            PVstMidiEvent(FMyEvents.events[i])^.midiData[1],
            PVstMidiEvent(FMyEvents.events[i])^.midiData[2]);
      end;
      FMDataCnt := 0;
    end;
  finally
    FDataSection.Release;
  end;

  ChOfs := FCurrentOutputChannel * 2;
  if FCurrentInputChannel = 0 then
    for i := 0 to bs - 1 do
    begin
      FInBufL[i] := 0;
      FInBufR[i] := 0;
    end
  else
    for i := 0 to bs - 1 do
    begin
      FInBufL[i] := InputVol * InBuffer[(FCurrentInputChannel - 1) * 2, i];
      FInBufR[i] := InputVol * InBuffer[(FCurrentInputChannel - 1) * 2 + 1, i];
    end;

  // fill WavBufL and WavBufR
  if Wavefile.FPMode = wpmPlay then
    for i := 0 to bs - 1 do
      WaveFile.Process(FWavBufL[i], FWavBufR[i])
  else
  begin
    Assert(Length(FWavBufL) >= bs);
    Assert(Length(FWavBufR) >= bs);
    FillChar(FWavBufL[0], bs * SizeOf(Single), 0);
    FillChar(FWavBufR[0], bs * SizeOf(Single), 0);
  end;

  if FNumOut > 0 then
  begin
    // assign Input to VSTBufIn
    for i := 0 to FNumOut - 1 do
      FillChar(FVSTBufOut[i][0], bs * SizeOf(Single), 0);

    if effFlagsIsSynth in VSTHost[0].EffectOptions then
      for i := 0 to FNumIn - 1 do
        FillChar(FVSTBufIn[i][0], bs * SizeOf(Single), 0)
    else
    for i := 0 to bs - 1 do
    begin
      FVSTBufIn[0][i] := (FWavBufL[i] * Wavefile.Volume) + FInBufL[i];
      FVSTBufIn[1][i] := (FWavBufR[i] * Wavefile.Volume) + FInBufR[i];
    end;

    // apply Processing
    if FProcessing then
    begin
      if effFlagsCanReplacing in VSTHost[0].EffectOptions then
        VSTHost[0].Process32Replacing(@FVSTBufIn[0], @FVSTBufOut[0], bs)
      else
        VSTHost[0].Process(@FVSTBufIn[0], @FVSTBufOut[0], bs);
      if FDownMix then
        for i := 0 to bs - 1 do
          for j := 2 to FNumOut - 1 do
          begin
            if FVSTPinProps[j].ArrangementType = satMono then
            begin
              FVSTBufOut[0][i] := FVSTBufOut[0][i] + FVSTBufOut[j][i];
              FVSTBufOut[1][i] := FVSTBufOut[1][i] + FVSTBufOut[j][i];
            end
            else
              FVSTBufOut[j mod 2][i] := FVSTBufOut[j mod 2][i] + FVSTBufOut[j][i];
          end;
    end;

    // assign Output from VSTBufOut
    if FNumOut = 1 then j := 0 else j := 1;
    if effFlagsIsSynth in VSTHost[0].EffectOptions then
      for i := 0 to bs - 1 do
      begin
        OutBuffer[ChOfs][i] := (FVSTBufOut[0][i] * VSTVol + FInBufL[i] + FWavBufL[i] * Wavefile.Volume) * FOverallVol;
        OutBuffer[ChOfs + 1][i] := (FVSTBufOut[j][i] * VSTVol + FInBufR[i] + FWavBufR[i] * Wavefile.Volume) * FOverallVol;
      end
    else
      for i := 0 to bs - 1 do
      begin
        OutBuffer[ChOfs][i] := (FVSTBufOut[0][i] * VSTVol + (1 - VSTVol) * FVSTBufIn[0][i]) * FOverallVol;
        OutBuffer[ChOfs + 1][i] := (FVSTBufOut[j][i] * VSTVol + (1 - VSTVol) * FVSTBufIn[j][i]) * FOverallVol;
      end;
  end
  else
  for i := 0 to bs - 1 do
  begin
    OutBuffer[ChOfs][i] := (FInBufL[i] + FWavBufL[i] * Wavefile.Volume) * FOverallVol;
    OutBuffer[ChOfs + 1][i] := (FInBufR[i] + FWavBufR[i] * Wavefile.Volume) * FOverallVol;
  end;

  if FRecordState = rsRecord then
  begin
    FTotalFrames := FTotalFrames + Integer(ASIOHost.BufferSize);
    with FWavWriter do
      if Format.nChannels = 1 then
        WriteFloatData(OutBuffer[ChOfs], bs)
      else
        WriteFloatDataSeparateStereo(OutBuffer[ChOfs], OutBuffer[ChOfs + 1], bs);
  end;

  // by Daniel: this line messes up, midi data may have changed in the meantime
  // by other thread so this will kill data which was just processed
  // Line has been moved to just after vstPlugin.processEvents has been called
  //  FMDataCnt := 0;
end;

{$IFDEF FPC}
initialization
  {$i MiniHostForm.lrs}
{$ENDIF}

end.
