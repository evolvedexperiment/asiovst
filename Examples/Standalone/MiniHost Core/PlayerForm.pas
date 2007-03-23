unit PlayerForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls;

type
  TPlayer = class(TForm)
    GroupBox4: TGroupBox;
    MidiBox: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    onlych1: TCheckBox;
    GroupBox1: TGroupBox;
    WavBox: TListBox;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    wavfile: TLabel;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    recinmono: TCheckBox;
    rstatus: TLabel;
    rformat: TComboBox;
    mode1: TComboBox;
    Label9: TLabel;
    mode2: TComboBox;
    Label10: TLabel;
    s_tempo: TScrollBar;
    s_pos: TScrollBar;
    s_pitch: TScrollBar;
    s_pos2: TScrollBar;
    procedure wmdropfiles(var msg: tmessage); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
    procedure MidiBoxDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WavBoxDblClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure wavfileClick(Sender: TObject);
    procedure mode2Change(Sender: TObject);
    procedure s_tempoChange(Sender: TObject);
    procedure s_posChange(Sender: TObject);
    procedure s_pitchChange(Sender: TObject);
    procedure s_pos2Change(Sender: TObject);
  private
    tmp: TLabel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var Player: TPlayer;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses MiniHostForm, OptionsForm, ShellAPI;

var FmMiniHost: TFmMiniHost;

procedure TPlayer.WMDropfiles(var msg: tmessage);
var size: Integer;
    name: pchar;
    s: string;
    t: TStringList;
    i, nCount: Integer;
begin
 inherited;
 nCount := DragQueryFile(msg.WParam, $FFFFFFFF, nil, 0);
 t := TStringList.Create;
 for i := 0 to nCount - 1 do
 begin
  size := DragQueryFile(msg.WParam, i, nil, 0) + 1;
  name := StrAlloc(size);
  DragQueryFile(msg.WParam, i, name, size);
  s := StrPas(name);
  t.Add(s);
  StrDispose(name);
 end;
 DragFinish(msg.WParam);

 if t.Count = 0 then exit;
 s := UpperCase(ExtractFileExt(t.Strings[0]));
 if (s = '.MID') then
  FmMiniHost.AddMID(t.Strings[0])
 else if (s = '.WAV') then
  FmMiniHost.AddWAV(t.Strings[0]);
 t.free;
end;

procedure TPlayer.FormCreate(Sender: TObject);
begin
 DragAcceptFiles(Self.Handle, true);
end;

procedure TPlayer.MidiBoxDblClick(Sender: TObject);
begin
 Button4Click(Sender);
end;

procedure TPlayer.Button1Click(Sender: TObject);
begin
 //add
 FmMiniHost.LoadMIDIFile1Click(Sender);
end;

procedure TPlayer.Button2Click(Sender: TObject);
var i: Integer;
begin
 //remove
 if MidiBox.ItemIndex >= 0 then
 begin
  for i := 0 to MidiBox.Items.Count - 1 do
   if MidiBox.Selected[i] then
    FreeMem(pshortstr(MidiBox.Items.Objects[i]));
{$IFNDEF FPC}
  MidiBox.DeleteSelected;
{$ENDIF}
 end;
end;

procedure TPlayer.Button4Click(Sender: TObject);
begin
 //play
 with FmMiniHost do
 begin
  MidiFile.StopPlaying;
  MidiPlaying := false;
  Panic1Click(nil);
  if (MidiBox.ItemIndex >= 0) and (MidiBox.Items.Count > 0) then
  begin
   MidiFile.Filename := pshortstr(MidiBox.Items.objects[MidiBox.itemindex])^;
   Label2.Caption := MidiBox.Items[MidiBox.itemindex];
   MidiFile.ReadFile;
   s_tempo.position := MidiFile.Bpm;
   MidiFile.StartPlaying;
   MidiPlaying := true;
  end; 
 end;
end;

procedure TPlayer.Button3Click(Sender: TObject);
begin
 //stop
 with FmMiniHost do
 begin
  MidiFile.StopPlaying;
  MidiPlaying := false;
  Panic1Click(nil);
 end;
end;

procedure TPlayer.WavBoxDblClick(Sender: TObject);
begin
 Button8Click(sender);
end;

procedure TPlayer.Button5Click(Sender: TObject);
begin
 //add
 FmMiniHost.WAVFile1Click(Sender);
end;

procedure TPlayer.Button6Click(Sender: TObject);
var i: Integer;
begin
 //remove
 if WavBox.ItemIndex >= 0 then
  begin
   for i := 0 to WavBox.Items.Count - 1 do
    if WavBox.Selected[i] then
     FreeMem(pshortstr(WavBox.Items.Objects[i]));
 {$IFNDEF FPC}
   WavBox.DeleteSelected;
 {$ENDIF}
  end;
end;

procedure TPlayer.Button8Click(Sender: TObject);
begin
 //play
 if (WavBox.ItemIndex >= 0) and (WavBox.Items.Count > 0) then
  with FmMiniHost do
   begin
    LoadWAV(pshortstr(WavBox.Items.Objects[WavBox.itemindex])^);
    Label4.Caption := WavBox.Items[WavBox.itemindex];
    StartPlayback2Click(nil);
   end;
end;

procedure TPlayer.Button7Click(Sender: TObject);
begin
 //stop
 FmMiniHost.StopPlayback2Click(nil)
end;

procedure TPlayer.Button12Click(Sender: TObject);
begin
 //rec
 with FmMiniHost do
  if recording = 2 then recording := 1
  else if recording = 0 then
   StartRecording1Click(nil);
end;

procedure TPlayer.Button10Click(Sender: TObject);
begin
 //pause
 with FmMiniHost do
  if recording = 1 then recording := 2;
end;

procedure TPlayer.Button11Click(Sender: TObject);
begin
 //stop
 FmMiniHost.StopRecording1Click(nil);
end;

procedure TPlayer.wavfileClick(Sender: TObject);
begin
 FmMiniHost.RecordWAVfile1Click(Sender);
end;

constructor TPlayer.Create(AOwner: TComponent);
begin
 inherited;
 FmMiniHost := AOwner as TFmMiniHost; 
end;

procedure TPlayer.mode2Change(Sender: TObject);
begin
 FmMiniHost.WaveFile.looped := mode2.ItemIndex = 1;
end;

procedure TPlayer.s_tempoChange(Sender: TObject);
begin
 FmMiniHost.MidiFile.Bpm := s_tempo.position;
 tmp.caption := 'tempo: ' + inttostr(s_tempo.position) + ' bpm';
end;

procedure TPlayer.s_posChange(Sender: TObject);
begin
 if (FmMiniHost.MIDIPlaying) then
 begin
  FmMiniHost.Timer1.Enabled := false;
  FmMiniHost.MidiFile.stopplaying;
  FmMiniHost.Panic1Click(sender);
  FmMiniHost.MidiFile.PlayToTime(
   round(FmMiniHost.MidiFile.GetTrackLength * s_pos.position * 0.01));
  FmMiniHost.MidiFile.continueplaying;
  FmMiniHost.Timer1.Enabled := true;
 end;
 Label5.caption := 'position: ' + inttostr(s_pos.position) + ' %';
end;

procedure TPlayer.s_pitchChange(Sender: TObject);
begin
 FmMiniHost.Wavefile.speed := 2 * s_pitch.position / 341;
 Label6.caption := 'pitch: ' + inttostr(round(200 * s_pitch.position / 341)) + ' %';
end;

procedure TPlayer.s_pos2Change(Sender: TObject);
begin
 FmMiniHost.Wavefile.SetPos(round((FmMiniHost.Wavefile.size - 1) * s_pos2.position * 0.01));
 Label7.caption := 'position: ' + inttostr(s_pos2.position) + ' %';
end;

{$IFDEF FPC}
initialization
  {$i PlayerForm.lrs}
{$ENDIF}

end.


