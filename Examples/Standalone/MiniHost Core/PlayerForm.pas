unit PlayerForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, Classes, Controls, Forms, StdCtrls, ComCtrls;

type
  TPlayer = class(TForm)
    ButtonWavAdd: TButton;
    ButtonWavPause: TButton;
    ButtonWavPlay: TButton;
    ButtonWavRecord: TButton;
    ButtonWavRemove: TButton;
    ButtonWavStop: TButton;
    ButtonWavStopRec: TButton;
    CheckBoxRecInMono: TCheckBox;
    ComboBoxRecordFormat: TComboBox;
    ComboBoxWavPlayMode: TComboBox;
    GroupBoxWavFilePlayer: TGroupBox;
    GroupBoxWavRecorder: TGroupBox;
    LabelCurrentRecordFile: TLabel;
    LabelRecordFile: TLabel;
    LabelStatus: TLabel;
    LabelWavCurrentFile: TLabel;
    LabelWaveFile: TLabel;
    LabelWavPitch: TLabel;
    LabelWavPlayMode: TLabel;
    LabelWavPosition: TLabel;
    ScrollBarPitch: TScrollBar;
    ScrollBarWavPosition: TScrollBar;
    ListBoxWavFiles: TListBox;
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxWavFilesDblClick(Sender: TObject);
    procedure ButtonWavAddClick(Sender: TObject);
    procedure ButtonWavRemoveClick(Sender: TObject);
    procedure ButtonWavPlayClick(Sender: TObject);
    procedure ButtonWavStopClick(Sender: TObject);
    procedure ButtonWavRecordClick(Sender: TObject);
    procedure ButtonWavPauseClick(Sender: TObject);
    procedure ButtonWavStopRecClick(Sender: TObject);
    procedure LabelRecordFileClick(Sender: TObject);
    procedure ComboBoxWavPlayModeChange(Sender: TObject);
    procedure ScrollBarPitchChange(Sender: TObject);
    procedure ScrollBarWavPositionChange(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Player: TPlayer;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, MiniHostForm, OptionsForm, ShellAPI;

var
  FormMiniHost: TFormMiniHost;

procedure TPlayer.WMDropfiles(var msg: TMessage);
var
  Size: Integer;
  Name: PChar;
  s: string;
  i, nCount: Integer;
begin
  inherited;
  nCount := DragQueryFile(msg.WParam, $FFFFFFFF, nil, 0);
  with TStringList.Create do
  try
    for i := 0 to nCount - 1 do
    begin
      Size := DragQueryFile(msg.WParam, i, nil, 0) + 1;
      Name := StrAlloc(size);
      DragQueryFile(msg.WParam, i, Name, Size);
      s := StrPas(Name);
      Add(s);
      StrDispose(Name);
    end;
    DragFinish(msg.WParam);

   if Count = 0 then
     Exit;

   s := UpperCase(ExtractFileExt(Strings[0]));
   if (s = '.WAV') then
     FormMiniHost.AddWAV(Strings[0]);
  finally
    Free;
  end;
end;

procedure TPlayer.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, True);
end;

procedure TPlayer.ListBoxWavFilesDblClick(Sender: TObject);
begin
  ButtonWavPlayClick(Sender);
end;

procedure TPlayer.ButtonWavAddClick(Sender: TObject);
begin
  FormMiniHost.LoadWAVFile;
end;

procedure TPlayer.ButtonWavRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  if ListBoxWavFiles.ItemIndex >= 0 then
  begin
    for i := 0 to ListBoxWavFiles.Items.Count - 1 do
      if ListBoxWavFiles.Selected[i] then
        FreeMem(PShortStr(ListBoxWavFiles.Items.Objects[i]));
    {$IFNDEF FPC}
    ListBoxWavFiles.DeleteSelected;
    {$ENDIF}
  end;
end;

procedure TPlayer.ButtonWavPlayClick(Sender: TObject);
begin
  if (ListBoxWavFiles.ItemIndex >= 0) and (ListBoxWavFiles.Items.Count > 0) then
    with FormMiniHost do
    begin
      LoadWAV(PShortStr(ListBoxWavFiles.Items.Objects[ListBoxWavFiles.ItemIndex])^);
      LabelWaveFile.Caption := ListBoxWavFiles.Items[ListBoxWavFiles.ItemIndex];
      StartPlayback2Click(nil);
    end;
end;

procedure TPlayer.ButtonWavStopClick(Sender: TObject);
begin
  FormMiniHost.StopPlayback2Click(nil)
end;

procedure TPlayer.ButtonWavRecordClick(Sender: TObject);
begin
  with FormMiniHost do
    if RecordState = rsPause then
      RecordState := rsRecord
    else
    if RecordState = rsStop then
      MenuItemStartRecordingClick(nil);
end;

procedure TPlayer.ButtonWavPauseClick(Sender: TObject);
begin
  with FormMiniHost do
    if RecordState = rsRecord then
      RecordState := rsPause;
end;

procedure TPlayer.ButtonWavStopRecClick(Sender: TObject);
begin
  FormMiniHost.MenuItemStopRecordingClick(nil);
end;

procedure TPlayer.LabelRecordFileClick(Sender: TObject);
begin
  FormMiniHost.RecordWAVFileSelect;
end;

constructor TPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FormMiniHost := AOwner as TFormMiniHost;
end;

procedure TPlayer.ComboBoxWavPlayModeChange(Sender: TObject);
begin
  FormMiniHost.WaveFile.Looped := ComboBoxWavPlayMode.ItemIndex = 1;
end;

procedure TPlayer.ScrollBarPitchChange(Sender: TObject);
begin
  FormMiniHost.Wavefile.Speed := 2 * ScrollBarPitch.Position / 341;
  LabelWavPitch.Caption := 'pitch: ' + IntToStr(Round(200 * ScrollBarPitch.Position / 341)) + ' %';
end;

procedure TPlayer.ScrollBarWavPositionChange(Sender: TObject);
begin
  FormMiniHost.Wavefile.SetPos(Round((FormMiniHost.Wavefile.Size - 1) * ScrollBarWavPosition.Position * 0.01));
  LabelWavPosition.Caption := 'position: ' + IntToStr(ScrollBarWavPosition.Position) + ' %';
end;

{$IFDEF FPC}
initialization
  {$i PlayerForm.lrs}
{$ENDIF}

end.
