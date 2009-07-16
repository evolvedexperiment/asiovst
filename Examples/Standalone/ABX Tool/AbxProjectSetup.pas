unit AbxProjectSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_AudioFile, DAV_AudioFileWAV;

type
  TFmProjectSetup = class(TForm)
    BtCancel: TButton;
    BtOk: TButton;
    BtSelectA: TButton;
    BtSelectB: TButton;
    CBProtectSettings: TCheckBox;
    EdFilenameA: TEdit;
    EdFilenameB: TEdit;
    LbSelectA: TLabel;
    LbSelectB: TLabel;
    procedure BtOkClick(Sender: TObject);
    procedure BtSelectAClick(Sender: TObject);
    procedure BtSelectBClick(Sender: TObject);
    procedure EdFilenameChange(Sender: TObject);
  private
  public
  end;

implementation

uses
  Inifiles, AbxMain;

{$R *.dfm}

resourcestring
  RCStrFileDoesNotExist = 'File does not exist anymore!';
  RCStrWaveFileFilter = 'Wave File (*.wav)|*.wav';
  RCStrIdenticalFiles = 'You must choose two different files for comparison';
  RCStrInvalidWavFile = 'File %s is not a valid WAV file!';
  RCStrSamplerateMismatch = 'Samplerate mismatch!';

procedure TFmProjectSetup.BtOkClick(Sender: TObject);
var
  SR : Single;
begin
 try
  if not (FileExists(EdFilenameA.Text) and FileExists(EdFilenameB.Text))
   then raise Exception.Create(RCStrFileDoesNotExist);
  if EdFilenameA.Text = EdFilenameB.Text
   then raise Exception.Create(RCStrIdenticalFiles);

  with TAudioFileWAV.Create(Self) do
   try
    LoadFromFile(EdFilenameA.Text);
    SR := SampleRate;
    LoadFromFile(EdFilenameB.Text);
    if SR <> SampleRate
     then raise Exception.Create(RCStrSamplerateMismatch);
   finally
    Free;
   end;
 except
  on E: Exception do
   begin
    ModalResult := mrNone;
    MessageDlg('Error: ' + E.Message, mtError, [mbOK], 0);
   end;
 end;
end;

procedure TFmProjectSetup.BtSelectAClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filter := RCStrWaveFileFilter;
   Options := Options + [ofFileMustExist];
   with TIniFile.Create(FmAbxMain.IniFileName) do
    try
     InitialDir := ReadString('Recent', 'Audio Directory', InitialDir);
    finally
     Free;
    end;
   if Execute then
    begin
     if not TCustomAudioFileWAV.CanLoad(FileName)
      then raise Exception.CreateFmt(RCStrInvalidWavFile, [FileName]);

     EdFilenameA.Text := FileName;
     InitialDir := ExtractFileDir(FileName);
     with TIniFile.Create(FmAbxMain.IniFileName) do
      try
       WriteString('Recent', 'Audio Directory', InitialDir);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmProjectSetup.BtSelectBClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filter := RCStrWaveFileFilter;
   Options := Options + [ofFileMustExist];
   with TIniFile.Create(FmAbxMain.IniFileName) do
    try
     InitialDir := ReadString('Recent', 'Audio Directory', InitialDir);
    finally
     Free;
    end;
   if Execute then
    begin
     if not TCustomAudioFileWAV.CanLoad(FileName)
      then raise Exception.CreateFmt(RCStrInvalidWavFile, [FileName]);

     EdFilenameB.Text := FileName;
     InitialDir := ExtractFileDir(FileName);
     with TIniFile.Create(FmAbxMain.IniFileName) do
      try
       WriteString('Recent', 'Audio Directory', InitialDir);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmProjectSetup.EdFilenameChange(Sender: TObject);
begin
 BtOk.Enabled := FileExists(EdFilenameA.Text) and
                 FileExists(EdFilenameB.Text);
end;

end.
