unit CafMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, DAV_Common, DAV_AudioFile, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, ExtCtrls, Spin;

type
  TFmConvertAudioFile = class(TForm)
    CbEncoding: TComboBox;
    EdBitsPerSample: TEdit;
    EdChannel: TEdit;
    EdEncoding: TEdit;
    EdSampleFrames: TEdit;
    EdSampleRate: TEdit;
    EdTotalTime: TEdit;
    LbBitsPerSample: TLabel;
    LbChannel: TLabel;
    LbEncoding: TLabel;
    LbNew: TLabel;
    LbNoFileLoaded: TLabel;
    LbOld: TLabel;
    LbSampleFrames: TLabel;
    LbSampleRate: TLabel;
    LbTotalTime: TLabel;
    MainMenu: TMainMenu;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiOpen: TMenuItem;
    MiSave: TMenuItem;
    MiSaveAs: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PnAudioDetails: TPanel;
    SaveDialog: TSaveDialog;
    SEBitsPerSample: TSpinEdit;
    SeSampleRate: TSpinEdit;
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenClick(Sender: TObject);
    procedure MiSaveAsClick(Sender: TObject);
  private
    FAudioFile  : TCustomAudioFile;
    FFileName   : TFileName;
    procedure SaveToFile(FileName: TFileName);
  public
    procedure LoadFromFile(FileName: TFileName);
  end;

var
  FmConvertAudioFile: TFmConvertAudioFile;

implementation

resourcestring
  RCStrDataOffsetNotFound = 'Data offset couldn''t be found';

{$R *.dfm}

procedure TFmConvertAudioFile.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmConvertAudioFile.MiOpenClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then LoadFromFile(OpenDialog.FileName);
end;

procedure TFmConvertAudioFile.MiSaveAsClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then SaveToFile(SaveDialog.FileName);
end;

procedure TFmConvertAudioFile.LoadFromFile(FileName: TFileName);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := FilenameToFileFormat(OpenDialog.FileName);
 if AudioFileClass <> nil then
  begin
   // check if a previous file has been loaded
   if assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   // create and load audio file from stream (only headers)
   FAudioFile := AudioFileClass.Create(FileName);

  (*
   // seek data offset
   if FAudioFile.DataOffset > 0
    then FFileStream.Seek(FAudioFile.DataOffset, soBeginning)
    else raise Exception.Create(RCStrDataOffsetNotFound);
  *)

   EdChannel.Text := IntToStr(FAudioFile.ChannelCount);
   EdSampleFrames.Text := IntToStr(FAudioFile.SampleFrames);
   EdTotalTime.Text := FloatToStrF(FAudioFile.TotalTime, ffGeneral, 5, 5) + ' s';
   EdSampleRate.Text := FloatToStr(FAudioFile.SampleRate) + ' Hz';
   SeSampleRate.Value := round(FAudioFile.SampleRate);
   if FAudioFile is TAudioFileWAV then
    with TAudioFileWAV(FAudioFile) do
     begin
      EdBitsPerSample.Text := IntToStr(BitsPerSample);
      SEBitsPerSample.Value := BitsPerSample;
      case Encoding of
        aeInteger : EdEncoding.Text := 'Integer';
          aeFloat : EdEncoding.Text := 'Float';
            aeMP3 : EdEncoding.Text := 'MP3';
            aeACM : EdEncoding.Text := 'ACM';
          aeADPCM : EdEncoding.Text := 'ADPCM';
        aeMSADPCM : EdEncoding.Text := 'MS ADPCM';
       aeDVIADPCM : EdEncoding.Text := 'DVI ADPCM';
          aeMuLaw : EdEncoding.Text := 'µ-Law';
           aeALaw : EdEncoding.Text := 'A-Law';
      end;
      if Integer(Encoding) < CbEncoding.Items.Count
       then CbEncoding.ItemIndex := Integer(Encoding);

      FFileName := OpenDialog.FileName;
      MiSaveAs.Enabled := FFileName <> '';
      MiSave.Enabled := FFileName <> '';
      PnAudioDetails.Visible := True;
     end;
  end;
end;

procedure TFmConvertAudioFile.SaveToFile(FileName: TFileName);
begin
 if FFileName = FileName
  then raise Exception.Create('not yet supported');

 FFileName := SaveDialog.FileName;
 MiSave.Enabled := FFileName <> '';
end;

end.
