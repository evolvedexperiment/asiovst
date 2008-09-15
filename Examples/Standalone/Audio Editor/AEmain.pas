unit AEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, DAV_Common, DAV_GuiStaticWaveform,
  DAV_GuiBaseControl, DAV_GuiLevelMeter, DAV_AudioFile, DAV_AudioFileWav,
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_ASIOHost, DAV_AudioData,
  DAV_GuiAudioDataDisplay;

type
  TFmAudioEditor = class(TForm)
    ASIOHost: TASIOHost;
    BtPause: TToolButton;
    BtPlay: TToolButton;
    GuiLevelMeter: TGuiLevelMeter;
    MainMenu: TMainMenu;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIGenerate: TMenuItem;
    MIInvert: TMenuItem;
    MINoise: TMenuItem;
    MINormalize: TMenuItem;
    MIOpen: TMenuItem;
    MIProcess: TMenuItem;
    MIRectify: TMenuItem;
    MIRemoveDC: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MISetup: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    ToolBar: TToolBar;
    GuiAudioDataDisplay: TGuiAudioDataDisplay;
    AudioDataCollection32: TAudioDataCollection32;
    procedure FormCreate(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MISetupClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MINormalizeClick(Sender: TObject);
    procedure MINoiseClick(Sender: TObject);
    procedure MIRectifyClick(Sender: TObject);
    procedure MIRemoveDCClick(Sender: TObject);
    procedure MIInvertClick(Sender: TObject);
  private
    fFileName : TFileName;
  public
    property FileName: TFileName read fFileName;
  end;

var
  FmAudioEditor: TFmAudioEditor;

implementation

{$R *.dfm}

uses
  WaveIOX, AESetup;

procedure TFmAudioEditor.FormCreate(Sender: TObject);
var
  Sample: Integer;
begin
 for Sample := 0 to AudioDataCollection32.SampleFrames - 1 do
  begin
   AudioDataCollection32[0].ChannelData[Sample] := 2 * random - 1;
  end;
end;

procedure TFmAudioEditor.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAudioEditor.MINormalizeClick(Sender: TObject);
var
  ch, i  : Integer;
  max    : Double;
  chdata : PAVDSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   max := 0;

   // scan for maximum
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointerList[ch];
     for i := 0 to SampleFrames - 1 do
      if abs(chdata^[i]) > max
       then max := abs(chdata^[i]);
    end;

   // actually normalize
   if max > 0 then
    begin
     max := 1 / max;
     for ch := 0 to ChannelCount - 1 do
      begin
       chdata := ChannelDataPointerList[ch];
       for i := 0 to SampleFrames - 1
        do chdata^[i] := max * chdata^[i];
      end;
    end;

   Invalidate
  end;
end;

procedure TFmAudioEditor.MIRectifyClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PAVDSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   // rectify every sample
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointerList[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := abs(chdata^[i]);
    end;

   Invalidate
  end;
end;

procedure TFmAudioEditor.MIRemoveDCClick(Sender: TObject);
var
  ch, i   : Integer;
  Sum, DC : Double;
  chdata  : PAVDSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointerList[ch];

     // build sum of
     Sum := 0;
     for i := 0 to SampleFrames - 1
      do Sum := Sum + chdata^[i];

     DC := Sum / SampleFrames;
     for i := 0 to SampleFrames - 1
      do chdata^[i] := chdata^[i] - DC;
    end;

   Invalidate
  end;
end;

procedure TFmAudioEditor.MIInvertClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PAVDSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   // rectify every sample
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointerList[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := -chdata^[i];
    end;

   Invalidate
  end;
end;

procedure TFmAudioEditor.MINoiseClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PAVDSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
//   ChannelCount := 2;
   SampleFrames   := round(ASIOHost.SampleRate);
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointerList[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := 2 * random - 1;
    end;

   Invalidate
  end;
end;

procedure TFmAudioEditor.MIOpenClick(Sender: TObject);
var
  AudioFile : TCustomAudioFile;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'Wave File (*.wav)|*.wav|' +
             'AIFF File (*.aif)|*.aif*|' +
             'AU File (*.au)|*.au';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load Audio File';
   if Execute then
    begin
     fFileName := FileName;

     // select file format
     case FilterIndex of
       1 : AudioFile := TCustomAudioFileWAV.Create(Self);
       2 : AudioFile := TCustomAudioFileAIFF.Create(Self);
       3 : AudioFile := TCustomAudioFileAU.Create(Self);
      else raise Exception.Create('file format unknown');
     end;

     // if file format selected, load file
     with AudioFile do
      try
       LoadFromFile(FileName);
      finally
       Free;
      end;
      
    end;
  finally
   Free;
  end;
end;

procedure TFmAudioEditor.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'Wave File (*.wav)|*.wav';
   Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
   Title := 'Save Audio File';
   if Execute then
    begin
     fFileName := FileName;
     with TCustomAudioFileWAV.Create(Self) do
      try
       SaveToFile(FileName);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmAudioEditor.MISetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

end.
