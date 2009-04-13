unit AEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, StdCtrls, Buttons, DAV_Common,
  DAV_GuiStaticWaveform, DAV_GuiBaseControl, DAV_GuiLevelMeter, DAV_AudioFile,
  DAV_AudioData, DAV_AudioFileWav, DAV_AudioFileAIFF, DAV_AudioFileAU,
  DAV_ASIOHost, DAV_VSTHost, DAV_GuiAudioDataDisplay;

type
  TFmAudioEditor = class(TForm)
    ASIOHost: TASIOHost;
    AudioDataCollection32: TAudioDataCollection32;
    ControlBar1: TControlBar;
    GuiAudioDataDisplay: TGuiAudioDataDisplay;
    GuiLevelMeter: TGuiLevelMeter;
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MIASIOSetup: TMenuItem;
    MIEdit: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIGenerate: TMenuItem;
    MIHelp: TMenuItem;
    MIInvert: TMenuItem;
    MINew: TMenuItem;
    MINoise: TMenuItem;
    MINormalize: TMenuItem;
    MIOpen: TMenuItem;
    MIOptions: TMenuItem;
    MIProcess: TMenuItem;
    MIRectify: TMenuItem;
    MIRemoveDC: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MIView: TMenuItem;
    N1: TMenuItem;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    MIUndo: TMenuItem;
    MIRedo: TMenuItem;
    N2: TMenuItem;
    MIAusschneiden: TMenuItem;
    MIKopieren: TMenuItem;
    MIPaste: TMenuItem;
    MIDelete: TMenuItem;
    N3: TMenuItem;
    MISelectAll: TMenuItem;
    MISelectNone: TMenuItem;
    MIWaveform: TMenuItem;
    WhiteNoise1: TMenuItem;
    MIPinkNoise: TMenuItem;
    VstHost: TVstHost;
    MIVstSetup: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIASIOSetupClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MINormalizeClick(Sender: TObject);
    procedure MIWhiteNoiseClick(Sender: TObject);
    procedure MIRectifyClick(Sender: TObject);
    procedure MIRemoveDCClick(Sender: TObject);
    procedure MIInvertClick(Sender: TObject);
    procedure DataChangedHandler(Sender: TObject);
    procedure MIVstSetupClick(Sender: TObject);
  private
    FFileName : TFileName;
  public
    property FileName: TFileName read FFileName;
  end;

var
  FmAudioEditor: TFmAudioEditor;

implementation

{$R *.dfm}

uses
  AEAsioSetup, AEVstSetup;

procedure TFmAudioEditor.DataChangedHandler(Sender: TObject);
begin
 GuiAudioDataDisplay.Invalidate;
end;

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
  chdata : PDAVSingleFixedArray;
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

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIRectifyClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
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

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIRemoveDCClick(Sender: TObject);
var
  ch, i   : Integer;
  Sum, DC : Double;
  chdata  : PDAVSingleFixedArray;
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

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIInvertClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
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

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIWhiteNoiseClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
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

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'All known files |*.wav;*.aif*;*.au|' +
             'Wave File (*.wav)|*.wav|' +
             'AIFF File (*.aif)|*.aif*|' +
             'AU File (*.au)|*.au;*.snd';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load Audio File';
   if Execute then
    begin
     FFileName := FileName;
     AudioDataCollection32.LoadFromFile(FileName);
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
   Filter := 'All known files |*.wav;*.aif*;*.au|' +
             'Wave File (*.wav)|*.wav|' +
             'AIFF File (*.aif)|*.aif*|' +
             'AU File (*.au)|*.au;*.snd';
   Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
   Title := 'Save Audio File';
   if Execute then
    begin
     FFileName := FileName;
     AudioDataCollection32.SaveToFile(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmAudioEditor.MIVstSetupClick(Sender: TObject);
begin
 FmVstSetup.ShowModal;
end;

procedure TFmAudioEditor.MIASIOSetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

end.
