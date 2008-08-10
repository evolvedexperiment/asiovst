unit AEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, DAVDCommon, DGuiStaticWaveform,
  DGuiBaseControl, DGuiLevelMeter, DAudioFile, DAudioFileWav, DASIOHost;

type
  TFmAudioEditor = class(TForm)
    ASIOHost: TASIOHost;
    BtPause: TToolButton;
    BtPlay: TToolButton;
    GuiLevelMeter: TGuiLevelMeter;
    GuiStaticWaveform: TGuiStaticWaveform;
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

procedure TFmAudioEditor.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAudioEditor.MINormalizeClick(Sender: TObject);
var
  ch, i : Integer;
  max   : Double;
begin
 with GuiStaticWaveform do
  begin
   max := 0;

   // scan for maximum
   for ch := 0 to WaveChannels - 1 do
    for i := 0 to WaveLength - 1 do
     if abs(Wavedata[ch, i]) > max
      then max := abs(Wavedata[ch, i]);

   // actually normalize
   if max > 0 then
    begin
     max := 1 / max;
     for ch := 0 to WaveChannels - 1 do
      for i := 0 to WaveLength - 1 do
       begin
        Wavedata[ch, i] := max * Wavedata[ch, i];
       end;
    end;

   // Redraw data
   RedrawBuffer(True);
  end;
end;

procedure TFmAudioEditor.MIRectifyClick(Sender: TObject);
var
  ch, i : Integer;
begin
 with GuiStaticWaveform do
  begin
   // rectify every sample
   for ch := 0 to WaveChannels - 1 do
    for i := 0 to WaveLength - 1
     do Wavedata[ch, i] := abs(Wavedata[ch, i]);
   RedrawBuffer(True);
  end;
end;

procedure TFmAudioEditor.MIRemoveDCClick(Sender: TObject);
var
  ch, i   : Integer;
  Sum, DC : Double;
begin
 with GuiStaticWaveform do
  begin
   for ch := 0 to WaveChannels - 1 do
    begin
     // build sum of
     Sum := 0;
     for i := 0 to WaveLength - 1
      do Sum := Sum + Wavedata[ch, i];

     DC := Sum / WaveLength;
     for i := 0 to WaveLength - 1
      do Wavedata[ch, i] := Wavedata[ch, i] - DC;
    end;
   RedrawBuffer(True);  
  end;
end;

procedure TFmAudioEditor.MIInvertClick(Sender: TObject);
var
  ch, i : Integer;
begin
 with GuiStaticWaveform do
  begin
   // rectify every sample
   for ch := 0 to WaveChannels - 1 do
    for i := 0 to WaveLength - 1
     do Wavedata[ch, i] := -Wavedata[ch, i];
   RedrawBuffer(True);
  end;
end;

procedure TFmAudioEditor.MINoiseClick(Sender: TObject);
var
  ch, i : Integer;
begin
 with GuiStaticWaveform do
  begin
   WaveChannels := 2;
   WaveLength   := round(ASIOHost.SampleRate);
   for ch := 0 to WaveChannels - 1 do
    for i := 0 to WaveLength - 1
     do Wavedata[ch, i] := 2 * random - 1;
   RedrawBuffer(True);
  end;
end;

procedure TFmAudioEditor.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'Wave File (*.wav)|*.wav';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load Audio File';
   if Execute then
    begin
     fFileName := FileName;
     with TMFCustomAudioFileWAV.Create(Self) do
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
     with TMFCustomAudioFileWAV.Create(Self) do
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
