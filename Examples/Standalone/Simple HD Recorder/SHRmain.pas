unit SHRmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_Common, DAV_GuiLabel, DAV_GuiButton, DAV_GuiBaseControl, DAV_GuiLED, ExtCtrls,
  DAV_ASIOHost;

type
  TStorageThread = class(TThread)
  private
    FCurrentIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
  end;

  TFmSimpleHDRecorder = class(TForm)
    GuiLED1: TGuiLED;
    BtStartStop: TGuiButton;
    GuiLED2: TGuiLED;
    GuiLED3: TGuiLED;
    GuiLED4: TGuiLED;
    LbMic1: TGuiLabel;
    LbMic2: TGuiLabel;
    LbMic3: TGuiLabel;
    LbMic4: TGuiLabel;
    BtSetup: TGuiButton;
    BtExit: TGuiButton;
    Timer: TTimer;
    ASIOHost: TASIOHost;
    procedure TimerTimer(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtSetupClick(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure ASIOHostBufferSwitch32Recording(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
  private
    FBackgrounBitmap : TBitmap;
    FPeaks           : Array [0..3] of Single;
    FDataBuffers     : Array [0..1, 0..3] of PDAVSingleFixedArray;
    FDataBufferIndex : Integer;
    FDataBufferPos   : Integer;
    FDataBufferSize  : Integer;
    FRecordTimeInSec : Integer;      
    FStorageThread   : TStorageThread;
    procedure PrepareRecording;
    procedure SwitchRecordingBuffer;
    procedure StopRecording;
  public
    procedure StoreData(const Index: Integer);
    property DataBufferIndex: Integer read FDataBufferIndex;
  end;

var
  FmSimpleHDRecorder: TFmSimpleHDRecorder;

implementation

uses
  Math, IniFiles, WaveIOX, DAV_GuiCommon, SHRSetup;

{$R *.dfm}

{ StorageThread }

constructor TStorageThread.Create(CreateSuspended: Boolean);
begin
 inherited Create(CreateSuspended);
 FCurrentIndex := 0;
end;

procedure TStorageThread.Execute;
begin
 while not Terminated do
  begin
   if FCurrentIndex <> FmSimpleHDRecorder.DataBufferIndex then
    begin
     FCurrentIndex := FmSimpleHDRecorder.DataBufferIndex;
     try
      FmSimpleHDRecorder.StoreData(1 - FCurrentIndex);
     finally
      Suspend;
     end;
    end;
   sleep(10);
  end;
end;


{ TFmSimpleHDRecorder }

procedure TFmSimpleHDRecorder.ASIOHostBufferSwitch32Recording(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
var
  sample, ch : Integer;
begin
 for sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   for ch := 0 to min(4, ASIOHost.InputChannelCount) - 1 do
    begin
     FPeaks[ch] := 0.999 * FPeaks[ch];
     if abs(InBuffer[ch, sample]) > FPeaks[ch]
      then FPeaks[ch] := abs(InBuffer[ch, sample]);
     FDataBuffers[FDataBufferIndex, ch]^[FDataBufferPos] := InBuffer[ch, sample];
    end;
   Inc(FDataBufferPos);
   if FDataBufferPos >= FDataBufferSize then SwitchRecordingBuffer;
  end;
end;

procedure TFmSimpleHDRecorder.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
var
  sample, ch : Integer;
begin
 for sample := 0 to ASIOHost.BufferSize - 1 do
  for ch := 0 to min(4, ASIOHost.InputChannelCount) - 1 do
   begin
    FPeaks[ch] := 0.999 * FPeaks[ch];
    if abs(InBuffer[ch, sample]) > FPeaks[ch]
     then FPeaks[ch] := abs(InBuffer[ch, sample]);
   end;
end;

procedure TFmSimpleHDRecorder.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmSimpleHDRecorder.BtSetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmSimpleHDRecorder.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := 'Stop';
   Caption := 'Simple HD Recorder (running)';
   PrepareRecording;
  end
 else
  begin
   ASIOHost.Active := False;
   BtStartStop.Caption := 'Start';
   Caption := 'Simple HD Recorder';
   StopRecording;
  end;
end;

procedure TFmSimpleHDRecorder.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
  h, hr  : Single;
begin
 // Create Background Image
 fBackgrounBitmap := TBitmap.Create;
 with fBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width       := Self.Width;
   Height      := Self.Height;
   hr          := 1 / Height;
   s[0]        := 0;
   s[1]        := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.6 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := round($3F + $8 * (h + s[1]));
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

 FPeaks[0] := 0; // reset peaks
 FPeaks[1] := 0; // reset peaks
 FPeaks[2] := 0; // reset peaks
 FPeaks[3] := 0; // reset peaks
end;

procedure TFmSimpleHDRecorder.FormDestroy(Sender: TObject);
begin
 FreeAndNil(fBackgrounBitmap);
 if assigned(FStorageThread) then
  begin
   if FStorageThread.Suspended then FStorageThread.Resume;
   FStorageThread.Terminate;
   FStorageThread.WaitFor;
   FreeAndNil(FStorageThread);
  end;
 Dispose(FDataBuffers[0, 0]);
 Dispose(FDataBuffers[0, 1]);
 Dispose(FDataBuffers[0, 2]);
 Dispose(FDataBuffers[0, 3]);
 Dispose(FDataBuffers[1, 0]);
 Dispose(FDataBuffers[1, 1]);
 Dispose(FDataBuffers[1, 2]);
 Dispose(FDataBuffers[1, 3]);
end;

procedure TFmSimpleHDRecorder.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, fBackgrounBitmap);
end;

procedure TFmSimpleHDRecorder.PrepareRecording;
begin
 FDataBufferSize := round(ASIOHost.SampleRate * 10);
 ReallocMem(FDataBuffers[0, 0], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 1], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 2], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 3], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 0], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 1], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 2], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 3], FDataBufferSize * SizeOf(Single));
 FDataBufferIndex := 0;
 FDataBufferPos := 0;

 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
  try
   if ReadBool('Application', 'Use Threads', True) then
    begin
     if assigned(FStorageThread) then
      begin
       if FStorageThread.Suspended then FStorageThread.Resume;
       FStorageThread.Terminate;
       FStorageThread.WaitFor;
       FreeAndNil(FStorageThread);
      end;
     FStorageThread := TStorageThread.Create;
    end;
   FRecordTimeInSec := ReadInteger('Application', 'Block Time', 10);
  finally
   Free;
  end;
 ASIOHost.OnBufferSwitch32 := ASIOHostBufferSwitch32Recording;
end;

procedure TFmSimpleHDRecorder.StoreData(const Index: Integer);
var
  Y, H, M, D, S, MO : Word;
  FileBaseName   : TFileName;
begin
 DecodeTime(Now, H, M, S, MO);
 DecodeDate(Now, Y, MO, D);
 FileBaseName := 'Rec' + IntToStr(Y) + '-' + IntToStr(MO) + '-' +
   IntToStr(D) + '-' + IntToStr(H)+ '-' + IntToStr(M) + '-' + IntToStr(S);
 SaveWAVFile(FileBaseName + '-Ch1.wav', @FDataBuffers[Index, 0]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch2.wav', @FDataBuffers[Index, 1]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch3.wav', @FDataBuffers[Index, 2]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch4.wav', @FDataBuffers[Index, 3]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
end;

procedure TFmSimpleHDRecorder.SwitchRecordingBuffer;
begin
 if not assigned(FStorageThread)
  then StoreData(FDataBufferIndex)
  else FStorageThread.Resume;
 FDataBufferIndex := 1 - FDataBufferIndex;
 FDataBufferPos := 0;
end;

procedure TFmSimpleHDRecorder.StopRecording;
var
  Y, H, M, D, MO : Word;
  FileBaseName   : TFileName;
begin
 DecodeTime(Now, H, M, D, MO);
 DecodeDate(Now, Y, MO, D);
 FileBaseName := 'Rec' + IntToStr(Y) + '-' + IntToStr(MO) + '-' +
   IntToStr(D) + '-' + IntToStr(H)+ '-' + IntToStr(M);
 SaveWAVFile(FileBaseName + '-Ch1.wav', FDataBuffers[FDataBufferIndex, 0], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch2.wav', FDataBuffers[FDataBufferIndex, 1], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch3.wav', FDataBuffers[FDataBufferIndex, 2], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch4.wav', FDataBuffers[FDataBufferIndex, 3], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
end;

procedure TFmSimpleHDRecorder.TimerTimer(Sender: TObject);
begin
 GuiLED1.Brightness_Percent := FPeaks[0] * 100;
 GuiLED2.Brightness_Percent := FPeaks[1] * 100;
 GuiLED3.Brightness_Percent := FPeaks[2] * 100;
 GuiLED4.Brightness_Percent := FPeaks[3] * 100;
end;

end.
