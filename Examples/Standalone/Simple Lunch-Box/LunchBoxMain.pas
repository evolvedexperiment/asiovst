unit LunchBoxMain;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages, XPMan,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ToolWin,
  ExtCtrls, StdCtrls, Menus, Types, Spin, DAV_Common, DAV_Complex, DAV_VSTHost,
  DAV_ASIOHost, LunchBoxEvent, LunchBoxEventList,
  LunchBoxInputFilter;

type
  TSampleRec = record
                Data       : TDAVSingleDynArray;
                SampleRate : Double;
               end;
  TFmLunchBox = class(TForm)
    ASIOHost: TASIOHost;
    Bt1: TButton;
    Bt2: TButton;
    Bt3: TButton;
    Bt4: TButton;
    Bt5: TButton;
    Bt6: TButton;
    Bt7: TButton;
    Bt8: TButton;
    Bt9: TButton;
    BtClear: TButton;
    BtFlange: TButton;
    BtRecRev: TButton;
    BtRobotize: TButton;
    CBDelay: TCheckBox;
    CBKit: TComboBox;
    CBMetronome: TCheckBox;
    CBOverdrive: TCheckBox;
    CBQuantize: TComboBox;
    CBStyle: TComboBox;
    LbBar: TLabel;
    LbBPM: TLabel;
    LbKit: TLabel;
    LbQuantize: TLabel;
    LbStyle: TLabel;
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MIExportMID: TMenuItem;
    MIExportWAV: TMenuItem;
    MIFile: TMenuItem;
    MIHelp: TMenuItem;
    MILoadBeat: TMenuItem;
    MIMoreSettings: TMenuItem;
    MINewBeat: TMenuItem;
    MIOptions: TMenuItem;
    MIQuit: TMenuItem;
    MISaveBeat: TMenuItem;
    MISaveBeatAs: TMenuItem;
    MISettings: TMenuItem;
    MIShowKeys: TMenuItem;
    MIVST: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveMIDIDialog: TSaveDialog;
    SaveWAVDialog: TSaveDialog;
    SEBar: TSpinEdit;
    SETempo: TSpinEdit;
    TBVolume: TTrackBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    VstHost: TVstHost;
    {$IFNDEF FPC}XPManifest: TXPManifest; {$ENDIF}
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtClearClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure BtFlangeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtFlangeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRecRevMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRecRevMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRobotizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRobotizeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CBDelayClick(Sender: TObject);
    procedure CBKitChange(Sender: TObject);
    procedure CBMetronomeClick(Sender: TObject);
    procedure DrumPadClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExportMIDClick(Sender: TObject);
    procedure MIExportWAVClick(Sender: TObject);
    procedure MILoadBeatClick(Sender: TObject);
    procedure MINewBeatClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure MISaveBeatAsClick(Sender: TObject);
    procedure MISaveBeatClick(Sender: TObject);
    procedure MISettingsClick(Sender: TObject);
    procedure MIShowKeysClick(Sender: TObject);
    procedure MIVSTClick(Sender: TObject);
    procedure SEBarChange(Sender: TObject);
    procedure SETempoChange(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
  private
    FMetAngle       : TComplexDouble;
    FMetPosition    : TComplexDouble;
    FFlangeAngle    : TComplexDouble;
    FFlangePosition : TComplexDouble;
    FFlange         : Boolean;
    FRobotize       : Boolean;
    FRecRev         : Boolean;
    FRealtimeVST    : Boolean;
    FBeatPos        : Integer;
    FVolume         : Single;
    FSamplesPerBeat : Single;
    FSamplesCount   : Single;
    FMetroVolume    : TDAV2SingleArray;
    FFlangeBuffer   : TDAVArrayOfSingleDynArray;
    FRobotBuffer    : TDAVArrayOfSingleDynArray;
    FRecRevBuffer   : TDAVArrayOfSingleDynArray;
    FRobotPos       : Integer;
    FDelayBuffer    : TDAVArrayOfSingleDynArray;
    FDelayPos       : array of Integer;
    FDelayLength    : array of Integer;
    FDelayVolume    : TDAV2SingleArray;
    FPatPos         : Integer;
    FMaxPatSamples  : Integer;
    FEventList      : TLunchBoxEventList;
    FInputEnvs      : TDAV2DoubleArray;
    FInputDCs       : TDAV2DoubleArray;
    FInputFilter    : Array [0..1] of TInputFilter;

    FVSTInBuffer    : TDAVArrayOfSingleDynArray;
    FVSTOutBuffer   : TDAVArrayOfSingleDynArray;
    procedure CalculateSineAngles;
    procedure CreateSample(Index: Integer; Amplitude : Double = 1);
    procedure Requantize;
    procedure AdjustDelayLength;
    procedure RenderOutput(Buffer: TDAVArrayOfSingleDynArray; BufferLength: Integer; Loop: Boolean);
  public
    property PatternPosition : Integer read FPatPos write FPatPos;
    property EventList : TLunchBoxEventList read FEventList;
  end;

var
  FmLunchBox: TFmLunchBox;
  Samples: Array [0..8] of TSampleRec;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, IniFiles, DAV_Approximations, WaveIOX, LunchBoxSetup, LunchBoxAbout,
  LunchBoxVST;

procedure TFmLunchBox.FormActivate(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TFmLunchBox.FormDeactivate(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TFmLunchBox.FormDestroy(Sender: TObject);
begin
 FEventList.Free;
end;

procedure TFmLunchBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  86 : FRobotize := True;
  66 : FRecRev := True;
  78 : FFlange := True;
  79 : CBOverdrive.Checked := not CBOverdrive.Checked;
  80 : CBDelay.Checked := not CBDelay.Checked;
  82 : FRealtimeVST := True;
 end;
end;

procedure TFmLunchBox.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  86 : FRobotize := False;
  66 : FRecRev := False;
  78 : FFlange := False;
  82 : FRealtimeVST := False;
 end;
end;

procedure TFmLunchBox.MIAboutClick(Sender: TObject);
begin
 FmAbout.Show;
end;

procedure TFmLunchBox.MIExportMIDClick(Sender: TObject);
begin
 if SaveMIDIDialog.Execute then
  begin
   ShowMessage('Feature not implemented yet');
  end;
end;

procedure TFmLunchBox.MIExportWAVClick(Sender: TObject);
var
  Buffer : TDAVArrayOfSingleDynArray;
  i      : Integer;
begin
 if SaveWAVDialog.Execute then
  begin
   ASIOHost.Active := False;
   SetLength(Buffer, 2);
   SetLength(Buffer[0], 2 * FMaxPatSamples);
   SetLength(Buffer[1], 2 * FMaxPatSamples);
   FSamplesCount := 0; FPatPos := 0;
   for i := 0 to Length(FDelayBuffer) - 1  do FillChar(FDelayBuffer[i, 0],Length(FDelayBuffer[i])*SizeOf(Single),0);
   for i := 0 to Length(FFlangeBuffer) - 1 do FillChar(FFlangeBuffer[i, 0],Length(FFlangeBuffer[i])*SizeOf(Single),0);
   for i := 0 to Length(FRobotBuffer) - 1  do FillChar(FRobotBuffer[i, 0],Length(FRobotBuffer[i])*SizeOf(Single),0);
   for i := 0 to Length(FRecRevBuffer) - 1 do FillChar(FRecRevBuffer[i, 0],Length(FRecRevBuffer[i])*SizeOf(Single),0);
   for i := 0 to FEventList.Count - 1 do FEventList.Items[i].NoteOff;
   RenderOutput(Buffer, 2 * FMaxPatSamples,false);
   SaveWAVFileSeparateStereo(SaveWAVDialog.FileName, @Buffer[0,0], @Buffer[1,0],Round(ASIOHost.SampleRate),2,16,2*FMaxPatSamples);
   FSamplesCount := 0; FPatPos := 0;
   ASIOHost.Active := True;
  end;
end;

procedure TFmLunchBox.MILoadBeatClick(Sender: TObject);
begin
 if OpenDialog.Execute then
  begin
   ShowMessage('Feature not implemented yet');
  end;
end;

procedure TFmLunchBox.MINewBeatClick(Sender: TObject);
begin
 FEventList.Clear;
 CBKit.ItemIndex := 0;
 CBKit.OnChange(Sender);
 CBStyle.ItemIndex := 0;
 SEBar.Value := 1;
 SETempo.Value := 120;
end;

procedure TFmLunchBox.MIQuitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmLunchBox.MISaveBeatAsClick(Sender: TObject);
begin
 ShowMessage('Feature not implemented yet');
end;

procedure TFmLunchBox.MISaveBeatClick(Sender: TObject);
begin
 ShowMessage('Feature not implemented yet');
end;

procedure TFmLunchBox.MISettingsClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmLunchBox.MIShowKeysClick(Sender: TObject);
begin
 ShowMessage('Feature not implemented yet');
end;

procedure TFmLunchBox.MIVSTClick(Sender: TObject);
begin
 FmVST.Show;
end;

procedure TFmLunchBox.SEBarChange(Sender: TObject);
begin
 FMaxPatSamples := Round(FSamplesPerBeat*4*SEBar.Value);
end;

procedure TFmLunchBox.SETempoChange(Sender: TObject);
var
  i : Integer;
  R : Double;
begin
 R := FSamplesPerBeat;
 FSamplesPerBeat := 60 / SETempo.Value*ASIOHost.SampleRate;
 R := FSamplesPerBeat / R;
 for i := 0 to FEventList.Count - 1
  do FEventList[i].PatternPosition := round(FEventList[i].PatternPosition*r);
 FMaxPatSamples := Round(FSamplesPerBeat * 4 * SEBar.Value);
 AdjustDelayLength;
end;

procedure TFmLunchBox.TBVolumeChange(Sender: TObject);
begin
 FMetroVolume[1] := TBVolume.Position * 0.01;
end;

procedure TFmLunchBox.CalculateSineAngles;
begin
 GetSinCos(2 * Pi * 1000 / ASIOHost.SampleRate, FMetAngle.Im, FMetAngle.Re);
 FMetPosition.Re := 1;
 FMetPosition.Im := 0;

 GetSinCos(Pi / ASIOHost.SampleRate, FFlangeAngle.Im, FFlangeAngle.Re);
 FFlangePosition.Re := 1;
 FFlangePosition.Im := 0;
end;

procedure TFmLunchBox.CBDelayClick(Sender: TObject);
begin
 if CBDelay.Checked
  then FDelayVolume[0] := 0.3
  else FDelayVolume[0] := 0;
end;

procedure TFmLunchBox.CBKitChange(Sender: TObject);
var
  sr,c,sz,i : Integer;
  pt        : PSingle;
  str       : string;
  Fl        : TSearchRec;
  done      : Boolean;
begin
 with TStringList.Create do
  try
   str := ExtractFilePath(Application.ExeName) + '.\sounds\' + CBKit.Text + '.kit';
   if not fileexists(str) then
    begin
     done := FindFirst(ExtractFilePath(Application.ExeName) + '.\sounds\*.kit', faAnyFile, Fl) <> 0;
     while not done do
      begin
       with TStringList.Create do
        try
         LoadFromFile(ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name);
         if CBKit.Text=Strings[0] then
          begin
           str := ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name;
           Break;
          end;
        finally
         Free;
         done := FindNext(Fl)<>0;
        end;
      end;
     FindClose(Fl);
     if not fileexists(str) then exit;
    end;
   LoadFromFile(str);
   for i := 0 to 8 do
    begin
     str := ExtractFilePath(Application.ExeName)+'.\sounds\'+Strings[i+1];
     if FileExists(str) then
      begin
       pt := LoadWAVFileMono(str,sr, c, sz);
       SetLength(Samples[i].Data,sz);
       Samples[i].SampleRate := sr;
       for c := 0 to sz - 1 do
        begin
         Samples[i].Data[c] := (pt)^;
         Inc(pt);
        end;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmLunchBox.CBMetronomeClick(Sender: TObject);
begin
 FMetroVolume[1] := Integer(CBMetronome.Checked) * TBVolume.Position * 0.01;
 TBVolume.Visible := CBMetronome.Checked;
end;

procedure TFmLunchBox.FormCreate(Sender: TObject);
var
  Fl       : TSearchRec;
  done     : Boolean;
begin
 FEventList := TLunchBoxEventList.Create;
 FSamplesPerBeat := 60 / SETempo.Value * ASIOHost.SampleRate;
 FMaxPatSamples := Round(FSamplesPerBeat * 4 * SEBar.Value);
 FSamplesCount := 0;
 FMetroVolume[0] := 1;
 FMetroVolume[1] := Integer(CBMetronome.Checked) * TBVolume.Position * 0.01;
 FMetPosition.Re := 1;
 FMetPosition.Im := 0;
 FVolume := 1;
 FRobotPos := 0;
 FDelayVolume[1] := 0;
 FInputDCs[0] := -1E-3;
 FInputDCs[1] := 0.1;
 FInputEnvs[0] := 0.1;
 FInputEnvs[1] := 0.1;
 FInputFilter[0] := TInputFilterLP.Create;
 with FInputFilter[0] do
  begin
   SampleRate := ASIOHost.SampleRate;
   SetFilterValues(150,-1,0.5);
   Order := 14;
  end;

 CBKit.Items.Clear;
 done := FindFirst(ExtractFilePath(Application.ExeName)+'.\sounds\*.kit',faAnyFile,Fl)<>0;
 while not done do
  begin
   with TStringList.Create do
    try
     LoadFromFile(ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name);
     CBKit.Items.Add(Strings[0]);
//     CBKit.Items.Add(Copy(Fl.Name,1,Pos('.kit',Fl.Name)-1));
    finally
     Free;
     done := FindNext(Fl)<>0;
    end;
  end;
 FindClose(Fl);
 CBKit.ItemIndex := 0;
 CBKit.OnChange(Sender);

 CalculateSineAngles;

 SetLength(FVSTInBuffer,2);
 SetLength(FVSTOutBuffer,2);

 with TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI') do
  try
   Top := ReadInteger('Layout','Main Top',Top);
   Left := ReadInteger('Layout','Main Left',Left);
  finally
   Free;
  end;
end;

procedure TFmLunchBox.CreateSample(Index : Integer; Amplitude : Double = 1);
var
  nn : TLunchBoxSample;
begin
 nn := TLunchBoxSample.Create(Index);
 with nn do
  begin
   PatternPosition := FPatPos;
   SampleRate := sqr(ASIOHost.SampleRate) / FmSetup.SESampleRate.Value;
   Frequency := Samples[Index].SampleRate / SampleRate;
   NoteOn(Amplitude);
  end;
 FEventList.Add(nn)
end;

procedure TFmLunchBox.DrumPadClick(Sender: TObject);
begin
 with Sender as TButton
  do CreateSample(Tag);
end;

procedure TFmLunchBox.BtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i : Integer;
begin
 i := 0;
 if Button = mbRight then
  while i < FEventList.Count do
   begin
    if FEventList[i].SampleIndex = TButton(Sender).Tag
     then FEventList.Delete(i)
     else inc(i);
   end;
end;

procedure TFmLunchBox.BtClearClick(Sender: TObject);
begin
 FEventList.Clear;
end;

procedure TFmLunchBox.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmLunchBox.BtFlangeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FFlange := True;
end;

procedure TFmLunchBox.BtFlangeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FFlange := False;
end;

procedure TFmLunchBox.BtRecRevMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FRecRev := True;
end;

procedure TFmLunchBox.BtRecRevMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FRecRev := False;
end;

procedure TFmLunchBox.BtRobotizeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FRobotize := True;
end;

procedure TFmLunchBox.BtRobotizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FRobotize := False;
end;

procedure TFmLunchBox.Requantize;
var i : Integer;
    q : Double;
begin
 case CBQuantize.ItemIndex of
  0 : q := 1;
  1 : q := 1 / FSamplesPerBeat;
  2 : q := 2 / FSamplesPerBeat;
  3 : q := 4 / FSamplesPerBeat;
  else exit;
 end;
 for i := 0 to FEventList.Count - 1 do
  with FEventList.Items[i] do
   begin
    PatternPosition := round(round(PatternPosition * q) / q) mod FMaxPatSamples;
   end;
end;

procedure TFmLunchBox.RenderOutput(Buffer: TDAVArrayOfSingleDynArray; BufferLength : Integer; Loop: Boolean);
var
  i, j : Integer;
  tmp  : Single;
begin
 for i := 0 to BufferLength - 1 do
  begin
   for j := 0 to FEventList.Count - 1 do
    begin
     if FPatPos = FEventList.Items[j].PatternPosition
      then FEventList.Items[j].NoteOn(1);
     if FEventList.Items[j].IsPlaying
      then Buffer[0, i] := Buffer[0, i] + FEventList.Items[j].Process;
    end;
   inc(FPatPos);
   if (FPatPos >= FMaxPatSamples) and Loop then
    begin
     FPatPos := 0;
//     FSamplesCount := FSamplesPerBeat; FBeatPos := 4;
     Requantize;
    end;
  end;

 move(Buffer[0, 0], Buffer[1, 0], BufferLength * SizeOf(Single));

 // Apply Overdrive
 if CBOverdrive.Checked then
  for j := 0 to Length(Buffer) - 1 do
   for i := 0 to BufferLength - 1
    do Buffer[j,i] := 0.3 * FastTanhOpt4TermFPU(12 * Buffer[j, i]);

 // Apply Flange
 if FFlange then
  for i := 0 to BufferLength - 1 do
   begin
    for j := 0 to Length(Buffer) - 1 do
     begin
      tmp := Buffer[j, i];
      if (i mod 2) = 0
       then Buffer[j, i] := Buffer[j, i] - FFlangePosition.Re * FFlangeBuffer[j, 1]
       else Buffer[j, i] := Buffer[j, i] - FFlangePosition.Im * FFlangeBuffer[j, 2];
      FFlangeBuffer[j, 2] := FFlangeBuffer[j, 1];
      FFlangeBuffer[j, 1] := FFlangeBuffer[j, 0];
      FFlangeBuffer[j, 0] := tmp;
     end;
    tmp := FFlangePosition.Re * FFlangeAngle.Re - FFlangePosition.Im * FFlangeAngle.Im;
    FFlangePosition.Im := FFlangePosition.Im * FFlangeAngle.Re + FFlangePosition.Re * FFlangeAngle.Im;
    FFlangePosition.Re := tmp;
   end;

 // Apply Robotize
 if FRobotize then
  for i := 0 to BufferLength - 1 do
   begin
    for j := 0 to Length(Buffer)-1 do
     begin
      FRobotBuffer[j,FRobotPos] := 0.7 * FRobotBuffer[j,FRobotPos] + 0.6 * Buffer[j,i];
      Buffer[j,i] := FRobotBuffer[j,FRobotPos];
     end;
    if FRobotPos<Length(FRobotBuffer[0])
     then inc(FRobotPos)
     else FRobotPos := 0;
    end;

 if FRecRev then
  for j := 0 to Length(Buffer) - 1 do
   begin
    SetLength(FRecRevBuffer[j], Length(FRecRevBuffer[j]) + BufferLength);
    Move(Buffer[j, 0], FRecRevBuffer[j, Length(FRecRevBuffer[j]) - BufferLength], BufferLength * SizeOf(Single));
   end else
 if Length(FRecRevBuffer[0]) > 0 then
  for j := 0 to Length(Buffer) - 1 do
   begin
    for i := 0 to BufferLength - 1
     do Buffer[j, i] := Buffer[j, i] + FRecRevBuffer[j, Length(FRecRevBuffer[j]) - i - 1];
    SetLength(FRecRevBuffer[j], Length(FRecRevBuffer[j]) - BufferLength);
   end;

 for i := 0 to BufferLength - 1 do
  begin
   for j := 0 to Length(Buffer) - 1 do
    begin
     Buffer[j, i] := Buffer[j, i] + FDelayVolume[1] * FDelayBuffer[j, FDelayPos[j]];
     FDelayBuffer[j, FDelayPos[j]] := Buffer[j, i];
     inc(FDelayPos[j]);
     if FDelayPos[j] >= FDelayLength[j]
      then FDelayPos[j] := 0;
    end;
   FDelayVolume[1] := 0.9999 * FDelayVolume[1] + 0.0001 * FDelayVolume[0];
  end;

 if VSTHost[0].Active and FRealtimeVST
  then VSTHost[0].ProcessReplacing(@Buffer[0],@Buffer[0],BufferLength);
 if VSTHost[1].Active
  then VSTHost[1].ProcessReplacing(@Buffer[0],@Buffer[0],BufferLength);

 // Apply Metronome
 if Loop then
  for i := 0 to BufferLength - 1 do
   begin
    tmp := FMetPosition.Re * FMetAngle.Re - FMetPosition.Im * FMetAngle.Im;
    FMetPosition.Im := FMetPosition.Im * FMetAngle.Re + FMetPosition.Re * FMetAngle.Im;
    FMetPosition.Re := tmp;

    if FBeatPos = 0 then tmp := 2 * sqr(tmp) - 1;
    tmp := FVolume * tmp * FMetroVolume[0];
    FMetroVolume[0] := 0.995 * FMetroVolume[0];
    FSamplesCount   := FSamplesCount + 1;
    if FSamplesCount > FSamplesPerBeat then
     begin
      FMetroVolume[0] := 1;
      FSamplesCount   := FSamplesCount - FSamplesPerBeat;
      FMetPosition.Re := 1;
      FMetPosition.Im := 0;
      if FBeatPos < 3
       then inc(FBeatPos)
       else begin FBeatPos := 0; FRecRev := False; end;
     end;
    for j := 0 to Length(Buffer) - 1
     do Buffer[j, i] := Buffer[j, i] + tmp * FMetroVolume[1];
   end;
end;

procedure TFmLunchBox.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleDynArray);
var
  i    : Integer;
  d, t : Double;
begin
 RenderOutput(OutBuffer, ASIOHost.BufferSize, True);
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   t := 5E-3 + InBuffer[0, i];
   d := 0.5 * (t + FInputDCs[0]);
   FInputDCs[0] := t;

   d := abs(d);
   FInputEnvs[0] := 0.99995 * FInputEnvs[0];
   if d > FInputEnvs[0] then FInputEnvs[0] := d;

   FInputEnvs[1] := 0.99995 * FInputEnvs[1];
   if d > FInputEnvs[1]
    then FInputEnvs[1] := FInputEnvs[1] + 0.5*(d - FInputEnvs[1]);

   t := FInputEnvs[0] / FInputEnvs[1] - 1;
   d := abs(t - FInputDCs[1]);
   FInputDCs[1] := t;

   if d > 0.15 then
    begin
     CreateSample(Random(9), min(d - 0.15, 1));
     FInputEnvs[1] := FInputEnvs[0];
     FInputDCs[1] := 0;
    end;
  end;

(*
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[0,i] := FInputFilter[0].ProcessSample(OutBuffer[0,i]+1E-32);
   OutBuffer[1,i] := OutBuffer[0,i];
//   z := fHPFilterArray[j].ProcessSample(d+1E-32);
  end;
*)
end;

procedure TFmLunchBox.ASIOHostReset(Sender: TObject);
var
  i : Integer;
begin
 VSTHost.BlockSize := ASIOHost.BufferSize;
 SetLength(FVSTInBuffer[0],  VSTHost.BlockSize);
 SetLength(FVSTInBuffer[1],  VSTHost.BlockSize);
 SetLength(FVSTOutBuffer[0], VSTHost.BlockSize);
 SetLength(FVSTOutBuffer[1], VSTHost.BlockSize);
 SetLength(FRecRevBuffer, ASIOHost.OutputChannelCount);
 SetLength(FFlangeBuffer, ASIOHost.OutputChannelCount);
 for i := 0 to Length(FFlangeBuffer) - 1
  do SetLength(FFlangeBuffer[i],4);
 SetLength(FRobotBuffer,ASIOHost.OutputChannelCount);
 for i := 0 to Length(FRobotBuffer) - 1
  do SetLength(FRobotBuffer[i],512);
 FRobotPos := 0;

 SetLength(FDelayBuffer, ASIOHost.OutputChannelCount);
 SetLength(FDelayLength, ASIOHost.OutputChannelCount);
 SetLength(FDelayPos, ASIOHost.OutputChannelCount);
 AdjustDelayLength;
end;

procedure TFmLunchBox.AdjustDelayLength;
var
  i : Integer;
begin
 for i := 0 to Length(FDelayBuffer) - 1 do
  begin
   if i mod 2 = 0
    then FDelayLength[i] := Round(1.5 * FSamplesPerBeat)
    else FDelayLength[i] := Round(FSamplesPerBeat);
   SetLength(FDelayBuffer[i], FDelayLength[i]);
   FDelayPos[i] := 0;
  end;
end;

procedure TFmLunchBox.ASIOHostSampleRateChanged(Sender: TObject);
begin
 FSamplesPerBeat := 60 / SETempo.Value * ASIOHost.SampleRate;
 FMaxPatSamples := Round(FSamplesPerBeat * 4 * SEBar.Value);
 FInputFilter[0].SampleRate := ASIOHost.SampleRate;
 CalculateSineAngles;
end;

procedure TFmLunchBox.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ASIOHOST.Active := False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI') do
  try
   WriteInteger('Layout', 'Main Top', Top);
   WriteInteger('Layout', 'Main Left', Left);
  finally
   Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$i LunchBoxMain.lrs}
{$ENDIF}

end.
