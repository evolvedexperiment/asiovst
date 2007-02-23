unit LunchBoxMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  XPMan, ComCtrls, ToolWin, ExtCtrls, StdCtrls, DVstHost, DASIOHost, DDSPBase,
  Menus, Types, Spin, LunchBoxEvent, LunchBoxEventList;

type
  TComplexDouble = record Re, Im : Double; end;
  TFmLunchBox = class(TForm)
    VstHost: TVstHost;
    ASIOHost: TASIOHost;
    XPManifest: TXPManifest;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOptions: TMenuItem;
    MIHelp: TMenuItem;
    MISettings: TMenuItem;
    MIAbout: TMenuItem;
    MIShowKeys: TMenuItem;
    MINewBeat: TMenuItem;
    MILoadBeat: TMenuItem;
    MISaveBeat: TMenuItem;
    MISaveBeatAs: TMenuItem;
    MIExportWAV: TMenuItem;
    MIExportMID: TMenuItem;
    MIQuit: TMenuItem;
    MIVST: TMenuItem;
    MIMoreSettings: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveWAVDialog: TSaveDialog;
    SaveMIDIDialog: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    SETempo: TSpinEdit;
    SEBar: TSpinEdit;
    LbBar: TLabel;
    LbBPM: TLabel;
    LbKit: TLabel;
    LbStyle: TLabel;
    LbQuantize: TLabel;
    Bt1: TButton;
    Bt2: TButton;
    Bt3: TButton;
    Bt4: TButton;
    Bt5: TButton;
    Bt6: TButton;
    Bt7: TButton;
    Bt8: TButton;
    Bt9: TButton;
    BtRobotize: TButton;
    BtRecRev: TButton;
    BtFlange: TButton;
    BtClear: TButton;
    CBMetronome: TCheckBox;
    CBOverdrive: TCheckBox;
    CBDelay: TCheckBox;
    CBKit: TComboBox;
    CBStyle: TComboBox;
    CBQuantize: TComboBox;
    TBVolume: TTrackBar;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleDynArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
    procedure BtFlangeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtFlangeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRecRevMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRecRevMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRobotizeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtRobotizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtClearClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure CBKitChange(Sender: TObject);
    procedure CBMetronomeClick(Sender: TObject);
    procedure SEBarChange(Sender: TObject);
    procedure SETempoChange(Sender: TObject);
    procedure DrumPadClick(Sender: TObject);
    procedure MIVSTClick(Sender: TObject);
    procedure MINewBeatClick(Sender: TObject);
    procedure MISaveBeatClick(Sender: TObject);
    procedure MISaveBeatAsClick(Sender: TObject);
    procedure MIExportWAVClick(Sender: TObject);
    procedure MIExportMIDClick(Sender: TObject);
    procedure MIShowKeysClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure MISettingsClick(Sender: TObject);
    procedure MILoadBeatClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure CBDelayClick(Sender: TObject);
    procedure BtMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fMetAngle       : TComplexDouble;
    fMetPosition    : TComplexDouble;
    fFlangeAngle    : TComplexDouble;
    fFlangePosition : TComplexDouble;
    fFlange         : Boolean;
    fRobotize       : Boolean;
    fRecRev         : Boolean;
    fBeatPos        : Integer;
    fVolume         : Single;
    fSamplesPerBeat : Single;
    fSamplesCount   : Single;
    fMetroVolume    : array [0..1] of Single;
    fFlangeBuffer   : TArrayOfSingleDynArray;
    fRobotBuffer    : TArrayOfSingleDynArray;
    fRecRevBuffer   : TArrayOfSingleDynArray;
    fRobotPos       : Integer;
    fDelayBuffer    : TArrayOfSingleDynArray;
    fDelayPos       : array of Integer;
    fDelayLength    : array of Integer;
    fDelayVolume    : array [0..1] of Single;
    fPatPos         : Integer;
    fMaxPatSamples  : Integer;
    fEventList      : TLunchBoxEventList;

    VSTInBuffer     : TArrayOfSingleDynArray;
    VSTOutBuffer    : TArrayOfSingleDynArray;
    procedure CalculateSineAngles;
    procedure CreateSample(Index: Integer);
    procedure Requantize;
    procedure AdjustDelayLength;
  public
    property PatternPosition : Integer read fPatPos write fPatPos;
  end;

var FmLunchBox: TFmLunchBox;
    Samples: Array [0..8] of TSingleDynArray;

implementation

{$R *.DFM}

uses inifiles, WaveIOX, LunchBoxSetup, LunchBoxAbout, LunchBoxVST;

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
 fEventList.Free;
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
begin
 if SaveWAVDialog.Execute then
  begin
   ShowMessage('Feature not implemented yet');
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
 fEventList.Clear;
 CBKit.ItemIndex:=0;
 CBKit.OnChange(Sender);
 CBStyle.ItemIndex:=0;
 SEBar.Value:=1;
 SETempo.Value:=120;
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
 fMaxPatSamples:=Round(fSamplesPerBeat*4*SEBar.Value);
end;

procedure TFmLunchBox.SETempoChange(Sender: TObject);
var i : Integer;
    R : Double;
begin
 R:=fSamplesPerBeat;
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
 R:=fSamplesPerBeat/R;
 for i:=0 to fEventList.Count-1
  do fEventList[i].PatternPosition:=round(fEventList[i].PatternPosition*r);
 fMaxPatSamples:=Round(fSamplesPerBeat*4*SEBar.Value);
 AdjustDelayLength;
end;

procedure TFmLunchBox.TBVolumeChange(Sender: TObject);
begin
 fMetroVolume[1]:=TBVolume.Position*0.01;
end;

procedure TFmLunchBox.CalculateSineAngles;
begin
 GetSinCos(2*Pi*1000/ASIOHost.SampleRate,fMetAngle.Im,fMetAngle.Re);
 fMetPosition.Re := 1;
 fMetPosition.Im := 0;

 GetSinCos(Pi/ASIOHost.SampleRate,fFlangeAngle.Im,fFlangeAngle.Re);
 fFlangePosition.Re := 1;
 fFlangePosition.Im := 0;
end;

procedure TFmLunchBox.CBDelayClick(Sender: TObject);
begin
 if CBDelay.Checked
  then fDelayVolume[0]:=0.3
  else fDelayVolume[0]:=0;
end;

procedure TFmLunchBox.CBKitChange(Sender: TObject);
var sr,c,sz,i : Integer;
    pt        : PSingle;
    str       : string;
begin
 with TStringList.Create do
  try
   str:=ExtractFilePath(Application.ExeName)+'.\sounds\'+CBKit.Text+'.kit';
   if not fileexists(str) then exit;
   LoadFromFile(str);
   for i := 0 to 8 do
    begin
     str:=ExtractFilePath(Application.ExeName)+'.\sounds\'+Strings[i+1];
     if FileExists(str) then
      begin
       pt:=LoadWAVFileMono(str,sr, c, sz);
       SetLength(Samples[i],sz);
       for c := 0 to sz - 1 do
        begin
         Samples[i,c]:=(pt)^;
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
 fMetroVolume[1]:=Integer(CBMetronome.Checked)*TBVolume.Position*0.01;
 TBVolume.Visible:=CBMetronome.Checked;
end;

procedure TFmLunchBox.FormCreate(Sender: TObject);
var Settings : TInifile;
    Fl       : TSearchRec;
    done     : Boolean;
begin
 fEventList:=TLunchBoxEventList.Create;
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
 fMaxPatSamples:=Round(fSamplesPerBeat*4*SEBar.Value);
 fSamplesCount := 0;
 fMetroVolume[0] := 1;
 fMetroVolume[1]:=Integer(CBMetronome.Checked)*TBVolume.Position*0.01;
 fMetPosition.Re := 1;
 fMetPosition.Im := 0;
 fVolume := 1;
 fRobotPos:=0;
 fDelayVolume[1]:=0;

 CBKit.Items.Clear;
 done:=FindFirst(ExtractFilePath(Application.ExeName)+'.\sounds\*.kit',faAnyFile,Fl)<>0;
 while not done do
  begin
   with TStringList.Create do
    try
     LoadFromFile(ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name);
     CBKit.Items.Add(Strings[0]);
//     CBKit.Items.Add(Copy(Fl.Name,1,Pos('.kit',Fl.Name)-1));
    finally
     Free;
     done:=FindNext(Fl)<>0;
    end;
  end;
 FindClose(Fl);
 CBKit.ItemIndex:=0;
 CBKit.OnChange(Sender);

 CalculateSineAngles;

 SetLength(VSTInBuffer,2);
 SetLength(VSTOutBuffer,2);

 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Top:=Settings.ReadInteger('Layout','Main Top',Top);
 Left:=Settings.ReadInteger('Layout','Main Left',Left);
 Settings.Free;
end;

procedure TFmLunchBox.CreateSample(Index : Integer);
var nn : TLunchBoxSample;
begin
 nn:=TLunchBoxSample.Create(Index);
 with nn do
  begin
   PatternPosition:=fPatPos;
   SampleRate:=ASIOHost.SampleRate;
   Frequency:=32000/SampleRate;
   NoteOn(1);
  end;
 fEventList.Add(nn)
end;

procedure TFmLunchBox.DrumPadClick(Sender: TObject);
begin
 with Sender as TButton
  do CreateSample(Tag);
end;

procedure TFmLunchBox.BtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i : Integer;
begin
 i:=0;
 if Button=mbRight then
  while i<fEventList.Count do
   begin
    if fEventList[i].SampleIndex=TButton(Sender).Tag
     then fEventList.Delete(i)
     else inc(i);
   end;
end;

procedure TFmLunchBox.BtClearClick(Sender: TObject);
begin
 fEventList.Clear;
end;

procedure TFmLunchBox.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmLunchBox.BtFlangeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fFlange:=True;
end;

procedure TFmLunchBox.BtFlangeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fFlange:=False;
end;

procedure TFmLunchBox.BtRecRevMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fRecRev:=True;
end;

procedure TFmLunchBox.BtRecRevMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fRecRev:=False;
end;

procedure TFmLunchBox.BtRobotizeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 fRobotize:=True;
end;

procedure TFmLunchBox.BtRobotizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fRobotize:=False;
end;

procedure TFmLunchBox.Requantize;
var i : Integer;
    q : Double;
begin
    case CBQuantize.ItemIndex of
     0 : q:=1;
     1 : q:=1/fSamplesPerBeat;
     2 : q:=2/fSamplesPerBeat;
     3 : q:=4/fSamplesPerBeat;
     else exit;
    end;
 for i := 0 to fEventList.Count - 1 do
  with fEventList.Items[i] do
   begin
    PatternPosition := round(round(PatternPosition*q)/q) mod fMaxPatSamples;
   end;
end;

procedure TFmLunchBox.ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleDynArray);
var i,j : Integer;
    tmp : Single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   inc(fPatPos);
   if fPatPos>=fMaxPatSamples then
    begin
     fPatPos:=0;
//     fSamplesCount:=fSamplesPerBeat; fBeatPos:=4;
     Requantize;
    end;
   for j := 0 to fEventList.Count - 1 do
    begin
     if fPatPos=fEventList.Items[j].PatternPosition
      then fEventList.Items[j].NoteOn(1);
     if fEventList.Items[j].IsPlaying
      then OutBuffer[0,i] := OutBuffer[0,i] + fEventList.Items[j].Process;
    end;
  end;

 move(OutBuffer[0,0], OutBuffer[1,0], ASIOHost.BufferSize * SizeOf(Single));

 // Apply Overdrive
 if CBOverdrive.Checked then
  for j := 0 to ASIOHost.OutputChannelCount - 1 do
   for i := 0 to ASIOHost.BufferSize - 1
    do OutBuffer[j,i] := 0.3*Tanh2c(12*OutBuffer[j,i]);

 // Apply Flange
 if fFlange then
  for i := 0 to ASIOHost.BufferSize - 1 do
   begin
    for j := 0 to ASIOHost.OutputChannelCount - 1 do
     begin
      tmp:=OutBuffer[j,i];
      if (i mod 2)=0
       then OutBuffer[j,i] := OutBuffer[j,i] - fFlangePosition.Re*fFlangeBuffer[j,1]
       else OutBuffer[j,i] := OutBuffer[j,i] - fFlangePosition.Im*fFlangeBuffer[j,2];
      fFlangeBuffer[j,2]:=fFlangeBuffer[j,1];
      fFlangeBuffer[j,1]:=fFlangeBuffer[j,0];
      fFlangeBuffer[j,0]:=tmp;
     end;
    tmp:=fFlangePosition.Re*fFlangeAngle.Re-fFlangePosition.Im*fFlangeAngle.Im;
    fFlangePosition.Im:=fFlangePosition.Im*fFlangeAngle.Re+fFlangePosition.Re*fFlangeAngle.Im;
    fFlangePosition.Re:=tmp;
   end;

 // Apply Robotize
 if fRobotize then
  for i := 0 to ASIOHost.BufferSize - 1 do
   begin
    for j := 0 to ASIOHost.OutputChannelCount-1 do
     begin
      fRobotBuffer[j,fRobotPos] := 0.7 * fRobotBuffer[j,fRobotPos] + 0.6 * OutBuffer[j,i];
      OutBuffer[j,i] := fRobotBuffer[j,fRobotPos];
     end;
    if fRobotPos<Length(fRobotBuffer[0])
     then inc(fRobotPos)
     else fRobotPos:=0;
    end;

 if fRecRev then
  for j := 0 to ASIOHost.OutputChannelCount-1 do
   begin
    SetLength(fRecRevBuffer[j],Length(fRecRevBuffer[j])+ASIOHost.BufferSize);
    Move(OutBuffer[j,0],fRecRevBuffer[j,Length(fRecRevBuffer[j])-ASIOHost.BufferSize],ASIOHost.BufferSize*SizeOf(Single));
   end else
 if Length(fRecRevBuffer[0])>0 then
  for j := 0 to ASIOHost.OutputChannelCount-1 do
   begin
    for i:=0 to ASIOHost.BufferSize-1
     do OutBuffer[j,i]:=OutBuffer[j,i]+fRecRevBuffer[j,Length(fRecRevBuffer[j])-i-1];
    SetLength(fRecRevBuffer[j],Length(fRecRevBuffer[j])-ASIOHost.BufferSize);
   end;

 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   for j := 0 to ASIOHost.OutputChannelCount - 1 do
    begin
     OutBuffer[j,i] := OutBuffer[j,i] + fDelayVolume[1]*fDelayBuffer[j,fDelayPos[j]];
     fDelayBuffer[j,fDelayPos[j]]:=OutBuffer[j,i];
     inc(fDelayPos[j]);
     if fDelayPos[j]>=fDelayLength[j]
      then fDelayPos[j]:=0;
    end;
   fDelayVolume[1]:=0.9999*fDelayVolume[1]+0.0001*fDelayVolume[0];
  end;

//   VSTHost[0].ProcessReplacing(@OutBuffer[ASIOHost.OuputChannelOffset],@OutBuffer[ASIOHost.OutputChannelOffset],ASIOHost.BufferSize);
//   VSTHost[1].ProcessReplacing(@OutBuffer[ASIOHost.OuputChannelOffset],@OutBuffer[ASIOHost.OutputChannelOffset],ASIOHost.BufferSize);

 // Apply Metronome
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   tmp:=fMetPosition.Re*fMetAngle.Re-fMetPosition.Im*fMetAngle.Im;
   fMetPosition.Im:=fMetPosition.Im*fMetAngle.Re+fMetPosition.Re*fMetAngle.Im;
   fMetPosition.Re:=tmp;

   if fBeatPos=0 then tmp:=2*sqr(tmp)-1;
   tmp:=fVolume*tmp*fMetroVolume[0];
   fMetroVolume[0]:=0.995*fMetroVolume[0];
   fSamplesCount:=fSamplesCount+1;
   if fSamplesCount>fSamplesPerBeat then
    begin
     fMetroVolume[0]:=1;
     fSamplesCount:=fSamplesCount-fSamplesPerBeat;
     fMetPosition.Re:=1;
     fMetPosition.Im:=0;
     if fBeatPos<3
      then inc(fBeatPos)
      else begin fBeatPos:=0; fRecRev:=False; end;
    end;
   for j := 0 to ASIOHost.OutputChannelCount - 1
    do OutBuffer[j,i] := OutBuffer[j,i] + tmp * fMetroVolume[1];
  end;
end;

procedure TFmLunchBox.ASIOHostReset(Sender: TObject);
var i : Integer;
begin
 VSTHost.BlockSize:=ASIOHost.BufferSize;
 SetLength(VSTInBuffer[0],VSTHost.BlockSize);
 SetLength(VSTInBuffer[1],VSTHost.BlockSize);
 SetLength(VSTOutBuffer[0],VSTHost.BlockSize);
 SetLength(VSTOutBuffer[1],VSTHost.BlockSize);
 SetLength(fRecRevBuffer,ASIOHost.OutputChannelCount);
 SetLength(fFlangeBuffer,ASIOHost.OutputChannelCount);
 for i := 0 to Length(fFlangeBuffer) - 1
  do SetLength(fFlangeBuffer[i],4);
 SetLength(fRobotBuffer,ASIOHost.OutputChannelCount);
 for i := 0 to Length(fRobotBuffer) - 1
  do SetLength(fRobotBuffer[i],512);
 fRobotPos:=0;

 SetLength(fDelayBuffer,ASIOHost.OutputChannelCount);
 SetLength(fDelayLength,ASIOHost.OutputChannelCount);
 SetLength(fDelayPos,ASIOHost.OutputChannelCount);
 AdjustDelayLength;
end;

procedure TFmLunchBox.AdjustDelayLength;
var i : Integer;
begin
 for i := 0 to Length(fDelayBuffer) - 1 do
  begin
   if i mod 2 = 0
    then fDelayLength[i]:=Round(1.5*fSamplesPerBeat)
    else fDelayLength[i]:=Round(fSamplesPerBeat);
   SetLength(fDelayBuffer[i],fDelayLength[i]);
   fDelayPos[i]:=0;
  end;
end;

procedure TFmLunchBox.ASIOHostSampleRateChanged(Sender: TObject);
begin
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
 fMaxPatSamples:=Round(fSamplesPerBeat*4*SEBar.Value);
 CalculateSineAngles;
end;

procedure TFmLunchBox.FormClose(Sender: TObject;
  var Action: TCloseAction);
var Settings : TInifile;
begin
 ASIOHOST.Active:=False;
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Settings.WriteInteger('Layout','Main Top',Top);
 Settings.WriteInteger('Layout','Main Left',Left);
 Settings.Free;
end;

end.
