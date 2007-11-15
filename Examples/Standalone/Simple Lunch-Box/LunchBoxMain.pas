unit LunchBoxMain;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages, XPMan,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ToolWin,
  ExtCtrls, StdCtrls, DAVDCommon, DAVDComplex, Menus, Types, Spin, LunchBoxEvent,
  LunchBoxEventList, LunchBoxInputFilter, DVSTHost, DASIOHost;

type
  TSampleRec = record
                Data       : TAVDSingleDynArray;
                SampleRate : Double;
               end;
  TFmLunchBox = class(TForm)
    VstHost: TVstHost;
    ASIOHost: TASIOHost;
    {$IFNDEF FPC}XPManifest: TXPManifest; {$ENDIF}
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
    procedure BtMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fMetAngle       : TComplexDouble;
    fMetPosition    : TComplexDouble;
    fFlangeAngle    : TComplexDouble;
    fFlangePosition : TComplexDouble;
    fFlange         : Boolean;
    fRobotize       : Boolean;
    fRecRev         : Boolean;
    fRealtimeVST    : Boolean;
    fBeatPos        : Integer;
    fVolume         : Single;
    fSamplesPerBeat : Single;
    fSamplesCount   : Single;
    fMetroVolume    : T2SingleArray;
    fFlangeBuffer   : TAVDArrayOfSingleDynArray;
    fRobotBuffer    : TAVDArrayOfSingleDynArray;
    fRecRevBuffer   : TAVDArrayOfSingleDynArray;
    fRobotPos       : Integer;
    fDelayBuffer    : TAVDArrayOfSingleDynArray;
    fDelayPos       : array of Integer;
    fDelayLength    : array of Integer;
    fDelayVolume    : T2SingleArray;
    fPatPos         : Integer;
    fMaxPatSamples  : Integer;
    fEventList      : TLunchBoxEventList;
    fInputEnvs      : T2DoubleArray;
    fInputDCs       : T2DoubleArray;
    fInputFilter    : Array [0..1] of TInputFilter;

    VSTInBuffer     : TAVDArrayOfSingleDynArray;
    VSTOutBuffer    : TAVDArrayOfSingleDynArray;
    procedure CalculateSineAngles;
    procedure CreateSample(Index: Integer; Amplitude : Double = 1);
    procedure Requantize;
    procedure AdjustDelayLength;
    procedure RenderOutput(Buffer: TAVDArrayOfSingleDynArray; BufferLength: Integer; Loop: Boolean);
  public
    property PatternPosition : Integer read fPatPos write fPatPos;
    property EventList : TLunchBoxEventList read fEventList;
  end;

var FmLunchBox: TFmLunchBox;
    Samples: Array [0..8] of TSampleRec;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses Math, inifiles, WaveIOX, LunchBoxSetup, LunchBoxAbout, LunchBoxVST;

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

procedure TFmLunchBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  86 : fRobotize:=True;
  66 : fRecRev:=True;
  78 : fFlange:=True;
  79 : CBOverdrive.Checked:=not CBOverdrive.Checked;
  80 : CBDelay.Checked:=not CBDelay.Checked;
  82 : fRealtimeVST:=True;
 end;
end;

procedure TFmLunchBox.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  86 : fRobotize:=False;
  66 : fRecRev:=False;
  78 : fFlange:=False;
  82 : fRealtimeVST:=False;
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
var Buffer : TAVDArrayOfSingleDynArray;
    i      : Integer;
begin
 if SaveWAVDialog.Execute then
  begin
   ASIOHost.Active:=False;
   SetLength(Buffer,2);
   SetLength(Buffer[0],2*fMaxPatSamples);
   SetLength(Buffer[1],2*fMaxPatSamples);
   fSamplesCount:=0; fPatPos:=0;
   for i:=0 to Length(fDelayBuffer)-1 do FillChar(fDelayBuffer[i,0],Length(fDelayBuffer[i])*SizeOf(Single),0);
   for i:=0 to Length(fFlangeBuffer)-1 do FillChar(fFlangeBuffer[i,0],Length(fFlangeBuffer[i])*SizeOf(Single),0);
   for i:=0 to Length(fRobotBuffer)-1 do FillChar(fRobotBuffer[i,0],Length(fRobotBuffer[i])*SizeOf(Single),0);
   for i:=0 to Length(fRecRevBuffer)-1 do FillChar(fRecRevBuffer[i,0],Length(fRecRevBuffer[i])*SizeOf(Single),0);
   for i:=0 to fEventList.Count - 1 do fEventList.Items[i].NoteOff;
   RenderOutput(Buffer,2*fMaxPatSamples,false);
   SaveWAVFileSeparateStereo(SaveWAVDialog.FileName,@Buffer[0,0],@Buffer[1,0],Round(ASIOHost.SampleRate),2,16,2*fMaxPatSamples);
   fSamplesCount:=0; fPatPos:=0;
   ASIOHost.Active:=True;
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
    Fl        : TSearchRec;
    done      : Boolean;
begin
 with TStringList.Create do
  try
   str:=ExtractFilePath(Application.ExeName)+'.\sounds\'+CBKit.Text+'.kit';
   if not fileexists(str) then
    begin
     done:=FindFirst(ExtractFilePath(Application.ExeName)+'.\sounds\*.kit',faAnyFile,Fl)<>0;
     while not done do
      begin
       with TStringList.Create do
        try
         LoadFromFile(ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name);
         if CBKit.Text=Strings[0] then
          begin
           str:=ExtractFilePath(Application.ExeName)+'.\sounds\'+Fl.Name;
           Break;
          end;
        finally
         Free;
         done:=FindNext(Fl)<>0;
        end;
      end;
     FindClose(Fl);
     if not fileexists(str) then exit;
    end;
   LoadFromFile(str);
   for i := 0 to 8 do
    begin
     str:=ExtractFilePath(Application.ExeName)+'.\sounds\'+Strings[i+1];
     if FileExists(str) then
      begin
       pt:=LoadWAVFileMono(str,sr, c, sz);
       SetLength(Samples[i].Data,sz);
       Samples[i].SampleRate:=sr;
       for c := 0 to sz - 1 do
        begin
         Samples[i].Data[c]:=(pt)^;
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
 fInputDCs[0]:=-1E-3;
 fInputDCs[1]:=0.1;
 fInputEnvs[0]:=0.1;
 fInputEnvs[1]:=0.1;
 fInputFilter[0]:=TInputFilterLP.Create;
 with fInputFilter[0] do
  begin
   SampleRate:=ASIOHost.SampleRate;
   SetFilterValues(150,-1,0.5);
   Order:=14;
  end;

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

procedure TFmLunchBox.CreateSample(Index : Integer; Amplitude : Double = 1);
var nn : TLunchBoxSample;
begin
 nn:=TLunchBoxSample.Create(Index);
 with nn do
  begin
   PatternPosition:=fPatPos;
   SampleRate:=sqr(ASIOHost.SampleRate)/FmSetup.SESampleRate.Value;
   Frequency:=Samples[Index].SampleRate/SampleRate;
   NoteOn(Amplitude);
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

procedure TFmLunchBox.RenderOutput(Buffer: TAVDArrayOfSingleDynArray; BufferLength : Integer; Loop: Boolean);
var i,j : Integer;
    tmp : Single;
begin
 for i := 0 to BufferLength - 1 do
  begin
   for j := 0 to fEventList.Count - 1 do
    begin
     if fPatPos=fEventList.Items[j].PatternPosition
      then fEventList.Items[j].NoteOn(1);
     if fEventList.Items[j].IsPlaying
      then Buffer[0,i] := Buffer[0,i] + fEventList.Items[j].Process;
    end;
   inc(fPatPos);
   if (fPatPos>=fMaxPatSamples) and Loop then
    begin
     fPatPos:=0;
//     fSamplesCount:=fSamplesPerBeat; fBeatPos:=4;
     Requantize;
    end;
  end;

 move(Buffer[0,0], Buffer[1,0], BufferLength * SizeOf(Single));

 // Apply Overdrive
 if CBOverdrive.Checked then
  for j := 0 to Length(Buffer) - 1 do
   for i := 0 to BufferLength - 1
    do Buffer[j,i] := 0.3*Tanh2c(12*Buffer[j,i]);

 // Apply Flange
 if fFlange then
  for i := 0 to BufferLength - 1 do
   begin
    for j := 0 to Length(Buffer) - 1 do
     begin
      tmp:=Buffer[j,i];
      if (i mod 2)=0
       then Buffer[j,i] := Buffer[j,i] - fFlangePosition.Re*fFlangeBuffer[j,1]
       else Buffer[j,i] := Buffer[j,i] - fFlangePosition.Im*fFlangeBuffer[j,2];
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
  for i := 0 to BufferLength - 1 do
   begin
    for j := 0 to Length(Buffer)-1 do
     begin
      fRobotBuffer[j,fRobotPos] := 0.7 * fRobotBuffer[j,fRobotPos] + 0.6 * Buffer[j,i];
      Buffer[j,i] := fRobotBuffer[j,fRobotPos];
     end;
    if fRobotPos<Length(fRobotBuffer[0])
     then inc(fRobotPos)
     else fRobotPos:=0;
    end;

 if fRecRev then
  for j := 0 to Length(Buffer)-1 do
   begin
    SetLength(fRecRevBuffer[j],Length(fRecRevBuffer[j])+BufferLength);
    Move(Buffer[j,0],fRecRevBuffer[j,Length(fRecRevBuffer[j])-BufferLength],BufferLength*SizeOf(Single));
   end else
 if Length(fRecRevBuffer[0])>0 then
  for j := 0 to Length(Buffer)-1 do
   begin
    for i:=0 to BufferLength-1
     do Buffer[j,i]:=Buffer[j,i]+fRecRevBuffer[j,Length(fRecRevBuffer[j])-i-1];
    SetLength(fRecRevBuffer[j],Length(fRecRevBuffer[j])-BufferLength);
   end;

 for i := 0 to BufferLength - 1 do
  begin
   for j := 0 to Length(Buffer) - 1 do
    begin
     Buffer[j,i] := Buffer[j,i] + fDelayVolume[1]*fDelayBuffer[j,fDelayPos[j]];
     fDelayBuffer[j,fDelayPos[j]]:=Buffer[j,i];
     inc(fDelayPos[j]);
     if fDelayPos[j]>=fDelayLength[j]
      then fDelayPos[j]:=0;
    end;
   fDelayVolume[1]:=0.9999*fDelayVolume[1]+0.0001*fDelayVolume[0];
  end;

 if VSTHost[0].Active and fRealtimeVST
  then VSTHost[0].ProcessReplacing(@Buffer[0],@Buffer[0],BufferLength);
 if VSTHost[1].Active
  then VSTHost[1].ProcessReplacing(@Buffer[0],@Buffer[0],BufferLength);

 // Apply Metronome
 if Loop then
  for i := 0 to BufferLength - 1 do
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
    for j := 0 to Length(Buffer) - 1
     do Buffer[j,i] := Buffer[j,i] + tmp * fMetroVolume[1];
   end;
end;

procedure TFmLunchBox.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TAVDArrayOfSingleDynArray);
var i   : Integer;
    d,t : Double;
begin
 RenderOutput(OutBuffer, ASIOHost.BufferSize, True);
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   t:=5E-3+InBuffer[0,i];
   d:=0.5*(t+fInputDCs[0]);
   fInputDCs[0]:=t;

   d:=abs(d);
   fInputEnvs[0]:=0.99995*fInputEnvs[0];
   if d>fInputEnvs[0] then fInputEnvs[0]:=d;

   fInputEnvs[1]:=0.99995*fInputEnvs[1];
   if d>fInputEnvs[1]
    then fInputEnvs[1]:=fInputEnvs[1]+0.5*(d-fInputEnvs[1]);

   t:=fInputEnvs[0]/fInputEnvs[1]-1;
   d:=abs(t-fInputDCs[1]);
   fInputDCs[1]:=t;

   if d>0.15 then
    begin
     CreateSample(Random(9),min(d-0.15,1));
     fInputEnvs[1]:=fInputEnvs[0];
     fInputDCs[1]:=0;
    end;
  end;

(*
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[0,i]:=fInputFilter[0].ProcessSample(OutBuffer[0,i]+1E-32);
   OutBuffer[1,i]:=OutBuffer[0,i];
//   z:=fHPFilterArray[j].ProcessSample(d+1E-32);
  end;
*)
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
 fInputFilter[0].SampleRate:=ASIOHost.SampleRate;
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

{$IFDEF FPC}
initialization
  {$i LunchBoxMain.lrs}
{$ENDIF}

end.
