unit OversampleTemplateDM;

interface

{$I DAV_Compiler.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VSTEffect, DAV_VSTParameters, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule, DAV_DspUpDownsampling, DAV_VstHost, DAV_VstOfflineTask;

type
  TOversampleTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleEditSleep(Sender: TObject);
    procedure VSTModuleEditTop(Sender: TObject);
    procedure VSTModuleGetVU(var VU: Single);
    procedure VSTModuleOfflineNotify(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure VSTModuleProcess32OversampleSingle(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64OversampleSingle(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);

    procedure VSTModuleProcessEvents(Sender: TObject; Events: PVstEvents);
    procedure VSTModuleProcessVarIO(Sender: TObject; const varIo: TVstVariableIo);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleEditorKeyUp(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleAfterProgramChange(Sender: TObject);

    function VSTModuleInputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleVendorSpecific(Sender: TObject; const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;

    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAutomate(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
    procedure ParamOSFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOversamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamPreFilterOrderValue(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPreTransBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostFilterBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostCharChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOfflinePrepare(Sender: TObject; const OfflineTasks: array of TVstOfflineTask);
    procedure VSTModuleOfflineRun(Sender: TObject; const OfflineTasks: array of TVstOfflineTask);
  private
    FUpsampler        : array of TDAVUpsampling;
    FDownsampler      : array of TDAVDownsampling;
    FIn64, FOut64     : array of PDAVDoubleFixedArray;
    FIn32, FOut32     : array of PDAVSingleFixedArray;
    FBaseParCount     : Integer;
    FOSActive         : Boolean;
    FOSFactor         : Integer;
    FMaximumBlockSize : Integer;
    FTempBufferSize   : Integer;
    FSemaphore        : Integer;
    procedure SetOSFactor(const NewOSFactor: Integer);
    procedure SetTempBufferSize(const Value: Integer);
    procedure VSTBuffersChanged;
    procedure PluginSampleRateChanged;
    procedure CheckSampleFrames(const SampleFrames: Integer);
  public
    function HostCallIdle(Index: Integer; Value: Integer; ptr: Pointer; opt: Single): Integer; override;
    function HostCallGetTailSize(Index: Integer; Value: Integer; ptr: Pointer; opt: Single): Integer; override;
  published
    property TempBufferSize: Integer read FTempBufferSize write SetTempBufferSize default 0;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$R *.DFM}

uses
  Math, Dialogs, Controls, Types, OversampleTemplateGUI, DAV_VSTPrograms,
  DAV_VSTModuleWithDsp, DAV_DSPButterworthFilter, DAV_DSPChebyshevFilter,
  DAV_DSPBesselFilter;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

function EnumRCDATANamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  ShowMessage(lpName);
end;

function EnumTypesFunc(hModule:THandle; lpType: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  ShowMessage(IntToStr(Integer(lpType)));
//  ShowMessage(lpType);
end;

procedure TOversampleTemplateDataModule.VSTModuleCreate(Sender: TObject);
var
  RN       : TStringList;
  RS       : TResourceStream;
  PI       : TCustomVstPlugIn;
  ch, i, j : Integer;
  str      : string;
begin
 FBaseParCount            := numParams;
 FOSFactor                := 1;
 FOSActive                := False;
 FTempBufferSize          := 0;
 FSemaphore               := 0;
 FMaximumBlockSize        := VstHost.BlockSize;
 OnProcess                := VSTModuleProcess32OversampleSingle;
 OnProcessReplacing       := VSTModuleProcess32OversampleSingle;
 OnProcessDoubleReplacing := VSTModuleProcess64OversampleSingle;

 RN := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  if RN.Count > 0 then
   begin
    PI := VstHost.VstPlugIns[0];

    // load plugin from resource
    RS := TResourceStream.Create(hInstance, RN[0], 'DLL');
    try
     PI.LoadFromStream(RS);
    finally
     FreeAndNil(RS);
    end;

    // set parameter count
    for i := 0 to VstHost[0].numParams - 1 do
     with ParameterProperties.Add do
      begin
       OnParameterChange        := VSTModuleParameterChange;
       OnCustomParameterLabel   := CustomParameterLabel;
       OnCustomParameterDisplay := CustomParameterDisplay;
      end;

    // activate plugin
    PI.Active := True;

    // scan and rename parameters
    for i := 0 to VstHost[0].numParams - 1 do
     with ParameterProperties[FBaseParCount + i]
      do DisplayName := VstHost[0].GetParamName(i);

    UniqueID    := VstHost[0].UniqueID[1] +
                   VstHost[0].UniqueID[2] +
                   VstHost[0].UniqueID[3] + '²';
    EffectName  := VstHost[0].EffectName + '²';
    ProductName := VstHost[0].ProductString + '²';
    VendorName  := VstHost[0].VendorString + ' (powered by Delphi ASIO & VST Packages)';

    // program replication
    Parameter[0] := 0;
    Parameter[1] := 1;
    Parameter[2] := 4;
    Parameter[3] := 99;
    Parameter[4] := 1;
    Parameter[5] := 4;
    Parameter[6] := 99;

    for i := 0 to VstHost[0].numPrograms - 1 do
     with Programs.Add do
      begin
       PI.GetProgramNameIndexed(0, i, str);
       if PI.ProgramNr <> i
        then PI.ProgramNr := i;
       Parameter[0] := 0;
       Parameter[1] := 2;
       Parameter[2] := 4;
       Parameter[3] := 99;
       Parameter[4] := 2;
       Parameter[5] := 4;
       Parameter[6] := 99;
       DisplayName := str;
       for j := 0 to PI.numParams - 1
        do Parameter[FBaseParCount + j] := PI.Parameter[j];
      end;
    if numPrograms > 0 then
     begin
      PI.ProgramNr := 0;
      for j := 0 to PI.numParams - 1
       do Parameter[FBaseParCount + j] := PI.Parameter[j];
     end
    else
     with Programs.Add do
      begin
       DisplayName := 'Default';
       Parameter[0] := 0;
       Parameter[1] := 2;
       Parameter[2] := 4;
       Parameter[3] := 99;
       Parameter[4] := 2;
       Parameter[5] := 4;
       Parameter[6] := 99;
      end;

    // enable 64bit processing if supported by both plugins
    if (effFlagsCanDoubleReplacing in VstHost[0].EffectOptions)
     then
      begin
       Flags := Flags + [effFlagsCanDoubleReplacing];
       ProcessPrecisition := pp64;
      end
     else
      begin
       Flags := Flags - [effFlagsCanDoubleReplacing];
       ProcessPrecisition := pp32;
      end;
    numInputs  := VstHost[0].numInputs;
    numOutputs := VstHost[0].numOutputs;
    PlugCategory := VstHost[0].PlugCategory;
   end
  else
   begin
    Parameter[0] := 0;
    Parameter[1] := 1;
    Parameter[2] := 4;
    Parameter[3] := 99;
    Parameter[4] := 1;
    Parameter[5] := 4;
    Parameter[6] := 99;
   end;
 finally
  FreeAndNil(RN);
 end;

 SetLength(FUpsampler, numInputs);
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch] := TDAVUpsampling.Create(Self);
 SetLength(FDownsampler, numOutputs);
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch] := TDAVDownsampling.Create(Self);

 SetLength(FIn32, numInputs);
 SetLength(FIn64, numInputs);
 SetLength(FOut32, numOutputs);
 SetLength(FOut64, numOutputs);

 VSTBuffersChanged;
end;

procedure TOversampleTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  ch : Integer;
begin
 FSemaphore := 0;
 for ch := 0 to Length(FIn64) - 1 do Dispose(FIn64[ch]);
 for ch := 0 to Length(FOut64) - 1 do Dispose(FOut64[ch]);

 for ch := 0 to Length(FUpsampler) - 1
  do FreeAndNil(FUpsampler[ch]);
 for ch := 0 to Length(FDownsampler) - 1
  do FreeAndNil(FDownsampler[ch]);

 VSTHost.VstPlugIns.Clear;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
begin
 with VstHost[0] do if EditVisible then CloseEdit;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditIdle(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditIdle;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  Rct      : array [0..1] of TRect;
  Oversize : Integer;
begin
 GUI := TFmOversampler.Create(Self);

 // set plugin GUI size
 if assigned(VstHost[0]) and VstHost[0].Active then
  with TFmOversampler(GUI) do
   begin
    PnGui.Visible    := True;
    ShBorder.Visible := True;

    if not VstHost[0].EditVisible
     then VstHost[0].ShowEdit(TForm(PnGui));
    Rct[0] := VstHost[0].GetRect;

    Oversize := PnControl.Width - (Rct[0].Right - Rct[0].Left);
    if Oversize < 0 then
     begin
      // current editor is too small, enlarge!
      PnGui.Align := alClient;
      ClientWidth := (Rct[0].Right - Rct[0].Left);
      ClientHeight := PnControl.Height + (Rct[0].Bottom - Rct[0].Top);
      ShBorder.Visible := False;
     end
    else
     begin
      PnGui.Align  := alNone;
      PnGui.Left   := Oversize div 2;
      PnGui.Width  := (Rct[0].Right - Rct[0].Left);

      // calculate new height and y position
      PnGui.Height := (Rct[0].Bottom - Rct[0].Top);
      Oversize     := round(Oversize * (PnGui.Height) / PnGui.Width);
      PnGui.Top    := PnControl.Height + Oversize div 2;
      ClientHeight := PnControl.Height + PnGui.Height + Oversize;

      // show border
      ShBorder.Visible := True;
      ShBorder.SetBounds(PnGui.Left - ShBorder.Pen.Width,
                         PnGui.Top - ShBorder.Pen.Width,
                         PnGui.Width + 2 * ShBorder.Pen.Width,
                         PnGui.Height + 2 * ShBorder.Pen.Width);
     end;
    if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   end
 else
  with TFmOversampler(GUI) do
   begin
    PnGui.Visible    := False;
    ShBorder.Visible := False;
   end;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditorKeyDown(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 with VstHost[0] do if EditVisible
  then EditKeyDown(Char(keyCode.Character), keyCode.Virt, TVstModifierKeys(keyCode.Modifier)); 
end;

procedure TOversampleTemplateDataModule.VSTModuleEditorKeyUp(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 with VstHost[0] do if EditVisible
  then EditKeyUp(Char(keyCode.Character), keyCode.Virt, TVstModifierKeys(keyCode.Modifier))
end;

procedure TOversampleTemplateDataModule.VSTModuleEditSleep(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditDeactivate;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditTop(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditActivate;
end;

procedure TOversampleTemplateDataModule.VSTModuleGetVU(var VU: Single);
begin
 if VstHost[0].Active then VU := VstHost[0].GetVu;
end;

function TOversampleTemplateDataModule.VSTModuleInputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 Result := False;
 if VstHost[0].Active then
  try
   PinProperties := VstHost[0].GetInputProperties(Index);
   Flags         := PinProperties.Flags;
   vLabel        := StrPas(@PinProperties.Caption[0]);
   shortLabel    := StrPas(@PinProperties.ShortLabel[0]);
  except
   Result        := False;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineNotify(Sender: TObject;
  const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
begin
 if VstHost[0].Active then VstHost[0].OfflineNotify(@AudioFile, numAudioFiles, start);
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflinePrepare(Sender: TObject;
  const OfflineTasks: array of TVstOfflineTask);
var
  VstOfflineTaskRecords : array of TVstOfflineTaskRecord;
  i                     : Integer;
begin
 if VstHost[0].Active and (Length(OfflineTasks) > 0) then
  begin
   SetLength(VstOfflineTaskRecords, Length(OfflineTasks));
   for i := 0 to Length(OfflineTasks) - 1
    do VstOfflineTaskRecords[i] := OfflineTasks[i].VstOfflineTaskRecord;
   VstHost[0].OfflinePrepare(@VstOfflineTaskRecords[0], Length(OfflineTasks));
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineRun(Sender: TObject;
  const OfflineTasks: array of TVstOfflineTask);
var
  VstOfflineTaskRecords : array of TVstOfflineTaskRecord;
  i                     : Integer;
begin
 if VstHost[0].Active and (Length(OfflineTasks) > 0) then
  begin
   SetLength(VstOfflineTaskRecords, Length(OfflineTasks));
   for i := 0 to Length(OfflineTasks) - 1
    do VstOfflineTaskRecords[i] := OfflineTasks[i].VstOfflineTaskRecord;
   VstHost[0].OfflineRun(@VstOfflineTaskRecords[0], Length(OfflineTasks));
  end;
end;

function TOversampleTemplateDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 Result := False;
 if VstHost[0].Active then
  try
   PinProperties := VstHost[0].GetOutputProperties(Index);
   Flags         := PinProperties.Flags;
   vLabel        := StrPas(@PinProperties.Caption[0]);
   shortLabel    := StrPas(@PinProperties.ShortLabel[0]);
  except
   Result        := False;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
end;

procedure TOversampleTemplateDataModule.ParamAutomate(
  Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
begin
 Parameter[FBaseParCount + Index] := ParamValue;
end;

procedure TOversampleTemplateDataModule.ParamPreTransBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].TransitionBandwidth := 0.01 * Value;
end;

procedure TOversampleTemplateDataModule.ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := ConvertOrderToString(round(Parameter[Index]));
end;

procedure TOversampleTemplateDataModule.ParamPreFilterOrderValue(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].Order := round(Value);
end;

procedure TOversampleTemplateDataModule.VSTModuleAfterProgramChange(Sender: TObject);
begin
(*
 with VstHost[0] do if Active then
  begin
   SetProgram(CurrentProgram);
   EditIdle;
   if EditorForm is TFmOversampler
    then TFmOversampler(EditorForm).PnGui.Invalidate;
  end;
*)
end;

procedure TOversampleTemplateDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VSTBuffersChanged;
end;

procedure TOversampleTemplateDataModule.VSTBuffersChanged;
begin
 VstHost.BlockSize := BlockSize * FOSFactor;
 with VstHost[0] do if Active then SetBlockSizeAndSampleRate(BlockSize * FOSFactor, SampleRate * FOSFactor);
 FMaximumBlockSize := BlockSize;
 TempBufferSize := FMaximumBlockSize * FOSFactor;
end;

procedure TOversampleTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].SampleRate := SampleRate;
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].SampleRate := SampleRate;

 PluginSampleRateChanged;
end;

procedure TOversampleTemplateDataModule.PluginSampleRateChanged;
begin
 with VstHost[0] do if Active then SetSampleRate(FOSFactor * SampleRate);
end;

procedure TOversampleTemplateDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StartProcess;
end;

procedure TOversampleTemplateDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StopProcess;
end;

procedure TOversampleTemplateDataModule.VSTModuleSuspend(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(False);
end;

function TOversampleTemplateDataModule.VSTModuleVendorSpecific(Sender: TObject;
  const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
 result := 0;
 with VstHost[0] do if Active then result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
end;

procedure TOversampleTemplateDataModule.VSTModuleProcessVarIO(Sender: TObject;
  const varIo: TVstVariableIo);
begin
 with VstHost[0] do if Active then ProcessVarIo(@varIo);
end;

procedure TOversampleTemplateDataModule.VSTModuleResume(Sender: TObject);
begin
 VSTModuleSampleRateChange(Sender, SampleRate);
 with VstHost[0] do if Active then MainsChanged(True);
end;

procedure TOversampleTemplateDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then VstHost[n].Parameter[Index - pnr] := Value;
end;

procedure TOversampleTemplateDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TOversampleTemplateDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

function TOversampleTemplateDataModule.HostCallGetTailSize(Index,
  Value: Integer; ptr: Pointer; opt: Single): Integer;
begin
 with VstHost[0] do
  if Active
   then result := VstHost[0].GetTailSize
   else result := -1;
end;

function TOversampleTemplateDataModule.HostCallIdle(Index, Value: Integer;
  ptr: Pointer; opt: Single): Integer;
begin
 with VstHost[0] do if Active then
  begin
   Idle;
   result := 0;
  end else result := -1;
end;

procedure TOversampleTemplateDataModule.ParamPostCharChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1 do
  case round(Value) of
   4, 5, 6 : FUpsampler[ch].FilterClass := TChebyshev1LP;
   else FUpsampler[ch].FilterClass := TButterworthLP;
  end;
end;

procedure TOversampleTemplateDataModule.ParamPostFilterBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].TransitionBandwidth := 0.01 * Value;
end;

procedure TOversampleTemplateDataModule.ParamPostOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].Order := round(Value);
end;

procedure TOversampleTemplateDataModule.ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1 do
  case round(Value) of
   4, 5, 6 : FUpsampler[ch].FilterClass := TChebyshev1LP;
   else FUpsampler[ch].FilterClass := TButterworthLP;
  end;
end;

procedure TOversampleTemplateDataModule.ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  4, 5, 6 : PreDefined := 'Chebyshev';
  else PreDefined := 'Butterworth';
 end;
end;

procedure TOversampleTemplateDataModule.ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index])) + 'x';
end;

function ConvertOrderToString(Order: Integer): string;
begin
 case Order of
   0 : result := 'Off';
   1 : result := IntToStr(Order) + 'st';
   2 : result := IntToStr(Order) + 'nd';
   3 : result := IntToStr(Order) + 'rd';
  else result := IntToStr(Order) + 'th';
 end;
end;

procedure TOversampleTemplateDataModule.ParamOversamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Boolean(round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TOversampleTemplateDataModule.ParamOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do sleep(1);
 inc(FSemaphore);
 try
  FOSActive := Boolean(round(Value));
  if FOSActive = True
   then SetOSFactor(round(ParameterByName['OS Factor']))
   else SetOSFactor(1);
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOverSampling;
end;

procedure TOversampleTemplateDataModule.ParamOSFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do sleep(1);
 inc(FSemaphore);
 try
  if FOSActive = True
   then SetOSFactor(round(Value))
   else SetOSFactor(1);
 finally
  dec(FSemaphore);
 end;

 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOSFactor;
end;

procedure TOversampleTemplateDataModule.SetOSFactor(const NewOSFactor: Integer);
var
  ch : Integer;
begin
 FOSFactor := NewOSFactor;
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].Factor := FOSFactor;
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].Factor := FOSFactor;
 TempBufferSize := FMaximumBlockSize * FOSFactor;
 PluginSampleRateChanged;
end;

procedure TOversampleTemplateDataModule.SetTempBufferSize(const Value: Integer);
var
  i : Integer;
begin
 if (FTempBufferSize <> Value) and
   ((Length(FIn64) > 0) or (Length(FOut64) > 0)) then
  begin
   FTempBufferSize := Value;
   {$IFDEF DELPHI10}
   SetMinimumBlockAlignment(mba16Byte);
   {$ENDIF}
   for i := 0 to numInputs - 1 do
    begin
     ReallocMem(FIn64[i], FTempBufferSize * SizeOf(Double));
     FIn32[i] := PDAVSingleFixedArray(FIn64[i]);
     ReallocMem(FOut64[i], FTempBufferSize * SizeOf(Double));
     FOut32[i] := PDAVSingleFixedArray(FOut64[i]);
    end;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleProcessEvents(Sender: TObject;
  Events: PVstEvents);
begin
 if VstHost[0].Active then VstHost[0].ProcessEvents(Events);
end;

procedure TOversampleTemplateDataModule.CheckSampleFrames(const SampleFrames: Integer);
begin
 if SampleFrames > FMaximumBlockSize then
  begin
   FMaximumBlockSize := SampleFrames;
   if TempBufferSize < FMaximumBlockSize * FOSFactor
    then TempBufferSize := FMaximumBlockSize * FOSFactor;
  end;
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
const
  SCRound8087CW : Word = $133F; // round FPU codeword, with exceptions disabled
asm
 fnclex                  // Don't raise pending exceptions enabled by the new flags
 fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess32OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 while FSemaphore > 0 do;
 inc(FSemaphore);
 if FOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FUpsampler[ch].Upsample32(Inputs[ch, i], @FIn32[ch, i * FOSFactor]);

(*
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames * FOSFactor - 1
     do assert(not IsNaN(FIn32[ch, i]));

   DontRaiseExceptionsAndSetFPUcodeword;
*)
   // process serial chain
   if VstHost[0].Active
    then VstHost[0].ProcessReplacing(@FIn32[0], @FOut32[0], SampleFrames * FOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(FIn32[ch, 0], FOut32[ch, 0], SampleFrames * SizeOf(Single) * FOSFactor);

(*
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames * FOSFactor - 1
     do assert(not IsNaN(FOut32[ch, i]));
*)

   // downsample
   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := FDownsampler[ch].Downsample32(@FOut32[ch, i * FOSFactor]);
  end
 else
  begin
   if VstHost[0].Active
    then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(Inputs[ch, 0], Outputs[ch, 0], SampleFrames * SizeOf(Single) * FOSFactor);
  end;
 dec(FSemaphore);
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess64OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 while FSemaphore > 0 do;
 inc(FSemaphore);
 if FOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FUpsampler[ch].Upsample64(Inputs[ch, i], @FIn64[ch, i * FOSFactor]);

   // process serial chain
   if VstHost[0].Active
    then VstHost[0].ProcessDoubleReplacing(@FIn64[0], @FOut64[0], SampleFrames * FOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(FIn64[ch, 0], FOut64[ch, 0], SampleFrames * SizeOf(Single) * FOSFactor);

   // downsample
   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := FDownsampler[ch].Downsample64(@FOut64[ch, i * FOSFactor]);
  end
 else
  if VstHost[0].Active
   then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
   else
    for ch := 0 to min(numInputs, numOutputs) - 1
     do Move(Inputs[ch], Outputs[ch, 0], SampleFrames * SizeOf(Double) * FOSFactor);
 dec(FSemaphore);
end;

end.
