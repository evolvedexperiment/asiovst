unit OversampleTemplateDM;

interface

{$I ASIOVST.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VSTEffect, DAV_VSTParameters, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule, DAV_DspUpDownsampling, DAV_VstHost;

type
  TOversampleTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
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
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleEditSleep(Sender: TObject);
    procedure VSTModuleEditTop(Sender: TObject);
    procedure VSTModuleGetVU(var VU: Single);
    procedure VSTModuleOfflineNotify(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
    procedure VSTModuleOfflinePrepare(Sender: TObject; const OfflineTask: TVstOfflineTask; const count: Integer);
    procedure VSTModuleOfflineRun(Sender: TObject; const OfflineTask: TVstOfflineTask; const count: Integer);
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
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOrderValue(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPreTransBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleEditorKeyUp(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleAfterProgramChange(Sender: TObject);
    procedure ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    fOversample       : array of TDAVUpDownsampling;
    fIn64, fOut64     : array of PDAVDoubleFixedArray;
    fIn32, fOut32     : array of PDAVSingleFixedArray;
    fBaseParCount     : Integer;
    fOSActive         : Boolean;
    fOSFactor         : Integer;
    fMaximumBlockSize : Integer;
    fTempBufferSize   : Integer;
    fSemaphore        : Integer;
    procedure SetOSFactor(const NewOSFactor: Integer);
    procedure SetTempBufferSize(const Value: Integer);
    procedure VSTBuffersChanged;
    procedure PluginSampleRateChanged;
    procedure CheckSampleFrames(const SampleFrames: Integer);
  public
    function HostCallIdle(Index: Integer; Value: Integer; ptr: Pointer; opt: Single): Integer; override;
    function HostCallGetTailSize(Index: Integer; Value: Integer; ptr: Pointer; opt: Single): Integer; override;
  published
    property TempBufferSize: Integer read fTempBufferSize write SetTempBufferSize default 0;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$R *.DFM}

uses
  Math, Dialogs, Controls, Types, OversampleTemplateGUI, DAV_VSTPrograms,
  DAV_VSTModuleWithDsp;

function EnumNamesFunc(hModule:THandle; lpType, lpName:PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

function EnumRCDATANamesFunc(hModule:THandle; lpType, lpName:PChar; lParam: DWORD): Boolean; stdcall;
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
 fBaseParCount := numParams;
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

    PI.Active := True;
    for i := 0 to VstHost[0].numParams - 1 do
     with ParameterProperties.Add do
      begin
       OnParameterChange        := VSTModuleParameterChange;
       OnCustomParameterLabel   := CustomParameterLabel;
       OnCustomParameterDisplay := CustomParameterDisplay;
       DisplayName              := VstHost[0].GetParamName(i);
      end;
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

    for i := 0 to VstHost[0].numPrograms - 1 do
     with Programs.Add do
      begin
       PI.GetProgramNameIndexed(0, i, str);
       PI.SetProgram(i);
       Parameter[0] := 0;
       Parameter[1] := 2;
       Parameter[2] := 4;
       Parameter[3] := 99;
       Parameter[4] := 2;
       Parameter[5] := 4;
       Parameter[6] := 99;
       DisplayName := str;
       for j := 0 to PI.numParams - 1
        do Parameter[fBaseParCount + j] := PI.GetParameter(j);
      end;
    if numPrograms > 0 then
     begin
      PI.SetProgram(0);
      for j := 0 to PI.numParams - 1
       do Parameter[fBaseParCount + j] := PI.GetParameter(j);
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
   end;

 finally
  FreeAndNil(RN);
 end;

 SetLength(fOversample, max(numInputs, numOutputs));
 for ch := 0 to Length(fOversample) - 1
  do fOversample[ch] := TDAVUpDownsampling.Create(Self);

 fOSFactor     := 1;
 fOSActive     := False;

 OnProcess := VSTModuleProcess32OversampleSingle;
 OnProcessReplacing := VSTModuleProcess32OversampleSingle;
 OnProcessDoubleReplacing := VSTModuleProcess64OversampleSingle;

 fTempBufferSize := 0;
 fSemaphore := 0;
 fMaximumBlockSize := VstHost.BlockSize;

 SetLength(fIn32, numInputs);
 SetLength(fIn64, numInputs);
 SetLength(fOut32, numOutputs);
 SetLength(fOut64, numOutputs);

 VSTBuffersChanged;
end;

procedure TOversampleTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  ch : Integer;
begin
 fSemaphore := 0;
 for ch := 0 to Length(fIn64) - 1 do Dispose(fIn64[ch]);
 for ch := 0 to Length(fOut64) - 1 do Dispose(fOut64[ch]);

 for ch := 0 to Length(fOversample) - 1
  do FreeAndNil(fOversample[ch]);

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
  then EditKeyDown(Char(keyCode.Character), keyCode.Virt, keyCode.Modifier); 
end;

procedure TOversampleTemplateDataModule.VSTModuleEditorKeyUp(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 with VstHost[0] do if EditVisible
  then EditKeyUp(Char(keyCode.Character), keyCode.Virt, keyCode.Modifier)
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
 PinProperties := VstHost[0].GetInputProperties(Index);
 vLabel := PinProperties.Caption;
 shortLabel := PinProperties.ShortLabel;
 Flags := PinProperties.Flags;
 Result := False;
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineNotify(Sender: TObject;
  const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
begin
 if VstHost[0].Active then VstHost[0].OfflineNotify(@AudioFile, numAudioFiles, start);
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflinePrepare(Sender: TObject;
  const OfflineTask: TVstOfflineTask; const count: Integer);
begin
 if VstHost[0].Active then VstHost[0].OfflinePrepare(@OfflineTask, count);
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineRun(Sender: TObject;
  const OfflineTask: TVstOfflineTask; const count: Integer);
begin
 if VstHost[0].Active then VstHost[0].OfflineRun(@OfflineTask, count);
end;

procedure TOversampleTemplateDataModule.VSTModuleOpen(Sender: TObject);
var
  j : Integer;
begin
 if VstHost[0].Active then
  for j := 0 to VstHost[0].numParams - 1
   do Parameter[fBaseParCount + j] := VstHost[0].GetParameter(j);
end;

function TOversampleTemplateDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 PinProperties := VstHost[0].GetOutputProperties(Index);
 vLabel        := PinProperties.Caption;
 shortLabel    := PinProperties.ShortLabel;
 Flags         := PinProperties.Flags;
 Result        := False;
end;

procedure TOversampleTemplateDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
end;

procedure TOversampleTemplateDataModule.ParamAutomate(
  Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
begin
 Parameter[fBaseParCount + Index] := ParamValue;
end;

procedure TOversampleTemplateDataModule.ParamPreTransBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(fOversample) - 1
  do fOversample[ch].TransitionBandwidth := 0.01 * Value;
 Parameter[Index + 3] := Value;
end;

procedure TOversampleTemplateDataModule.ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := ConvertOrderToString(round(Parameter[Index]));
end;

procedure TOversampleTemplateDataModule.ParamOrderValue(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(fOversample) - 1
  do fOversample[ch].Order := round(Value);
 Parameter[Index + 3] := Value;
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
 VstHost.BlockSize := BlockSize * fOSFactor;
 with VstHost[0] do if Active then SetBlockSizeAndSampleRate(BlockSize * fOSFactor, SampleRate * fOSFactor);
 fMaximumBlockSize := BlockSize;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
end;

procedure TOversampleTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(fOversample) - 1
  do fOversample[ch].SampleRate := SampleRate;

 PluginSampleRateChanged;
end;

procedure TOversampleTemplateDataModule.PluginSampleRateChanged;
begin
 with VstHost[0] do if Active then SetSampleRate(fOSFactor * SampleRate);
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
 with VstHost[0] do if Active then MainsChanged(True);
end;

procedure TOversampleTemplateDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := fBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then VstHost[n].Parameters[Index - pnr] := Value;
end;

procedure TOversampleTemplateDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := fBaseParCount;
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
 pnr := fBaseParCount;
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

procedure TOversampleTemplateDataModule.ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Parameter[Index + 3] := Value;
end;

procedure TOversampleTemplateDataModule.ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := 'Butterworth';
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
 fOSActive := Boolean(round(Value));
 if fOSActive = True
  then SetOSFactor(round(ParameterByName['OS Factor']))
  else SetOSFactor(1);
 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOverSampling;
end;

procedure TOversampleTemplateDataModule.ParamOSFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if fOSActive = True
  then SetOSFactor(round(Value))
  else SetOSFactor(1);

 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOSFactor;
end;

procedure TOversampleTemplateDataModule.SetOSFactor(const NewOSFactor: Integer);
var
  ch : Integer;
begin
 while fSemaphore > 0 do sleep(1);
 inc(fSemaphore);
 fOSFactor := NewOSFactor;
 for ch := 0 to Length(fOversample) - 1
  do fOversample[ch].Factor := fOSFactor;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
 PluginSampleRateChanged;
 dec(fSemaphore);
end;

procedure TOversampleTemplateDataModule.SetTempBufferSize(const Value: Integer);
var
  i : Integer;
begin
 if fTempBufferSize <> Value then
  begin
   fTempBufferSize := Value;
   for i := 0 to numInputs - 1 do
    begin
     {$IFDEF DELPHI10}
     SetMinimumBlockAlignment(mba16Byte);
     {$ENDIF}
     ReallocMem(fIn64[i], fTempBufferSize * SizeOf(Double));
     fIn32[i] := PDAVSingleFixedArray(fIn64[i]);
     ReallocMem(fOut64[i], fTempBufferSize * SizeOf(Double));
     fOut32[i] := PDAVSingleFixedArray(fOut64[i]);
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
 if SampleFrames > fMaximumBlockSize then
  begin
   fMaximumBlockSize := SampleFrames;
   if TempBufferSize < fMaximumBlockSize * fOSFactor
    then TempBufferSize := fMaximumBlockSize * fOSFactor;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess32OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 while fSemaphore > 0 do;
 inc(fSemaphore);
 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversample[ch].Upsample32(Inputs[ch, i], @fIn32[ch, i * fOSFactor]);

   // process serial chain
   if VstHost[0].Active
    then VstHost[0].ProcessReplacing(@fIn32[0], @fOut32[0], SampleFrames * fOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(fIn32[ch, 0], fOut32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);

   // downsample
   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := fOversample[ch].Downsample32(@fOut32[ch, i * fOSFactor]);
  end
 else
  begin
   if VstHost[0].Active
    then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(Inputs[ch, 0], Outputs[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
  end;
 dec(fSemaphore);
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess64OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 while fSemaphore > 0 do;
 inc(fSemaphore);
 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversample[ch].Upsample64(Inputs[ch, i], @fIn64[ch, i * fOSFactor]);

   // process serial chain
   if VstHost[0].Active
    then VstHost[0].ProcessDoubleReplacing(@fIn64[0], @fOut64[0], SampleFrames * fOSFactor)
    else
     for ch := 0 to min(numInputs, numOutputs) - 1
      do Move(fIn64[ch, 0], fOut64[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);

   // downsample
   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := fOversample[ch].Downsample64(@fOut64[ch, i * fOSFactor]);
  end
 else
  if VstHost[0].Active
   then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor)
   else
    for ch := 0 to min(numInputs, numOutputs) - 1
     do Move(Inputs[ch], Outputs[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
 dec(fSemaphore);
end;

end.
