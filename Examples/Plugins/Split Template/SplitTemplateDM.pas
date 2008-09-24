unit SplitTemplateDM;

interface

{$I ASIOVST.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VSTEffect, DAV_VSTParameters, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule, DAV_DspButterworthFilter, DAV_DspUpDownsampling,
  DAV_VstHost;

type
  TLowPassArray = array [0..1] of TButterworthLP;
  THighPassArray = array [0..1] of TButterworthHP;
  TUpDownsampling = array [0..1] of TDAVUpDownsampling;

  TSplitType = (stSimple, stLiRi, stDyn, stLeftRight, stMS, stSerial, stBypass);
  TSplitTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
    function VSTModuleInputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleVendorSpecific(Sender: TObject; const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure HighParameterAutomate(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
    procedure LowParameterAutomate(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
    procedure ParamFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
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
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure VSTModuleProcess32SplitVST(const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitVST(const SampleFrames: Integer);

    // 32 bit stuff
    procedure VSTModuleProcess32SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32SplitDynamic(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32SplitLeftRight(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32SplitMidSide(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess32Serial(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);

    // 64 bit stuff
    procedure VSTModuleProcess64Serial(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitDynamic(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitLeftRight(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitMidSide(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);

    procedure VSTModuleProcessEvents(Sender: TObject; Events: PVstEvents);
    procedure VSTModuleProcessVarIO(Sender: TObject; const varIo: TVstVariableIo);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
  private
    fLowpass          : array of TLowPassArray;
    fHighpass         : array of THighPassArray;
    fOversampler      : array of TUpDownsampling;
    fLow64, fHigh64   : array of PDAVDoubleFixedArray;
    fTmpOutput64      : array of PDAVDoubleFixedArray;
    fLow32, fHigh32   : array of PDAVSingleFixedArray;
    fTmpOutput32      : array of PDAVSingleFixedArray;
    fEnvelope         : array of Single;
    fLiRiSign         : Single;
    fMaxChannels      : Integer;
    fMinChannels      : Integer;
    fSplitType        : TSplitType;
    fOSActive         : Boolean;
    fOSFactor         : Integer;
    fMaximumBlockSize : Integer;
    fTempBufferSize   : Integer;
    procedure SetOSFactor(NewOSFactor: Integer);
    procedure SetTempBufferSize(const Value: Integer);
    procedure VSTBuffersChanged;
    procedure PluginSampleRateChanged;
    procedure CheckSampleFrames(const SampleFrames: Integer);
  published
    property SplitType: TSplitType read fSplitType;
    property TempBufferSize: Integer read fTempBufferSize write SetTempBufferSize default 0;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$R *.DFM}

uses
  Math, Dialogs, Controls, Types, PNGImage, SplitTemplateGUI, DAV_VSTPrograms;

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

procedure TSplitTemplateDataModule.VSTModuleCreate(Sender: TObject);
var
  RN       : TStringList;
  RS       : TResourceStream;
  PI       : TCustomVstPlugIn;
  ch, i, n : Integer;
//  str      : string;
begin
 RN          := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  if RN.Count > 0 then
   begin
    for n := 0 to 1 do
     begin
      PI := VstHost.VstPlugIns[n];

      // load plugin from resource
      RS := TResourceStream.Create(hInstance, RN[n mod RN.Count], 'DLL');
      try
       PI.LoadFromStream(RS);
      finally
       FreeAndNil(RS);
      end;

      PI.Active := True;
      for i := 0 to VstHost[n].numParams - 1 do
       with ParameterProperties.Add do
        begin
         OnParameterChange        := VSTModuleParameterChange;
         OnCustomParameterLabel   := CustomParameterLabel;
         OnCustomParameterDisplay := CustomParameterDisplay;
         DisplayName              := VstHost[n].GetParamName(i);
        end;
     end;
    UniqueID    := 'S' + VstHost[0].UniqueID[1] + VstHost[0].UniqueID[2] + '1';
    EffectName  := 'Splitted ' + VstHost[0].EffectName;
    ProductName := 'Splitted ' + VstHost[0].ProductString;
    VendorName  := 'Delphi ASIO & VST Packages and ' + VstHost[0].VendorString;

(*
    // program replication
    for i := 0 to VstHost[0].numPrograms - 1 do
     with Programs.Add do
      begin
       VstHost[0].GetProgramNameIndexed(0, i, str);
       VstHost[0].SetProgram(i);
       DisplayName := str;
      end;
*)

    if (effFlagsCanDoubleReplacing in VstHost[0].EffectOptions) and
       (effFlagsCanDoubleReplacing in VstHost[1].EffectOptions)
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
    if VstHost[0].numInputs = VstHost[1].numInputs then
     begin
      numInputs  := VstHost[0].numInputs;
      numOutputs := max(2, VstHost[0].numOutputs);
      case numInputs of
       1 : CanDos := CanDos - [vcd2in2out] + [vcd1in1out];
       2 : CanDos := CanDos - [vcd1in1out] + [vcd2in2out];
      end;
     end;
    if VstHost[0].PlugCategory = VstHost[1].PlugCategory
     then PlugCategory := VstHost[0].PlugCategory;
   end;
 finally
  FreeAndNil(RN);
 end;

 if numInputs > numOutputs then
  begin
   fMaxChannels := numInputs;
   fMinChannels := numOutputs;
  end
 else
  begin
   fMaxChannels := numOutputs;
   fMinChannels := numInputs;
  end;

 SetLength(fLowpass, numInputs);
 SetLength(fHighpass, numInputs);
 SetLength(fOversampler, numInputs);
 for ch := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    fLowpass[ch, n]     := TButterworthLP.Create;
    fHighpass[ch, n]    := TButterworthHP.Create;
   end;

 for ch := 0 to fMaxChannels - 1 do
  for n := 0 to 1
   do fOversampler[ch, n] := TDAVUpDownsampling.Create(Self);

 fOSFactor   := 1;
 fOSActive   := False;

 OnProcess := VSTModuleProcess32SplitFrequencySimple;
 OnProcessReplacing := VSTModuleProcess32SplitFrequencySimple;
 OnProcessDoubleReplacing := VSTModuleProcess64SplitFrequencySimple;
 fTempBufferSize := 0;
 fMaximumBlockSize := VstHost.BlockSize;
 SetLength(fEnvelope, fMaxChannels);
 SetLength(fLow32, fMaxChannels);
 SetLength(fLow64, fMaxChannels);
 SetLength(fHigh32, fMaxChannels);
 SetLength(fHigh64, fMaxChannels);
 SetLength(fTmpOutput32, numOutputs);
 SetLength(fTmpOutput64, numOutputs);
 VSTBuffersChanged;
end;

procedure TSplitTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  ch, n : Integer;
begin
 for ch := 0 to fMaxChannels - 1 do Dispose(fLow64[ch]);
 for ch := 0 to fMaxChannels - 1 do Dispose(fHigh64[ch]);
 for ch := 0 to numOutputs - 1   do Dispose(fTmpOutput64[ch]);

 for ch := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    FreeAndNil(fLowpass[ch, n]);
    FreeAndNil(fHighpass[ch, n]);
   end;
 for ch := 0 to fMaxChannels - 1 do
  for n := 0 to 1
   do FreeAndNil(fOversampler[ch, n]);
 VSTHost.VstPlugIns.Clear;
end;

procedure TSplitTemplateDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
begin
 with VstHost[0] do if EditVisible then CloseEdit;
 with VstHost[1] do if EditVisible then CloseEdit;
end;

procedure TSplitTemplateDataModule.VSTModuleEditIdle(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditIdle;
 with VstHost[1] do if EditVisible then EditIdle;
end;

procedure TSplitTemplateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  R        : TRect;
  Oversize : Integer;
begin
 GUI := TFmSplitter.Create(Self);

 // set plugin GUI size
 if assigned(VstHost[0]) and VstHost[0].Active then
  with TFmSplitter(GUI) do
   begin
    PnGui.Visible    := True;
    ShBorder.Visible := True;

    if not VstHost[0].EditVisible
     then VstHost[0].ShowEdit(TForm(PnGui));
    R        := VstHost[0].GetRect;
    Oversize := PnControl.Width - (R.Right - R.Left);
    if Oversize < 0 then
     begin
      // current editor is too small, enlarge!
      PnGui.Align := alClient;
      ClientWidth := (R.Right - R.Left);
      ClientHeight := PnControl.Height + (R.Bottom - R.Top);
      ShBorder.Visible := False;
     end
    else
     begin
      PnGui.Align  := alNone;
      PnGui.Left   := Oversize div 2;
      PnGui.Width  := (R.Right - R.Left);

      // calculate new height and y position
      PnGui.Height := (R.Bottom - R.Top);
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
  with TFmSplitter(GUI) do
   begin
    PnGui.Visible    := False;
    ShBorder.Visible := False;
   end;
end;

procedure TSplitTemplateDataModule.VSTModuleEditSleep(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditDeactivate;
 with VstHost[1] do if EditVisible then EditDeactivate;
end;

procedure TSplitTemplateDataModule.VSTModuleEditTop(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditActivate;
 with VstHost[1] do if EditVisible then EditActivate;
end;

procedure TSplitTemplateDataModule.VSTModuleGetVU(var VU: Single);
begin
 if VstHost[0].Active then VU := VstHost[0].GetVu;
 if VstHost[1].Active then
  if VstHost[1].GetVu > VU then VU := VstHost[1].GetVu;
end;

function TSplitTemplateDataModule.VSTModuleInputProperties(Sender: TObject;
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

procedure TSplitTemplateDataModule.VSTModuleOfflineNotify(Sender: TObject;
  const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
begin
 if VstHost[0].Active then VstHost[0].OfflineNotify(@AudioFile, numAudioFiles, start);
 if VstHost[1].Active then VstHost[1].OfflineNotify(@AudioFile, numAudioFiles, start);
end;

procedure TSplitTemplateDataModule.VSTModuleOfflinePrepare(Sender: TObject;
  const OfflineTask: TVstOfflineTask; const count: Integer);
begin
 if VstHost[0].Active then VstHost[0].OfflinePrepare(@OfflineTask, count);
 if VstHost[1].Active then VstHost[1].OfflinePrepare(@OfflineTask, count);
end;

procedure TSplitTemplateDataModule.VSTModuleOfflineRun(Sender: TObject;
  const OfflineTask: TVstOfflineTask; const count: Integer);
begin
 if VstHost[0].Active then VstHost[0].OfflineRun(@OfflineTask, count);
 if VstHost[1].Active then VstHost[1].OfflineRun(@OfflineTask, count);
end;

procedure TSplitTemplateDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 2000;
 Parameter[2] := 4;
 Parameter[3] := 0;
 Parameter[4] := 2;

 with Programs[0] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 2000;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 3000;
   Parameter[2] := 4;
   Parameter[3] := 1;
   Parameter[4] := 2;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 1;
   Parameter[1] := 1300;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 2;
   Parameter[1] := 1300;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 3;
   Parameter[1] := 200;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
 with Programs[5] do
  begin
   Parameter[0] := 4;
   Parameter[1] := 400;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
 with Programs[6] do
  begin
   Parameter[0] := 5;
   Parameter[1] := 800;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 2;
  end;
end;

function TSplitTemplateDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 PinProperties := VstHost[0].GetOutputProperties(Index);
 vLabel := PinProperties.Caption;
 shortLabel := PinProperties.ShortLabel;
 Flags := PinProperties.Flags;
 Result := False;
end;

procedure TSplitTemplateDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
 VstHost[1].Active := False;
end;

procedure TSplitTemplateDataModule.LowParameterAutomate(
  Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
begin
 Parameter[5 + Index] := ParamValue;
end;

procedure TSplitTemplateDataModule.HighParameterAutomate(
  Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
begin
 Parameter[5 + VstHost[0].numParams + Index] := ParamValue;
end;

procedure TSplitTemplateDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VSTBuffersChanged;
end;

procedure TSplitTemplateDataModule.VSTBuffersChanged;
begin
 VstHost.BlockSize := BlockSize * fOSFactor;
 VstHost[0].SetBlockSizeAndSampleRate(BlockSize * fOSFactor, SampleRate * fOSFactor);
 VstHost[1].SetBlockSizeAndSampleRate(BlockSize * fOSFactor, SampleRate * fOSFactor);
 fMaximumBlockSize := BlockSize;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
end;

procedure TSplitTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 PluginSampleRateChanged;
end;

procedure TSplitTemplateDataModule.PluginSampleRateChanged;
begin
 with VstHost[0] do if Active then SetSampleRate(fOSFactor * SampleRate);
 with VstHost[1] do if Active then SetSampleRate(fOSFactor * SampleRate);
end;

procedure TSplitTemplateDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StartProcess;
 with VstHost[1] do if Active then StartProcess;
end;

procedure TSplitTemplateDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StopProcess;
 with VstHost[1] do if Active then StopProcess;
end;

procedure TSplitTemplateDataModule.VSTModuleSuspend(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(False);
 with VstHost[1] do if Active then MainsChanged(False);
end;

function TSplitTemplateDataModule.VSTModuleVendorSpecific(Sender: TObject;
  const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
 result := 0;
 with VstHost[0] do if Active then result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
 with VstHost[1] do if Active then result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
end;

procedure TSplitTemplateDataModule.VSTModuleProcessVarIO(Sender: TObject;
  const varIo: TVstVariableIo);
begin
 with VstHost[0] do if Active then ProcessVarIo(@varIo);
 with VstHost[1] do if Active then ProcessVarIo(@varIo);
end;

procedure TSplitTemplateDataModule.VSTModuleResume(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(True);
 with VstHost[1] do if Active then MainsChanged(True);
end;

procedure TSplitTemplateDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 5;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 VstHost[n].Parameters[Index - pnr] := Value;
end;

procedure TSplitTemplateDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 5;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TSplitTemplateDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 5;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

procedure TSplitTemplateDataModule.ParamOSFactorDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index])) + 'x';
end;

function ConvertOrderToString(Order: Integer): string;
begin
 case Order of
   1 : result := IntToStr(Order) + 'st';
   2 : result := IntToStr(Order) + 'nd';
   3 : result := IntToStr(Order) + 'rd';
  else result := IntToStr(Order) + 'th';
 end;
end;

procedure TSplitTemplateDataModule.ParamOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if SplitType = stLiRi
  then PreDefined := ConvertOrderToString(2 * round(Parameter[Index]))
  else PreDefined := ConvertOrderToString(round(Parameter[Index]))
end;

procedure TSplitTemplateDataModule.ParamOversamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Boolean(round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TSplitTemplateDataModule.ParamOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOSActive := Boolean(round(Value));
 if fOSActive = True
  then SetOSFactor(round(ParameterByName['OS Factor']))
  else SetOSFactor(1);
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOverSampling;
end;

procedure TSplitTemplateDataModule.ParamOSFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if fOSActive = True
  then SetOSFactor(round(Value))
  else SetOSFactor(1);

 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOSFactor;
end;

procedure TSplitTemplateDataModule.SetOSFactor(NewOSFactor: Integer);
var
  ch, n : Integer;
begin
 fOSFactor := NewOSFactor;
 for ch := 0 to fMaxChannels - 1 do
  for n := 0 to 1
   do fOversampler[ch, n].Factor := fOSFactor;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
 PluginSampleRateChanged;
end;

procedure TSplitTemplateDataModule.SetTempBufferSize(
  const Value: Integer);
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
     ReallocMem(fLow64[i], fTempBufferSize * SizeOf(Double));
     fLow32[i] := PDAVSingleFixedArray(fLow64[i]);
     ReallocMem(fHigh64[i], fTempBufferSize * SizeOf(Double));
     fHigh32[i] := PDAVSingleFixedArray(fHigh64[i]);
     ReallocMem(fTmpOutput64[i], fTempBufferSize * SizeOf(Double));
     fTmpOutput32[i] := PDAVSingleFixedArray(fTmpOutput64[i]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, n : Integer;
begin
 fLiRiSign := 1 - 2 * (round(Value) mod 2);
 for ch := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    fLowpass[ch, n].Order  := round(Value);
    fHighpass[ch, n].Order := round(Value);
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOrder;
end;

procedure TSplitTemplateDataModule.ParamFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, n : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    fLowpass[ch, n].Frequency  := Value;
    fHighpass[ch, n].Frequency := Value;
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateFrequency;
end;

procedure TSplitTemplateDataModule.ParamModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSplitType := TSplitType(round(Value));
 case fSplitType of
     stSimple : begin
                 OnProcess := VSTModuleProcess32SplitFrequencySimple;
                 OnProcessReplacing := VSTModuleProcess32SplitFrequencySimple;
                 OnProcessDoubleReplacing := VSTModuleProcess64SplitFrequencySimple;
                end;
       stLiRi : begin
                 OnProcess := VSTModuleProcess32SplitFrequencyLiRi;
                 OnProcessReplacing := VSTModuleProcess32SplitFrequencyLiRi;
                 OnProcessDoubleReplacing := VSTModuleProcess64SplitFrequencyLiRi;
                end;
        stDyn : begin
                 OnProcess := VSTModuleProcess32SplitDynamic;
                 OnProcessReplacing := VSTModuleProcess32SplitDynamic;
                 OnProcessDoubleReplacing := VSTModuleProcess64SplitDynamic;
                end;
  stLeftRight : begin
                 OnProcess := VSTModuleProcess32SplitLeftRight;
                 OnProcessReplacing := VSTModuleProcess32SplitLeftRight;
                 OnProcessDoubleReplacing := VSTModuleProcess64SplitLeftRight;
                end;
         stMS : begin
                 OnProcess := VSTModuleProcess32SplitMidSide;
                 OnProcessReplacing := VSTModuleProcess32SplitMidSide;
                 OnProcessDoubleReplacing := VSTModuleProcess64SplitMidSide;
                end;
     stSerial : begin
                 OnProcess := VSTModuleProcess32Serial;
                 OnProcessReplacing := VSTModuleProcess32Serial;
                 OnProcessDoubleReplacing := VSTModuleProcess64Serial;
                end;
 end;

 if EditorForm is TFmSplitter then
  with TFmSplitter(EditorForm) do
   begin
    UpdateMode;
    UpdateOrder;
   end;
end;

procedure TSplitTemplateDataModule.ParamModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : Predefined := 'Split';
  1 : Predefined := 'Linkwitz-Riley';
  2 : Predefined := 'L/R';
  3 : Predefined := 'M/S';
 end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcessEvents(Sender: TObject;
  Events: PVstEvents);
begin
 if VstHost[0].Active then VstHost[0].ProcessEvents(Events);
 if VstHost[1].Active then VstHost[1].ProcessEvents(Events);
end;

procedure TSplitTemplateDataModule.CheckSampleFrames(const SampleFrames: Integer);
begin
 if SampleFrames > fMaximumBlockSize then
  begin
   fMaximumBlockSize := SampleFrames;
   if TempBufferSize < fMaximumBlockSize * fOSFactor
    then TempBufferSize := fMaximumBlockSize * fOSFactor;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitVST(const SampleFrames: Integer);
var
  ch : Integer;
begin
 if VstHost[0].Active then
  begin
   VstHost[0].ProcessReplacing(@fLow32[0], @fTmpOutput32[0], SampleFrames * fOSFactor);
   for ch := 0 to numOutputs - 1
    do Move(fTmpOutput32[ch, 0], fLow32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].ProcessReplacing(@fHigh32[0], @fTmpOutput32[0], SampleFrames * fOSFactor);
   for ch := 0 to numOutputs - 1
    do Move(fTmpOutput32[ch, 0], fHigh32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitVST(const SampleFrames: Integer);
var
  ch : Integer;
begin
 if VstHost[0].Active then
  begin
   VstHost[0].ProcessDoubleReplacing(@fLow64[0], @fTmpOutput64[0], SampleFrames * fOSFactor);
   for ch := 0 to numOutputs - 1
    do Move(fTmpOutput64[ch, 0], fLow64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].ProcessDoubleReplacing(@fHigh64[0], @fTmpOutput64[0], SampleFrames * fOSFactor);
   for ch := 0 to numOutputs - 1
    do Move(fTmpOutput64[ch, 0], fHigh64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  L     : Double;
  ch, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
     fOversampler[ch, 0].Upsample32(L, @fLow32[ch, i * fOSFactor]);
     fOversampler[ch, 1].Upsample32(Inputs[ch, i] - L, @fHigh32[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fLow32[ch, i]  := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
     fHigh32[ch, i] := Inputs[ch, i] - fLow32[ch, i];
    end;

 VSTModuleProcess32SplitVST(SampleFrames);

 if fOSActive then
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[ch, i] := fOversampler[ch, 0].DownSample32(@fLow32[ch, i * fOSFactor]) +
                       fOversampler[ch, 1].DownSample32(@fHigh32[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fLow32[ch, i] + fHigh32[ch, i];
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fOversampler[ch, 0].Upsample32(
       fLowpass[ch, 1].ProcessSample(
       fLowpass[ch, 0].ProcessSample(fLiRiSign * Inputs[ch, i])), @fLow32[ch, i * fOSFactor]);
     fOversampler[ch, 1].Upsample32(
       fHighpass[ch, 0].ProcessSample(
       fHighpass[ch, 1].ProcessSample(Inputs[ch, i])), @fHigh32[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fLow32[ch, i]  := fLowpass[ch, 1].ProcessSample(
                       fLowpass[ch, 0].ProcessSample(fLiRiSign * Inputs[ch, i]));;
     fHigh32[ch, i] := fHighpass[ch, 0].ProcessSample(
                       fHighpass[ch, 1].ProcessSample(Inputs[ch, i]));
    end;

 VSTModuleProcess32SplitVST(SampleFrames);

 if fOSActive then
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[ch, i] := fOversampler[ch, 0].DownSample32(@fLow32[ch, i * fOSFactor]) +
                       fOversampler[ch, 1].DownSample32(@fHigh32[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fLow32[ch, i] + fHigh32[ch, i];
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitDynamic(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  L, H  : Double;
  ch, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversampler[ch, 0].Upsample32(Inputs[ch, i], @fTmpOutput32[ch, i * fOSFactor]);

   // process
   with VstHost[0] do
    if Active then ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);
   with VstHost[1] do
    if Active then ProcessReplacing(@fTmpOutput32[0], @fHigh32[0], SampleFrames * fOSFactor);

   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := fOversampler[ch, 0].DownSample32(@fLow32[ch, i * fOSFactor]);
      H := fOversampler[ch, 1].DownSample32(@fHigh32[ch, i * fOSFactor]);
      fEnvelope[ch]  := fLowpass[ch, 0].ProcessSample(abs(Inputs[ch, i]));
      Outputs[ch, i] := fEnvelope[ch] * H + (1 - fEnvelope[ch]) * L;
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then ProcessReplacing(@Inputs[0], @fLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then ProcessReplacing(@Inputs[0], @fHigh32[0], SampleFrames);

   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      fEnvelope[ch]  := fLowpass[ch, 0].ProcessSample(abs(Inputs[ch, i]));
      Outputs[ch, i] := fEnvelope[ch] * fHigh32[ch, i] + (1 - fEnvelope[ch]) * fLow32[ch, i];
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32Serial(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversampler[ch, 0].Upsample32(Inputs[ch, i], @fTmpOutput32[ch, i * fOSFactor]);

   // process serial chain
   if VstHost[0].Active then
    begin
     VstHost[0].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);
     for ch := 0 to numOutputs - 1
      do Move(fLow32[ch, 0], fTmpOutput32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
    end;
   if VstHost[1].Active then
    begin
     VstHost[1].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);
     for ch := 0 to numOutputs - 1
      do Move(fLow32[ch, 0], fTmpOutput32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
    end;

   // downsample 
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := fOversampler[ch, 0].Downsample32(@fTmpOutput32[ch, i * fOSFactor]);
  end
 else
  begin
   if VstHost[0].Active then
    begin
     VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor);

     // move output to input to prepare for the next stage
     if VstHost[1].Active then
      for ch := 0 to fMinChannels - 1
       do Move(Outputs[ch, 0], Inputs[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
    end;
   if VstHost[1].Active
    then VstHost[1].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitLeftRight(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then
  begin
   if fOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample32(Inputs[0, i], @fTmpOutput32[0, i * fOSFactor]);

     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);
     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@fTmpOutput32[0], @fHigh32[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample32(@fLow32[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample32(@fHigh32[0, i * fOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   if fOSActive then
    begin
     // upsample left channel
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample32(Inputs[0, i], @fTmpOutput32[0, i * fOSFactor]);
     move(fTmpOutput32[0, 0], fTmpOutput32[1, 0], SampleFrames * fOSFactor * SizeOf(Single));

     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);

     // upsample right channel
     for i := 0 to SampleFrames - 1
      do fOversampler[1, 0].Upsample32(Inputs[1, i], @fTmpOutput32[0, i * fOSFactor]);
     move(fTmpOutput32[0, 0], fTmpOutput32[1, 0], SampleFrames * fOSFactor * SizeOf(Single));

     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@fTmpOutput32[0], @fHigh32[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample32(@fLow32[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample32(@fHigh32[0, i * fOSFactor]);
      end
    end
   else
    begin
     // move left channel
     move(Inputs[0, 0], fTmpOutput32[0, 0], SampleFrames * SizeOf(Single));
     move(Inputs[0, 0], fTmpOutput32[1, 0], SampleFrames * SizeOf(Single));

     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames);

     // move right channel
     move(Inputs[1, 0], fTmpOutput32[0, 0], SampleFrames * SizeOf(Single));
     move(Inputs[1, 0], fTmpOutput32[1, 0], SampleFrames * SizeOf(Single));

     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@fTmpOutput32[0], @fHigh32[0], SampleFrames);

     move(fLow32[0, 0],  Outputs[0, 0], SampleFrames * SizeOf(Single));
     move(fHigh32[0, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitMidSide(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  L, H : Double;
  i    : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then // process mono here
  begin
   if fOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample32(Inputs[0, i], @fTmpOutput32[0, i * fOSFactor]);

     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@fTmpOutput32[0], @fLow32[0], SampleFrames * fOSFactor);
     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@fTmpOutput32[0], @fHigh32[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample32(@fLow32[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample32(@fHigh32[0, i * fOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].ProcessReplacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   // upsample or move channels
   if fOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      fOversampler[0, 0].Upsample32(Inputs[0, i] + Inputs[1, i], @fLow32[0, i * fOSFactor]);
      fOversampler[0, 1].Upsample32(Inputs[0, i] - Inputs[1, i], @fHigh32[0, i * fOSFactor]);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      fLow32[0, i]  := Inputs[0, i] + Inputs[1, i];
      fHigh32[0, i] := Inputs[0, i] - Inputs[1, i];
     end;
   // dublicate internal channels
   Move(fLow32[0, 0], fLow32[1, 0], SampleFrames * SizeOf(Single) * fOSFactor);
   Move(fHigh32[0, 0], fHigh32[1, 0], SampleFrames * SizeOf(Single) * fOSFactor);

   VSTModuleProcess32SplitVST(SampleFrames);

   // downsample or move channels
   if fOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      L := fOversampler[0, 0].Downsample32(@fLow32[0, i * fOSFactor]);
      H := fOversampler[0, 1].Downsample32(@fHigh32[0, i * fOSFactor]);
      Outputs[0, i] := L + H;
      Outputs[1, i] := L - H;
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      Outputs[0, i] := fLow32[0, i] + fHigh32[0, i];
      Outputs[1, i] := fLow32[0, i] - fHigh32[0, i];
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  L     : Double;
  ch, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
     fOversampler[ch, 0].Upsample64(L, @fLow64[ch, i * fOSFactor]);
     fOversampler[ch, 1].Upsample64(Inputs[ch, i] - L, @fHigh64[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fLow64[ch, i]  := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
     fHigh64[ch, i] := Inputs[ch, i] - fLow64[ch, i];
    end;

 VSTModuleProcess64SplitVST(SampleFrames);

 if fOSActive then
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[ch, i] := fOversampler[ch, 0].DownSample64(@fLow64[ch, i * fOSFactor]) +
                       fOversampler[ch, 1].DownSample64(@fHigh64[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fLow64[ch, i] + fHigh64[ch, i];
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fOversampler[ch, 0].Upsample64(
       fLowpass[ch, 1].ProcessSample(
       fLowpass[ch, 0].ProcessSample(fLiRiSign * Inputs[ch, i])), @fLow64[ch, i * fOSFactor]);
     fOversampler[ch, 1].Upsample64(
       fHighpass[ch, 0].ProcessSample(
       fHighpass[ch, 1].ProcessSample(Inputs[ch, i])), @fHigh64[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     fLow64[ch, i]  := fLowpass[ch, 1].ProcessSample(
                       fLowpass[ch, 0].ProcessSample(fLiRiSign * Inputs[ch, i]));;
     fHigh64[ch, i] := fHighpass[ch, 0].ProcessSample(
                       fHighpass[ch, 1].ProcessSample(Inputs[ch, i]));
    end;

 VSTModuleProcess64SplitVST(SampleFrames);

 if fOSActive then
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[ch, i] := fOversampler[ch, 0].DownSample64(@fLow64[ch, i * fOSFactor]) +
                       fOversampler[ch, 1].DownSample64(@fHigh64[ch, i * fOSFactor]);
    end
 else
  for ch := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fLow64[ch, i] + fHigh64[ch, i];
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitDynamic(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  L, H  : Double;
  ch, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversampler[ch, 0].Upsample64(Inputs[ch, i], @fTmpOutput64[ch, i * fOSFactor]);

   // process
   with VstHost[0] do
    if Active then ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);
   with VstHost[1] do
    if Active then ProcessDoubleReplacing(@fTmpOutput64[0], @fHigh64[0], SampleFrames * fOSFactor);

   for ch := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := fOversampler[ch, 0].DownSample64(@fLow64[ch, i * fOSFactor]);
      H := fOversampler[ch, 1].DownSample64(@fHigh64[ch, i * fOSFactor]);
      fEnvelope[ch]  := fLowpass[ch, 0].ProcessSample(abs(Inputs[ch, i]));
      Outputs[ch, i] := fEnvelope[ch] * H + (1 - fEnvelope[ch]) * L;
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then ProcessDoubleReplacing(@Inputs[0], @fLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then ProcessDoubleReplacing(@Inputs[0], @fHigh64[0], SampleFrames);

   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      fEnvelope[ch]  := fLowpass[ch, 0].ProcessSample(abs(Inputs[ch, i]));
      Outputs[ch, i] := fEnvelope[ch] * fHigh64[ch, i] + (1 - fEnvelope[ch]) * fLow64[ch, i];
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64Serial(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  ch, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if fOSActive then
  begin
   // upsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do fOversampler[ch, 0].Upsample64(Inputs[ch, i], @fTmpOutput64[ch, i * fOSFactor]);

   // process serial chain
   if VstHost[0].Active then
    begin
     VstHost[0].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);
     for ch := 0 to numOutputs - 1
      do Move(fLow64[ch, 0], fTmpOutput64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
    end;
   if VstHost[1].Active then
    begin
     VstHost[1].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);
     for ch := 0 to numOutputs - 1
      do Move(fLow64[ch, 0], fTmpOutput64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
    end;

   // downsample
   for ch := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[ch, i] := fOversampler[ch, 0].Downsample64(@fTmpOutput64[ch, i * fOSFactor]);
  end
 else
  begin
   if VstHost[0].Active then
    begin
     VstHost[0].ProcessDoubleReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor);

     // move output to input to prepare for the next stage
     if VstHost[1].Active then
      for ch := 0 to fMinChannels - 1
       do Move(Outputs[ch, 0], Inputs[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
    end;
   if VstHost[1].Active
    then VstHost[1].ProcessDoubleReplacing(@Inputs[0], @Outputs[0], SampleFrames * fOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitLeftRight(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then
  begin
   if fOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample64(Inputs[0, i], @fTmpOutput64[0, i * fOSFactor]);

     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);
     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@fTmpOutput64[0], @fHigh64[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample64(@fLow64[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample64(@fHigh64[0, i * fOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   if fOSActive then
    begin
     // upsample left channel
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample64(Inputs[0, i], @fTmpOutput64[0, i * fOSFactor]);
     move(fTmpOutput64[0, 0], fTmpOutput64[1, 0], SampleFrames * fOSFactor * SizeOf(Double));

     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);

     // upsample right channel
     for i := 0 to SampleFrames - 1
      do fOversampler[1, 0].Upsample64(Inputs[1, i], @fTmpOutput64[0, i * fOSFactor]);
     move(fTmpOutput64[0, 0], fTmpOutput64[1, 0], SampleFrames * fOSFactor * SizeOf(Double));

     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@fTmpOutput64[0], @fHigh64[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample64(@fLow64[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample64(@fHigh64[0, i * fOSFactor]);
      end
    end
   else
    begin
     // move left channel
     move(Inputs[0, 0], fTmpOutput64[0, 0], SampleFrames * SizeOf(Double));
     move(Inputs[0, 0], fTmpOutput64[1, 0], SampleFrames * SizeOf(Double));

     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames);

     // move right channel
     move(Inputs[1, 0], fTmpOutput64[0, 0], SampleFrames * SizeOf(Double));
     move(Inputs[1, 0], fTmpOutput64[1, 0], SampleFrames * SizeOf(Double));

     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@fTmpOutput64[0], @fHigh64[0], SampleFrames);

     move(fLow64[0, 0],  Outputs[0, 0], SampleFrames * SizeOf(Double));
     move(fHigh64[0, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitMidSide(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  L, H : Double;
  i    : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then // process mono here
  begin
   if fOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do fOversampler[0, 0].Upsample64(Inputs[0, i], @fTmpOutput64[0, i * fOSFactor]);

     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@fTmpOutput64[0], @fLow64[0], SampleFrames * fOSFactor);
     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@fTmpOutput64[0], @fHigh64[0], SampleFrames * fOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := fOversampler[0, 0].DownSample64(@fLow64[0, i * fOSFactor]);
       Outputs[1, i] := fOversampler[1, 0].DownSample64(@fHigh64[0, i * fOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].ProcessDoubleReplacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].ProcessDoubleReplacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   // upsample or move channels
   if fOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      fOversampler[0, 0].Upsample64(Inputs[0, i] + Inputs[1, i], @fLow64[0, i * fOSFactor]);
      fOversampler[0, 1].Upsample64(Inputs[0, i] - Inputs[1, i], @fHigh64[0, i * fOSFactor]);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      fLow64[0, i]  := Inputs[0, i] + Inputs[1, i];
      fHigh64[0, i] := Inputs[0, i] - Inputs[1, i];
     end;
   // dublicate internal channels
   Move(fLow64[0, 0], fLow64[1, 0], SampleFrames * SizeOf(Double) * fOSFactor);
   Move(fHigh64[0, 0], fHigh64[1, 0], SampleFrames * SizeOf(Double) * fOSFactor);

   VSTModuleProcess64SplitVST(SampleFrames);

   // downsample or move channels
   if fOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      L := fOversampler[0, 0].Downsample64(@fLow64[0, i * fOSFactor]);
      H := fOversampler[0, 1].Downsample64(@fHigh64[0, i * fOSFactor]);
      Outputs[0, i] := L + H;
      Outputs[1, i] := L - H;
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      Outputs[0, i] := fLow64[0, i] + fHigh64[0, i];
      Outputs[1, i] := fLow64[0, i] - fHigh64[0, i];
     end;
  end;
end;

end.
