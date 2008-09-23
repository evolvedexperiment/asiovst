unit SplitTemplateDM;

interface

{$I ASIOVST.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VstHost, DAV_DspButterworthFilter, DAV_DspUpDownsampling;

type
  TJoiner = function (const Input1, Input2: Double): Double of object;

  TSplitType = (stSimple, stLinkwitzRiley, stDyn, stLeftRight, stMS);
  TSplitTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure LowParameterAutomate(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
    procedure HighParameterAutomate(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single);
    procedure ParamOversamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fLowpass          : array[0..1, 0..1] of TButterworthLP;
    fHighpass         : array[0..1, 0..1] of TButterworthHP;
    fOversampler      : array[0..1, 0..1] of TDAVUpDownsampling;
    fLow64, fHigh64   : array of PDAVDoubleFixedArray;
    fTmpOutput64      : array of PDAVDoubleFixedArray;
    fLow32, fHigh32   : array of PDAVSingleFixedArray;
    fTmpOutput32      : array of PDAVSingleFixedArray;
    fEnvelope         : array[0..1] of Single;
    fLiRiSign         : Single;
    fSplitType        : TSplitType;
    fOSActive         : Boolean;
    fOSFactor         : Integer;
    fMaximumBlockSize : Integer;
    fTempBufferSize   : Integer;
    fJoiners          : array [0..1] of TJoiner;
    procedure SetOSFactor(NewOSFactor: Integer);
    function JoinInput1(const Input1, Input2: Double): Double;
    function JoinInput2(const Input1, Input2: Double): Double;
    function JoinMid(const Input1, Input2: Double): Double;
    function JoinSide(const Input1, Input2: Double): Double;
    function JoinAdd(const Input1, Input2: Double): Double;
    function JoinDynamicLeft(const Input1, Input2: Double): Double;
    function JoinDynamicRight(const Input1, Input2: Double): Double;
    procedure SetTempBufferSize(const Value: Integer);
    procedure VSTBuffersChanged;
  published
    property SplitType: TSplitType read fSplitType;
    property TempBufferSize: Integer read fTempBufferSize write SetTempBufferSize default 0;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$R *.DFM}

uses
  Math, Dialogs, Controls, PNGImage, DAV_VSTEffect, DAV_VSTParameters,
  DAV_VSTModuleWithPrograms, SplitTemplateGUI, DAV_VSTCustomModule,
  Types;

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
  RN   : TStringList;
  RS   : TResourceStream;
  PI   : TCustomVstPlugIn;
  i, n : Integer;
begin
 RN          := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  if RN.Count > 0 then
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
 finally
  FreeAndNil(RN);
 end;

 for i := 0 to 1 do
  for n := 0 to 1 do
   begin
    fLowpass[i, n]     := TButterworthLP.Create;
    fHighpass[i, n]    := TButterworthHP.Create;
    fOversampler[i, n] := TDAVUpDownsampling.Create(Self);
   end;

 fOSFactor   := 1;
 fOSActive   := False;
 fJoiners[0] := JoinAdd;
 fJoiners[1] := JoinAdd;
 fMaximumBlockSize := 0;
 fTempBufferSize := 0;
 SetLength(fLow32, numInputs);
 SetLength(fLow64, numInputs);
 SetLength(fHigh32, numInputs);
 SetLength(fHigh64, numInputs);
 SetLength(fTmpOutput32, numInputs);
 SetLength(fTmpOutput64, numInputs);
end;

procedure TSplitTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  i, n : Integer;
begin
 for i := 0 to 1 do
  for n := 0 to 1 do
   begin
    fLowpass[i, n]     := TButterworthLP.Create;
    fHighpass[i, n]    := TButterworthHP.Create;
    fOversampler[i, n] := TDAVUpDownsampling.Create(Self);
   end;
 VSTHost.VstPlugIns.Clear;
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

      // Show border
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

procedure TSplitTemplateDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 2000;
 Parameter[2] := 4;
 Parameter[3] := 0;
 Parameter[4] := 2;
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
 VstHost[0].SetBlockSizeAndSampleRate(BlockSize * fOSFactor, SampleRate);
 VstHost[1].SetBlockSizeAndSampleRate(BlockSize * fOSFactor, SampleRate);
 fMaximumBlockSize := BlockSize;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
end;

procedure TSplitTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if VstHost[0].Active then VstHost[0].SetSampleRate(SampleRate);
 if VstHost[1].Active then VstHost[1].SetSampleRate(SampleRate);
end;

procedure TSplitTemplateDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 VstHost[0].StartProcess;
 VstHost[1].StartProcess;
end;

procedure TSplitTemplateDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 VstHost[0].StopProcess;
 VstHost[1].StopProcess;
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
 if SplitType = stLinkwitzRiley
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
  i, n : Integer;
begin
 fOSFactor := NewOSFactor;
 for i := 0 to 1 do
  for n := 0 to 1
   do fOversampler[i, n].Factor := fOSFactor;
 TempBufferSize := fMaximumBlockSize * fOSFactor;
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
  i, n : Integer;
begin
 fLiRiSign := 1 - 2 * (round(Value) mod 2);
 for i := 0 to 1 do
  for n := 0 to 1 do
   begin
    fLowpass[i, n].Order  := round(Value);
    fHighpass[i, n].Order := round(Value);
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOrder;
end;

procedure TSplitTemplateDataModule.ParamFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  i, n : Integer;
begin
 for i := 0 to 1 do
  for n := 0 to 1 do
   begin
    fLowpass[i, n].Frequency  := Value;
    fHighpass[i, n].Frequency := Value;
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateFrequency;
end;

procedure TSplitTemplateDataModule.ParamModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSplitType := TSplitType(round(Value));
 case fSplitType of
  stSimple,
  stLinkwitzRiley : begin
                     fJoiners[0] := JoinAdd;
                     fJoiners[1] := JoinAdd;
                    end;
            stDyn : begin
                     fJoiners[0] := JoinDynamicLeft;
                     fJoiners[1] := JoinDynamicRight;
                    end;
      stLeftRight : begin
                     fJoiners[0] := JoinInput1;
                     fJoiners[1] := JoinInput2;
                    end;
             stMS : begin
                     fJoiners[0] := JoinMid;
                     fJoiners[1] := JoinSide;
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

function TSplitTemplateDataModule.JoinMid(const Input1, Input2: Double): Double;
begin
 result := 0.5 * (Input1 + Input2);
end;

function TSplitTemplateDataModule.JoinSide(const Input1, Input2: Double): Double;
begin
 result := 0.5 * (Input1 - Input2);
end;

function TSplitTemplateDataModule.JoinAdd(const Input1, Input2: Double): Double;
begin
 result := Input1 + Input2;
end;

function TSplitTemplateDataModule.JoinDynamicLeft(const Input1, Input2: Double): Double;
begin
 result := fEnvelope[0] * Input1 + (1 - fEnvelope[0]) * Input2;
end;

function TSplitTemplateDataModule.JoinDynamicRight(const Input1,
  Input2: Double): Double;
begin
 result := fEnvelope[1] * Input1 + (1 - fEnvelope[1]) * Input2;
end;

function TSplitTemplateDataModule.JoinInput1(const Input1,
  Input2: Double): Double;
begin
 result := Input1;
end;

function TSplitTemplateDataModule.JoinInput2(const Input1,
  Input2: Double): Double;
begin
 result := Input2;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  L, H   : Double;
  ch, i  : Integer;
begin
 if SampleFrames > fMaximumBlockSize then
  begin
   fMaximumBlockSize := SampleFrames;
   if TempBufferSize < fMaximumBlockSize * fOSFactor
    then TempBufferSize := fMaximumBlockSize * fOSFactor;
  end;

 for ch := 0 to numInputs - 1 do
  for i := 0 to SampleFrames - 1 do
   begin
    case fSplitType of
     stSimple        : begin
                        L := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
                        H := Inputs[ch, i] - L;
                       end;
     stLinkwitzRiley : begin
                        L := fLowpass[ch, 1].ProcessSample(
                             fLowpass[ch, 0].ProcessSample(fLiRiSign * Inputs[ch, i]));
                        H := fHighpass[ch, 0].ProcessSample(
                             fHighpass[ch, 1].ProcessSample(Inputs[ch, i]));
                       end;
     stDyn           : begin
                        L := Inputs[ch, i]; H := L;
                        fEnvelope[ch] := fLowpass[ch, 0].ProcessSample(abs(L));
                       end;
     stLeftRight     : begin
                        L := Inputs[0, i];
                        H := Inputs[1, i];
                       end;
     stMS            : begin
                        L := Inputs[0, i] + Inputs[1, i];
                        H := Inputs[0, i] - Inputs[1, i];
                       end;
     else
      begin
       L := Inputs[0, i];
       H := Inputs[1, i];
      end;
    end;
    if fOSActive then
     begin
      fOversampler[ch, 0].Upsample32(L, @fLow32[ch, i * fOSFactor]);
      fOversampler[ch, 1].Upsample32(H, @fHigh32[ch, i * fOSFactor]);
     end
    else
     begin
      fLow32[ch, i]  := L;
      fHigh32[ch, i] := H;
     end;
   end;

 if VstHost[0].Active then
  begin
   VstHost[0].ProcessReplacing(@fLow32[0], @fTmpOutput32[0], SampleFrames * fOSFactor);
   for ch := 0 to numInputs - 1
    do Move(fTmpOutput32[ch, 0], fLow32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].ProcessReplacing(@fHigh32[0], @fTmpOutput32[0], SampleFrames * fOSFactor);
   for ch := 0 to numInputs - 1
    do Move(fTmpOutput32[ch, 0], fHigh32[ch, 0], SampleFrames * SizeOf(Single) * fOSFactor);
  end;

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := fOversampler[ch, 0].DownSample32(@fLow32[ch, i * fOSFactor]);
     H := fOversampler[ch, 1].DownSample32(@fHigh32[ch, i * fOSFactor]);

     Outputs[ch, i] := fJoiners[ch](L, H);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fJoiners[ch](fLow32[ch, i], fHigh32[ch, i]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  L, H   : Double;
  ch, i  : Integer;
begin
 if SampleFrames > fMaximumBlockSize then
  begin
   fMaximumBlockSize := SampleFrames;
   if TempBufferSize < fMaximumBlockSize * fOSFactor
    then TempBufferSize := fMaximumBlockSize * fOSFactor;
  end;

 for ch := 0 to numInputs - 1 do
  for i := 0 to SampleFrames - 1 do
   begin
    case fSplitType of
     stSimple        : begin
                        L := fLowpass[ch, 0].ProcessSample(Inputs[ch, i]);
                        H := Inputs[ch, i] - L;
                       end;
     stLinkwitzRiley : begin
                        L := fLowpass[ch, 1].ProcessSample(
                             fLowpass[ch, 0].ProcessSample(Inputs[ch, i]));
                        H := fHighpass[ch, 0].ProcessSample(
                             fHighpass[ch, 1].ProcessSample(Inputs[ch, i]));
                       end;
     stDyn           : begin
                        L := Inputs[ch, i]; H := L;
                        fEnvelope[ch] := fLowpass[ch, 0].ProcessSample(abs(L));
                       end;
     stLeftRight     : begin
                        L := Inputs[0, i];
                        H := Inputs[1, i];
                       end;
     stMS            : begin
                        L := Inputs[0, i] + Inputs[1, i];
                        H := Inputs[0, i] - Inputs[1, i];
                       end;
     else
      begin
       L := Inputs[ch, i];
       H := Inputs[ch, i];
      end;
    end;
    if fOSActive then
     begin
      fOversampler[ch, 0].Upsample64(L, @fLow64[ch, i * fOSFactor]);
      fOversampler[ch, 1].Upsample64(H, @fHigh64[ch, i * fOSFactor]);
     end
    else
     begin
      fLow64[ch, i]  := L;
      fHigh64[ch, i] := H;
     end;
   end;

 if VstHost[0].Active then
  begin
   VstHost[0].ProcessDoubleReplacing(@fLow64[0], @fTmpOutput64[0], SampleFrames * fOSFactor);
   for ch := 0 to numInputs - 1
    do Move(fTmpOutput64[ch, 0], fLow64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].ProcessDoubleReplacing(@fHigh64[0], @fTmpOutput64[0], SampleFrames * fOSFactor);
   for ch := 0 to numInputs - 1
    do Move(fTmpOutput64[ch, 0], fHigh64[ch, 0], SampleFrames * SizeOf(Double) * fOSFactor);
  end;

 if fOSActive then
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := fOversampler[ch, 0].DownSample64(@fLow64[ch, i * fOSFactor]);
     H := fOversampler[ch, 1].DownSample64(@fHigh64[ch, i * fOSFactor]);

     Outputs[ch, i] := fJoiners[ch](L, H);
    end
 else
  for ch := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[ch, i] := fJoiners[ch](fLow64[ch, i], fHigh64[ch, i]);
end;

end.
