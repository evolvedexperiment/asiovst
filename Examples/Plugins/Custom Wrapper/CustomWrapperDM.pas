unit CustomWrapperDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VstHost, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TCustomWrapperDataModule = class(TVSTModule)
    VstHost: TVstHost;
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
  private
    fDials      : array of TGuiDial;
    fLabels     : array of TGuiLabel;
    fDisplays   : array of TGuiLabel;
    fMaxInputs  : Integer;
    fMaxOutputs : Integer;
    procedure DialChanged(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, DAV_VSTEffect, DAV_VSTParameters, Controls,
  DAV_VSTModuleWithPrograms;

function EnumNamesFunc(hModule:THandle; lpType, lpName:PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

procedure TCustomWrapperDataModule.VSTModuleCreate(Sender: TObject);
var
  RN   : TStringList;
  RS   : TResourceStream;
  PI   : TCustomVstPlugIn;
  i, n : Integer;
begin
 fMaxInputs  := 0;
 fMaxOutputs := 0;
 RN          := TStringList.Create;
 n           := 0;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  for n := 0 to RN.Count - 1 do
   begin
    PI := VstHost.VstPlugIns.Add;
    RS := TResourceStream.Create(hInstance, RN[n], 'DLL');
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
    if PI.numInputs  > fMaxInputs  then fMaxInputs  := PI.numInputs;
    if PI.numOutputs > fMaxOutputs then fMaxOutputs := PI.numOutputs;
   end;
 finally
  FreeAndNil(RN);
 end;
end;

procedure TCustomWrapperDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
var
  i : Integer;
begin
 for i := 0 to Length(fDials) - 1 do
  begin
   FreeAndNil(fDials[i])
  end;
 SetLength(fDials, 0); 
end;

procedure TCustomWrapperDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  i : Integer;
const
  numElementsPerRow = 10;
begin
 GUI := TForm.Create(Self);
 with GUI do
  begin
   BorderStyle  := bsNone;
   ClientWidth  := numElementsPerRow * 64;
   ClientHeight := (numParams div numElementsPerRow) * 96;

   SetLength(fDials, numParams);
   SetLength(fLabels, numParams);
   SetLength(fDisplays, numParams);
   for i := 0 to numParams - 1 do
    begin
     fDials[i] := TGuiDial.Create(Gui);
     with fDials[i] do
      begin
       Parent              := GUI;
       Width               := 48;
       Height              := 48;
       Left                := 8 + (i mod numElementsPerRow) * 64;
       Top                 := 24 + (i div numElementsPerRow) * 96;
       LineWidth           := 2;
       LineColor           := clRed;
       PointerAngles.Range := 270;
       PointerAngles.Start := 225;
       Min                 := 0;
       Max                 := 1;
       Tag                 := i;
       Position            := Parameter[i];
       OnChange            := DialChanged;
//       AntiAlias           := gaaLinear2x;
      end;
     fLabels[i] := TGuiLabel.Create(Gui);
     with fLabels[i] do
      begin
       Parent    := GUI;
       Width     := 64;
       Height    := 16;
       Left      := (i mod numElementsPerRow) * 64;
       Top       := 8 + (i div numElementsPerRow) * 96;
       Tag       := i;
       Alignment := taCenter;
       Caption   := ParameterProperties[i].DisplayName;
//       AntiAlias := gaaLinear2x;
      end;
     fDisplays[i] := TGuiLabel.Create(Gui);
     with fDisplays[i] do
      begin
       Parent    := GUI;
       Width     := 64;
       Height    := 16;
       Left      := (i mod numElementsPerRow) * 64;
       Top       := 72 + (i div numElementsPerRow) * 96;
       Tag       := i;
       Alignment := taCenter;
       Caption   := '';
//       AntiAlias := gaaLinear2x;
      end;
    end;
   GUI.Visible     := True;
  end;
end;

procedure TCustomWrapperDataModule.DialChanged(Sender: TObject);
begin
 if Sender is TGuiDial then
  with TGuiDial(Sender) do
   begin
    Parameter[Tag] := Position;
   end;
end;

procedure TCustomWrapperDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Todo
end;

procedure TCustomWrapperDataModule.VSTModuleClose(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].Active := False;
end;

procedure TCustomWrapperDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].SetBlockSizeAndSampleRate(BlockSize, SampleRate)
end;

procedure TCustomWrapperDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 VstHost[n].Parameters[Index - pnr] := Value;
 if (Index < Length(fDials)) and assigned(fDials[Index])
  then fDials[Index].Position := Value;
 if (Index < Length(fDisplays)) and assigned(fDisplays[Index])
  then fDisplays[Index].Caption := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TCustomWrapperDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].SetSampleRate(SampleRate);
end;

procedure TCustomWrapperDataModule.VSTModuleStartProcess(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].StartProcess;
end;

procedure TCustomWrapperDataModule.VSTModuleStopProcess(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].StopProcess;
end;

procedure TCustomWrapperDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TCustomWrapperDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

procedure TCustomWrapperDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfDoubleDynArray;
  InOut : TDAVArrayOfDoubleDynArray;
begin
 SetLength(Temp, fMaxInputs, SampleFrames);
 ChCnt := min(fMaxInputs, fMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Double));
   VstHost[n].ProcessDoubleReplacing(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

procedure TCustomWrapperDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfSingleDynArray;
  InOut : TDAVArrayOfSingleDynArray;
begin
 SetLength(Temp, fMaxInputs, SampleFrames);
 ChCnt := min(fMaxInputs, fMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Single));
   VstHost[n].Process(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

procedure TCustomWrapperDataModule.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfSingleDynArray;
  InOut : TDAVArrayOfSingleDynArray;
begin
 SetLength(Temp, fMaxInputs, SampleFrames);
 ChCnt := min(fMaxInputs, fMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Single));
   VstHost[n].ProcessReplacing(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

end.
