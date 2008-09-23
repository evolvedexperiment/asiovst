unit CustomWrapperDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, DAV_Common,
  DAV_VSTModule, DAV_VstHost, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel,
  DAV_ChunkPluginGUI;

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
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    fDials      : array of TGuiDial;
    fLabels     : array of TGuiLabel;
    fDisplays   : array of TGuiLabel;
    fMaxInputs  : Integer;
    fMaxOutputs : Integer;
    procedure DialChanged(Sender: TObject);
    procedure AssignKnobBitmap(const Dial: TGuiDial);
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, Controls, PNGImage, DAV_VSTEffect, DAV_VSTParameters,
  DAV_VSTModuleWithPrograms;

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
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  for n := 0 to RN.Count - 1 do
   begin
    PI := VstHost.VstPlugIns.Add;

    // load plugin from resource
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

procedure TCustomWrapperDataModule.VSTModuleDestroy(Sender: TObject);
begin
 VSTHost.VstPlugIns.Clear;
end;

procedure TCustomWrapperDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
var
  i : Integer;
begin
 for i := 0 to Length(fDials) - 1 do FreeAndNil(fDials[i]);
 SetLength(fDials, 0);

 for i := 0 to Length(fLabels) - 1 do FreeAndNil(fLabels[i]);
 SetLength(fLabels, 0);

 for i := 0 to Length(fDisplays) - 1 do FreeAndNil(fDisplays[i]);
 SetLength(fDisplays, 0);
end;

procedure TCustomWrapperDataModule.AssignKnobBitmap(const Dial: TGuiDial);
var
  i, j   : Integer;
  Aspect : Single;
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 if FindResource(hInstance, 'KNOB', 'PNG') = 0 then exit;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(HInstance, 'KNOB', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   i := 0;
   Dial.DialBitmap.Assign(PngBmp);
   with Dial.DialBitmap do
    if Width > Height then
     begin
      Dial.StitchKind := skHorizontal;
      j := Width div Height;
      while True do
       begin
        Aspect := Width / (j + i);
        if (Aspect >= 1) and (abs(Aspect - round(Aspect)) < 1E-24)
         then break;
        Aspect := Width / (j - i);
        if (Aspect > 0) and (abs(Aspect - round(Aspect)) < 1E-24)
         then break
         else inc(i);
       end;
     end
    else
     begin
      Dial.StitchKind := skVertical;
      j := Height div Width;
      while True do
       begin
        Aspect := Height / (j + i);
        if (Aspect >= 1) and (abs(Aspect - round(Aspect)) < 1E-24)
         then break;
        Aspect := Height / (j - i);
        if (Aspect > 0) and (abs(Aspect - round(Aspect)) < 1E-24)
         then break
         else inc(i);
       end;
     end;
   Dial.NumGlyphs := j + i;

  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

end;

procedure TCustomWrapperDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  numElementsPerRow, i : Integer;
  RS                   : TResourceStream;
  ChunkName            : TChunkName;
  FontSize             : Byte;
  FontAntiAlias        : TGuiAntiAlias;
  PluginGUI            : TDAVPluginGuiChunk;
begin
 numElementsPerRow := 4;
 GUI := TForm.Create(Self);

 if FindResource(hInstance, 'PLUGINGUI', PChar(10)) <> 0 then
  begin
   RS := TResourceStream.Create(HInstance, 'PLUGINGUI', PChar(10));
   PluginGUI := TDAVPluginGuiChunk.Create;
   try
    RS.Read(ChunkName, 4);
    assert(ChunkName = 'PGUI');
    PluginGUI.LoadFromStream(RS);
    GUI.Color         := PluginGUI.BackgroundColor;
    numElementsPerRow := PluginGUI.KnobsPerRow;
    FontSize          := PluginGUI.FontSize;
    FontAntiAlias     := PluginGUI.FontAntiAliasing;
   finally
    FreeAndNil(PluginGUI);
   end;
  end
 else
  begin
   FontSize      := 8;
   FontAntiAlias := gaaNone;
   ShowMessage('Could not load GUI definition, defaults used');
  end;

 with GUI do
  begin
   BorderStyle  := bsNone;
   ClientWidth  := numElementsPerRow * 64;
   ClientHeight := ((numParams + numElementsPerRow - 1) div numElementsPerRow) * 96;

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
       AssignKnobBitmap(fDials[i]);
       Left                := 8 + (i mod numElementsPerRow) * (fDials[i].Width + 16);
       Top                 := 24 + (i div numElementsPerRow) * (fDials[i].Height + 48);
       LineWidth           := 2;
       LineColor           := clRed;
       PointerAngles.Range := 270;
       PointerAngles.Start := 225;
       Min                 := 0;
       Max                 := 1;
       Tag                 := i;
       Position            := Parameter[i];
       OnChange            := DialChanged;
       AntiAlias           := FontAntiAlias;
      end;
     fLabels[i] := TGuiLabel.Create(Gui);
     with fLabels[i] do
      begin
       Parent    := GUI;
       Width     := (fDials[i].Width + 16);
       Height    := 16;
       Left      := (i mod numElementsPerRow) * (fDials[i].Width + 16);
       Top       := 8 + (i div numElementsPerRow) * (fDials[i].Height + 48);
       Tag       := i;
       Alignment := taCenter;
       Font.Size := FontSize;
       Caption   := ParameterProperties[i].DisplayName;
       AntiAlias := FontAntiAlias;
      end;
     fDisplays[i] := TGuiLabel.Create(Gui);
     with fDisplays[i] do
      begin
       Parent    := GUI;
       Width     := (fDials[i].Width + 16);
       Height    := 16;
       Left      := (i mod numElementsPerRow) * (fDials[i].Width + 16);
       Top       := (fDials[i].Height + 24) + (i div numElementsPerRow) * (fDials[i].Height + 48);
       Tag       := i;
       Alignment := taCenter;
       Caption   := '';
       Font.Size := FontSize;
       AntiAlias := FontAntiAlias;
      end;
    end;
   if Length(fDials) > 0 then
    begin
     ClientWidth  := min(numElementsPerRow, Length(fDials)) * (fDials[0].Width + 16);
     ClientHeight := ((numParams + numElementsPerRow - 1) div numElementsPerRow) * (fDials[0].Height + 48);
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
  then fDisplays[Index].Caption := VstHost[n].GetParamDisplay(Index - pnr) +
                                   ' ' + VstHost[n].GetParamLabel(Index - pnr);
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