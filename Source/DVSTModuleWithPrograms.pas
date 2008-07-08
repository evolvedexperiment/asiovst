unit DVSTModuleWithPrograms;

interface

{$I ASIOVST.INC}

uses
  Classes, DVSTModuleWithMidi, DVSTParameters, DVSTPrograms, DVSTEffect;

type
  TGetChunkParameterEvent = function(Sender: TObject; const Index: Integer): Single of object;
  TOnBeginLoadBankEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;
  TOnBeginLoadProgramEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;

  TVSTModuleWithPrograms = class(TVSTModuleWithMidi)
  protected
    FParameterUpdate        : Boolean;
    FCurProgram             : Integer;
    FVstPrograms            : TCustomVstPrograms;
    FParameter              : array of Single;
    FChunkData              : TMemoryStream;
    FParameterProperties    : TCustomVstParameterProperties;
    FIsHostAutomation       : Boolean;

    FOnBeforeProgramChange  : TNotifyEvent;
    FOnAfterProgramChange   : TNotifyEvent;
    FOnParameterSizeFailed  : TNotifyEvent;
    FOnGetChunkParamEvent   : TGetChunkParameterEvent;
    FOnParameterChangeEvent : TParameterChangeEvent;
    FOnBeginLoadBank        : TOnBeginLoadBankEvent;
    FOnBeginLoadProgram     : TOnBeginLoadProgramEvent;
    FOnBeginSetProgram      : TNotifyEvent;
    FOnEndSetProgram        : TNotifyEvent;

    {$IFDEF UseDelphi}
    procedure ReadState(Reader: TReader); override;
    {$ENDIF}
    function  GetCurrentProgramName:string; virtual;
    function  GetParameter(Index: Integer): Single; virtual;
    function  Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
    function  VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
    procedure SetCurrentProgramName(AName: string); virtual;
    procedure SetNumParams(newNum : Integer); virtual;
    procedure SetNumPrograms(newNum : Integer); virtual;
    procedure SetParameter(const Index: Integer; Value: Single); virtual;
    procedure SetParameterAutomated(Index: Integer; Value: Single); override;
    procedure SetParameterProperties(const Value : TCustomVstParameterProperties);
    procedure SetProgram(aProgram: Integer); virtual;
    procedure SetVstPrograms(const Value: TCustomVstPrograms);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetParameterCount(cnt: integer);

    function  HostCallGetParameter(Index: Integer): Single; override;
    procedure HostCallSetParameter(Index: Integer; Value: Single); override;

    function HostCallEditOpen                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetProgramm               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetProgramm               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetProgramName            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetProgramName            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetParamLabel             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetParamDisplay           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetParamName              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetChunk                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetChunk                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallCanBeAutomated            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallString2Parameter          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetNumProgramCategories   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetProgramNameIndexed     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetParameterProperties    (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallBeginSetProgram           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEndSetProgram             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallBeginLoadBank             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallBeginLoadProgram          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;

    property numParams: Integer read FEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read FEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read FCurProgram write SetProgram;
    property CurrentProgramName: string read GetCurrentProgramName write SetCurrentProgramName;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
    property Programs: TCustomVstPrograms read FVstPrograms write SetVstPrograms;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
    property Parameter[Index: Integer]: Single read getParameter write setParameterAutomated;

    property OnParameterChange: TParameterChangeEvent read FOnParameterChangeEvent write FOnParameterChangeEvent;
    property OnBeginSetProgram: TNotifyEvent read FOnBeginSetProgram write FOnBeginSetProgram;
    property OnEndSetProgram: TNotifyEvent read FOnEndSetProgram write FOnEndSetProgram;
    property OnBeginLoadBank: TOnBeginLoadBankEvent read FOnBeginLoadBank write FOnBeginLoadBank;
    property OnBeginLoadProgram: TOnBeginLoadProgramEvent read FOnBeginLoadProgram write FOnBeginLoadProgram;
    property OnParameterSizeFailed: TNotifyEvent read FOnParameterSizeFailed write FOnParameterSizeFailed;
    property OnBeforeProgramChange: TNotifyEvent read FOnBeforeProgramChange write FOnBeforeProgramChange;
    property OnAfterProgramChange: TNotifyEvent read FOnAfterProgramChange write FOnAfterProgramChange;
    property OnGetChunkParameter: TGetChunkParameterEvent read FOnGetChunkParamEvent write FOnGetChunkParamEvent;
  end;


implementation

uses
  SysUtils, Math, DAVDCommon;

constructor TVSTModuleWithPrograms.Create(AOwner: TComponent);
begin
 inherited;
 FCurProgram := -1;
 FParameterProperties := TCustomVstParameterProperties.Create(Self);
 FVstPrograms := TCustomVstPrograms.Create(Self);
 FParameterUpdate := False;
 FChunkData := TMemoryStream.Create;
end;

destructor TVSTModuleWithPrograms.Destroy;
begin
 try
  if Assigned(FParameterProperties) then FreeAndNil(FParameterProperties);
  if Assigned(FVstPrograms) then FreeAndNil(FVstPrograms);
  if Assigned(FChunkData) then FreeAndNil(FChunkData);
 finally
  inherited;
 end;
end;

{$IFDEF UseDelphi}
procedure TVSTModuleWithPrograms.ReadState(Reader: TReader);
var
  i : Integer;
begin
 for i := 0 to numPrograms - 1 do
  if Assigned(Programs[i].OnInitialize)
   then Programs[i].OnInitialize(Programs[i]);

 if numPrograms < 0
  then FCurProgram := -1
  else CurrentProgram := 0;

 inherited;
end;
{$ENDIF}

function TVSTModuleWithPrograms.HostCallGetParameter(Index: Integer): Single;
begin
 if (Index < numParams) and (Index < FParameterProperties.Count)
  then Result := Parameter2VSTParameter(GetParameter(Index), Index)
  else Result := 0;
end;

procedure TVSTModuleWithPrograms.HostCallSetParameter(Index: Integer; Value: Single);
begin
 if FIsHostAutomation then exit;

 FIsHostAutomation := True;
 if ((Index >= numParams) or (Index >= FParameterProperties.Count)) and
    Assigned(fOnParameterSizeFailed)
  then fOnParameterSizeFailed(TVSTModuleWithPrograms(Effect^.vObject))
  else SetParameter(Index, VSTParameter2Parameter(Value, Index));

 FIsHostAutomation := False;
end;

function TVSTModuleWithPrograms.HostCallSetProgramm(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if (Value < FEffect.numPrograms) and (Value >= 0) and (Value <> FCurProgram)
  then CurrentProgram := Value;
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgramm(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := FCurProgram;
end;

function TVSTModuleWithPrograms.HostCallSetProgramName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if numPrograms > 0
  then Programs[FCurProgram].DisplayName := string(PChar(ptr));

 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgramName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if numPrograms > 0
  then StrPCopy(ptr, Programs[FCurProgram].DisplayName)
  else StrPCopy(ptr, '');

 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamLabel(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  str : string;
begin
 if (Index >= FEffect.numParams) or (Index>=FParameterProperties.Count)
  then str := 'undefined'
  else
   begin
    str := FParameterProperties[Index].Units;
    if Assigned(FParameterProperties[Index].OnCustomParameterLabel)
     then FParameterProperties[Index].OnCustomParameterLabel(Self,Index,str);
   end;
 StrPCopy(ptr, str);

 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamDisplay(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  str : string;
begin
 if (Index >= FEffect.numParams) or (Index>=FParameterProperties.Count)
  then str := 'undefined'
  else
   begin
    if (effFlagsProgramChunks in FEffect.EffectFlags)
     then str := FloatToStr(FOnGetChunkParamEvent(Self, Index))
     else if (numPrograms > 0)
      then str := FloatToStrF(Programs[FCurProgram].Parameter[Index], ffGeneral, 4, 4)
      else str := FloatToStrF(FParameter[Index], ffGeneral, 4, 4);

    if Assigned(FParameterProperties[Index].OnCustomParameterDisplay)
     then FParameterProperties[Index].OnCustomParameterDisplay(Self, Index, str);
   end;

 StrPCopy(ptr, str);
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if (Index >= FEffect.numParams) or (Index >= FParameterProperties.Count)
  then StrPCopy(ptr, 'undefined')
  else StrPCopy(ptr, FParameterProperties[Index].DisplayName);

  Result := 0;
end;

function TVSTModuleWithPrograms.HostCallEditOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  i, pr : Integer;
  tmp   : single;
begin
 Result := inherited HostCallEditOpen(Index, Value, ptr, opt);

 if (effFlagsHasEditor in FEffect.EffectFlags) and Assigned(FEditorForm) then
  begin
   pr := min(numParams, FParameterProperties.Count);
   if Assigned(FOnParameterChangeEvent) and
      (not (effFlagsProgramChunks in FEffect.EffectFlags)) then
    if numPrograms > 0 then
     for i := 0 to pr - 1 do
      begin
       tmp := Programs[FCurProgram].Parameter[i];
       FOnParameterChangeEvent(Self, i, tmp);
       Programs[FCurProgram].Parameter[i] := tmp;
      end
    else
     for i := 0 to pr - 1
      do FOnParameterChangeEvent(Self, i, FParameter[i]);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetChunk(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  i, j : Integer;
  tmps : TMemoryStream;
begin
 Result := 0;
 if (numPrograms <= 0) then Exit;

 if Index <> 0 then
  with Programs[FCurProgram] do
   begin
    Chunk.Position := 0;
    if Assigned(OnStoreChunk)
     then OnStoreChunk(Programs[FCurProgram], FCurProgram, True);

    pointer(ptr^) := Chunk.Memory;
    Result := Chunk.Size;
   end
  else
   begin
    tmps := TMemoryStream.Create;
    for i := 0 to numPrograms - 1 do
     begin
      Programs[i].Chunk.Position := 0;
      if Assigned(Programs[i].OnStoreChunk)
       then Programs[i].OnStoreChunk(Programs[FCurProgram], FCurProgram, False);

      j := Programs[i].Chunk.Size;
      tmps.Write(j, 4);
      tmps.Write(Programs[i].Chunk.Memory^, Programs[i].Chunk.Size);
     end;
    pointer(ptr^) := tmps.Memory;
    Result := tmps.Size;
   end;
end;

function TVSTModuleWithPrograms.HostCallSetChunk(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  i  : Integer;
  pi : pInteger;
  pb : pbyte;
begin
 Result := 0;
 if (numPrograms <= 0) then Exit;
 if Index <> 0 then
  with Programs[FCurProgram] do
   begin
    Chunk.Clear;
    Chunk.Write(ptr^, Value);
    Chunk.Position := 0;
    Result := Value;

    if Assigned(OnLoadChunk)
     then OnLoadChunk(Programs[FCurProgram], FCurProgram, True);
   end
  else
   begin
    pb := ptr;
    for i := 0 to NumPrograms - 1 do
     begin
      Programs[i].Chunk.Clear;
      pi := pInteger(pb);
      inc(pb, 4);
      Programs[i].Chunk.Write(pb^, pi^);
      Programs[i].Chunk.Position := 0;
      inc(pb, pi^);

      if Assigned(Programs[i].OnLoadChunk)
       then Programs[i].OnLoadChunk(Programs[i], i, False);
     end;
    Result := Value;
    if Assigned(Programs[CurrentProgram].OnLoadChunk)
     then Programs[CurrentProgram].OnLoadChunk(Programs[CurrentProgram], CurrentProgram, False);
  end;
 FEditorNeedUpdate := True;
end;

function TVSTModuleWithPrograms.HostCallCanBeAutomated(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if Index < ParameterProperties.Count
  then Result := Integer(ParameterProperties[Index].CanBeAutomated)
  else Result := 1;
end;

function TVSTModuleWithPrograms.HostCallString2Parameter(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  tmp : string;
begin
 Result := 0;
 if ptr <> nil then
  try
   tmp := pchar(ptr);
   Parameter[Index] := StrtoFloat(tmp);
   Result := 1;
  except
  end;
end;

function TVSTModuleWithPrograms.HostCallGetNumProgramCategories(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := fNumCategories;
end;

function TVSTModuleWithPrograms.HostCallGetProgramNameIndexed(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := 0;
 if (Index < FEffect.numPrograms) and not (Index < 0) then
  begin
   StrPCopy(ptr, Programs[Index].DisplayName);
   Result := 1;
  end;
end;


function TVSTModuleWithPrograms.HostCallGetParameterProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var str: string;
begin
  Result := Integer(ParameterProperties[Index].ReportVST2Properties);
  if Result > 0 then
   with PVstParameterProperties(ptr)^ do
    begin
      StrCopy(Caption, @ParameterProperties[Index].DisplayName[1]);
      str := ParameterProperties[Index].ShortLabel;
      StrCopy(shortLabel, @str);
      minInteger := ParameterProperties[Index].MinInteger;
      maxInteger := ParameterProperties[Index].MaxInteger;
      stepInteger := ParameterProperties[Index].StepInteger;
      largeStepInteger := ParameterProperties[Index].LargeStepInteger;
      stepFloat := ParameterProperties[Index].StepFloat;
      largeStepFloat := ParameterProperties[Index].LargeStepFloat;
      smallStepFloat := ParameterProperties[Index].SmallStepFloat;
      displayIndex := 0;
      Flags := ParameterProperties[Index].Flags;
    end;
end;

function TVSTModuleWithPrograms.HostCallBeginSetProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if Assigned(FOnBeginSetProgram) then
  begin
   FOnBeginSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;

function TVSTModuleWithPrograms.HostCallEndSetProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if Assigned(FOnEndSetProgram) then
  begin
   FOnEndSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;


function TVSTModuleWithPrograms.HostCallBeginLoadBank(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
  then Result := -1
  else Result := 0;

 if Assigned(FOnBeginLoadBank) then FOnBeginLoadBank(Self, PVstPatchChunkInfo(ptr)^)
end;

function TVSTModuleWithPrograms.HostCallBeginLoadProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
   then Result := -1
   else Result := 0;

  if Assigned(FOnBeginLoadProgram) then FOnBeginLoadProgram(Self, PVstPatchChunkInfo(ptr)^)
end;


procedure TVSTModuleWithPrograms.SetNumParams(newNum : Integer);
begin
 if Assigned(FParameterProperties)
  then FEffect.numParams := FParameterProperties.Count
  else FEffect.numParams := 0;
end;

procedure TVSTModuleWithPrograms.SetNumPrograms(newNum : Integer);
begin
 if Assigned(fVstPrograms)
  then FEffect.numPrograms := fVstPrograms.Count
  else FEffect.numPrograms := 0;
end;

procedure TVSTModuleWithPrograms.SetProgram(aProgram: Integer);
var i: Integer;
begin
 if (aProgram >= 0) and (aProgram < FEffect.numPrograms) and (numPrograms > 0) then
  begin
   if Assigned(FOnBeforeProgramChange) then FOnBeforeProgramChange(Self);
   FCurProgram := aProgram;
   if Assigned(FOnAfterProgramChange) then FOnAfterProgramChange(Self);
//   if (effFlagsProgramChunks in FEffect.EffectFlags) then
    try
     for i := 0 to Programs[FCurProgram].ParameterCount - 1
      do setParameter(i, Programs[FCurProgram].Parameter[i]);
    except
    end;
   FEditorNeedUpdate := True;
  end;
 updateDisplay;
end;

procedure TVSTModuleWithPrograms.SetCurrentProgramName(AName: string);
begin
 if (FCurProgram < numPrograms) and (numPrograms > 0) then
  begin
   Programs[FCurProgram].DisplayName := AName;
   FEditorNeedUpdate := True;
  end;
 updateDisplay;
end;

function TVSTModuleWithPrograms.GetCurrentProgramName:string;
begin
 if (FCurProgram<numPrograms) and (numPrograms > 0) and (FCurProgram >= 0)
  then Result := Programs[FCurProgram].DisplayName
  else Result := '';
end;

procedure TVSTModuleWithPrograms.SetParameterCount(cnt: integer);
begin
  setlength(fParameter, cnt);
end;


procedure TVSTModuleWithPrograms.SetVstPrograms(const Value: TCustomVstPrograms);
begin
 FVstPrograms.Assign(Value);
end;

procedure TVSTModuleWithPrograms.SetParameterProperties(const Value : TCustomVstParameterProperties);
begin
 FParameterProperties.Assign(Value);
end;

function TVSTModuleWithPrograms.Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
begin
 if (Index >= numParams) or
    (Index >= FParameterProperties.Count) then
  begin
   Result := 0;
   Exit;
  end;
 with FParameterProperties[Index] do
  begin
   Result := (Value - Min) / (Max - Min);
   case FParameterProperties[Index].Curve of
    ctLogarithmic: Result := log2(CurveFactor * Result + 1) / log2(CurveFactor + 1);
    ctExponential: Result := exp(Result * ln(CurveFactor + 1)) - 1;
    ctFrequencyScale: if min <> 0
                       then Result := log2(Max / Min * Result + 1) / log2(Max / Min)
                       else Result := log2(Max * Result + 1) / log2(Max);
    else
   end;
  end;
 Result := f_limit(Result, 0, 1);
end;

function TVSTModuleWithPrograms.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 Result := Value;
 with FParameterProperties[Index] do
  begin
   case Curve of
    ctLogarithmic: Result := (exp(Result * ln(curveFactor + 1)) - 1) / curveFactor;
    ctExponential: Result := log2(curveFactor * Result + 1) / log2(curveFactor + 1);
    ctFrequencyScale: Result := (exp(Result * ln((Max / Min) + 1)) - 1) / (Max / Min);
   else
   end;
   Result := Smooth(Result * (Max - Min) + Min);
  end;
end;

procedure TVSTModuleWithPrograms.SetParameterAutomated(Index: Integer; Value: Single);
begin
  if (Index >= numParams) or (Index >= FParameterProperties.Count) then Exit;

  setParameter(Index, Value);

  if Assigned(FParameterProperties[Index]) then
    if FParameterProperties[Index].CanBeAutomated and not FIsHostAutomation then
      inherited SetParameterAutomated(Index, Parameter2VSTParameter(Value, Index));
end;


procedure TVSTModuleWithPrograms.SetParameter(const Index: Integer; Value: Single);
var
  tmp: Single;
begin
 if FParameterUpdate then exit;
 FParameterUpdate := True;
 try
  if (Index >= FEffect.numParams) or (Index < 0) or
     (Index >= FParameterProperties.Count)
   then raise Exception.Create('Index out of bounds');
  if (effFlagsProgramChunks in FEffect.EffectFlags)
   then
    begin
     if Assigned(ParameterProperties[Index].OnParameterChange)
      then FParameterProperties[Index].OnParameterChange(Self,Index,Value);
     if Assigned(OnParameterChange)
      then OnParameterChange(Self, Index, Value);
    end
   else
    begin
     if (numPrograms > 0) and (FCurProgram >= 0)
      then
       begin
        Programs[FCurProgram].Parameter[Index] := Value;

        tmp:=Programs[FCurProgram].Parameter[Index];

        if Assigned(ParameterProperties[Index].OnParameterChange)
         then FParameterProperties[Index].OnParameterChange(Self, Index, tmp);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, tmp);

        Programs[FCurProgram].Parameter[Index] := tmp;
       end
      else
       begin
        FParameter[Index] := Value;
        if Assigned(ParameterProperties[Index].OnParameterChange)
         then FParameterProperties[Index].OnParameterChange(Self, Index, FParameter[Index]);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, FParameter[Index]);
       end
    end;
  FEditorNeedUpdate := True;
 finally
  FParameterUpdate := False;
 end;
end;

function TVSTModuleWithPrograms.GetParameter(Index: Integer): Single;
begin
 if (effFlagsProgramChunks in FEffect.EffectFlags)
  then Result := FOnGetChunkParamEvent(Self, Index)
  else
   if numPrograms > 0
    then Result := Programs[FCurProgram].Parameter[Index]
    else Result := FParameter[Index];
end;

end.
