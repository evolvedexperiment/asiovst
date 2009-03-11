unit DAV_VSTModuleWithPrograms;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_VSTEffect, DAV_VSTModuleWithMidi, DAV_VSTParameters,
  DAV_VSTPrograms;

type
  TGetChunkParameterEvent = function(Sender: TObject; const Index: Integer): Single of object;
  TOnBeginLoadBankEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;
  TOnBeginLoadProgramEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;

  TVSTModuleWithPrograms = class(TVSTModuleWithMidi)
  private
    function TranslateParameterNameToIndex(ParameterName: string): Integer;
    function TranslateProgramNameToIndex(ProgramName: string): Integer;
    function GetParameterByName(ParameterName: string): Single;
    function GetVstProgramByName(ProgramName: string): TVstProgram;
    procedure SetParameterByName(ParameterName: string; const Value: Single);
    procedure SetVstProgramByName(ProgramName: string; const Value: TVstProgram);
    procedure SetParameterProperties(const Value: TCustomVstParameterProperties);
    procedure SetParameterCategories(const Value: TCustomVstParameterCategories);
    procedure SetVstPrograms(const Value: TCustomVstPrograms);
    function GetParameterDisplay(Index: Integer): string;
    function GetParameterLabel(Index: Integer): string;
    function GetParameterName(Index: Integer): string;
  protected
    FParameterUpdate        : Boolean;
    FUseDefaultStr2Param    : Boolean;
    FCurProgram             : Integer;
    FVstPrograms            : TCustomVstPrograms;
    FParameter              : TDAVSingleDynArray;
    FChunkData              : TMemoryStream;
    FParameterProperties    : TCustomVstParameterProperties;
    FParameterCategories    : TCustomVstParameterCategories;
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
    function  HostCallVendorSpecific(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    procedure CurrentProgramChanged; virtual;
    procedure SetCurrentProgramName(AName: string); virtual;
    procedure SetNumParams(const Value: Integer); virtual;
    procedure SetNumPrograms(const Value: Integer); virtual;
    procedure SetParameter(const Index: Integer; Value: Single); virtual;
    procedure SetParameterAutomated(Index: Integer; const Value: Single); override;
    procedure SetProgram(const AProgramIndex: Integer); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetProgramParameters(const ProgramIndex: Integer; Parameters: TDAVSingleDynArray); virtual;
    procedure SetParameterCount(const Value: Integer);

    function  HostCallGetParameter(const Index: Integer): Single; override;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); override;

    function HostCallEditOpen                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetProgramm             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetProgramm             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetProgramName          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetProgramName          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetParamLabel           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetParamDisplay         (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetParamName            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetChunk                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetChunk                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallCanBeAutomated          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallString2Parameter        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetNumProgramCategories (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetProgramNameIndexed   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetParameterProperties  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallBeginSetProgram         (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEndSetProgram           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallBeginLoadBank           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallBeginLoadProgram        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;

    property UseDefaultString2ParameterHandler: Boolean read FUseDefaultStr2Param write FUseDefaultStr2Param default False;
    property numParams: Integer read FEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read FEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read FCurProgram write SetProgram;
    property CurrentProgramName: string read GetCurrentProgramName write SetCurrentProgramName;
    property Chunk: TMemoryStream read FChunkData;
    property Programs: TVstPrograms read FVstPrograms write SetVstPrograms;
    property ProgramByName[ProgramName: string]: TVstProgram read GetVstProgramByName write SetVstProgramByName;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
    property ParameterCategories: TCustomVstParameterCategories read FParameterCategories write SetParameterCategories;
    property Parameter[Index: Integer]: Single read GetParameter write SetParameterAutomated;
    property ParameterByName[ParameterName: string]: Single read GetParameterByName write SetParameterByName;

    property ParameterName[Index: Integer]: string read GetParameterName;
    property ParameterLabel[Index: Integer]: string read GetParameterLabel;
    property ParameterDisplay[Index: Integer]: string read GetParameterDisplay;

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
  SysUtils, Math, DAV_VSTCustomModule;

resourcestring
  RStrUndefined = 'undefined';
  RStrNoParameterAvailable = 'No parameter available!';
  RStrUnknownParameterName = 'Unknown parameter name';
  RStrNoProgramAvailable = 'No program available!';
  RStrUnknownProgramName = 'Unknown program name';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrParameterMismatch = 'Parameter mismatch (%d)';

constructor TVSTModuleWithPrograms.Create(AOwner: TComponent);
begin
 inherited;
 FCurProgram          := -1;
 FParameterProperties := TCustomVstParameterProperties.Create(Self);
 FParameterCategories := TCustomVstParameterCategories.Create(Self);
 FVstPrograms         := TCustomVstPrograms.Create(Self);
 FParameterUpdate     := False;
 FChunkData           := TMemoryStream.Create;
end;

destructor TVSTModuleWithPrograms.Destroy;
begin
 try
  if Assigned(FParameterProperties) then FreeAndNil(FParameterProperties);
  if Assigned(FParameterCategories) then FreeAndNil(FParameterCategories);
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

function TVSTModuleWithPrograms.GetParameterDisplay(Index: Integer): string;
begin
 if (Index >= FEffect.numParams) or (Index >= FParameterProperties.Count)
  then result := RStrUndefined
  else
   begin
    if (effFlagsProgramChunks in FEffect.EffectFlags)
     then result := FloatToStr(FOnGetChunkParamEvent(Self, Index))
     else if (numPrograms > 0)
      then result := FloatToStrF(Programs[FCurProgram].Parameter[Index], ffGeneral, 4, 4)
      else result := FloatToStrF(FParameter[Index], ffGeneral, 4, 4);

    with FParameterProperties[Index] do
     if Assigned(OnCustomParameterDisplay)
      then OnCustomParameterDisplay(Self, Index, result);
   end;

 if FTruncateStrings and (Length(result) > 8)
  then SetLength(result, 8);
end;

function TVSTModuleWithPrograms.GetParameterLabel(Index: Integer): string;
begin
 if (Index >= FEffect.numParams) or (Index >= FParameterProperties.Count)
  then result := RStrUndefined
  else
   begin
    result := FParameterProperties[Index].Units;
    if Assigned(FParameterProperties[Index].OnCustomParameterLabel)
     then FParameterProperties[Index].OnCustomParameterLabel(Self, Index, result);
   end;
 if FTruncateStrings and (Length(result) > 8)
  then SetLength(result, 8);
end;

function TVSTModuleWithPrograms.GetParameterName(Index: Integer): string;
begin
 if (Index >= FEffect.numParams) or (Index >= FParameterProperties.Count)
  then result := RStrUndefined
  else result := FParameterProperties[Index].DisplayName;

 if FTruncateStrings and (Length(result) > 8)
  then SetLength(result, 8);
end;

function TVSTModuleWithPrograms.HostCallGetParameter(const Index: Integer): Single;
begin
 if (Index < numParams) and (Index < FParameterProperties.Count)
  then Result := Parameter2VSTParameter(GetParameter(Index), Index)
  else Result := 0;
end;

procedure TVSTModuleWithPrograms.HostCallSetParameter(const Index: Integer; const Value: Single);
begin
 if FIsHostAutomation then exit;

 FIsHostAutomation := True;
 if ((Index >= numParams) or (Index >= FParameterProperties.Count)) and
    Assigned(FOnParameterSizeFailed)
  then FOnParameterSizeFailed(TVSTModuleWithPrograms(Effect^.vObject))
  else SetParameter(Index, VSTParameter2Parameter(Value, Index));

 FIsHostAutomation := False;
end;

function TVSTModuleWithPrograms.HostCallSetProgramm(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if (Value < FEffect.numPrograms) and (Value >= 0) and (Value <> FCurProgram)
  then CurrentProgram := Value;
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgramm(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := FCurProgram;
end;

function TVSTModuleWithPrograms.HostCallSetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if numPrograms > 0
  then Programs[FCurProgram].DisplayName := string(PChar(ptr));

 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 if numPrograms > 0
  then str := Programs[FCurProgram].DisplayName
  else str := '';

 if FTruncateStrings and (Length(str) > 24)
  then SetLength(str, 24);
 StrPCopy(ptr, str);
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamLabel(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Str := ParameterLabel[Index];
 StrPCopy(ptr, str);
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamDisplay(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 str := ParameterDisplay[Index];
 StrPCopy(ptr, str);
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetParamName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 str := ParameterName[Index];
 StrPCopy(ptr, str);
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallEditOpen(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  i, pr : Integer;
  tmp   : Single;
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

function TVSTModuleWithPrograms.HostCallGetChunk(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
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

function TVSTModuleWithPrograms.HostCallSetChunk(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
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

function TVSTModuleWithPrograms.HostCallCanBeAutomated(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if Index < ParameterProperties.Count
  then Result := Integer(ParameterProperties[Index].CanBeAutomated)
  else Result := 1;
end;

function TVSTModuleWithPrograms.HostCallString2Parameter(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  tmp : string;
  val : Single;
begin
 if (Index < 0) or (Index >= 0)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 with ParameterProperties[Index] do
  begin
   Result := Integer(assigned(OnStringToParameterDisplay) or FUseDefaultStr2Param);
   if Ptr <> nil then
    begin
     tmp := StrPas(pchar(ptr));
     if FUseDefaultStr2Param then
      try
       Parameter[Index] := StrToFloat(tmp);
      except
      end;
     if assigned(ParameterProperties[Index].OnStringToParameterDisplay) then
      begin
       Val := Parameter[Index];
       OnStringToParameterDisplay(ParameterProperties[Index], tmp, Val);
       Parameter[Index] := Val;
      end;
    end;
  end;
end;

function TVSTModuleWithPrograms.HostCallVendorSpecific(const Index, Value: Integer;
  const ptr: pointer; const opt: Single): Integer;
var
  ParamStr  : string;
  ParamUnit : string;
begin
 result := inherited HostCallVendorSpecific(Index, Value, ptr, opt);
 if (vcdCockosExtension in CanDos) then
  begin
   if (Index = Integer(effGetParamDisplay)) and
      assigned(ptr) and (Value >= 0) and (Value < numParams) then
    begin
     ParamStr := FloatToStrF(Opt, ffGeneral, 5, 5);
     with ParameterProperties[Value] do
      begin
       if assigned(OnCustomParameterDisplay)
        then OnCustomParameterDisplay(Self, Value, ParamStr);
       ParamUnit := Units;
       if assigned(OnCustomParameterLabel)
        then OnCustomParameterDisplay(Self, Value, ParamUnit);
       ParamStr := ParamStr + ParamUnit;
      end;
     ParamStr := ParamStr + #0;
     StrCopy(ptr, PChar(ParamStr));
     result := $BEEF;
    end else
   if (Index = Integer($DEADBEF0)) and assigned(Ptr) and
      (Value >= 0) and (Value < numParams) then
    begin
     PDAV2SingleArray(Ptr)^[0] := 0;
     PDAV2SingleArray(Ptr)^[1] := 1;
     result := $BEEF;
    end;
  end;
end;

procedure TVSTModuleWithPrograms.Loaded;
begin
 inherited;
 FParameterCategories.CheckParametersInUse;
end;

function TVSTModuleWithPrograms.HostCallGetNumProgramCategories(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
  Result := FNumCategories;
end;

function TVSTModuleWithPrograms.HostCallGetProgramNameIndexed(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Result := 0;
 if (Index < FEffect.numPrograms) and not (Index < 0) then
  begin
   str := Programs[Index].DisplayName;
   if FTruncateStrings and (Length(str) > 24)
    then SetLength(str, 24);
   StrPCopy(ptr, str);
   Result := 1;
  end;
end;


function TVSTModuleWithPrograms.HostCallGetParameterProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
  Result := Integer(ParameterProperties[Index].ReportVST2Properties);
  if Result > 0 then
   with PVstParameterPropertyRecord(ptr)^ do
    begin
     // copy display name
     StrCopy(Caption, @ParameterProperties[Index].DisplayName[1]);

     // copy short label
     str := ParameterProperties[Index].ShortLabel;
     StrCopy(shortLabel, @str);

     // assign flags
     Flags := ParameterProperties[Index].Flags;

     // use integer min/max
     if kVstParameterUsesIntegerMinMax in Flags then
      begin
       minInteger       := ParameterProperties[Index].MinInteger;
       maxInteger       := ParameterProperties[Index].MaxInteger;
      end;

     // use integer steps
     if kVstParameterUsesIntStep in Flags then
      begin
       stepInteger      := ParameterProperties[Index].StepInteger;
       largeStepInteger := ParameterProperties[Index].LargeStepInteger;
      end;

     // use float steps
     if kVstParameterUsesFloatStep in Flags then
      begin
       stepFloat        := ParameterProperties[Index].StepFloat;
       largeStepFloat   := ParameterProperties[Index].LargeStepFloat;
       smallStepFloat   := ParameterProperties[Index].SmallStepFloat;
      end;

     // assign display index
     if kVstParameterSupportsDisplayIndex in Flags
      then displayIndex := Index;

     // copy category label
     if kVstParameterSupportsDisplayCategory in Flags then
      begin
       str := ParameterProperties[Index].Category;
       StrCopy(CategoryLabel, @str);
       Category := ParameterProperties[Index].CategoryIndex;
       if (Category > 0) and (Category <= ParameterCategories.Count)
        then numParametersInCategory := ParameterCategories[Category - 1].ParametersInCategory
        else numParametersInCategory := 0;
      end;
    end;
end;

function TVSTModuleWithPrograms.HostCallBeginSetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if Assigned(FOnBeginSetProgram) then
  begin
   FOnBeginSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;

function TVSTModuleWithPrograms.HostCallEndSetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if Assigned(FOnEndSetProgram) then
  begin
   FOnEndSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;


function TVSTModuleWithPrograms.HostCallBeginLoadBank(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
  then Result := -1
  else Result := 0;

 if Assigned(FOnBeginLoadBank) then FOnBeginLoadBank(Self, PVstPatchChunkInfo(ptr)^)
end;

function TVSTModuleWithPrograms.HostCallBeginLoadProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
  then Result := -1
  else Result := 0;

 if Assigned(FOnBeginLoadProgram) then FOnBeginLoadProgram(Self, PVstPatchChunkInfo(ptr)^)
end;


procedure TVSTModuleWithPrograms.SetNumParams(const Value: Integer);
begin
 if Assigned(FParameterProperties)
  then FEffect.numParams := FParameterProperties.Count
  else FEffect.numParams := 0;
end;

procedure TVSTModuleWithPrograms.SetNumPrograms(const Value: Integer);
begin
 if Assigned(fVstPrograms)
  then FEffect.numPrograms := FVstPrograms.Count
  else FEffect.numPrograms := 0;
end;

procedure TVSTModuleWithPrograms.SetProgram(const AProgramIndex: Integer);
var
  NeedProgramUpdate: Boolean;
begin
 if (AProgramIndex >= 0) and (AProgramIndex < FEffect.numPrograms) and (numPrograms > 0) then
  begin
   if Assigned(FOnBeforeProgramChange) then FOnBeforeProgramChange(Self);
   NeedProgramUpdate := FCurProgram >= 0;
   FCurProgram := AProgramIndex;
   if Assigned(FOnAfterProgramChange) then FOnAfterProgramChange(Self);
   if NeedProgramUpdate then CurrentProgramChanged;
  end;
end;

procedure TVSTModuleWithPrograms.CurrentProgramChanged;
var
  i: Integer;
begin
 try
  for i := 0 to Programs[FCurProgram].ParameterCount - 1
   do SetParameter(i, Programs[FCurProgram].Parameter[i]);
 except
 end;
 FEditorNeedUpdate := True;
 UpdateDisplay;
end;

procedure TVSTModuleWithPrograms.SetProgramParameters(
  const ProgramIndex: Integer; Parameters: TDAVSingleDynArray);
var
  i : Integer;
begin
 if Length(Parameters) > numParams
  then raise Exception.CreateFmt(RCStrParameterMismatch, [Length(Parameters)]);
 with Programs[ProgramIndex] do
  for i := 0 to Length(Parameters) - 1
   do Programs[ProgramIndex].Parameter[i] := Parameters[i];
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

function TVSTModuleWithPrograms.GetCurrentProgramName: string;
begin
 if (FCurProgram < numPrograms) and (numPrograms > 0) and (FCurProgram >= 0)
  then Result := Programs[FCurProgram].DisplayName
  else Result := '';
end;

procedure TVSTModuleWithPrograms.SetParameterCategories(
  const Value: TCustomVstParameterCategories);
begin
 FParameterCategories.Assign(Value);
end;

procedure TVSTModuleWithPrograms.SetParameterCount(const Value: Integer);
begin
 SetLength(FParameter, Value);
end;


procedure TVSTModuleWithPrograms.SetVstProgramByName(ProgramName: string;
  const Value: TVstProgram);
begin
 Programs[TranslateProgramNameToIndex(ProgramName)] := Value;
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
 assert(Index >= 0);
 assert(Index < numParams);
 assert(Index < FParameterProperties.Count);

 if (Index >= numParams) or
    (Index >= FParameterProperties.Count) then
  begin
   Result := 0;
   Exit;
  end;
 Result := FParameterProperties[Index].Parameter2VSTParameter(Value);
end;

function TVSTModuleWithPrograms.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 assert(Index >= 0);
 assert(Index < numParams);
 assert(Index < FParameterProperties.Count);
 Result := FParameterProperties[Index].VSTParameter2Parameter(Value);
end;

procedure TVSTModuleWithPrograms.SetParameterAutomated(Index: Integer; const Value: Single);
begin
 if (Index >= numParams) or (Index >= FParameterProperties.Count) then Exit;
 setParameter(Index, Value);
 if Assigned(FParameterProperties[Index]) then
  with FParameterProperties[Index] do
   if CanBeAutomated and not FIsHostAutomation
    then inherited SetParameterAutomated(Index, Parameter2VSTParameter(Value));
end;

function TVSTModuleWithPrograms.TranslateParameterNameToIndex(ParameterName: string): Integer;
begin
 if FParameterProperties.Count = 0
  then raise Exception.Create(RStrNoParameterAvailable);
 result := 0;
 while result < FParameterProperties.Count do
  if ParameterName = FParameterProperties[result].DisplayName
   then break
   else inc(result);
 if result = FParameterProperties.Count
  then raise Exception.Create(RStrUnknownParameterName + ': ' + ParameterName);
end;

function TVSTModuleWithPrograms.TranslateProgramNameToIndex(ProgramName: string): Integer;
begin
 if FVstPrograms.Count = 0
  then raise Exception.Create(RStrNoProgramAvailable);
 result := 0;
 while result < FVstPrograms.Count do
  if ProgramName = FVstPrograms[result].DisplayName
   then break
   else inc(result);
 if result = FVstPrograms.Count
  then raise Exception.Create(RStrUnknownProgramName);
end;

procedure TVSTModuleWithPrograms.SetParameterByName(ParameterName: string; const Value: Single);
begin
 Parameter[TranslateParameterNameToIndex(ParameterName)] := Value;
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
   then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
  if (effFlagsProgramChunks in FEffect.EffectFlags)
   then
    begin
     if Assigned(ParameterProperties[Index].OnParameterChange)
      then FParameterProperties[Index].OnParameterChange(Self, Index, Value);
     if Assigned(OnParameterChange)
      then OnParameterChange(Self, Index, Value);
    end
   else
    begin
     if (numPrograms > 0) and (FCurProgram >= 0)
      then
       begin
        Programs[FCurProgram].Parameter[Index] := Value;

        tmp := Programs[FCurProgram].Parameter[Index];

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
  then
   begin
    assert(assigned(FOnGetChunkParamEvent));
    Result := FOnGetChunkParamEvent(Self, Index)
   end
  else
   if numPrograms > 0
    then Result := Programs[FCurProgram].Parameter[Index]
    else Result := FParameter[Index];
// ShowMessage('Parameter: ' + IntToStr(Index) + ': ' + FloatToStr(Result));   
end;

function TVSTModuleWithPrograms.GetParameterByName(ParameterName: string): Single;
begin
 result := Parameter[TranslateParameterNameToIndex(ParameterName)];
end;

function TVSTModuleWithPrograms.GetVstProgramByName(ProgramName: string): TVstProgram;
begin
 result := Programs[TranslateProgramNameToIndex(ProgramName)];
end;

end.
