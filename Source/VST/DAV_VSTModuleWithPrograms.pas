unit DAV_VSTModuleWithPrograms;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_VSTEffect, DAV_VSTModuleWithMidi, DAV_VSTParameters,
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
    function GetParameterDisplay(Index: Integer): string;
    function GetParameterLabel(Index: Integer): string;
    function GetParameterName(Index: Integer): string;
    function GetParameterString(Index: Integer): string;
    procedure SetParameterByName(ParameterName: string; const Value: Single);
    procedure SetVstProgramByName(ProgramName: string; const Value: TVstProgram);
    procedure SetParameterProperties(const Value: TCustomVstParameterProperties);
    procedure SetParameterCategories(const Value: TCustomVstParameterCategories);
    procedure SetVstPrograms(const Value: TCustomVstPrograms);
    procedure SetParameterString(Index: Integer; const Value: string);
  protected
    FCurProgram             : Integer;
    FVstPrograms            : TCustomVstPrograms;
    FParameter              : TDAVSingleDynArray;
    FChunkData              : TMemoryStream;
    FParameterProperties    : TCustomVstParameterProperties;
    FParameterCategories    : TCustomVstParameterCategories;

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
    procedure SetParameterDirect(const Index: Integer; Value: Single); virtual;
    procedure SetParameter(Index: Integer; const Value: Single); virtual;
    procedure SetProgram(const AProgramIndex: Integer); virtual;
    procedure Loaded; override;

    function  HostCallGetParameter(const Index: Integer): Single; override;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); override;

    function HostCallEditOpen                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetProgram              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetProgram              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetProgramParameters(const ProgramIndex: Integer; Parameters: TDAVSingleDynArray); virtual;
    procedure SetParameterCount(const Value: Integer);
    function StringToParameter(const Index: Integer; Text: string): Boolean;

    property numParams: Integer read FEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read FEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read FCurProgram write SetProgram default 0;
    property CurrentProgramName: string read GetCurrentProgramName write SetCurrentProgramName;
    property Chunk: TMemoryStream read FChunkData;
    property Programs: TVstPrograms read FVstPrograms write SetVstPrograms;
    property ProgramByName[ProgramName: string]: TVstProgram read GetVstProgramByName write SetVstProgramByName;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
    property ParameterCategories: TCustomVstParameterCategories read FParameterCategories write SetParameterCategories;
    property Parameter[Index: Integer]: Single read GetParameter write SetParameter;
    property ParameterString[Index: Integer]: string read GetParameterString write SetParameterString;
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
  SysUtils, Math, DAV_VSTCustomModule, DAV_VSTBasicModule;

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
 FChunkData           := TMemoryStream.Create;
 FParameterProperties := TCustomVstParameterProperties.Create(Self);
 FParameterCategories := TCustomVstParameterCategories.Create(Self);
 FVstPrograms         := TCustomVstPrograms.Create(Self);
end;

destructor TVSTModuleWithPrograms.Destroy;
begin
 try
  // free programs
  if Assigned(FVstPrograms) then FreeAndNil(FVstPrograms);

  // free parameter categories and properties
  if Assigned(FParameterCategories) then FreeAndNil(FParameterCategories);
  if Assigned(FParameterProperties) then FreeAndNil(FParameterProperties);

  // free chunk data
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
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
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
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
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
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then result := FParameterProperties[Index].DisplayName
  else result := RStrUndefined;

 if FTruncateStrings and (Length(result) > 8)
  then SetLength(result, 8);
end;

function TVSTModuleWithPrograms.GetParameterString(Index: Integer): string;
begin
 Result := ParameterDisplay[Index] + ' ' + ParameterLabel[Index];  
end;

function TVSTModuleWithPrograms.HostCallGetParameter(const Index: Integer): Single;
begin
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then Result := Parameter2VSTParameter(GetParameter(Index), Index)
  else Result := 0;
end;

procedure TVSTModuleWithPrograms.HostCallSetParameter(const Index: Integer; const Value: Single);
begin
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)) then
  if Assigned(FOnParameterSizeFailed)
   then FOnParameterSizeFailed(Self) else
  else SetParameterDirect(Index, VSTParameter2Parameter(Value, Index));
end;

function TVSTModuleWithPrograms.HostCallSetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if (Value >= 0) or (Value < numPrograms)
  then CurrentProgram := Value;
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := FCurProgram;
end;

function TVSTModuleWithPrograms.HostCallSetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   if numPrograms > 0
    then Programs[FCurProgram].DisplayName := StrPas(PChar(ptr));
  end;
end;

function TVSTModuleWithPrograms.HostCallGetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Result := 0;

 if Assigned(ptr) then
  begin
   if numPrograms > 0
    then str := Programs[FCurProgram].DisplayName
    else str := '';
   if FTruncateStrings and (Length(str) > 24)
    then SetLength(str, 24);
   StrPCopy(ptr, str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamLabel(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   Str := ParameterLabel[Index];
   if FTruncateStrings and (Length(str) > 8)
    then SetLength(str, 8);
   StrPCopy(ptr, str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamDisplay(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   str := ParameterDisplay[Index];
   if FTruncateStrings and (Length(str) > 8)
    then SetLength(str, 8);
   StrPCopy(ptr, str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  str : string;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   str := ParameterName[Index];
   if FTruncateStrings and (Length(str) > 8)
    then SetLength(str, 8);
   StrPCopy(ptr, str);
  end;
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
 if (numPrograms <= 0) or (ptr = nil) then Exit;

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
 if (numPrograms <= 0) or (ptr = nil) then Exit;
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

function TVSTModuleWithPrograms.StringToParameter(const Index: Integer; Text: string): Boolean;
var
  ProcStr : string;
  CurrVal : Single;
  Indxes  : array [0..1] of Integer;
  Mult    : Single;
begin
 if (Index < 0) or (Index >= numParams)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 with ParameterProperties[Index] do
  begin
   Result := Assigned(OnStringToParameter) or UseDefaultString2ParameterHandler;

   CurrVal := Parameter[Index];
   if UseDefaultString2ParameterHandler then
    try
     ProcStr := Trim(Text);

     Indxes[0] := Pos(Units, ProcStr);
     if Indxes[0] > 0
      then Delete(ProcStr, Indxes[0], Length(Units));

     Indxes[0] := 1;
     while (Indxes[0] <= Length(ProcStr)) and
      (not (ProcStr[Indxes[0]] in ['0'..'9', '-', '+', ',', '.'])) do Inc(Indxes[0]);

     if (Indxes[0] <= Length(ProcStr)) then
      begin
       Indxes[1] := Indxes[0] + 1;
       while (Indxes[1] <= Length(ProcStr)) and
        (ProcStr[Indxes[1]] in ['0'..'9', 'E', ',', '.']) do Inc(Indxes[1]);

       // process unit extensions
       if Pos('k', ProcStr) >= Indxes[1] then Mult := 1E3 else
       if Pos('K', ProcStr) >= Indxes[1] then Mult := 1024 else
       if Pos('G', ProcStr) >= Indxes[1] then Mult := 1048576 else
       if Pos('m', ProcStr) >= Indxes[1] then Mult := 1E-3 else
       if Pos('µ', ProcStr) >= Indxes[1] then Mult := 1E-6 else
       if Pos('c', ProcStr) >= Indxes[1] then Mult := 1E-2
        else Mult := 1;

       ProcStr := Copy(ProcStr, Indxes[0], Indxes[1] - Indxes[0]);

       CurrVal := Mult * StrToFloat(ProcStr);
      end;
    except
    end;

   if Assigned(ParameterProperties[Index].OnStringToParameter)
    then OnStringToParameter(Self, Index, Text, CurrVal);

   Parameter[Index] := CurrVal;
  end;
end;

function TVSTModuleWithPrograms.HostCallString2Parameter(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if Assigned(Ptr)
  then Result := Integer(StringToParameter(Index, StrPas(PChar(ptr))))
  else Result := 0;
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
      Assigned(ptr) and (Value >= 0) and (Value < numParams) then
    begin
     ParamStr := FloatToStrF(Opt, ffGeneral, 5, 5);
     with ParameterProperties[Value] do
      begin
       if Assigned(OnCustomParameterDisplay)
        then OnCustomParameterDisplay(Self, Value, ParamStr);
       ParamUnit := Units;
       if Assigned(OnCustomParameterLabel)
        then OnCustomParameterDisplay(Self, Value, ParamUnit);
       ParamStr := ParamStr + ParamUnit;
      end;
     ParamStr := ParamStr + #0;
     StrCopy(ptr, PChar(ParamStr));
     result := $BEEF;
    end else
   if (Index = Integer($DEADBEF0)) and Assigned(Ptr) and
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
 if (Index >= 0) and (Index < Programs.Count) and Assigned(Ptr) {and (Value = -1)} then
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
 if (Index < 0) or (Index >= ParameterProperties.Count) then
  begin
   Result := 0;
   exit;
  end;

 Result := Integer(ParameterProperties[Index].ReportVST2Properties);
 if (Result > 0) and Assigned(ptr) then
  with PVstParameterPropertyRecord(ptr)^ do
   begin
    // copy display name
    str := ParameterProperties[Index].DisplayName;
    SetLength(str, 64);
    StrCopy(Caption, @str[1]);

    // copy short label
    str := ParameterProperties[Index].ShortLabel;
    SetLength(str, 8);
    StrCopy(shortLabel, @str[1]);

    // assign flags
    Flags := ParameterProperties[Index].Flags;

    // use integer min/max
    if ppfParameterUsesIntegerMinMax in Flags then
     begin
      minInteger := ParameterProperties[Index].MinInteger;
      maxInteger := ParameterProperties[Index].MaxInteger;
     end;

    // use integer steps
    if ppfParameterUsesIntStep in Flags then
     begin
      stepInteger      := ParameterProperties[Index].StepInteger;
      largeStepInteger := ParameterProperties[Index].LargeStepInteger;
     end;

    // use float steps
    if ppfParameterUsesFloatStep in Flags then
     begin
      stepFloat        := ParameterProperties[Index].StepFloat;
      largeStepFloat   := ParameterProperties[Index].LargeStepFloat;
      smallStepFloat   := ParameterProperties[Index].SmallStepFloat;
     end;

    // assign display index
    if ppfParameterSupportsDisplayIndex in Flags
     then displayIndex := Index;

    // copy category label
    if ppfParameterSupportsDisplayCategory in Flags then
     begin
      str := ParameterProperties[Index].Category;
      SetLength(str, 24);
      StrCopy(CategoryLabel, @str[1]);
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
 Result := 0;
 if Assigned(ptr) then
  begin
   if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
    then Result := -1
    else Result :=  1;

   if Assigned(FOnBeginLoadBank)
    then FOnBeginLoadBank(Self, PVstPatchChunkInfo(ptr)^)
  end;
end;

function TVSTModuleWithPrograms.HostCallBeginLoadProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   if PVstPatchChunkInfo(ptr)^.pluginUniqueID <> FEffect.uniqueID
    then Result := -1
    else Result :=  1;

   if Assigned(FOnBeginLoadProgram)
    then FOnBeginLoadProgram(Self, PVstPatchChunkInfo(ptr)^)
  end;
end;


procedure TVSTModuleWithPrograms.SetNumParams(const Value: Integer);
begin
 if Assigned(FParameterProperties)
  then FEffect.numParams := FParameterProperties.Count
  else FEffect.numParams := 0;
end;

procedure TVSTModuleWithPrograms.SetNumPrograms(const Value: Integer);
begin
 if Assigned(FVstPrograms)
  then FEffect.numPrograms := FVstPrograms.Count
  else FEffect.numPrograms := 0;
end;

procedure TVSTModuleWithPrograms.SetProgram(const AProgramIndex: Integer);
var
  NeedProgramUpdate: Boolean;
begin
 if (numPrograms > 0) and (AProgramIndex >= 0) and
  (AProgramIndex < numPrograms) and (AProgramIndex <> FCurProgram) then
  begin
   if Assigned(FOnBeforeProgramChange)
    then FOnBeforeProgramChange(Self);
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
   do SetParameterDirect(i, Programs[FCurProgram].Parameter[i]);
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

procedure TVSTModuleWithPrograms.SetParameterString(Index: Integer;
  const Value: string);
begin

end;

function TVSTModuleWithPrograms.Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
begin
 Result := 0;
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then Result := FParameterProperties[Index].Parameter2VSTParameter(Value);
end;

function TVSTModuleWithPrograms.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 Result := 0;
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then Result := FParameterProperties[Index].VSTParameter2Parameter(Value);
end;

function TVSTModuleWithPrograms.TranslateParameterNameToIndex(ParameterName: string): Integer;
begin
 if not Assigned(FParameterProperties) or (FParameterProperties.Count = 0)
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

procedure TVSTModuleWithPrograms.SetParameter(Index: Integer; const Value: Single);
begin
 // check parameter index is valid
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
  then Exit;

 SetParameterDirect(Index, Value);

 if Assigned(FParameterProperties[Index]) then
  with FParameterProperties[Index] do
   if CanBeAutomated
    then SetParameterAutomated(Index, Parameter2VSTParameter(Value));
end;

procedure TVSTModuleWithPrograms.SetParameterDirect(const Index: Integer; Value: Single);
begin
 // check parameter index is valid
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
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
       Value := Programs[FCurProgram].Parameter[Index];

       if Assigned(ParameterProperties[Index].OnParameterChange)
        then FParameterProperties[Index].OnParameterChange(Self, Index, Value);
       if Assigned(OnParameterChange)
        then OnParameterChange(Self, Index, Value);

       Programs[FCurProgram].Parameter[Index] := Value;
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
end;

function TVSTModuleWithPrograms.GetParameter(Index: Integer): Single;
begin
 if (effFlagsProgramChunks in FEffect.EffectFlags)
  then
   begin
    assert(Assigned(FOnGetChunkParamEvent));
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
