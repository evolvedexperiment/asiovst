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
    function TranslateParameterNameToIndex(ParameterName: AnsiString): Integer;
    function TranslateProgramNameToIndex(ProgramName: AnsiString): Integer;
    function GetParameterByName(ParameterName: AnsiString): Single;
    function GetParameterDisplay(Index: Integer): AnsiString;
    function GetParameterLabel(Index: Integer): AnsiString;
    function GetParameterName(Index: Integer): AnsiString;
    function GetParameterString(Index: Integer): AnsiString;
    function GetVSTParameter(Index: Integer): Single;
    function GetVstProgramByName(ProgramName: AnsiString): TVstProgram;
    procedure SetParameterByName(ParameterName: AnsiString; const Value: Single);
    procedure SetVstProgramByName(ProgramName: AnsiString; const Value: TVstProgram);
    procedure SetParameterProperties(const Value: TCustomVstParameterProperties);
    procedure SetParameterCategories(const Value: TCustomVstParameterCategories);
    procedure SetParameterString(Index: Integer; const Value: AnsiString);
    procedure SetVSTParameter(Index: Integer; const Value: Single);
    procedure SetVstPrograms(const Value: TCustomVstPrograms);
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
    function  GetCurrentProgramName: AnsiString; virtual;
    function  GetParameter(Index: Integer): Single; virtual;
    function  Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
    function  VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
    function  HostCallVendorSpecific(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    procedure CurrentProgramChanged; virtual;
    procedure SetCurrentProgramName(AName: AnsiString); virtual;
    procedure SetNumParams(const Value: Integer); virtual;
    procedure SetNumPrograms(const Value: Integer); virtual;
    procedure SetParameterDirect(const Index: Integer; Value: Single); virtual;
    procedure SetParameter(Index: Integer; const Value: Single); virtual;
    procedure SetProgram(const AProgramIndex: Integer); virtual;
    procedure Loaded; override;

    function  HostCallGetParameter(const Index: Integer): Single; override;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); override;

    function HostCallEditOpen                (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetProgram              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetProgram              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetProgramName          (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetProgramName          (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetParamLabel           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetParamDisplay         (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetParamName            (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetChunk                (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetChunk                (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallCanBeAutomated          (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallString2Parameter        (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetNumProgramCategories (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetProgramNameIndexed   (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetParameterProperties  (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallBeginSetProgram         (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEndSetProgram           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallBeginLoadBank           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallBeginLoadProgram        (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetProgramParameters(const ProgramIndex: Integer; Parameters: TDAVSingleDynArray); virtual;
    procedure SetParameterCount(const Value: Integer);
    function StringToParameter(const Index: Integer; Text: AnsiString): Boolean;

    property numParams: Integer read FEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read FEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read FCurProgram write SetProgram default 0;
    property CurrentProgramName: AnsiString read GetCurrentProgramName write SetCurrentProgramName;
    property Chunk: TMemoryStream read FChunkData;
    property Programs: TVstPrograms read FVstPrograms write SetVstPrograms;
    property ProgramByName[ProgramName: AnsiString]: TVstProgram read GetVstProgramByName write SetVstProgramByName;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
    property ParameterCategories: TCustomVstParameterCategories read FParameterCategories write SetParameterCategories;
    property Parameter[Index: Integer]: Single read GetParameter write SetParameter;
    property ParameterString[Index: Integer]: AnsiString read GetParameterString write SetParameterString;
    property ParameterByName[ParameterName: AnsiString]: Single read GetParameterByName write SetParameterByName;

    property ParameterName[Index: Integer]: AnsiString read GetParameterName;
    property ParameterLabel[Index: Integer]: AnsiString read GetParameterLabel;
    property ParameterDisplay[Index: Integer]: AnsiString read GetParameterDisplay;
    property VSTParameter[Index: Integer]: Single read GetVSTParameter write SetVSTParameter;

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
  {$IFDEF DELPHI14_UP}AnsiStrings, {$ENDIF} SysUtils, Math,
  DAV_VSTCustomModule, DAV_VSTBasicModule;

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
  ProgramIndex : Integer;
begin
 for ProgramIndex := 0 to numPrograms - 1 do
  if Assigned(Programs[ProgramIndex].OnInitialize)
   then Programs[ProgramIndex].OnInitialize(Programs[ProgramIndex]);

 if numPrograms < 0
  then FCurProgram := -1
  else CurrentProgram := 0;

 inherited;
end;
{$ENDIF}

function TVSTModuleWithPrograms.GetParameterDisplay(Index: Integer): AnsiString;
begin
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
  then Result := AnsiString(RStrUndefined)
  else
   begin
    if (effFlagsProgramChunks in FEffect.EffectFlags)
     then Result := AnsiString(FloatToStr(FOnGetChunkParamEvent(Self, Index)))
     else if (numPrograms > 0)
      then Result := AnsiString(FloatToStrF(Programs[FCurProgram].Parameter[Index], ffGeneral, 4, 4))
      else Result := AnsiString(FloatToStrF(FParameter[Index], ffGeneral, 4, 4));

    with FParameterProperties[Index] do
     if Assigned(OnCustomParameterDisplay)
      then OnCustomParameterDisplay(Self, Index, Result);
   end;

 if FTruncateStrings and (Length(Result) > 8)
  then SetLength(Result, 8);
end;

function TVSTModuleWithPrograms.GetParameterLabel(Index: Integer): AnsiString;
begin
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
  then Result := AnsiString(RStrUndefined)
  else
   begin
    Result := FParameterProperties[Index].Units;
    if Assigned(FParameterProperties[Index].OnCustomParameterLabel)
     then FParameterProperties[Index].OnCustomParameterLabel(Self, Index, Result);
   end;
 if FTruncateStrings and (Length(Result) > 8)
  then SetLength(Result, 8);
end;

function TVSTModuleWithPrograms.GetParameterName(Index: Integer): AnsiString;
begin
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then Result := AnsiString(FParameterProperties[Index].DisplayName)
  else Result := AnsiString(RStrUndefined);

 if FTruncateStrings and (Length(Result) > 8)
  then SetLength(Result, 8);
end;

function TVSTModuleWithPrograms.GetParameterString(Index: Integer): AnsiString;
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
 {$IFDEF DebugLog}
 AddLogMessage('HostCallSetParameter: Index = ' + IntToStr(Index) +
   ' Value = ' + FloatToStr(Value));
 {$ENDIF}

 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)) then
  if Assigned(FOnParameterSizeFailed)
   then FOnParameterSizeFailed(Self) else
  else SetParameterDirect(Index, VSTParameter2Parameter(Value, Index));
end;

function TVSTModuleWithPrograms.HostCallSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if (Value >= 0) or (Value < numPrograms)
  then CurrentProgram := Value;
 Result := 0;
end;

function TVSTModuleWithPrograms.HostCallGetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := FCurProgram;
end;

function TVSTModuleWithPrograms.HostCallSetProgramName(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := 0;
 if Assigned(ptr) then
  begin
   if numPrograms > 0
    then Programs[FCurProgram].DisplayName := string(StrPas(PAnsiChar(ptr)));
  end;
end;

function TVSTModuleWithPrograms.HostCallGetProgramName(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  Str : AnsiString;
begin
 Result := 0;

 if Assigned(Ptr) then
  begin
   if numPrograms > 0
    then Str := AnsiString(Programs[FCurProgram].DisplayName)
    else Str := '';
   if FTruncateStrings and (Length(Str) > 24)
    then SetLength(Str, 24);
   StrPCopy(Ptr, Str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamLabel(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  Str : AnsiString;
begin
 Result := 0;
 if Assigned(Ptr) then
  begin
   Str := ParameterLabel[Index];
   if FTruncateStrings and (Length(Str) > 8)
    then SetLength(Str, 8);
   StrPCopy(Ptr, Str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamDisplay(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  Str : AnsiString;
begin
 Result := 0;
 if Assigned(Ptr) then
  begin
   Str := ParameterDisplay[Index];
   if FTruncateStrings and (Length(Str) > 8)
    then SetLength(Str, 8);
   StrPCopy(Ptr, Str);
  end;
end;

function TVSTModuleWithPrograms.HostCallGetParamName(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  Str : AnsiString;
begin
 Result := 0;
 if Assigned(Ptr) then
  begin
   Str := ParameterName[Index];
   if FTruncateStrings and (Length(Str) > 8)
    then SetLength(Str, 8);
   StrPCopy(Ptr, Str);
  end;
end;

function TVSTModuleWithPrograms.HostCallEditOpen(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  i, pr : Integer;
  tmp   : Single;
begin
 Result := inherited HostCallEditOpen(Index, Value, ptr, opt);

 if (effFlagsHasEditor in FEffect.EffectFlags) and Assigned(FEditorForm) then
  begin
   pr := Min(numParams, FParameterProperties.Count);
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

function TVSTModuleWithPrograms.HostCallGetChunk(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
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

    Pointer(ptr^) := Chunk.Memory;
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
    Pointer(ptr^) := tmps.Memory;
    Result := tmps.Size;
   end;
end;

function TVSTModuleWithPrograms.HostCallSetChunk(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
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
      pi := PInteger(pb);
      Inc(pb, 4);
      Programs[i].Chunk.Write(pb^, pi^);
      Programs[i].Chunk.Position := 0;
      Inc(pb, pi^);

      if Assigned(Programs[i].OnLoadChunk)
       then Programs[i].OnLoadChunk(Programs[i], i, False);
     end;
    Result := Value;
    if Assigned(Programs[CurrentProgram].OnLoadChunk)
     then Programs[CurrentProgram].OnLoadChunk(Programs[CurrentProgram], CurrentProgram, False);
  end;
 FEditorNeedUpdate := True;
end;

function TVSTModuleWithPrograms.HostCallCanBeAutomated(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if Index < ParameterProperties.Count
  then Result := Integer(ParameterProperties[Index].CanBeAutomated)
  else Result := 1;
end;

function TVSTModuleWithPrograms.StringToParameter(const Index: Integer; Text: AnsiString): Boolean;
var
  ProcStr : AnsiString;
  CurrVal : Single;
  Indxes  : array [0..1] of Integer;
  Mult    : Single;
begin
 {$IFDEF DebugLog}
 AddLogMessage('StringToParameter');
 {$ENDIF}

 if (Index < 0) or (Index >= numParams)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 with ParameterProperties[Index] do
  begin
   Result := Assigned(OnStringToParameter) or UseDefaultString2ParameterHandler;

   CurrVal := Parameter[Index];
   if UseDefaultString2ParameterHandler then
    try
     {$IFDEF DELPHI14_UP}
     ProcStr := AnsiStrings.Trim(Text);
     Indxes[0] := AnsiStrings.AnsiPos(Units, ProcStr);
     {$ELSE}
     ProcStr := Trim(Text);
     Indxes[0] := Pos(Units, ProcStr);
     {$ENDIF}

     if Indxes[0] > 0
      then Delete(ProcStr, Indxes[0], Length(Units));

     Indxes[0] := 1;
     {$IFDEF DELPHI2009_UP}
     while (Indxes[0] <= Length(ProcStr)) and
      (not (CharInSet(ProcStr[Indxes[0]], ['0'..'9', '-', '+', ',', '.']))) do Inc(Indxes[0]);
     {$ELSE}
     while (Indxes[0] <= Length(ProcStr)) and
      (not (ProcStr[Indxes[0]] in ['0'..'9', '-', '+', ',', '.'])) do Inc(Indxes[0]);
     {$ENDIF}

     if (Indxes[0] <= Length(ProcStr)) then
      begin
       Indxes[1] := Indxes[0] + 1;
       {$IFDEF DELPHI2009_UP}
       while (Indxes[1] <= Length(ProcStr)) and
        (CharInSet(ProcStr[Indxes[1]], ['0'..'9', 'E', ',', '.'])) do Inc(Indxes[1]);
       {$ELSE}
       while (Indxes[1] <= Length(ProcStr)) and
        (ProcStr[Indxes[1]] in ['0'..'9', 'E', ',', '.']) do Inc(Indxes[1]);
       {$ENDIF}

       // process unit extensions
       {$IFDEF DELPHI14_UP}
       if AnsiStrings.AnsiPos('k', ProcStr) >= Indxes[1] then Mult := 1E3 else
       if AnsiStrings.AnsiPos('K', ProcStr) >= Indxes[1] then Mult := 1024 else
       if AnsiStrings.AnsiPos('G', ProcStr) >= Indxes[1] then Mult := 1048576 else
       if AnsiStrings.AnsiPos('m', ProcStr) >= Indxes[1] then Mult := 1E-3 else
       if AnsiStrings.AnsiPos('µ', ProcStr) >= Indxes[1] then Mult := 1E-6 else
       if AnsiStrings.AnsiPos('c', ProcStr) >= Indxes[1] then Mult := 1E-2
        else Mult := 1;
       {$ELSE}
       if Pos('k', ProcStr) >= Indxes[1] then Mult := 1E3 else
       if Pos('K', ProcStr) >= Indxes[1] then Mult := 1024 else
       if Pos('G', ProcStr) >= Indxes[1] then Mult := 1048576 else
       if Pos('m', ProcStr) >= Indxes[1] then Mult := 1E-3 else
       if Pos('µ', ProcStr) >= Indxes[1] then Mult := 1E-6 else
       if Pos('c', ProcStr) >= Indxes[1] then Mult := 1E-2
        else Mult := 1;
       {$ENDIF}

       ProcStr := Copy(ProcStr, Indxes[0], Indxes[1] - Indxes[0]);

       CurrVal := Mult * StrToFloat(string(ProcStr));
      end;
    except
    end;

   if Assigned(ParameterProperties[Index].OnStringToParameter)
    then OnStringToParameter(Self, Index, Text, CurrVal);

   Parameter[Index] := CurrVal;
  end;
end;

function TVSTModuleWithPrograms.HostCallString2Parameter(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if Assigned(Ptr)
  then Result := Integer(StringToParameter(Index, StrPas(PAnsiChar(ptr))))
  else Result := 0;
end;

function TVSTModuleWithPrograms.HostCallVendorSpecific(const Index: Integer; const Value: TVstIntPtr;
  const ptr: pointer; const opt: Single): TVstIntPtr;
var
  ParamStr  : AnsiString;
  ParamUnit : AnsiString;
begin
 Result := inherited HostCallVendorSpecific(Index, Value, ptr, opt);
 if (vcdCockosExtension in CanDos) then
  begin
   if (Index = Integer(effGetParamDisplay)) and
      Assigned(ptr) and (Value >= 0) and (Value < numParams) then
    begin
     ParamStr := AnsiString(FloatToStrF(Opt, ffGeneral, 5, 5));
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
     StrPCopy(ptr, ParamStr);
     Result := $BEEF;
    end else
   if (Index = Integer($DEADBEF0)) and Assigned(Ptr) and
      (Value >= 0) and (Value < numParams) then
    begin
     PDAV2SingleArray(Ptr)^[0] := 0;
     PDAV2SingleArray(Ptr)^[1] := 1;
     Result := $BEEF;
    end;
  end;
end;

procedure TVSTModuleWithPrograms.Loaded;
begin
 inherited;
 FParameterCategories.CheckParametersInUse;
end;

function TVSTModuleWithPrograms.HostCallGetNumProgramCategories(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := FNumCategories;
end;

function TVSTModuleWithPrograms.HostCallGetProgramNameIndexed(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  str : AnsiString;
begin
 Result := 0;
 if (Index >= 0) and (Index < Programs.Count) and Assigned(Ptr) {and (Value = -1)} then
  begin
   str := AnsiString(Programs[Index].DisplayName);
   if FTruncateStrings and (Length(str) > 24)
    then SetLength(str, 24);
   StrPCopy(ptr, str);
   Result := 1;
  end;
end;


function TVSTModuleWithPrograms.HostCallGetParameterProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  str : AnsiString;
begin
 Result := 0;
 if (Index < 0) or (Index >= ParameterProperties.Count)
  then Exit;

 if ParameterProperties[Index].ReportVST2Properties
  then Result := 1;

 if (Result > 0) and Assigned(ptr) then
  with PVstParameterPropertyRecord(ptr)^ do
   begin
    // copy display name
    Str := AnsiString(ParameterProperties[Index].DisplayName) + #0;
    FillChar(ParamLabel, SizeOf(ParamLabel), 0);
    if Length(Str) > 64 then SetLength(Str, 64);
    StrPCopy(@ParamLabel[0], str);

    // copy short label
    Str := ParameterProperties[Index].ShortLabel + #0;
    FillChar(shortLabel, SizeOf(shortLabel), 0);
    if Length(Str) > 8 then SetLength(str, 8);
    StrPCopy(@shortLabel[0], str);

    // assign flags
    Flags := ParameterProperties[Index].Flags;

    // clear future
    FillChar(Future, SizeOf(Future), 0);

    // use integer min/max
    if ppfParameterUsesIntegerMinMax in Flags then
     begin
      minInteger := ParameterProperties[Index].MinInteger;
      maxInteger := ParameterProperties[Index].MaxInteger;
     end
    else
     begin
      minInteger := 0;
      maxInteger := 0;
     end;

    // use integer steps
    if ppfParameterUsesIntStep in Flags then
     begin
      stepInteger      := ParameterProperties[Index].StepInteger;
      largeStepInteger := ParameterProperties[Index].LargeStepInteger;
     end
    else
     begin
      stepInteger := 1;
      largeStepInteger := 2;
     end;

    // use float steps
    if ppfParameterUsesFloatStep in Flags then
     begin
      stepFloat      := Parameter2VSTParameter(ParameterProperties[Index].StepFloat, Index);
      largeStepFloat := Parameter2VSTParameter(ParameterProperties[Index].LargeStepFloat, Index);
      smallStepFloat := Parameter2VSTParameter(ParameterProperties[Index].SmallStepFloat, Index);
     end
    else
     begin
      stepFloat      := 0.01;
      largeStepFloat := 0.05;
      smallStepFloat := 0.002;
     end;

    // assign display index
    if ppfParameterSupportsDisplayIndex in Flags
     then DisplayIndex := Index
     else DisplayIndex := 0;

    // copy category label
    if ppfParameterSupportsDisplayCategory in Flags then
     begin
      str := ParameterProperties[Index].Category + #0;
      if Length(Str) > 24 then SetLength(Str, 24);
      FillChar(CategoryLabel, SizeOf(CategoryLabel), 0);
      StrPCopy(@CategoryLabel[0], str);
      Category := ParameterProperties[Index].CategoryIndex;
      if (Category > 0) and (Category <= ParameterCategories.Count)
       then numParametersInCategory := ParameterCategories[Category - 1].ParametersInCategory
       else numParametersInCategory := 0;
     end;
   end;
end;

function TVSTModuleWithPrograms.HostCallBeginSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if Assigned(FOnBeginSetProgram) then
  begin
   FOnBeginSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;

function TVSTModuleWithPrograms.HostCallEndSetProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if Assigned(FOnEndSetProgram) then
  begin
   FOnEndSetProgram(Self);
   Result := 1;
  end
 else Result := 0;
end;


function TVSTModuleWithPrograms.HostCallBeginLoadBank(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
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

function TVSTModuleWithPrograms.HostCallBeginLoadProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
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
 {$IFDEF DebugLog}
 AddLogMessage('SetProgramParameters');
 {$ENDIF}

 if Length(Parameters) > numParams
  then raise Exception.CreateFmt(RCStrParameterMismatch, [Length(Parameters)]);
 with Programs[ProgramIndex] do
  for i := 0 to Length(Parameters) - 1
   do Programs[ProgramIndex].Parameter[i] := Parameters[i];
end;

procedure TVSTModuleWithPrograms.SetCurrentProgramName(AName: AnsiString);
begin
 if (FCurProgram < numPrograms) and (numPrograms > 0) then
  begin
   Programs[FCurProgram].DisplayName := string(AName);
   FEditorNeedUpdate := True;
  end;
 updateDisplay;
end;

function TVSTModuleWithPrograms.GetCurrentProgramName: AnsiString;
begin
 if (FCurProgram < numPrograms) and (numPrograms > 0) and (FCurProgram >= 0)
  then Result := AnsiString(Programs[FCurProgram].DisplayName)
  else Result := '';
end;

procedure TVSTModuleWithPrograms.SetParameterCategories(
  const Value: TCustomVstParameterCategories);
begin
 FParameterCategories.Assign(Value);
end;

procedure TVSTModuleWithPrograms.SetParameterCount(const Value: Integer);
begin
 {$IFDEF DebugLog}
 AddLogMessage('SetParameterCount');
 {$ENDIF}
 SetLength(FParameter, Value);
end;

procedure TVSTModuleWithPrograms.SetVstProgramByName(ProgramName: AnsiString;
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
  const Value: AnsiString);
begin
end;

function TVSTModuleWithPrograms.Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
begin
 {$IFDEF DebugLog}
 AddLogMessage('Parameter2VSTParameter: Index = ' + IntToStr(Index) +
   ' Value = ' + FloatToStr(Value));
 {$ENDIF}

 Result := 0;
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count) then
  with FParameterProperties[Index] do
   begin
    {$IFDEF DebugLog}
    case Curve of
     ctLinear         : AddLogMessage('Parameter2VSTParameter: Linear');
     ctLogarithmic    : AddLogMessage('Parameter2VSTParameter: Logarithmic');
     ctExponential    : AddLogMessage('Parameter2VSTParameter: Exponential');
     ctFrequencyScale : AddLogMessage('Parameter2VSTParameter: FrequencyScale');
     else AddLogMessage('Parameter2VSTParameter: Other');
    end;

    if Curve in [ctLogarithmic, ctExponential]
     then AddLogMessage('Parameter2VSTParameter: CurveFactor = ' + FloatToStr(CurveFactor));

    AddLogMessage('Parameter2VSTParameter: Min = ' + FloatToStr(Min) +
      ' Max = ' + FloatToStr(Max));
    {$ENDIF}

    Result := Parameter2VSTParameter(Value);
   end;

 {$IFDEF DebugLog}
 AddLogMessage('Parameter2VSTParameter: Result = ' + FloatToStr(Result));
 {$ENDIF}
end;

function TVSTModuleWithPrograms.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 {$IFDEF DebugLog}
 AddLogMessage('VSTParameter2Parameter: Index = ' + IntToStr(Index) +
   ' Value = ' + FloatToStr(Value));
 {$ENDIF}

 Result := 0;
 if Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count)
  then Result := FParameterProperties[Index].VSTParameter2Parameter(Value);

 {$IFDEF DebugLog}
 AddLogMessage('VSTParameter2Parameter: Result = ' + FloatToStr(Result));
 {$ENDIF}
end;

function TVSTModuleWithPrograms.TranslateParameterNameToIndex(ParameterName: AnsiString): Integer;
begin
 {$IFDEF DebugLog}
 AddLogMessage('TranslateParameterNameToIndex');
 {$ENDIF}

 if not Assigned(FParameterProperties) or (FParameterProperties.Count = 0)
  then raise Exception.Create(RStrNoParameterAvailable);
 Result := 0;
 while Result < FParameterProperties.Count do
  if string(ParameterName) = FParameterProperties[Result].DisplayName
   then Break
   else Inc(Result);
 if Result = FParameterProperties.Count
  then raise Exception.Create(RStrUnknownParameterName + ': ' + string(ParameterName));
end;

function TVSTModuleWithPrograms.TranslateProgramNameToIndex(ProgramName: AnsiString): Integer;
begin
 {$IFDEF DebugLog}
 AddLogMessage('TranslateProgramNameToIndex');
 {$ENDIF}

 if FVstPrograms.Count = 0
  then raise Exception.Create(RStrNoProgramAvailable);
 Result := 0;
 while Result < FVstPrograms.Count do
  if string(ProgramName) = FVstPrograms[Result].DisplayName
   then Break
   else Inc(Result);
 if Result = FVstPrograms.Count
  then raise Exception.Create(RStrUnknownProgramName);
end;

procedure TVSTModuleWithPrograms.SetParameterByName(ParameterName: AnsiString; const Value: Single);
begin
 {$IFDEF DebugLog}
 AddLogMessage('SetParameterByName');
 {$ENDIF}

 Parameter[TranslateParameterNameToIndex(ParameterName)] := Value;
end;

procedure TVSTModuleWithPrograms.SetVSTParameter(Index: Integer; const Value: Single);
begin
 {$IFDEF DebugLog}
 AddLogMessage('SetVSTParameter');
 {$ENDIF}

 // check parameter index is valid
 if not (Assigned(FParameterProperties) and (Index >= 0) and (Index < FParameterProperties.Count))
  then Exit;

 SetParameterDirect(Index, VSTParameter2Parameter(Value, Index));

 if Assigned(FParameterProperties[Index]) then
  with FParameterProperties[Index] do
   if CanBeAutomated
    then SetParameterAutomated(Index, Value);
end;

procedure TVSTModuleWithPrograms.SetParameter(Index: Integer; const Value: Single);
begin
 {$IFDEF DebugLog}
 AddLogMessage('SetParameter');
 {$ENDIF}

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
 {$IFDEF DebugLog}
 AddLogMessage('SetParameterDirect: Index = ' + IntToStr(Index) +
   '; Value = ' + FloatToStr(Value));
 {$ENDIF}

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

 {$IFDEF DebugLog}
 AddLogMessage('SetParameterDirect - done');
 {$ENDIF}

 FEditorNeedUpdate := True;
end;

function TVSTModuleWithPrograms.GetParameter(Index: Integer): Single;
begin
 if (effFlagsProgramChunks in FEffect.EffectFlags)
  then
   begin
    Assert(Assigned(FOnGetChunkParamEvent));
    Result := FOnGetChunkParamEvent(Self, Index)
   end
  else
   if numPrograms > 0
    then Result := Programs[FCurProgram].Parameter[Index]
    else Result := FParameter[Index];
end;

function TVSTModuleWithPrograms.GetParameterByName(ParameterName: AnsiString): Single;
begin
 Result := Parameter[TranslateParameterNameToIndex(ParameterName)];
end;

function TVSTModuleWithPrograms.GetVSTParameter(Index: Integer): Single;
begin
 Result := Parameter2VSTParameter(GetParameter(Index), Index);
end;

function TVSTModuleWithPrograms.GetVstProgramByName(ProgramName: AnsiString): TVstProgram;
begin
 Result := Programs[TranslateProgramNameToIndex(ProgramName)];
end;

end.
