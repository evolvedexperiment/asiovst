unit DAV_SynthUtils;

interface

uses
  Classes, RTLConsts;

type
  TBasicSynthVoice = class(TObject);

  TSampleRateSynthVoice = class(TBasicSynthVoice)
  private
    FSampleRate: Single;
    FReciprocalSampleRate: Single;
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SamplerateChanged; virtual;
    property ReciprocalSampleRate: Single read FReciprocalSampleRate;
  public
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TSynthVoice = class(TSampleRateSynthVoice)
  private
    FMidiKey: Integer;
    FVelocity: Integer;
  public
    function Process: Single; virtual;

    property MidiKey: Integer read FMidiKey write FMidiKey;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

  TVoiceList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
  protected
    function Get(Index: Integer): TSynthVoice;
    procedure Grow; virtual;
    procedure Put(Index: Integer; SimpleSamplerVoice: TSynthVoice);
    procedure Notify(SimpleSamplerVoice: TSynthVoice; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create; overload; virtual;
    constructor Create(AOwnsObjects: Boolean); overload; virtual;
    destructor Destroy; override;

    function Add(SimpleSamplerVoice: TSynthVoice): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TVoiceList;
    function Extract(SimpleSamplerVoice: TSynthVoice): TSynthVoice;
    function First: TSynthVoice;
    function IndexOf(SimpleSamplerVoice: TSynthVoice): Integer;
    procedure Insert(Index: Integer; SimpleSamplerVoice: TSynthVoice);
    function Last: TSynthVoice;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(SimpleSamplerVoice: TSynthVoice): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TVoiceList; AOperator: TListAssignOp = laCopy; ListB: TVoiceList = nil);

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TSynthVoice read Get write Put; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property List: PPointerList read FList;
  end;

function KeyToNote(const Key: Integer; var Note: Byte): Boolean;

implementation

uses
  SysUtils;

function KeyToNote(const Key: Integer; var Note: Byte): Boolean;
begin
  Result := True;
  case Key of
    89:
      Note := 60;
    83:
      Note := 61;
    88:
      Note := 62;
    68:
      Note := 63;
    67:
      Note := 64;
    86:
      Note := 65;
    71:
      Note := 66;
    66:
      Note := 67;
    72:
      Note := 68;
    78:
      Note := 69;
    74:
      Note := 70;
    77:
      Note := 71;
    188:
      Note := 72;
    81:
      Note := 72;
    87:
      Note := 74;
    69:
      Note := 76;
    82:
      Note := 77;
    else
      Result := True;
  end;
end;

{ TSampleRateSynthVoice }

procedure TSampleRateSynthVoice.SamplerateChanged;
begin
  Assert(SampleRate <> 0);
  FReciprocalSampleRate := 1 / Samplerate;
end;

procedure TSampleRateSynthVoice.SetSampleRate(const Value: Single);
begin
  if Value <= 0 then
    raise Exception.Create('Samplerate must be larger than 0!');
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    SampleRateChanged;
  end;
end;


{ TSynthVoice }

function TSynthVoice.Process: Single;
begin
  Result := 0;
end;


{ TVoiceList }

constructor TVoiceList.Create;
begin
  inherited Create;
  FOwnsObjects := True;
end;

constructor TVoiceList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

destructor TVoiceList.Destroy;
begin
  inherited Destroy;
  Clear;
end;

function TVoiceList.Add(SimpleSamplerVoice: TSynthVoice): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := SimpleSamplerVoice;
  Inc(FCount);
  if SimpleSamplerVoice <> nil then
    Notify(SimpleSamplerVoice, lnAdded);
end;

procedure TVoiceList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TVoiceList.Delete(Index: Integer);
var
  Temp: TSynthVoice;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TVoiceList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX, [EBP + 4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TVoiceList.Error(Msg: PResStringRec; Data: Integer);
begin
  TVoiceList.Error(LoadResString(Msg), Data);
end;

procedure TVoiceList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TVoiceList.Expand: TVoiceList;
begin
 if FCount = FCapacity then
   Grow;
 Result := Self;
end;

function TVoiceList.First: TSynthVoice;
begin
  Result := Get(0);
end;

function TVoiceList.Get(Index: Integer): TSynthVoice;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TVoiceList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
  if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TVoiceList.IndexOf(SimpleSamplerVoice: TSynthVoice): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> SimpleSamplerVoice) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TVoiceList.Insert(Index: Integer; SimpleSamplerVoice: TSynthVoice);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := SimpleSamplerVoice;
  Inc(FCount);
  if SimpleSamplerVoice <> nil then
    Notify(SimpleSamplerVoice, lnAdded);
end;

function TVoiceList.Last: TSynthVoice;
begin
  Result := Get(FCount - 1);
end;

procedure TVoiceList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TVoiceList.Put(Index: Integer; SimpleSamplerVoice: TSynthVoice);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if SimpleSamplerVoice <> FList^[Index] then
  begin
    Temp := FList^[Index];
    FList^[Index] := SimpleSamplerVoice;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if SimpleSamplerVoice <> nil then
      Notify(SimpleSamplerVoice, lnAdded);
  end;
end;

function TVoiceList.Remove(SimpleSamplerVoice: TSynthVoice): Integer;
begin
  Result := IndexOf(SimpleSamplerVoice);
  if Result >= 0 then
    Delete(Result);
end;

procedure TVoiceList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TVoiceList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TVoiceList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure QuickSort(SorTVoiceList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SorTVoiceList^[(L + R) shr 1];
    repeat
      while SCompare(SorTVoiceList^[I], P) < 0 do
        Inc(I);
      while SCompare(SorTVoiceList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SorTVoiceList^[I];
        SorTVoiceList^[I] := SorTVoiceList^[J];
        SorTVoiceList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SorTVoiceList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TVoiceList.Sort(Compare: TListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TVoiceList.Extract(SimpleSamplerVoice: TSynthVoice): TSynthVoice;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(SimpleSamplerVoice);
  if I >= 0 then
  begin
    Result := SimpleSamplerVoice;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TVoiceList.Notify(SimpleSamplerVoice: TSynthVoice; Action: TListNotification);
begin
  if OwnsObjects and (Action = lnDeleted) then
    SimpleSamplerVoice.Free;
end;

procedure TVoiceList.Assign(ListA: TVoiceList; AOperator: TListAssignOp; ListB: TVoiceList);
var
  I: Integer;
  LTemp, LSource: TVoiceList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346: only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34: intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456: union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256: only those not in both lists
    laXor:
      begin
        LTemp := TVoiceList.Create(False); // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125: only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6: only those unique to dest
    laDestUnique:
      begin
        LTemp := TVoiceList.Create(False);
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

end.
