unit DDspBaseComponent;

interface

{$I ASIOVST.inc}

uses Classes, DAVDCommon, Contnrs, DAVDProcessingComponent;

type
  TDspQueueList = TComponentList;

  TDspBaseProcessFuncS   = function(channel: integer; input: Single): Single of object;
  TDspBaseProcessFuncD   = function(channel: integer; input: Double): Double of object;
  TDspBaseProcessFuncSA  = function(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray of object;
  TDspBaseProcessFuncDA  = function(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray of object;
  TDspBaseProcessFuncSAA = function(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray of object;
  TDspBaseProcessFuncDAA = function(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray of object;


  TDspBaseComponent = class(TAVDProcessingComponent)
  protected
    fNextDspQueueItem: TDspBaseComponent;
    fPrevDspQueueItem: TDspBaseComponent;

    fProcessS:   TDspBaseProcessFuncS;
    fProcessD:   TDspBaseProcessFuncD;
    fProcessSA:  TDspBaseProcessFuncSA;
    fProcessDA:  TDspBaseProcessFuncDA;
    fProcessSAA: TDspBaseProcessFuncSAA;
    fProcessDAA: TDspBaseProcessFuncDAA;

    fStdProcessS:   TDspBaseProcessFuncS;
    fStdProcessD:   TDspBaseProcessFuncD;
    fStdProcessSA:  TDspBaseProcessFuncSA;
    fStdProcessDA:  TDspBaseProcessFuncDA;
    fStdProcessSAA: TDspBaseProcessFuncSAA;
    fStdProcessDAA: TDspBaseProcessFuncDAA;

    fProcessQueueS:   TDspBaseProcessFuncS;
    fProcessQueueD:   TDspBaseProcessFuncD;
    fProcessQueueSA:  TDspBaseProcessFuncSA;
    fProcessQueueDA:  TDspBaseProcessFuncDA;
    fProcessQueueSAA: TDspBaseProcessFuncSAA;
    fProcessQueueDAA: TDspBaseProcessFuncDAA;

    fStdProcessQueueS:   TDspBaseProcessFuncS;
    fStdProcessQueueD:   TDspBaseProcessFuncD;
    fStdProcessQueueSA:  TDspBaseProcessFuncSA;
    fStdProcessQueueDA:  TDspBaseProcessFuncDA;
    fStdProcessQueueSAA: TDspBaseProcessFuncSAA;
    fStdProcessQueueDAA: TDspBaseProcessFuncDAA;
    
    procedure SetBypass(const Value: Boolean); override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetSampleRate(const Value: Single); override;
    procedure SetChannels(const Value: Integer); override;

    procedure SetNextDspQueueItem(const Value: TDspBaseComponent); virtual;
    procedure SampleRateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure UpdateParameters; virtual;

    procedure RegisterInOwner(item: TDspBaseComponent);
    procedure UnRegisterInOwner(item: TDspBaseComponent);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; UseSampleRate: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Init; override;               // called automaticaly in constructor
    procedure Reset; override;              // called manualy
    procedure ResetQueue; virtual;
    function  GetFollowingItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetPreviousItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetQueueItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback


    function ProcessSilence(channel: integer; input: Single): Single;                         overload; virtual;
    function ProcessSilence(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessSilence(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessSilence(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessSilence(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessSilence(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    function ProcessBypass(channel: integer; input: Single): Single;                         overload; virtual;
    function ProcessBypass(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessBypass(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessBypass(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessBypass(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessBypass(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    function ProcessBasic(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessBasic(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessBasic(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessBasic(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessBasic(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    function ProcessQueueBasic(channel: integer; input: Single): Single;                         overload; virtual;
    function ProcessQueueBasic(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessQueueBasic(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessQueueBasic(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessQueueBasic(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessQueueBasic(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    function ProcessQueueBypass(channel: integer; input: Single): Single;                         overload; virtual;
    function ProcessQueueBypass(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessQueueBypass(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessQueueBypass(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessQueueBypass(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessQueueBypass(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    property PrevDspQueueItem: TDspBaseComponent read fPrevDspQueueItem write fPrevDspQueueItem;

    property ProcessS:   TDspBaseProcessFuncS   read fProcessS;
    property ProcessD:   TDspBaseProcessFuncD   read fProcessD;
    property ProcessSA:  TDspBaseProcessFuncSA  read fProcessSA;
    property ProcessDA:  TDspBaseProcessFuncDA  read fProcessDA;
    property ProcessSAA: TDspBaseProcessFuncSAA read fProcessSAA;
    property ProcessDAA: TDspBaseProcessFuncDAA read fProcessDAA;

    property ProcessQueueS:   TDspBaseProcessFuncS   read fProcessQueueS;
    property ProcessQueueD:   TDspBaseProcessFuncD   read fProcessQueueD;
    property ProcessQueueSA:  TDspBaseProcessFuncSA  read fProcessQueueSA;
    property ProcessQueueDA:  TDspBaseProcessFuncDA  read fProcessQueueDA;
    property ProcessQueueSAA: TDspBaseProcessFuncSAA read fProcessQueueSAA;
    property ProcessQueueDAA: TDspBaseProcessFuncDAA read fProcessQueueDAA;
  published
    property Enabled: Boolean                    read fEnabled          write SetEnabled    default true;
    property Bypass: Boolean                     read fBypass           write SetBypass     default false;
    property Channels: Integer                   read fChannels         write SetChannels   default 2;
    property SampleRate: Single                  read fSampleRate       write SetSampleRate;
    property NextDspQueueItem: TDspBaseComponent read fNextDspQueueItem write SetNextDspQueueItem;
  end;

implementation

uses Sysutils, Math, DVSTModuleWithDsp;

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;
  fNextDspQueueItem := nil;
  fPrevDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := false;
  fSampleRate       := 44100;
  fChannels         := 2;

  fStdProcessS  := ProcessBypass;
  fStdProcessD  := ProcessBasic;
  fStdProcessSA := ProcessBasic;
  fStdProcessDA := ProcessBasic;
  fStdProcessSAA:= ProcessBasic;
  fStdProcessDAA:= ProcessBasic;

  fStdProcessQueueS  := ProcessQueueBasic;
  fStdProcessQueueD  := ProcessQueueBasic;
  fStdProcessQueueSA := ProcessQueueBasic;
  fStdProcessQueueDA := ProcessQueueBasic;
  fStdProcessQueueSAA:= ProcessQueueBasic;
  fStdProcessQueueDAA:= ProcessQueueBasic;

  Init;

  SetEnabled(fEnabled);
  SetBypass(fBypass);

  RegisterInOwner(self);
end;

constructor TDspBaseComponent.Create(AOwner: TComponent; UseSampleRate: Integer);
begin
  Create(AOwner);
  SetSampleRate(UseSampleRate);
end;

destructor TDspBaseComponent.Destroy;
begin
  UnRegisterInOwner(self);

  if assigned(fNextDspQueueItem) then
  begin
    fNextDspQueueItem.PrevDspQueueItem := fPrevDspQueueItem;
    if not assigned(fPrevDspQueueItem) then RegisterInOwner(fNextDspQueueItem);

  end;
  if assigned(fPrevDspQueueItem) then fPrevDspQueueItem.NextDspQueueItem := fNextDspQueueItem;
  inherited;
end;

procedure TDspBaseComponent.RegisterInOwner(item: TDspBaseComponent);
begin
  if Owner is TDspVSTModule then
    (Owner as TDspVSTModule).RegisterDSPItem(item);
end;

procedure TDspBaseComponent.UnRegisterInOwner(item: TDspBaseComponent);
begin
  if Owner is TDspVSTModule then
    (Owner as TDspVSTModule).UnRegisterDSPItem(item);
end;

procedure TDspBaseComponent.Init;  begin end;
procedure TDspBaseComponent.Reset; begin end;
procedure TDspBaseComponent.UpdateParameters; begin end;

procedure TDspBaseComponent.SampleRateChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ChannelsChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ResetQueue;
begin
  Reset;
  if assigned(fNextDspQueueItem) then fNextDspQueueItem.ResetQueue;
end;


function TDspBaseComponent.GetFollowingItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fNextDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fNextDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Add(fNextDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetFollowingItems(items);
  end;
end;

function TDspBaseComponent.GetPreviousItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fPrevDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fPrevDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Insert(0, fPrevDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetPreviousItems(items);
  end;
end;

function TDspBaseComponent.GetQueueItems(var items: TDspQueueList): boolean;
var mypos: integer;
begin
  result:=GetPreviousItems(items);
  if not result then exit;

  mypos:=items.Count;

  result:=result and GetFollowingItems(items);
  if not result then exit;
  items.Insert(mypos,self);
end;

procedure TDspBaseComponent.SetNextDspQueueItem(const Value: TDspBaseComponent);
var x: TDspQueueList; backup: TDspBaseComponent;
begin
  if (Value<>self) and (fNextDspQueueItem<>Value) then
  begin
    backup := fNextDspQueueItem;
    fNextDspQueueItem := Value;
    if Value<>nil then
    begin
      x:=nil;
      if (fPrevDspQueueItem=Value) or not (GetQueueItems(x)) then
      begin
        fNextDspQueueItem := backup;
        raise Exception.Create('Processing queue loopback');
        exit;
      end else begin
        if not assigned(fNextDspQueueItem.PrevDspQueueItem) then
        UnRegisterInOwner(fNextDspQueueItem.PrevDspQueueItem);

        fNextDspQueueItem.PrevDspQueueItem:=self;
      end;
    end;

    // Important
    SetEnabled(fEnabled);
    SetBypass(fBypass);
  end;
end;

procedure TDspBaseComponent.SetSampleRate(const Value: Single);
begin
  if (fSampleRate<>Value) and (Value>0) then
  begin
    fSampleRate := Value;
    SampleRateChanged;
    
    if assigned(fNextDspQueueItem) then fNextDspQueueItem.SampleRate:=fSampleRate;
  end;
end;

procedure TDspBaseComponent.SetChannels(const Value: Integer);
begin
  if (fChannels<>Value) and (Value>0) then
  begin
    fChannels := Value;
    ChannelsChanged;

    if assigned(fNextDspQueueItem) then fNextDspQueueItem.Channels:=fChannels;
  end;
end;

procedure TDspBaseComponent.SetBypass(const Value: Boolean);
begin
  fBypass := Value;
  if not fEnabled then exit;

  if Value then
  begin
    // bypass everything
    fProcessS  := ProcessBypass;
    fProcessD  := ProcessBypass;
    fProcessSA := ProcessBypass;
    fProcessDA := ProcessBypass;
    fProcessSAA:= ProcessBypass;
    fProcessDAA:= ProcessBypass;

    if assigned(fNextDspQueueItem) then
    begin
      // ignore this item and call directly the next item
      fProcessQueueS  := ProcessQueueBypass;
      fProcessQueueD  := ProcessQueueBypass;
      fProcessQueueSA := ProcessQueueBypass;
      fProcessQueueDA := ProcessQueueBypass;
      fProcessQueueSAA:= ProcessQueueBypass;
      fProcessQueueDAA:= ProcessQueueBypass;
    end else begin
      // bypass this item and there is no next item, so don't call it
      fProcessQueueS  := ProcessBypass;
      fProcessQueueD  := ProcessBypass;
      fProcessQueueSA := ProcessBypass;
      fProcessQueueDA := ProcessBypass;
      fProcessQueueSAA:= ProcessBypass;
      fProcessQueueDAA:= ProcessBypass;
    end;
  end else begin
    // process item
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end;
end;

procedure TDspBaseComponent.SetEnabled(const Value: Boolean);
begin
  if Value then
  begin
    // enable everything
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only Process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end else begin
    // disable everything
    fProcessS  := ProcessSilence;
    fProcessD  := ProcessSilence;
    fProcessSA := ProcessSilence;
    fProcessDA := ProcessSilence;
    fProcessSAA:= ProcessSilence;
    fProcessDAA:= ProcessSilence;

    // disable this item and don't process the next one
    fProcessQueueS  := ProcessSilence;
    fProcessQueueD  := ProcessSilence;
    fProcessQueueSA := ProcessSilence;
    fProcessQueueDA := ProcessSilence;
    fProcessQueueSAA:= ProcessSilence;
    fProcessQueueDAA:= ProcessSilence;
  end;
  fEnabled := Value;
  if fEnabled and fBypass then SetBypass(true);
end;





function TDspBaseComponent.ProcessSilence(channel: integer; input: Single): Single;
begin
  Result:=0;
end;

function TDspBaseComponent.ProcessSilence(channel: integer; input: Double): Double;
begin
  Result:=0;
end;

function TDspBaseComponent.ProcessSilence(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
begin
  setlength(Result, length(input));
  Fillchar(Result[0], length(input) * SizeOf(Single), 0);
end;

function TDspBaseComponent.ProcessSilence(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
begin
  setlength(Result, length(input));
  Fillchar(Result[0], length(input) * SizeOf(Double), 0);
end;

function TDspBaseComponent.ProcessSilence(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
  begin
    setlength(Result[i], length(input[i]));
    Fillchar(Result[i,0], length(input[i]) * SizeOf(Single), 0);
  end;
end;

function TDspBaseComponent.ProcessSilence(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
  begin
    setlength(Result[i], length(input[i]));
    Fillchar(Result[i,0], length(input[i]) * SizeOf(Double), 0);
  end;
end;





function TDspBaseComponent.ProcessBypass(channel: integer; input: Single): Single;
begin
  Result:=input
end;

function TDspBaseComponent.ProcessBypass(channel: integer; input: Double): Double;
begin
  Result:=input
end;

function TDspBaseComponent.ProcessBypass(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
begin
  Result:=Copy(input);
end;

function TDspBaseComponent.ProcessBypass(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
begin
  Result:=Copy(input);
end;

function TDspBaseComponent.ProcessBypass(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=Copy(input[i]);
end;

function TDspBaseComponent.ProcessBypass(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=Copy(input[i]);
end;







function TDspBaseComponent.ProcessBasic(channel: integer; input: Double): Double;
var tmp: single;
begin
  tmp:=input;
  result:=fProcessS(channel, tmp)
end;

function TDspBaseComponent.ProcessBasic(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=fProcessS(channel, input[i]);
end;

function TDspBaseComponent.ProcessBasic(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=fProcessD(channel, input[i]);
end;

function TDspBaseComponent.ProcessBasic(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=ProcessBasic(i, input[i])
end;

function TDspBaseComponent.ProcessBasic(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=ProcessBasic(i, input[i])
end;




function TDspBaseComponent.ProcessQueueBasic(channel: integer; input: Single): Single;
begin
  Result:=fNextDspQueueItem.ProcessQueueS(channel, fProcessS(channel, input));
end;

function TDspBaseComponent.ProcessQueueBasic(channel: integer; input: Double): Double;
begin
  Result:=fNextDspQueueItem.ProcessQueueD(channel, fProcessD(channel, input));
end;

function TDspBaseComponent.ProcessQueueBasic(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSA(channel, fProcessSA(channel, input));
end;

function TDspBaseComponent.ProcessQueueBasic(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDA(channel, fProcessDA(channel, input));
end;

function TDspBaseComponent.ProcessQueueBasic(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSAA(fProcessSAA(input));
end;

function TDspBaseComponent.ProcessQueueBasic(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDAA(fProcessDAA(input));
end;




function TDspBaseComponent.ProcessQueueBypass(channel: integer; input: Single): Single;
begin
  Result:=fNextDspQueueItem.ProcessQueueS(channel, input);
end;

function TDspBaseComponent.ProcessQueueBypass(channel: integer; input: Double): Double;
begin
  Result:=fNextDspQueueItem.ProcessQueueD(channel, input);
end;

function TDspBaseComponent.ProcessQueueBypass(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSA(channel, input);
end;

function TDspBaseComponent.ProcessQueueBypass(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDA(channel, input);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSAA(input);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDAA(input);
end;


end.
