unit DDspBaseComponent;

interface

uses Classes, DAVDCommon, Contnrs;

type
  TDspBaseComponent = class;
  TDspQueueList = TComponentList;
  TDspDestroyNotification = procedure(Sender: TObject; Replacement: TDspBaseComponent) of object;

  TDspBaseComponent = class(TComponent)
  protected
    fBypass: Boolean;
    fEnabled: Boolean;
    fSampleRate: Integer;
    fChannels: Integer;
    fNextDspQueueItem: TDspBaseComponent;
    fOnDestroy: TDspDestroyNotification;

    procedure SetNextDspQueueItem(const Value: TDspBaseComponent); virtual;
    procedure SetSampleRate(const Value: Integer); virtual;
    procedure SetChannels(const Value: Integer); virtual;
    procedure SampleRateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure UpdateParameters; virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; UseSampleRate: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure NextQueueItemDestroyed(Sender: TObject; Replacement: TDspBaseComponent);

    procedure Init; virtual;               // called automaticaly in constructor
    procedure Reset; virtual;              // called manualy
    procedure ResetQueue; virtual;
    function  GetQueueItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback


    function Process(channel: integer; input: Single): Single;                         overload; virtual;
    function Process(channel: integer; input: Double): Double;                         overload; virtual;
    function Process(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function Process(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function Process(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function Process(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;

    function ProcessQueue(channel: integer; input: Single): Single;                         overload; virtual;
    function ProcessQueue(channel: integer; input: Double): Double;                         overload; virtual;
    function ProcessQueue(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray; overload; virtual;
    function ProcessQueue(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray; overload; virtual;
    function ProcessQueue(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;           overload; virtual;
    function ProcessQueue(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;           overload; virtual;
    property OnDestroy: TDspDestroyNotification read fOnDestroy write fOnDestroy;
  published
    property Enabled: Boolean                    read FEnabled          write fEnabled      default true;
    property Bypass: Boolean                     read FBypass           write fBypass       default true;
    property SampleRate: Integer                 read fSampleRate       write SetSampleRate default 44100;
    property Channels: Integer                   read fChannels         write SetChannels   default 2;
    property NextDspQueueItem: TDspBaseComponent read fNextDspQueueItem write SetNextDspQueueItem;
  end;

implementation

uses Sysutils, Dialogs;

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;
  fNextDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := true;
  fSampleRate       := 44100;
  fChannels         := 2;
  Init;
end;

constructor TDspBaseComponent.Create(AOwner: TComponent; UseSampleRate: Integer);
begin
  Create(AOwner);
  SetSampleRate(UseSampleRate);
end;

destructor TDspBaseComponent.Destroy;
begin
  if assigned(fOnDestroy) then fOnDestroy(self, fNextDspQueueItem); 
  inherited;
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


function TDspBaseComponent.GetQueueItems(var items: TDspQueueList): boolean;
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
    Result:=Result and fNextDspQueueItem.GetQueueItems(items);
  end;
end;

procedure TDspBaseComponent.SetNextDspQueueItem(const Value: TDspBaseComponent);
var x: TDspQueueList; backup: TDspBaseComponent;
begin
  if (Value<>self) and (fNextDspQueueItem<>Value) then
  begin
    backup := fNextDspQueueItem;
    fNextDspQueueItem := Value;
    x:=nil;
    if not GetQueueItems(x) then
    begin
      fNextDspQueueItem := backup;
      raise Exception.Create('Processing queue loopback');
    end else
      fNextDspQueueItem.OnDestroy := NextQueueItemDestroyed;
  end;
end;

procedure TDspBaseComponent.NextQueueItemDestroyed(Sender: TObject; Replacement: TDspBaseComponent);
begin
  if fNextDspQueueItem = Sender then fNextDspQueueItem:=Replacement;
end;


procedure TDspBaseComponent.SetSampleRate(const Value: Integer);
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






function TDspBaseComponent.Process(channel: integer; input: Single): Single;
begin
  if fEnabled then
    Result:=input
  else
    Result:=0;
end;

function TDspBaseComponent.Process(channel: integer; input: Double): Double;
var tmp: single;
begin
  tmp:=input;
  if fEnabled then
    result:=Process(channel, tmp)
  else
    result:=0;
end;

function TDspBaseComponent.Process(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  if fEnabled then
  begin
    for i:=0 to length(input)-1 do
      Result[i]:=Process(channel, input[i]);
  end else
    Fillchar(Result[0], length(input) * SizeOf(Single), 0);
end;

function TDspBaseComponent.Process(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  if fEnabled then
  begin
    for i:=0 to length(input)-1 do
      Result[i]:=Process(channel, input[i]);
  end else
    Fillchar(Result[0], length(input) * SizeOf(Double), 0);
end;

function TDspBaseComponent.Process(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    if fEnabled then
      Result[i]:=Process(i, input[i])
    else
      Fillchar(Result[i,0], length(input[i]) * SizeOf(Single), 0);
end;

function TDspBaseComponent.Process(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    if fEnabled then
      Result[i]:=Process(i, input[i])
    else
      Fillchar(Result[i,0], length(input[i]) * SizeOf(Double), 0);
end;



function TDspBaseComponent.ProcessQueue(channel: integer; input: Single): Single;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(channel, input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(channel, Process(channel, input));
  end else
    Result:=Process(channel, input);
end;
  
function TDspBaseComponent.ProcessQueue(channel: integer; input: Double): Double;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(channel, input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(channel, Process(channel, input));
  end else
    Result:=Process(channel, input);
end;

function TDspBaseComponent.ProcessQueue(channel: integer; input: TAVDSingleDynArray): TAVDSingleDynArray;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(channel, input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(channel, Process(channel, input));
  end else
    Result:=Process(channel, input);
end;

function TDspBaseComponent.ProcessQueue(channel: integer; input: TAVDDoubleDynArray): TAVDDoubleDynArray;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(channel, input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(channel, Process(channel, input));
  end else
    Result:=Process(channel, input);
end;

function TDspBaseComponent.ProcessQueue(input: TArrayOfSingleDynArray): TArrayOfSingleDynArray;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(Process(input));
  end else
    Result:=Process(input);
end;

function TDspBaseComponent.ProcessQueue(input: TArrayOfDoubleDynArray): TArrayOfDoubleDynArray;
begin
  if fEnabled and assigned(fNextDspQueueItem) then
  begin
    if fBypass then
      Result:=fNextDspQueueItem.ProcessQueue(input)
    else
      Result:=fNextDspQueueItem.ProcessQueue(Process(input));
  end else
    Result:=Process(input);
end;


end.
