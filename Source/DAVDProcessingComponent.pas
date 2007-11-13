unit DAVDProcessingComponent;

interface

uses Classes, DAVDCommon;

type
  TDspBaseProcessFuncS   = procedure(var Data: Single; const channel: integer) of object;
  TDspBaseProcessFuncD   = procedure(var Data: Double; const channel: integer) of object;
  TDspBaseProcessFuncSA  = procedure(var ProcessBuffer: TAVDSingleDynArray; const channel: integer) of object;
  TDspBaseProcessFuncDA  = procedure(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer) of object;
  TDspBaseProcessFuncSAA = procedure(var ProcessBuffer: TAVDArrayOfSingleDynArray) of object;
  TDspBaseProcessFuncDAA = procedure(var ProcessBuffer: TAVDArrayOfDoubleDynArray) of object;

  TAVDProcessingComponent = class(TComponent)
  protected
    fBypass: Boolean;
    fEnabled: Boolean;
    fSampleRate: Single;
    fChannels: Integer;

    fProcessS:   TDspBaseProcessFuncS;
    fProcessD:   TDspBaseProcessFuncD;
    fProcessSA:  TDspBaseProcessFuncSA;
    fProcessDA:  TDspBaseProcessFuncDA;
    fProcessSAA: TDspBaseProcessFuncSAA;
    fProcessDAA: TDspBaseProcessFuncDAA;

    fProcessQueueS:   TDspBaseProcessFuncS;
    fProcessQueueD:   TDspBaseProcessFuncD;
    fProcessQueueSA:  TDspBaseProcessFuncSA;
    fProcessQueueDA:  TDspBaseProcessFuncDA;
    fProcessQueueSAA: TDspBaseProcessFuncSAA;
    fProcessQueueDAA: TDspBaseProcessFuncDAA;

    procedure SetBypass(const Value: Boolean); virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
    procedure SetSampleRate(const Value: Single); virtual; abstract;
    procedure SetChannels(const Value: Integer); virtual; abstract;
  public
    procedure Init; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Enabled: Boolean   read fEnabled    write SetEnabled    default true;
    property Bypass: Boolean    read fBypass     write SetBypass     default true;
    property Channels: Integer  read fChannels   write SetChannels   default 2;
    property SampleRate: Single read fSampleRate write SetSampleRate;    

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
  end;

  TAVDProcessingComponentList = class(TList)
  protected
    function Get(Index: Integer): TAVDProcessingComponent;
    procedure Put(Index: Integer; Item: TAVDProcessingComponent);
  public
    function Add(Item: TAVDProcessingComponent): Integer;
    function Extract(Item: TAVDProcessingComponent): TAVDProcessingComponent;
    function First: TAVDProcessingComponent;
    function IndexOf(Item: TAVDProcessingComponent): Integer;
    procedure Insert(Index: Integer; Item: TAVDProcessingComponent);
    function Last: TAVDProcessingComponent;
    function Remove(Item: TAVDProcessingComponent): Integer;

    procedure SetSampleRate(Value: Single);
    procedure SetChannels(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetBypass(Value: Boolean);

    property Items[Index: Integer]: TAVDProcessingComponent read Get write Put;
  end;

implementation

{ TAVDProcessingComponentList }

procedure TAVDProcessingComponentList.Insert(Index: Integer; Item: TAVDProcessingComponent);
begin
  inherited Insert(Index, Item);
end;

procedure TAVDProcessingComponentList.Put(Index: Integer; Item: TAVDProcessingComponent);
begin
   inherited Put(Index, Item);
end;

function TAVDProcessingComponentList.Add(Item: TAVDProcessingComponent): Integer;
begin
  Result := inherited Add(Item);
end;

function TAVDProcessingComponentList.Extract(Item: TAVDProcessingComponent): TAVDProcessingComponent;
begin
  Result := TAVDProcessingComponent(inherited Extract(Item));
end;

function TAVDProcessingComponentList.First: TAVDProcessingComponent;
begin
  Result := TAVDProcessingComponent(inherited First);
end;

function TAVDProcessingComponentList.Get(Index: Integer): TAVDProcessingComponent;
begin
  Result := TAVDProcessingComponent(inherited Get(Index));
end;

function TAVDProcessingComponentList.IndexOf(Item: TAVDProcessingComponent): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TAVDProcessingComponentList.Last: TAVDProcessingComponent;
begin
  Result := TAVDProcessingComponent(inherited Last);
end;

function TAVDProcessingComponentList.Remove(Item: TAVDProcessingComponent): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TAVDProcessingComponentList.SetSampleRate(Value: Single);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].SampleRate := Value;
end;

procedure TAVDProcessingComponentList.SetChannels(Value: Integer);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].Channels := Value;
end;

procedure TAVDProcessingComponentList.SetEnabled(Value: Boolean);
var i: integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].Enabled := Value;
end;

procedure TAVDProcessingComponentList.SetBypass(Value: Boolean);
var i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Bypass := Value;
end;

end.
