unit DAV_ASIOExtendedDriver;

interface

uses Classes, DAV_ASIO, DAV_ASIODriver;

type
  TDavASIOEDClockListItem = class
    Name: string;
    ChannelGroup: LongInt;
    IsCurrentSource: boolean;
  end;

  TDavASIOEDChannelListItem = class
    IsActive: boolean;
    ChannelGroup: LongInt;
    SampleType: TASIOSampleType;
    Name: string;
  end;

  TDavASIOExtendedDriver = class(TDavASIODriver)
  private
    fDriverName: string;
    fDriverVersion: LongInt;
    fClockList: TList;
    fInChannelList: TList;
    fOutChannelList: TList;
    fClocksAreDefault: boolean;
    fChannelsAreDefault: boolean;
    procedure ClearClockList;
    procedure ClearChannelLists;
  protected
    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
    procedure AddClock(name: string; channelgroup: LongInt);
    procedure AddChannel(name: string; channelgroup: LongInt; SampleType: TASIOSampleType; IsInput: Boolean);
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper); override;
    destructor Destroy; override;

    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; override;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
  end;

implementation

{ TDavASIOExtendedDriver }

uses sysutils, dialogs;

constructor TDavASIOExtendedDriver.Create(TCWrapper: TDavASIOTCWrapper);
begin
  inherited;
  fClockList:=TList.Create;
  fInChannelList:=TList.Create;
  fOutChannelList:=TList.Create;
  fDriverName := 'DAV Abstract Ext';
  fDriverVersion := 1;

  fClocksAreDefault:=false;
  fChannelsAreDefault:=false;

  AddClock('Default Clock', 0);
  AddChannel('Default Input', 0, ASIOSTFloat32LSB, true);
  AddChannel('Default Output', 0, ASIOSTFloat32LSB, false);

  fClocksAreDefault:=true;
  fChannelsAreDefault:=true;

  InitializeDriverParams;
end;

destructor TDavASIOExtendedDriver.destroy;
begin
  ClearClockList;
  FreeAndNil(fClockList);

  ClearChannelLists;
  FreeAndNil(fInChannelList);
  FreeAndNil(fOutChannelList);

  inherited;
end;

procedure TDavASIOExtendedDriver.ClearClockList;
var i:integer;
begin
  for i := fClockList.Count-1 downto 0 do TDavASIOEDClockListItem(fClockList.Items[i]).Free;
  fClockList.Clear;
end;

procedure TDavASIOExtendedDriver.ClearChannelLists;
var i:integer;
begin
  for i := fInChannelList.Count-1 downto 0 do TDavASIOEDChannelListItem(fInChannelList.Items[i]).Free;
  for i := fOutChannelList.Count-1 downto 0 do TDavASIOEDChannelListItem(fOutChannelList.Items[i]).Free;
  fInChannelList.Clear;
  fOutChannelList.Clear;
end;

procedure TDavASIOExtendedDriver.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOExtendedDriver.SetDriverName(name: string);
begin
  fDriverName := name;
end;

procedure TDavASIOExtendedDriver.SetDriverVersion(version: LongInt);
begin
  fDriverVersion := version;
end;

procedure TDavASIOExtendedDriver.AddClock(name: string; channelgroup: Integer);
var t: TDavASIOEDClockListItem;
begin
  if fClocksAreDefault then
  begin
    ClearClockList;
    fClocksAreDefault := false;
  end;
  
  t := TDavASIOEDClockListItem.Create;
  t.Name := copy(name,0,32);
  t.ChannelGroup := channelgroup;
  t.IsCurrentSource := false;
  fClockList.Add(t);
end;

procedure TDavASIOExtendedDriver.AddChannel(name: string; channelgroup: Integer; SampleType: TASIOSampleType; IsInput: Boolean);
var t: TDavASIOEDChannelListItem;
begin
  if fChannelsAreDefault then
  begin
    ClearChannelLists;
    fChannelsAreDefault := false;
  end;

  t := TDavASIOEDChannelListItem.Create;
  t.Name := copy(name,0,32);
  t.ChannelGroup := channelgroup;
  t.SampleType := SampleType;
  t.IsActive := false;

  if IsInput then
    fInChannelList.Add(t)
  else
    fOutChannelList.Add(t);
end;

function TDavASIOExtendedDriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
begin
 // still a todo part
  showmessage(inttostr(NumSources));
  with Clocks^ do
  begin
    Index := 0;
    AssociatedChannel := -1;
    AssociatedGroup := -1;
    IsCurrentSource := ASIOTrue;
    StrCopy(Name, 'Internal');
  end;
  NumSources := 1;
  result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
  NumInputChannels:=fInChannelList.Count;
  NumOutputChannels:=fOutChannelList.Count;

  if NumInputChannels+NumOutputChannels<1 then
    result := ASE_NotPresent
  else
    result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
var querylist: TList;
begin
  if Info.IsInput=ASIOTrue then
    querylist := fInChannelList
  else
    querylist := fOutChannelList;

  if Info.Channel>=querylist.Count then
    result := ASE_InvalidParameter
  else with TDavASIOEDChannelListItem(querylist.Items[Info.Channel]) do begin
    Info.SampleType := SampleType;
    Info.ChannelGroup := ChannelGroup;
    Info.IsActive := TASIOBool(IsActive);
    StrPCopy(Info.Name, Pchar(Name));
    result := ASE_OK;
  end;
end;

end.
