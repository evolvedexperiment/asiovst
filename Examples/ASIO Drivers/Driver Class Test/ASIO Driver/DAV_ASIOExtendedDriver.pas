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
    IsInput: boolean;
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
    fChannelList: TList;
    fClocksAreDefault: boolean;
    fChannelsAreDefault: boolean;
    procedure ClearClockList;
    procedure ClearChannelList;
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
  end;

implementation

{ TDavASIOExtendedDriver }

uses sysutils, dialogs;

constructor TDavASIOExtendedDriver.Create(TCWrapper: TDavASIOTCWrapper);
begin
  inherited;
  fClockList:=TList.Create;
  fChannelList:=TList.Create;
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

  ClearChannelList;
  FreeAndNil(fChannelList);

  inherited;
end;

procedure TDavASIOExtendedDriver.ClearClockList;
var i:integer;
begin
  for i := fClockList.Count-1 downto 0 do TDavASIOEDClockListItem(fClockList.Items[i]).Free;
  fClockList.Clear;
end;

procedure TDavASIOExtendedDriver.ClearChannelList;
var i:integer;
begin
  for i := fChannelList.Count-1 downto 0 do TDavASIOEDChannelListItem(fChannelList.Items[i]).Free;
  fChannelList.Clear;
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
    ClearChannelList;
    fChannelsAreDefault := false;
  end;

  t := TDavASIOEDChannelListItem.Create;
  t.Name := copy(name,0,32);
  t.ChannelGroup := channelgroup;
  t.SampleType := SampleType;
  t.IsInput := IsInput;
  t.IsActive := false;
  fChannelList.Add(t);
end;

function TDavASIOExtendedDriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
begin
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


end.
