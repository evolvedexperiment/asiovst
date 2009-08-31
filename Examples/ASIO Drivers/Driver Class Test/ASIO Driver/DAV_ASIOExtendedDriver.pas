unit DAV_ASIOExtendedDriver;

interface

uses Classes, windows, DAV_ASIO, DAV_ASIODriver;

type
  TDavASIOEDClockListItem = class
    ClockName: string;
    ChannelGroup: LongInt;
    IsCurrentSource: boolean;
  end;

  TDavASIOEDChannelListItem = class
    IsActive: boolean;
    ChannelGroup: LongInt;
    SampleType: TASIOSampleType;
    ChannelName: string;
    DoubleBuffer: array [0..1] of Pointer;
    constructor Create(cname: string; cchannelgroup: LongInt; cSampleType: TASIOSampleType; cIsInput: Boolean);
    destructor Destroy; override;
    procedure CreateBuffer({todo});
    procedure DestroyBuffer;
  end;

  TDavASIOExtendedDriver = class(TDavASIODriver)
  private
    fHostHandle: HWND;
    fLastErrorMsg: string;
    fDriverName: string;
    fDriverVersion: LongInt;
    fClockList: TList;
    fInChannelList: TList;
    fOutChannelList: TList;
    fClocksAreDefault: boolean;
    fChannelsAreDefault: boolean;
    procedure ClearClockList;
    procedure ClearChannelLists;
    function GetFirstGroupChannel(GroupNr: Longint; IsInput: Boolean): Longint;
  protected
    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
    procedure SetErrorMessage(s: string);
    procedure AddClock(name: string; channelgroup: LongInt);
    procedure AddChannel(name: string; channelgroup: LongInt; SampleType: TASIOSampleType; IsInput: Boolean);
    function GetCurrentClockSource: Integer;   
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper); override;
    destructor Destroy; override;

    function Init(SysHandle: HWND): boolean; override;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;

    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function GetErrorMessage: string; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; override;
    function DisposeBuffers: TASIOError; override;
  end;

implementation

{ TDavASIOExtendedDriver }

uses sysutils, math, {for debug:}dialogs;



{ TDavASIOEDChannelListItem }

constructor TDavASIOEDChannelListItem.Create(cname: string; cchannelgroup: Integer; cSampleType: TASIOSampleType; cIsInput: Boolean);
begin
  ChannelName := copy(cname,0,32);
  ChannelGroup := cchannelgroup;
  SampleType := cSampleType;
  IsActive := false;
  DoubleBuffer[0] := nil;
  DoubleBuffer[1] := nil;
end;

destructor TDavASIOEDChannelListItem.Destroy;
begin
  DestroyBuffer;
  inherited;
end;

procedure TDavASIOEDChannelListItem.CreateBuffer({todo});
begin
  if IsActive then DestroyBuffer;

  // TODO

  // on success:
  IsActive := true;
end;


procedure TDavASIOEDChannelListItem.DestroyBuffer;
begin
  if not IsActive then exit;
  IsActive := false;

  FreeMem(DoubleBuffer[0]);
  FreeMem(DoubleBuffer[1]);
end;

constructor TDavASIOExtendedDriver.Create(TCWrapper: TDavASIOTCWrapper);
begin
  inherited;
  fClockList:=TList.Create;
  fInChannelList:=TList.Create;
  fOutChannelList:=TList.Create;
  fLastErrorMsg := '';
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

procedure TDavASIOExtendedDriver.SetErrorMessage(s: string);
begin
  fLastErrorMsg := s;
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
  t.ClockName := copy(name,0,32);
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

  t := TDavASIOEDChannelListItem.Create(name, channelgroup, SampleType, IsInput);

  if IsInput then
    fInChannelList.Add(t)
  else
    fOutChannelList.Add(t);
end;

function TDavASIOExtendedDriver.GetCurrentClockSource: Integer;
var i: integer;
begin
  result:=-1;
  if fClockList.Count<1 then exit;

  for i:=0 to fClockList.Count-1 do
    with TDavASIOEDClockListItem(fClockList.Items[i]) do
    begin
      if IsCurrentSource then
      begin
        result:=i;
        break;
      end;
    end;
end;

function TDavASIOExtendedDriver.GetFirstGroupChannel(GroupNr: Longint; IsInput: Boolean): Longint;
var querylist: TList;
    i: integer;
begin
  result := -1;
  if IsInput then
    querylist := fInChannelList
  else
    querylist := fOutChannelList;

  if querylist.Count>0 then
    for i := 0 to querylist.Count-1 do
      with TDavASIOEDChannelListItem(querylist.Items[i]) do
        if ChannelGroup=GroupNr then
        begin
          result := i;
          break;
        end;
end;

function TDavASIOExtendedDriver.Init(SysHandle: HWND): boolean;
begin
  fHostHandle := SysHandle;
  result := true;
end;

function TDavASIOExtendedDriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
var i: integer;
begin
  Result := ASE_NotPresent;

  NumSources := min(NumSources,fClockList.Count);
  if NumSources>0 then
  begin
    for i := 0 to fClockList.Count-1 do
      with TDavASIOEDClockListItem(fClockList.Items[i]) do
      begin
        Clocks^.Index := i;
        Clocks^.AssociatedChannel := GetFirstGroupChannel(ChannelGroup,true);
        Clocks^.AssociatedGroup := ChannelGroup ;
        Clocks^.IsCurrentSource := TASIOBool(IsCurrentSource);
        StrCopy(Clocks^.Name, PChar(ClockName));

        Inc(Clocks);
      end;

    result := ASE_OK;
  end;
end;

function TDavASIOExtendedDriver.SetClockSource(Reference: Integer): TASIOError;
var last: integer;
begin
  result := ASE_OK;

  if (Reference>=fClockList.Count) or (Reference<0) then
  begin
    result := ASE_InvalidParameter;
    exit;
  end;

  last:=GetCurrentClockSource;
  if Reference=last then exit;

  if last>=0 then TDavASIOEDClockListItem(fClockList.Items[last]).IsCurrentSource := false;
  TDavASIOEDClockListItem(fClockList.Items[Reference]).IsCurrentSource := true;

  // TODO: call Event handler here, with last and Reference as params
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
    StrPCopy(Info.Name, Pchar(ChannelName));
    result := ASE_OK;
  end;
end;

function TDavASIOExtendedDriver.GetErrorMessage: string;
begin
  result := fLastErrorMsg;
end;

function TDavASIOExtendedDriver.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
var Channel: integer;
begin
   for Channel := 0 to NumChannels-1 do
   begin
     // TODO
   end;
end;

function TDavASIOExtendedDriver.DisposeBuffers: TASIOError;
begin
  // TODO
end;


end.
