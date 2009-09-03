unit DAV_ASIODriverInterceptor;

interface

uses Classes,SysUtils,Registry,Windows,DAV_ASIO,DAV_ASIODriver,DAV_AsioInterface;

const
  DAVIntASIOInprocServer = 'InprocServer32';
  DAVIntASIOPath         = 'software\asio';
  DAVIntASIOComClsId     = 'clsid';
  DAVIntASIODescription  = 'description';

type
  TDAVIntAsioDriverDesc = class
  private
    fGuid: TGUID;
    fName: string;
    fFilename: string;
  public
    constructor Create(nGuid: TGUID; nName: string; nFilename: string); overload;
    constructor Create(nGuid, nName, nFilename: string); overload;
    property Guid: TGUID read fGuid;
    property Name: string read fName;
    property Filename: string read fFilename;
  end;

  TDAVIntAsioDriverList = class
  private
    fIgnoreGuid: TGuid;
    fHasIgnoreGuid: boolean;
    fList: TList;
    procedure ClearList;
    procedure LoadList;
    function GetDriverFileName(DrvGuidStr: string): string;
    function GetItem(Index: Integer): TDAVIntAsioDriverDesc;
    function GetCount: Integer;
  public
    constructor Create(Ignore: TGuid); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure UpdateList;
    function GetDriverNames: TStrings;
    property Items[Index: Integer]: TDAVIntAsioDriverDesc read GetItem;
    property Count: Integer read GetCount;
  end;

  TDavASIOInterceptor = class(TDavASIODriver)
  protected
    fDriverName: string;
    fDriverVersion: Longint;
    fDriverList: TDAVIntAsioDriverList;
    FHostInterface: IStdCallASIO;
    FCurrentDriverIndex: integer;
    procedure UnloadHostInterface;
    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid); override;
    destructor Destroy; override;

    function Init(SysHandle: HWND): boolean; override;
    function GetDriverName: string; override;
    function GetDriverVersion: LongInt; override;
    function GetErrorMessage: string; override;
    function Start: TASIOError; override;
    function Stop: TASIOError; override;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; override;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; override;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; override;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; override;
    function DisposeBuffers: TASIOError; override;
    function ControlPanel: TASIOError; override;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; override;
    function OutputReady: TASIOError; override;
  end;

implementation

{debug:}
uses dialogs;

{ TDAVIntAsioDriverDesc }

constructor TDAVIntAsioDriverDesc.Create(nGuid: TGUID; nName: string; nFilename: string);
begin
  fGuid:=nGuid;
  fName:=nName;
  fFilename:=nFilename;
end;

constructor TDAVIntAsioDriverDesc.Create(nGuid, nName, nFilename: string);
begin
  fGuid:=StringToGUID(nGuid);
  fName:=nName;
  fFilename:=nFilename;
end;

{ TDAVIntAsioDriverList }

constructor TDAVIntAsioDriverList.Create;
begin
  fHasIgnoreGuid := false;
  fList:=TList.Create;
end;

constructor TDAVIntAsioDriverList.Create(Ignore: TGuid);
begin
  Create;
  fIgnoreGuid := Ignore;
  fHasIgnoreGuid := true;
end;

destructor TDAVIntAsioDriverList.Destroy;
begin
  ClearList;
  fList.Free;
  inherited;
end;

procedure TDAVIntAsioDriverList.ClearList;
var i:integer;
begin
  for i := Count-1 downto 0 do Items[i].Free;

  fList.Clear;
end;

function TDAVIntAsioDriverList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TDAVIntAsioDriverList.GetDriverFileName(DrvGuidStr: string): string;
var Filename: string;
    DirStr : PChar;
begin
  result := '';
  if DrvGuidStr='' then exit;

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly(DAVIntASIOComClsId + '\' + Lowercase(DrvGuidStr) + '\' + DAVIntASIOInprocServer) then
    begin
      result := ReadString('');
      Filename := ExtractFileName(result);
      DirStr := StrAlloc(MAX_PATH);

      if not FileExists(result) and (GetSystemDirectory(DirStr, MAX_PATH) <> 0) then
        result := StrPas(DirStr) + '\' + Filename;
      
      if not FileExists(result) and (GetWindowsDirectory(DirStr, MAX_PATH) <> 0) then
        result := StrPas(DirStr) + '\' + Filename;

      if not FileExists(result) then
        Result := '';

      StrDispose(DirStr);
      CloseKey;
    end;
  
  finally
    Free;
  end;
end;

function TDAVIntAsioDriverList.GetItem(Index: Integer): TDAVIntAsioDriverDesc;
begin
  Result := TDAVIntAsioDriverDesc(fList.Items[Index]);
end;

procedure TDAVIntAsioDriverList.LoadList;
var SubKeys: TStringList;
    i: Integer;
    DrvName: string;
    DrvGuidStr: string;
    DrvFile: string;
    DrvGuid: TGuid;
    newAsioDriverItem: TDAVIntAsioDriverDesc;
begin
  SubKeys := TStringList.Create;
  with TRegistry.Create do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(DAVIntASIOPath) then
    begin
      GetKeyNames(SubKeys);
      CloseKey;
    end;

    for i:=0 to SubKeys.Count-1 do
    begin
      if OpenKeyReadOnly(DAVIntASIOPath + '\' + SubKeys[i]) then
      begin
        DrvGuidStr := ReadString(DAVIntASIOComClsId);
        DrvGuid := StringToGUID(DrvGuidStr);

        DrvFile:=GetDriverFileName(DrvGuidStr);
        if (DrvFile<>'') and not ( fHasIgnoreGuid and IsEqualGUID(DrvGuid,fIgnoreGuid) ) then
        begin
          DrvName := ReadString(DAVIntASIODescription);
          if DrvName='' then DrvName:=SubKeys[i];

          newAsioDriverItem:=TDAVIntAsioDriverDesc.Create(DrvGuidStr,DrvName,DrvFile);
          fList.Add(newAsioDriverItem);
        end;

        CloseKey;
      end;
    end;
  finally
    Free;
    SubKeys.Free;
  end;
end;

procedure TDAVIntAsioDriverList.UpdateList;
begin
  ClearList;
  LoadList;
end;

function TDAVIntAsioDriverList.GetDriverNames: TStrings;
var i:integer;
begin
  result:=TStringList.Create;
  for i := 0 to Count-1 do Result.Add(Items[i].Name);
end;


{ TDavASIOInterceptor }


constructor TDavASIOInterceptor.Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid);
begin
  inherited;
  fDriverList := TDAVIntAsioDriverList.Create(InterfaceGUID);
  fDriverList.UpdateList;
  FCurrentDriverIndex:=0;
  FHostInterface := nil;
  fDriverName := 'DAV Abstract Int';
  fDriverVersion := 1;
  InitializeDriverParams;
end;

destructor TDavASIOInterceptor.Destroy;
begin
  UnloadHostInterface;
  fDriverList.Free;
  inherited;
end;

procedure TDavASIOInterceptor.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOInterceptor.SetDriverName(name: string);
begin
  fDriverName := name;
end;

procedure TDavASIOInterceptor.SetDriverVersion(version: LongInt);
begin
  fDriverVersion := version;
end;

procedure TDavASIOInterceptor.UnloadHostInterface;
begin
  if not assigned(FHostInterface) then exit;
  FHostInterface.Stop;
  FHostInterface.DisposeBuffers;
  FHostInterface := nil;
end;

function TDavASIOInterceptor.Init(SysHandle: HWND): boolean;
begin
  UnloadHostInterface;
  if FCurrentDriverIndex>fDriverList.Count-1 then
    result:=false
  else begin
    try
      result := CreateStdCallASIO(TDAVIntAsioDriverDesc(fDriverList.Items[FCurrentDriverIndex]).Guid,FHostInterface);
      if result then
        result := FHostInterface.Init(SysHandle) = ASIOTrue;
    except
      result := false;
      FHostInterface := nil;
    end;
  end;
end;

function TDavASIOInterceptor.GetDriverName: string;
begin
  result := fDriverName;
end;

function TDavASIOInterceptor.GetDriverVersion: LongInt;
begin
  result := fDriverVersion;
end;

function TDavASIOInterceptor.GetErrorMessage: string;
var tmp: array[0..124] of char;
begin
  result := '';
  if assigned(FHostInterface) then
  try
    FHostInterface.GetErrorMessage(tmp);
    Result := tmp;
  except
  end;
end;

function TDavASIOInterceptor.Start: TASIOError;
begin
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.Start;
  except
    result := ASE_NotPresent;
  end;    
end;

function TDavASIOInterceptor.Stop: TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.Stop;
  except
    result := ASE_NotPresent;
  end; 
end;

function TDavASIOInterceptor.GetChannels(out NumInputChannels,NumOutputChannels: Integer): TASIOError;
begin  
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetChannels(NumInputChannels, NumOutputChannels);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetLatencies(out InputLatency,OutputLatency: Integer): TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetLatencies(InputLatency, OutputLatency);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetBufferSize(MinSize, MaxSize, PreferredSize, Granularity);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin   
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.CanSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin  
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin  
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.SetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetClockSources(Clocks: PASIOClockSource;out NumSources: Integer): TASIOError;
begin  
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetClockSources(Clocks, NumSources);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetClockSource(Reference: Integer): TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.SetClockSource(Reference);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSamplePosition(out SamplePosition: TASIOSamples;out TimeStamp: TASIOTimeStamp): TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetSamplePosition(SamplePosition, TimeStamp);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
begin  
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.GetChannelInfo(Info);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
begin   
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.CreateBuffers(BufferInfos, NumChannels, BufferSize, Callbacks);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.DisposeBuffers: TASIOError;
begin
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.DisposeBuffers;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.ControlPanel: TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.ControlPanel;
  except
    result := ASE_NotPresent;
  end;
end;


function TDavASIOInterceptor.Future(Selector: Integer;Opt: Pointer): TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.Future(Selector,Opt);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.OutputReady: TASIOError;
begin 
  if not assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := FHostInterface.OutputReady;
  except
    result := ASE_NotPresent;
  end;
end;

end.
