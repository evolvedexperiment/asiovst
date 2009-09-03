unit DAV_ASIODriverInterceptor;

interface

uses Classes,SysUtils,Windows,Forms,
     DAV_ASIO,DAV_ASIOList,DAV_ASIODriver,DAV_AsioInterface;

type
  TDavASIOInterceptor = class;

  TDavASIOInterceptorCP = class(TForm)
  protected
    Interceptor: TDavASIOInterceptor;
  public
    constructor Create(AOwner: TComponent; cInterceptor: TDavASIOInterceptor); reintroduce;
  end;

  TTDavASIOInterceptorCP = class of TDavASIOInterceptorCP;

  TDavASIOInterceptor = class(TDavASIODriver)
  protected
    fDriverName: string;
    fDriverVersion: Longint;
    fDriverList: TDAVAsioDriverList;
    fHostInterface: IStdCallASIO;
    fCurrentDriverIndex: integer;
    fHostCallbacks: PASIOCallbacks;
    fDriverCallbacks: TASIOCallbacks;

    fControlPanel: TDavASIOInterceptorCP;
    fControlPanelClass: TTDavASIOInterceptorCP;

    procedure UnloadHostInterface;
    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
    procedure SetControlPanelClass(cp: TTDavASIOInterceptorCP);
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
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; override;
    function DisposeBuffers: TASIOError; override;
    function ControlPanel: TASIOError; override;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; override;
    function OutputReady: TASIOError; override;

    procedure ASIOBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool); virtual;
    function ASIOBufferSwitchTimeInfo(var Params: TASIOTime; DoubleBufferIndex: Integer; DirectProcess: TASIOBool): PASIOTime; virtual;
    procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); virtual;
    function ASIOMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble): Integer; virtual;
  end;

implementation

var GlobalCallbackInst: TDavASIOInterceptor;

procedure callbackBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool); cdecl;
begin
  if assigned(GlobalCallbackInst) then GlobalCallbackInst.ASIOBufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function callbackBufferSwitchTimeInfo(var Params: TASIOTime; DoubleBufferIndex: Integer; DirectProcess: TASIOBool): PASIOTime; cdecl;
begin
  if assigned(GlobalCallbackInst) then result:=GlobalCallbackInst.ASIOBufferSwitchTimeInfo(params, DoubleBufferIndex, DirectProcess)
  else result := @Params; // dummy
end;

procedure callbackSampleRateDidChange(SampleRate: TASIOSampleRate); cdecl;
begin
  if assigned(GlobalCallbackInst) then GlobalCallbackInst.ASIOSampleRateDidChange(SampleRate);
end;

function callbackMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble): Integer; cdecl;
begin
  if assigned(GlobalCallbackInst) then result := GlobalCallbackInst.ASIOMessage(Selector, Value, msg, Opt)
  else result := 0;
end;


{ TDavASIOInterceptorCP }

constructor TDavASIOInterceptorCP.Create(AOwner: TComponent; cInterceptor: TDavASIOInterceptor);
begin
  inherited Create(AOwner);
  Interceptor := cInterceptor;
end;

{ TDavASIOInterceptor }

constructor TDavASIOInterceptor.Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid);
begin
  inherited;
  GlobalCallbackInst := self;
  fDriverList := TDAVAsioDriverList.Create(InterfaceGUID);
  fDriverList.UpdateList;
  fCurrentDriverIndex:=0;
  fHostInterface := nil;
  fDriverName := 'DAV Abstract Int';
  fDriverVersion := 1; 
  fControlPanelClass := nil;
  fControlPanel := nil;

  fHostCallbacks := nil;
  with fDriverCallbacks do
   begin
    BufferSwitch := callbackBufferSwitch;
    SampleRateDidChange := callbackSampleRateDidChange;
    BufferSwitchTimeInfo := callbackBufferSwitchTimeInfo;
    ASIOMessage := callbackMessage;
   end;

  InitializeDriverParams;

  if assigned(fControlPanelClass) then
  begin
    fControlPanel := fControlPanelClass.Create(nil, self);
    //fControlPanel.ParentWindow := GetDesktopWindow;
  end;
end;

destructor TDavASIOInterceptor.Destroy;
begin
  if Assigned(fControlPanel) then FreeAndNil(fControlPanel);

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

procedure TDavASIOInterceptor.SetControlPanelClass(cp: TTDavASIOInterceptorCP);
begin
  fControlPanelClass := cp;
end;

procedure TDavASIOInterceptor.UnloadHostInterface;
begin
  if not assigned(fHostInterface) then exit;
  fHostInterface.Stop;
  fHostInterface.DisposeBuffers;
  fHostInterface := nil;
end;

function TDavASIOInterceptor.Init(SysHandle: HWND): boolean;
begin
  UnloadHostInterface;
  if fCurrentDriverIndex>fDriverList.Count-1 then
    result:=false
  else begin
    try
      result := CreateStdCallASIO(TDAVAsioDriverDesc(fDriverList.Items[fCurrentDriverIndex]).Guid,fHostInterface);
      if result then
        result := fHostInterface.Init(SysHandle) = ASIOTrue;
    except
      result := false;
      fHostInterface := nil;
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
  if assigned(fHostInterface) then
  try
    fHostInterface.GetErrorMessage(tmp);
    Result := tmp;
  except
  end;
end;

function TDavASIOInterceptor.Start: TASIOError;
begin
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.Start;
  except
    result := ASE_NotPresent;
  end;    
end;

function TDavASIOInterceptor.Stop: TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.Stop;
  except
    result := ASE_NotPresent;
  end; 
end;

function TDavASIOInterceptor.GetChannels(out NumInputChannels,NumOutputChannels: Integer): TASIOError;
begin  
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetChannels(NumInputChannels, NumOutputChannels);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetLatencies(out InputLatency,OutputLatency: Integer): TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetLatencies(InputLatency, OutputLatency);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetBufferSize(MinSize, MaxSize, PreferredSize, Granularity);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin   
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.CanSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin  
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin  
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.SetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetClockSources(Clocks: PASIOClockSources;out NumSources: Integer): TASIOError;
begin  
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetClockSources(Clocks, NumSources);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetClockSource(Reference: Integer): TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.SetClockSource(Reference);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSamplePosition(out SamplePosition: TASIOSamples;out TimeStamp: TASIOTimeStamp): TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetSamplePosition(SamplePosition, TimeStamp);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
begin  
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.GetChannelInfo(Info);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
begin   
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.CreateBuffers(BufferInfos, NumChannels, BufferSize, Callbacks);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.DisposeBuffers: TASIOError;
begin
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.DisposeBuffers;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.ControlPanel: TASIOError;
begin
  if Assigned(fControlPanel) then
  begin
    fControlPanel.ShowModal;
    //TODO: Event handler with ModalResult
    Result := ASE_OK;
    exit;
  end;

  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.ControlPanel;
  except
    result := ASE_NotPresent;
  end;
end;


function TDavASIOInterceptor.Future(Selector: Integer;Opt: Pointer): TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.Future(Selector,Opt);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.OutputReady: TASIOError;
begin 
  if not assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;
  
  try
    result := fHostInterface.OutputReady;
  except
    result := ASE_NotPresent;
  end;
end;

procedure TDavASIOInterceptor.ASIOBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool);
begin
  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.bufferSwitch) then
    fHostCallbacks^.bufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function TDavASIOInterceptor.ASIOBufferSwitchTimeInfo(var Params: TASIOTime; DoubleBufferIndex: Integer; DirectProcess: TASIOBool): PASIOTime;
begin
  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.bufferSwitchTimeInfo) then
    result := fHostCallbacks^.bufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else
    result := @Params; // dummy
end;

procedure TDavASIOInterceptor.ASIOSampleRateDidChange(SampleRate: TASIOSampleRate);
begin
  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.sampleRateDidChange) then
    fHostCallbacks^.sampleRateDidChange(SampleRate);
end;

function TDavASIOInterceptor.ASIOMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble): Integer;
begin
  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.asioMessage) then
    result := fHostCallbacks^.asioMessage(Selector, Value, msg, Opt)
  else result := 0;
end;

initialization

GlobalCallbackInst:=nil;

end.
