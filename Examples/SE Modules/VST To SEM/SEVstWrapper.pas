unit SEVstWrapper;

interface

uses
  Windows, Classes, DAV_Common, DAV_SECommon, DAV_SEModule, DAV_VSTHost;

type
  TCustomVST2SEModule = class(TSEModuleBase)
  protected
    FVSTHost    : TVstHost;
    FInputPtr   : array of PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputPtr  : array of PDAVSingleFixedArray;
    FVSTInputs  : array of PDAVSingleFixedArray;
    FVSTOutputs : array of PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TStaticVST2SEModule = class(TCustomVST2SEModule)
  protected
    FParamPtr   : TDAVSingleDynArray;
    FParamEnum  : array of Integer;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

  TAutomatableVST2SEModule = class(TCustomVST2SEModule)
  protected
    FParamPtr   : array of PDAVSingleDynArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

implementation

uses
  SysUtils;

var
 FS  : TFormatSettings;


function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

constructor TCustomVST2SEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
var
  i  : Integer;
  RS : TResourceStream;
  CM : TStringList;
begin
 inherited Create(SEAudioMaster, Reserved);
 FVSTHost := TVstHost.Create(nil);

 CM := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'VST', @EnumNamesFunc, DWord(CM));
  FVSTHost.VstPlugIns.Clear;
  assert(CM.Count <= 1);
  for i := 0 to CM.Count - 1 do
   begin
    FVSTHost.VstPlugIns.Add;
    RS := TResourceStream.Create(HInstance, CM[i], 'VST');
    try
     FVSTHost.VstPlugIns[i].LoadFromStream(RS);
     FVSTHost.VstPlugIns[i].Open;
     SetLength(FInputPtr, FVSTHost[i].numInputs);
     SetLength(FOutputPtr, FVSTHost[i].numOutputs);
     SetLength(FVSTInputs, FVSTHost[i].numInputs);
     SetLength(FVSTOutputs, FVSTHost[i].numOutputs);
    finally
     FreeAndNil(RS);
    end;
   end;
 finally
  FreeAndNil(CM);
 end;
end;

destructor TCustomVST2SEModule.Destroy;
begin
 FreeAndNil(FVSTHost);
 inherited;
end;

procedure TCustomVST2SEModule.Open;
begin
 OnProcess := SubProcess;
 if FVSTHost.Count = 0
  then CallHost(SEAudioMasterSleepMode);
 inherited Open; // always call the base class
end;

class procedure TCustomVST2SEModule.GetModuleProperties(Properties: PSEModuleProperties);
var
  CM  : TStringList;
  plg : string;
  str : string;
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   str := 'DAV VST-Wrapper';
   plg := 'Wrapper';
   CM := TStringList.Create;
   try
    EnumResourceNames(HInstance, 'VST', @EnumNamesFunc, DWord(CM));
    if CM.Count = 1
     then plg := CM[0]
     else plg := 'error!';
    str := str + ' - ' + plg;
   finally
    FreeAndNil(CM);
   end;

   str := str + #0;
   GetMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   str := 'VST2SEM - ' + plg + #0;
   GetMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));

   // Info, may include Author, Web page whatever
   About := 'Wrapper created by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
//   GuiFlags := [gfControlView, gfStructureView];
  end;
end;

// describe the pins (plugs)
function TCustomVST2SEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  i, ndx : Integer;
  s, p   : Single;
  str    : string;
begin
 if FVSTHost.Count = 0 then
  begin
   result := False;
   exit;
  end;
 result := True;
 if Index < FVSTHost[0].numInputs then
  with Properties^ do
   begin
    str             := 'Input ' + IntToStr(Index + 1);
    Name            := PChar(str);
    VariableAddress := @FInputPtr[Index];
    Direction       := drIn;
    Datatype        := dtFSample;
    Flags           := [iofPolyphonicActive];
   end else
 if Index - FVSTHost[0].numInputs < FVSTHost[0].numOutputs then
  with Properties^ do
   begin
    str             := 'Output ' + IntToStr(Index + 1 - FVSTHost[0].numInputs);
    Name            := PChar(str);
    VariableAddress := @FOutputPtr[Index - FVSTHost[0].numInputs];
    Direction       := drOut;
    Datatype        := dtFSample;
   end else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
end;

{ TStaticVST2SEModule }

constructor TStaticVST2SEModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 if FVSTHost.Count > 0 then
  begin
   SetLength(FParamPtr, FVSTHost[0].numParams);
   SetLength(FParamEnum, FVSTHost[0].numParams);
  end;
end;

procedure TStaticVST2SEModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to Length(FVSTInputs)  - 1 do FVSTInputs[i]  := @FInputPtr[i, BufferOffset];
 for i := 0 to Length(FVSTOutputs) - 1 do FVSTOutputs[i] := @FOutputPtr[i, BufferOffset];
 FVSTHost[0].ProcessAudio(@FVSTInputs[0], @FVSTOutputs[0], SampleFrames);
end;

function TryStrToFloatX(const S: string): Boolean;
var
 dbl : Double;
 FS  : TFormatSettings;
begin
 FS.DecimalSeparator := '.';
 Result := TryStrToFloat(s, dbl, FS);
 FS.DecimalSeparator := ',';
 if TryStrToFloat(s, dbl, FS)
  then Result := True;
end;

class procedure TStaticVST2SEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
var
  str : string;
begin
 inherited;
 with Properties^ do
  begin
   str := StrPas(Name) + ' (static)';
   str := str + #0;
   ReallocMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   str := StrPas(ID) + ' (static)';
   str := str + #0;
   ReallocMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));
  end;
end;

// describe the pins (plugs)
function TStaticVST2SEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  i, ndx : Integer;
  s, p   : Single;
  str    : string;
  pd, un : array [0..1] of string;
  IsFlt  : Boolean;
begin
 if FVSTHost.Count = 0 then
  begin
   result := False;
   exit;
  end;

 result := inherited GetPinProperties(Index, Properties);

 if not result and (Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs < FVSTHost[0].numParams) then
  with Properties^ do
   begin
    ndx := Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs;

    // parameter name
    str := FVSTHost[0].ParameterName[ndx] + #0;
    GetMem(Name, Length(str));
    Move(str[1], Name[0], Length(str));
    Direction := drParameter;

    // detect
    FVSTHost[0].Parameter[ndx] := 0;
    pd[0] := FVSTHost[0].ParameterDisplay[ndx] + FVSTHost[0].ParameterLabel[ndx];
    FVSTHost[0].Parameter[ndx] := 0.01;
    pd[1] := FVSTHost[0].ParameterDisplay[ndx] + FVSTHost[0].ParameterLabel[ndx];

    if pd[0] = pd[1] then
     begin
      VariableAddress := @FParamPtr[ndx];
      Datatype        := dtEnum;

      // build enum values
      str := '';
      with TStringList.Create do
       try
        FVSTHost[0].Parameter[ndx] := 0;
        s := 0.01;
        p := FVSTHost[0].Parameter[ndx];
        pd[0] := FVSTHost[0].ParameterDisplay[ndx];
        un[0] := FVSTHost[0].ParameterLabel[ndx];
        IsFlt := TryStrToFloatX(pd[0]);
        Add(pd[0] + un[0]);
        while FVSTHost[0].Parameter[ndx] < 1 do
         begin
          FVSTHost[0].Parameter[ndx] := FVSTHost[0].Parameter[ndx] + s;
          if FVSTHost[0].Parameter[ndx] = p then
           begin
            s := s + 0.03;
            if s >= 1
             then Break
             else Continue;
           end;
          pd[1] := FVSTHost[0].ParameterDisplay[ndx];
          un[1] := FVSTHost[0].ParameterLabel[ndx];
          if pd[0] + un[0] <> pd[1] + un[1] then
           begin
            pd[0] := pd[1];
            un[0] := un[1]; 
            Add(pd[0] + un[0]);
            if not TryStrToFloatX(pd[0])
             then IsFlt := False;
           end;
         end;

        if (Count = 1) or (IsFlt and (Count > 10)) then
         begin
          VariableAddress := @FParamPtr[ndx];
          Datatype        := dtSingle;
          FParamEnum[ndx] := 0;
          DefaultValue    := '0';
          exit;
         end;

        FParamEnum[ndx] := Count;
        for i := 0 to Count - 1
         do str := str + Strings[i] + '=' + IntToStr(i) + ',';
       finally
        Free;
       end;

      if Length(str) > 0 then SetLength(str, Length(str) - 1);
      DatatypeExtra := PChar(str);
      DefaultValue  := '0';
     end
    else
     begin
      VariableAddress := @FParamPtr[ndx];
      Datatype        := dtSingle;
      FParamEnum[ndx] := 0;
      DefaultValue    := '0';
     end;
    result := True;
   end;
end;

// An input plug has changed value
procedure TStaticVST2SEModule.PlugStateChange(const CurrentPin: TSEPin);
var
  ndx : Integer;
begin
 if FVSTHost.Count >= 0 then
  with FVSTHost[0] do
   if (CurrentPin.PinID >= numInputs + numOutputs) and
      (CurrentPin.PinID < numInputs + numOutputs + numParams)  then
    begin
     ndx := CurrentPin.PinID - numInputs - numOutputs;
     if FParamEnum[ndx] > 0
      then Parameter[ndx] := PInteger(@FParamPtr[ndx])^ / FParamEnum[ndx]
      else Parameter[ndx] := FParamPtr[ndx]
    end;

 // ToDo detect changes

 inherited;
end;


{ TAutomatableVST2SEModule }

constructor TAutomatableVST2SEModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 if FVSTHost.Count > 0
  then SetLength(FParamPtr, FVSTHost[0].numParams);
end;

class procedure TAutomatableVST2SEModule.GetModuleProperties(Properties: PSEModuleProperties);
var
  str : string;
begin
 inherited;
 with Properties^ do
  begin
   str := StrPas(Name) + ' (automatable)';
   str := str + #0;
   ReallocMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   str := StrPas(ID) + ' (automatable)';
   str := str + #0;
   ReallocMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));
  end;
end;

function TAutomatableVST2SEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
var
  i, ndx : Integer;
  s, p   : Single;
  str    : string;
  pd, un : array [0..1] of string;
  IsFlt  : Boolean;
begin
 if FVSTHost.Count = 0 then
  begin
   result := False;
   exit;
  end;

 result := inherited GetPinProperties(Index, Properties);
 if not result and (Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs < FVSTHost[0].numParams) then
  with Properties^ do
   begin
    ndx := Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs;

    // parameter name
    str := FVSTHost[0].ParameterName[ndx] + #0;
    GetMem(Name, Length(str));
    Move(str[1], Name[0], Length(str));
    Direction       := drIn;
    VariableAddress := @FParamPtr[ndx];
    Datatype        := dtFSample;
    DefaultValue    := '0';
    result          := True;
   end;
end;

procedure TAutomatableVST2SEModule.PlugStateChange(const CurrentPin: TSEPin);
var
  ndx : Integer;
begin
 if FVSTHost.Count >= 0 then
  with FVSTHost[0] do
   if (CurrentPin.PinID >= numInputs + numOutputs) and
      (CurrentPin.PinID < numInputs + numOutputs + numParams)  then
    begin
     ndx := CurrentPin.PinID - numInputs - numOutputs;
     Parameter[ndx] := CurrentPin.Value;
    end;
 inherited;
end;

procedure TAutomatableVST2SEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to Length(FVSTInputs)  - 1 do FVSTInputs[i]  := @FInputPtr[i, BufferOffset];
 for i := 0 to Length(FVSTOutputs) - 1 do FVSTOutputs[i] := @FOutputPtr[i, BufferOffset];
 FVSTHost[0].ProcessAudio(@FVSTInputs[0], @FVSTOutputs[0], SampleFrames);
end;

procedure TAutomatableVST2SEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to Length(FVSTInputs)  - 1 do FVSTInputs[i]  := @FInputPtr[i, BufferOffset];
 for i := 0 to Length(FVSTOutputs) - 1 do FVSTOutputs[i] := @FOutputPtr[i, BufferOffset];
 FVSTHost[0].ProcessAudio(@FVSTInputs[0], @FVSTOutputs[0], SampleFrames);
end;

initialization
 GetLocaleFormatSettings(GetThreadLocale, FS);

end.
