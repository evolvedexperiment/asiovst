unit DAV_SEHost;

interface

uses
  Windows, Classes, SysUtils, DAV_SECommon, DAV_SEModule, DAV_SEGUI,
  DAV_DLLLoader;

type
  TSEGetModuleProperties = function(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl;
  TSEMakeModule = function(Index, ProcessType: Integer; Callback: Pointer; Reserved: Pointer): Pointer; cdecl;

  TCustomSEHost = class;
  TCustomSEHostedModule = class;

  TCustomSEHostedModulePart = class(TPersistent)
  private
    FIndex            : Integer;
    FProperties       : TSEModuleProperties;
    FSEHostedModule   : TCustomSEHostedModule;
    FSE2ModStructBase : PSE2ModStructBase;
    FSEGUIStructBase  : PSEGUIStructBase;

    FOnRepaintRequest : TNotifyEvent;

    function GetAbout: string;
    function GetID: string;
    function GetName: string;
    procedure SetActive(const Value: Boolean);
    function GetActive: Boolean;
    function GetMagic: Integer;
    function GetVersion: Integer;
    procedure DisposeStructures;
  protected
    function CallPlugin(Opcode: TSEPluginModuleOpcodes; Index: Integer = 0;
      Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer; virtual;
    procedure GuiHostRepaintRequest;
    function GuiHostGetHandle: THandle;
    procedure GuiHostSetWindowSize(const x, y: Integer);
    procedure GuiHostSetWindowSizeable;
    procedure GuiHostAddGuiPlug;
    function GuiHostGetTotalPinCount: Integer;
    procedure GuiHostResolveFilename(FileName: PWideChar; MaxStringLength: Integer);
  public
    constructor Create(Owner: TCustomSEHostedModule; Index: Integer = 0;
      Properties: PSEModuleProperties = nil); virtual;
    procedure Instanciate; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure AddEvent(Event: TSEEvent); virtual;
    function IsEventListEmpty: Boolean; virtual;
    function QueryDebugInfo: Pointer; virtual;
    procedure Resume(Index: Integer = 0); virtual;
    procedure GuiNotify(Value: Integer = 0; Index: Integer = 0; Ptr: Pointer = nil); virtual;
    procedure SetSampleRate(Value: Single); virtual;
    procedure SetBlockSize(Value: Integer); virtual;
    function GetPinProperties(Index: Integer; var Pin: TSEPinProperties): Boolean;

    property Properties: TSEModuleProperties read FProperties;
  published
    property Active: Boolean read GetActive write SetActive;
    property Name: string read GetName;
    property ID: string read GetID;
    property Magic: Integer read GetMagic;
    property Version: Integer read GetVersion;
    property About: string read GetAbout;

    property OnRepaintRequest: TNotifyEvent read FOnRepaintRequest write FOnRepaintRequest;
  end;

  TCustomSEHostedModule = class(TCollectionItem)
  private
    FLoaded              : Boolean;
    FDisplayName         : string;
    FParts               : array of TCustomSEHostedModulePart;
    FSEMFileName         : TFileName;
    FSEModuleHandle      : THandle;
    FInternalDLLLoader   : TDLLLoader;
    FGetModuleProperties : TSEGetModuleProperties;
    FMakeModule          : TSEMakeModule;
    procedure SetSEMFileName(const Value: TFileName);
    procedure InitializeVariables;
    procedure ListParts;
    procedure CloseParts;
    function GetPart(index: Integer): TCustomSEHostedModulePart;
    function GetPartCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: TFilename);
    procedure LoadFromStream(Stream: TStream);
    procedure UnLoad;

    property Loaded: Boolean read FLoaded;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property SEMFileName: TFileName read FSEMFileName write SetSEMFileName;
    property Part[index: Integer]: TCustomSEHostedModulePart read GetPart;
    property PartCount: Integer read GetPartCount; 
  end;

  TSEHostedModule = class(TCustomSEHostedModule)
  published
    property Loaded;
    property DisplayName;
    property SEMFileName;
  end;

  TSEHostedModules = class(TOwnedCollection)
  private
    FOwner: TComponent;
    function GetVSTHost: TCustomSEHost;
    function GetItem(Index: Integer): TSEHostedModule;
    procedure SetItem(Index: Integer; const Value: TSEHostedModule);
  protected
    property Items[Index: Integer]: TSEHostedModule read GetItem write SetItem; default;
    property VstHost: TCustomSEHost read GetVSTHost; 
  public
    constructor Create(AOwner: TComponent);
    function Add: TSEHostedModule;
    function CloneAdd(Source: TSEHostedModule): TSEHostedModule;
    function Insert(Index: Integer): TSEHostedModule;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TCustomSEHost = class(TComponent)
  private
    FSampleRate         : Single;
    FBlockSize          : Integer;
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    FSEHostedModules    : TSEHostedModules;
    function GetItem(Index: Integer): TCustomSEHostedModule;
    procedure SetHostedModules(const Value: TSEHostedModules);
  protected
    property Items[Index: Integer]: TCustomSEHostedModule read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property HostedSEModules: TSEHostedModules read FSEHostedModules write SetHostedModules;
  end;

  TSEHost = class(TCustomSEHost)
  published
    property OnCreate;
    property OnDestroy;
    property HostedSEModules;
  end;

implementation

uses
  DAV_Common;
  
resourcestring
  RStrLoadingFailed              = 'Loading failed!';
  RStrFileDoesNotExist           = 'File %d does not exists';
  RStrNoEntryPoint               = 'VST entry point could not be detected';
  RStrValue                      = 'Value';


function HostOpcodeToString(Opcode: TSEHostOpcodes): string;
begin
 case Opcode of
  SEAudioMasterSetPinStatus       : result := 'SEAudioMasterSetPinStatus';
  SEAudioMasterIsPinConnected     : result := 'SEAudioMasterIsPinConnected';
  SEAudioMasterGetPinInputText    : result := 'SEAudioMasterGetPinInputText';
  SEAudioMasterGetSampleClock     : result := 'SEAudioMasterGetSampleClock';
  SEAudioMasterSendMIDI           : result := 'SEAudioMasterSendMIDI';
  SEAudioMasterGetInputPinCount   : result := 'SEAudioMasterGetInputPinCount';
  SEAudioMasterGetOutputPinCount  : result := 'SEAudioMasterGetOutputPinCount';
  SEAudioMasterGetPinVarAddress   : result := 'SEAudioMasterGetPinVarAddress';
  SEAudioMasterGetBlockStartClock : result := 'SEAudioMasterGetBlockStartClock';
  SEAudioMasterGetTime            : result := 'SEAudioMasterGetTime';
  SEAudioMasterSleepMode          : result := 'SEAudioMasterSleepMode';
  SEAudioMasterGetRegisteredName  : result := 'SEAudioMasterGetRegisteredName';
  SEAudioMasterGetFirstClone      : result := 'SEAudioMasterGetFirstClone';
  SEAudioMasterGetNextClone       : result := 'SEAudioMasterGetNextClone';
  SEAudioMasterGetTotalPinCount   : result := 'SEAudioMasterGetTotalPinCount';
  SEAudioMasterCallVstHost        : result := 'SEAudioMasterCallVstHost';
  SEAudioMasterResolveFilename    : result := 'SEAudioMasterResolveFilename';
  SEAudioMasterSendStringToGui    : result := 'SEAudioMasterSendStringToGui';
  SEAudioMasterGetModuleHandle    : result := 'SEAudioMasterGetModuleHandle';
  SEAudioMasterAddEvent           : result := 'SEAudioMasterAddEvent';
  SEAudioMasterCreateSharedLookup : result := 'SEAudioMasterCreateSharedLookup';
  SEAudioMasterSetPinOutputText   : result := 'SEAudioMasterSetPinOutputText';
  SEAudioMasterSetProcessFunction : result := 'SEAudioMasterSetProcessFunction';
  SEAudioMasterResolveFilename2   : result := 'SEAudioMasterResolveFilename2';
  SEAudioMasterGetSeVersion       : result := 'SEAudioMasterGetSeVersion';
 else result := 'Unknown opcode';
 end;
end;
{ Callbacks }

function SE2AudioMasterCallback(Effect: PSE2ModStructBase; Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;
begin
 result := 0;
 if (Effect = nil) or not (TObject(Effect.HostPtr) is TCustomSEHostedModulePart)
  then exit;

{
 with TCustomSEHostedModulePart(Effect.HostPtr) do
  case Opcode of
   SEAudioMasterSetPinStatus       : result := AMSetPinStatus;
   SEAudioMasterIsPinConnected     : result := AMIsPinConnected;
   SEAudioMasterGetPinInputText    : result := AMGetPinInputText;   // gets pointer to plugs input string (DT_TEXT only)
   SEAudioMasterGetSampleClock     : result := AMGetSampleClock;    // get current sampleclock at block start
   SEAudioMasterSendMIDI           : result := AMSendMIDI;          // send short MIDI msg
   SEAudioMasterGetInputPinCount   : result := AMGetInputPinCount;  // total AUDIO ins
   SEAudioMasterGetOutputPinCount  : result := AMGetOutputPinCount; // total AUDIO outs
   SEAudioMasterGetPinVarAddress   : result := AMGetPinVarAddress;
   SEAudioMasterGetBlockStartClock : result := AMGetBlockStartClock;
   SEAudioMasterGetTime            : result := AMGetTime;
   SEAudioMasterSleepMode          : result := AMSleepMode;
   SEAudioMasterGetRegisteredName  : result := AMGetRegisteredName; // limited to 50 characters or less
   (* EXAMPLE CALLING CODE
     name : array [0..49] of Char;
     CallHost(SEAudioMasterGetRegisteredName, 0, 0, @name[0]);
   *)
   SEAudioMasterGetFirstClone      : result := AMGetFirstClone;
   SEAudioMasterGetNextClone       : result := AMGetNextClone;
   (* EXAMPLE CALLING CODE

     procedure IterateThroughAllClones;
     var
       CloneStruct : PSE2ModStructBase;
       Clone       : PModule;
     begin
       // get first one
       CallHost(SEAudioMasterGetFirstClone, 0, 0, CloneStruct);

       while (clone_struct <> 0)
        begin
         // convert host's clone pointer to a 'Module' object
         Clone := PModule(CloneStruct.Object);

         // Access each clone here

         // step to Next clone
         Clone.CallHost(SEAudioMasterGetNextClone, 0, 0, CloneStruct);
        end;
     end;
   *)
   SEAudioMasterGetTotalPinCount   : result := AMGetTotalPinCount;   // Total pins of all types
   SEAudioMasterCallVstHost        : result := AMCallVstHost;        // Call VST Host direct (see se_call_vst_host_params struct)
   SEAudioMasterResolveFilename    : result := AMResolveFilename;    // get full path from a short filename, (int pin_idx, float max_characters, Char *destination)
   SEAudioMasterSendStringToGui    : result := AMSendStringToGui;    // Reserved for Experimental use (by Jef)
   SEAudioMasterGetModuleHandle    : result := AMGetModuleHandle;    // Reserved for Experimental use (by Jef)
   SEAudioMasterAddEvent           : result := AMAddEvent;           // pass SeEvent *, host will copy data from struct. Safe to discard after call.
   SEAudioMasterCreateSharedLookup : result := AMCreateSharedLookup;
   SEAudioMasterSetPinOutputText   : result := AMSetPinOutputText;   // sets plug's output string (DT_TEXT only)
   SEAudioMasterSetProcessFunction : result := AMSetProcessFunction; // sets the current SubProcess function
   SEAudioMasterResolveFilename2   : result := AMrResolveFilename2;  // get full path from a short filename - UNICODE
   (* EXAMPLE CALLING CODE
     uses windows;  //for WideCharToMultiByte

     // get the full path of an imbedded file when you only know it's short name
     const
       MAX_FILENAME_LENGTH : Integer = 300;

     // Both source and destination are UNICODE (two-byte) character strings
     unsigned short *source = L"test.txt";
     unsigned short dest[MAX_FILENAME_LENGTH];

     CallHost(SEAudioMasterResolveFilename2, Integer(source), MAX_FILENAME_LENGTH, &dest);

     // to convert to ascii (optional)
     Char ascii_filename[MAX_FILENAME_LENGTH];
     WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_filename, MAX_FILENAME_LENGTH, NULL, NULL);
   *)
   SEAudioMasterGetSeVersion       : result := AMHostVersion; // returns SE Version number times 100,000 ( e.g. 120000 is V 1.2 )
   else raise Exception.Create('Unknown Opcode');
  end;
}
end;

function SEGuiCallback(Effect: PSEGUIStructBase; Opcode: TSEGuiHostOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 result := 0;
 if (Effect = nil) or not (TObject(Effect.HostPtr) is TCustomSEHostedModulePart)
  then exit;

 with TCustomSEHostedModulePart(Effect.HostPtr) do
  case Opcode of
   seGuiHostRequestRepaint         : GuiHostRepaintRequest;
   seGuiHostGetHandle              : result := GuiHostGetHandle;
   seGuiHostSendStringToAudio      : result := 0;  // SendStringToAudio test
   seGuiHostSetWindowSize          : GuiHostSetWindowSize(Index, Value);
   seGuiHostSetWindowSizeable      : GuiHostSetWindowSizeable;
   seGuiHostGetTotalPinCount       : result := GuiHostGetTotalPinCount;
   seGuiHostPlugSetValText         : result := 0;
   seGuiHostPlugGetValText         : result := 0;
   seGuiHostAddGuiPlug             : GuiHostAddGuiPlug;
   seGuiRegisterPatchParameter     : result := 0; // Obsolete, use IO_PATCH_STORE or IO_UI_COMMUNICATION_DUAL flags instead. Will crash module on destruction (mayby need Unregister Opcode to fix this)
   seGuiHostGetFontInfo            : result := 0;
   seGuiHostSetWindowType          : result := 0; // pass 1 to provide your GuiModule with a 'real' HWND (else SE draws your module on the parent window)
   seGuiHostGetWindowHandle        : result := 0;
   (* example code... (WI is a SEWndInfo pointer )
     result := HWND(CallHost(seGuiHostGetWindowHandle, WI.context_handle));
   *)
   seGuiHostSetWindowFlags         : result := 0;
   seGuiHostPlugGetVal             : result := 0;
   seGuiHostPlugSetVal             : result := 0;
   seGuiHostPlugSetExtraData       : result := 0;  // sets enum list or file extension (depending on datatype)
   (* example code...
     // pass pin number and new list
     CallHost(seGuiHostPlugSetExtraData, 4, 0, 'moose, cat, dog', 0);
   *)
   seGuiHostPlugGetExtraData       : result := 0;  // gets enum list or file extension (depending on datatype). Easier to use SeGuiPin.getExtraData
   (* example code...
     var
       string_length : Integer;
       dest          : ^wchar_t;
       ascii_text    : PChar;
     begin
      string_length := CallHost(seGuiHostPlugGetExtraData, getIndex, 0, 0);

      // Destination is UNICODE (two-byte) character string
      dest := new wchar_t[string_length];

      CallHost(seGuiHostPlugGetExtraData, PN_ENUM_OUT, string_length, @dest);

      // to convert to ascii
      ascii_text := new char[string_length];
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);

      // clean up
      Dispose(dest);
      Dispose(ascii_text);
     end;
   *)
   seGuiHostSetCapture             : result := 0; // see SEGUI_base::SetCapture(...)
   seGuiHostReleaseCapture         : result := 0;
   seGuiHostGetCapture             : result := 0;
   seGuiHostCallVstHost            : result := 0; // pass se_call_vst_host_params structure in Ptr
   seGuiHostSetIdle                : result := 0; // pass 1 to receive regular calls to OnIdle(), pass zero to cancel
   seGuiHostGetModuleFilename      : result := 0; // returns full module path
   (* example code...
     const
       MAX_STRING_LENGTH : Integer = 300;
     var
       dest       : array [0..MAX_STRING_LENGTH-1] of ShortInt;
       ascii_text : array [0..MAX_STRING_LENGTH-1] of Char;
     begin
      // Destination is UNICODE (two-byte) character string
      CallHost(seGuiHostGetModuleFilename, 0, MAX_STRING_LENGTH, @dest);

      // to convert to ascii
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);
     end;
   *)
   seGuiHostResolveFilename     : GuiHostResolveFilename(Ptr, Value); // returns full module path
   (* example code...
     const
       MAX_STRING_LENGTH : Integer = 300;
     var
       dest       : array [0..MAX_STRING_LENGTH-1] of ShortInt;
       ascii_text : array [0..MAX_STRING_LENGTH-1] of Char;
     begin
      // Destination is UNICODE (two-byte) character string
      // convert filename to UNICODE
      MultiByteToWideChar(CP_ACP, 0, "test.wav", -1, LPWSTR(@dest), MAX_STRING_LENGTH);

      // query full filename (SE concatenates default path for that type of file, depending on file extension)
      CallHost(seGuiHostResolveFilename, 0, MAX_STRING_LENGTH, @dest);

      // to convert to ascii
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);
     end;
   *)
   seGuiHostGetHostType            : result := 0; // return code 0 = unsuported, 1 = module is running in SynthEdit, 2 = Module is in a VST plugin (made with SE)
   seGuiHostRemoveGuiPlug          : result := 0;
   seGuiHostGetParentContext       : result := 0; // Get 'handle' of parent window.  This is an SE handle, not an HWND. Use seGuiHostGetWindowHandle to convert.
   seGuiHostMapWindowPoints        : result := 0; // map a point on one window to the co-ordinate system of a 2nd window
   (*
     var
       parent_context : Integer;
       h              : HWND;
     begin
       // Example: getting parent HWND, and your position relative to it
       parent_context := WI.context_handle;
       h := 0;
       while h = 0 do
        begin
         parent_context = CallHost(seGuiHostGetParentContext, parent_context);
         h := HWND(CallHost(seGuiHostGetWindowHandle, parent_context));
        end;

       sepoint offset(0,0);
       CallHost(seGuiHostMapWindowPoints, WI.context_handle, parent_context, @offset, 0);
     end;
   *)
   seGuiHostMapClientPointToScreen : result := 0; // maps a point on your gui to the system screen (absolute co-ords)
   (*
     // Example: converting a point on your GUI to an absolute co-ordinate. Useful for pop-up menus
     var
       offset : TSEPoint;
     begin
       offset.x := 0;
       offset.y := 0;
       CallHost(seGuiHostMapClientPointToScreen, WI.context_handle, 0, @offset, 0);
     end;
   *)                              
   seGuiHostInvalidateRect         : result := 0; // invlalidate (cause redraw) of any SE window
   (*
     var
       n: TRect;
     begin
       n.top = 0;
       n.bottom = 1;
       n.left = 2;
       n.right = 20;
       CallHost(seGuiHostInvalidateRect, WI.context_handle, 0, @n, 0);
     end;
   *)                              
   seGuiHostIsGraphInitialsed      : result := 0; // test if pin updates are due to file loading, or from user.
   else raise Exception.Create('Unknown Opcode');
  end; 
end;

{ TCustomSEHostedModulePart }

constructor TCustomSEHostedModulePart.Create(Owner: TCustomSEHostedModule;
  Index: Integer = 0; Properties: PSEModuleProperties = nil);
begin
 FIndex := Index;
 FSE2ModStructBase := nil;
 FSEGUIStructBase := nil;
 FSEHostedModule := Owner;
 if assigned(Properties)
  then FProperties := Properties^
  else FillChar(FProperties, SizeOf(TSEModuleProperties), 0);
end;

function TCustomSEHostedModulePart.GetAbout: string;
begin
 result := Properties.About;
end;

function TCustomSEHostedModulePart.GetActive: Boolean;
begin
 result := FSE2ModStructBase <> nil;
end;

function TCustomSEHostedModulePart.GetID: string;
begin
 result := Properties.ID;
end;

function TCustomSEHostedModulePart.GetMagic: Integer;
begin
 result := FSE2ModStructBase.Magic;
end;

function TCustomSEHostedModulePart.GetName: string;
begin
 result := Properties.Name;
end;

function TCustomSEHostedModulePart.GetPinProperties(Index: Integer;
  var Pin: TSEPinProperties): Boolean;
begin
 if Active
  then result := (CallPlugin(seffGetPinProperties, Index, 0, @Pin) <> 0)
  else result := False
end;

function TCustomSEHostedModulePart.GetVersion: Integer;
begin
 result := FSE2ModStructBase.Version;
end;

procedure TCustomSEHostedModulePart.GuiHostAddGuiPlug;
begin
 // do nothing yet;
end;

function TCustomSEHostedModulePart.GuiHostGetHandle: THandle;
begin
 result := 0;
end;

function TCustomSEHostedModulePart.GuiHostGetTotalPinCount: Integer;
begin
 result := 0; // ToDo
end;

procedure TCustomSEHostedModulePart.GuiHostRepaintRequest;
begin
 if assigned(FOnRepaintRequest)
  then FOnRepaintRequest(Self);
end;

procedure TCustomSEHostedModulePart.GuiHostResolveFilename(FileName: PWideChar;
  MaxStringLength: Integer);
begin
 // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiHostSetWindowSize(const x, y: Integer);
begin
 // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiHostSetWindowSizeable;
begin
 // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiNotify(Value: Integer = 0; Index: Integer = 0; Ptr: Pointer = nil);
begin
 if Active
  then CallPlugin(seffGuiNotify, Index, Value, Ptr);
end;

procedure TCustomSEHostedModulePart.Instanciate;
begin
 try
  DisposeStructures;
  FSE2ModStructBase := FSEHostedModule.FMakeModule(FIndex, 1, @SE2AudioMasterCallback, Self);
  FSEGUIStructBase  := FSEHostedModule.FMakeModule(FIndex, 2, @SEGuiCallback, Self); // nasty
 except
  FSE2ModStructBase := nil;
 end;
end;

procedure TCustomSEHostedModulePart.DisposeStructures;
begin
 if assigned(FSE2ModStructBase) then
  begin
   Dispose(FSE2ModStructBase);
   FSE2ModStructBase := nil;
  end;
 if assigned(FSEGUIStructBase) then
  begin
   Dispose(FSEGUIStructBase);
   FSEGUIStructBase := nil;
  end;
end;

function TCustomSEHostedModulePart.IsEventListEmpty: Boolean;
begin
 if Properties.SdkVersion >= 2000
  then raise Exception.Create('not used in SDK2')
  else
   if Active
    then result := CallPlugin(seffIsEventListEmpty) <> 0
    else result := False;
end;

procedure TCustomSEHostedModulePart.Open;
begin
 if Active
  then CallPlugin(seffOpen);
end;

function TCustomSEHostedModulePart.QueryDebugInfo: Pointer;
begin
 if Active
  then result := Pointer(CallPlugin(seffQueryDebugInfo))
  else result := nil;
end;

procedure TCustomSEHostedModulePart.Resume(Index: Integer = 0);
begin
 if Active
  then CallPlugin(seffResume, Index);
end;

procedure TCustomSEHostedModulePart.AddEvent(Event: TSEEvent);
begin
 if Properties.SdkVersion >= 2000
  then raise Exception.Create('not used in SDK2')
  else
   if Active
    then CallPlugin(seffAddEvent, 0, 0, @Event);
end;

function TCustomSEHostedModulePart.CallPlugin(Opcode: TSEPluginModuleOpcodes;
  Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
begin
 try
  if Active
   then result := FSE2ModStructBase.Dispatcher(FSE2ModStructBase, Opcode, Index, Value, Ptr, Opt)
   else raise Exception.Create('SE module part not opened yet');
 except
  result := 0;
 end;
end;

procedure TCustomSEHostedModulePart.Close;
begin
 try
  if Active then CallPlugin(seffClose);
 finally
  FSE2ModStructBase := nil;
 end;
end;

procedure TCustomSEHostedModulePart.SetActive(const Value: Boolean);
begin
 if Active <> Value then
  if Value
   then Open
   else Close;
end;

procedure TCustomSEHostedModulePart.SetBlockSize(Value: Integer);
begin
 if Active
  then CallPlugin(seffSetBlockSize, 0, Value);
end;

procedure TCustomSEHostedModulePart.SetSampleRate(Value: Single);
begin
 if Active
  then CallPlugin(seffSetSampleRate, 0, 0, nil, Value);
end;

{ TCustomSEHostedModule }

procedure TCustomSEHostedModule.CloseParts;
var
  Module : Integer;
begin
 for Module := 0 to Length(FParts) - 1 do
  try
   FParts[Module].Close;
  finally
   FreeAndNil(FParts[Module]);
  end;
end;

constructor TCustomSEHostedModule.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := inherited GetDisplayName;
 InitializeVariables;
end;

destructor TCustomSEHostedModule.Destroy;
begin
 Unload;
 inherited;
end;

procedure TCustomSEHostedModule.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSEHostedModule then
  with TCustomSEHostedModule(Dest) do
   begin
    DisplayName  := Self.DisplayName;
    FSEMFileName := Self.FSEMFileName;
   end else inherited;
end;

function TCustomSEHostedModule.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TCustomSEHostedModule.GetPart(index: Integer): TCustomSEHostedModulePart;
begin
 if (Index >= 0) and (Index < Length(FParts))
  then result := FParts[Index]
  else result := nil;
end;

function TCustomSEHostedModule.GetPartCount: Integer;
begin
 result := Length(FParts);
end;

procedure TCustomSEHostedModule.LoadFromFile(FileName: TFilename);
{$IFNDEF FPC}
var
  Buf : array[0..255] of Char;
  LE  : Integer;
  Str : string;
{$ENDIF}
begin
 if not FileExists(FileName)
  then raise Exception.CreateFmt(RStrFileDoesNotExist, [FileName]);

 if FLoaded
  then Unload;

 FSEMFileName := FileName;

 try
  FSEModuleHandle := SafeLoadLibrary(PChar(FileName), 7);
  if FSEModuleHandle = 0 then
   begin
    {$IFNDEF FPC}
    LE := GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LE, 0, @Buf[0], SizeOf(Buf), nil);
    if Buf = '' then
     begin
      Str := IntToStr(LE) + StrPas(Buf);
      raise Exception.Create(str);
     end else raise Exception.Create(StrPas(Buf));
    {$ENDIF}
   end
  else // GetProcAddresses
   begin
    FGetModuleProperties := GetProcAddress(FSEModuleHandle, 'getModuleProperties');
    FMakeModule := GetProcAddress(FSEModuleHandle, 'makeModule');

    if not assigned(FGetModuleProperties)
     then raise Exception.Create(RStrNoEntryPoint);
    if not assigned(FMakeModule)
     then raise Exception.Create(RStrNoEntryPoint);
    ListParts;
   end;
 except
  Unload;
 end;
end;

procedure TCustomSEHostedModule.ListParts;
var
  ModuleProperties : TSEModuleProperties;
  Module           : Integer;
begin
 CloseParts;
 Module := 0;
 SetLength(FParts, 0);
 FillChar(ModuleProperties, SizeOf(TSEModuleProperties), 0);
 while FGetModuleProperties(Module, @ModuleProperties) do
  begin
   SetLength(FParts, Module + 1);
   FParts[Module] := TCustomSEHostedModulePart.Create(Self, Module, @ModuleProperties);
   inc(Module); FillChar(ModuleProperties, SizeOf(TSEModuleProperties), 0);
  end;
end;

procedure TCustomSEHostedModule.LoadFromStream(Stream: TStream);
begin
 if FLoaded
  then Unload;

 if not assigned(FInternalDLLLoader)
  then FInternalDLLLoader := TDLLLoader.Create;
 try
  FInternalDLLLoader.Load(Stream);
  FGetModuleProperties := FInternalDLLLoader.FindExport('getModuleProperties');
  FMakeModule := FInternalDLLLoader.FindExport('makeModule');

  if not assigned(FGetModuleProperties)
   then raise Exception.Create(RStrNoEntryPoint);
  if not assigned(FMakeModule)
   then raise Exception.Create(RStrNoEntryPoint);
 except
  Unload;
 end;
end;

procedure TCustomSEHostedModule.SetSEMFileName(const Value: TFileName);
begin
 if FSEMFileName <> Value then
  if FileExists(Value)
   then LoadFromFile(Value) else
  if not assigned(FInternalDLLLoader)
   then Unload;
end;

procedure TCustomSEHostedModule.UnLoad;
begin
 CloseParts;

 if FSEModuleHandle > 0 then
  try
   FreeLibrary(FSEModuleHandle);
  finally
   FSEModuleHandle := 0;
  end;
 if assigned(FInternalDLLLoader) then
  begin
   FInternalDLLLoader.Unload;
   FreeAndNil(FInternalDLLLoader);
  end;
 InitializeVariables;
end;

procedure TCustomSEHostedModule.InitializeVariables;
begin
 FSEMFileName         := '';
 FGetModuleProperties := nil;
 FMakeModule          := nil;
end;

{ TSEHostedModules }

constructor TSEHostedModules.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TSEHostedModule);
 FOwner := AOwner;
end;

function TSEHostedModules.Add: TSEHostedModule;
begin
 Result := TSEHostedModule(inherited Add);
end;

function TSEHostedModules.CloneAdd(Source: TSEHostedModule): TSEHostedModule;
begin
 Result := TSEHostedModule(inherited Add);
 Source.AssignTo(Result);
end;

procedure TSEHostedModules.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TSEHostedModules.GetItem(Index: Integer): TSEHostedModule;
begin
 Result := TSEHostedModule(inherited GetItem(Index));
end;

function TSEHostedModules.GetVSTHost: TCustomSEHost;
begin
 Result := TCustomSEHost(FOwner);
end;

function TSEHostedModules.Insert(Index: Integer): TSEHostedModule;
begin
 Result := TSEHostedModule(inherited Insert(Index));
end;

procedure TSEHostedModules.SetItem(Index: Integer; const Value: TSEHostedModule);
begin
 inherited SetItem(Index, Value);
end;

{ TCustomSEHost }

constructor TCustomSEHost.Create(AOwner: TComponent);
begin
 inherited;
 FSampleRate := 44100;
 FBlocksize  := 2048;
 FSEHostedModules := TSEHostedModules.Create(Self);
 if Assigned(FOnCreate) then FOnCreate(Self);
end;

destructor TCustomSEHost.Destroy;
begin
 if Assigned(FOnDestroy) then FOnDestroy(Self);
 FreeAndNil(FSEHostedModules);
 inherited;
end;

function TCustomSEHost.GetItem(Index: Integer): TCustomSEHostedModule;
begin
 assert(assigned(FSEHostedModules));
 Result := FSEHostedModules[Index];
end;

procedure TCustomSEHost.SetHostedModules(const Value: TSEHostedModules);
begin
 assert(assigned(FSEHostedModules));
 FSEHostedModules.Assign(Value);
end;

end.
