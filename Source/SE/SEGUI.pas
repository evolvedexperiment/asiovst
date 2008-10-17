unit SEGUI;

interface

uses
  Windows, Classes, SECommon;

type
  PSEGUIStructBase = ^TSEGUIStructBase;
  TSEGUIBase = class;

  TSEGuiCallback = function(Effect: PSEGUIStructBase; Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSEGuiDispatcher = function(Effect: PSEGUIStructBase; Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;

  TSEGUIStructBase = record
    Magic      : Integer;              // magic number
    Version    : Integer;
    Dispatcher : TSEGuiDispatcher;
    HostPtr    : Pointer;              // reserved for host use, must be 0
    SEGUIBase  : TSEGUIBase;           // for class access
    User       : Pointer;              // user access
    Future     : array[0..15] of Char; // pls zero
  end;

  /////////////////////////////////
  // Plugin Module opCodes (GUI) //
  /////////////////////////////////

  TSEGuiPluginOpcodes = (
    seGuiInitialise = 0,       // initialise
    seGuiClose,                // exit, release all memory and other resources!
    seGuiPaint,
    seGuiLButtonDown,
    seGuiLButtonUp,
    seGuiMouseMove,
    seGuiOnModuleMessage,      // OnModuleMsg test
    seGuiOnGuiPlugValueChange,
    seGuiOnWindowOpen,
    seGuiOnWindowClose,
    seGuiOnIdle,
    seGuiOnNewConnection,
    seGuiOnDisconnect);

  ////////////////////////
  // Host opCodes (GUI) //
  ////////////////////////

  TSEGuiHostOpcodes = (
    seGuiHostRequestRepaint = 0,  //
    seGuiHostGetHandle,
    seGuiHostSendStringToAudio,   // SendStringToAudio test
    seGuiHostSetWindowSize,
    seGuiHostSetWindowSizeable,
    seGuiHostGetTotalPinCount,
    seGuiHostPlugSetValText,
    seGuiHostPlugGetValText,
    seGuiHostAddGuiPlug,
    seGuiRegisterPatchParameter,  // Obsolete, use IO_PATCH_STORE or IO_UI_COMMUNICATION_DUAL flags instead. Will crash module on destruction (mayby need Unregister Opcode to fix this)
    seGuiHostGetFontInfo,
    seGuiHostSetWindowType,       // pass 1 to provide your GuiModule with a 'real' HWND (else SE draws your module on the parent window)
    seGuiHostGetWindowHandle,
    (* example code... (WI is a SEWndInfo pointer )
      result := HWND(CallHost(seGuiHostGetWindowHandle, WI.context_handle));
    *)
    seGuiHostSetWindowFlags,
    seGuiHostPlugGetVal,
    seGuiHostPlugSetVal,
    seGuiHostPlugSetExtraData,    // sets enum list or file extension (depending on datatype)
    (* example code...
      // pass pin number and new list
      CallHost(seGuiHostPlugSetExtraData, 4, 0, 'moose, cat, dog', 0);
    *)
    seGuiHostPlugGetExtraData,    // gets enum list or file extension (depending on datatype). Easier to use SeGuiPin.getExtraData
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
    seGuiHostSetCapture,          // see SEGUI_base::SetCapture(...)
    seGuiHostReleaseCapture,
    seGuiHostGetCapture,
    seGuiHostCallVstHost,         // pass se_call_vst_host_params structure in Ptr
    seGuiHostSetIdle,             // pass 1 to receive regular calls to OnIdle(), pass zero to cancel
    seGuiHostGetModuleFilename,   // returns full module path
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
    seGuiHostResolveFilename, // returns full module path
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
    seGuiHostGetHostType, // return code 0 =unsuported, 1=module is running in SynthEdit, 2= Module is in a VST plugin (made with SE)
    seGuiHostRemoveGuiPlug,
    seGuiHostGetParentContext, // Get 'handle' of parent window.  This is an SE handle, not an HWND.  Use seGuiHostGetWindowHandle to convert.
    seGuiHostMapWindowPoints, // map a point on one window to the co-ordinate system of a 2nd window
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
    seGuiHostMapClientPointToScreen, // maps a point on your gui to the system screen (absolute co-ords)
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
    seGuiHostInvalidateRect, // invlalidate (cause redraw) of any SE window
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
    seGuiHostIsGraphInitialsed); // test if pin updates are due to file loading, or from user.


  TSEHostWindowFlags = (HWF_RESIZEABLE = 1, HWF_NO_CUSTOM_GFX_ON_STRUCTURE = 2);

  // painting info
  TSEpoint = TPoint;

  PSEWndInfo = ^TSEWndInfo;
  TSEWndInfo = record
    Width         : Integer;
    Height        : Integer;
    ContextHandle : THandle;
  end;


  TSEFontInfo = record
    Size            : Integer;
    Color           : Integer;
    ColorBackground : Integer;
    Flags           : Integer; // alignment etc
    FontHeight      : Integer;
//    Category        : array[0..19] of Char;
//    Facename        : array[0..49] of Char;
    Future          : array[0..99] of Char;
  end;

  TSEGuiPin = class(TObject)
  private
    FIndex  : Integer;
    FModule : TSEGUIBase;
  public
    procedure Init(AIndex: Integer; AModule: TSEGUIBase);
//    procedure setValueText(const SeSdkString &Value);
//    function GetValueText: SeSdkString;
    function GetValueInt: Integer; // int, bool, and list type values
    procedure SetValueInt(Value: Integer);
    function GetValueFloat: Single;
    procedure SetValueFloat(Value: Single);
//    function GetExtraData: SeSdkString2;
    function GetModule: TSEGUIBase;
    function GetIndex: Integer;
  end;

  TSEGUIBase = class(TObject)
  private
//    FPins: array of TSeGuiPin;
    procedure SetupPins;
  protected
    FAudioMaster : TSEGuiCallback;
    FEffect      : TSEGUIStructBase;
  public
// friend Integer dispatchEffectClass(SEGUI_struct_base *e, Integer Opcode, Integer Index, Integer Value, void *Ptr, float Opt);
    Magic         : Integer;
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
    destructor Destroy; override;
    function GetAeffect: PSEGUIStructBase;

    // called from audio master
    function CallHost(Opcode: TSEGuiHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
    function Dispatcher(Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; virtual;
    function GetCapture(WI: PSEWndInfo): Boolean;
    function GetPin(Index: Integer): TSEGuiPin; //{ return &m_pins[Index];}
    function OnIdle: Boolean; {return false;} virtual;
    procedure AddGuiPlug(p_datatype: TSEPlugDataType; p_direction: TSEDirection; const p_name: PChar);
    procedure Close; virtual;
    procedure Initialise(LoadedFromFile: Boolean); virtual;
    procedure Paint(hDC: HDC; WI: PSEWndInfo); virtual;
    procedure ReleaseCapture(WI: PSEWndInfo);
    procedure SetCapture(WI: PSEWndInfo);
    procedure OnDisconnect(PinIndex: Integer); virtual;
    procedure OnGuiPinValueChange(Pin: TSeGuiPin); virtual;
    procedure OnLButtonDown(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure OnLButtonUp(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure OnModuleMsg(UserMsg_id: Integer; MsgLength: Integer; MsgData: Pointer); virtual;
    procedure OnMouseMove(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure OnNewConnection(PinIndex: Integer); virtual;
    procedure OnWindowClose(WI: PSEWndInfo); virtual;
    procedure OnWindowOpen(WI: PSEWndInfo); virtual;
  end;

implementation

(*
procedure TSeGuiPin.setValueText(const Value: PSeSdkString);
begin
 getModule.CallHost(seGuiHostPlugSetValText, getIndex, 0, Value.data);
end;

function TSeGuiPin.getValueText: SeSdkString;
begin
 // warning, unstable over 2000 bytes  ( that's 1000 UNICODE characters )
 result := getModule.CallHost(seGuiHostPlugGetValText, getIndex, 0, 0);
end;
*)

function TSeGuiPin.getValueInt: Integer; // int, bool, and list type values
begin
 getModule.CallHost(seGuiHostPlugGetVal, getIndex, 0, @result);
end;

function TSEGuiPin.GetIndex: Integer;
begin
 result := FIndex;
end;

function TSEGuiPin.GetModule: TSEGUIBase;
begin
 result := FModule;
end;

procedure TSEGuiPin.Init(AIndex: Integer; AModule: TSEGUIBase);
begin
{
 m_index  := p_index;
 m_module :=p_module;
}
end;

function TSeGuiPin.getValueFloat: Single;
begin
 getModule.CallHost(seGuiHostPlugGetVal, getIndex, 0, @result);
end;

procedure TSeGuiPin.setValueFloat(Value: Single);
begin
 getModule.CallHost(seGuiHostPlugSetVal, getIndex, 0, nil, Value);
end;

procedure TSeGuiPin.setValueInt(Value: Integer);
begin
 getModule.CallHost(seGuiHostPlugSetVal, getIndex, Value, nil);
end;

(*
function TSeGuiPin.getExtraData: SeSdkString2;
{
ToDo
var
  StringLength : Integer;
  temp         : Pwchar_t;
}
begin
{
 StringLength := getModule.CallHost(seGuiHostPlugGetExtraData, getIndex, 0, 0);
   *temp = new wchar_t[string_length];

  getModule.CallHost(seGuiHostPlugGetExtraData, getIndex(), string_length, temp );
  SeSdkString2 temp2(temp);
  delete [] temp;
  return temp2;
}
end;
*)

function dispatchEffectClass(Effect: PSEGUIStructBase; Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
begin
 if assigned(Effect) then
  begin
   assert(assigned(Effect.SEGUIBase));
   result := Effect.SEGUIBase.dispatcher(TSEGuiPluginOpcodes(Opcode), Index, Value, Ptr, Opt);
  end;
end;

constructor TSEGUIBase.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 FAudioMaster := SEGuiCallback;
 FillChar(FEffect, SizeOf(TSEGUIStructBase), 0);
 with FEffect do
  begin
   Magic      := SepMagic2;
   Dispatcher := @dispatchEffectClass;
   SEGUIBase  := Self;
   HostPtr    := AHostPtr;
   Version    := 1;
  end;
end;

destructor TSEGUIBase.Destroy;
begin
 // do nothing yet
end;

function TSEGUIBase.dispatcher(Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
var
  pnt : TSEPoint;
  pin : TSeGuiPin;
begin
 result := 0;
 case Opcode of
  seGuiInitialise: Initialise(Index = 1);
  seGuiClose:
   begin
    Close;
    Free;
    result := 1; // this object now deleted, must do nothing more.
   end;
  seGuiPaint: Paint(HDC(Index), PSEWndInfo(Ptr));
  seGuiLButtonDown:
   begin
    pnt := Point(Index, Value);
    OnLButtonDown(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiLButtonUp:
   begin
    pnt := Point(Index, Value);
    OnLButtonUp(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiMouseMove:
   begin
    pnt := Point(Index, Value);
    OnMouseMove(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiOnModuleMessage: OnModuleMsg(Value, Index, Ptr);
  seGuiOnGuiPlugValueChange:
   begin
    pin.Init(Index, self);
    OnGuiPinValueChange(@pin);
   end;
  seGuiOnWindowOpen    : OnWindowOpen(PSEWndInfo(Ptr));
  seGuiOnWindowClose   : OnWindowClose(PSEWndInfo(Ptr));
  seGuiOnIdle          : result := Integer(OnIdle);
  seGuiOnNewConnection : OnNewConnection(Index);
  seGuiOnDisconnect    : OnDisconnect(Index);
 end;
end;

function TSEGUIBase.CallHost(Opcode: TSEGuiHostOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
begin
 assert(assigned(FAudioMaster));
 result := FAudioMaster(@FEffect, Integer(Opcode), Index, Value, Ptr, Opt);
end;

procedure TSEGUIBase.Close;
begin
 // do nothing yet
end;

procedure TSEGUIBase.Initialise(LoadedFromFile: Boolean);
begin
 SetupPins;
end;

procedure TSEGUIBase.OnDisconnect(PinIndex: Integer);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnGuiPinValueChange(Pin: TSeGuiPin);
begin
 // do nothing yet
end;

function TSEGUIBase.OnIdle: Boolean;
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnLButtonDown(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnLButtonUp(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnModuleMsg(UserMsg_id, MsgLength: Integer; MsgData: Pointer);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnMouseMove(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnNewConnection(PinIndex: Integer);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnWindowClose(WI: PSEWndInfo);
begin
 // do nothing yet
end;

procedure TSEGUIBase.OnWindowOpen(WI: PSEWndInfo);
begin
 // do nothing yet
end;

procedure TSEGUIBase.Paint(hDC: HDC; WI: PSEWndInfo);
begin
 // do nothing yet
end;

procedure TSEGUIBase.AddGuiPlug(p_datatype: TSEPlugDataType; p_direction: TSEDirection; const p_name: Pchar);
begin
  CallHost(seGuiHostAddGuiPlug, Integer(p_datatype), Integer(p_direction), p_name);
  SetupPins;
end;

procedure TSEGUIBase.SetupPins;
var
  i, ActualPlugCount: Integer;
begin
 // commented out, may need reinstating if pins ever gain state
(*
 // get actual number of pins used (may be more or less if auto-duplicating plugs used)
 ActualPlugCount := CallHost(seGuiHostGetTotalPinCount);

 FPins.Resize(ActualPlugCount);

 for i := 0 to ActualPlugCount - 1
  do FPins[i].Init(i, self);
*)
end;

function TSEGUIBase.getPin(Index: Integer): TSeGuiPin;
var
  pin : TSeGuiPin; // static
begin
  // there are no pins.
  // pins currently hold no state, implement them as a flyweight (saves having to track pin add/remove, we're not notified of autoduplicate add/remove anyhow)
  pin.Init(Index, Self);
  result := @pin;
end;

// capture mouse movement
procedure TSEGUIBase.SetCapture(WI: PSEWndInfo);
begin
  CallHost(seGuiHostSetCapture, 0, 0, WI);
end;

// release capture mouse movement
procedure TSEGUIBase.ReleaseCapture(WI: PSEWndInfo);
begin
  CallHost(seGuiHostReleaseCapture, 0, 0, WI);
end;

// query mouse capture state
function TSEGUIBase.GetAeffect: PSEGUIStructBase;
begin
 result := @FEffect;
end;

function TSEGUIBase.GetCapture(WI: PSEWndInfo): Boolean;
begin
  result := CallHost(seGuiHostGetCapture, 0, 0, WI) <> 0;
end;

end.
