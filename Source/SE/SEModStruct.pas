unit SEModStruct;

interface

uses
  DAV_VSTEffect, SEDataTypes, SEModStructBase;

type
  PSEEvent = ^TSEEvent;
  TSEEvent = record // a generic timestamped event
  public
    time_stamp : Cardinal;
    event_type : TUGEventType;
    int_parm1  : Integer;
    int_parm2  : Integer;
    ptr_parm1  : Pointer;
    next       : PSEEvent; // next in list (not used)
    constructor Create(p_time_stamp: Cardinal; p_event_type: TUGEventType;
      p_int_parm1, p_int_parm2: Integer; p_ptr_parm1: Pointer);
  end;

const
  // module flags, indicate special abilities
  UGF_VOICE_MON_IGNORE = $0002;     { DON'T WANT VOICE HELD OPEN BECAUSE A SCOPE IS CONNECTED }
  UGF_POLYPHONIC_AGREGATOR = $0040; { A ug that always combines voices }

  // read only
  UGF_SUSPENDED = $0080;
  UGF_OPEN = $0100;
  UGF_CLONE = $0800;
  UGF_SEND_TIMEINFO_TO_HOST = $20000;

  // normally when a voice has faded out, SE shuts all that voice's modules off
  // in very special cases you can prevent SE from shutting off your module
  UGF_NEVER_SUSPEND = $0200;

  // visible on control panel (applys to GuiFlags member)
  CF_CONTROL_VIEW = 128;
  CF_STRUCTURE_VIEW = 256;

type
  PSEModuleProperties = ^TSEModuleProperties;
  TSEModuleProperties = record
    Name       : PChar;
    ID         : PChar;
    About      : PChar;
    Flags      : Integer;
    GuiFlags   : Integer;
    SdkVersion : Integer;
  end;

  PSEPinProperties = ^TSEPinProperties;
  TSEPinProperties = record
    VariableAddress  : Pointer;
    Direction        : TDirection;
    Datatype         : TPlugDataType;
    Name             : PChar;
    DefaultValue     : PChar;
    DatatypeExtra    : PChar;
    Flags            : Integer;
    Spare            : Integer;
  end;

implementation

{ TSEEvent }

constructor TSEEvent.Create(p_time_stamp: Cardinal; p_event_type: TUGEventType;
  p_int_parm1, p_int_parm2: Integer; p_ptr_parm1: Pointer);
begin
 time_stamp := p_time_stamp;
 event_type := p_event_type;
 int_parm1  := p_int_parm1;
 int_parm2  := p_int_parm2;
 ptr_parm1  := p_ptr_parm1;
 next       := 0;
end;

end.
