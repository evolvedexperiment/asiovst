unit SEButterworthModule;

interface

uses
  DAV_Common, DAV_DSPFilterButterworth, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEButterworthPins = (pinInput, pinOutput, pinFrequency, pinOrder);

  TSEButterworthModule = class(TSEModuleBase)
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TCustomButterworthFilter;
    FStaticCount  : Integer;
    FFrequency    : Single;
    FOrder        : Integer;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    class function GetFilterClass: TCustomButterworthFilterClass; virtual; abstract;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSEStaticButterworthLPModule = class(TSEButterworthModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function GetFilterClass: TCustomButterworthFilterClass; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEStaticButterworthHPModule = class(TSEButterworthModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function GetFilterClass: TCustomButterworthFilterClass; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEControlableButterworthLPModule = class(TSEStaticButterworthLPModule)
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEControlableButterworthHPModule = class(TSEStaticButterworthHPModule)
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEAutomatableButterworthLPModule = class(TSEStaticButterworthLPModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatableButterworthHPModule = class(TSEStaticButterworthHPModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatableXButterworthLPModule = class(TSEStaticButterworthLPModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
    class function GetFilterClass: TCustomButterworthFilterClass; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatableXButterworthHPModule = class(TSEStaticButterworthHPModule)
  protected
    FFreqBuffer : PDAVSingleFixedArray;
    class function GetFilterClass: TCustomButterworthFilterClass; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

implementation

uses
  SysUtils, DAV_DspFilter;

constructor TSEButterworthModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 FFrequency := 1000;
 FOrder := 4;

 FFilter := GetFilterClass.Create;
 FFilter.SetFilterValues(FFrequency, 0);
 FFilter.Order := FOrder;
end;

destructor TSEButterworthModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSEButterworthModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEButterworthModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

// The most important part, processing the audio
procedure TSEButterworthModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

procedure TSEButterworthModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEButterworthModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;


// describe your module
class procedure TSEButterworthModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEButterworthModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEButterworthPins(index) of
  // typical input plug (inputs are listed first)
  pinInput:
    with Properties^ do
     begin
      Name            := 'Input';
      VariableAddress := @FInputBuffer;
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;

  // typical output plug
  pinOutput:
    with Properties^ do
     begin
      Name            := 'Output';
      VariableAddress := @FOutputBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

{ TSEButterworthModule }

class function TSEStaticButterworthLPModule.GetFilterClass: TCustomButterworthFilterClass;
begin
 result := TButterworthLowpassFilter;
end;

class procedure TSEStaticButterworthLPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Lowpass (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Lowpass (static)';
  end;
end;

function TSEStaticButterworthLPModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(index) of
  pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency';
      VariableAddress := @FFrequency;
      Direction       := drParameter;
      DataType        := dtSingle;
      Flags           := [iofLinearInput];
      DefaultValue    := '1000';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Name            := 'Order';
      VariableAddress := @FOrder;
      Direction       := drParameter;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      DefaultValue    := '4';
      DatatypeExtra   := 'range -0,64';
      result          := True;
     end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticButterworthLPModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEButterworthPins(CurrentPin.PinID) of
      pinInput : begin
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                  ChooseProcess;
                 end;
  pinFrequency : FFilter.Frequency := 1E-5 + abs(FFrequency);
      pinOrder : FFilter.Order     := FOrder;
 end;
 inherited;
end;

{ TSEStaticButterworthHPModule }

class function TSEStaticButterworthHPModule.GetFilterClass: TCustomButterworthFilterClass;
begin
 result := TButterworthHighpassFilter;
end;

class procedure TSEStaticButterworthHPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Highpass (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Highpass (static)';
  end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticButterworthHPModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEButterworthPins(CurrentPin.PinID) of
      pinInput : begin
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                  ChooseProcess;
                 end;
  pinFrequency : FFilter.Frequency := 1E-5 + abs(FFrequency);
      pinOrder : FFilter.Order     := FOrder;
 end;
 inherited;
end;

function TSEStaticButterworthHPModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(index) of
  pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency';
      VariableAddress := @FFrequency;
      Direction       := drParameter;
      DataType        := dtSingle;
      Flags           := [iofLinearInput];
      DefaultValue    := '1000';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Name            := 'Order';
      VariableAddress := @FOrder;
      Direction       := drParameter;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      DefaultValue    := '4';
      DatatypeExtra   := 'range -0,64';
      result          := True;
     end;
 end;
end;

{ TSEControlableButterworthLPModule }

function TSEControlableButterworthLPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if index in [2..3] then Properties^.Direction := drIn;
end;

class procedure TSEControlableButterworthLPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Lowpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Lowpass';
  end;
end;

{ TSEControlableButterworthHPModule }

function TSEControlableButterworthHPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if index in [2..3] then Properties^.Direction := drIn;
end;

class procedure TSEControlableButterworthHPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Highpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Highpass';
  end;
end;

{ TSEAutomatableButterworthLPModule }

class procedure TSEAutomatableButterworthLPModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Lowpass (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Lowpass (automatable)';
  end;
end;

function TSEAutomatableButterworthLPModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      Datatype        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '5';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Direction       := drIn;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      result          := True;
     end;
 end;
end;

procedure TSEAutomatableButterworthLPModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatableButterworthHPModule }

class procedure TSEAutomatableButterworthHPModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Highpass (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Highpass (automatable)';
  end;
end;

function TSEAutomatableButterworthHPModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      Flags           := [iofLinearInput];
      Datatype        := dtFSample;
      DefaultValue    := '5';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      VariableAddress := @FOrder;
      Direction       := drIn;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      result          := True;
     end;
 end;
end;

procedure TSEAutomatableButterworthHPModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatableXButterworthLPModule }

class function TSEAutomatableXButterworthLPModule.GetFilterClass: TCustomButterworthFilterClass;
begin
 result := TButterworthLowPassFilterAutomatable;
end;

class procedure TSEAutomatableXButterworthLPModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Lowpass (automatable+)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Lowpass (automatable+)';
  end;
end;

function TSEAutomatableXButterworthLPModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      Datatype        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '5';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      Direction       := drIn;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      result          := True;
     end;
 end;
end;

procedure TSEAutomatableXButterworthLPModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   if Sample mod 2 = 0
    then FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatableXButterworthHPModule }

class function TSEAutomatableXButterworthHPModule.GetFilterClass: TCustomButterworthFilterClass;
begin
 result := TButterworthHighPassFilterAutomatable;
end;

class procedure TSEAutomatableXButterworthHPModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Butterworth Highpass (automatable+)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Butterworth Highpass (automatable+)';
  end;
end;

function TSEAutomatableXButterworthHPModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEButterworthPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      Flags           := [iofLinearInput];
      Datatype        := dtFSample;
      DefaultValue    := '5';
      result          := True;
     end;
  pinOrder:
    with Properties^ do
     begin
      VariableAddress := @FOrder;
      Direction       := drIn;
      DataType        := dtEnum;
      Flags           := [iofLinearInput];
      result          := True;
     end;
 end;
end;

procedure TSEAutomatableXButterworthHPModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   if Sample mod 2 = 0
    then FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   Output^[Sample] := FFilter.ProcessSample64(Input[Sample] + cDenorm64);
  end;
end;

end.
