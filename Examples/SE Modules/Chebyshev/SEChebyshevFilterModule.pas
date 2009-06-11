unit SEChebyshevFilterModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DSPFilterChebyshev,
  DAV_DspFilterChebyshevType1, DAV_DspFilterChebyshevType2;

type
  // define some constants to make referencing in/outs clearer
  TSEChebyshevFilterPins = (pinInput, pinOutput, pinFrequency, pinOrder,
    pinRipple);

  TSECustomChebyshevFilterModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TCustomChebyshev1Filter;
    FStaticCount  : Integer;
    class function GetModueName: string; virtual; abstract;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEStaticChebyshevFilterModule = class(TSECustomChebyshevFilterModule)
  protected
    FFrequency    : Single;
    FOrder        : Integer;
    FRipple       : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStaticChebyshevFilterLPModule = class(TSEStaticChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEStaticChebyshevFilterHPModule = class(TSEStaticChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEControlableChebyshevFilterLPModule = class(TSEStaticChebyshevFilterLPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: string; override;
  end;

  TSEControlableChebyshevFilterHPModule = class(TSEStaticChebyshevFilterHPModule)
  protected
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetModueName: string; override;
  end;

  TSEAutomatebleChebyshevFilterModule = class(TSECustomChebyshevFilterModule)
  protected
    FFreqBuffer  : PDAVSingleFixedArray;
    FRipplBuffer : PDAVSingleFixedArray;
    FOrder       : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEAutomatebleChebyshevFilterLPModule = class(TSEAutomatebleChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleChebyshevFilterHPModule = class(TSEAutomatebleChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshevFilterModule = class(TSEAutomatebleChebyshevFilterModule)
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEAutomatebleXChebyshevFilterLPModule = class(TSEAutomatebleXChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSEAutomatebleXChebyshevFilterHPModule = class(TSEAutomatebleChebyshevFilterModule)
  protected
    class function GetModueName: string; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

implementation

uses
  SysUtils;

destructor TSECustomChebyshevFilterModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSECustomChebyshevFilterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSECustomChebyshevFilterModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

// describe your module
class procedure TSECustomChebyshevFilterModule.getModuleProperties(Properties : PSEModuleProperties);
var
  str : string;
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   str := GetModueName;
   Name := PChar(str);

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := PChar(str);

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSECustomChebyshevFilterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEChebyshevFilterPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInput1Buffer;
              Direction       := drIn;
              Datatype        := dtFSample;
              DefaultValue    := '0';
             end;

  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
              end;
  pinFrequency: with Properties^ do Name := 'Frequency [kHz]';
  pinOrder: with Properties^ do Name := 'Order';
  pinRipple: with Properties^ do Name := 'Ripple [dB]';
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TSECustomChebyshevFilterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSECustomChebyshevFilterModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

{ TSEStaticChebyshevFilterModule }

constructor TSEStaticChebyshevFilterModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFrequency := 1;
 FOrder := 4;
 FRipple := 1;
end;

function TSEStaticChebyshevFilterModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshevFilterPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  VariableAddress := @FFrequency;
                  Direction       := drParameter;
                  DataType        := dtSingle;
                  DefaultValue    := '1';
                 end;
  pinOrder: with Properties^ do
             begin
              VariableAddress := @FOrder;
              Direction       := drParameter;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range -0,32';
             end;
  pinRipple: with Properties^ do
              begin
               VariableAddress := @FRipple;
               Direction       := drParameter;
               DataType        := dtSingle;
               DefaultValue    := '1';
              end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEStaticChebyshevFilterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshevFilterPins(CurrentPin.PinID) of
      pinInput : begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
  pinFrequency : FFilter.Frequency := 1E-5 + abs(1000 * FFrequency);
      pinOrder : FFilter.Order     := FOrder;
     pinRipple : FFilter.Ripple    := 10 * FRipple;
 end;
 inherited;
end;

procedure TSEStaticChebyshevFilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Output^[Sample] := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;

{ TSEStaticChebyshevFilterLPModule }

constructor TSEStaticChebyshevFilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshevFilterLPModule.GetModueName: string;
begin
 result := 'Chebyshev Lowpass (static)';
end;

{ TSEStaticChebyshevFilterHPModule }

constructor TSEStaticChebyshevFilterHPModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilter.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEStaticChebyshevFilterHPModule.GetModueName: string;
begin
 result := 'Chebyshev Highpass (static)';
end;

{ TSEControlableChebyshevFilterLPModule }

class function TSEControlableChebyshevFilterLPModule.GetModueName: string;
begin
 result := 'Chebyshev Lowpass';
end;

function TSEControlableChebyshevFilterLPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshevFilterPins(index) of
  pinFrequency..pinRipple : with Properties^ do Direction := drIn;
 end;
end;

{ TSEControlableChebyshevFilterHPModule }

class function TSEControlableChebyshevFilterHPModule.GetModueName: string;
begin
 result := 'Chebyshev Highpass';
end;

function TSEControlableChebyshevFilterHPModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshevFilterPins(index) of
  pinFrequency..pinRipple : with Properties^ do Direction := drIn;
 end;
end;

{ TSEAutomatebleChebyshevFilterModule }

constructor TSEAutomatebleChebyshevFilterModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FOrder := 4;
end;

function TSEAutomatebleChebyshevFilterModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEChebyshevFilterPins(index) of
  pinFrequency: with Properties^ do
                 begin
                  VariableAddress := @FFreqBuffer;
                  Direction       := drIn;
                  DataType        := dtFSample;
                  DefaultValue    := '1';
                 end;
  pinOrder: with Properties^ do
             begin
              VariableAddress := @FOrder;
              Direction       := drIn;
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range -0,32';
             end;
  pinRipple: with Properties^ do
              begin
               VariableAddress := @FRipplBuffer;
               Direction       := drIn;
               DataType        := dtFSample;
               DefaultValue    := '1';
              end;
 end;
end;

procedure TSEAutomatebleChebyshevFilterModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshevFilterPins(CurrentPin.PinID) of
 pinInput : begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
  pinOrder: FFilter.Order := FOrder;
  pinFrequency,
  pinRipple:
    if (Pin[2].Status <> stRun) and (Pin[4].Status <> stRun)
     then OnProcess := SubProcessStatic
     else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSEAutomatebleChebyshevFilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Ripple : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Ripple := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
   FFilter.Ripple    := 10 * Ripple[Sample];
   Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;

procedure TSEAutomatebleChebyshevFilterModule.SubProcessStatic(
  const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
end;

{ TSEAutomatebleChebyshevFilterLPModule }

constructor TSEAutomatebleChebyshevFilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshevFilterLPModule.GetModueName: string;
begin
 result := 'Chebyshev Lowpass (automatable)';
end;

{ TSEAutomatebleChebyshevFilterHPModule }

constructor TSEAutomatebleChebyshevFilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilter.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleChebyshevFilterHPModule.GetModueName: string;
begin
 result := 'Chebyshev Highpass (automatable)';
end;

{ TSEAutomatebleXChebyshevFilterModule }

procedure TSEAutomatebleXChebyshevFilterModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Ripple : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Ripple := PDAVSingleFixedArray(@FRipplBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   if (Sample div 2 = 0) then
    begin
     FFilter.Frequency := 1E-5 + abs(10000 * Freq[Sample]);
     FFilter.Ripple    := 10 * Ripple[Sample];
    end;
   Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;

{ TSEAutomatebleXChebyshevFilterLPModule }

constructor TSEAutomatebleXChebyshevFilterLPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LowpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshevFilterLPModule.GetModueName: string;
begin
 result := 'Chebyshev Lowpass (automatable+)';
end;

{ TSEAutomatebleXChebyshevFilterHPModule }

constructor TSEAutomatebleXChebyshevFilterHPModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HighpassFilterAutomatable.Create;
 FFilter.SetFilterValues(10000, 0, 0.1);
 FFilter.Order := FOrder;
end;

class function TSEAutomatebleXChebyshevFilterHPModule.GetModueName: string;
begin
 result := 'Chebyshev Highpass (automatable+)';
end;

end.
