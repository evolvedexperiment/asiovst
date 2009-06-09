unit SEBassEnhancerModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspFrequencyDivider,
  DAV_DspPsychoacousticBassEnhancer;

type
  // define some constants to make referencing in/outs clearer
  TSEBassEnhancerPins = (pinInput, pinOutput, pinFrequency, pinDrive,
    pinResponse, pinHighpassSelect, pinDecay, pinInputLevel,
    pinOriginalBassLevel, pinMaxxBassLevel, pinHighFrequencyLevel);

  TSEBassEnhancerModule = class(TSEModuleBase)
  private
    FInputBuffer        : PDAVSingleFixedArray;
    FOutputBuffer       : PDAVSingleFixedArray;
    FStaticCount        : Integer;

    FFrequency          : Single;
    FDrive              : Single;
    FResponse           : Single;
    FHighpassSelect     : Integer;
    FDecay              : Single;
    FMaxxBassLevel      : PDAVSingleFixedArray;
    FHighFrequencyLevel : PDAVSingleFixedArray;
    FInputLevel         : PDAVSingleFixedArray;
    FOriginalBassLevel  : PDAVSingleFixedArray;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FBassEnhancer : TLinkwitzBass;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

{ TSEBassEnhancerModule }

constructor TSEBassEnhancerModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FBassEnhancer := TLinkwitzBass.Create
end;

destructor TSEBassEnhancerModule.Destroy;
begin
 FreeAndNil(FBassEnhancer);
 inherited;
end;

procedure TSEBassEnhancerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEBassEnhancerModule.SampleRateChanged;
begin
 inherited;
 FBassEnhancer.SampleRate := SampleRate;
end;

procedure TSEBassEnhancerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  MBLev  : PDAVSingleFixedArray;
  HFLev  : PDAVSingleFixedArray;
  InpLv  : PDAVSingleFixedArray;
  OrigLv : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp    := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp   := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 MBLev  := PDAVSingleFixedArray(@FMaxxBassLevel[BufferOffset]);
 HFLev  := PDAVSingleFixedArray(@FHighFrequencyLevel[BufferOffset]);
 InpLv  := PDAVSingleFixedArray(@FInputLevel[BufferOffset]);
 OrigLv := PDAVSingleFixedArray(@FOriginalBassLevel[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outp^[Sample] := FBassEnhancer.Process(Inp^[Sample]);
   if (Sample div 2) = 0 then
    begin
     FBassEnhancer.HarmonyBassLevel := MBLev^[Sample];
     FBassEnhancer.HighFrequencyLevel := HFLev^[Sample];
     FBassEnhancer.InputLevel := InpLv^[Sample];
     FBassEnhancer.OriginalBassLevel := OrigLv^[Sample];
    end;
  end;
end;

procedure TSEBassEnhancerModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEBassEnhancerModule.ChooseProcess;
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
class procedure TSEBassEnhancerModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Linkwitz Bass Enhancer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bass Enhancer';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEBassEnhancerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEBassEnhancerPins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency';
     VariableAddress := @FFrequency;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '80';
    end;
  pinDrive:
   with Properties^ do
    begin
     Name            := 'Drive';
     VariableAddress := @FDrive;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
    end;
  pinResponse:
   with Properties^ do
    begin
     Name            := 'Response';
     VariableAddress := @FResponse;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '20';
    end;
  pinHighpassSelect:
   with Properties^ do
    begin
     Name            := 'Highpass Select';
     VariableAddress := @FHighpassSelect;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 0,2';
     DefaultValue    := '1';
    end;
  pinDecay:
   with Properties^ do
    begin
     Name            := 'Decay';
     VariableAddress := @FDecay;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '20';
    end;
  pinInputLevel:
   with Properties^ do
    begin
     Name            := 'Input Level';
     VariableAddress := @FInputLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinMaxxBassLevel:
   with Properties^ do
    begin
     Name            := 'Harmonic Bass Level';
     VariableAddress := @FMaxxBassLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinHighFrequencyLevel:
   with Properties^ do
    begin
     Name            := 'High Frequency Level';
     VariableAddress := @FHighFrequencyLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinOriginalBassLevel:
   with Properties^ do
    begin
     Name            := 'OriginalBassLevel';
     VariableAddress := @FOriginalBassLevel;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSEBassEnhancerModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEBassEnhancerPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
       pinFrequency : FBassEnhancer.Frequency := FFrequency;
           pinDrive : FBassEnhancer.Drive := FDrive;
        pinResponse : FBassEnhancer.Response := FResponse;
  pinHighpassSelect : FBassEnhancer.HighpassSelect := THighpassSelect(FHighpassSelect);
           pinDecay : FBassEnhancer.Decay := FDecay;
 end;
end;

end.
