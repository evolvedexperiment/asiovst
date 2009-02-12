unit SEConvolutionModule;

interface

{$I DAV_Compiler.INC}

uses
  SysUtils, DAV_Common, DAV_SECommon, DAV_SEModule, DAV_Complex,
  DAV_DspConvolution;

type
  // define some constants to make referencing in/outs clearer
  TSEConvolutionPins = (pinInput, pinOutput, pinFileName, pinMaxIRSize,
    pinDesiredLatency, pinRealLatency);

  TSEConvolutionModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : PDAVSingleFixedArray;

    FConvolver           : TConvolution32;

    FSemaphore           : Integer;
    FStaticCount         : Integer;
    FFileName            : PChar;
    FMaxIRSize           : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);

    procedure LoadIR(FileName: TFileName);
  end;

implementation

uses
  WaveIOX;

resourcestring
  RCStrSynthEditOnly = 'This module is not allowed to be embedded into a VST Plugin';

constructor TSEConvolutionModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
{$IFDEF Use_IPPS}
var
  VSTHostParams : TSECallVstHostParams;
  VendorString  : string;
{$ENDIF}
begin
 inherited Create(SEAudioMaster, Reserved);
 FFileName            := '';
 FSemaphore           := 0;
 FConvolver           := TConvolution32.Create;
 FMaxIRSize           := 16384;
 FDesiredLatencyIndex := 5;

 {$IFDEF Use_IPPS}
 if CSepMagic <> 2 * $29A2A826
  then raise Exception.Create(RCStrSynthEditOnly);
 VSTHostParams.Opcode := 32;
 if CallHost(SEAudioMasterCallVstHost, 0, 0, @VSTHostParams) <> -1
  then raise Exception.Create(RCStrSynthEditOnly);
 {$ENDIF}
end;

destructor TSEConvolutionModule.Destroy;
begin
 FreeAndNil(FConvolver);
 inherited;
end;

procedure TSEConvolutionModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessBypass;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEConvolutionModule.SampleRateChanged;
begin
 inherited;
 // ignore
end;

// The most important part, processing the audio
procedure TSEConvolutionModule.SubProcessBypass(const BufferOffset, SampleFrames: Integer);
begin
 Move(FInputBuffer[BufferOffset], FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single));
end;

procedure TSEConvolutionModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEConvolutionModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun then
  if FileExists(FFileName)
   then OnProcess := SubProcess
   else OnProcess := SubProcessBypass
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSEConvolutionModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   {$IFDEF Use_IPPS}
   Name       := 'Convolution Module (IPP based)';
   ID         := 'IPP Convolution Module';
   {$ELSE}
   Name       := 'Simple Convolution Module';
   ID         := 'DAV Simple Convolution Module';
   {$ENDIF}
   About      := 'by Christian-W. Budde';
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEConvolutionModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEConvolutionPins(index) of
  // typical input plug (inputs are listed first)
  pinInput:
    with Properties^ do
     begin
      Name            := 'Input';
      VariableAddress := @FInputBuffer;
      Flags           := [iofLinearInput];
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
  pinFileName:
    with Properties^ do
     begin
      Name            := 'FileName';
      VariableAddress := @FFileName;
      Flags           := [iofFilename];
      Direction       := drIn;
      DataType        := dtText;
      DefaultValue    := 'IR.wav';
     end;
  pinMaxIRSize:
    with Properties^ do
     begin
      Name            := 'Maximum IR Size';
      VariableAddress := @FMaxIRSize;
      Direction       := drIn;
      DataType        := dtInteger;
      DefaultValue    := '16384';
     end;
  pinDesiredLatency:
    with Properties^ do
     begin
      Name            := 'Desired Latency';
      VariableAddress := @FDesiredLatencyIndex;
      Direction       := drParameter;
      DataType        := dtEnum;
      DefaultValue    := '3';
      DatatypeExtra   := '64, 128, 256, 512, 1024, 2048, 4096, 8192';
     end;
  pinRealLatency:
    with Properties^ do
     begin
      Name            := 'Real Latency';
      VariableAddress := @FRealLatency;
      Direction       := drOut;
      DataType        := dtInteger;
     end;
  else result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEConvolutionModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEConvolutionPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
        pinFileName : begin
                       if FileExists(FFileName)
                        then LoadIR(StrPas(FFileName));
                       ChooseProcess;
                      end;
       pinMaxIRSize : begin
                       while FSemaphore > 0 do;
                       Inc(FSemaphore);
                       try
                        if FConvolver.IRSize > FMaxIRSize
                         then FConvolver.IRSize := FMaxIRSize;
                       finally
                        Dec(FSemaphore);
                       end;
                      end;
  pinDesiredLatency : FConvolver.FFTOrder := 6 + FDesiredLatencyIndex;
 end; inherited;
end;


procedure TSEConvolutionModule.LoadIR(FileName: TFileName);
var
  sr, c, sz : Integer;
  pt        : PSingle;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  pt := LoadWAVFileMono(FileName, sr, c, sz);
  FConvolver.LoadImpulseResponse(@pt^, sz);
 finally
  dec(FSemaphore);
 end;
end;

procedure TSEConvolutionModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 // lock processing
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  FConvolver.ProcessBlock(PDAVSingleFixedArray(@FInputBuffer[BufferOffset]),
                          PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]),
                          SampleFrames);
 finally
  dec(FSemaphore);
 end;
end;

end.
