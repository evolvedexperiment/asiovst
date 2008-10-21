unit SEChebyshevWaveshaperModule;

interface

uses
  DAV_Common, DAV_DSPWaveshaper, DAV_SECommon, DAV_SEModule;

const
  CHarmonicCount = 24;

type
  // define some constants to make referencing in/outs clearer
  TSEChebyshevWaveshaperModule = class(TSEModuleBase)
  private
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FHarmonics    : array [0..CHarmonicCount - 1] of Single;
    FWaveShaper   : TChebyshevWaveshaper;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure Close; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

implementation

uses
  SysUtils;

constructor TSEChebyshevWaveshaperModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
var
  i : Integer;
begin
 inherited Create(SEAudioMaster, Reserved);
 for i := 0 to CHarmonicCount - 1
  do FHarmonics[i] := 0;
 FWaveShaper := TChebyshevWaveshaper.Create; 
end;

destructor TSEChebyshevWaveshaperModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FWaveShaper);
 inherited;
end;

procedure TSEChebyshevWaveshaperModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEChebyshevWaveshaperModule.Close;
begin
 // nothing here todo yet
 inherited;
end;

procedure TSEChebyshevWaveshaperModule.SampleRateChanged;
begin
 inherited;
end;

// The most important part, processing the audio
procedure TSEChebyshevWaveshaperModule.SubProcess(const BufferOffset, SampleFrames: Integer);
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
   Output^[Sample] := FWaveShaper.ProcessSample(Input[Sample]);
  end;
end;

// describe your module
class function TSEChebyshevWaveshaperModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Chebyshev Waveshaper';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Chebyshev Waveshaper';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSEChebyshevWaveshaperModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInput1Buffer;
       Direction       := drIn;
       Datatype        := dtFSAMPLE;
       DefaultValue    := '0';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  2: with Properties^ do
      begin
       Name            := 'Fundamental';
       VariableAddress := @FHarmonics[0];
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       DatatypeExtra   := 'range -1,1';
      end;
  3..CHarmonicCount + 1:
     with Properties^ do
      begin
       Name            := 'Harmonic';
       VariableAddress := @FHarmonics[Index - 2];
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '0';
       DatatypeExtra   := 'range -1,1';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEChebyshevWaveshaperModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered ChebyshevWaveshaper time parameter?
 case CurrentPin.PinID of
  2..CHarmonicCount + 1: FWaveShaper.Gain[CurrentPin.PinID - 2] := FHarmonics[CurrentPin.PinID - 2];
 end;
 inherited;
end;

end.
