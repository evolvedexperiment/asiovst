unit SETanhAproximationsModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule;

type
  TSETanhAproximationsModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FTerms        : Integer;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    procedure SubProcessOpt3asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt4asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt5asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt6asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt7asm(const BufferOffset, SampleFrames: Integer);
  end;

  TSETanhAproxModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FTerms        : Integer;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    procedure SubProcess2a(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2b(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2c(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2d(const BufferOffset, SampleFrames: Integer);
  end;

  TSETanhModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    procedure Open; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  Math, SysUtils;

constructor TSETanhAproximationsModule.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FTerms := 3;
end;

procedure TSETanhAproximationsModule.Open;
begin
 // choose which function is used to process audio
 OnProcess := SubProcessOpt3asm;

 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSETanhAproximationsModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 2 then
  begin
   case FTerms of
    3 : OnProcess := SubProcessOpt3asm;
    4 : OnProcess := SubProcessOpt4asm;
    5 : OnProcess := SubProcessOpt5asm;
    6 : OnProcess := SubProcessOpt6asm;
    7 : OnProcess := SubProcessOpt7asm;
   end;
  end;
end;

// The most important part, processing the audio
procedure TSETanhAproximationsModule.SubProcessOpt3asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt3asm(Input[Sample]);
end;

procedure TSETanhAproximationsModule.SubProcessOpt4asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt4asm(Input[Sample]);
end;

procedure TSETanhAproximationsModule.SubProcessOpt5asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt5asm(Input[Sample]);
end;

procedure TSETanhAproximationsModule.SubProcessOpt6asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt6asm(Input[Sample]);
end;

procedure TSETanhAproximationsModule.SubProcessOpt7asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt7asm(Input[Sample]);
end;

// describe your module
class function TSETanhAproximationsModule.GetModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Tanh Aproximations';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Tanh Aproximations';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSETanhAproximationsModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
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

  // parameter
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drParameter;
       Datatype        := dtEnum;
       DatatypeExtra   := 'range 3,7';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSETanhAproxModule }

constructor TSETanhAproxModule.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FTerms := 3;
end;

procedure TSETanhAproxModule.Open;
begin
 // choose which function is used to process audio
 OnProcess := SubProcess2d;

 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

class function TSETanhAproxModule.GetModuleProperties(Properties: PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Tanh Aprox.';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Tanh Aprox.';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

function TSETanhAproxModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
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

  // parameter
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drParameter;
       Datatype        := dtEnum;
       DatatypeExtra   := 'range 3,6';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSETanhAproxModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 2 then
  begin
   case FTerms of
    3 : OnProcess := SubProcess2d;
    4 : OnProcess := SubProcess2c;
    5 : OnProcess := SubProcess2b;
    6 : OnProcess := SubProcess2a;
   end;
  end;
 inherited;
end;

procedure TSETanhAproxModule.SubProcess2a(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh2a(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2b(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh2b(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2c(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh2c(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2d(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh2d(Input[Sample]);
end;

{ TSETanhModule }

procedure TSETanhModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

class function TSETanhModule.GetModuleProperties(Properties: PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Tanh';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Tanh';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

function TSETanhModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin  
 result := True;
 case index of
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
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TSETanhModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh(Input[Sample]);
end;

end.
