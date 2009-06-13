unit SEFiltersModule;

interface

uses
  DAV_Common, DAV_DSPFilter, DAV_DSPFilterBasics, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEFiltersPins = (pinInput, pinOutput, pinFilterReference, pinFrequency,
    pinGain, pinBandwidth, pinShape);

  TCustomSEFiltersModule = class(TSEModuleBase)
  private
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TBiquadIIRFilter;
    FFilterRef    : Pointer;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure Open; override;
    procedure SampleRateChanged; override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TCustomSEGainFrequencyModule = class(TCustomSEFiltersModule)
  protected
    FFreqBuffer      : PDAVSingleFixedArray;
    FGainBuffer      : PDAVSingleFixedArray;
    FBandwidthBuffer : PDAVSingleFixedArray;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEBasicLowpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicBandpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicNotchModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicLowshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfAModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicHighshelfBModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicPeakModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEBasicAllpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TShapeFilter = class(TBasicPeakFilter)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  protected
    procedure CalculateCoefficients; override;
    procedure BandwidthChanged; override;
  public
    property Shape : Double read FShape write SetShape;
  end;

  TSEBasicShapeModule = class(TCustomSEGainFrequencyModule)
  protected
    FShapeBuffer : PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  Math, SysUtils;

destructor TCustomSEFiltersModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomSEFiltersModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
 FFilterRef := FFilter;

 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stStatic);

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TCustomSEFiltersModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  begin
   ChooseProcess;
   Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
  end;
end;

// The most important part, processing the audio
procedure TCustomSEFiltersModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

procedure TCustomSEFiltersModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 FFilterRef := @FFilter;
 Pin[Integer(pinFilterReference)].TransmitStatusChange(SampleClock, stStatic);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FFilter.ProcessSample(Input[Sample] + cDenorm64);
end;

procedure TCustomSEFiltersModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEFiltersModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEFiltersModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEFiltersModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEFiltersPins(index) of
  // typical input plug (inputs are listed first)
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

  // typical output plug
  pinOutput:
    with Properties^ do
     begin
      Name            := 'Output';
      VariableAddress := @FOutputBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;

  // filter reference
  pinFilterReference:
    with Properties^ do
     begin
      Name            := 'Filter Reference';
      VariableAddress := @FFilterRef;
      Direction       := drOut;
      Datatype        := dtInteger;
     end;

  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;


{ TCustomSEGainFrequencyModule }

function TCustomSEGainFrequencyModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEFiltersPins(Index) of
  pinFrequency:
    with Properties^ do
     begin
      Name            := 'Frequency [kHz]';
      VariableAddress := @FFreqBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0.5';
      result          := True;
     end;
  pinGain:
    with Properties^ do
     begin
      Name            := 'Gain [dB]';
      VariableAddress := @FGainBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0';
      result          := True;
     end;
  pinBandwidth:
    with Properties^ do
     begin
      Name            := 'Bandwidth';
      VariableAddress := @FBandwidthBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0.5';
      result          := True;
     end;
 end;
end;

procedure TCustomSEGainFrequencyModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Gain   : PDAVSingleFixedArray;
  BW     : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Gain   := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);
 BW     := PDAVSingleFixedArray(@FBandwidthBuffer[BufferOffset]); 

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 10000 * Freq[Sample];
   FFilter.Gain      := 10 * Gain[Sample];
   FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
   Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;


{ TSEBasicLowpassModule }

constructor TSEBasicLowpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicLowpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicLowpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowpass';
  end;
end;

{ TSEBasicHighpassModule }

constructor TSEBasicHighpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicHighpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicHighpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highpass';
  end;
end;

{ TSEBasicBandpassModule }

constructor TSEBasicBandpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicBandpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicBandpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Bandpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Bandpass';
  end;
end;

{ TSEBasicNotchModule }

constructor TSEBasicNotchModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicNotchFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicNotchModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Notch';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Notch';
  end;
end;

{ TSEBasicLowshelfModule }

constructor TSEBasicLowshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicLowshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf';
  end;
end;

{ TSEBasicLowshelfAModule }

constructor TSEBasicLowshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicLowShelfAFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicLowshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf A';
  end;
end;

{ TSEBasicLowshelfBModule }

constructor TSEBasicLowshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicLowShelfBFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicLowshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Lowshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Lowshelf B';
  end;
end;

{ TSEBasicHighshelfModule }

constructor TSEBasicHighshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicHighshelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicHighshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf';
  end;
end;

{ TSEBasicHighshelfAModule }

constructor TSEBasicHighshelfAModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicHighshelfAFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicHighshelfAModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf A';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf A';
  end;
end;

{ TSEBasicHighshelfBModule }

constructor TSEBasicHighshelfBModule.Create(
  SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicHighshelfBFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicHighshelfBModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Highshelf B';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Highshelf B';
  end;
end;

{ TSEBasicPeakModule }

constructor TSEBasicPeakModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicPeakFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicPeakModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Peak';
  end;
end;

{ TSEBasicAllpassModule }

constructor TSEBasicAllpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TBasicAllpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicAllpassModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Allpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Allpass';
  end;
end;

{ TShapeFilter }

procedure TShapeFilter.BandwidthChanged;
var
  d : Double;
begin
 if abs(FShape) > 1
  then d := ln(1 + Power(FBandWidth, abs(FShape)))
  else d := ln(1 + FBandWidth);
 if abs(FShape) > 1
  then FAlpha := tan(FW0 * 0.5) * d / (cos(0.5 * FW0)) * 2
  else FAlpha := tan(FW0 * 0.5) * d / (cos(0.5 * FW0)) * Power(2, abs(FShape));
end;

procedure TShapeFilter.CalculateCoefficients;
var t, K, G, V, A  : Double;
begin
 if FShape < -1 then
  begin
   G := FGainFactor * (2 + FShape);
   V := Power(FGainFactor ,(2 + FShape));

   K := tan(FW0 * 0.5);
   A := Power(FGainFactor, - 0.5);

   t               := 1 / (sqr(K) / V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (sqr(K) / V - 1) * t;
   FDenominator[2] := t * (sqr(K) / V + 1 - FAlpha * A);

   FNominator[0]   :=     (sqr(K) * G + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * (sqr(K) * G              - 1) * t;
   FNominator[2]   :=     (sqr(K) * G - FAlpha / A + 1) * t;
  end else
 if FShape > 1 then
  begin
   G := FGainFactor * (2 - FShape);
   V := Power(FGainFactor ,(2 - FShape));

   K := tan(FW0 * 0.5);
   A := Power(FGainFactor, 0.5);

   t               := 1 / (sqr(K) * V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (sqr(K) * V - 1) * t;
   FDenominator[2] := t * (sqr(K) * V + 1 - FAlpha * A);

   FNominator[0]   :=     V * (sqr(K) + FAlpha * A + G) * t;
   FNominator[1]   := 2 * V * (sqr(K)              - G) * t;
   FNominator[2]   :=     V * (sqr(K) - FAlpha * A + G) * t;
  end
 else
  begin
   if FShape < 0
    then G := 1
    else G := Power(FGainFactor, 2 * FShape);

   K := tan(FW0*0.5);
   V := Power(FGainFactor, FShape);
   A := Power(FGainFactor, sqr(FShape) + 0.5 * FShape - 1);

   t               := 1 / (sqr(K) * V + FAlpha * A + 1);
   FDenominator[1] := 2 * (sqr(K) * V              - 1) * t;
   FDenominator[2] := t * (sqr(K) * V - FAlpha * A + 1);

   FNominator[0]   :=     G * (sqr(K) / V + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * G * (sqr(K) / V              - 1) * t;
   FNominator[2]   :=     G * (sqr(K) / V - FAlpha / A + 1) * t;
  end;

 CalcPolesZeros;
end;

procedure TShapeFilter.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   BandwidthChanged;
   CalculateCoefficients;
  end;
end;

{ TSEBasicShapeModule }

constructor TSEBasicShapeModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TShapeFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSEBasicShapeModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Shape Peak';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Filter Shape Peak';
  end;
end;

function TSEBasicShapeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEFiltersPins(Index) of
  pinShape:
    with Properties^ do
     begin
      Name            := 'Shape';
      VariableAddress := @FShapeBuffer;
      Direction       := drIn;
      DataType        := dtFSample;
      Flags           := [iofLinearInput];
      DefaultValue    := '0';
      result          := True;
     end;
 end;
end;

procedure TSEBasicShapeModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Freq   : PDAVSingleFixedArray;
  Gain   : PDAVSingleFixedArray;
  BW     : PDAVSingleFixedArray;
  Sym    : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Freq   := PDAVSingleFixedArray(@FFreqBuffer[BufferOffset]);
 Gain   := PDAVSingleFixedArray(@FGainBuffer[BufferOffset]);
 BW     := PDAVSingleFixedArray(@FBandwidthBuffer[BufferOffset]);
 Sym    := PDAVSingleFixedArray(@FShapeBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   FFilter.Frequency := 10000 * Freq[Sample];
   FFilter.Gain      := 15 * Gain[Sample];
   FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
   TShapeFilter(FFilter).Shape := Sym[Sample];
   Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;

end.
