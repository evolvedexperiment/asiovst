unit SEFiltersModule;

interface

uses
  DAV_Common, DAV_DSPFilter, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEFiltersPins = (pinInput, pinOutput, pinFrequency, pinGain, pinBandwidth);

  TCustomSEFiltersModule = class(TSEModuleBase)
  private
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFilter       : TBiquadIIRFilter;
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

  TSESimpleLowpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleHighpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleBandpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleNotchModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleLowshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleHighshelfModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimplePeakModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSESimpleAllpassModule = class(TCustomSEGainFrequencyModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TShapeFilter = class(TSimplePeakFilter)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  protected
    procedure CalculateCoefficients; override;
    procedure BandwidthChanged; override;
  public
    property Shape : Double read FShape write SetShape;
  end;

  TSESimpleShapeModule = class(TCustomSEGainFrequencyModule)
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
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInputBuffer;
              Direction       := drIn;
              Flags           := [iofLinearInput];
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
   FFilter.Frequency := 10000 * Freq[Sample] * FFilter.SampleRate;
   FFilter.Gain      := 15 * Gain[Sample];
   FFilter.Bandwidth := 0.1 + 9.9 * abs(BW[Sample]);
   Output^[Sample]   := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;


{ TSESimpleLowpassModule }

constructor TSESimpleLowpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleLowpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleLowpassModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleHighpassModule }

constructor TSESimpleHighpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleHighpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleHighpassModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleBandpassModule }

constructor TSESimpleBandpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleBandpass.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleBandpassModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleNotchModule }

constructor TSESimpleNotchModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleNotch.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleNotchModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleLowshelfModule }

constructor TSESimpleLowshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleLowShelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleLowshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleHighshelfModule }

constructor TSESimpleHighshelfModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleHighshelfFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleHighshelfModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimplePeakModule }

constructor TSESimplePeakModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimplePeakFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimplePeakModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleAllpassModule }

constructor TSESimpleAllpassModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TSimpleAllpassFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleAllpassModule.GetModuleProperties(Properties: PSEModuleProperties);
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

{ TSESimpleShapeModule }

constructor TSESimpleShapeModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TShapeFilter.Create;
 FFilter.Frequency := 1000;
 FFilter.Gain      := 0;
 FFilter.Bandwidth := 1;
end;

class procedure TSESimpleShapeModule.GetModuleProperties(Properties: PSEModuleProperties);
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

function TSESimpleShapeModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case Index of
  5: with Properties^ do
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

procedure TSESimpleShapeModule.SubProcess(const BufferOffset,
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
