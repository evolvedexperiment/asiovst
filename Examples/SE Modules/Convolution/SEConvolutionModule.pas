unit SEConvolutionModule;

interface

{$I ASIOVST.INC}
{$DEFINE Use_IPPS}

uses
  SysUtils, DAV_Common, DAV_SECommon, DAV_SEModule, DAV_Complex,
  DAV_DspFftReal2Complex {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF};

type
  // define some constants to make referencing in/outs clearer
  TSEConvolutionPins = (pinInput, pinOutput, pinFileName, pinDesiredLatency,
    pinRealLatency);

  TSEConvolutionModule = class(TSEModuleBase)
  private
    FFilterKernel       : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVComplexSingleFixedArray;
    FSignalFreq         : PDAVComplexSingleFixedArray;
    FConvolved          : PDAVComplexSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FBlockInBuffer32    : PDAVSingleFixedArray;
    FBlockOutBuffer32   : PDAVSingleFixedArray;
    FSemaphore          : Integer;
    {$IFDEF Use_IPPS}
    FFft                : TFftReal2ComplexIPPSFloat32;
    {$ELSE}
    FFft                : TFftReal2ComplexNativeFloat32;
    {$ENDIF}
    FIRSize             : Integer;
    FOffsetSize         : Integer;
    FBlockPosition      : Integer;
    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FFFTSizeQuarter     : Integer;
    procedure SetIRSizePadded(const Value: Integer);
    procedure SetIRBlockSize(const Value: Integer);
  protected
    FInputBuffer         : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : PDAVSingleFixedArray;
    FFileName            : PChar;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;

    procedure CalculateFilterBlockFrequencyResponses;
    procedure CalculatePaddedIRSize;
    procedure CalculateFrequencyResponseBlockCount;
    procedure IRSizePaddedChanged;
    procedure IRBlockSizeChanged;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray);

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);

    procedure LoadIR(FileName: TFileName);
    property IRBlockSize: Integer read FFFTSize write SetIRBlockSize;
  end;

implementation

uses
  WaveIOX;

constructor TSEConvolutionModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 {$IFDEF Use_IPPS}
 if CSepMagic <> 2 * $29A2A826
  then raise Exception.Create('This module is not allowed to be embedded into a VST Plugin');
 {$ENDIF}

 inherited Create(SEAudioMaster, Reserved);
 FFileName            := '';
 FSemaphore           := 0;
 FFilterKernel        := nil;
 FSignalFreq          := nil;
 FConvolved           := nil;
 FConvolvedTime       := nil;
 FBlockInBuffer32     := nil;
 FBlockOutBuffer32    := nil;
 FFFTSize             := 0;
 FFreqRespBlockCount  := 0;
 FIRSizePadded        := 0;
 FFFTSize             := 0;
 FFFTSizeHalf         := 0;
 FFFTSizeQuarter      := 0;
 IRBlockSize          := 512;
 FDesiredLatencyIndex := 5;
end;

destructor TSEConvolutionModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFft);
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
procedure TSEConvolutionModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input, Output   : PDAVSingleFixedArray;
  CurrentPosition : Integer;
begin
 // lock processing
 while FSemaphore > 0 do;
 inc(FSemaphore);

 try
  // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
  Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
  Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

  CurrentPosition := 0;

  repeat
    if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSize then
     begin
      Move(Input^[CurrentPosition], FBlockInBuffer32^[FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));
      Move(FBlockOutBuffer32^[FBlockPosition - FFFTSizeHalf], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * Sizeof(Single));

      FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
      CurrentPosition := SampleFrames;
     end
    else
     begin
      Move(Input^[CurrentPosition], FBlockInBuffer32^[FBlockPosition], (FFFTSize - FBlockPosition) * Sizeof(Single));
      Move(FBlockOutBuffer32^[FBlockPosition - FFFTSizeHalf], Output^[CurrentPosition], (FFFTSize - FBlockPosition) * Sizeof(Single));

      // shift already played signal part
      Move(FBlockOutBuffer32^[FOffsetSize], FBlockOutBuffer32^[0], (FIRSizePadded - FOffsetSize) * SizeOf(Single));
      FillChar(FBlockOutBuffer32^[(FIRSizePadded - FOffsetSize)], FOffsetSize * SizeOf(Single), 0);

      // perform convolution for the next block
      PerformConvolution(FBlockInBuffer32, FBlockOutBuffer32);

      Move(FBlockInBuffer32[FOffsetSize], FBlockInBuffer32[0], FFFTSizeHalf * Sizeof(Single));

      CurrentPosition := CurrentPosition + (FFFTSize - FBlockPosition);
      FBlockPosition := FFFTSizeHalf;
     end;
  until CurrentPosition >= SampleFrames;
 finally
  dec(FSemaphore);
 end;
end;

procedure TSEConvolutionModule.SubProcessBypass(const BufferOffset, SampleFrames: Integer);
begin
 Move(FInputBuffer[BufferOffset], FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single));
end;

// describe your module
class procedure TSEConvolutionModule.getModuleProperties(Properties : PSEModuleProperties);
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
      Direction       := drIn;
      DataType        := dtText;
      DefaultValue    := '1000';
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
        pinFileName : if FileExists(FFileName) then
                       begin
                        LoadIR(StrPas(FFileName));
                        if FIRSizePadded > 0
                         then OnProcess := SubProcess
                         else OnProcess := SubProcessBypass;
                       end
                      else OnProcess := SubProcessBypass;
  pinDesiredLatency : case 6 + FDesiredLatencyIndex of
                        6 : IRBlockSize :=   64;
                        7 : IRBlockSize :=  128;
                        8 : IRBlockSize :=  256;
                        9 : IRBlockSize :=  512;
                       10 : IRBlockSize := 1024;
                       11 : IRBlockSize := 2048;
                       12 : IRBlockSize := 4096;
                       13 : IRBlockSize := 8192;
                       else exit;
                      end;
 end; inherited;
end;


procedure TSEConvolutionModule.CalculateFilterBlockFrequencyResponses;
var
  TempIR     : PDAVSingleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   ReallocMem(FFilterFreqs[Blocks], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   // calculate IR part size to be copied
   sz := IRSize - Blocks * FFFTSizeHalf;
   if sz > FFFTSizeHalf then sz := FFFTSizeHalf;

   // build temporary IR part
   move(FFilterKernel^[Blocks * FFFTSizeHalf], TempIR^[0], sz * SizeOf(Single));
   FillChar(TempIR^[sz], (FFFTSize - sz) * SizeOf(Single), 0);

   // perform FFT
   {$IFDEF Use_IPPS}
   FFft.Perform_FFT(FFilterFreqs[Blocks], TempIR);
   {$ELSE}
   FFft.PerformFFT32(FFilterFreqs[Blocks], TempIR);
   {$ENDIF}
  end;
end;

procedure TSEConvolutionModule.CalculateFrequencyResponseBlockCount;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;
end;

procedure TSEConvolutionModule.CalculatePaddedIRSize;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;
end;

procedure TSEConvolutionModule.SetIRBlockSize(const Value: Integer);
begin
 if FFFTSize <> Value then
  begin
   FFFTSize := Value;
   IRBlockSizeChanged;
  end;
end;

procedure TSEConvolutionModule.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;

procedure TSEConvolutionModule.IRBlockSizeChanged;
var
  i : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  i := CeilLog2(FFFTSize);
  if not assigned(FFft)
  {$IFDEF Use_IPPS}
   then FFft := TFftReal2ComplexIPPSFloat32.Create(i)
  {$ELSE}
   then FFft := TFftReal2ComplexNativeFloat32.Create(i)
  {$ENDIF}
   else FFft.Order := i;
  FFft.AutoScaleType := astDivideInvByN;

  FFFTSizeHalf    := FFFTSize div 2;
  FFFTSizeQuarter := FFFTSize div 4;
  FBlockPosition  := FFFTSizeHalf;
  FRealLatency    := FFFTSizeHalf;
  FOffsetSize     := FFFTSize - FFFTSizeHalf;

  ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
  ReallocMem(FConvolved, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
  ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));
  ReallocMem(FBlockInBuffer32,  FFFTSize * SizeOf(Single));

  FillChar(FSignalFreq^[0], FFFTSize * SizeOf(Single), 0);
  FillChar(FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
  FillChar(FConvolvedTime^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
  FillChar(FBlockInBuffer32^[0],  FFFTSize * SizeOf(Single), 0);

  CalculatePaddedIRSize;
  CalculateFilterBlockFrequencyResponses;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSEConvolutionModule.IRSizePaddedChanged;
begin
 GetMem(FBlockOutBuffer32, FIRSizePadded * SizeOf(Single));
 FillChar(FBlockOutBuffer32^[0], FIRSizePadded * SizeOf(Single), 0);
end;

procedure TSEConvolutionModule.LoadIR(FileName: TFileName);
var
  sr, c : Integer;
  pt    : PSingle;
begin
 if assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    pt := LoadWAVFileMono(FileName, sr, c, FIRSize);
    ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
    Move(pt^, FFilterKernel^[0], FIRSize * SizeOf(Single));

    CalculatePaddedIRSize;

    CalculateFilterBlockFrequencyResponses;
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 4 * ecx - 4].Single
  fadd  [edx + 4 * ecx - 4].Single
  fstp  [edx + 4 * ecx - 4].Single
  loop @Start
end;

procedure ComplexMultiply(InplaceBuffer: PDAVComplexSingleFixedArray; Filter: PDAVComplexSingleFixedArray; SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

procedure TSEConvolutionModule.PerformConvolution(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Half   : Integer;
 {$IFNDEF Use_IPPS}
  Bin    : Integer;
 {$ENDIF}
begin
 Half := FFFTSizeHalf;

 {$IFDEF Use_IPPS}
 FFft.Perform_FFT(FSignalFreq, SignalIn);
 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   ComplexMultiply(@FConvolved^[0], @FFilterFreqs[Block]^[0], Half);

   FFft.Perform_IFFT(PDAVComplexSingleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   MixBuffers_FPU(@FConvolvedTime^[Half], @SignalOut^[Block * Half], Half);
  end;

 {$ELSE}
 FFft.PerformFFT32(FSignalFreq, SignalIn);
 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   move(FSignalFreq^[0], FConvolved^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

   // DC & Nyquist
   FConvolved^[0].Re := FFilterFreqs[Block]^[0].Re * FConvolved^[0].Re;
   FConvolved^[0].Im := FFilterFreqs[Block]^[0].Im * FConvolved^[0].Im;

   // inbetween...
   for Bin := 0 to Half - 1
    do ComplexMultiplyInplace(FConvolved^[Bin], FFilterFreqs[Block]^[Bin]);

   FFft.PerformIFFT32(PDAVComplexSingleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   MixBuffers_FPU(@FConvolvedTime^[Half], @SignalOut^[Block * Half], Half);
  end;
 {$ENDIF}
end;

end.
