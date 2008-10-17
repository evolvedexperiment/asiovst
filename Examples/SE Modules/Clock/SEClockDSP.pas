unit SEClockDSP;

interface

uses
  DAV_Common, SECommon, SEDSP;

const
  CDefaultTempo = 10; // 100 BPM

type
  // define some constants to make referencing in/outs clearer
  TSEClockPins = (pinClockOut, pinBarResetOut, pinTempoOut, pinTransportOut,
    pinPulseDivideIn);

  TSmartOutput = class
  private
    FOutputBuffer       : PDAVSingleFixedArray;
    FCurrentOutputValue : Single;
    FModule             : TSEModuleBase;
    FPinNumber          : Integer;
    FStaticOutputCount  : Integer;
  public
    constructor Create(AModule : TSEModuleBase; APlugNumber: Integer);
    destructor Destroy; override;
    function PointerAddress: PDAVArrayOfSingleFixedArray;
    procedure Process(BufferPosition, SampleFrames: Integer);
    procedure SetValue(SampleClock: Cardinal; Value: Single);
    function GetValue: Single;
//    procedure SetPin(SEPin *Pin){m_pin=Pin;};
  end;

  TSEClockModule = class(TSEModuleBase)
  private
    FBMPOut            : TSmartOutput;
    FClockOut          : TSmartOutput;
    FBarResetOut       : TSmartOutput;
    FTransportOut      : TSmartOutput;
    FError             : Single;
    FpulseCount        : Single;
    FpulseCountInt     : Integer;
    FMidiClock         : Integer;
    FQuarterNoteCount  : Integer;
    FQuartersPerBar    : Integer;
    FPulsesPerBeat     : ShortInt;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean;
    function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(BufferOffset: Integer; SampleFrames: Integer);
  end;

implementation

uses
  DAV_VSTEffect;

constructor TSmartOutput.Create(AModule: TSEModuleBase; APlugNumber: Integer);
begin
 FCurrentOutputValue := 0;
 FOutputBuffer := nil;
 FModule := AModule;
 FPinNumber := APlugNumber;
 FStaticOutputCount := 200;
end;

destructor TSmartOutput.Destroy;
begin
 inherited;
end;

function TSmartOutput.GetValue: Single;
begin
 result := FCurrentOutputValue;
end;

function TSmartOutput.PointerAddress: PDAVArrayOfSingleFixedArray;
begin
 result := @FOutputBuffer;
end;

procedure TSmartOutput.process(BufferPosition, SampleFrames: Integer);
var
  sample : Integer;
begin
  if (FStaticOutputCount < 0) then exit;

  if (FStaticOutputCount < SampleFrames) // lazy as posible
   then SampleFrames := FStaticOutputCount;

  for sample := SampleFrames - 1 downto 0 do
   begin
    FOutputBuffer[BufferPosition + Sample] := FCurrentOutputValue;
   end;

 FStaticOutputCount := FStaticOutputCount - SampleFrames;
end;

procedure TSmartOutput.SetValue(SampleClock: Cardinal; Value: Single);
begin
 if FCurrentOutputValue <> Value then
  begin
   FStaticOutputCount := FModule.getBlockSize;
   FCurrentOutputValue := Value;
   FModule.getPin(FPinNumber).TransmitStatusChange(SampleClock, stOneOff);
  end;
end;

{ TSEClockModule }

constructor TSEClockModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FpulseCount       := 0;
 FpulseCountInt    := 0;
 FMidiClock        := -1;
 FQuarterNoteCount := -1;
 FQuartersPerBar   := 4;
 FError            := 0;
 FBMPOut           := TSmartOutput.Create(Self, Integer(pinTempoOut));
 FClockOut         := TSmartOutput.Create(Self, Integer(pinClockOut));
 FBarResetOut      := TSmartOutput.Create(Self, Integer(pinBarResetOut));
 FTransportOut     := TSmartOutput.Create(Self, Integer(pinTransportOut));
end;

destructor TSEClockModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 inherited;
end;

procedure TSEClockModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
 FBMPOut.SetValue(SampleClock, CDefaultTempo);
end;

// The most important part, processing the audio
procedure TSEClockModule.SubProcess(BufferOffset: Integer; SampleFrames: Integer);
var
  SamplesPerBeat          : Single;
  ti                      : PVstTimeInfo;
  SamplesRemain           : Integer;
  BufferPosition          : Integer;
  to_do                   : Integer;
  SamplesAlreadyProcessed : Integer;
  CurSampleClock          : Cardinal;
  NewBarReset             : Single;
begin
 ti := nil;
 SamplesRemain := SampleFrames;
 BufferPosition := BufferOffset;
(*

 while(true)
  begin
   to_do := SamplesRemain;
    if (FPulseCountInt < SamplesRemain)
     then to_do := FPulseCountInt;

    FBMPOut.Process(BufferPosition, to_do);
    FClockOut.Process(BufferPosition, to_do);
    FBarResetOut.Process(BufferPosition, to_do);
    FTransportOut.Process(BufferPosition, to_do);

    SamplesRemain := SamplesRemain - to_do;
    FPulseCountInt := FPulseCountInt - to_do;

    if (SamplesRemain = 0) then exit;

    BufferPosition := to_do + BufferPosition;

    assert(FPulseCountInt = 0);
    assert(FPulseCount <= 0);

    begin
      SamplesAlreadyProcessed := sampleFrames - SamplesRemain;
      CurSampleClock := SampleClock + SamplesAlreadyProcessed;

      NewBarReset := 0;
      if FClockOut.GetValue = 0 then
       begin
        // keep track of position within beat (24 clock/beat)
        inc(FMidiClock);
        FMidiClock := FMidiClock mod FPulsesPerBeat;

        if (FMidiClock = 0) then // start of quarter note
         begin
          inc(FQuarterNoteCount);
          FQuarterNoteCount := FQuarterNoteCount mod FQuartersPerBar;
          if (FQuarterNoteCount = 0) // start of bar
           then NewBarReset := 0.5;
         end;

        FClockOut.SetValue(CurSampleClock, 0.5);
       end
      else FClockOut.SetValue(CurSampleClock, 0);

      FBarResetOut.SetValue(CurSampleClock, NewBarReset);

      if (ti = 0) then // only do this a max of once per call
       begin
        // add on time since block start
        ti := PVstTimeInfo(CallHost(seaudioMasterGetTime, 0, kVstTempoValid|kVstPpqPosValid, 0));
        if (ti <> nil) then
         begin
          // calculate how many samples per beat/clock
          double SamplesToBeats = ti.tempo / ( sampleRate * 60.0 ); // converts secs to beats (quarter notes)
          double SamplesToClocks = SamplesToBeats * FPulsesPerBeat;

          //int SamplesAlreadyProcessed = sampleFrames - SamplesRemain;//s;
          // half a midi clock (for on/off transition)
          SamplesPerBeat = 0.5f / (float)SamplesToClocks;

          float NewTransportRunning = 0.f;

          // lock to host if transport playing, else just freewheel
          if( (ti.flags & kVstTransportPlaying) != 0 )
          begin
            NewTransportRunning = 1.f;

            double ExtraSamples = buffer_offset + (double) SamplesAlreadyProcessed;
            double HostTotalBeats = ti.ppqPos + ExtraSamples * SamplesToBeats;
            FQuarterNoteCount = (int) HostTotalBeats;

            float BeatFrac = (float) FMidiClock;

            // this routine called twice per midi clock (leading edge/trailing edge)
            // on trailing edge, we're half way between midi clocks
            if( FClockOut.GetValue() == 0.f )//outval == 0.f )
            begin
              BeatFrac += 0.5;
            end;

            // add small fractional ammount due to pulse not always
            // falling on an exact sampleframe
            BeatFrac -= 0.5f * FPulseCount/SamplesPerBeat;

            // convert from MIDI clocks to beats
            BeatFrac = BeatFrac / (float) FPulsesPerBeat;

            float HostBeatFrac = (float)(HostTotalBeats - floor( HostTotalBeats ));
            
            // calc FError as fraction of beat
            FError = HostBeatFrac - BeatFrac;
            if( FError < -0.5 )
              FError += 1.0;
            if( FError > 0.5 )
              FError -= 1.0;

            // convert FError back to samples
            float Correction = FError * 2.f * SamplesPerBeat * FPulsesPerBeat;

            // jump count a fraction to catch up with host
            FPulseCount -= Correction; //SamplesPerBeat * FError * 0.8f;

//#ifdef _DEBUG
//            getPin(PN_OUTPUT5).TransmitStatusChange( SampleClock() + SamplesAlreadyProcessed, ST_ONE_OFF );
//#endif
          end;
          
          float NewTempo = (float) ti.tempo * 0.1f;

          // avoid divide-by-zero errors when host don't provide tempo info
          if( NewTempo == 0.f )
            NewTempo = CDefaultTempo;

          // note: these are not sample-accurate. Being Polled, they always lag actual event.
          FBMPOut.SetValue      ( CurSampleClock, NewTempo  );
          FTransportOut.SetValue( CurSampleClock, NewTransportRunning );
        end;
      end;

      // reset pulse count
      FPulseCount += SamplesPerBeat;

      // can't skip a pulse completely, that would lose sync
      if( FPulseCount < 2.f )
      begin
        FPulseCount = 2.f;
      end;

      FPulseCountInt = (int) ceil(FPulseCount);

      // pre-calculate pulse count at next clock
      FPulseCount -= FPulseCountInt;
    end;
/*
#ifdef _DEBUG
    *out4++ = FError;
#endif
*/
  end;
end;
*)
end;

// describe your module
class function TSEClockModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'BPM Clock2';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'Synthedit BPM Clock V2';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSEClockModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEClockPins(index) of
  // typical input plug (inputs are listed first)
  pinClockOut:
   with properties^ do
    begin
     Name            := 'Pulse Out';
     VariableAddress := FClockOut.PointerAddress; // @OutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinBarResetOut:
   with properties^ do
    begin
     Name            := 'Bar Start';
     VariableAddress := FBarResetOut.PointerAddress; //@output4_buffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinTempoOut:
   with properties^ do
    begin
     Name            := 'Tempo Out';
     VariableAddress := FBMPOut.PointerAddress;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinTransportOut:
   with properties^ do
    begin
     Name            := 'Transport Run';
     VariableAddress := FTransportOut.PointerAddress; //&output3_buffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinPulseDivideIn:
   with properties^ do
    begin
     Name            := 'Pulse Divide';
     VariableAddress := @FPulsesPerBeat;
     DatatypeExtra   := '64=64,32=32,24=24,16=16,12=12,8=8,6=6,4=4,3=3,2=2,1=1';
     DefaultValue    := '4';
     Direction       := drIn;
     Datatype        := dtEnum;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
