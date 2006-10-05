unit DVSTEffect;

interface 

{$WARNINGS OFF}
{$IFDEF FPC}
 {$MODE DELPHI}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

uses Windows;

const kEffectMagic = 'VstP';

// flags bits ////////////////////////////////////////////////////////////////
type
  PPSingle = ^PSingle;
  PPDouble = ^PDouble;
  PVSTEffect = ^TVSTEffect;
  TAudioMasterCallbackFunc = function(Effect: PVSTEffect; Opcode, Index, Value: Integer; Ptr: Pointer; Opt: Single): longint; cdecl;
  TDispatcherFunc = function(Effect: PVSTEffect; Opcode, Index, Value: Integer; Ptr: Pointer; Opt: Single): longint; cdecl;
  TProcessProc = procedure(Effect: PVSTEffect; Inputs, Outputs: PPSingle; Sampleframes: Integer); cdecl;
  TProcessDoubleProc = procedure(Effect: PVSTEffect; Inputs, Outputs: PPDouble; Sampleframes: Integer); cdecl;
  TSetParameterProc = procedure(Effect: PVSTEffect; Index: Longint; Parameter: Single); cdecl;
  TGetParameterFunc = function(Effect: PVSTEffect; Index: longint): Single; cdecl;

  // prototype for plug-in main ////////////////////////////////////////////////
  TMainProc = function(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl;

  TEffFlag = (
    effFlagsHasEditor,	         // if set, is expected to react to editor messages
    effFlagsHasClip,             // NOT USED SINCE 2.4 - return > 1. in getVu() if clipped
    effFlagsHasVu,               // NOT USED SINCE 2.4 - return vu value in getVu(); > 1. means clipped
    effFlagsCanMono,	         // NOT USED SINCE 2.4 - if numInputs == 2, makes sense to be used for mono in
    effFlagsCanReplacing,        // MUST BE SET! supports in place output (processReplacing() exsists)
    effFlagsProgramChunks,       // program data are handled in formatless chunks
    effFlagsIsSynth,             // host may assign mixer channels for its outputs
    effFlagsNoSoundInStop,       // does not produce sound when input is all silence
    effFlagsExtIsAsync,          // NOT USED IN 2.4! - for external dsp; plug returns immedeately from process()
                                 // host polls plug position (current block) via effGetCurrentPosition
    effFlagsExtHasBuffer,        // NOT USED IN 2.4! - external dsp, may have their own output buffe (32 bit float)
                                 // host then requests this via effGetDestinationBuffer
    effFlagsCanDoubleReplacing); // plug-in supports double precision processing

  TEffFlags = set of TEffFlag;

  // VST Effect Record /////////////////////////////////////////////////////////
  TVSTEffect = record
    Magic: array [0..3] of char;      // must be kEffectMagic ('VstP')
    Dispatcher: TDispatcherFunc;
    Process: TProcessProc;            // Not used since 2.4, use ProcessReplacing instead!
    SetParameter: TSetParameterProc;
    GetParameter: TGetParameterFunc;
    numPrograms: LongInt;
    numParams: LongInt;               // all programs are assumed to have numParams parameters
    numInputs: LongInt;	              //
    numOutputs: LongInt;	      //
    EffectFlags: TEffFlags;   	      // see constants
    reservedForHost: Pointer;	      // reserved for Host, must be 0 (Dont use it)
    resvd2: LongInt;		      // reserved for Host, must be 0 (Dont use it)
    InitialDelay: LongInt;	      // for algorithms which need input in the first place
    RealQualities: LongInt;	      // number of realtime qualities (0: realtime)
    OffQualities: LongInt;	      // number of offline qualities (0: realtime only)
    ioRatio: LongInt;		      // input samplerate to output samplerate ratio, not used yet
    vObject: pointer;		      // for class access (see AudioEffect.hpp), MUST be 0 else!
    User: pointer;		      // user access
    UniqueID: LongInt;	              // pls choose 4 character as unique as possible.
                                      // this is used to identify an effect for save+load
    Version: longint;		      // (example 1100 for version 1.1.0.0)
    ProcessReplacing: TProcessProc;
    ProcessDoubleReplacing: TProcessDoubleProc;
    Future: array[0..55] of char;     // pls zero
  end;

(*
  TEffFlags = (
    effFlagsHasEditor      = 1,	        // if set, is expected to react to editor messages
    effFlagsHasClip	   = 2,         // NOT USED SINCE 2.4 - return > 1. in getVu() if clipped
    effFlagsHasVu	   = 4,         // NOT USED SINCE 2.4 - return vu value in getVu(); > 1. means clipped
    effFlagsCanMono        = 8,	        // NOT USED SINCE 2.4 - if numInputs == 2, makes sense to be used for mono in
    effFlagsCanReplacing   = 16,        // MUST BE SET! supports in place output (processReplacing() exsists)
    effFlagsProgramChunks  = 32,        // program data are handled in formatless chunks

    effFlagsIsSynth        = 256,       // host may assign mixer channels for its outputs
    effFlagsNoSoundInStop  = 512,       // does not produce sound when input is all silence
    effFlagsExtIsAsync     = 1024,      // NOT USED IN 2.4! - for external dsp; plug returns immedeately from process()
                                        // host polls plug position (current block) via effGetCurrentPosition
    effFlagsExtHasBuffer   = 2048,      // NOT USED IN 2.4! - external dsp, may have their own output buffe (32 bit float)
                                        // host then requests this via effGetDestinationBuffer
    effFlagsCanDoubleReplacing = 4096); // plug-in supports double precision processing
*)

// VstEvent types ////////////////////////////////////////////////////////////
const
  kVstMidiType      = 1;  // midi event, can be cast as VstMidiEvent (see below)
  kVstAudioType     = 2;  // audio
  kVstVideoType     = 3;  // video
  kVstParameterType = 4;  // parameter
  kVstTriggerType   = 5;  // trigger
  kVstSysExType     = 6;  // midi system exclusive
   // ...etc

// dispatcher opCodes ////////////////////////////////////////////////////////
const
  effOpen            =  0;  // initialise
  effClose           =  1;  // exit, release all memory and other resources!
  effSetProgram      =  2;  // program no in <value>
  effGetProgram      =  3;  // return current program no.
  effSetProgramName  =  4;  // user changed program name (max 24 char + 0) to as passed in string
  effGetProgramName  =  5;  // stuff program name (max 24 char + 0) into string
  effGetParamLabel   =  6;  // stuff parameter <index> label (max 8 char + 0) into string
                            // (examples: sec, dB, type)
  effGetParamDisplay =  7;  // stuff parameter <index> textual representation into string
                            // (examples: 0.5, -3, PLATE)
  effGetParamName    =  8;  // stuff parameter <index> label (max 8 char + 0) into string
                            // (examples: Time, Gain, RoomType)
  effGetVu           =  9;  // NOT USED SINCE 2.4 - called if (flags & (effFlagsHasClip | effFlagsHasVu))

  // system
  effSetSampleRate   = 10;  // in opt (float value in Hz; for example 44100.0Hz)
  effSetBlockSize    = 11;  // in value (this is the maximun size of an audio block,
                            // pls check sampleframes in process call)
  effMainsChanged    = 12;  // the user has switched the 'power on' button to
                            // value (0 off, else on). This only switches audio
                            // processing; you should flush delay buffers etc.
  // editor
  effEditGetRect     = 13;  // stuff rect (top, left, bottom, right) into ptr
  effEditOpen        = 14;  // system dependant Window pointer in ptr
  effEditClose       = 15;  // no arguments
  effEditDraw        = 16;  // NOT USED SINCE 2.4 - draw method, ptr points to rect  (MAC only)
  effEditMouse       = 17;  // NOT USED SINCE 2.4 - index: x, value: y (MAC only)
  effEditKey         = 18;  // NOT USED SINCE 2.4 - system keycode in value
  effEditIdle        = 19;  // no arguments. Be gentle!
  effEditTop         = 20;  // NOT USED SINCE 2.4 - window has topped, no arguments
  effEditSleep       = 21;  // NOT USED SINCE 2.4 - window goes to background

  // new
  effIdentify        = 22;  // NOT USED SINCE 2.4 - returns 'NvEf'
  effGetChunk        = 23;  // host requests pointer to chunk into (void**)ptr, byteSize returned
  effSetChunk        = 24;  // plug-in receives saved chunk, byteSize passed

  // VstEvents
  effProcessEvents   = 25;  // VstEvents* in <ptr>

  // parameters and programs
  effCanBeAutomated   = 26; // parameter index in <index>
  effString2Parameter = 27; // parameter index in <index>, string in <ptr>
  effGetNumProgramCategories = 28; // NOT USED IN 2.4 - no arguments. this is for dividing programs into groups (like GM)
  effGetProgramNameIndexed = 29;   // get program name of category <value>, program <index> into <ptr>.
                                   // category (that is, <value>) may be -1, in which case program indices
                                   // are enumerated linearily (as usual); otherwise, each category starts
                                   // over with index 0.
  effCopyProgram = 30;   // NOT USED IN 2.4 - copy current program to destination <index>, note: implies setParameter
                         // connections, configuration
  effConnectInput = 31;	 // NOT USED IN 2.4 - input at <index> has been (dis-)connected; <value> == 0: disconnected, else connected
  effConnectOutput = 32; // NOT USED IN 2.4 - same as input
  effGetInputProperties = 33;  // <index>, VstPinProperties* in ptr, return != 0 => true
  effGetOutputProperties = 34; // dto
  effGetPlugCategory = 35;     // no parameter, return value is category

  // realtime
  effGetCurrentPosition = 36;   // NOT USED IN 2.4 - for external dsp, see flag bits below
  effGetDestinationBuffer = 37; // NOT USED IN 2.4 - for external dsp, see flag bits below. returns float*

  // offline
  effOfflineNotify = 38;  // ptr = VstAudioFile array, value = count, index = start flag
  effOfflinePrepare = 39; // ptr = VstOfflineTask array, value = count
  effOfflineRun = 40;     // dto
  // other

  effProcessVarIo = 41;          // VstVariableIo* in <ptr>
  effSetSpeakerArrangement = 42; // VstSpeakerArrangement* pluginInput in <value>
                                 // VstSpeakerArrangement* pluginOutput in <ptr>
  effSetBlockSizeAndSampleRate = 43; // NOT USED IN 2.4 - block size in <value>, sampleRate in <opt>
  effSetBypass = 44;          	     // onOff in <value> (0 = off)
  effGetEffectName = 45;             // char* name (max 32 bytes) in <ptr>
  effGetErrorText = 46;              // NOT USED IN 2.4 - char* text (max 256 bytes) in <ptr>
  effGetVendorString = 47;           // fills <ptr> with a string identifying the vendor (max 64 char)
  effGetProductString = 48;          // fills <ptr> with a string with product name (max 64 char)
  effGetVendorVersion = 49;          // returns vendor-specific version
  effVendorSpecific = 50;            // no definition, vendor specific handling
  effCanDo = 51;              	     // <ptr>
  effGetTailSize = 52;	             // returns tail size; 0 is default (return 1 for 'no tail')
  effIdle = 53;	                     // idle call in response to audioMasterneedIdle. must
                                     // NOT USED IN 2.4 - return 1 to keep idle calls beeing issued
  // gui
  effGetIcon = 54;	             // NOT USED IN 2.4 - void* in <ptr>, not yet defined
  effSetViewPosition = 55;           // NOT USED IN 2.4 - set view position (in window) to x <index> y <value>

  // and...
  effGetParameterProperties = 56;    // of param <index>, VstParameterProperties* in <ptr>
  effKeysRequired = 57;	             // NOT USED IN 2.4 - returns 0: needs keys (default for 1.0 plugs), 1: don't need
  effGetVstVersion = 58;	     // returns 2; older versions return 0

  effEditKeyDown = 59;               // character in <index>, virtual in <value>, modifiers in <opt>  return -1 if not used, return 1 if used
  effEditKeyUp = 60;                 // character in <index>, virtual in <value>, modifiers in <opt>  return -1 if not used, return 1 if used
  effSetEditKnobMode = 61;           // mode in <value>: 0: circular, 1:circular relativ, 2:linear

  // midi plugins channeldependent programs
  effGetMidiProgramName = 62;        // passed <ptr> points to MidiProgramName struct.
                                     // struct will be filled with information for 'thisProgramIndex'.
                                     // returns number of used programIndexes.
                                     // if 0 is returned, no MidiProgramNames supported.

  effGetCurrentMidiProgram = 63;  // returns the programIndex of the current program.
                                  // passed <ptr> points to MidiProgramName struct.
                                  // struct will be filled with information for the current program.

  effGetMidiProgramCategory = 64; // passed <ptr> points to MidiProgramCategory struct.
                                  // struct will be filled with information for 'thisCategoryIndex'.
                                  // returns number of used categoryIndexes.
                                  // if 0 is returned, no MidiProgramCategories supported.

  effHasMidiProgramsChanged = 65; // returns 1 if the MidiProgramNames or MidiKeyNames
                                  // had changed on this channel, 0 otherwise. <ptr> ignored.

  effGetMidiKeyName = 66;         // passed <ptr> points to MidiKeyName struct.
                                  // struct will be filled with information for 'thisProgramIndex' and
                                  // 'thisKeyNumber'. If keyName is "" the standard name of the key
                                  // will be displayed. If 0 is returned, no MidiKeyNames are defined for 'thisProgramIndex'.

  effBeginSetProgram = 67;        // called before a new program is loaded
  effEndSetProgram = 68;          // called when the program is loaded

  effGetSpeakerArrangement = 69;   // VstSpeakerArrangement** pluginInput in <value>
                                   // VstSpeakerArrangement** pluginOutput in <ptr>
  effShellGetNextPlugin = 70;      // This opcode is only called, if plugin is of type kPlugCategShell.
                                   // returns the next plugin's uniqueID.
                                   // <ptr> points to a char buffer of size 64, which is to be filled
                                   // with the name of the plugin including the terminating zero.
  effStartProcess = 71;            // Called before the start of process call
  effStopProcess = 72;             // Called after the stop of process call
  effSetTotalSampleToProcess = 73; // Called in offline (non RealTime) Process before process is called, indicates how many sample will be processed
  effSetPanLaw = 74;               // PanLaw : Type (Linear, Equal Power,.. see enum PanLaw Type) in <value>,
                                   // Gain in <opt>: for Linear : [1.0 => 0dB PanLaw], [~0.58 => -4.5dB], [0.5 => -6.02dB]
  effBeginLoadBank = 75;           // Called before a Bank is loaded, <ptr> points to VstPatchChunkInfo structure
                                   // return -1 if the Bank can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
  effBeginLoadProgram = 76;        // Called before a Program is loaded, <ptr> points to VstPatchChunkInfo structure
                                   // return -1 if the Program can not be loaded, return 1 if it can be loaded else 0 (for compatibility)

// audioMaster opCodes ///////////////////////////////////////////////////////
const
  audioMasterAutomate     = 0;  // index, value, returns 0
  audioMasterVersion      = 1;  // vst version, currently 2 (0 for older), 2400 for VST 2.4!
  audioMasterCurrentId    = 2;  // returns the unique id of a plug that's currently
                                // loading
  audioMasterIdle         = 3;  // call application idle routine (this will
                                // call effEditIdle for all open editors too)
  audioMasterPinConnected = 4;  // inquire if an input or output is beeing connected;
                                // index enumerates input or output counting from zero,
                                // value is 0 for input and != 0 otherwise. note: the
                                // return value is 0 for <true> such that older versions

  // VstEvents + VstTimeInfo
  audioMasterWantMidi = 6;      // <value> is a filter which is currently ignored
  audioMasterGetTime = 7;       // returns const VstTimeInfo* (or 0 if not supported)
                                // <value> should contain a mask indicating which fields are required
                                // (see valid masks above), as some items may require extensive conversions
  audioMasterProcessEvents = 8; // VstEvents* in <ptr>
  audioMasterSetTime = 9;	      // NOT USED IN 2.4 - VstTimenfo* in <ptr>, filter in <value>, not supported
  audioMasterTempoAt = 10;      // NOT USED IN 2.4 - returns tempo (in bpm * 10000) at sample frame location passed in <value>

  // parameters
  audioMasterGetNumAutomatableParameters = 11; // NOT USED IN 2.4
  audioMasterGetParameterQuantization = 12;    // NOT USED IN 2.4 - returns the integer value for +1.0 representation,
                                               // or 1 if full single float precision is maintained in automation. parameter index in <value> (-1: all, any) connections, configuration
  audioMasterIOChanged = 13;		       // numInputs and/or numOutputs has changed
  audioMasterNeedIdle = 14;		       // NOT USED IN 2.4 - plug needs idle calls (outside its editor window)
  audioMasterSizeWindow = 15;		       // index: width, value: height
  audioMasterGetSampleRate = 16;
  audioMasterGetBlockSize = 17;
  audioMasterGetInputLatency = 18;
  audioMasterGetOutputLatency = 19;
  audioMasterGetPreviousPlug = 20;	       // NOT USED IN 2.4 - input pin in <value> (-1: first to come), returns cEffect*
  audioMasterGetNextPlug = 21;	    	       // NOT USED IN 2.4 - output pin in <value> (-1: first to come), returns cEffect*

  // realtime info
  audioMasterWillReplaceOrAccumulate = 22;     // NOT USED IN 2.4 - returns: 0: not supported, 1: replace, 2: accumulate
  audioMasterGetCurrentProcessLevel = 23;      // returns: 0: not supported,
                                               // 1: currently in user thread (gui)
                                               // 2: currently in audio thread (where process is called)
                                               // 3: currently in 'sequencer' thread (midi, timer etc)
                                               // 4: currently offline processing and thus in user thread
                                               // other: not defined, but probably pre-empting user thread.
  audioMasterGetAutomationState = 24;	       // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

  // offline
  audioMasterOfflineStart = 25;
  audioMasterOfflineRead = 26;		       // ptr points to offline structure, see below. return 0: error, 1 ok
  audioMasterOfflineWrite = 27;                // same as read
  audioMasterOfflineGetCurrentPass = 28;
  audioMasterOfflineGetCurrentMetaPass = 29;

  // other
  audioMasterSetOutputSampleRate = 30;	       // NOT USED IN 2.4 - for variable i/o, sample rate in <opt>
  audioMasterGetOutputSpeakerArrangement = 31; // NOT USED IN 2.4 - result in ret
  audioMasterGetVendorString = 32;	       // fills <ptr> with a string identifying the vendor (max 64 char)
  audioMasterGetProductString = 33;	       // fills <ptr> with a string with product name (max 64 char)
  audioMasterGetVendorVersion = 34;	       // returns vendor-specific version
  audioMasterVendorSpecific = 35;	       // no definition, vendor specific handling
  audioMasterSetIcon = 36;		       // NOT USED IN 2.4 - void* in <ptr>, format not defined yet
  audioMasterCanDo = 37;		       // string in ptr, see below
  audioMasterGetLanguage = 38;		       // see enum
  audioMasterOpenWindow = 39;		       // NOT USED IN 2.4 - returns platform specific ptr
  audioMasterCloseWindow = 40;		       // NOT USED IN 2.4 - close window, platform specific handle in <ptr>
  audioMasterGetDirectory = 41;		       // get plug directory, FSSpec on MAC, else char*
  audioMasterUpdateDisplay = 42;	       // something has changed, update 'multi-fx' display

  //---from here VST 2.1 extension opcodes------------------------------------------------------
  audioMasterBeginEdit        = 43;            // begin of automation session (when mouse down), parameter index in <index>
  audioMasterEndEdit          = 44;            // end of automation session (when mouse up),     parameter index in <index>
  audioMasterOpenFileSelector = 45;	       // open a fileselector window with VstFileSelect* in <ptr>

  //---from here VST 2.2 extension opcodes------------------------------------------------------
  audioMasterVST22 = 46;
  audioMasterCloseFileSelector = 46;           // close a fileselector operation with VstFileSelect* in <ptr>: Must be always called after an open !
  audioMasterEditFile          = 47;           // NOT USED IN 2.4 - open an editor for audio (defined by XML text in ptr)
  audioMasterGetChunkFile      = 48;           // NOT USED IN 2.4 - get the native path of currently loading bank or project
                                               // (called from writeChunk) void* in <ptr> (char[2048], or sizeof(FSSpec))
  audioMasterGetInputSpeakerArrangement = 49;  // NOT USED IN 2.4 - result a VstSpeakerArrangement in ret
                                               // will always return true.

type
  PVstEvent = ^TVstEvent;
  TVstEvent = packed record    // a generic timestamped event
    vType       : longint;    // see enum below
    byteSize    : longint;    // of this event, excl. type and byteSize
    deltaFrames : longint;	// sample frames related to the current block start sample position
    flags       : longint;    // generic flags, none defined yet (0)
    data        : array[0..15] of byte;  // size may vary but is usually 16
  end;

  PVstEvents = ^TVstEvents;
  TVstEvents = packed record    // a block of events for the current audio block
    numEvents : longint;
    reserved  : longint;                   // zero
    events    : array[0..2047] of PVstEvent;  // variable
  end;

  // defined events ////////////////////////////////////////////////////////////
  PVstMidiEvent = ^TVstMidiEvent;
  TVstMidiEvent = packed record // to be casted from a VstEvent
    vType           : longint;    // kVstMidiType
    byteSize        : longint;    // 24
    deltaFrames     : longint;    // sample frames related to the current block start sample position
    flags           : longint;    // see below
    noteLength      : longint;    // (in sample frames) of entire note, if available, else 0
    noteOffset      : longint;    // offset into note from note start if available, else 0
    midiData        : array[0..3] of byte;  // 1 thru 3 midi bytes; midiData[3] is reserved (zero)
    detune          : byte;		      // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    noteOffVelocity : byte;
    reserved1       : byte;		      // zero
    reserved2       : byte;      	      // zero
  end;

  // VstMidiEventFlag //////////////////////////////////////////////////////////
const
  kVstMidiEventIsRealtime = 1;

// VstTimeInfo ///////////////////////////////////////////////////////////////
//
// VstTimeInfo as requested via audioMasterGetTime (getTimeInfo())
// refers to the current time slice. note the new slice is
// already started when processEvents() is called

type
  PVstTimeInfo = ^TVstTimeInfo;
  TVstTimeInfo = packed record
    samplePos: Double;         // current location
    sampleRate: Double;
    nanoSeconds: Double;       // system time
    ppqPos: Double;            // 1 ppq
    tempo: Double;             // in bpm
    barStartPos: Double;       // last bar start, in 1 ppq
    cycleStartPos: Double;     // 1 ppq
    cycleEndPos: Double;       // 1 ppq
    timeSigNumerator: longint; // time signature
    timeSigDenominator: longint;
    smpteOffset: longint;
    smpteFrameRate: longint;   // 0:24, 1:25, 2:29.97, 3:30, 4:29.97 df, 5:30 df
    samplesToNextClock: longint; // midi clock resolution (24 ppq), can be negative
    flags: longint;              // see below
  end;

const
  kVstTransportChanged     = 1; // Indicates that Playing, Cycle or Recording has changed
  kVstTransportPlaying     = 2;
  kVstTransportCycleActive = 4;
  kVstTransportRecording   = 8;
  kVstAutomationWriting    = 64;
  kVstAutomationReading    = 128;

  // flags which indicate which of the fields in this VstTimeInfo
  //  are valid; samplePos and sampleRate are always valid
  kVstNanosValid           = 1 shl 8;    // 1 << 8
  kVstPpqPosValid          = 1 shl 9;    // 1 << 9
  kVstTempoValid           = 1 shl 10;   // 1 << 10
  kVstBarsValid            = 1 shl 11;   // 1 << 11
  kVstCyclePosValid        = 1 shl 12;   // 1 << 12  // start and end
  kVstTimeSigValid         = 1 shl 13;   // 1 << 13
  kVstSmpteValid           = 1 shl 14;   // 1 << 14
  kVstClockValid           = 1 shl 15;   // 1 << 15

type
  // Variable IO for Offline Processing ////////////////////////////////////////
  PVstVariableIo = ^TVstVariableIo;
  TVstVariableIo = packed record
    inputs: PPSingle;
    outputs: PPSingle;
    numSamplesInput: longint;
    numSamplesOutput: longint;
    numSamplesInputProcessed: ^longint;
    numSamplesOutputProcessed: ^longint;
  end;

  // Language //////////////////////////////////////////////////////////////////
  TVstHostLanguage = (
    kVstLangUnknown  = 0,
    kVstLangEnglish  = 1,
    kVstLangGerman   = 2,
    kVstLangFrench   = 3,
    kVstLangItalian  = 4,
    kVstLangSpanish  = 5,
    kVstLangJapanese = 6);

  // Parameter Properties //////////////////////////////////////////////////////
  PVstParameterProperties = ^TVstParameterProperties;
  TVstParameterProperties = packed record
    stepFloat        : single;
    smallStepFloat   : single;
    largeStepFloat   : single;
    vLabel           : array[0..63] of char;
    flags            : longint;    // see below
    minInteger       : longint;
    maxInteger       : longint;
    stepInteger      : longint;
    largeStepInteger : longint;
    shortLabel       : array[0..7] of char;   // recommended: 6 + delimiter

    // the following are for remote controller display purposes.
    // note that the kVstParameterSupportsDisplayIndex flag must be set.
    // host can scan all parameters, and find out in what order
    // to display them:

    displayIndex     : smallint;  // for remote controllers, the index where this parameter
                                  // should be displayed (starting with 0)

    // host can also possibly display the parameter group (category), such as
    // ---------------------------
    // Osc 1
    // Wave  Detune  Octave  Mod
    // ---------------------------
    // if the plug supports it (flag kVstParameterSupportsDisplayCategory)
    category         : smallint;     // 0: no category, else group index + 1
    numParametersInCategory : smallint;
    reserved         : smallint;
    categoryLabel    : array[0..23] of char;    // for instance, "Osc 1"
    future           : array[0..15] of char;
  end;

// Parameter Properties Flags ////////////////////////////////////////////////
const
  kVstParameterIsSwitch                =  1;   // 1 << 0
  kVstParameterUsesIntegerMinMax       =  2;   // 1 << 1
  kVstParameterUsesFloatStep           =  4;   // 1 << 2
  kVstParameterUsesIntStep             =  8;   // 1 << 3
  kVstParameterSupportsDisplayIndex    = 16;   // 1 << 4
  kVstParameterSupportsDisplayCategory = 32;   // 1 << 5
  kVstParameterCanRamp                 = 64;   // 1 << 6


// Pin Properties ////////////////////////////////////////////////////////////
type
  PVstPinProperties = ^TVstPinProperties;
  TVstPinProperties = packed record
    vLabel          : array[0..63] of char;
    flags           : longint;              // see pin properties flags
    arrangementType : longint;
    shortLabel      : array[0..7] of char;  // recommended: 6 + delimiter
    future          : array[0..47] of byte;
  end;

// Pin Properties Flags //////////////////////////////////////////////////////
const
  kVstPinIsActive   = 1;
  kVstPinIsStereo   = 2;
  kVstPinUseSpeaker = 4;

// Plugin Category ///////////////////////////////////////////////////////////
type
  TVstPlugCategory = longint;

const
  kPlugCategUnknown        = 0;
  kPlugCategEffect         = 1;
  kPlugCategSynth          = 2;
  kPlugCategAnalysis       = 3;
  kPlugCategMastering      = 4;
  kPlugCategSpacializer    = 5;     // 'panners'
  kPlugCategRoomFx         = 6;     // delays and reverbs
  kPlugSurroundFx          = 7;     // dedicated surround processor
  kPlugCategRestoration    = 8;
  kPlugCategOfflineProcess = 9;
  kPlugCategShell          = 10;    // plugin which is only a container of plugins.
  kPlugCategGenerator      = 11;

// Midi Plugins Channel Dependent Programs ///////////////////////////////////
type
  PMidiProgramName = ^MidiProgramName;
  MidiProgramName = packed record
    thisProgramIndex      : longint;  // >= 0. fill struct for this program index.
    name                  : array[0..63] of char;
    midiProgram           : shortint;	// -1:off, 0-127
    midiBankMsb           : shortint;	// -1:off, 0-127
    midiBankLsb           : shortint;	// -1:off, 0-127
    reserved              : byte;     // zero
    parentCategoryIndex   : longint;  // -1:no parent category
    flags                 : longint;  // omni etc, see below
  end;

// MidiProgramName Flags /////////////////////////////////////////////////////
const
  kMidiIsOmni = 1;  // default is multi. for omni mode, channel 0
                    // is used for inquiries and program changes

// MidiProgramName ///////////////////////////////////////////////////////////
type
  PMidiProgramCategory = ^MidiProgramCategory;
  MidiProgramCategory = packed record
    thisCategoryIndex   : longint;      // >= 0. fill struct for this category index.
    name                : array[0..63] of char;
    parentCategoryIndex : longint;      // -1:no parent category
    flags               : longint;      // reserved, none defined yet, zero.
  end;

// MidiKeyName ///////////////////////////////////////////////////////////////
type
  PMidiKeyName = ^MidiKeyName;
  MidiKeyName = packed record
    thisProgramIndex : longint;    // >= 0. fill struct for this program index.
    thisKeyNumber    : longint;    // 0 - 127. fill struct for this key number.
    keyName          : array[0..63] of char;
    reserved         : longint;    // zero
    flags            : longint;    // reserved, none defined yet, zero.
  end;

// surround setup ////////////////////////////////////////////////////////////

// Speaker Properties ////////////////////////////////////////////////////////
type
  PVstSpeakerProperties = ^TVstSpeakerProperties;
  TVstSpeakerProperties = record
                                      // units:      range:            except:
    azimuth   : single;               // rad         -PI...PI		10.f for LFE channel
    elevation : single;               // rad         -PI/2...PI/2	10.f for LFE channel
    radius    : single;               // meter                          0.f for LFE channel
    reserved  : single;               // 0.
    name      : array[0..63] of char; // for new setups, new names should be given (L/R/C... won't do)
    vType     : longint;              // speaker type
    future    : array[0..27] of byte;
  end;

// note: the origin for azimuth is right (as by math conventions dealing with radians);
// the elevation origin is also right, visualizing a rotation of a circle across the
// -pi/pi axis of the horizontal circle. thus, an elevation of -pi/2 corresponds
// to bottom, and a speaker standing on the left, and 'beaming' upwards would have
// an azimuth of -pi, and an elevation of pi/2.
// for user interface representation, grads are more likely to be used, and the
// origins will obviously 'shift' accordingly.

//---Speaker Arrangement---------------------------
type
  PVstSpeakerArrangement = ^TVstSpeakerArrangement;
  TVstSpeakerArrangement = record
    vType       : longint;                               // (was float lfeGain) LFE channel gain is adjusted [dB] higher than other channels)
    numChannels : longint;                               // number of channels in this speaker arrangement
    speakers    : array[0..7] of TVstSpeakerProperties;  // variable
  end;


//---Speaker Types---------------------------------
type
  TVstSpeakerType = longint;

const
  kSpeakerUndefined = $7fffffff;       // Undefined
  kSpeakerM         = 0;               // Mono (M)
  kSpeakerL         = 1;               // Left (L)
  kSpeakerR         = 2;               // Right (R)
  kSpeakerC         = 3;               // Center (C)
  kSpeakerLfe       = 4;               // Subbass (Lfe)
  kSpeakerLs        = 5;               // Left Surround (Ls)
  kSpeakerRs        = 6;               // Right Surround (Rs)
  kSpeakerLc        = 7;               // Left of Center (Lc)
  kSpeakerRc        = 8;               // Right of Center (Rc)
  kSpeakerS         = 9;               // Surround (S)
  kSpeakerCs        = kSpeakerS;       // Center of Surround (Cs) = Surround (S)
  kSpeakerSl        = 10;              // Side Left (Sl)
  kSpeakerSr        = 11;              // Side Right (Sr)
  kSpeakerTm        = 12;              // Top Middle (Tm)
  kSpeakerTfl       = 13;              // Top Front Left (Tfl)
  kSpeakerTfc       = 14;              // Top Front Center (Tfc)
  kSpeakerTfr       = 15;              // Top Front Right (Tfr)
  kSpeakerTrl       = 16;              // Top Rear Left (Trl)
  kSpeakerTrc       = 17;              // Top Rear Center (Trc)
  kSpeakerTrr       = 18;              // Top Rear Right (Trr)
  kSpeakerLfe2      = 19;              // Subbass 2 (Lfe2)


// user-defined speaker types (to be extended in the negative range)
// (will be handled as their corresponding speaker types with abs values:
// e.g abs(kSpeakerU1) == kSpeakerL, abs(kSpeakerU2) == kSpeakerR)
const
  kSpeakerU32 = -32;
  kSpeakerU31 = -31;
  kSpeakerU30 = -30;
  kSpeakerU29 = -29;
  kSpeakerU28 = -28;
  kSpeakerU27 = -27;
  kSpeakerU26 = -26;
  kSpeakerU25 = -25;
  kSpeakerU24 = -24;
  kSpeakerU23 = -23;
  kSpeakerU22 = -22;
  kSpeakerU21 = -21;
  kSpeakerU20 = -20;			// == kSpeakerLfe2
  kSpeakerU19 = -19;			// == kSpeakerTrr
  kSpeakerU18 = -18;			// == kSpeakerTrc
  kSpeakerU17 = -17;			// == kSpeakerTrl
  kSpeakerU16 = -16;			// == kSpeakerTfr
  kSpeakerU15 = -15;			// == kSpeakerTfc
  kSpeakerU14 = -14;			// == kSpeakerTfl
  kSpeakerU13 = -13;			// == kSpeakerTm
  kSpeakerU12 = -12;			// == kSpeakerSr
  kSpeakerU11 = -11;			// == kSpeakerSl
  kSpeakerU10 = -10;			// == kSpeakerCs
  kSpeakerU9	 = -9;			// == kSpeakerS
  kSpeakerU8	 = -8;			// == kSpeakerRc
  kSpeakerU7	 = -7;			// == kSpeakerLc
  kSpeakerU6	 = -6;			// == kSpeakerRs
  kSpeakerU5	 = -5;			// == kSpeakerLs
  kSpeakerU4	 = -4;			// == kSpeakerLfe
  kSpeakerU3	 = -3;			// == kSpeakerC
  kSpeakerU2	 = -2;			// == kSpeakerR
  kSpeakerU1	 = -1;			// == kSpeakerL

// Speaker Arrangement Types /////////////////////////////////////////////////
type
  TVstSpeakerArrangementType = (
    satUserDefined     = -2,
    satEmpty           = -1,

    satMono            =  0,  // M

    satStereo          =  1,	 // L R
    satStereoSurround  =  2,	 // Ls Rs
    satStereoCenter	   =  3,  // Lc Rc
    satStereoSide      =  4,  // Sl Sr
    satStereoCLfe      =  5,  // C Lfe

    sat30Cine          =  6,  // L R C
    sat30Music         =  7,  // L R S
    sat31Cine          =  8,  // L R C Lfe
    sat31Music         =  9,  // L R Lfe S

    sat40Cine          = 10,  // L R C   S (LCRS)
    sat40Music         = 11,  // L R Ls  Rs (Quadro)
    sat41Cine          = 12,  // L R C   Lfe S (LCRS+Lfe)
    sat41Music         = 13,  // L R Lfe Ls Rs (Quadro+Lfe)

    sat50              = 14,  // L R C Ls  Rs
    sat51              = 15,  // L R C Lfe Ls Rs

    sat60Cine          = 16,  // L R C   Ls  Rs Cs
    sat60Music         = 17,  // L R Ls  Rs  Sl Sr
    sat61Cine          = 18,  // L R C   Lfe Ls Rs Cs
    sat61Music         = 19,  // L R Lfe Ls  Rs Sl Sr

    sat70Cine          = 20,  // L R C Ls  Rs Lc Rc
    sat70Music         = 21,  // L R C Ls  Rs Sl Sr
    sat71Cine          = 22,  // L R C Lfe Ls Rs Lc Rc
    sat71Music         = 23,  // L R C Lfe Ls Rs Sl Sr

    sat80Cine          = 24,  // L R C Ls  Rs Lc Rc Cs
    sat80Music         = 25,  // L R C Ls  Rs Cs Sl Sr
    sat81Cine          = 26,  // L R C Lfe Ls Rs Lc Rc Cs
    sat81Music         = 27,  // L R C Lfe Ls Rs Cs Sl Sr

    sat102             = 28,  // L R C Lfe Ls Rs Tfl Tfc Tfr Trl Trr Lfe2

    satNumSpeakerArr   = 29);

// Offline Processing ////////////////////////////////////////////////////////
type
  PVstOfflineTask = ^TVstOfflineTask;
  TVstOfflineTask = packed record
    processName: array[0..95] of char;  // set by plug

    // audio access
    readPosition: Double;               // set by plug/host
    writePosition: Double;              // set by plug/host
    readCount: longint;                 // set by plug/host
    writeCount: longint;                // set by plug
    sizeInputBuffer: longint;           // set by host
    sizeOutputBuffer: longint;          // set by host
    inputBuffer: pointer;               // set by host
    outputBuffer: pointer;              // set by host
    positionToProcessFrom: Double;      // set by host
    numFramesToProcess: Double;         // set by host
    maxFramesToWrite: Double;           // set by plug

    // other data access
    extraBuffer: pointer;               // set by plug
    value: longint;                     // set by host or plug
    index: longint;                     // set by host or plug

    // file attributes
    numFramesInSourceFile: Double;      // set by host
    sourceSampleRate: Double;           // set by host or plug
    destinationSampleRate: Double;      // set by host or plug
    numSourceChannels: longint;         // set by host or plug
    numDestinationChannels: longint;    // set by host or plug
    sourceFormat: longint;              // set by host
    destinationFormat: longint;         // set by plug
    outputText: array[0..511] of char;  // set by plug or host

    // progress notification
    progress: Double;                   // set by plug
    progressMode: longint;              // reserved for future
    progressText: array[0..99] of char; // set by plug

    flags: longint;         // set by host and plug; see TVstOfflineTaskFlags
    returnValue: longint;   // reserved for future
    hostOwned: pointer;     // set by host
    plugOwned: pointer;     // set by plug

    future: array[0..1023] of byte;
  end;

// VstOfflineTask Flags //////////////////////////////////////////////////////
type
  TVstOfflineTaskFlags = longint;

const
  // set by host
  kVstOfflineUnvalidParameter = 1;
  kVstOfflineNewFile          = 2;

  // set by plug
  kVstOfflinePlugError        = 1024;
  kVstOfflineInterleavedAudio = 2048;
  kVstOfflineTempOutputFile   = 4096;
  kVstOfflineFloatOutputFile  = 8192;
  kVstOfflineRandomWrite      = 16384;
  kVstOfflineStretch          = 32768;
  kVstOfflineNoThread         = 65536;

// Option passed to offlineRead/offlineWrite
type
  TVstOfflineOption = longint;

const
  kVstOfflineAudio      = 0;  // reading/writing audio samples
  kVstOfflinePeaks	     = 1;  // reading graphic representation
  kVstOfflineParameter  = 2;  // reading/writing parameters
  kVstOfflineMarker     = 3;  // reading/writing marker
  kVstOfflineCursor     = 4;  // reading/moving edit cursor
  kVstOfflineSelection  = 5;  // reading/changing selection
  kVstOfflineQueryFiles = 6;  // to request the host to call asynchronously offlineNotify

// Structure passed to offlineNotify and offlineStart ////////////////////////
type
  PVstAudioFile = ^TVstAudioFile;
  TVstAudioFile = packed record
    flags: longint;		  	// see enum TVstAudioFileFlags
    hostOwned: pointer;                 // any data private to host
    plugOwned: pointer;                 // any data private to plugin
    name: array[0..99] of char; 	// file title
    uniqueId: longint;                  // uniquely identify a file during a session
    sampleRate: Double;                 // file sample rate
    numChannels: longint;               // number of channels (1 for mono, 2 for stereo...)
    numFrames: Double;                  // number of frames in the audio file
    format: longint;		  	// reserved for future
    editCursorPosition: Double;	        // -1 if no such cursor
    selectionStart: Double;		// frame index of first selected frame, or -1
    selectionSize: Double;		// number of frames in selection, or 0
    selectedChannelsMask: longint;	// 1 bit per channel
    numMarkers: longint;   		// number of markers in the file
    timeRulerUnit: longint;		// see doc for possible values
    timeRulerOffset: Double;	        // offset in time ruler (positive or negative)
    tempo: Double;			// as bpm
    timeSigNumerator: longint;	        // time signature numerator
    timeSigDenominator: longint;	// time signature denominator
    ticksPerBlackNote: longint;	        // resolution
    smpteFrameRate: longint;		// smpte rate (set as in TVstTimeInfo)

    future: array[0..63] of byte;
  end;


// VstAudioFile Flags
type
  TVstAudioFileFlags = longint;

const
  // set by host (in call offlineNotify)
  kVstOfflineReadOnly	           = 1;
  kVstOfflineNoRateConversion      = 2;
  kVstOfflineNoChannelChange	   = 4;

  // Set by plug (in function offlineStart)
  kVstOfflineCanProcessSelection   = 1024;
  kVstOfflineNoCrossfade	   = 2048;
  kVstOfflineWantRead	           = 4096;
  kVstOfflineWantWrite	           = 8192;
  kVstOfflineWantWriteMarker	   = 16384;
  kVstOfflineWantMoveCursor	   = 32768;
  kVstOfflineWantSelect	           = 65536;

// VstAudioFileMarker ////////////////////////////////////////////////////////
type
  PVstAudioFileMarker = ^TVstAudioFileMarker;
  TVstAudioFileMarker = packed record
    Position: Double;
    Name: array[0..31] of char;
    vType: longint;
    ID: longint;
    Reserved: longint;
  end;

// Structure used for openWindow and closeWindow /////////////////////////////
type
  PVstWindow = ^TVstWindow;
  TVstWindow = packed record
    Title: array[0..127] of char;    // title
    xPos: smallint;                  // position and size
    yPos: smallint;
    Width: smallint;
    Height: smallint;
    Style: longint;                  // 0: with title, 1: without title

    Parent: pointer;                 // parent of this window
    userHandle: pointer;             // reserved
    winHandle: pointer;              // reserved

    Future: array[0..103] of byte;
  end;


// Structure and enum used for keyUp/keyDown /////////////////////////////////
type
  PVstKeyCode = ^TVstKeyCode;
  TVstKeyCode = packed record
    Character : longint;
    Virt      : byte;                // see enum TVstVirtualKey
    Modifier  : byte;                // see enum TVstModifierKey
  end;

// Used by member virt of VstKeyCode /////////////////////////////////////////
type
  TVstVirtualKey = longint;

const
  VKEY_BACK         = 1;
  VKEY_TAB          = 2;
  VKEY_CLEAR        = 3;
  VKEY_RETURN       = 4;
  VKEY_PAUSE        = 5;
  VKEY_ESCAPE       = 6;
  VKEY_SPACE        = 7;
  VKEY_NEXT         = 8;
  VKEY_END          = 9;
  VKEY_HOME         = 10;

  VKEY_LEFT         = 11;
  VKEY_UP           = 12;
  VKEY_RIGHT        = 13;
  VKEY_DOWN         = 14;
  VKEY_PAGEUP       = 15;
  VKEY_PAGEDOWN     = 16;

  VKEY_SELECT       = 17;
  VKEY_PRINT        = 18;
  VKEY_ENTER        = 19;
  VKEY_SNAPSHOT     = 20;
  VKEY_INSERT       = 21;
  VKEY_DELETE       = 22;
  VKEY_HELP         = 23;
  VKEY_NUMPAD0      = 24;
  VKEY_NUMPAD1      = 25;
  VKEY_NUMPAD2      = 26;
  VKEY_NUMPAD3      = 27;
  VKEY_NUMPAD4      = 28;
  VKEY_NUMPAD5      = 29;
  VKEY_NUMPAD6      = 30;
  VKEY_NUMPAD7      = 31;
  VKEY_NUMPAD8      = 32;
  VKEY_NUMPAD9      = 33;
  VKEY_MULTIPLY     = 34;
  VKEY_ADD          = 35;
  VKEY_SEPARATOR    = 36;
  VKEY_SUBTRACT     = 37;
  VKEY_DECIMAL      = 38;
  VKEY_DIVIDE       = 39;
  VKEY_F1           = 40;
  VKEY_F2           = 41;
  VKEY_F3           = 42;
  VKEY_F4           = 43;
  VKEY_F5           = 44;
  VKEY_F6           = 45;
  VKEY_F7           = 46;
  VKEY_F8           = 47;
  VKEY_F9           = 48;
  VKEY_F10          = 49;
  VKEY_F11          = 50;
  VKEY_F12          = 51;
  VKEY_NUMLOCK      = 52;
  VKEY_SCROLL       = 53;

  VKEY_SHIFT        = 54;
  VKEY_CONTROL      = 55;
  VKEY_ALT          = 56;

  VKEY_EQUALS       = 57;


// Used by member modifier of VstKeyCode /////////////////////////////////////
type
  VstModifierKey = longint;

const
  MODIFIER_SHIFT     = 1 shl 0; // Shift
  MODIFIER_ALTERNATE = 1 shl 1; // Alt
  MODIFIER_COMMAND   = 1 shl 2; // Control on Mac
  MODIFIER_CONTROL   = 1 shl 3; // Ctrl on PC, Apple on Mac



// Used by audioMasterOpenFileSelector ///////////////////////////////////////
type
  PVstFileType = ^TVstFileType;
  TVstFileType = packed record
    name      : array[0..127] of char;
    macType   : array[0..7] of char;
    dosType   : array[0..7] of char;
    unixType  : array[0..7] of char;
    mimeType1 : array[0..127] of char;
    mimeType2 : array[0..127] of char;
  end;

  PVstFileSelect = ^TVstFileSelect;
  TVstFileSelect = packed record
    command              : longint;         // see enum kVstFileLoad....
    vType                : longint;         // see enum kVstFileType...

    macCreator           : longint;         // optional: 0 = no creator

    nbFileTypes          : longint;         // nb of fileTypes to used
    fileTypes            : PVstFileType;    // list of fileTypes

    title                : array[0..1023] of char;  // text display in the file selector's title

    initialPath          : pchar;   // initial path

    returnPath           : pchar;   // use with kVstFileLoad and kVstDirectorySelect
                                    // if null is passed, the host will allocated memory
                                    // the plugin should then called closeOpenFileSelector for freeing memory
    sizeReturnPath       : longint;

    returnMultiplePaths  : ^pchar;  // use with kVstMultipleFilesLoad
                                    // the host allocates this array. The plugin should then called closeOpenFileSelector for freeing memory
    nbReturnPath         : longint; // number of selected paths

    reserved             : longint; // reserved for host application
    future               : array[0..115] of byte;   // future use
  end;

const
  kVstFileLoad          = 0;
  kVstFileSave          = 1;
  kVstMultipleFilesLoad = 2;
  kVstDirectorySelect   = 3;

  kVstFileType          = 0;


// Structure used for effBeginLoadBank/effBeginLoadProgram ///////////////////
type
  PVstPatchChunkInfo = ^TVstPatchChunkInfo;
  TVstPatchChunkInfo = packed record
    version        : longint;               // Format Version (should be 1)
    pluginUniqueID : longint;               // UniqueID of the plugin
    pluginVersion  : longint;               // Plugin Version
    numElements    : longint;	            // Number of Programs (Bank) or Parameters (Program)
    future         : array[0..47] of char;
  end;


// PanLaw Type ///////////////////////////////////////////////////////////////
type
  TVstPanLawType = (
    kLinearPanLaw     = 0,   // L = pan * M; R = (1 - pan) * M;
    kEqualPowerPanLaw = 1);  // L = pow (pan, 0.5) * M; R = pow ((1 - pan), 0.5) * M;

const
  cMagic           = 'CcnK';
  presetMagic      = 'FxCk';
  bankMagic        = 'FxBk';
  chunkPresetMagic = 'FPCh';
  chunkBankMagic   = 'FBCh';
  chunkGlobalMagic = 'FxCh'; // not used
  fMagic           = presetMagic;

type
  //--------------------------------------------------------------------
  // For Preset (Program) (.fxp) without chunk (magic = 'FxCk')
  //--------------------------------------------------------------------
  TFXPreset = packed record
    chunkMagic : longint;   // 'CcnK'
    byteSize   : longint;   // of this chunk, excl. magic + byteSize

    fxMagic    : longint;   // 'FxCk'
    version    : longint;
    fxID       : longint;   // fx unique id
    fxVersion  : longint;

    numParams  : longint;
    prgName    : array[0..27] of char;
    params     : pointer; //array[0..0] of single;    // variable no. of parameters
  end;

  //--------------------------------------------------------------------
  // For Preset (Program) (.fxp) with chunk (magic = 'FPCh')
  //--------------------------------------------------------------------
  TFXChunkSet = packed record
    chunkMagic  : longint;                // 'CcnK'
    byteSize    : longint;                // of this chunk, excl. magic + byteSize

    fxMagic     : longint;                // 'FPCh'
    version     : longint;
    fxID        : longint;                // fx unique id
    fxVersion   : longint;

    numPrograms : longint;
    prgName     : array[0..27] of char;

    chunkSize   : longint;
    chunk       : pointer; //array[0..7] of char;    // variable
  end;

  //--------------------------------------------------------------------
  // For Bank (.fxb) without chunk (magic = 'FxBk')
  //--------------------------------------------------------------------
  TFXSet = packed record
    chunkMagic  : longint;                   // 'CcnK'
    byteSize    : longint;                   // of this chunk, excl. magic + byteSize

    fxMagic     : longint;                   // 'FxBk'
    version     : longint;
    fxID        : longint;                   // fx unique id
    fxVersion   : longint;

    numPrograms : longint;
    future      : array[0..127] of byte;

    programs    : pointer;//array[0..0] of fxPreset;  // variable no. of programs
  end;


  //--------------------------------------------------------------------
  // For Bank (.fxb) with chunk (magic = 'FBCh')
  //--------------------------------------------------------------------
  TFXChunkBank = packed record
    chunkMagic  : longint;                // 'CcnK'
    byteSize    : longint;                // of this chunk, excl. magic + byteSize

    fxMagic     : longint;                // 'FBCh'
    version     : longint;
    fxID        : longint;                // fx unique id
    fxVersion   : longint;

    numPrograms : longint;
    future      : array[0..127] of byte;

    chunkSize   : longint;
    chunk       : pointer; //array[0..7] of char;    // variable
  end;

type
  PPERect = ^PERect;
  PERect = ^ERect;
  ERect = record
    Top, Left,
    Bottom, Right : Smallint;
  end;

function FourCharToLong(C1, C2, C3, C4: Char): Longint;
function FMod(d1, d2: Double): Double;

procedure dB2string(value: Single; text: PChar);
procedure dB2stringRound(value: single; text: pchar);
procedure float2string(value: Single; text: PChar);
procedure long2string(value: Longint; text: PChar);
procedure float2stringAsLong(value: Single; text: PChar);
procedure Hz2string(samples, sampleRate: Single; text: pchar);
procedure ms2string(samples, sampleRate: Single; text: pchar);

function gapSmallValue(value, maxValue: Double): Double;
function invGapSmallValue(value, maxValue: Double): Double;

implementation

uses Math, SysUtils;

{ this function converts four char variables to one longint. }
function FourCharToLong(C1, C2, C3, C4: Char): Longint;
begin
  Result := Ord(C4)  + (Ord(C3) shl 8) + (Ord(C2) shl 16) + (Ord(C1) shl 24);
end;

function FMod(d1, d2: Double): Double;
var
   i: Integer;
begin
  try
    i := Trunc(d1 / d2);
  except
    on EInvalidOp do i := High(Longint);
  end;
  Result := d1 - (i * d2);
end;

procedure dB2string(value: Single; text: PChar);
begin
 if (value <= 0)
  then StrCopy(text, '   -oo  ')
  else float2string(20 * log10(value), text);
end;

procedure dB2stringRound(value: single; text: pchar);
begin
 if (value <= 0)
  then StrCopy(text, '    -96 ')
  else long2string(Round(20 * log10(value)), text);
end;

procedure float2string(value: Single; text: PChar);
begin
 StrCopy(text, PChar(Format('%f', [value])));
end;

procedure long2string(value: Longint; text: PChar);
begin
  if (value >= 100000000) then
  begin
    StrCopy(text, ' Huge!  ');
    Exit;
  end;

  StrCopy(text, PChar(Format('%7d', [Value])));  // sprintf(aString, '%7d', value);
end;

procedure float2stringAsLong(value: Single; text: PChar);
begin
 if (value >= 100000000) then
  begin
   StrCopy(text, ' Huge!  ');
   Exit;
  end;

 StrCopy(text, PChar(Format('%7.0f', [value])));  // sprintf(aString, '%7d', value);
end;

procedure Hz2string(samples, sampleRate: single; text: pchar);
begin
 if (samples = 0)
  then float2string(0, text)
  else float2string(sampleRate / samples, text);
end;

procedure ms2string(samples, sampleRate: single; text: pchar);
begin
 float2string(samples * 1000 / sampleRate, text);
end;

function gapSmallValue(value, maxValue: double): double;
begin
 Result := Power(maxValue, value);
end;

function invGapSmallValue(value, maxValue: double): double;
var r: Double;
begin
 r := 0;
 if (value <> 0)
  then r := logN(maxValue, value);
 Result :=  r;
end;

{$WARNINGS ON}

end.
