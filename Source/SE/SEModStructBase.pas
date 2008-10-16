unit SEModStructBase;

interface

uses
  DAV_Common, SEDataTypes;

const
  SDK_VERSION = 2230;

  // the 'magic number' that identifies a SynthEdit module (spells SEPL)
  SepMagic  = $5345504C;
  SepMagic2 = SepMagic + 1;

////////////////////////////////////////////////////////////////////////////////

type
  PSEModStructBase   = ^TSEModStructBase;
  PSEModStructBase2  = ^TSEModStructBase2;

  TProcessFuntionPtr = procedure(Effect: PSEModStructBase2; BufferOffset: Integer; SampleFrames: Integer);
  TDispatcher = function(Effect: PSEModStructBase; Opcode, Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer;
  TProcess = procedure(Effect: PSEModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer);

  TSetParameter = procedure(Effect: PSEModStructBase; Index: Integer; Parameter: Single);
  TGetParameter = function(Effect: PSEModStructBase; Index: Integer): Single;

  TDispatcherPtrType = function (Effect: PSEModStructBase2; Opcode: Integer; Index: Integer; Value: Integer; Ptr: Pointer; Opt: Double): Integer; cdecl;

  TSEModStructBase = record
    magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    flags            : Integer;              // see constants
    resvd1           : Pointer;              // reserved for host use, must be 0
    resvd2           : Integer;              // reserved, must be 0
    vObject          : Pointer;              // for class access, MUST be 0 else!
    user             : Pointer;              // user access
    version          : Integer;              //
    processReplacing : TProcessFuntionPtr;   // procedure ProcessReplacing(Effect: PSEModStructBase; BufferOffset, Sampleframes: Integer); cdecl;
    future           : array[0..59] of char; // pls zero
  end;

  TSEModStructBase2 = record
    magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    version          : Integer;              //
    resvd1           : Pointer;              // reserved for host use, must be 0
    vObject          : Pointer;              // for class access, MUST be 0 else!
    user             : Pointer;              // user access
    dispatcher       : TDispatcherPtrType;
    SubProcessPtr    : Pointer;
    EventHandlerPtr  : Pointer;
    Future           : array[0..15] of char; // pls zero
  end;

////////////////
// misc def's //
////////////////

  TSEAudioMasterCallback  = function (Effect: PSEModStructBase; Opcode, Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;
  TSEAudioMasterCallback2 = function (Effect: PSEModStructBase2; Opcode, Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;

(*
  typedef  void ( SEModule_base::*process_function_ptr2)(long, long);
  typedef  void ( SEModule_base::*event_function_ptr)(SeEvent *e);
  typedef  long ( *dispatcher_ptr_type)(SEMod_struct_base2 *effect, long opCode, long index, long value,void *ptr, float opt);
*)

  ///////////////////////////
  // Plugin Module Opcodes //
  ///////////////////////////

  TSEPluginModuleOpcodes =
    (seffOpen  = 0,       // initialise
     seffClose = 1,       // exit, release all memory and other resources!

     seffSetSampleRate,   // in opt (float)
     seffSetBlockSize,    // in value
     seffGetUniqueId,
     seffGetEffectName,
     seffGetPinProperties,
     seffAddEvent,
     seffResume,
     seffGetModuleProperties,
     seffIsEventListEmpty,
     seffGetSdkVersion,
     seffGuiNotify,
     seffQueryDebugInfo); // allows to host to determin compiler settings etc

  //////////////////
  // Host Opcodes //
  //////////////////

  TSEHostOpcodes = (
    SEAudioMasterSetPinStatus = 0,
    SEAudioMasterIsPinConnected,     //
    SEAudioMasterGetPinInputText,    // gets pointer to plugs input string (DT_TEXT only)
    SEAudioMasterGetSampleClock,     // get current sampleclock at block start
    SEAudioMasterSendMIDI,           // send short MIDI msg
    SEAudioMasterGetInputPinCount,   // total AUDIO ins
    SEAudioMasterGetOutputPinCount,  // total AUDIO outs
    SEAudioMasterGetPinVarAddress,
    SEAudioMasterGetBlockStartClock,
    SEAudioMasterGetTime,
    SEAudioMasterSleepMode,
    SEAudioMasterGetRegisteredName,  // limited to 50 characters or less
    (* EXAMPLE CALLING CODE
      name : array [0..49] of char;
      CallHost(SEAudioMasterGetRegisteredName, 0, 0, @name[0]);
    *)
    SEAudioMasterGetFirstClone,
    SEAudioMasterGetNextClone,
    (* EXAMPLE CALLING CODE

      procedure IterateThroughAllClones;
      var
        CloneStruct : PSEModStructBase2;
        Clone       : PModule;
      begin
        // get first one
        CallHost(SEAudioMasterGetFirstClone, 0, 0, CloneStruct);

        while (clone_struct <> 0)
         begin
          // convert host's clone pointer to a 'Module' object
          Clone := PModule(CloneStruct.Object);

          // Access each clone here

          // step to next clone
          Clone.CallHost(SEAudioMasterGetNextClone, 0, 0, CloneStruct);
         end;
      end;
    *)
    SEAudioMasterGetTotalPinCount,   // Total pins of all types
    SEAudioMasterCallVstHost,        // Call VST Host direct (see se_call_vst_host_params struct)
    SEAudioMasterResolveFilename,    // get full path from a short filename, (int pin_idx, float max_characters, char *destination)
    SEAudioMasterSendStringToGui,    // Reserved for Experimental use (by Jef)
    SEAudioMasterGetModuleHandle,    // Reserved for Experimental use (by Jef)
    SEAudioMasterAddEvent,           // pass SeEvent *, host will copy data from struct. Safe to discard after call.
    SEAudioMasterCreateSharedLookup,
    SEAudioMasterSetPinOutputText,   // sets plug's output string (DT_TEXT only)
    SEAudioMasterSetProcessFunction, // sets the current sub_process() function
    SEAudioMasterResolveFilename2,   // get full path from a short filename - UNICODE
    (* EXAMPLE CALLING CODE
      uses windows;  //for WideCharToMultiByte

      // get the full path of an imbedded file when you only know it's short name
      const
        MAX_FILENAME_LENGTH : Integer = 300;

      // Both source and destination are UNICODE (two-byte) character strings
      unsigned short *source = L"test.txt";
      unsigned short dest[MAX_FILENAME_LENGTH];

      CallHost(SEAudioMasterResolveFilename2, Integer(source), MAX_FILENAME_LENGTH, &dest);

      // to convert to ascii (optional)
      char ascii_filename[MAX_FILENAME_LENGTH];
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_filename, MAX_FILENAME_LENGTH, NULL, NULL);
    *)
    SEAudioMasterGetSeVersion        // returns SE version number times 100,000 ( e.g. 120000 is V 1.2 )
    (* EXAMPLE CALLING CODE
      int v = CallHost(SEAudioMasterGetSeVersion, 0, 0, 0);
    *)
  );


  // fill in this structure when using Opcode SEAudioMasterCallVstHost
  (*
      Download the VST SDK for a list of VST opcodes.
      http://www.steinberg.net/en/support/3rdparty/vst_sdk/download/

      Look in aeffect.h and aeffectx.h
  *)

  TSECallVstHostParams = record
    Opcode : Integer;
    Index  : Integer;
    Value  : Integer;
    Ptr    : Pointer;
    Opt    : Double;
  end;

implementation

end.
