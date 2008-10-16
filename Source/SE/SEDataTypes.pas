unit SEDataTypes;

interface

type
  TDirection = (drIn, drOut, drContainer_IO, DR_Parameter, drFeature = drIn,
    drCntrl = drOut);

  { plug direction }
  { new enum EDirectionDR_IN, DR_OUT, DR_CONTAINER_IO, DR_PARAMETER,DR_FEATURE=DR_OUT,DR_CNTRL=DR_IN ; // plug direction }
  TPlugDataType = (dtNONE = -1, dtENUM, dtTEXT, dtMIDI2, dtDOUBLE, dtBOOL,
       dtFSAMPLE, dtFLOAT, dtVST_PARAM, dtINT, dtBLOB);

  {plug datatype }
  { ST_STOP, ST_ONE_OFF obsolete. Use ST_STATIC or ST_RUN. }
  TStateType = (stStop, stOneOff, stStatic = 1, stRun);

  // Order is important for comparissons }
  { old, use uetRUN_FUNCTION2 instead }
  {  , uetUI_NOTIFY2 // newer method using fifo }
  { do nothing }
  { MODULES CAN USE FOR WHATEVER }
  TUGEventType = (uetStatChange, uetSuspend, uetMIDI, uetRunFunction,
    uetIOFunc, uetUINotify, uetProgChange, uetNoteOn, uetNoteOff,
    uetNoteMute, uetPitchBend, uetAftertouch, uetStartPortamento,
    uetWSTableChange, uetDelayedGate, uetParamAutomation,
    uetNoteOffHeld, uetHeldNotesOff, uetNull, uetGeneric1,
    uetSetOutputPin, uetRunFunction2);


  TFloatSample = Double;
  { posible plug Flags values }
  { is this an 'active' input (ie you can't combine voices into it) eg filter cutoff }
  { or input to non-linear process (clipper, distortion etc) }
  { obsolete, use IO_LINEAR_INPUT instead (it's the oposit though) }

const
  CIOPolyphonicActive = 1;
  // midi channel selection etc should should ignore patch changes
  CIO_IGNORE_PATCH_CHANGE = 2;
  // auto-rename on new connection
  CIO_RENAME = 4;
  // plugs which are automaticly duplicated (like a container's 'spare' plug)
  CIO_AUTODUPLICATE = 8;
  CIO_FILENAME = 16;
  // ALLOW USER TO SET THE VALUE OF THIS OUTPUt eg on 'constant value' ug
  CIO_SETABLE_OUTPUT = 32;
  // plugs which can be duplicated/deleted by CUG
  CIO_CUSTOMISABLE = 64;
  // plugs which handle multiple inputs, must belong to an Adder ug
  CIO_ADDER = 128;
  // plugs which are private or obsolete, but are enabled on load if connected somewhere
  CIO_PRIVATE = 256;     // obsolete use IO_HIDE_PIN
  CIO_HIDE_PIN = 256;
  CIO_DISABLE_IF_POS = CIO_PRIVATE;
  // set this if this input can handle more that one polyphonic voice
  CIO_LINEAR_INPUT = 512;
  CIO_UI_COMMUNICATION = 1024;
  CIO_AUTO_ENUM = $800;
  CIO_HIDE_WHEN_LOCKED = $1000;
  // DON'T EXPOSE AS PLUG (TO SAVE SPACE)
  CIO_PARAMETER_SCREEN_ONLY = $2000;
  CIO_DONT_CHECK_ENUM = $4000;
  // don't use IO_UI_DUAL_FLAG by itself, use IO_UI_COMMUNICATION_DUAL
  CIO_UI_DUAL_FLAG = $8000;
  // obsolete, use IO_PATCH_STORE instead
  CIO_UI_COMMUNICATION_DUAL = CIO_UI_DUAL_FLAG or CIO_UI_COMMUNICATION;
  // Patch store is similar to dual but for DSP output plugs that appear as input plugs on UI (Output paramters) could consolodate?
  CIO_PATCH_STORE = $10000;
  // Private parameter (not exposed to user of VST plugin)
  CIO_PAR_PRIVATE = $20000;
  // minimised (not exposed on structure view (only properties window)
  CIO_MINIMISED = $40000;

// handy macro to pack a 4 char message id into an int
// e.g. int id = id_to_int('d','u','c','k')

function IDtoInt(c1, c2, c3, c4: Integer): Integer;

implementation

function IDtoInt(c1, c2, c3, c4: Integer): Integer;
begin
 result := ((c1 + (c2 shl 8)) + (c3 shl 16)) + (c4 shl 24);
end;

end.
