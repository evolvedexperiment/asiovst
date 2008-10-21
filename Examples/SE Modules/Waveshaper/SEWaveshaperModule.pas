unit SEWaveshaperModule;

interface

uses
  Windows, DAV_Common, DAV_SECommon, DAV_SEModule;

const
  CTableSize   = 512;
  CWsNodeCount = 11;
  CNodeSize    =  6;

// define some constants to make referencing in/outs clearer
  pinPatchParam = 0;
  pinInput = 1;
  pinOutput = 2;

type
  TSEWaveshaperModule = class(TSEModuleBase)
  private
    FInputPtr       : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputPtr      : PDAVSingleFixedArray;
    FShapePtr       : Pchar;
    FStaticCount    : Integer;
    FLookupTable    : Array [0..CTableSize] of TSEFloatSample;

    function GetHandle: THandle;
    function CreateSharedLookup(ATableName: PChar; ATablePointer: Pointer; ASampleRate: Single; ASize: Integer): boolean ;
    procedure SetupLookupTable;
    procedure FillLookupTable;
    procedure LookupTableChanged;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);

    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure ChooseProcess;
  end;

implementation

uses
  SysUtils;

(*
 If you use several waveshapers, each may be set to a different curve, so each
 needs it's own lookup table.  You need to give each table a unique name..

 e.g. 'Waveshaper 1 curve', 'Waveshaper 2 curve', etc

 SE assigns every module a unique number (handle).  If your module has a GUI
 object and a DSP object, they share the same FHandle. This is how SE know
 where to send GUI.Module communication.

 Each waveshaper needs a unique number to identify it's wavetable.  The handle
 is ideal for this purpose.
*)

constructor TSEWaveshaperModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
end;

procedure TSEWaveshaperModule.Open;
begin
 SetupLookupTable;
 OnProcess := SubProcess;
 inherited Open; // always call the base class
end;

procedure TSEWaveshaperModule.ChooseProcess;
begin
 if Pin[pinInput].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

procedure TSEWaveshaperModule.LookupTableChanged;
begin
//  _RPT1(_CRT_WARN, 'LookupTableChanged %x\n',this );

  ChooseProcess;
  if Pin[pinInput].Status <> stRun then
   begin
    // can't add event if sleeping
//    OutputChange(SampleClock, GetPlug(pinOutput), stOneOff);
    Pin[pinOutput].TransmitStatusChange(SampleClock, stOneOff);
   end;
end;

function TSEWaveshaperModule.GetHandle: THandle;
begin
  result := CallHost(SEAudioMasterGetModuleHandle);
end;

procedure TSEWaveshaperModule.SetupLookupTable;
var
  TableName      : string;
  NeedInitialise : Boolean;
begin
 TableName := 'SE wave shaper ' + IntToStr(Integer(GetHandle)) + ' curve' + #0;
 NeedInitialise := CreateSharedLookup(@TableName[1], @FLookupTable, -1, CTableSize + 2);
end;

procedure TSEWaveshaperModule.SubProcess(const BufferOffset, SampleFrames: Integer);
const
  CHalf      : Single = 0.5;
  CTableSize : Single = 512;
var
  Input         : PSingle;
  Output        : PSingle;
  s             : Integer;
  ControlWordA  : Word;
  ControlWordB  : Word;
  {$IFDEF PUREPASCAL}
  Index         : Single;
  IndexFrac     : Single;
  Count         : TSEFloatSample;
  p             : P4SingleArray;
  {$ELSE}
  IntIndex      : Integer;
  {$ENDIF}
begin
 Input  := @FInputPtr^[BufferOffset];
 Output := @FOutputPtr^[BufferOffset];
 asm
  fstcw  ControlWordA    // store fpu control word
  mov    dx, word ptr [ControlWordA]
  or     dx, $400        // round towards -oo
  mov    ControlWordB, dx
  fldcw  ControlWordB    // load modfied control word
 end;

  for s := SampleFrames - 1 downto 0 do
   begin
    {$IFDEF PUREPASCAL}
    count := f_Limit(Input^ + 0, 0, 1); // map +/-5 volt to 0.0 . 1.0
    Inc(Input);

    Index := Count * 512;
    IndexFrac := Index - trunc(index);
    p := @FLookupTable[round(index)]; // keep top 9 bits as index into 512 entry wavetable
    //single s 1 = *p++;
    //*out++ = s1 * (1.f - idx_frac) + *p * idx_frac;
    Output^ := p^[0] + (p^[1] - p^[0]) * (IndexFrac - 1);
    Inc(Output);
    {$ELSE}
    asm
      push ebx
      mov    ebx, dword ptr [Input]

      // multiply index by table size
      fld    dword ptr [edx]
      fadd   dword ptr [CHalf]
      fmul   dword ptr CTableSize
      mov    eax, dword ptr [self]

      fist   dword ptr [IntIndex]  // IntIndex = (integer) idx
      fisub  dword ptr [IntIndex]  // frac = idx - IntIndex

      mov    ecx, [self.FLookupTable].Single
      mov    edx, dword ptr [IntIndex]

      cmp    edx, 0
      jge    @not_too_small
      mov    edx, 0
      fstp   st(0)
      fldz
      jmp    @in_range
    @not_too_small:
      cmp    edx, 512
      jl     @in_range
      mov    edx, 512
      fstp   st(0)
      fldz
    @in_range:
      // perform table lookup, interpolate
      lea    esi, [ecx + edx * 4]     // esi = & (table[IntIndex] )
      fld    dword ptr [esi]          // table[0]
      fld    dword ptr [esi + 4]      // table[1]
      fsub   st(0), st(1)             // table[1] - table[0]
      mov    ecx, dword ptr [Output]
      fmulp  st(2), st(0)             // * idx_frac
      fadd                            // + table[0]

      // inc pointers, store result
      add         ebx, 4
      mov         dword ptr [Input], ebx
      fstp        dword ptr [ecx]

      add         ecx, 4
      mov         dword ptr [Output], ecx
      pop ebx
    end;
    {$ENDIF}
   end;

  // restore original control word
  asm
    fldcw  ControlWordA
  end;
end;

procedure TSEWaveshaperModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

function TSEWaveshaperModule.CreateSharedLookup(ATableName: PChar;
  ATablePointer: Pointer; ASampleRate: Single; ASize: Integer): boolean;
begin
 result := CallHost(SEAudioMasterCreateSharedLookup, Integer(ATablePointer), ASize, ATableName, ASampleRate) <> 0;
end;

procedure TSEWaveshaperModule.FillLookupTable;
var
  nodes       : array [0..CWsNodeCount - 1] of TPoint; // x,y co-ords of control points
  segments    : Integer;
  from, fto   : Single; // first x co=ord
  table_index : Integer;
  i, int_to   : Integer;
  delta_y     : Single;
  delta_x     : Single;
  slope, c    : Single;
//  gain, t     : Single;
begin
// GuiTSEWaveshaperModule.UpdateNodes(nodes, SeSdkString(FShapePtr));

(* Old slower code
  segments := 11;
  from     := 0; // first x co=ord
  for i := 1 to segments - 1 do
   begin
    fto := nodes[i].x * CTableSize *0.01; // convert to table index (0-512)

    if fto >= CTableSize
     then fto := CTableSize - 1;

    t := from;
    while t < fto do
     begin
      Gain := nodes[i - 1].y + ((t - from) / (fto - from)) * (nodes[i].y - nodes[i-1].y);
      Gain = (50 - gain) * 0.01;
      FLookupTable[round(t)] := Gain;
      t := t + 1;
     end;

    from := round(to);
   end;
*)

  // new, faster code
  segments    := 11;
  from        := 0; // first x co=ord
  table_index := 0;
  for i := 1 to segments - 1 do
   begin
    fto := nodes[i].x * CTableSize * 0.01;

    delta_y := nodes[i].y - nodes[i-1].y;
    delta_x := table_index - fto;
    slope := 0.01 * delta_y / delta_x;
    c := 0.5 - 0.01 * nodes[i-1].y - from * slope;

    int_to := round(fto);
    if int_to > CTableSize
     then int_to := CTableSize;

    while table_index < int_to do
     begin
      FLookupTable[table_index] := table_index * slope + c;
      Inc(table_index);
     end;

    from := fto;
   end;

  // extrapolate last entry
  FLookupTable[512] := 2 * FLookupTable[511] - FLookupTable[510];
end;

// describe the pins (plugs)
function TSEWaveshaperModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
(*
  LIST_PIN2('Signal In', FInputPtr, DR_IN, '0','', IO_POLYPHONIC_ACTIVE,'');
  LIST_PIN2('Signal Out', FOutputPtr, DR_OUT, '','', 0,'');
  // move this to patch mgr
  LIST_VAR3('Ignore Program Change', ignore_prog_change, DR_PARAMETER, DT_BOOL ,'0','', 0,'Prevents this control responding to patch changes from 'Patch Select' module');
  LIST_VAR3('shape', m_shape_string,  DR_IN, DT_TEXT ,'', '', IO_UI_COMMUNICATION(*| IO_PRIVATE* /,'');
*)

 result := True;
 case Index of                // !!TODO!! list your in / out plugs
  0: with Properties^ do
      begin
       name            := 'Shape';
       VariableAddress := @FShapePtr;
       Direction       := drIn;
       Datatype        := dtText;
//       DefaultValue    = '';
       Flags           := [iofUICommunication, iofUIDualFlag, iofHidePin];
      end;
  1: with Properties^ do
      begin
       Name            := 'Signal In';
       VariableAddress := @FInputPtr;
       Direction       := drIn;
       Datatype        := dtFSample;
       Flags           := [iofPolyphonicActive];
      end;
  2: with Properties^ do
      begin
       Name            := 'Signal Out';
       VariableAddress := @FOutputPtr;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  else result := false; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSEWaveshaperModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  pinPatchParam:
   begin
    FillLookupTable;
    LookupTableChanged;
   end;

  pinInput:
   begin
    ChooseProcess;
    Pin[pinOutput].TransmitStatusChange(SampleClock, Pin[pinInput].Status);
   end;
 end;
 inherited;
end;

end.
