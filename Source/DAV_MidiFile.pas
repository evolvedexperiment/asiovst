unit DAV_MidiFile;

interface

{$I DAV_Compiler.inc}

{
  Load a MidiFile and get access to Tracks and Events
  I did build this component to convert MidiFiles to wave files
  or play the files on a software synthesizer which I'm currenly
  building.

  version 1.0 first release

  version 1.1
    added some function
    function KeyToStr(Key: Integer): string;
    function MyTimeToStr(Value: Integer): string;
    Bpm can be set to change speed

  version 1.2
    added some functions
    function GetTrackLength: Integer;
    function Ready: Boolean;

  version 1.3
    update by Chulwoong,
    He knows how to use the MM timer, the timing is much better now, thank you

  version 1.4
    update by Tobybear,
    Rewrote component to allow usage in DLLs,
    timing improvements, some additional functions...

  for comments/bugs
  F.Bouwmans
  fbouwmans@spiditel.nl

  If you think this component is nice and you use it, sent me a short email.
  I've seen that other of my components have been downloaded a lot, but I've
  got no clue wether they are actually used.
  Don't worry because you are free to use these components

  Timing has improved, however because the messages are handled by the normal
  windows message loop (of the main window) it is still influenced by actions
  done on the window (minimize/maximize ..).
  Use of a second thread with higher priority which only handles the
  timer message should increase performance. If somebody knows such a component
  which is freeware please let me know.

  interface description:

  procedure ReadFile:
    actually read the file which is set in Filename

  function GetTrack(index: Integer): TMidiTrack;

  property Filename
    set/read filename of FMidiFile

  property NumberOfTracks
    read number of FTracks in current file

  property TicksPerQuarter: Integer
    ticks per quarter, tells how to interpret the time value in midi FEvents

  property FileFormat: TFileFormat
    tells the format of the current FMidiFile

  property Bpm:Integer
    tells Beats per minut

  property OnMidiEvent:TOnMidiEvent
    called while playing for each midi event

  procedure StartPlaying;
    start playing the current loaded FMidiFile from the beginning

  procedure StopPlaying;
    stop playing the current FMidiFile

  procedure PlayToTime(time : Integer);
    if playing yourSelf then FEvents from last time to this time are produced


  function KeyToStr(Key: Integer): string;
      give note string on key value:  e.g. C4

  function MyTimeToStr(Value: Integer): string;
      give time string from msec time

  function  GetTrackLength:Integer;
      gives the track lenght in msec (assuming the bpm at the start oof the file)

  function  Ready: Boolean;
      now you can check wether the playback is finished

}

uses
  {$IFDEF FPC}LCLIntf, LMessages, {$ELSE}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Stdctrls, ExtCtrls;

type
  TChunkType = (ctIllegal, ctHeader, ctTrack);
  TFileFormat = (ffSingleSynch, ffMultiSynch, ffMultiAsynch);
  PByte = ^Byte;

  TMidiEvent = record
    event   : Byte;
    data1   : Byte;
    data2   : Byte;
    str     : String;
    dticks  : Integer;
    time    : Integer;
    mtime   : Integer;
    len     : Integer;
  end;
  PMidiEvent = ^TMidiEvent;

  TOnMidiEvent = procedure(event: PMidiEvent) of object;
  TEvent = procedure of object;

  TMidiTrack = class(TObject)
  protected
    FEvents       : TList;
    FName         : string;
    FInstrument   : string;
    FCurrentTime  : Integer;
    FCurrentPos   : Integer;
    FReady        : Boolean;
    FTrackLenght  : Integer;
    FOnMidiEvent  : TOnMidiEvent;
    FOnTrackReady : TEvent;
    procedure CheckReady;
    function GetEventCount: Integer;
    function GetTrackLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Rewind(const pos: Integer);
    procedure PlayUntil(const pos: Integer);
    procedure GoUntil(const pos: Integer);

    procedure PutEvent(event: PMidiEvent);
    function GetEvent(const index: Integer): PMidiEvent;
  public
    property Name: string read FName;
    property Instrument: string read FInstrument;
    property EventCount: Integer read GetEventCount;
    property CurrentTime: Integer read FCurrentTime;
    property TrackLength: Integer read GetTrackLength;
    property IsReady: Boolean read FReady;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write FOnMidiEvent;
    property OnTrackReady: TEvent read FOnTrackReady write FOnTrackReady;
  end;

  TMidiFile = class(TComponent)
  private
    FManual: Boolean;
    procedure SetManual(const Value: Boolean);
  protected
    FMidiTimer       : TTimer;
    FMidiFileHandle  : THandle;

    { Protected declarations }
    FMidiFile        : file of Byte;
    FChunkType       : TChunkType;
    FChunkLength     : Integer;
    FChunkData       : PByte;
    FChunkIndex      : PByte;
    FChunkEnd        : PByte;
    FPriority        : DWORD;

    FBpmOld          : Integer;

    // midi file attributes
    FFileFormat      : TFileFormat;
    FNumberTracks    : Integer;
    FDeltaTicks      : Integer;
    FBPM             : Integer;
    FBeatsPerMeasure : Integer;
    FFusPerTick       : Double;
    FFilename        : string;

    FTracks          : TList;
    FCurrentTrack    : TMidiTrack;
    FOnMidiEvent     : TOnMidiEvent;
    FOnUpdateEvent   : TNotifyEvent;

    // playing attributes
    FPlaying         : Boolean;
    FPlayStartTime   : Integer;
    FCurrentTime     : Integer; // Current playtime in msec
    FCurrentPos      : Double;  // Current Position in ticks

    procedure OnTrackReady;
    procedure SetFileName(val: string);
    procedure ReadChunkHeader;
    procedure ReadChunkContent;
    procedure ReadChunk;
    procedure ProcessHeaderChunk;
    procedure ProcessTrackChunk;
    function ReadVarLength: Integer;
    function ReadString(l: Integer): string;
    procedure SetOnMidiEvent(handler: TOnMidiEvent);
    procedure SetBpm(val: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MidiTimer(sender : TObject);

    procedure ReadFile;
    function GetTrack(index: Integer): TMidiTrack;
    function GetTrackLength2: Integer;

    procedure StartPlaying;
    procedure StopPlaying;
    procedure ContinuePlaying;

    procedure PlayToTime(time: Integer);
    procedure GoToTime(time: Integer);
    function GetCurrentTime: Integer;
    function GetCurrentPos: Double;
    function GetfFusPerTick : Double;
    function GetTrackLength: Integer;
    function Ready: Boolean;
  published
    property ManualCall: Boolean read FManual write SetManual;
    property Filename: string read FFilename write SetFileName;
    property NumberOfTracks: Integer read FNumberTracks;
    property TicksPerQuarter: Integer read FDeltaTicks;
    property FileFormat: TFileFormat read FFileFormat;
    property Playing: Boolean read FPlaying;
    property Bpm: Integer read FBPM write SetBpm;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write SetOnMidiEvent;
    property OnUpdateEvent: TNotifyEvent read FOnUpdateEvent write FOnUpdateEvent;
  end;

function KeyToStr(const Key: Integer): string;
function MyTimeToStr(const Value: Integer): string;

implementation

uses
  MMSystem;

constructor TMidiTrack.Create;
begin
 inherited Create;
 FEvents := TList.Create;
 FCurrentTime := 0;
 FCurrentPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEvents.count - 1 do
    Dispose(PMidiEvent(FEvents.items[i]));
  FEvents.Free;
  inherited Destroy;
end;

procedure TMidiTRack.PutEvent(event: PMidiEvent);
var
  command : Integer;
  i       : Integer;
  pevent  : PMidiEvent;
begin
  if (event.event = $FF) then
   begin
    if (event.data1 = 3) then FName := event.str;
    if (event.data1 = 4) then FInstrument := event.str;
   end;
  FCurrentTime := FCurrentTime + event.dticks;
  event.time := FCurrentTime; // for the moment just add dticks
  event.len := 0;
  FEvents.Add(TObject(event));
  command := event.event and $F0;

  if ((command = $80) // note off
    or ((command = $90) and (event.data2 = 0))) //note on with speed 0
  then
  begin
    // this is a note off, try to find the accompanion note on
    command := event.event or $90;
    i := FEvents.count - 2;
    while i >= 0 do
    begin
      pevent := PMidiEvent(FEvents[i]);
      if (pevent.event = command) and
        (pevent.data1 = event.data1)
        then
      begin
        pevent.len := FCurrentTime - pevent.time;
        i := 0;
        event.len := -1;
      end;
      dec(i);
    end;
  end;
end;

function TMiditrack.GetEventCount: Integer;
begin
  result := FEvents.count;
end;

function TMiditrack.GetEvent(const index: Integer): PMidiEvent;
begin
  if ((index < FEvents.count) and (index >= 0))
   then result := FEvents[index]
   else result := nil;
end;

procedure TMiditrack.Rewind(const Pos: Integer);
begin
 if FCurrentPos = FEvents.Count then dec(FCurrentPos);
 while ((FCurrentPos > 0) and (PMidiEvent(FEvents[FCurrentPos]).time > Pos))
  do dec(FCurrentPos);
 CheckReady;
end;

procedure TMiditrack.PlayUntil(const pos: Integer);
begin
 if assigned(OnMidiEvent) then
  begin
   while ((FCurrentPos < FEvents.count) and (PMidiEvent(FEvents[FCurrentPos]).time < pos)) do
    begin
     OnMidiEvent(PMidiEvent(FEvents[FCurrentPos]));
     inc(FCurrentPos);
    end;
  end;
 CheckReady;
end;

procedure TMidiTrack.GoUntil(const pos: Integer);
begin
 while ((FCurrentPos < FEvents.count) and (PMidiEvent(FEvents[FCurrentPos]).time < pos))
  do inc(FCurrentPos);
 CheckReady;
end;

procedure TMidiTrack.CheckReady;
begin
 if FCurrentPos >= FEvents.count then
  begin
   FReady := True;
   if assigned(OnTrackReady)
    then OnTrackReady;
  end
 else FReady := False;
end;

function TMidiTrack.GetTrackLength: Integer;
begin
  result := PMidiEvent(FEvents[FEvents.count-1]).time
end;

constructor TMidiFile.Create(AOwner: TComponent);
begin
 inherited Create(AOWner);
 FManual := False;
 FChunkData := nil;
 FChunkType := ctIllegal;
 FTracks := TList.Create;
 FMidiTimer := TTimer.Create(nil);
 FMidiTimer.Interval := 2;
 FMidiTimer.OnTimer := MidiTimer;
 FMidiTimer.Enabled := True;
end;

destructor TMidiFile.Destroy;
var
  i: Integer;
begin
 FMidiTimer.Free;
 if not (FChunkData = nil) then FreeMem(FChunkData);
 for i := 0 to FTracks.Count - 1
  do TMidiTrack(FTracks.Items[i]).Free;
 FTracks.Free;
 inherited Destroy;
end;

function TMidiFile.GetTrack(index: Integer): TMidiTrack;
begin
  result := FTracks.Items[index];
end;

procedure TMidiFile.SetFileName(val: string);
begin
  FFilename := val;
//  ReadFile;
end;

procedure TMidiFile.SetOnMidiEvent(handler: TOnMidiEvent);
var
  i: Integer;
begin
//  if not (FOnMidiEvent = handler) then
//  begin
  FOnMidiEvent := handler;
  for i := 0 to FTracks.count - 1 do
    TMidiTrack(FTracks.items[i]).OnMidiEvent := handler;
//  end;
end;

procedure TMidiFile.MidiTimer(Sender: TObject);
begin
  if FPlaying then
  begin
    PlayToTime(GetTickCount - FPlayStartTime);
    if assigned(FOnUpdateEvent) then FOnUpdateEvent(Self);
  end;
end;

procedure TMidiFile.StartPlaying;
var
  i: Integer;
begin
  for i := 0 to FTracks.count - 1 do TMidiTrack(FTracks[i]).Rewind(0);
  FPlayStartTime := getTickCount;
  FPlaying := True;
  if not FManual then FMidiTimer.Enabled := True;
  FCurrentPos := 0.0;
  FCurrentTime := 0;
end;

procedure TMidiFile.ContinuePlaying;
begin
 FPlayStartTime := GetTickCount - FCurrentTime;
 FPlaying := True;
 if not FManual then FMidiTimer.Enabled := True;
end;

procedure TMidiFile.StopPlaying;
var
  i: Integer;
begin
 for i := 0 to FTracks.count - 1
  do TMidiTrack(FTracks.items[i]).Rewind(0);

 FPlaying := False;
 FMidiTimer.Enabled := False;
//  KillMIDITimer;
//  SetPriorityClass(FMidiFileHandle, FPriority);
end;

function TMidiFile.GetCurrentTime: Integer;
begin
  Result := FCurrentTime;
end;

procedure TMidiFile.PlayToTime(time: Integer);
var
  i         : Integer;
  pos       : Integer;
  deltaTime : Integer;
begin
 // calculate the pos in the file.
 // pos is actually tick
 // Current FFusPerTick is uses to determine the actual pos
 deltaTime := time - FCurrentTime;
 FCurrentPos := FCurrentPos + (deltaTime * 1000) / FFusPerTick;
 pos := round(FCurrentPos);
 for i := 0 to FTracks.count - 1 do TMidiTrack(FTracks.items[i]).PlayUntil(pos);
 FCurrentTime := time;
end;

procedure TMidiFile.GoToTime(time: Integer);
var
  i   : Integer;
  pos : Integer;
begin
 // this function should be changed because FFusPerTick might not be constant
 pos := round((time * 1000) / FFusPerTick);
 for i := 0 to FTracks.count - 1 do
  with TMidiTrack(FTracks.items[i]) do
   begin
    Rewind(0);
    GoUntil(pos);
   end;
 FCurrentTime := time;
end;

procedure TMidiFile.SetBpm(val: Integer);
var
  us_per_quarter: Integer;
begin
 if not (val = FBPM) then
  begin
   us_per_quarter := 60000000 div val;
   FBPM := 60000000 div us_per_quarter;
   FFusPerTick := us_per_quarter / FDeltaTicks;
  end;
end;

procedure TMidiFile.ReadChunkHeader;
var
  theByte: array[0..7] of Byte;
begin
 BlockRead(FMidiFile, theByte, 8);
 if (theByte[0] = $4D) and (theByte[1] = $54) then
  begin
   if (theByte[2] = $68) and (theByte[3] = $64)
    then FChunkType := ctHeader else
   if (theByte[2] = $72) and (theByte[3] = $6B)
    then FChunkType := ctTrack
    else FChunkType := ctIllegal;
  end
 else FChunkType := ctIllegal;
 FChunkLength := theByte[7] + theByte[6] * $100 + theByte[5] * $10000 + theByte[4] * $1000000;
end;

procedure TMidiFile.ReadChunkContent;
begin
 if not (FChunkData = nil) then FreeMem(FChunkData);
 GetMem(FChunkData, FChunkLength + 10);
 BlockRead(FMidiFile, FChunkData^, FChunkLength);
 FChunkIndex := FChunkData;
 FChunkEnd := PByte(Integer(FChunkIndex) + Integer(FChunkLength) - 1);
end;

procedure TMidiFile.ReadChunk;
begin
  ReadChunkHeader;
  ReadChunkContent;
  case FChunkType of
   ctHeader : ProcessHeaderChunk;
    ctTrack : ProcessTrackChunk;
  end;
end;

procedure TMidiFile.ProcessHeaderChunk;
begin
 FChunkIndex := FChunkData;
 inc(FChunkIndex);
 if FChunkType = ctHeader then
  begin
    case FChunkIndex^ of
     0: FFileFormat := ffSingleSynch;
     1: FFileFormat := ffMultiSynch;
     2: FFileFormat := ffMultiAsynch;
    end;
    inc(FChunkIndex);
    FNumberTracks := FChunkIndex^ * $100;
    inc(FChunkIndex);
    FNumberTracks := FNumberTracks + FChunkIndex^;
    inc(FChunkIndex);
    FDeltaTicks := FChunkIndex^ * $100;
    inc(FChunkIndex);
    FDeltaTicks := FDeltaTicks + FChunkIndex^;
  end;
end;

procedure TMidiFile.ProcessTrackChunk;
var
  dTime          : Integer;
  event          : Integer;
  len            : Integer;
  midiEvent      : PMidiEvent;
  us_per_quarter : Integer;
begin
  FChunkIndex := FChunkData;
//  inc(FChunkIndex);
  event := 0;
  if FChunkType = ctTrack then
   begin
    FCurrentTrack := TMidiTrack.Create;
    FCurrentTrack.OnMidiEvent := FOnMidiEvent;
    FTracks.add(FCurrentTrack);
    while Integer(FChunkIndex) < Integer(FChunkEnd) do
     begin
      // each event starts with var length delta time
      dTime := ReadVarLength;
      if FChunkIndex^ >= $80 then
       begin
        event := FChunkIndex^;
        inc(FChunkIndex);
       end;
      // else it is a running status event (just the same event as before)

      if event = $FF then
       begin
{        case FChunkIndex^ of
        $00: // sequence number, not implemented jet
            begin
              inc(FChunkIndex); // $02
              inc(FChunkIndex);
            end;
        $01 .. $0f: // text FEvents  FF ty len text
            begin
              New(midiEvent);
              midiEvent.event := $FF;
              midiEvent.data1 := FChunkIndex^;     // type is stored in data1
              midiEvent.dticks := dtime;

              inc(FChunkIndex);
              len := ReadVarLength;
              midiEvent.str    := ReadString(len);

              FCurrentTrack.PutEvent(midiEvent);
            end;
        $20: // Midi channel prefix  FF 20 01 cc
             begin
               inc(FChunkIndex); // $01
               inc(FChunkIndex); // channel
               inc(FChunkIndex);
             end;
        $2F: // End of ctTrack FF 2F 00
             begin
               inc(FChunkIndex); // $00
               inc(FChunkIndex);
             end;
        $51: // Set Tempo  FF 51 03 tttttt
             begin
               inc(FChunkIndex); // $03
               inc(FChunkIndex); // tt
               inc(FChunkIndex); // tt
               inc(FChunkIndex); // tt
               inc(FChunkIndex);
             end;
        $54: // SMPTE offset  FF 54 05 hr mn se fr ff
             begin
               inc(FChunkIndex); // $05
               inc(FChunkIndex); // hr
               inc(FChunkIndex); // mn
               inc(FChunkIndex); // se
               inc(FChunkIndex); // fr
               inc(FChunkIndex); // ff
               inc(FChunkIndex);
             end;
        $58: // Time signature FF 58 04 nn dd cc bb
             begin
               inc(FChunkIndex); // $04
               inc(FChunkIndex); // nn
               inc(FChunkIndex); // dd
               inc(FChunkIndex); // cc
               inc(FChunkIndex); // bb
               inc(FChunkIndex);
             end;
        $59: // Key signature FF 59 02 df mi
             begin
               inc(FChunkIndex); // $02
               inc(FChunkIndex); // df
               inc(FChunkIndex); // mi
               inc(FChunkIndex);
             end;
        $7F: // Sequence specific Meta-event
            begin
              inc(FChunkIndex);
              len := ReadVarLength;
              str := ReadString(len);
            end;
        else // unknown meta event
        }
         begin
          New(midiEvent);
          midiEvent.event := $FF;
          midiEvent.data1 := FChunkIndex^; // type is stored in data1
          midiEvent.dticks := dtime;

          inc(FChunkIndex);
          len := ReadVarLength;
          midiEvent.str := ReadString(len);
          FCurrentTrack.PutEvent(midiEvent);

          case midiEvent.data1 of
            $51:
              begin
                us_per_quarter :=
                  (Integer(Byte(midiEvent.str[1])) shl 16 +
                  Integer(Byte(midiEvent.str[2])) shl 8 +
                  Integer(Byte(midiEvent.str[3])));
                FBPM := 60000000 div us_per_quarter;
                FBpmOld := FBPM;
                FFusPerTick := us_per_quarter / FDeltaTicks;
              end;
          end;
        end;
//        end;
       end
      else
       begin
      // these are all midi FEvents
        New(midiEvent);
        midiEvent.event := event;
        midiEvent.dticks := dtime;
//         inc(FChunkIndex);
        case event of
          $80..$8F, // note off
          $90..$9F, // note on
          $A0..$AF, // key aftertouch
          $B0..$BF, // control change
          $E0..$EF: // pitch wheel change
            begin
              midiEvent.data1 := FChunkIndex^; inc(FChunkIndex);
              midiEvent.data2 := FChunkIndex^; inc(FChunkIndex);
            end;
          $C0..$CF, // program change
          $D0..$DF: // channel aftertouch
            begin
              midiEvent.data1 := FChunkIndex^; inc(FChunkIndex);
            end;
        else
           // error
        end;
        FCurrentTrack.PutEvent(midiEvent);
       end;
     end;
   end;
end;


function TMidiFile.ReadVarLength: Integer;
var
  i: Integer;
  b: Byte;
begin
 b := 128;
 i := 0;
 while b > 127 do
  begin
   i := i shl 7;
   b := FChunkIndex^;
   i := i + b and $7F;
   inc(FChunkIndex);
  end;
 result := i;
end;

function TMidiFile.ReadString(l: Integer): string;
var
  s: PChar;
  i: Integer;
begin
 GetMem(s, l + 1); ;
 s[l] := chr(0);
 for i := 0 to l - 1 do
  begin
   s[i] := Chr(FChunkIndex^);
   inc(FChunkIndex);
  end;
 result := string(s);
end;

procedure TMidiFile.ReadFile;
var
  i: Integer;
begin
  for i := 0 to FTracks.Count - 1
   do TMidiTrack(FTracks.Items[i]).Free;
  FTracks.Clear;
  FChunkType := ctIllegal;

  AssignFile(FMidiFile, FFilename);
  FileMode := 0;
  Reset(FMidiFile);
  while not EoF(FMidiFile) do ReadChunk;
  CloseFile(FMidiFile);
  FNumberTracks := FTracks.Count;
end;

function KeyToStr(const Key: Integer): string;
var
  n   : Integer;
  str : string;
begin
  n := key mod 12;
  case n of
    0: str := 'C';
    1: str := 'C#';
    2: str := 'D';
    3: str := 'D#';
    4: str := 'E';
    5: str := 'F';
    6: str := 'F#';
    7: str := 'G';
    8: str := 'G#';
    9: str := 'A';
   10: str := 'A#';
   11: str := 'B';
  end;
  Result := str + IntToStr(key div 12);
end;

function IntToLenStr(val: Integer; len: Integer): string;
var
  str: string;
begin
  str := IntToStr(val);
  while Length(str) < len do str := '0' + str;
  Result := str;
end;

function MyTimeToStr(const Value: Integer): string;
var
  hour : Integer;
  min  : Integer;
  sec  : Integer;
  msec : Integer;
begin
  msec := Value mod 1000;
  sec  := Value div 1000;
  min  := sec div 60;
  sec  := sec mod 60;
  hour := min div 60;
  min  := min mod 60;
  Result := IntToStr(hour) + ':' + IntToLenStr(min, 2) + ':' +
            IntToLenStr(sec, 2) + '.' + IntToLenStr(msec, 3);
end;

function TMidiFile.GetfFusPerTick: Double;
begin
 Result := FFusPerTick;
end;

function  TMidiFile.GetTrackLength: Integer;
var
  i, length : Integer;
  time      : Extended;
begin
 length := 0;
 for i := 0 to FTracks.Count - 1 do
  if TMidiTrack(FTracks.Items[i]).GetTrackLength > length
   then length := TMidiTrack(FTracks.Items[i]).GetTrackLength;
 time := length * FFusPerTick;
 time := time / 1000.0;
 result := round(time);
end;

function TMidiFile.GetTrackLength2: Integer;
var
  i, length: Integer;
begin
 length := 0;
 for i := 0 to FTracks.Count - 1 do
  if TMidiTrack(FTracks.Items[i]).GetTrackLength > length
   then length := TMidiTrack(FTracks.Items[i]).GetTrackLength;
 result := length;
end;

function TMidiFile.Ready: Boolean;
var
  i : Integer;
begin
 result := True;
 for i := 0 to FTracks.Count - 1 do
  if not TMidiTrack(FTracks.Items[i]).isready then
    result := False;
end;

procedure TMidiFile.OnTrackReady;
begin
 if Ready then
  if assigned(FOnUpdateEvent) then FOnUpdateEvent(Self);
end;

function TMidiFile.GetCurrentPos: Double;
begin
 result := FCurrentPos;
end;

procedure TMidiFile.SetManual(const Value: Boolean);
begin
 FManual := Value;
 FMidiTimer.Enabled := not Value;
end;

end.
