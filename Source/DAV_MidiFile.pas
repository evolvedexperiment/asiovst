unit DAV_MidiFile;

{$I ASIOVST.INC}

{
  Load a fMidiFile and get access to fTracks and fEvents
  I did build this component to convert MidiFiles to wave files
  or play the files on a software synthesizer which I'm currenly
  building.

  version 1.0 first release

  version 1.1
    added some function
    function KeyToStr(key : Integer) : string;
    function MyTimeToStr(val : Integer) : string;
    Bpm can be set to change speed

  version 1.2
    added some functions
    function  GetTrackLength:Integer;
    function  fReady: Boolean;

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

  if you think this component is nice and you use it, sent me a short email.
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

  function GetTrack(index: Integer) : TMidiTrack;

  property Filename
    set/read filename of fMidiFile

  property NumberOfTracks
    read number of fTracks in current file

  property TicksPerQuarter: Integer
    ticks per quarter, tells how to interpret the time value in midi fEvents

  property FileFormat: TFileFormat
    tells the format of the current fMidiFile

  property Bpm:Integer
    tells Beats per minut

  property OnMidiEvent:TOnMidiEvent
    called while playing for each midi event

  procedure StartPlaying;
    start playing the current loaded fMidiFile from the beginning

  procedure StopPlaying;
    stop playing the current fMidiFile

  procedure PlayToTime(time : Integer);
    if playing yourSelf then fEvents from last time to this time are produced


  function KeyToStr(key : Integer) : string;
      give note string on key value:  e.g. C4

  function MyTimeToStr(val : Integer) : string;
      give time string from msec time

  function  GetTrackLength:Integer;
      gives the track lenght in msec (assuming the bpm at the start oof the file)

  function  fReady: Boolean;
      now you can check wether the playback is finished

}

interface

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
    fEvents      : TList;
    fName        : string;
    fInstrument  : string;
    fCurrentTime : Integer;
    fCurrentPos  : Integer;
    fReady       : Boolean;
    fTrackLenght : Integer;
    procedure checkReady;
  public
    OnMidiEvent: TOnMidiEvent;
    OnTrackReady: TEvent;
    constructor Create;
    destructor Destroy; override;

    procedure Rewind(pos: Integer);
    procedure PlayUntil(pos: Integer);
    procedure GoUntil(pos: Integer);

    procedure putEvent(event: PMidiEvent);
    function getEvent(index: Integer): PMidiEvent;
    function getName: string;
    function getInstrument: string;
    function getEventCount: Integer;
    function getCurrentTime: Integer;
    function getTrackLength: Integer;
    function isReady: Boolean;
  end;

  TMidiFile = class(TComponent)
  private
    FManual: Boolean;
    procedure SetManual(const Value: Boolean);
  protected
    fMTimer          : TTimer;
    fMidiFileHandle  : HWND;

    { Protected declarations }
    fMidiFile        : file of Byte;
    fChunkType       : TChunkType;
    fChunkLength     : Integer;
    fChunkData       : PByte;
    fChunkIndex      : PByte;
    fChunkEnd        : PByte;
    fPriority        : DWORD;

    // midi file attributes
    fFileFormat      : TFileFormat;
    fNumberTracks    : Integer;
    fDeltaTicks      : Integer;
    fBPM             : Integer;
    fBeatsPerMeasure : Integer;
    fFusPerTick       : Double;
    fFilename        : string;

    fTracks          : TList;
    fCurrentTrack    : TMidiTrack;
    fOnMidiEvent     : TOnMidiEvent;
    fOnUpdateEvent   : TNotifyEvent;

    // playing attributes
    fPlayStartTime   : Integer;
    fCurrentTime     : Integer; // Current playtime in msec
    fCurrentPos      : Double;  // Current Position in ticks

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
    FBpmOld: Integer;
    Playing: Boolean;
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
    property Filename: string read fFilename write SetFileName;
    property NumberOfTracks: Integer read fNumberTracks;
    property TicksPerQuarter: Integer read fDeltaTicks;
    property FileFormat: TFileFormat read fFileFormat;
    property Bpm: Integer read fBPM write SetBpm;
    property OnMidiEvent: TOnMidiEvent read fOnMidiEvent write SetOnMidiEvent;
    property OnUpdateEvent: TNotifyEvent read fOnUpdateEvent write fOnUpdateEvent;
  end;

function KeyToStr(key: Integer): string;
function MyTimeToStr(val: Integer): string;
procedure Register;

implementation

uses
  MMSystem;

constructor TMidiTrack.Create;
begin
 inherited Create;
 fEvents := TList.Create;
 fCurrentTime := 0;
 fCurrentPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: Integer;
begin
  for i := 0 to fEvents.count - 1 do
    Dispose(PMidiEvent(fEvents.items[i]));
  fEvents.Free;
  inherited Destroy;
end;

procedure TMidiTRack.putEvent(event: PMidiEvent);
var
  command : Integer;
  i       : Integer;
  pevent  : PMidiEvent;
begin
  if (event.event = $FF) then
   begin
    if (event.data1 = 3) then fName := event.str;
    if (event.data1 = 4) then fInstrument := event.str;
   end;
  fCurrentTime := fCurrentTime + event.dticks;
  event.time := fCurrentTime; // for the moment just add dticks
  event.len := 0;
  fEvents.Add(TObject(event));
  command := event.event and $F0;

  if ((command = $80) // note off
    or ((command = $90) and (event.data2 = 0))) //note on with speed 0
  then
  begin
    // this is a note off, try to find the accompanion note on
    command := event.event or $90;
    i := fEvents.count - 2;
    while i >= 0 do
    begin
      pevent := PMidiEvent(fEvents[i]);
      if (pevent.event = command) and
        (pevent.data1 = event.data1)
        then
      begin
        pevent.len := fCurrentTime - pevent.time;
        i := 0;
        event.len := -1;
      end;
      dec(i);
    end;
  end;
end;

function TMidiTrack.getName: string;
begin
  result := fName;
end;

function TMidiTrack.getInstrument: string;
begin
  result := fInstrument;
end;

function TMiditrack.getEventCount: Integer;
begin
  result := fEvents.count;
end;

function TMiditrack.getEvent(index: Integer): PMidiEvent;
begin
  if ((index < fEvents.count) and (index >= 0))
   then result := fEvents[index]
   else result := nil;
end;

function TMiditrack.getCurrentTime: Integer;
begin
  result := fCurrentTime;
end;

procedure TMiditrack.Rewind(pos: Integer);
begin
 if fCurrentPos = fEvents.count then dec(fCurrentPos);
 while ((fCurrentPos > 0) and (PMidiEvent(fEvents[fCurrentPos]).time > pos))
  do dec(fCurrentPos);
 checkReady;
end;

procedure TMiditrack.PlayUntil(pos: Integer);
begin
 if assigned(OnMidiEvent) then
  begin
   while ((fCurrentPos < fEvents.count) and (PMidiEvent(fEvents[fCurrentPos]).time < pos)) do
    begin
     OnMidiEvent(PMidiEvent(fEvents[fCurrentPos]));
     inc(fCurrentPos);
    end;
  end;
 checkReady;
end;

procedure TMidiTrack.GoUntil(pos: Integer);
begin
 while ((fCurrentPos < fEvents.count) and (PMidiEvent(fEvents[fCurrentPos]).time < pos))
  do inc(fCurrentPos);
 checkReady;
end;

procedure TMidiTrack.checkReady;
begin
 if fCurrentPos >= fEvents.count then
  begin
   fReady := True;
   if assigned(OnTrackReady)
    then OnTrackReady;
  end
 else fReady := False;
end;

function TMidiTrack.getTrackLength: Integer;
begin
  result := PMidiEvent(fEvents[fEvents.count-1]).time
end;

function TMidiTrack.isReady: Boolean;
begin
 result := fReady;
end;

constructor TMidiFile.Create(AOwner: TComponent);
begin
 inherited Create(AOWner);
 FManual := False;
 fChunkData := nil;
 fChunkType := ctIllegal;
 fTracks := TList.Create;
 fMTimer := TTimer.Create(nil);
 fMTimer.Interval := 2;
 fMTimer.OnTimer := MidiTimer;
 fMTimer.Enabled := True;
end;

destructor TMidiFile.Destroy;
var
  i: Integer;
begin
 fMTimer.Free;
 if not (fChunkData = nil) then FreeMem(fChunkData);
 for i := 0 to fTracks.Count - 1
  do TMidiTrack(fTracks.Items[i]).Free;
 fTracks.Free;
 inherited Destroy;
end;

function TMidiFile.GetTrack(index: Integer): TMidiTrack;
begin
  result := fTracks.Items[index];
end;

procedure TMidiFile.SetFileName(val: string);
begin
  fFilename := val;
//  ReadFile;
end;

procedure TMidiFile.SetOnMidiEvent(handler: TOnMidiEvent);
var
  i: Integer;
begin
//  if not (fOnMidiEvent = handler) then
//  begin
  fOnMidiEvent := handler;
  for i := 0 to fTracks.count - 1 do
    TMidiTrack(fTracks.items[i]).OnMidiEvent := handler;
//  end;
end;

procedure TMidiFile.MidiTimer(Sender: TObject);
begin
  if playing then
  begin
    PlayToTime(GetTickCount - fPlayStartTime);
    if assigned(fOnUpdateEvent) then fOnUpdateEvent(Self);
  end;
end;

procedure TMidiFile.StartPlaying;
var
  i: Integer;
begin
  for i := 0 to fTracks.count - 1 do TMidiTrack(fTracks[i]).Rewind(0);
  fPlayStartTime := getTickCount;
  playing := True;
  if not FManual then fMTimer.Enabled := True;
  fCurrentPos := 0.0;
  fCurrentTime := 0;
end;

procedure TMidiFile.ContinuePlaying;
begin
 fPlayStartTime := GetTickCount - fCurrentTime;
 playing := True;
 if not FManual then fMTimer.Enabled := True;
end;

procedure TMidiFile.StopPlaying;
var
  i: Integer;
begin
 for i := 0 to fTracks.count - 1
  do TMidiTrack(fTracks.items[i]).Rewind(0);

 playing := False;
 fMTimer.Enabled := False;
//  KillMIDITimer;
//  SetPriorityClass(fMidiFileHandle, fPriority);
end;

function TMidiFile.GetCurrentTime: Integer;
begin
  Result := fCurrentTime;
end;

procedure TMidiFile.PlayToTime(time: Integer);
var
  i         : Integer;
  pos       : Integer;
  deltaTime : Integer;
begin
 // calculate the pos in the file.
 // pos is actually tick
 // Current fFusPerTick is uses to determine the actual pos
 deltaTime := time - fCurrentTime;
 fCurrentPos := fCurrentPos + (deltaTime * 1000) / fFusPerTick;
 pos := round(fCurrentPos);
 for i := 0 to fTracks.count - 1 do TMidiTrack(fTracks.items[i]).PlayUntil(pos);
 fCurrentTime := time;
end;

procedure TMidiFile.GoToTime(time: Integer);
var
  i   : Integer;
  pos : Integer;
begin
 // this function should be changed because fFusPerTick might not be constant
 pos := round((time * 1000) / fFusPerTick);
 for i := 0 to fTracks.count - 1 do
  with TMidiTrack(fTracks.items[i]) do
   begin
    Rewind(0);
    GoUntil(pos);
   end;
 fCurrentTime := time;
end;

procedure TMidiFile.SetBpm(val: Integer);
var
  us_per_quarter: Integer;
begin
 if not (val = fBPM) then
  begin
   us_per_quarter := 60000000 div val;
   fBPM := 60000000 div us_per_quarter;
   fFusPerTick := us_per_quarter / fDeltaTicks;
  end;
end;

procedure TMidiFile.ReadChunkHeader;
var
  theByte: array[0..7] of Byte;
begin
 BlockRead(fMidiFile, theByte, 8);
 if (theByte[0] = $4D) and (theByte[1] = $54) then
  begin
   if (theByte[2] = $68) and (theByte[3] = $64)
    then fChunkType := ctHeader else
   if (theByte[2] = $72) and (theByte[3] = $6B)
    then fChunkType := ctTrack
    else fChunkType := ctIllegal;
  end
 else fChunkType := ctIllegal;
 fChunkLength := theByte[7] + theByte[6] * $100 + theByte[5] * $10000 + theByte[4] * $1000000;
end;

procedure TMidiFile.ReadChunkContent;
begin
 if not (fChunkData = nil) then FreeMem(fChunkData);
 GetMem(fChunkData, fChunkLength + 10);
 BlockRead(fMidiFile, fChunkData^, fChunkLength);
 fChunkIndex := fChunkData;
 fChunkEnd := PByte(Integer(fChunkIndex) + Integer(fChunkLength) - 1);
end;

procedure TMidiFile.ReadChunk;
begin
  ReadChunkHeader;
  ReadChunkContent;
  case fChunkType of
   ctHeader : ProcessHeaderChunk;
    ctTrack : ProcessTrackCHunk;
  end;
end;

procedure TMidiFile.ProcessHeaderChunk;
begin
 fChunkIndex := fChunkData;
 inc(fChunkIndex);
 if fChunkType = ctHeader then
  begin
    case fChunkIndex^ of
     0: fFileFormat := ffSingleSynch;
     1: fFileFormat := ffMultiSynch;
     2: fFileFormat := ffMultiAsynch;
    end;
    inc(fChunkIndex);
    fNumberTracks := fChunkIndex^ * $100;
    inc(fChunkIndex);
    fNumberTracks := fNumberTracks + fChunkIndex^;
    inc(fChunkIndex);
    fDeltaTicks := fChunkIndex^ * $100;
    inc(fChunkIndex);
    fDeltaTicks := fDeltaTicks + fChunkIndex^;
  end;
end;

procedure TMidiFile.ProcessTrackChunk;
var
  dTime: Integer;
  event: Integer;
  len: Integer;
  midiEvent: PMidiEvent;
  us_per_quarter: Integer;
begin
  fChunkIndex := fChunkData;
//  inc(fChunkIndex);
  event := 0;
  if fChunkType = ctTrack then
   begin
    fCurrentTrack := TMidiTrack.Create;
    fCurrentTrack.OnMidiEvent := fOnMidiEvent;
    fTracks.add(fCurrentTrack);
    while Integer(fChunkIndex) < Integer(fChunkEnd) do
     begin
      // each event starts with var length delta time
      dTime := ReadVarLength;
      if fChunkIndex^ >= $80 then
       begin
        event := fChunkIndex^;
        inc(fChunkIndex);
       end;
      // else it is a running status event (just the same event as before)

      if event = $FF then
       begin
{        case fChunkIndex^ of
        $00: // sequence number, not implemented jet
            begin
              inc(fChunkIndex); // $02
              inc(fChunkIndex);
            end;
        $01 .. $0f: // text fEvents  FF ty len text
            begin
              New(midiEvent);
              midiEvent.event := $FF;
              midiEvent.data1 := fChunkIndex^;     // type is stored in data1
              midiEvent.dticks := dtime;

              inc(fChunkIndex);
              len := ReadVarLength;
              midiEvent.str    := ReadString(len);

              fCurrentTrack.putEvent(midiEvent);
            end;
        $20: // Midi channel prefix  FF 20 01 cc
             begin
               inc(fChunkIndex); // $01
               inc(fChunkIndex); // channel
               inc(fChunkIndex);
             end;
        $2F: // End of ctTrack FF 2F 00
             begin
               inc(fChunkIndex); // $00
               inc(fChunkIndex);
             end;
        $51: // Set Tempo  FF 51 03 tttttt
             begin
               inc(fChunkIndex); // $03
               inc(fChunkIndex); // tt
               inc(fChunkIndex); // tt
               inc(fChunkIndex); // tt
               inc(fChunkIndex);
             end;
        $54: // SMPTE offset  FF 54 05 hr mn se fr ff
             begin
               inc(fChunkIndex); // $05
               inc(fChunkIndex); // hr
               inc(fChunkIndex); // mn
               inc(fChunkIndex); // se
               inc(fChunkIndex); // fr
               inc(fChunkIndex); // ff
               inc(fChunkIndex);
             end;
        $58: // Time signature FF 58 04 nn dd cc bb
             begin
               inc(fChunkIndex); // $04
               inc(fChunkIndex); // nn
               inc(fChunkIndex); // dd
               inc(fChunkIndex); // cc
               inc(fChunkIndex); // bb
               inc(fChunkIndex);
             end;
        $59: // Key signature FF 59 02 df mi
             begin
               inc(fChunkIndex); // $02
               inc(fChunkIndex); // df
               inc(fChunkIndex); // mi
               inc(fChunkIndex);
             end;
        $7F: // Sequence specific Meta-event
            begin
              inc(fChunkIndex);
              len := ReadVarLength;
              str := ReadString(len);
            end;
        else // unknown meta event
        }
         begin
          New(midiEvent);
          midiEvent.event := $FF;
          midiEvent.data1 := fChunkIndex^; // type is stored in data1
          midiEvent.dticks := dtime;

          inc(fChunkIndex);
          len := ReadVarLength;
          midiEvent.str := ReadString(len);
          fCurrentTrack.putEvent(midiEvent);

          case midiEvent.data1 of
            $51:
              begin
                us_per_quarter :=
                  (Integer(Byte(midiEvent.str[1])) shl 16 +
                  Integer(Byte(midiEvent.str[2])) shl 8 +
                  Integer(Byte(midiEvent.str[3])));
                fBPM := 60000000 div us_per_quarter;
                FBpmOld := fBPM;
                fFusPerTick := us_per_quarter / fDeltaTicks;
              end;
          end;
        end;
//        end;
       end
      else
       begin
      // these are all midi fEvents
        New(midiEvent);
        midiEvent.event := event;
        midiEvent.dticks := dtime;
//         inc(fChunkIndex);
        case event of
          $80..$8F, // note off
          $90..$9F, // note on
          $A0..$AF, // key aftertouch
          $B0..$BF, // control change
          $E0..$EF: // pitch wheel change
            begin
              midiEvent.data1 := fChunkIndex^; inc(fChunkIndex);
              midiEvent.data2 := fChunkIndex^; inc(fChunkIndex);
            end;
          $C0..$CF, // program change
          $D0..$DF: // channel aftertouch
            begin
              midiEvent.data1 := fChunkIndex^; inc(fChunkIndex);
            end;
        else
           // error
        end;
        fCurrentTrack.putEvent(midiEvent);
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
   b := fChunkIndex^;
   i := i + b and $7F;
   inc(fChunkIndex);
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
   s[i] := Chr(fChunkIndex^);
   inc(fChunkIndex);
  end;
 result := string(s);
end;

procedure TMidiFile.ReadFile;
var
  i: Integer;
begin
  for i := 0 to fTracks.Count - 1 do TMidiTrack(fTracks.Items[i]).Free;
  fTracks.Clear;
  fChunkType := ctIllegal;

  AssignFile(fMidiFile, fFilename);
  FileMode := 0;
  Reset(fMidiFile);
  while not EoF(fMidiFile) do ReadChunk;
  CloseFile(fMidiFile);
  fNumberTracks := fTracks.Count;
end;

function KeyToStr(key: Integer): string;
var
  n: Integer;
  str: string;
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

function MyTimeToStr(val: Integer): string;
var
  hour : Integer;
  min  : Integer;
  sec  : Integer;
  msec : Integer;
begin
  msec := val mod 1000;
  sec  := val div 1000;
  min  := sec div 60;
  sec  := sec mod 60;
  hour := min div 60;
  min  := min mod 60;
  Result := IntToStr(hour) + ':' + IntToLenStr(min, 2) + ':' +
            IntToLenStr(sec, 2) + '.' + IntToLenStr(msec, 3);
end;

function TMidiFile.GetfFusPerTick: Double;
begin
 Result := fFusPerTick;
end;

function  TMidiFile.GetTrackLength: Integer;
var
  i, length : Integer;
  time      : Extended;
begin
 length := 0;
 for i := 0 to fTracks.Count - 1 do
  if TMidiTrack(fTracks.Items[i]).getTrackLength > length
   then length := TMidiTrack(fTracks.Items[i]).getTrackLength;
 time := length * fFusPerTick;
 time := time / 1000.0;
 result := round(time);
end;

function TMidiFile.GetTrackLength2: Integer;
var
  i, length: Integer;
begin
 length := 0;
 for i := 0 to fTracks.Count - 1 do
  if TMidiTrack(fTracks.Items[i]).getTrackLength > length
   then length := TMidiTrack(fTracks.Items[i]).getTrackLength;
 result := length;
end;

function TMidiFile.Ready: Boolean;
var
  i : Integer;
begin
 result := True;
 for i := 0 to fTracks.Count - 1 do
  if not TMidiTrack(fTracks.Items[i]).isready then
    result := False;
end;

procedure TMidiFile.OnTrackReady;
begin
 if Ready then
  if assigned(fOnUpdateEvent) then fOnUpdateEvent(Self);
end;

function TMidiFile.GetCurrentPos: Double;
begin
 result := fCurrentPos;
end;

procedure TMidiFile.SetManual(const Value: Boolean);
begin
 FManual := Value;
 fMTimer.Enabled := not Value;
end;

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TMidiFile]);
end;

end.
