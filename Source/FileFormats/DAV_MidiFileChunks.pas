{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_MidiFileChunks;

interface

{$I ..\DAV_Compiler.Inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types, DAV_Common, DAV_ChunkClasses,
  DAV_FileFormat, DAV_MidiFileEvents;

type
  EDavMidiFileChunkError = class(Exception);

  TDavMidiFileHeaderChunk = class(TDavDefinedChunk)
  private
    FFormatType   : UInt16;
    FTrackCount   : UInt16;
    FTimeDivision : UInt16;
    function GetIsTicksPerBeat: Boolean;
    procedure SetIsTicksPerBeat(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;

    property FormatType: UInt16 read FFormatType write FFormatType;
    property TrackCount: UInt16 read FTrackCount write FTrackCount;
    property TimeDivision: UInt16 read FTimeDivision write FTimeDivision;
    property TimeDivisionIsTicksPerBeat: Boolean read GetIsTicksPerBeat write SetIsTicksPerBeat;
  end;

  TDavCustomMidiFileTrackChunk = class(TDavDefinedChunk)
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
  end;

  TDavMidiFileTrackPositionChunk = class(TDavCustomMidiFileTrackChunk)
  private
    FPosition : Int64;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavMidiFileTrackMemoryChunk = class(TDavCustomMidiFileTrackChunk)
  private
    FData : TMemoryStream;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavMidiFileTrackEventsChunk = class(TDavCustomMidiFileTrackChunk)
  private
    FList : TList;
    function GetCount: Int32;
    function GetEvent(Index: Int32): TDavCustomMidiFileTrackEvent;
    function GetMaxDeltaTimeAsUInt32: Cardinal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(Index: Int32; Event: TDavCustomMidiFileTrackEvent);
    procedure Delete(Index: Int32);

    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;

    property Count: Int32 read GetCount;
    property Event[Index: Int32]: TDavCustomMidiFileTrackEvent read GetEvent; default;

    property MaximumDeltaTimeAsUInt32: Cardinal read GetMaxDeltaTimeAsUInt32;
  end;

implementation

uses
  DAV_Strings;

resourcestring
  RCStrEndOfTrackEventMissing = 'End of track event is missing';
  RCStrFormat0Error = 'Format 0 may only contain one track chunk!';
  RCStrUnknownMetaEvent = 'Unknown Meta Event (%d)';
  RCStrUnknownEvent = 'Unknown Event (%d)';
  RCStrEventMayNotBeNil = 'Event may not be nil!';
  RCStrWrongIndexOrDeltaTime = 'Wrong index or delta time';


{ TDavMidiFileHeaderChunk }

procedure TDavMidiFileHeaderChunk.AssignTo(Dest: TPersistent);
begin
  if Dest is TDavMidiFileHeaderChunk then
  with TDavMidiFileHeaderChunk(Dest) do
  begin
    FFormatType   := Self.FFormatType;
    FTrackCount   := Self.FTrackCount;
    FTimeDivision := Self.FTimeDivision;
  end;
end;

constructor TDavMidiFileHeaderChunk.Create;
begin
  inherited;
  FChunkFlags := [cfReversedByteOrder];
end;

class function TDavMidiFileHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'MThd';
end;

function TDavMidiFileHeaderChunk.GetIsTicksPerBeat: Boolean;
begin
  Result := FTimeDivision and $8000 > 0;
end;

procedure TDavMidiFileHeaderChunk.SetIsTicksPerBeat(const Value: Boolean);
begin
  FTimeDivision := (FTimeDivision and $7FFF);
  if Value then
    FTimeDivision := FTimeDivision + $8000
end;

procedure TDavMidiFileHeaderChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  with Stream do
  begin
    // read format type
    Read(FFormatType, 2);
    Flip16(FFormatType);

    // read track count
    Read(FTrackCount, 2);
    Flip16(FTrackCount);

    // read time division
    Read(FTimeDivision, 2);
    Flip16(FTimeDivision);
  end;
end;

procedure TDavMidiFileHeaderChunk.SaveToStream(Stream: TStream);
var
  Value : UInt16;
begin
  inherited;

  with Stream do
  begin
    // write format type
    Value := Swap16(FFormatType);
    Write(Value, 2);

    // write track count
    Value := Swap16(FTrackCount);
    Write(Value, 2);

    if (FFormatType = 0) and (FTrackCount <> 1) then
      raise EDavMidiFileChunkError.Create(RCStrFormat0Error);

    // write time division
    Value := Swap16(FTimeDivision);
    Write(Value, 2);
  end;
end;


{ TDavCustomMidiFileTrackChunk }

constructor TDavCustomMidiFileTrackChunk.Create;
begin
  inherited;
  FChunkFlags := [cfReversedByteOrder];
end;

class function TDavCustomMidiFileTrackChunk.GetClassChunkName: TChunkName;
begin
  Result := 'MTrk';
end;


{ TDavMidiFileTrackPositionChunk }

procedure TDavMidiFileTrackPositionChunk.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TDavMidiFileTrackPositionChunk then
    with TDavMidiFileTrackPositionChunk(Dest) do
    begin
      FPosition := Self.FPosition;
    end;
end;

procedure TDavMidiFileTrackPositionChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  FPosition := Stream.Position;

  // advance
  Stream.Seek(ChunkSize, soFromCurrent)
end;

procedure TDavMidiFileTrackPositionChunk.SaveToStream(Stream: TStream);
begin
  inherited;

end;


{ TDavMidiFileTrackMemoryChunk }

constructor TDavMidiFileTrackMemoryChunk.Create;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TDavMidiFileTrackMemoryChunk.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TDavMidiFileTrackMemoryChunk.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TDavMidiFileTrackMemoryChunk then
    with TDavMidiFileTrackMemoryChunk(Dest) do
    begin
      FData.Clear;
      Self.FData.Seek(0, soFromBeginning);
      FData.CopyFrom(Self.FData, FData.Size);
    end;
end;

procedure TDavMidiFileTrackMemoryChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  FData.Clear;
  FData.CopyFrom(Stream, ChunkSize);
end;

procedure TDavMidiFileTrackMemoryChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  FData.Seek(0, soFromBeginning);
  Stream.CopyFrom(FData, ChunkSize);
end;


{ TDavMidiFileTrackEventsChunk }

constructor TDavMidiFileTrackEventsChunk.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TDavMidiFileTrackEventsChunk.Destroy;
begin
  while FList.Count > 0 do
  begin
    TDavCustomMidiFileTrackEvent(FList[0]).Free;
    FList.Delete(0);
  end;

  FreeAndNil(FList);
  inherited;
end;

procedure TDavMidiFileTrackEventsChunk.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TDavMidiFileTrackEventsChunk then
    with TDavMidiFileTrackEventsChunk(Dest) do
    begin
      FList.Assign(Self.FList);
    end;
end;

procedure TDavMidiFileTrackEventsChunk.Add(Index: Int32;
  Event: TDavCustomMidiFileTrackEvent);
begin
  if not Assigned(Event) then
    raise EDavMidiFileChunkError.Create(RCStrEventMayNotBeNil);

  // eventually shift delta time
  if Index < FList.Count then
    with TDavCustomMidiFileTrackEvent(FList[Index]) do
      if DeltaTimeAsUInt32 > Event.DeltaTimeAsUInt32 then
        DeltaTimeAsUInt32 := DeltaTimeAsUInt32 - Event.DeltaTimeAsUInt32
      else
        raise EDavMidiFileChunkError.Create(RCStrWrongIndexOrDeltaTime);

  FList.Insert(Index, Event);
end;

procedure TDavMidiFileTrackEventsChunk.Delete(Index: Int32);
begin
  if (Index < 0) or (Index >= FList.Count) then
    raise EDavMidiFileChunkError.CreateFmt(RStrIndexOutOfBounds, [Index])
  else
  begin
    // eventually shift delta time
    if Index + 1 < FList.Count then
      with TDavCustomMidiFileTrackEvent(FList[Index + 1]) do
        DeltaTimeAsUInt32 := DeltaTimeAsUInt32 +
          TDavCustomMidiFileTrackEvent(FList[Index]).DeltaTimeAsUInt32;

    // delete event from list
    FList.Delete(Index);
  end;
end;

function TDavMidiFileTrackEventsChunk.GetCount: Int32;
begin
  Result := FList.Count;
end;

function TDavMidiFileTrackEventsChunk.GetEvent(
  Index: Int32): TDavCustomMidiFileTrackEvent;
begin
  if (Index < 0) or (Index >= FList.Count) then
    raise EDavMidiFileChunkError.CreateFmt(RStrIndexOutOfBounds, [Index])
  else
    Result := TDavCustomMidiFileTrackEvent(FList[Index]);
end;

function TDavMidiFileTrackEventsChunk.GetMaxDeltaTimeAsUInt32: Cardinal;
var
  EventIndex: Integer;
begin
  Result := 0;
  for EventIndex := 0 to Count - 1 do
  begin
    Result := Result + Event[EventIndex].DeltaTimeAsUInt32;
  end;
end;

procedure TDavMidiFileTrackEventsChunk.LoadFromStream(Stream: TStream);
var
  EndPos    : Int64;
  Value     : Byte;
  DeltaTime : TDavMidiVariableLength;
  Event     : TDavCustomMidiFileTrackEvent;
const
  EventClasses : array [$8..$E] of TDavCustomMidiFileTrackChannelEventClass = (
    TDavMidiFileTrackNoteOnEvent, TDavMidiFileTrackNoteOffEvent,
    TDavMidiFileTrackNoteAftertouchEvent, TDavMidiFileTrackControllerEvent,
    TDavMidiFileTrackProgramChangeEvent,
    TDavMidiFileTrackChannelAftertouchEvent, TDavMidiFileTrackPitchBendEvent
  );
  MetaEventClasses : array [$0..$7] of TDavCustomMidiFileTrackMetaEventClass = (
    TDavMidiFileTrackMetaSequencyNumberEvent, TDavMidiFileTrackMetaTextEvent,
    TDavMidiFileTrackMetaCopyrightEvent, TDavMidiFileTrackMetaNameEvent,
    TDavMidiFileTrackMetaInstrumentEvent, TDavMidiFileTrackMetaLyricsEvent,
    TDavMidiFileTrackMetaMarkerEvent, TDavMidiFileTrackMetaCuePointEvent
  );
begin
  inherited;

  with Stream do
  begin
    Event := nil;
    EndPos := Position + ChunkSize;
    while Position < EndPos do
    begin
      // read delta time from stream
      DeltaTime := TDavMidiVariableLength.Create;
      DeltaTime.LoadFromStream(Stream);

      // read event type and channel
      Read(Value, 1);

      case Value shr 4 of
        $8..$E : Event := EventClasses[Value shr 4].Create(DeltaTime, Value and $F);
        $F : case Value and $F of
               $0 : begin
                      // start sysex
                    end;
               $7 : begin
                      // continue sysex
                    end;
               $F : begin
                      // read meta event type
                      Read(Value, 1);

                      case Value of
                        $0..$7 : Event := MetaEventClasses[Value].Create(DeltaTime);
                        $20    : Event := TDavMidiFileTrackMetaMidiChannelPrefixEvent.Create(DeltaTime);
                        $2F    : Event := TDavMidiFileTrackMetaEndOfTrackEvent.Create(DeltaTime);
                        $51    : Event := TDavMidiFileTrackMetaTempoEvent.Create(DeltaTime);
                        $54    : Event := TDavMidiFileTrackMetaSMPTEOffsetEvent.Create(DeltaTime);
                        $58    : Event := TDavMidiFileTrackMetaTimeSignatureEvent.Create(DeltaTime);
                        $59    : Event := TDavMidiFileTrackMetaKeySignaturEvent.Create(DeltaTime);
                        $7F    : Event := TDavMidiFileTrackMetaSequencerSpecificEvent.Create(DeltaTime);
                        else raise EDavMidiFileChunkError.CreateFmt(RCStrUnknownMetaEvent, [Value]);
                      end;
                    end;
               else raise EDavMidiFileChunkError.CreateFmt(RCStrUnknownEvent, [Value]);
             end;
        else
          if not (Event is TDavCustomMidiFileTrackChannelEvent) then
            raise EDavMidiFileChunkError.CreateFmt(RCStrUnknownEvent, [Value shr 4])
          else
          begin
            with (TDavCustomMidiFileTrackChannelEventClass(Event.ClassType)) do
              Event := Create(DeltaTime, TDavCustomMidiFileTrackChannelEvent(Event).Channel);
            Stream.Seek(-1, soFromCurrent);
          end;
      end;

      // load event data from stream
      Event.LoadFromStream(Stream);

      // add event to event list
      FList.Add(Event);
    end;

    // verify that the last event is an end of track event
    if not (Event is TDavMidiFileTrackMetaEndOfTrackEvent) then
      raise EDavMidiFileChunkError.Create(RCStrEndOfTrackEventMissing);
  end;
end;

procedure TDavMidiFileTrackEventsChunk.SaveToStream(Stream: TStream);
var
  Index     : Integer;
  EventType : array [0..1] of Byte;
begin
  inherited;

  with Stream do
  begin
    EventType[0] := $F;
    EventType[1] := $F;
    for Index := 0 to FList.Count - 1 do
    begin

      EventType[1] := TDavCustomMidiFileTrackEvent(FList[Index]).GetEventTypeValue shr 4;
      if (EventType[0] < $F) and (EventType[0] = EventType[1]) then
      begin
        // two events are equal
        Assert(TObject(FList[Index]) is TDavCustomMidiFileTrackChannelEvent);
        TDavCustomMidiFileTrackChannelEvent(FList[Index]).SaveToStream(Stream, True);
      end else
        TDavCustomMidiFileTrackEvent(FList[Index]).SaveToStream(Stream);
      EventType[0] := EventType[1];
    end;
  end;
end;

end.
