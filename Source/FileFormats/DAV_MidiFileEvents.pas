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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_MidiFileEvents;

interface

{$I ..\DAV_Compiler.Inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types, DAV_ChunkClasses, DAV_FileFormat;

type
  EDavMidiEventError = class(Exception);

  TDavMidiVariableLength = class(TInterfacedPersistent, IStreamPersist)
  private
    function GetAsUInt32: UInt32;
    procedure SetAsUInt32(Value: UInt32);
  protected
    FVariableLength : array of Byte;
  public
    constructor Create; virtual;

    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;

    property AsUInt32: UInt32 read GetAsUInt32 write SetAsUInt32;
  end;

  TDavCustomMidiFileTrackEvent = class(TInterfacedPersistent, IStreamPersist)
  private
    function GetDeltaTimeAsUInt32: UInt32;
    procedure SetDeltaTimeAsUInt32(const Value: UInt32);
  protected
    FDeltaTime : TDavMidiVariableLength;
    procedure DeltaTimeChanged;
  public
    constructor Create(DeltaTime: TDavMidiVariableLength); virtual;
    destructor Destroy; override;

    class function GetEventTypeValue: Byte; virtual; abstract;

    procedure LoadFromStream(Stream : TStream); virtual; abstract;
    procedure SaveToStream(Stream : TStream); virtual;

    property DeltaTimeAsUInt32: UInt32 read GetDeltaTimeAsUInt32 write SetDeltaTimeAsUInt32;
  end;
  TDavCustomMidiFileTrackEventClass = class of TDavCustomMidiFileTrackEvent;

  TDavCustomMidiFileTrackChannelEvent = class(TDavCustomMidiFileTrackEvent)
  private
    FChannel   : Byte;
    procedure SetChannel(const Value: Byte);
  protected
    procedure ChannelChanged;
  public
    constructor Create(DeltaTime: TDavMidiVariableLength; Channel: Byte); reintroduce;

    procedure SaveToStream(Stream : TStream); overload; override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); overload; virtual;

    property Channel: Byte read FChannel write SetChannel;
  end;
  TDavCustomMidiFileTrackChannelEventClass = class of TDavCustomMidiFileTrackChannelEvent;

  TDavCustomMidiFileTrackNoteEvent = class(TDavCustomMidiFileTrackChannelEvent)
  private
    FParameter : array [0..1] of Byte;
    function GetNoteValue: Byte;
    procedure SetNoteValue(const Value: Byte);
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); override;

    property NoteValue: Byte read GetNoteValue write SetNoteValue;
  end;

  TDavCustomMidiFileTrackNoteVelocityEvent = class(TDavCustomMidiFileTrackNoteEvent)
  private
    function GetVelocity: Byte;
    procedure SetVelocity(const Value: Byte);
  public
    property Velocity: Byte read GetVelocity write SetVelocity;
  end;

  TDavMidiFileTrackNoteOnEvent = class(TDavCustomMidiFileTrackNoteVelocityEvent)
  public
    class function GetEventTypeValue: Byte; override;
  end;

  TDavMidiFileTrackNoteOffEvent = class(TDavCustomMidiFileTrackNoteVelocityEvent)
  public
    class function GetEventTypeValue: Byte; override;
  end;

  TDavMidiFileTrackNoteAftertouchEvent = class(TDavCustomMidiFileTrackNoteEvent)
  private
    function GetAmount: Byte;
    procedure SetAmount(const Value: Byte);
  public
    class function GetEventTypeValue: Byte; override;
    property Amount: Byte read GetAmount write SetAmount;
  end;

  TDavMidiFileTrackControllerEvent = class(TDavCustomMidiFileTrackChannelEvent)
  private
    FController : Byte;
    FValue      : Byte;
    procedure SetController(const Value: Byte);
    procedure SetValue(const Value: Byte);
  protected
    procedure ControllerChanged;
    procedure ValueChanged;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); override;
    class function GetEventTypeValue: Byte; override;

    property Controller: Byte read FController write SetController;
    property Value: Byte read FValue write SetValue;
  end;

  TDavMidiFileTrackProgramChangeEvent = class(TDavCustomMidiFileTrackChannelEvent)
  private
    FProgramNumber : Byte;
    procedure SetProgramNumber(const Value: Byte);
  protected
    procedure ProgramNumberChanged;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); override;
    class function GetEventTypeValue: Byte; override;

    property ProgramNumber: Byte read FProgramNumber write SetProgramNumber;
  end;

  TDavMidiFileTrackChannelAftertouchEvent = class(TDavCustomMidiFileTrackChannelEvent)
  private
    FAmount : Byte;
    procedure SetAmount(const Value: Byte);
  protected
    procedure AftertouchChanged;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); override;
    class function GetEventTypeValue: Byte; override;

    property Amount: Byte read FAmount write SetAmount;
  end;

  TDavMidiFileTrackPitchBendEvent = class(TDavCustomMidiFileTrackChannelEvent)
  private
    FParameter : array [0..1] of Byte;
    function GetAmount: Int16;
    procedure SetAmount(Value: Int16);
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream; Short: Boolean); override;
    class function GetEventTypeValue: Byte; override;

    property Amount: Int16 read GetAmount write SetAmount;
  end;

  TDavCustomMidiFileTrackMetaEvent = class(TDavCustomMidiFileTrackEvent)
  protected
    class function GetMetaEventTypeValue: Byte; virtual; abstract;
    function GetMetaDataSize: UInt32; virtual; abstract;
  public
    constructor Create(DeltaTime: TDavMidiVariableLength); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetEventTypeValue: Byte; override;

    property MetaDataSize: UInt32 read GetMetaDataSize;
  end;
  TDavCustomMidiFileTrackMetaEventClass = class of TDavCustomMidiFileTrackMetaEvent;

  TDavMidiFileTrackMetaSequencyNumberEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FSequencyNumber : Int16;
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property SequencyNumber: Int16 read FSequencyNumber write FSequencyNumber;
  end;

  TDavCustomMidiFileTrackMetaTextEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    procedure SetText(const Value: AnsiString);
  protected
    FText : AnsiString;
    function GetMetaDataSize: UInt32; override;
    procedure TextChanged;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Text: AnsiString read FText write SetText;
  end;

  TDavMidiFileTrackMetaTextEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property Text: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaCopyrightEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property Copyright: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaNameEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property Name: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaInstrumentEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property InstrumentName: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaLyricsEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property LyricsName: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaMarkerEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property Marker: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaCuePointEvent = class(TDavCustomMidiFileTrackMetaTextEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
  public
    property CuePoint: AnsiString read FText write FText;
  end;

  TDavMidiFileTrackMetaMidiChannelPrefixEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FMidiChannelPrefix: Byte;
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property MidiChannelPrefix: Byte read FMidiChannelPrefix write FMidiChannelPrefix;
  end;

  TDavMidiFileTrackMetaEndOfTrackEvent = class(TDavCustomMidiFileTrackMetaEvent)
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TDavMidiFileTrackMetaTempoEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FTempo: Int32;
    procedure SetTempo(Value: Int32);
    function GetBPM: Single;
  protected
    procedure TempoChanged;
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property MicrosecondsPerQuarterNote: Int32 read FTempo write SetTempo;
    property BeatsPerMinute: Single read GetBPM;
  end;

  TDavMidiFileTrackMetaSMPTEOffsetEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FMinutes   : Byte;
    FSubFrames : Byte;
    FFrames    : Byte;
    FSeconds   : Byte;
    FHour      : Byte;
    procedure SetFrames(const Value: Byte);
    procedure SetHour(const Value: Byte);
    procedure SetMinutes(const Value: Byte);
    procedure SetSeconds(const Value: Byte);
    procedure SetSubFrames(const Value: Byte);
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
    procedure FramesChanged;
    procedure HourChanged;
    procedure MinutesChanged;
    procedure SecondsChanged;
    procedure SubFramesChanged;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Hour: Byte read FHour write SetHour;
    property Minutes: Byte read FMinutes write SetMinutes;
    property Seconds: Byte read FSeconds write SetSeconds;
    property Frames: Byte read FFrames write SetFrames;
    property SubFrames: Byte read FSubFrames write SetSubFrames;
  end;

  TDavMidiFileTrackMetaTimeSignatureEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FThirtyTwo   : Byte;
    FMetronome   : Byte;
    FNumerator   : Byte;
    FDenominator : Byte;
    procedure SetDenominator(const Value: Byte);
    procedure SetMetronome(const Value: Byte);
    procedure SetNumerator(const Value: Byte);
    procedure SetThirtyTwo(const Value: Byte);
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: UInt32; override;
    procedure DenominatorChanged;
    procedure MetronomeChanged;
    procedure NumeratorChanged;
    procedure ThirtyTwoChanged;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Numerator: Byte read FNumerator write SetNumerator;
    property Denominator: Byte read FDenominator write SetDenominator;
    property Metronome: Byte read FMetronome write SetMetronome;
    property ThirtyTwo: Byte read FThirtyTwo write SetThirtyTwo;
  end;

  TDavMidiFileTrackMetaKeySignaturEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FKey   : Int8;
    FScale : Byte;
    procedure SetKey(const Value: Int8);
    procedure SetScale(const Value: Byte);
  protected
    class function GetMetaEventTypeValue: Byte; override;
    procedure KeyChanged;
    procedure ScaleChanged;
    function GetMetaDataSize: UInt32; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Key: Int8 read FKey write SetKey;
    property Scale: Byte read FScale write SetScale;
  end;

  TDavMidiFileTrackMetaSequencerSpecificEvent = class(TDavCustomMidiFileTrackMetaEvent)
  private
    FData : TMemoryStream;
  protected
    class function GetMetaEventTypeValue: Byte; override;
    function GetMetaDataSize: Cardinal; override;
  public
    constructor Create(DeltaTime: TDavMidiVariableLength); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TDavMidiFileTrackNormalSysExEvent = class(TDavCustomMidiFileTrackEvent)
  public
    constructor Create(DeltaTime: TDavMidiVariableLength); override;
    class function GetEventTypeValue: Byte; override;

    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

implementation

uses
  DAV_Math;

resourcestring
  RCStrUnknownEventSize = 'Unknown event size (%d)';

{ TDavMidiVariableLength }

constructor TDavMidiVariableLength.Create;
begin
  inherited Create;
  SetLength(FVariableLength, 0);
end;

function TDavMidiVariableLength.GetAsUInt32: UInt32;
var
  Index : Integer;
begin
  Result := 0;
  if Length(FVariableLength) > 1 then
    Result := 2 * Result;

//  for Index := Length(FVariableLength) - 1 downto 0 do
  for Index := 0 to Length(FVariableLength) - 1 do
  begin
    Result := Result shl 7 + (FVariableLength[Index] and $7F);
  end;
end;

procedure TDavMidiVariableLength.SetAsUInt32(Value: UInt32);
var
  Data  : array of Byte;
  Index : Byte;
begin
  SetLength(Data, 0);
  repeat
    SetLength(Data, Length(Data) + 1);
    Data[Length(Data) - 1] := Value and $7F;
    Value := Value shr 7;
  until (Value = 0);

  SetLength(FVariableLength, Length(Data));
  for Index := 0 to Length(Data) - 1 do
    FVariableLength[Index] := Data[Length(Data) - 1 - Index];
end;

procedure TDavMidiVariableLength.LoadFromStream(Stream: TStream);
var
  Value : Byte;
begin
  with Stream do
  begin
    SetLength(FVariableLength, 0);

    // read delta time
    repeat
      Read(Value, 1);
      SetLength(FVariableLength, Length(FVariableLength) + 1);
      FVariableLength[Length(FVariableLength) - 1] := Value;
    until (Value and $80) = 0;
  end;
end;

procedure TDavMidiVariableLength.SaveToStream(Stream: TStream);
begin
  inherited;
  Assert(Length(FVariableLength) > 0);
  Stream.Write(FVariableLength[0], Length(FVariableLength));
end;


{ TDavCustomMidiFileTrackEvent }

constructor TDavCustomMidiFileTrackEvent.Create(DeltaTime: TDavMidiVariableLength);
begin
  inherited Create;
  FDeltaTime := DeltaTime;
end;

procedure TDavCustomMidiFileTrackEvent.DeltaTimeChanged;
begin
  // delta time changed
end;

destructor TDavCustomMidiFileTrackEvent.Destroy;
begin
  FreeAndNil(FDeltaTime);
  inherited;
end;

procedure TDavCustomMidiFileTrackEvent.SaveToStream(Stream: TStream);
begin
  inherited;
  FDeltaTime.SaveToStream(Stream);
end;

procedure TDavCustomMidiFileTrackEvent.SetDeltaTimeAsUInt32(const Value: UInt32);
begin
  if FDeltaTime.AsUInt32 <> Value then
  begin
    FDeltaTime.AsUInt32 := Value;
    DeltaTimeChanged;
  end;
end;

function TDavCustomMidiFileTrackEvent.GetDeltaTimeAsUInt32: UInt32;
begin
  Result := FDeltaTime.AsUInt32;
end;


{ TDavCustomMidiFileTrackChannelDeltaTimeEvent }

constructor TDavCustomMidiFileTrackChannelEvent.Create(
  DeltaTime: TDavMidiVariableLength; Channel: Byte);
begin
  inherited Create(DeltaTime);
  FChannel := Channel;
end;

procedure TDavCustomMidiFileTrackChannelEvent.ChannelChanged;
begin
  // channel changed
end;

procedure TDavCustomMidiFileTrackChannelEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
var
  Value : Byte;
begin
  inherited SaveToStream(Stream);

  if not Short then
  begin
    // write event type & channel to stream
    Value := (GetEventTypeValue and $F0) or (FChannel and $F);
    Stream.Write(Value, 1)
  end;
end;

procedure TDavCustomMidiFileTrackChannelEvent.SaveToStream(Stream: TStream);
begin
  // call save to stream (not short)
  SaveToStream(Stream, False);
end;

procedure TDavCustomMidiFileTrackChannelEvent.SetChannel(const Value: Byte);
begin
  if FChannel <> Value then
  begin
    FChannel := Value;
    ChannelChanged;
  end;
end;


{ TDavCustomMidiFileTrackNoteEvent }

function TDavCustomMidiFileTrackNoteEvent.GetNoteValue: Byte;
begin
  Result := FParameter[0];
end;

procedure TDavCustomMidiFileTrackNoteEvent.SetNoteValue(const Value: Byte);
begin
  FParameter[0] := Value;
end;

procedure TDavCustomMidiFileTrackNoteEvent.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read parameters
  Stream.Read(FParameter[0], 2);
end;

procedure TDavCustomMidiFileTrackNoteEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
begin
  inherited;

  // write parameters to stream
  Stream.Write(FParameter[0], 2);
end;


{ TDavCustomMidiFileTrackNoteVelocityEvent }

function TDavCustomMidiFileTrackNoteVelocityEvent.GetVelocity: Byte;
begin
  Result := FParameter[1];
end;

procedure TDavCustomMidiFileTrackNoteVelocityEvent.SetVelocity(
  const Value: Byte);
begin
  FParameter[1] := Value;
end;


{ TDavMidiFileTrackNoteOnEvent }

class function TDavMidiFileTrackNoteOnEvent.GetEventTypeValue: Byte;
begin
  Result := $80;
end;


{ TDavMidiFileTrackNoteOffEvent }

class function TDavMidiFileTrackNoteOffEvent.GetEventTypeValue: Byte;
begin
  Result := $90;
end;


{ TDavMidiFileTrackNoteAftertouchEvent }

class function TDavMidiFileTrackNoteAftertouchEvent.GetEventTypeValue: Byte;
begin
  Result := $A0;
end;

function TDavMidiFileTrackNoteAftertouchEvent.GetAmount: Byte;
begin
  Result := FParameter[1];
end;

procedure TDavMidiFileTrackNoteAftertouchEvent.SetAmount(const Value: Byte);
begin
  FParameter[1] := Value;
end;


{ TDavMidiFileTrackControllerEvent }

class function TDavMidiFileTrackControllerEvent.GetEventTypeValue: Byte;
begin
  Result := $B0;
end;

procedure TDavMidiFileTrackControllerEvent.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read controller
  Stream.Read(FController, 1);

  // read value
  Stream.Read(FValue, 1);
end;

procedure TDavMidiFileTrackControllerEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
begin
  inherited;

  // read controller
  Stream.Write(FController, 1);

  // read value
  Stream.Write(FValue, 1);
end;

procedure TDavMidiFileTrackControllerEvent.SetController(const Value: Byte);
begin
  if FController <> Value then
  begin
    FController := Value;
    ControllerChanged;
  end;
end;

procedure TDavMidiFileTrackControllerEvent.SetValue(const Value: Byte);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    ValueChanged;
  end;
end;

procedure TDavMidiFileTrackControllerEvent.ControllerChanged;
begin
  // controller changed
end;

procedure TDavMidiFileTrackControllerEvent.ValueChanged;
begin
  // value changed
end;


{ TDavMidiFileTrackProgramChangeEvent }

class function TDavMidiFileTrackProgramChangeEvent.GetEventTypeValue: Byte;
begin
  Result := $C0;
end;

procedure TDavMidiFileTrackProgramChangeEvent.SetProgramNumber(
  const Value: Byte);
begin
  if FProgramNumber <> Value then
  begin
    FProgramNumber := Value;
    ProgramNumberChanged;
  end;
end;

procedure TDavMidiFileTrackProgramChangeEvent.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read program number from stream
  Stream.Read(FProgramNumber, 1);
end;

procedure TDavMidiFileTrackProgramChangeEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
begin
  inherited;

  // write program number to stream
  Stream.Write(FProgramNumber, 1);
end;

procedure TDavMidiFileTrackProgramChangeEvent.ProgramNumberChanged;
begin
  // program number changed
end;


{ TDavMidiFileTrackChannelAftertouchEvent }

class function TDavMidiFileTrackChannelAftertouchEvent.GetEventTypeValue: Byte;
begin
  Result := $D0;
end;

procedure TDavMidiFileTrackChannelAftertouchEvent.AftertouchChanged;
begin
  // aftertouch amount changed
end;

procedure TDavMidiFileTrackChannelAftertouchEvent.LoadFromStream(
  Stream: TStream);
begin
  inherited;

  // read program number from stream
  Stream.Read(FAmount, 1);
end;

procedure TDavMidiFileTrackChannelAftertouchEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
begin
  inherited;

  // write program number from stream
  Stream.Write(FAmount, 1);
end;

procedure TDavMidiFileTrackChannelAftertouchEvent.SetAmount(const Value: Byte);
begin
  if FAmount <> Value then
  begin
    FAmount := Value;
    AftertouchChanged;
  end;
end;


{ TDavMidiFileTrackPitchBendEvent }

class function TDavMidiFileTrackPitchBendEvent.GetEventTypeValue: Byte;
begin
  Result := $E0;
end;

function TDavMidiFileTrackPitchBendEvent.GetAmount: Int16;
begin
  Result := (FParameter[1] shl 7 + FParameter[0]) - 8192;
end;

procedure TDavMidiFileTrackPitchBendEvent.SetAmount(Value: Int16);
begin
  Value := Value + 8192;
  FParameter[0] := Value and $F;
  FParameter[1] := (Value shr 7) and $F;
end;

procedure TDavMidiFileTrackPitchBendEvent.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read parameters
  Stream.Read(FParameter[0], 2);
end;

procedure TDavMidiFileTrackPitchBendEvent.SaveToStream(Stream: TStream;
  Short: Boolean);
begin
  inherited;

  // write parameters
  Stream.Write(FParameter[0], 2);
end;


{ TDavCustomMidiFileTrackMetaEvent }

constructor TDavCustomMidiFileTrackMetaEvent.Create(DeltaTime: TDavMidiVariableLength);
begin
  inherited Create(DeltaTime);
end;

class function TDavCustomMidiFileTrackMetaEvent.GetEventTypeValue: Byte;
begin
  Result := $FF;
end;

procedure TDavCustomMidiFileTrackMetaEvent.SaveToStream(Stream: TStream);
var
  EventType : Byte;
begin
  inherited;

  // write event type to stream
  EventType := GetEventTypeValue;
  Stream.Write(EventType, 1);

  // write meta event type to stream
  EventType := GetMetaEventTypeValue;
  Stream.Write(EventType, 1);
end;


{ TDavMidiFileTrackMetaSequencyNumberEvent }

class function TDavMidiFileTrackMetaSequencyNumberEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 0;
end;

function TDavMidiFileTrackMetaSequencyNumberEvent.GetMetaDataSize: UInt32;
begin
  Result := 2;
end;

procedure TDavMidiFileTrackMetaSequencyNumberEvent.LoadFromStream(
  Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // read size
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read sequency number
    Read(FSequencyNumber, 2);
  end;
end;

procedure TDavMidiFileTrackMetaSequencyNumberEvent.SaveToStream(
  Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // write sequency number
    Write(FSequencyNumber, 2);
  end;
end;


{ TDavCustomMidiFileTrackMetaTextEvent }

procedure TDavCustomMidiFileTrackMetaTextEvent.LoadFromStream(Stream: TStream);
var
  TextLength : Int32;
begin
  inherited;
  with Stream do
  begin
    // read text length as variable length
    with TDavMidiVariableLength.Create do
    try
      LoadFromStream(Stream);
      TextLength := AsUInt32;
    finally
      Free;
    end;

    // set text length
    SetLength(FText, TextLength);

    // finally read text
    Read(FText[1], TextLength);
  end;
end;

function TDavCustomMidiFileTrackMetaTextEvent.GetMetaDataSize: UInt32;
begin
  Result := Length(FText);
end;

procedure TDavCustomMidiFileTrackMetaTextEvent.SaveToStream(Stream: TStream);
begin
  inherited;

  with Stream do
  begin
    // write text length as variable length
    with TDavMidiVariableLength.Create do
    try
      AsUInt32 := Length(FText);
      SaveToStream(Stream);
    finally
      Free;
    end;

    // write text
    Write(FText[1], Length(FText));
  end;
end;

procedure TDavCustomMidiFileTrackMetaTextEvent.SetText(const Value: AnsiString);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TDavCustomMidiFileTrackMetaTextEvent.TextChanged;
begin
  // text changed
end;


{ TDavMidiFileTrackMetaTextEvent }

class function TDavMidiFileTrackMetaTextEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 1;
end;


{ TDavMidiFileTrackMetaCopyrightEvent }

class function TDavMidiFileTrackMetaCopyrightEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 2;
end;


{ TDavMidiFileTrackMetaNameEvent }

class function TDavMidiFileTrackMetaNameEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 3;
end;


{ TDavMidiFileTrackMetaInstrumentEvent }

class function TDavMidiFileTrackMetaInstrumentEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 4;
end;


{ TDavMidiFileTrackMetaLyricsEvent }

class function TDavMidiFileTrackMetaLyricsEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 5;
end;


{ TDavMidiFileTrackMetaMarkerEvent }

class function TDavMidiFileTrackMetaMarkerEvent.GetMetaEventTypeValue: Byte;
begin
  Result :=  6;
end;


{ TDavMidiFileTrackMetaCuePointEvent }

class function TDavMidiFileTrackMetaCuePointEvent.GetMetaEventTypeValue: Byte;
begin
  Result := 7;
end;


{ TDavMidiFileTrackMetaMidiChannelPrefixEvent }

class function TDavMidiFileTrackMetaMidiChannelPrefixEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $20;
end;

function TDavMidiFileTrackMetaMidiChannelPrefixEvent.GetMetaDataSize: UInt32;
begin
  Result := 1;
end;

procedure TDavMidiFileTrackMetaMidiChannelPrefixEvent.LoadFromStream(
  Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read MIDI channel prefix
    Read(FMidiChannelPrefix, 1);
  end;
end;

procedure TDavMidiFileTrackMetaMidiChannelPrefixEvent.SaveToStream(
  Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // read MIDI channel prefix
    Write(FMidiChannelPrefix, 1);
  end;
end;


{ TDavMidiFileTrackMetaEndOfTrackEvent }

function TDavMidiFileTrackMetaEndOfTrackEvent.GetMetaDataSize: UInt32;
begin
  Result := 0;
end;

class function TDavMidiFileTrackMetaEndOfTrackEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $2F;
end;

procedure TDavMidiFileTrackMetaEndOfTrackEvent.LoadFromStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);
  end;
end;

procedure TDavMidiFileTrackMetaEndOfTrackEvent.SaveToStream(Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);
  end;
end;


{ TDavMidiFileTrackMetaTempoEvent }

function TDavMidiFileTrackMetaTempoEvent.GetMetaDataSize: UInt32;
begin
  Result := 3;
end;

class function TDavMidiFileTrackMetaTempoEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $51;
end;

procedure TDavMidiFileTrackMetaTempoEvent.LoadFromStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read tempo
    FTempo := 0;
    Read(FTempo, 3);
  end;
end;

procedure TDavMidiFileTrackMetaTempoEvent.SaveToStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // write tempo
    Write(FTempo, 3);
  end;
end;

function TDavMidiFileTrackMetaTempoEvent.GetBPM: Single;
begin
  Result := 60000000 / FTempo;
end;

procedure TDavMidiFileTrackMetaTempoEvent.SetTempo(Value: Int32);
begin
  // limit value
  if Value < 0 then Value := 0 else
  if Value > 8355711 then Value := 8355711;

  if FTempo <> Value then
  begin
    FTempo := Value;
    TempoChanged;
  end;
end;

procedure TDavMidiFileTrackMetaTempoEvent.TempoChanged;
begin
  // tempo changed
end;


{ TDavMidiFileTrackMetaSMPTEOffsetEvent }

function TDavMidiFileTrackMetaSMPTEOffsetEvent.GetMetaDataSize: UInt32;
begin
  Result := 5;
end;

class function TDavMidiFileTrackMetaSMPTEOffsetEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $54;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.FramesChanged;
begin
  // frames changed
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.HourChanged;
begin
  // hour changed
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.MinutesChanged;
begin
  // minutes changed
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SecondsChanged;
begin
  // seconds changed
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SubFramesChanged;
begin
  // sub frames changed
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.LoadFromStream(Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read hour
    Read(FHour, 1);

    // read minutes
    Read(FMinutes, 1);

    // read seconds
    Read(FSeconds, 1);

    // read frames
    Read(FFrames, 1);

    // read subframes
    Read(FSubFrames, 1);
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SaveToStream(Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // write hour
    Write(FHour, 1);

    // write minutes
    Write(FMinutes, 1);

    // write seconds
    Write(FSeconds, 1);

    // write frames
    Write(FFrames, 1);

    // write subframes
    Write(FSubFrames, 1);
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SetFrames(const Value: Byte);
begin
 if FFrames <> Value then
  begin
   FFrames := Value;
   FramesChanged;
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SetHour(const Value: Byte);
begin
 if FHour <> Value then
  begin
   FHour := Value;
   HourChanged;
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SetMinutes(const Value: Byte);
begin
 if FMinutes <> Value then
  begin
   FMinutes := Value;
   MinutesChanged;
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SetSeconds(const Value: Byte);
begin
 if FSeconds <> Value then
  begin
   FSeconds := Value;
   SecondsChanged;
  end;
end;

procedure TDavMidiFileTrackMetaSMPTEOffsetEvent.SetSubFrames(const Value: Byte);
begin
 if FSubFrames <> Value then
  begin
   FSubFrames := Value;
   SubFramesChanged;
  end;
end;


{ TDavMidiFileTrackMetaTimeSignatureEvent }

class function TDavMidiFileTrackMetaTimeSignatureEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $58;
end;

function TDavMidiFileTrackMetaTimeSignatureEvent.GetMetaDataSize: UInt32;
begin
  Result := 4;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.DenominatorChanged;
begin
  // denominator changed
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.MetronomeChanged;
begin
  // metronome changed
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.NumeratorChanged;
begin
  // numerator changed
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.ThirtyTwoChanged;
begin
  // 32nds changed
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.LoadFromStream(
  Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read numerator
    Read(FNumerator, 1);

    // read denominator
    Read(FDenominator, 1);

    // read metronome
    Read(FMetronome, 1);

    // read 32nds
    Read(FThirtyTwo, 1);
  end;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.SaveToStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // write numerator
    Write(FNumerator, 1);

    // write denominator
    Write(FDenominator, 1);

    // write metronome
    Write(FMetronome, 1);

    // write 32nds
    Write(FThirtyTwo, 1);
  end;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.SetDenominator(
  const Value: Byte);
begin
 if FDenominator <> Value then
  begin
   FDenominator := Value;
   DenominatorChanged;
  end;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.SetMetronome(
  const Value: Byte);
begin
 if FMetronome <> Value then
  begin
   FMetronome := Value;
   MetronomeChanged;
  end;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.SetNumerator(
  const Value: Byte);
begin
 if FNumerator <> Value then
  begin
   FNumerator := Value;
   NumeratorChanged;
  end;
end;

procedure TDavMidiFileTrackMetaTimeSignatureEvent.SetThirtyTwo(
  const Value: Byte);
begin
 if FThirtyTwo <> Value then
  begin
   FThirtyTwo := Value;
   ThirtyTwoChanged;
  end;
end;


{ TDavMidiFileTrackMetaKeySignaturEvent }

class function TDavMidiFileTrackMetaKeySignaturEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $59;
end;

function TDavMidiFileTrackMetaKeySignaturEvent.GetMetaDataSize: UInt32;
begin
  Result := 2;
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.KeyChanged;
begin
  // key changed
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.LoadFromStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // read size (non-variable)
    Read(Value, 1);
    if Value <> MetaDataSize then
      EDavMidiEventError.CreateFmt(RCStrUnknownEventSize, [Value]);

    // read key
    Read(FKey, 1);

    // read scale
    Read(FScale, 1);
  end;
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.SaveToStream(Stream: TStream);
var
  Value : Byte;
begin
  inherited;

  with Stream do
  begin
    // write size (non-variable)
    Value := MetaDataSize;
    Write(Value, 1);

    // write key
    Write(FKey, 1);

    // write scale
    Write(FScale, 1);
  end;
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.ScaleChanged;
begin
  // scale changed
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.SetKey(const Value: Int8);
begin
 if FKey <> Value then
  begin
   FKey := Value;
   KeyChanged;
  end;
end;

procedure TDavMidiFileTrackMetaKeySignaturEvent.SetScale(const Value: Byte);
begin
 if FScale <> Value then
  begin
   FScale := Value;
   ScaleChanged;
  end;
end;


{ TDavMidiFileTrackMetaSequencerSpecificEvent }

constructor TDavMidiFileTrackMetaSequencerSpecificEvent.Create(
  DeltaTime: TDavMidiVariableLength);
begin
  inherited;

  FData := TMemoryStream.Create;
end;

function TDavMidiFileTrackMetaSequencerSpecificEvent.GetMetaDataSize: Cardinal;
begin
  Result := FData.Size;
end;

class function TDavMidiFileTrackMetaSequencerSpecificEvent.GetMetaEventTypeValue: Byte;
begin
  Result := $7F;
end;

procedure TDavMidiFileTrackMetaSequencerSpecificEvent.LoadFromStream(
  Stream: TStream);
var
  DataSize : Int32;
begin
  inherited;

  with Stream do
  begin
    // read data size as variable length
    with TDavMidiVariableLength.Create do
    try
      LoadFromStream(Stream);
      DataSize := AsUInt32;
    finally
      Free;
    end;

    // clear and copy data to internal stream
    FData.Clear;
    FData.CopyFrom(Stream, DataSize);
  end;
end;

procedure TDavMidiFileTrackMetaSequencerSpecificEvent.SaveToStream(
  Stream: TStream);
begin
  inherited;

  with Stream do
  begin
    // write data size as variable length
    with TDavMidiVariableLength.Create do
    try
      AsUInt32 := MetaDataSize;
      SaveToStream(Stream);
    finally
      Free;
    end;

    // write sequencer specific data
    FData.Seek(0, soFromBeginning);
    Stream.CopyFrom(FData, FData.Size);
  end;
end;


{ TDavMidiFileTrackNormalSysExEvent }

constructor TDavMidiFileTrackNormalSysExEvent.Create(DeltaTime: TDavMidiVariableLength);
begin
 inherited Create(DeltaTime);
end;

class function TDavMidiFileTrackNormalSysExEvent.GetEventTypeValue: Byte;
begin
  Result := $F7;
end;

procedure TDavMidiFileTrackNormalSysExEvent.LoadFromStream(Stream: TStream);
begin

end;

procedure TDavMidiFileTrackNormalSysExEvent.SaveToStream(Stream: TStream);
begin

end;

end.

