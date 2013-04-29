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

unit DAV_MidiFile;

interface

{$I ..\DAV_Compiler.Inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types, DAV_Common, DAV_ChunkClasses,
  DAV_FileFormat, DAV_MidiFileChunks;

type
  EDavMidiFileError = class(Exception);

  TDavMidiFile = class(TDavCustomFileFormatFile)
  private
    FHeader : TDavMidiFileHeaderChunk;
    function GetTrack(Index: UInt32): TDavCustomMidiFileTrackChunk;
    function GetTrackCount: UInt32;
    function GetMaxDeltaTimeAsUInt32: Cardinal;
  protected
    FTracks : TDavChunkList;
    function GetFileSize: Cardinal; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    class function DefaultExtension: TFileName; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    procedure AddTrack(Track : TDavCustomMidiFileTrackChunk); virtual;
    procedure DeleteTrack(Index: UInt32); virtual;
    procedure InsertTrack(Index: UInt32; Track: TDavCustomMidiFileTrackChunk);

    property Header: TDavMidiFileHeaderChunk read FHeader write FHeader;
    property Track[Index : UInt32]: TDavCustomMidiFileTrackChunk read GetTrack; default;
    property Count: UInt32 read GetTrackCount;

    property MaximumDeltaTimeAsUInt32: Cardinal read GetMaxDeltaTimeAsUInt32;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TDavMidiFile }

constructor TDavMidiFile.Create;
begin
  inherited;
  FHeader := TDavMidiFileHeaderChunk.Create;
  FTracks := TDavChunkList.Create;
end;

destructor TDavMidiFile.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FTracks);
  inherited;
end;

procedure TDavMidiFile.AddTrack(Track: TDavCustomMidiFileTrackChunk);
begin
  FTracks.Add(Track);
end;

procedure TDavMidiFile.InsertTrack(Index: UInt32; Track: TDavCustomMidiFileTrackChunk);
begin
  if Index <= Count then
    FTracks.Insert(Index, Track)
  else
    raise EDavMidiFileError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

class function TDavMidiFile.DefaultExtension: TFileName;
begin
  Result := '.mid';
end;

procedure TDavMidiFile.DeleteTrack(Index: UInt32);
begin
  if Index < Count then
    FTracks.Delete(Index)
  else
    raise EDavMidiFileError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TDavMidiFile.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TDavMidiFile then
  begin
    TDavMidiFile(Dest).FHeader.Assign(FHeader);
    TDavMidiFile(Dest).FTracks.Assign(FTracks);
  end;
end;

function TDavMidiFile.GetTrackCount: UInt32;
begin
  Result := FTracks.Count;
end;

function TDavMidiFile.GetFileSize: Cardinal;
var
  ChunkIndex : Int32;
begin
  Result := 0;
  for ChunkIndex := 0 to FTracks.Count - 1 do
    Inc(Result, FTracks[ChunkIndex].ChunkSize + 8); // Track Size + Track Frame (8)
end;

function TDavMidiFile.GetMaxDeltaTimeAsUInt32: Cardinal;
var
  TrackIndex: Integer;
begin
  Result := 0;
  for TrackIndex := 0 to Count - 1 do
    if FTracks[TrackIndex] is TDavMidiFileTrackEventsChunk then
      with TDavMidiFileTrackEventsChunk(FTracks[TrackIndex]) do
      begin
        if MaximumDeltaTimeAsUInt32 > Result then
          Result := MaximumDeltaTimeAsUInt32;
      end;
end;

function TDavMidiFile.GetTrack(Index: UInt32): TDavCustomMidiFileTrackChunk;
begin
  if (Integer(Index) < FTracks.Count) then
    Result := TDavCustomMidiFileTrackChunk(FTracks[Index])
  else
    Result := nil;
end;

procedure TDavMidiFile.LoadFromStream(Stream: TStream);
var
  ChunkName  : TChunkName;
  TrackChunk : TDavCustomMidiFileTrackChunk;
begin
  with Stream do
  begin
    Read(ChunkName, 4);
    Position := Position - 4;
    if ChunkName <> 'MThd' then
      raise EDavMidiFileError.Create('Header chunk not found!');

    FHeader.LoadFromStream(Stream);

    while Position < Stream.Size do
    begin
      Read(ChunkName, 4);
      Position := Position - 4;

      if ChunkName <> 'MTrk' then
        raise EDavMidiFileError.Create('Track chunk expected!');

//      TrackChunk := TDavMidiFileTrackMemoryChunk.Create;
      TrackChunk := TDavMidiFileTrackEventsChunk.Create;

      TrackChunk.LoadFromStream(Stream);
      FTracks.Add(TrackChunk)
    end;
  end;
end;

procedure TDavMidiFile.SaveToStream(Stream: TStream);
var
  ChunkIndex : Int32;
begin
  inherited;

  FHeader.SaveToStream(Stream);

  for ChunkIndex := 0 to FTracks.Count - 1 do
    FTracks[ChunkIndex].SaveToStream(Stream);
end;

end.
