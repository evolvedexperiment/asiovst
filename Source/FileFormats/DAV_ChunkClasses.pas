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

unit DAV_ChunkClasses;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types;

type
  TDavChunkFlag = (cfSizeFirst, cfReversedByteOrder, cfPadSize,
    cfIncludeChunkInSize);
  TDavChunkFlags = set of TDavChunkFlag;
  {$IFDEF DELPHI5}
  TDavCustomChunk = class(TPersistent)
  {$ELSE}
  TDavCustomChunk = class(TInterfacedPersistent, IStreamPersist)
  {$ENDIF}
  protected
    FChunkName  : TChunkName;
    FChunkSize  : Cardinal;
    FChunkFlags : TDavChunkFlags;
    function GetChunkName: AnsiString; virtual;
    function GetChunkSize: Cardinal; virtual;
    function CalculateZeroPad: Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetChunkName(const Value: AnsiString); virtual;
    procedure CheckAddZeroPad(Stream: TStream);
  public
    constructor Create; virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(FileName : TFileName); virtual;
    procedure SaveToFile(FileName : TFileName); virtual;
    property ChunkName: AnsiString read GetChunkName write SetChunkName;
    property ChunkSize: Cardinal read GetChunkSize;
    property ChunkFlags: TDavChunkFlags read FChunkFlags write FChunkFlags default [];
  end;

  TDavCustomChunkClass = class of TDavCustomChunk;

  TDavDummyChunk = class(TDavCustomChunk)
  public
    procedure LoadFromStream(Stream : TStream); override;
  end;

  TDavUnknownChunk = class(TDavCustomChunk)
  private
    function GetData(Index: Integer): Byte;
    procedure SetData(Index: Integer; const Value: Byte);
  protected
    FDataStream : TMemoryStream;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;

    property Data[Index : Integer]: Byte read GetData write SetData;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TDavDefinedChunk = class(TDavCustomChunk)
  protected
    FFilePosition : Cardinal;
    procedure SetChunkName(const Value: AnsiString); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    class function GetClassChunkName : TChunkName; virtual; abstract;
  published
    property FilePosition : Cardinal read FFilePosition;
  end;

  TDavDefinedChunkClass = class of TDavDefinedChunk;

  TDavFixedDefinedChunk = class(TDavDefinedChunk)
  private
    function GetStartAddress: Pointer;
    procedure SetStartAddress(const Value: Pointer);
  protected
    FStartAddresses : array of Pointer;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
    property StartAddress: Pointer read GetStartAddress write SetStartAddress;
  public
    class function GetClassChunkSize : Integer; virtual; abstract;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavChunkList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TDavCustomChunk;
    procedure SetItem(Index: Integer; AChunk: TDavCustomChunk);
  public
    function Add(AChunk: TDavCustomChunk): Integer;
    function Extract(Item: TDavCustomChunk): TDavCustomChunk;
    function Remove(AChunk: TDavCustomChunk): Integer;
    function IndexOf(AChunk: TDavCustomChunk): Integer;
    procedure Insert(Index: Integer; AChunk: TDavCustomChunk);
    function First: TDavCustomChunk;
    function Last: TDavCustomChunk;
    property Items[Index: Integer]: TDavCustomChunk read GetItem write SetItem; default;
  end;

  TDavCustomChunkContainer = class(TDavDefinedChunk)
  private
    function GetSubChunk(Index: Integer): TDavCustomChunk;
    function GetCount: Integer;
  protected
    FChunkList : TDavChunkList;
    function GetChunkClass(ChunkName : TChunkName): TDavCustomChunkClass; virtual; abstract;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ConvertStreamToChunk(ChunkClass: TDavCustomChunkClass; Stream: TStream); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddChunk(Chunk : TDavCustomChunk); virtual;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    property SubChunk[Index : Integer]: TDavCustomChunk read GetSubChunk;
    property Count : Integer read GetCount;
  end;

  TDavChunkContainer = class(TDavCustomChunkContainer)
  protected
    FRegisteredChunks : array of TDavDefinedChunkClass;
    function GetChunkClass(ChunkName : TChunkName): TDavCustomChunkClass; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure RegisterChunkClass(ChunkClass : TDavDefinedChunkClass);
    procedure RegisterChunkClasses; overload;
    procedure RegisterChunkClasses(ChunkClasses: array of TDavDefinedChunkClass); overload;
  published
    property Count;
  end;

  TDavUnknownChunkContainer = class(TDavUnknownChunk)
  private
    function GetSubChunk(Index: Integer): TDavCustomChunk;
    function GetCount: Integer;
    function ConvertStreamToChunk(ChunkClass: TDavCustomChunkClass; Stream: TStream): TDavCustomChunk; virtual;
  protected
    FChunkList : TDavChunkList;
    function CheckForSubchunks: Boolean; virtual;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    property SubChunk[Index : Integer] : TDavCustomChunk read GetSubChunk;
  published
    property Count : Integer read GetCount;
  end;

  TDavPNGChunkContainer = class(TDavUnknownChunkContainer)
  protected
    function CheckForSubchunks: Boolean; override;
  public
    procedure LoadFromStream(Stream : TStream); override;
  end;

  TDavCustomBinaryChunk = class(TDavDefinedChunk)
  protected
    FBinaryData : Array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavCustomTextChunk = class(TDavDefinedChunk)
  protected
    FText : AnsiString;
    procedure SetText(const Value: AnsiString);
    procedure AssignTo(Dest: TPersistent); override;
    property Text: AnsiString read FText write SetText;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavCustomStreamChunk = class(TDavDefinedChunk)
  protected
    FStream : TStream;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TDavCustomMemoryStreamChunk = class(TDavCustomStreamChunk)
  private
    function GetMemoryStream: TMemoryStream;
  public
    constructor Create; override;
    property MemoryStream: TMemoryStream read GetMemoryStream;
  end;

const
  CZeroPad: Integer = 0;

function CompareChunkNames(ChunkNameA, ChunkNameB: TChunkName): Boolean;

implementation

uses
  DAV_Common;

function CompareChunkNames(ChunkNameA, ChunkNameB: TChunkName): Boolean;
begin
 Result := False;
 if ChunkNameA[0] <> ChunkNameB[0] then Exit;
 if ChunkNameA[1] <> ChunkNameB[1] then Exit;
 if ChunkNameA[2] <> ChunkNameB[2] then Exit;
 if ChunkNameA[3] <> ChunkNameB[3] then Exit;
 Result := True;
end;


{ TDavCustomChunk }

function TDavCustomChunk.CalculateZeroPad: Integer;
begin
 Result := (2 - (FChunkSize and 1)) and 1;
end;

procedure TDavCustomChunk.CheckAddZeroPad(Stream: TStream);
begin
 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;

constructor TDavCustomChunk.Create;
begin
 FChunkName := '';
 FChunkSize := 0;
end;

procedure TDavCustomChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TDavCustomChunk then
  begin
   TDavCustomChunk(Dest).FChunkName := FChunkName;
   TDavCustomChunk(Dest).FChunkSize := FChunkSize;
  end
 else inherited;
end;

function TDavCustomChunk.GetChunkName: AnsiString;
begin
 Result := AnsiString(FChunkName);
end;

function TDavCustomChunk.GetChunkSize: Cardinal;
begin
 Result := FChunkSize;
end;

procedure TDavCustomChunk.LoadFromFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   Free;
  end;
end;

procedure TDavCustomChunk.SaveToFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   Free;
  end;
end;

procedure TDavCustomChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   Assert(Position <= Size + 8);
   if cfSizeFirst in ChunkFlags then
    begin
     // order known from PNG
     Read(FChunkSize, 4);
     Read(FChunkName, 4);
    end
   else
    begin
     // order known from WAVE, AIFF, etc.
     Read(FChunkName, 4);
     Read(FChunkSize, 4);
    end;
  end;

 // eventually flip bytes
 if cfReversedByteOrder in ChunkFlags
  then Flip32(FChunkSize);
end;

procedure TDavCustomChunk.SaveToStream(Stream: TStream);
var
  TempSize : Cardinal;
begin
 TempSize := FChunkSize;

 // eventually flip bytes
 if cfReversedByteOrder in ChunkFlags
  then Flip32(TempSize);

 with Stream do
  if cfSizeFirst in ChunkFlags then
   begin
    // order known from PNG
    Write(TempSize, 4);
    Write(FChunkName[0], 4);
   end
  else
   begin
    // order known from WAVE, AIFF, etc.
    Write(FChunkName[0], 4);
    Write(TempSize, 4);
   end;
end;

procedure TDavCustomChunk.SetChunkName(const Value: AnsiString);
var
  ChunkNameSize : Integer;
begin
 ChunkNameSize := Length(Value);
 if ChunkNameSize > 3 then ChunkNameSize := 4;
 Move(Value[1], FChunkName[0], ChunkNameSize);
end;


{ TDavDummyChunk }

procedure TDavDummyChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   inherited;
   Position := Position + FChunkSize;
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;


{ TDavUnknownChunk }

function TDavUnknownChunk.CalculateChecksum: Integer;
var
  b : Byte;
begin
 with FDataStream do
  begin
   Position := 0;
   Result := 0;
   while Position < Size do
    begin
     Read(b, 1);
     Result := Result + b;
    end;
  end;
end;

constructor TDavUnknownChunk.Create;
begin
 inherited;
 FDataStream := TMemoryStream.Create;
end;

destructor TDavUnknownChunk.Destroy;
begin
 FreeAndNil(FDataStream);
 inherited;
end;

procedure TDavUnknownChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavUnknownChunk then
  begin
   TDavUnknownChunk(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
  end;
end;

function TDavUnknownChunk.GetData(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Read(Result, 1);
    end
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TDavUnknownChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   inherited;
   Assert(FChunkSize <= Size);
   Assert(FChunkName <> #0#0#0#0);
   FDataStream.Clear;
   FDataStream.Size := FChunkSize;
   FDataStream.Position := 0;
   if FChunkSize > 0 then
    if cfIncludeChunkInSize in ChunkFlags
     then FDataStream.CopyFrom(Stream, FChunkSize - 8)
     else FDataStream.CopyFrom(Stream, FChunkSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TDavUnknownChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   FChunkSize := FDataStream.Size; //Length(FData);
   inherited;
   FDataStream.Position := 0;
   CopyFrom(FDataStream, FDataStream.Position);

   // check and eventually add zero pad
   CheckAddZeroPad(Stream);
  end;
end;

procedure TDavUnknownChunk.SetData(Index: Integer; const Value: Byte);
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Write(Value, 1);
    end
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;


{ TDavDefinedChunk }

constructor TDavDefinedChunk.Create;
begin
 inherited;
 FFilePosition := 0;
 FChunkName := GetClassChunkName;
end;

procedure TDavDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavDefinedChunk
  then TDavDefinedChunk(Dest).FFilePosition := FFilePosition;
end;

procedure TDavDefinedChunk.LoadFromStream(Stream: TStream);
var
  TempChunkName : TChunkName;
begin
 with Stream do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // Assume chunk name fits the defined one
     Position := Position + 4;
     Read(TempChunkName, 4);
     Assert(TempChunkName = FChunkName);
     Position := Position - 8;
    end
   else
    begin
     // Assume chunk name fits the defined one
     Read(TempChunkName, 4);
     Assert(TempChunkName = FChunkName);
     Position := Position - 4;
    end;
   inherited;
  end;
end;

procedure TDavDefinedChunk.SetChunkName(const Value: AnsiString);
begin
 inherited;
 if Value <> FChunkName
  then raise Exception.Create('Chunk name must always be ''' +
    string(AnsiString(FChunkName)) + '''');
end;


{ TDavFixedDefinedChunk }

constructor TDavFixedDefinedChunk.Create;
begin
 inherited;
 SetLength(FStartAddresses, 1);
 FChunkSize := GetClassChunkSize;
end;

procedure TDavFixedDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavFixedDefinedChunk then
  begin
   SetLength(TDavFixedDefinedChunk(Dest).FStartAddresses, Length(FStartAddresses));
   Move(FStartAddresses[0], TDavFixedDefinedChunk(Dest).FStartAddresses[0], Length(FStartAddresses) * SizeOf(Pointer));
  end;
end;

function TDavFixedDefinedChunk.GetChunkSize: Cardinal;
begin
 Result := GetClassChunkSize;
end;

function TDavFixedDefinedChunk.GetStartAddress: Pointer;
begin
 Result := FStartAddresses[0];
end;

procedure TDavFixedDefinedChunk.SetStartAddress(const Value: Pointer);
begin
 FStartAddresses[0] := Value;
end;

procedure TDavFixedDefinedChunk.LoadFromStream(Stream: TStream);
var
  BytesReaded : Integer;
begin
 inherited;
 with Stream do
  begin
   if FChunkSize <= Cardinal(GetClassChunkSize)
    then Read(FStartAddresses[0]^, FChunkSize)
    else
     begin
      BytesReaded := Read(FStartAddresses[0]^, GetClassChunkSize);
      Assert(BytesReaded = GetClassChunkSize);
      Position := Position + FChunkSize - GetClassChunkSize;
     end;
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TDavFixedDefinedChunk.SaveToStream(Stream: TStream);
var
  BytesWritten: Cardinal;
begin
 FChunkSize := GetClassChunkSize;
 inherited;
 try
  BytesWritten := Stream.Write(FStartAddresses[0]^, GetClassChunkSize);
  Assert(BytesWritten = FChunkSize);

  // check and eventually add zero pad
  CheckAddZeroPad(Stream);
 except
  raise Exception.Create('Wrong Start Addess of Chunk: ' + string(ChunkName));
 end;
end;


{ TDavChunkList }

function TDavChunkList.Add(AChunk: TDavCustomChunk): Integer;
begin
 Result := inherited Add(TObject(AChunk));
end;

function TDavChunkList.Extract(Item: TDavCustomChunk): TDavCustomChunk;
begin
 Result := TDavCustomChunk(inherited Extract(TObject(Item)));
end;

function TDavChunkList.First: TDavCustomChunk;
begin
 Result := TDavCustomChunk(inherited First);
end;

function TDavChunkList.GetItem(Index: Integer): TDavCustomChunk;
begin
 Result := TDavCustomChunk(inherited GetItem(Index));
end;

function TDavChunkList.IndexOf(AChunk: TDavCustomChunk): Integer;
begin
 Result := inherited IndexOf(TObject(AChunk));
end;

procedure TDavChunkList.Insert(Index: Integer; AChunk: TDavCustomChunk);
begin
 inherited Insert(Index, TObject(AChunk));
end;

function TDavChunkList.Last: TDavCustomChunk;
begin
 Result := TDavCustomChunk(inherited Last);
end;

function TDavChunkList.Remove(AChunk: TDavCustomChunk): Integer;
begin
 Result := inherited Remove(TObject(AChunk));
end;

procedure TDavChunkList.SetItem(Index: Integer; AChunk: TDavCustomChunk);
begin
 inherited SetItem(Index, TObject(AChunk));
end;


{ TDavCustomChunkContainer }

constructor TDavCustomChunkContainer.Create;
begin
 inherited;
 FChunkList := TDavChunkList.Create;
end;

destructor TDavCustomChunkContainer.Destroy;
begin
 FreeAndNil(FChunkList);
 inherited;
end;

procedure TDavCustomChunkContainer.AssignTo(Dest: TPersistent);
{$IFDEF DELPHI5}
var
  i : Integer;
{$ENDIF}
begin
 inherited;
 if Dest is TDavCustomChunkContainer then
  begin
   {$IFDEF DELPHI5}
   for i := 0 to TDavCustomChunkContainer(Dest).FChunkList.Count - 1
    do TDavCustomChunk(TDavCustomChunkContainer(Dest).FChunkList[i]).Assign(TDavCustomChunk(FChunkList[i]));
   {$ELSE}
   TDavCustomChunkContainer(Dest).FChunkList.Assign(FChunkList);
   {$ENDIF}
  end;
end;

procedure TDavCustomChunkContainer.AddChunk(Chunk: TDavCustomChunk);
begin
 FChunkList.Add(Chunk);
end;

function TDavCustomChunkContainer.GetCount: Integer;
begin
 Result := FChunkList.Count;
end;

function TDavCustomChunkContainer.GetSubChunk(Index: Integer): TDavCustomChunk;
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then Result := FChunkList[Index]
  else Result := nil;
end;

procedure TDavCustomChunkContainer.LoadFromStream(Stream: TStream);
var
  ChunkEnd  : Integer;
  ChunkName : TChunkName;
begin
 inherited;
 with Stream do
  begin
   ChunkEnd := Position + FChunkSize;
   Assert(ChunkEnd <= Stream.Size);
   while Position < ChunkEnd do
    begin
     if cfSizeFirst in ChunkFlags then
      begin
       Position := Position + 4;
       Read(ChunkName, 4);
       Position := Position - 8;
      end
     else
      begin
       Read(ChunkName, 4);
       Position := Position - 4;
      end;
     ConvertStreamToChunk(GetChunkClass(ChunkName), Stream);
    end;
   if Position <> ChunkEnd
    then Position := ChunkEnd;

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TDavCustomChunkContainer.ConvertStreamToChunk(ChunkClass: TDavCustomChunkClass; Stream : TStream);
var
  Chunk : TDavCustomChunk;
begin
 Chunk := ChunkClass.Create;
 Chunk.ChunkFlags := ChunkFlags;
 Chunk.LoadFromStream(Stream);
 AddChunk(Chunk);
end;

function TDavCustomChunkContainer.GetChunkSize: Cardinal;
var
  i : Integer;
begin
 Result := 0;
 for i := 0 to FChunkList.Count - 1
  do Inc(Result, FChunkList[i].ChunkSize + 8); // Chunk Size + Chunk Frame (8)
end;

procedure TDavCustomChunkContainer.SaveToStream(Stream: TStream);
var
  i : Integer;
begin
 FChunkSize := GetChunkSize;
 inherited;
 for i := 0 to FChunkList.Count - 1
  do FChunkList[i].SaveToStream(Stream);

 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TDavChunkContainer }

procedure TDavChunkContainer.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavChunkContainer then
  begin
   SetLength(TDavChunkContainer(Dest).FRegisteredChunks, Length(FRegisteredChunks));
   Move(FRegisteredChunks, TDavChunkContainer(Dest).FRegisteredChunks, Length(FRegisteredChunks) * SizeOf(TDavCustomChunkClass));
  end;
end;

function TDavChunkContainer.GetChunkClass(ChunkName: TChunkName): TDavCustomChunkClass;
var
  Index : Integer;
begin
 Result := TDavUnknownChunk;
 for Index := 0 to Length(FRegisteredChunks) - 1 do
  if CompareChunkNames(FRegisteredChunks[Index].GetClassChunkName, ChunkName) then
   begin
    Result := FRegisteredChunks[Index];
    Exit;
   end;
end;

procedure TDavChunkContainer.RegisterChunkClass(ChunkClass: TDavDefinedChunkClass);
var
  i : Integer;
begin
 // Check if the chunk class is already in the list
 for i := 0 to Length(FRegisteredChunks) - 1 do
  if FRegisteredChunks[i] = ChunkClass then Exit;

 // If not, add chunk class to the list
 SetLength(FRegisteredChunks, Length(FRegisteredChunks) + 1);
 FRegisteredChunks[Length(FRegisteredChunks) - 1] := ChunkClass;
end;

procedure TDavChunkContainer.RegisterChunkClasses(ChunkClasses: array of TDavDefinedChunkClass);
var
  i : Integer;
begin
 for i := 0 to Length(ChunkClasses) - 1
  do RegisterChunkClass(ChunkClasses[i]);
end;

procedure TDavChunkContainer.RegisterChunkClasses;
var
  i : Integer;
begin
 for i := 0 to FChunkList.Count - 1
  do RegisterChunkClass(TDavDefinedChunkClass(FChunkList[i].ClassType));
end;


{ TDavUnknownChunkContainer }

constructor TDavUnknownChunkContainer.Create;
begin
 inherited;
 FChunkList := TDavChunkList.Create;
end;

destructor TDavUnknownChunkContainer.Destroy;
begin
 FreeAndNil(FChunkList);
 inherited;
end;

function TDavUnknownChunkContainer.ConvertStreamToChunk(ChunkClass: TDavCustomChunkClass; Stream : TStream): TDavCustomChunk;
begin
 Result := ChunkClass.Create;
 Result.ChunkFlags := ChunkFlags;
 Result.LoadFromStream(Stream);
 FChunkList.Add(Result);
end;

function TDavUnknownChunkContainer.CheckForSubchunks: Boolean;
var
  TempSize : Cardinal;
  TempName : TChunkName;
begin
 Result := False;
 if (ChunkName = 'RIFF') or (ChunkName = 'FORM') or (ChunkName = 'MTrk')
  then FDataStream.Position := 4
  else FDataStream.Position := 0;
 while FDataStream.Position + 8 < FChunkSize do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // read chunk size
     FDataStream.Read(TempSize, 4);

     // read chunk name
     FDataStream.Read(TempName, 4);
    end
   else
    begin
     // read chunk name
     FDataStream.Read(TempName, 4);

     // read chunk size
     FDataStream.Read(TempSize, 4);
    end;

   // eventually reverse byte order
   if cfReversedByteOrder in ChunkFlags
    then Flip32(TempSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then TempSize := TempSize + (2 - (TempSize and 1)) and 1;

   if (FDataStream.Position + TempSize) <= FChunkSize
    then
     begin
      FDataStream.Position := FDataStream.Position + TempSize;
      Result := FDataStream.Position = FChunkSize;
      if Result then break;
     end
    else Exit;
  end;
end;

procedure TDavUnknownChunkContainer.LoadFromStream(Stream: TStream);
begin
 inherited;

 if CheckForSubchunks then
  begin
   if (ChunkName = 'RIFF') or (ChunkName = 'FORM')
    then FDataStream.Position := 4
    else FDataStream.Position := 0;
   while FDataStream.Position + 8 < FChunkSize
    do ConvertStreamToChunk(TDavUnknownChunkContainer, FDataStream);
  end;
end;

procedure TDavUnknownChunkContainer.SaveToStream(Stream: TStream);
var
  i : Integer;
begin
 FChunkSize := GetChunkSize;
 inherited;
 for i := 0 to FChunkList.Count - 1
  do FChunkList[i].SaveToStream(Stream);

 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;

function TDavUnknownChunkContainer.GetChunkSize: Cardinal;
var
  i : Integer;
begin
 Result := 0;
 for i := 0 to FChunkList.Count - 1
  do Inc(Result, FChunkList[i].ChunkSize + 8); // Chunk Size + Chunk Frame (8)
end;

function TDavUnknownChunkContainer.GetCount: Integer;
begin
 Result := FChunkList.Count;
end;

function TDavUnknownChunkContainer.GetSubChunk(Index: Integer): TDavCustomChunk;
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then Result := FChunkList[Index]
  else Result := nil;
end;


{ TDavPNGChunkContainer }

function TDavPNGChunkContainer.CheckForSubchunks: Boolean;
var
  TempSize : Cardinal;
  TempName : TChunkName;
begin
 Result := False;
 FDataStream.Position := 0;
 while FDataStream.Position + 8 < FChunkSize do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // read chunk size
     FDataStream.Read(TempSize, 4);

     // read chunk name
     FDataStream.Read(TempName, 4);
    end
   else
    begin
     // read chunk name
     FDataStream.Read(TempName, 4);

     // read chunk size
     FDataStream.Read(TempSize, 4);
    end;

   // eventually reverse byte order
   if cfReversedByteOrder in ChunkFlags
    then Flip32(TempSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then TempSize := TempSize + (2 - (TempSize and 1)) and 1;

   // checksum 
   TempSize := TempSize + 4;

   if (FDataStream.Position + TempSize) <= FChunkSize
    then
     begin
      FDataStream.Position := FDataStream.Position + TempSize;
      Result := FDataStream.Position = FChunkSize;
      if Result then break;
     end
    else Exit;
  end;
end;

procedure TDavPNGChunkContainer.LoadFromStream(Stream: TStream);
var
  PngMagic : TChunkName;
  CheckSum : Integer;
  SubChunk : TDavUnknownChunkContainer;
begin
 with Stream do
  begin
   Read(FChunkName, 4);
   Read(PngMagic, 4);
   if PngMagic <> #$0D#$0A#$1A#$0A
    then Exception.Create('Not a valid PNG file');
   FChunkSize := Stream.Size - 8;

   FDataStream.Clear;
   FDataStream.Size := FChunkSize;
   FDataStream.Position := 0;
   FDataStream.CopyFrom(Stream, FChunkSize);
  end;

 if CheckForSubchunks then
  begin
   FDataStream.Position := 0;
   while FDataStream.Position + 8 < FChunkSize do
    begin
     SubChunk := TDavUnknownChunkContainer(ConvertStreamToChunk(TDavUnknownChunkContainer, FDataStream));

     // read checksum
     FDataStream.Read(CheckSum, 4);
     if Checksum <> SubChunk.CalculateChecksum
      then raise Exception.Create('Checksum Error');
    end;
  end;
end;


{ TDavCustomBinaryChunk }

procedure TDavCustomBinaryChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavCustomBinaryChunk then
  begin
   SetLength(TDavCustomBinaryChunk(Dest).FBinaryData, Length(FBinaryData));
   Move(FBinaryData, TDavCustomBinaryChunk(Dest).FBinaryData, SizeOf(FBinaryData));
  end;
end;

procedure TDavCustomBinaryChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(FBinaryData, FChunkSize);
 Stream.Read(FBinaryData[0], Length(FBinaryData));
end;

procedure TDavCustomBinaryChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FBinaryData);
 inherited;
 Stream.Write(FBinaryData[0], FChunkSize);
end;


{ TDavCustomTextChunk }

procedure TDavCustomTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavCustomTextChunk then
  begin
   TDavCustomTextChunk(Dest).FText  := FText;
  end;
end;

procedure TDavCustomTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(FText, FChunkSize);
 Stream.Read(FText[1], Length(FText));

 // eventually skip padded zeroes
 if cfPadSize in ChunkFlags
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TDavCustomTextChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FText);

 inherited;
 Stream.Write(FText[1], FChunkSize);

 // eventually skip padded zeroes
 if (cfPadSize in ChunkFlags)
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TDavCustomTextChunk.SetText(const Value: AnsiString);
begin
 if FText <> Value then
  begin
   FText := Value;
   FChunkSize := Length(FText);
  end;
end;


{ TDavCustomStreamChunk }

procedure TDavCustomStreamChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDavCustomStreamChunk then
  begin
   FStream.Position := 0;
   TDavCustomStreamChunk(Dest).FStream.Position := 0;
   TDavCustomStreamChunk(Dest).FStream.CopyFrom(FStream, FStream.Size); 
  end;
end;

destructor TDavCustomStreamChunk.Destroy;
begin
 FreeAndNil(FStream);
 inherited;
end;

function TDavCustomStreamChunk.GetChunkSize: Cardinal;
begin
 FChunkSize := FStream.Size;
 Result := inherited GetChunkSize;
end;

procedure TDavCustomStreamChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 FStream.Position := 0;
 FStream.CopyFrom(Stream, FChunkSize);
 FStream.Position := 0;

 // eventually skip padded zeroes
 if cfPadSize in ChunkFlags
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TDavCustomStreamChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := FStream.Size;
 inherited;
 FStream.Position := 0;
 Stream.CopyFrom(FStream, FStream.Size);

 // eventually skip padded zeroes
 if (cfPadSize in ChunkFlags)
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;


{ TDavCustomMemoryStreamChunk }

constructor TDavCustomMemoryStreamChunk.Create;
begin
 inherited;
 FStream := TMemoryStream.Create;
end;

function TDavCustomMemoryStreamChunk.GetMemoryStream: TMemoryStream;
begin
 Result := TMemoryStream(FStream);
end;

end.
