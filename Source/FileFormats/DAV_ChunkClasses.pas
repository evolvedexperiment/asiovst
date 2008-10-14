unit DAV_ChunkClasses;

interface

{$I ASIOVST.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Common;

type
  {$IFDEF DELPHI5}
  TCustomChunk = class(TPersistent)
  {$ELSE}
  TCustomChunk = class(TInterfacedPersistent, IStreamPersist)
  {$ENDIF}
  protected
    fChunkName : TChunkName;
    fChunkSize : Integer;
    function GetChunkName: string; virtual;
    function GetChunkSize: Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetChunkName(const Value: string); virtual;
  public
    constructor Create; virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(FileName : TFileName); virtual;
    procedure SaveToFile(FileName : TFileName); virtual;
    property ChunkName : string read GetChunkName write SetChunkName;
    property ChunkSize : Integer read GetChunkSize;
  end;

  TCustomChunkClass = class of TCustomChunk;

  TUnknownChunk = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
    procedure SetData(index: Integer; const Value: Byte);
  protected
    fData : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
  public
    property Data[index : Integer]: Byte read GetData write SetData;
  end;

  TDefinedChunk = class(TCustomChunk)
  protected
    fFilePosition : Cardinal;
    procedure SetChunkName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    class function GetClassChunkName : TChunkName; virtual; abstract;
  published
    property FilePosition : Cardinal read fFilePosition;
  end;

  TDefinedChunkClass = class of TDefinedChunk;

  TFixedDefinedChunk = class(TDefinedChunk)
  private
    function GetStartAddress: Pointer;
    procedure SetStartAddress(const Value: Pointer);
  protected
    fStartAddresses : array of Pointer;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Integer; override;
    property StartAddress: Pointer read GetStartAddress write SetStartAddress;
  public
    class function GetClassChunkSize : Integer; virtual; abstract;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TChunkList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TCustomChunk;
    procedure SetItem(Index: Integer; AChunk: TCustomChunk);
  public
    function Add(AChunk: TCustomChunk): Integer;
    function Extract(Item: TCustomChunk): TCustomChunk;
    function Remove(AChunk: TCustomChunk): Integer;
    function IndexOf(AChunk: TCustomChunk): Integer;
    procedure Insert(Index: Integer; AChunk: TCustomChunk);
    function First: TCustomChunk;
    function Last: TCustomChunk;
    property Items[Index: Integer]: TCustomChunk read GetItem write SetItem; default;
  end;

  TChunkContainer = class(TDefinedChunk)
  private
    function GetSubChunk(index: Integer): TCustomChunk;
    function GetCount: Integer;
  protected
    fChunkList        : TChunkList;
    fRegisteredChunks : array of TDefinedChunkClass;
    function GetChunkClass(ChunkName : TChunkName) : TCustomChunkClass;
    function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream: TStream); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddChunk(Chunk : TCustomChunk); virtual;
    procedure RegisterChunkClass(ChunkClass : TDefinedChunkClass);
    procedure RegisterChunkClasses; overload;
    procedure RegisterChunkClasses(ChunkClasses: array of TDefinedChunkClass); overload;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    property SubChunk[index : Integer] : TCustomChunk read GetSubChunk;
  published
    property Count : Integer read GetCount;
  end;

  TCustomBinaryChunk = class(TDefinedChunk)
  protected
    fBinaryData : Array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomTextChunk = class(TDefinedChunk)
  protected
    fText  : string;
    procedure SetText(const Value: string);
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

implementation

{ TCustomChunk }

constructor TCustomChunk.Create;
begin
 fChunkName := '';
 fChunkSize := 0;
end;

procedure TCustomChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunk then
  begin
   TCustomChunk(Dest).fChunkName := fChunkName;
   TCustomChunk(Dest).fChunkSize := fChunkSize;
  end
 else inherited;
end;

function TCustomChunk.GetChunkName: string;
begin
 result := fChunkName;
end;

function TCustomChunk.GetChunkSize: Integer;
begin
 result := fChunkSize;
end;

procedure TCustomChunk.LoadFromFile(FileName: TFileName);
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

procedure TCustomChunk.SaveToFile(FileName: TFileName);
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

procedure TCustomChunk.LoadFromStream(Stream: TStream);
begin
 Stream.Read(fChunkSize, 4);
end;

procedure TCustomChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   Write(fChunkName[0], 4);
   Write(fChunkSize, 4);
  end;
end;

procedure TCustomChunk.SetChunkName(const Value: string);
var
  ChunkNameSize : Integer;
begin
 ChunkNameSize := Length(Value);
 if ChunkNameSize > 3 then ChunkNameSize := 4;
 Move(Value[1], fChunkName[0], ChunkNameSize);
end;

{ TUnknownChunk }

procedure TUnknownChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TUnknownChunk then
  begin
   SetLength(TUnknownChunk(Dest).fData, Length(fData));
   Move(fData[0], TUnknownChunk(Dest).fData[0], Length(fData));
  end;
end;

function TUnknownChunk.GetData(index: Integer): Byte;
begin
 if (index >= 0) and (index < Length(fData))
  then result := fData[index]
  else result := 0;
end;

procedure TUnknownChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   Position := Position - 4;
   Read(fChunkName, 4);
   inherited;
   assert(fChunkSize < Size);
   SetLength(fData, fChunkSize);
   Read(fData[0], fChunkSize);
  end;
end;

procedure TUnknownChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   fChunkSize := Length(fData);
   inherited;
   Write(fData[0], fChunkSize);
  end;
end;

procedure TUnknownChunk.SetData(index: Integer; const Value: Byte);
begin
 if (index >= 0) and (index < Length(fData))
  then fData[index] := Value;
end;

{ TDefinedChunk }

constructor TDefinedChunk.Create;
begin
 inherited;
 fFilePosition := 0;
 fChunkName := GetClassChunkName;
end;

procedure TDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDefinedChunk
  then TDefinedChunk(Dest).fFilePosition := fFilePosition;
end;

procedure TDefinedChunk.LoadFromStream(Stream: TStream);
var
  TempChunkName : TChunkName;
begin
 with Stream do
  begin
   // Assume chunk name fits the defined one
   Position := Position - 4;
   Read(TempChunkName, 4);
   assert(TempChunkName = fChunkName);
   inherited;
  end;
end;

procedure TDefinedChunk.SetChunkName(const Value: string);
begin
 inherited;
 if Value <> fChunkName
  then raise Exception.Create('Chunk name must always be ''' + fChunkName + '''');
end;

{ TFixedDefinedChunk }

constructor TFixedDefinedChunk.Create;
begin
 inherited;
 SetLength(fStartAddresses, 1);
 fChunkSize := GetClassChunkSize;
end;

procedure TFixedDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TFixedDefinedChunk then
  begin
   SetLength(TFixedDefinedChunk(Dest).fStartAddresses, Length(fStartAddresses));
   Move(fStartAddresses[0], TFixedDefinedChunk(Dest).fStartAddresses[0], Length(fStartAddresses) * SizeOf(Pointer));
  end;
end;

function TFixedDefinedChunk.GetChunkSize: Integer;
begin
 result := GetClassChunkSize;
end;

function TFixedDefinedChunk.GetStartAddress: Pointer;
begin
 result := fStartAddresses[0];
end;

procedure TFixedDefinedChunk.SetStartAddress(const Value: Pointer);
begin
 fStartAddresses[0] := Value;
end;

procedure TFixedDefinedChunk.LoadFromStream(Stream: TStream);
var
  BytesReaded : Integer;
begin
 inherited;
 with Stream do
  if fChunkSize <= GetClassChunkSize
   then Read(fStartAddresses[0]^, fChunkSize)
   else
    begin
     BytesReaded := Read(fStartAddresses[0]^, GetClassChunkSize);
     assert(BytesReaded = GetClassChunkSize);
     Position := Position + fChunkSize - GetClassChunkSize;
    end;
end;

procedure TFixedDefinedChunk.SaveToStream(Stream: TStream);
var
  BytesWritten: Integer;
begin
 fChunkSize := GetClassChunkSize;
 inherited;
 try
  BytesWritten := Stream.Write(fStartAddresses[0]^, GetClassChunkSize);
  assert(BytesWritten = fChunkSize);
 except
  raise Exception.Create('Wrong Start Addess of Chunk: ' + ChunkName);
 end;
end;

{ TChunkList }

function TChunkList.Add(AChunk: TCustomChunk): Integer;
begin
 result := inherited Add(TObject(AChunk));
end;

function TChunkList.Extract(Item: TCustomChunk): TCustomChunk;
begin
 result := TCustomChunk(inherited Extract(TObject(Item)));
end;

function TChunkList.First: TCustomChunk;
begin
 result := TCustomChunk(inherited First);
end;

function TChunkList.GetItem(Index: Integer): TCustomChunk;
begin
 result := TCustomChunk(inherited GetItem(Index));
end;

function TChunkList.IndexOf(AChunk: TCustomChunk): Integer;
begin
 result := inherited IndexOf(TObject(AChunk));
end;

procedure TChunkList.Insert(Index: Integer; AChunk: TCustomChunk);
begin
 inherited Insert(Index, TObject(AChunk));
end;

function TChunkList.Last: TCustomChunk;
begin
 result := TCustomChunk(inherited Last);
end;

function TChunkList.Remove(AChunk: TCustomChunk): Integer;
begin
 result := inherited Remove(TObject(AChunk));
end;

procedure TChunkList.SetItem(Index: Integer; AChunk: TCustomChunk);
begin
 inherited SetItem(Index, TObject(AChunk));
end;

{ TChunkContainer }

procedure TChunkContainer.AssignTo(Dest: TPersistent);
{$IFDEF DELPHI5}
var
  i : Integer;
{$ENDIF}  
begin
 inherited;
 if Dest is TChunkContainer then
  begin
   SetLength(TChunkContainer(Dest).fRegisteredChunks, Length(fRegisteredChunks));
   Move(fRegisteredChunks, TChunkContainer(Dest).fRegisteredChunks, Length(fRegisteredChunks) * SizeOf(TCustomChunkClass));
   {$IFDEF DELPHI5}
   for i := 0 to TChunkContainer(Dest).fChunkList.Count - 1
    do TCustomChunk(TChunkContainer(Dest).fChunkList[i]).Assign(TCustomChunk(fChunkList[i]));
   {$ELSE}
   TChunkContainer(Dest).fChunkList.Assign(fChunkList);
   {$ENDIF}
  end;
end;

constructor TChunkContainer.Create;
begin
 inherited;
 fChunkList := TChunkList.Create;
end;

destructor TChunkContainer.Destroy;
begin
 FreeAndNil(fChunkList);
 inherited;
end;

procedure TChunkContainer.AddChunk(Chunk: TCustomChunk);
begin
 fChunkList.Add(Chunk);
end;

function TChunkContainer.GetChunkClass(ChunkName: TChunkName): TCustomChunkClass;
var
  i : Integer;
begin
 result := TUnknownChunk;
 for i := 0 to Length(fRegisteredChunks) - 1 do
  if fRegisteredChunks[i].GetClassChunkName = ChunkName then
   begin
    result := fRegisteredChunks[i];
    exit;
   end;
end;

function TChunkContainer.GetCount: Integer;
begin
 result := fChunkList.Count;
end;

function TChunkContainer.GetSubChunk(index: Integer): TCustomChunk;
begin
 if (index >= 0) and (index < fChunkList.Count)
  then result := fChunkList[index]
  else result := nil;
end;

procedure TChunkContainer.LoadFromStream(Stream: TStream);
var
  ChunkEnd  : Integer;
  ChunkName : TChunkName;
begin
 inherited;
 with Stream do
  begin
   ChunkEnd := Position + fChunkSize;
   while Position < ChunkEnd do
    begin
     Read(ChunkName, 4);
     ConvertStreamToChunk(GetChunkClass(ChunkName), Stream);
    end;
   if Position <> ChunkEnd
    then Position := ChunkEnd;
  end;
end;

procedure TChunkContainer.ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream : TStream);
var
  Chunk : TCustomChunk;
begin
 Chunk := ChunkClass.Create;
 Chunk.LoadFromStream(Stream);
 AddChunk(Chunk);
end;

procedure TChunkContainer.RegisterChunkClass(ChunkClass: TDefinedChunkClass);
var
  i : Integer;
begin
 // Check if the chunk class is already in the list
 for i := 0 to Length(fRegisteredChunks) - 1 do
  if fRegisteredChunks[i] = ChunkClass then exit;

 // If not, add chunk class to the list
 SetLength(fRegisteredChunks, Length(fRegisteredChunks) + 1);
 fRegisteredChunks[Length(fRegisteredChunks) - 1] := ChunkClass;
end;

procedure TChunkContainer.RegisterChunkClasses(ChunkClasses: array of TDefinedChunkClass);
var
  i : Integer;
begin
 for i := 0 to Length(ChunkClasses) - 1
  do RegisterChunkClass(ChunkClasses[i]);
end;

procedure TChunkContainer.RegisterChunkClasses;
var
  i : Integer;
begin
 for i := 0 to fChunkList.Count - 1
  do RegisterChunkClass(TDefinedChunkClass(fChunkList[i].ClassType));
end;

function TChunkContainer.GetChunkSize: Integer;
var
  i : Integer;
begin
 result := 0;
 for i := 0 to fChunkList.Count - 1
  do inc(result, fChunkList[i].ChunkSize + 8); // Chunk Size + Chunk Frame (8)
end;

procedure TChunkContainer.SaveToStream(Stream: TStream);
var
  i : Integer;
begin
 fChunkSize := GetChunkSize;
 inherited;
 for i := 0 to fChunkList.Count - 1
  do fChunkList[i].SaveToStream(Stream);
end;

{ TCustomBinaryChunk }

procedure TCustomBinaryChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBinaryChunk then
  begin
   SetLength(TCustomBinaryChunk(Dest).fBinaryData, Length(fBinaryData));
   Move(fBinaryData, TCustomBinaryChunk(Dest).fBinaryData, SizeOf(fBinaryData));
  end;
end;

procedure TCustomBinaryChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(fBinaryData, fChunkSize);
 Stream.Read(fBinaryData[0], Length(fBinaryData));
end;

procedure TCustomBinaryChunk.SaveToStream(Stream: TStream);
begin
 fChunkSize := Length(fBinaryData);
 inherited;
 Stream.Write(fBinaryData[0], fChunkSize);
end;

{ TCustomTextChunk }

procedure TCustomTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomTextChunk then
  begin
   TCustomTextChunk(Dest).fText  := fText;
  end;
end;

procedure TCustomTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(fText, fChunkSize);
 Stream.Read(fText[1], Length(fText));
end;

procedure TCustomTextChunk.SaveToStream(Stream: TStream);
begin
 fChunkSize := Length(fText);
 inherited;
 Stream.Write(fText[1], fChunkSize);
end;

procedure TCustomTextChunk.SetText(const Value: string);
begin
 if fText <> Value then
  begin
   fText := Value;
   fChunkSize := Length(fText);
  end;
end;

end.
