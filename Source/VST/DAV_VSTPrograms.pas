unit DAV_VSTPrograms;

interface

{$I ..\ASIOVST.INC}

uses
  Classes, DAV_VSTBasicModule;

type
  TChunkEvent = procedure(Sender: TObject; const Index : Integer; const isPreset : Boolean) of object;

  TCustomVstProgram = class(TCollectionItem)
  private
    FDisplayName      : string;
    FVSTModule        : TBasicVSTModule;
    FOnInitialize     : TNotifyEvent;
    FOnStoreChunk     : TChunkEvent;
    FOnLoadChunk      : TChunkEvent;

    procedure SetParameter(AIndex: Integer; AValue: Single);
    function GetParameter(AIndex: Integer): Single;
  protected
    FParameter        : array of Single;
    FChunkData        : TMemoryStream;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
    function ParameterCount: integer;
    procedure SetParameterCount(cnt: integer);
    property Parameter[AIndex: Integer]: Single read GetParameter write SetParameter;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnLoadChunk: TChunkEvent read FOnLoadChunk write FOnLoadChunk;
    property OnStoreChunk: TChunkEvent read FOnStoreChunk write FOnStoreChunk;
  end;

  TVstProgram = class(TCustomVstProgram)
    property DisplayName;
    property VSTModule;
    property OnInitialize;
    property OnLoadChunk;
    property OnStoreChunk;
  end;

  TCustomVstPrograms = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TVstProgram;
    procedure SetItem(Index: Integer; const Value: TVstProgram);
    property Items[Index: Integer]: TVstProgram read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TVstProgram;
    function Insert(Index: Integer): TVstProgram;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TVSTPrograms = TCustomVstPrograms;

implementation

uses
  SysUtils, DAV_VSTEffect, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
constructor TCustomVstProgram.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstProgram.Create(Collection: TCollection);
{$ENDIF}
begin
 inherited;
 FDisplayName := 'Init';
 fVSTModule := (Collection As TCustomVstPrograms).VSTModule;
 VSTModule.Effect^.numPrograms := Collection.Count;
 with TVSTModuleWithPrograms(VSTModule) do
  begin
   if not (effFlagsProgramChunks in VSTModule.Effect^.EffectFlags)
    then SetLength(FParameter, numParams)
    else fChunkData := TMemoryStream.Create;
   if CurrentProgram < 0 then CurrentProgram := 0;
  end;
end;

destructor TCustomVstProgram.Destroy;
begin
 try
  SetLength(FParameter, 0);
  FreeAndNil(fChunkData);
 finally
  inherited;
 end;
end;

function TCustomVstProgram.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstProgram.SetDisplayName(const AValue: string);
begin
 FDisplayName := Copy(AValue, 0, 50);
end;

procedure TCustomVstProgram.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
 if Dest is TCustomVstProgram then
  with TCustomVstProgram(Dest) do
   begin
    if Length(Self.FParameter) > 0 then
     begin
      SetLength(FParameter, Length(Self.FParameter));
      for i := 0 to Length(Self.FParameter) - 1
       do Parameter[i] := Self.Parameter[i];
     end;

    if Self.FChunkData.Size > 0 then
     begin
      FChunkData.Size := Self.FChunkData.Size;
      Move(Self.FChunkData.Memory^, FChunkData.Memory^, FChunkData.Size);
      FChunkData.Position := Self.FChunkData.Position;
     end;

    OnInitialize := Self.OnInitialize;
    OnStoreChunk := Self.OnStoreChunk;
    OnLoadChunk  := Self.OnLoadChunk;
    DisplayName  := Self.DisplayName;
   end
  else inherited;
end;

procedure TCustomVstProgram.SetParameter(AIndex: Integer; AValue: Single);
begin
 assert(fVSTModule is TVSTModuleWithPrograms);
 with TVSTModuleWithPrograms(fVSTModule) do
  begin
   if effFlagsProgramChunks in Flags then exit;
   if (AIndex >= 0) and (AIndex < numParams)
    then FParameter[AIndex] := AValue
   // else raise exception.Create('Index out of bounds');
  end;
end;

function TCustomVstProgram.GetParameter(AIndex: Integer): Single;
begin
 assert(fVSTModule is TVSTModuleWithPrograms);
 if (AIndex >= 0) and (AIndex < TVSTModuleWithPrograms(fVSTModule).numParams)
  then Result := FParameter[AIndex] else
   begin
    Result := 0;
    // raise exception.Create('Index out of bounds');
   end;
end;

procedure TCustomVstProgram.SetParameterCount(cnt: Integer);
begin
 SetLength(fParameter, cnt);
end;

function TCustomVstProgram.ParameterCount: Integer;
begin
  result := Length(FParameter);
end;


{ TCustomVstPrograms }

constructor TCustomVstPrograms.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TVstProgram);
 FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstPrograms.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomVstPrograms.Add: TVstProgram;
begin
  Result := TVstProgram(inherited Add);
end;

function TCustomVstPrograms.GetItem(Index: Integer): TVstProgram;
begin
 Result := TVstProgram(inherited GetItem(Index));
end;

function TCustomVstPrograms.Insert(Index: Integer): TVstProgram;
begin
 Result := TVstProgram(inherited Insert(Index));
end;

procedure TCustomVstPrograms.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TCustomVstPrograms.SetItem(Index: Integer; const Value: TVstProgram);
begin
 inherited SetItem(Index, Value);
end;

end.
