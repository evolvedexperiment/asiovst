unit DVSTPrograms;

interface

{$I ASIOVST.INC}

uses
  Classes, DVSTBasicModule;

type
  TChunkEvent = procedure(Sender: TObject; const Index : Integer; const isPreset : Boolean) of object;

  TCustomVstProgram = class(TCollectionItem)
  private
    FDisplayName      : string;
    FVSTModule        : TBasicVSTModule;
    FOnInitialize     : TNotifyEvent;
    FOnStoreChunk     : TChunkEvent;
    FOnLoadChunk      : TChunkEvent;

    procedure SetParameter(AIndex: Integer; s: Single);
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
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnLoadChunk: TChunkEvent read FOnLoadChunk write FOnLoadChunk;
    property OnStoreChunk: TChunkEvent read FOnStoreChunk write FOnStoreChunk;
  end;

  TCustomVstPrograms = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstProgram;
    procedure SetItem(Index: Integer; const Value: TCustomVstProgram);
    property Items[Index: Integer]: TCustomVstProgram read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstProgram;
    function Insert(Index: Integer): TCustomVstProgram;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;
  
  TVstProgram = TCustomVstProgram;
  TVstPrograms = TCustomVstPrograms;

implementation

uses
  SysUtils, DVSTEffect, DVSTModuleWithPrograms;

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
var i: Integer;
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
    DisplayName := Self.DisplayName;
   end
  else inherited;
end;

procedure TCustomVstProgram.SetParameter(AIndex: Integer; s: Single);
begin
 with TVSTModuleWithPrograms(fVSTModule) do
  begin
   if effFlagsProgramChunks in Flags then exit;
   if (AIndex >= 0) and (AIndex < numParams)
    then FParameter[AIndex] := s
   // else raise exception.Create('Index out of bounds');
  end;
end;

function TCustomVstProgram.GetParameter(AIndex: Integer): Single;
begin
 if (AIndex>=0) and (AIndex < TVSTModuleWithPrograms(fVSTModule).numParams)
  then Result := FParameter[AIndex] else
   begin
    Result := 0;
    // raise exception.Create('Index out of bounds');
   end;
end;

procedure TCustomVstProgram.SetParameterCount(cnt: integer);
begin
 SetLength(fParameter, cnt);
end;

function TCustomVstProgram.ParameterCount: integer;
begin
  result := Length(FParameter);
end;


{ TVstPrograms }

constructor TCustomVstPrograms.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstProgram);
 FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstPrograms.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomVstPrograms.Add: TCustomVstProgram;
begin
  Result := TCustomVstProgram(inherited Add);
end;

function TCustomVstPrograms.GetItem(Index: Integer): TCustomVstProgram;
begin
 Result := TCustomVstProgram(inherited GetItem(Index));
end;

function TCustomVstPrograms.Insert(Index: Integer): TCustomVstProgram;
begin
 Result := TCustomVstProgram(inherited Insert(Index));
end;

procedure TCustomVstPrograms.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TCustomVstPrograms.SetItem(Index: Integer; const Value: TCustomVstProgram);
begin
 inherited SetItem(Index, Value);
end;

end.
