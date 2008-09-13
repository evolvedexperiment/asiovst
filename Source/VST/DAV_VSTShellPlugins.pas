unit DAV_VSTShellPlugins;

interface

{$I ASIOVST.INC}

uses
  Classes, DAV_VSTEffect, DAV_VSTBasicModule;

type
  TUIDInstantiateEvent = procedure(Sender: TObject; UID: string) of object;

  TCustomVstShellPlugin = class(TCollectionItem)
  private
    FDisplayName      : string;
    FNumInputs        : Integer;
    FNumOutputs       : Integer;
    FNumParams        : Integer;
    FNumPrograms      : Integer;
    FPlugCategory     : TVstPluginCategory;
    FVSTModule        : TBasicVSTModule;
    FOnInstanciate    : TUIDInstantiateEvent;
    procedure SetUniqueID(fID: String);
    function GetUniqueID: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    UID  : Integer;
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property numInputs: Integer read FNumInputs write FNumInputs default -1;
    property numOutputs: Integer read FNumOutputs write FNumOutputs default -1;
    property numParams: Integer read FNumParams write FNumParams default -1;
    property numPrograms: Integer read FNumPrograms write FNumPrograms default -1;
    property PlugCategory: TVstPluginCategory read FPlugCategory write FPlugCategory;
    property UniqueID: string read GetUniqueID write SetUniqueID;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnInstanciate: TUIDInstantiateEvent read FOnInstanciate write FOnInstanciate;
  end;

  TCustomVstShellPlugins = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
    function GetItem(Index: Integer): TCustomVstShellPlugin;
    procedure SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
  protected
    property Items[Index: Integer]: TCustomVstShellPlugin read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstShellPlugin;
    function Insert(Index: Integer): TCustomVstShellPlugin;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

implementation


{$IFDEF FPC}
constructor TCustomVstShellPlugin.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstShellPlugin.Create(Collection: TCollection);
{$ENDIF}
begin
  inherited;
  FDisplayName  := 'Init'; // inherited GetDisplayName;
  FNumInputs    := -1;
  FNumOutputs   := -1;
  FNumPrograms  := -1;
  FNumParams    := -1;
  FPlugCategory := vpcUnknown;
  FVSTModule    := (Collection As TCustomVstShellPlugins).VSTModule;
end;

destructor TCustomVstShellPlugin.Destroy;
begin
  inherited;
end;

procedure TCustomVstShellPlugin.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomVstShellPlugin then
    with TCustomVstShellPlugin(Dest)
      do DisplayName := Self.DisplayName
  else inherited;
end;

function TCustomVstShellPlugin.GetUniqueID:string;
var i : Integer;
begin
  Result := '';
  for i := 3 downto 0 do
    Result := Result + char(UID shr (i * 8));
end;

procedure TCustomVstShellPlugin.SetUniqueID(fID:string);
begin
  UID := FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

procedure TCustomVstShellPlugin.SetDisplayName(const AValue: string);
begin
  FDisplayName := Copy(AValue,0,50);
end;

function TCustomVstShellPlugin.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

{ TCustomVstShellPlugins }

constructor TCustomVstShellPlugins.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCustomVstShellPlugin);
  FVSTModule := TBasicVSTModule(AOwner);
end;

destructor TCustomVstShellPlugins.Destroy;
begin
  while Count>0 do Delete(0);
  inherited;
end;

function TCustomVstShellPlugins.Add: TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited Add);
end;

function TCustomVstShellPlugins.Insert(Index: Integer): TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited Insert(Index));
end;

procedure TCustomVstShellPlugins.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TCustomVstShellPlugins.GetItem(Index: Integer): TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited GetItem(Index));
end;

procedure TCustomVstShellPlugins.SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
begin
  inherited SetItem(Index, Value);
end;

end.
