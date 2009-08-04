unit DAV_ModularContainer;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_ModularBase, DAV_ModularPin;

type
  TCustomModularItem = class(TCollectionItem)
  private
    FModuleProcessed: Boolean;
  protected
    FModule : TCustomModularBase;
  public
    property Module: TCustomModularBase read FModule write FModule;
    property ModuleProcessed: Boolean read FModuleProcessed write FModuleProcessed;
  end;

  TCustomModularCollection = class(TCollection)
  protected
    FOnModuleCountChange: TNotifyEvent;
    function IndexOf(Value: TCustomModularItem): Integer;
    function GetItem(Index: Integer): TCustomModularItem; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomModularItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TCustomModularItem read GetItem write SetItem; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add: TCustomModularItem;
    function Insert(const Index: Integer): TCustomModularItem;
    procedure Delete(const Index: Integer);

    property Count;
    property OnModuleCountChange: TNotifyEvent read FOnModuleCountChange write FOnModuleCountChange;
  end;

  TCustomModularContainer = class(TCustomModularBase)
  private
    FModuleCollection: TCustomModularCollection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessModule; override;
  end;

implementation

uses
  SysUtils;  

{ TCustomModularCollection }

destructor TCustomModularCollection.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomModularCollection.Add: TCustomModularItem;
begin
 Result := TCustomModularItem(inherited Add);
end;

constructor TCustomModularCollection.Create;
begin
 inherited Create(TCustomModularItem);
end;

procedure TCustomModularCollection.Delete(const Index: Integer);
begin
 inherited Delete(Index);
end;

function TCustomModularCollection.GetItem(Index: Integer): TCustomModularItem;
begin
 Result := TCustomModularItem(inherited GetItem(Index));
end;

function TCustomModularCollection.IndexOf(Value: TCustomModularItem): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Count - 1 do
  if Items[i] = Value then
   begin
    result := i;
    exit;
   end;
end;

function TCustomModularCollection.Insert(const Index: Integer): TCustomModularItem;
begin
 Result := TCustomModularItem(inherited Insert(Index));
end;

procedure TCustomModularCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 if assigned(FOnModuleCountChange)
  then FOnModuleCountChange(Self);
end;

procedure TCustomModularCollection.SetItem(Index: Integer;
  const Value: TCustomModularItem);
begin
 inherited SetItem(Index, Value);
end;

{ TCustomModularContainer }

constructor TCustomModularContainer.Create;
begin
 inherited;
 FModuleCollection := TCustomModularCollection.Create;
end;

destructor TCustomModularContainer.Destroy;
begin
 FreeAndNil(FModuleCollection);
 inherited;
end;

procedure TCustomModularContainer.ProcessModule;
var
  ModuleNo : Integer;
begin
 inherited;

(*
 // reset modules
 for ModuleNo := 0 to FModuleCollection - 1
  do FModuleCollection[ModuleNo].ModuleProcessed := False;

 // process modules
 for ModuleNo := 0 to FModuleCollection - 1 do
  if not FModuleCollection[ModuleNo].ModuleProcessed then
   begin
    FModuleCollection[ModuleNo].Module.ProcessModule;
    FModuleCollection[ModuleNo].ModuleProcessed := True;
   end;
*)
end;

end.
