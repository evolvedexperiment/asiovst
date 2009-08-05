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
    function GetItem(Index: Integer): TCustomModularItem; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomModularItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TCustomModularItem read GetItem write SetItem; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add: TCustomModularItem;
    function Insert(const Index: Integer): TCustomModularItem;
    function IndexOf(Value: TCustomModularItem): Integer; overload;
    function IndexOf(Value: TCustomModularBase): Integer; overload;
    procedure Delete(const Index: Integer);

    property Count;
    property OnModuleCountChange: TNotifyEvent read FOnModuleCountChange write FOnModuleCountChange;
  end;

  TCustomModularIO = class(TCustomModularBase)
  public
    constructor Create; override;
  end;

  TCustomModularContainer = class(TCustomModularBase)
  private
    FModuleCollection : TCustomModularCollection;

    FProcessRelevant  : Boolean;
    function GetModule(Index: Integer): TCustomModularBase;
    function GetModuleCount: Integer;
    function GetInputPinCount: Integer;
    function GetOutputPinCount: Integer;
  protected
    function AddInputPin: TCustomModularPinInput;
    function AddOutputPin: TCustomModularPinOutput;
    procedure ClearPins;
    procedure ClearInputPins;
    procedure ClearOutputPins;

    property ModuleCollection: TCustomModularCollection read FModuleCollection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessModule; override;
    procedure AddModule(NewModule : TCustomModularBase);
    procedure RemoveModule(Module : TCustomModularBase);

    property InputPinCount: Integer read GetInputPinCount;
    property OutputPinCount: Integer read GetOutputPinCount;

    property Module[Index: Integer]: TCustomModularBase read GetModule; default;
    property ModuleCount: Integer read GetModuleCount;
    property ProcessOnlyRelevantSubmodules: Boolean read FProcessRelevant write FProcessRelevant default False;
  end;

  TModularContainer = class(TCustomModularContainer)
  published
    property ProcessOnlyRelevantSubmodules;
  end;

implementation

uses
  SysUtils;

resourcestring
  RCStrModuleIsInContainer = 'Module is already in the container!';

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

function TCustomModularCollection.IndexOf(Value: TCustomModularBase): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Count - 1 do
  if Items[i].Module = Value then
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

{ TCustomModularIO }

constructor TCustomModularIO.Create;
begin
 inherited;
 FName := 'I/O';
 FDescription := 'Module for handling the I/Os of a container';
end;

{ TCustomModularContainer }

constructor TCustomModularContainer.Create;
begin
 inherited;
 FModuleCollection := TCustomModularCollection.Create;
 FProcessRelevant  := False;
end;

destructor TCustomModularContainer.Destroy;
begin
 FreeAndNil(FModuleCollection);
 inherited;
end;

function TCustomModularContainer.GetModule(Index: Integer): TCustomModularBase;
begin
 if (Index >= 0) and (Index < FModuleCollection.Count)
  then result := FModuleCollection.Items[Index].Module
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TCustomModularContainer.GetModuleCount: Integer;
begin
 result := FModuleCollection.Count;
end;

function TCustomModularContainer.GetInputPinCount: Integer;
begin
 result := FPinsInput.Count;
end;

function TCustomModularContainer.GetOutputPinCount: Integer;
begin
 result := FPinsOutput.Count;
end;

procedure TCustomModularContainer.ProcessModule;
(*
var
  ModuleNo : Integer;
*)
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

function TCustomModularContainer.AddInputPin: TCustomModularPinInput;
begin
 Result := TCustomModularPinInput(FPinsInput.Add);
 with Result do
  begin
   Datatype := mdtSingle;
  end;
end;

function TCustomModularContainer.AddOutputPin: TCustomModularPinOutput;
begin
 Result := TCustomModularPinOutput(FPinsOutput.Add);
 with Result do
  begin
   Datatype := mdtSingle;
  end;
end;

procedure TCustomModularContainer.ClearPins;
begin
 ClearInputPins;
 ClearOutputPins;
end;

procedure TCustomModularContainer.ClearInputPins;
begin
 FPinsInput.Clear;
end;

procedure TCustomModularContainer.ClearOutputPins;
begin
 FPinsOutput.Clear;
end;

procedure TCustomModularContainer.AddModule(NewModule: TCustomModularBase);
begin
 // check whether the module is not already in the container
 if FModuleCollection.IndexOf(NewModule) >= 0
  then raise Exception.Create(RCStrModuleIsInContainer);

 with FModuleCollection.Add do
  begin
   Module := NewModule;
  end;
end;

procedure TCustomModularContainer.RemoveModule(Module: TCustomModularBase);
var
  ModuleIndex : Integer;
begin
 // get module index
 ModuleIndex := FModuleCollection.IndexOf(Module);

 // delete module if inside the container
 if ModuleIndex >= 0
  then FModuleCollection.Delete(ModuleIndex);

 // make sure the module is removed entirely from the container
 assert(FModuleCollection.IndexOf(Module) < 0);
end;

end.
