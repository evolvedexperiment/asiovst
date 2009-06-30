unit DAV_ModularManager;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, DAV_Common, DAV_DspCommon, DAV_ModularBase;

type
  TModularManager = class(TComponent)
  private
    FModules : TObjectList;
    function GetModule(Index: Integer): TModularBase;
    function GetModuleCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddModule(Module: TModularBase);

    property ModuleCount: Integer read GetModuleCount;
    property Module[Index: Integer]: TModularBase read GetModule;
  end;

implementation

uses
  SysUtils;

{ TModularManager }

constructor TModularManager.Create(AOwner: TComponent);
begin
 inherited;
 FModules := TObjectList.Create;
end;

destructor TModularManager.Destroy;
begin
 FreeAndNil(FModules);
 inherited;
end;

procedure TModularManager.AddModule(Module: TModularBase);
begin
 FModules.Add(Module);
end;

function TModularManager.GetModule(Index: Integer): TModularBase;
begin
 if Index in [0..ModuleCount - 1]
  then result := TModularBase(FModules[Index])
  else result := nil;
end;

function TModularManager.GetModuleCount: Integer;
begin
 result := FModules.Count;
end;

end.
