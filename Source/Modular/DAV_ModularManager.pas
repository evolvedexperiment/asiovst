unit DAV_ModularManager;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, DAV_Common, DAV_DspCommon, DAV_ModularBase;

type
  TModularManager = class(TDspObject)
  private
    FModules : TObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TModularManager }

constructor TModularManager.Create;
begin
 FModules := TObjectList.Create;
end;

destructor TModularManager.Destroy;
begin
 FreeAndNil(FModules);
 inherited;
end;

end.
