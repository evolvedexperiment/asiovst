unit DAV_ModularBase;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon;

type
  TModularBase = class(TDspObject)

  public
(*
    constructor Create; virtual;
    destructor Destroy; override;
*)
    procedure ProcessData; virtual; abstract;
  end;

implementation

end.
