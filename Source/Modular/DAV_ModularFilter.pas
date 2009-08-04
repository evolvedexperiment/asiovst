unit DAV_ModularFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_ModularBase,
  DAV_ModularPin;

type
  TCustomModularFilter = class(TCustomModularBase)
  protected
    FFilter : TCustomFilter;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessModule; override;
  end;

implementation

uses
  SysUtils;

{ TCustomModularFilter }

constructor TCustomModularFilter.Create;
begin
 inherited;

 // setup input pin
 with FPinsInput.Add do
  begin
   Datatype := mdtSingle;
   TriggerType := mttBlock;
  end;

 // setup output pin
 with FPinsOutput.Add do
  begin
   Datatype := mdtSingle;
   TriggerType := mttBlock;
  end;
end;

destructor TCustomModularFilter.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomModularFilter.ProcessModule;
var
  Sample : Integer;
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
begin
 inherited;

 // some asserts to make sure everything is working as it should
 assert(FPinsInput.Count = 1);
 assert(FPinsOutput.Count = 1);
 assert(FPinsInput[0].Buffersize = FPinsOutput[0].Buffersize);

 Input  := FPinsInput[0].BufferAsSingleArray;
 Output := FPinsOutput[0].BufferAsSingleArray;

 // process samples
 for Sample := 0 to FPinsInput[0].Buffersize - 1
  do Output[Sample] := FFilter.ProcessSample(Input[Sample]);

end;

end.
