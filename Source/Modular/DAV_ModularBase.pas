unit DAV_ModularBase;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_ModularPin;

type
  TCustomModularBase = class(TDspComponent)
  private
    function GetPinInput(Index: Integer): TModularPin;
    function GetPinOutput(Index: Integer): TModularPin;
    procedure SetPinsInput(const Value: TModularInputPins);
    procedure SetPinsOutput(const Value: TModularOutputPins);
  protected
    FPinsInput  : TModularInputPins;
    FPinsOutput : TModularOutputPins;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessData; virtual; abstract;

    property PinsInput: TModularInputPins read FPinsInput write SetPinsInput;
    property PinsOutput: TModularOutputPins read FPinsOutput write SetPinsOutput;

    property PinInput[Index: Integer]: TModularPin read GetPinInput;
    property PinOutput[Index: Integer]: TModularPin read GetPinOutput;
  end;

  TModularBase = class(TCustomModularBase)
  published
    property PinsInput;
    property PinsOutput;
  end;

implementation

uses
  SysUtils;

{ TCustomModularBase }

constructor TCustomModularBase.Create(AOwner: TComponent);
begin
 inherited;
 FPinsInput  := TModularInputPins.Create(Self);
 FPinsOutput := TModularOutputPins.Create(Self);
end;

destructor TCustomModularBase.Destroy;
begin
 FreeAndNil(FPinsInput);
 FreeAndNil(FPinsOutput);
 inherited;
end;

function TCustomModularBase.GetPinInput(Index: Integer): TModularPin;
begin
 if Index in [0..FPinsInput.Count - 1]
  then result := FPinsInput[Index]
  else result := nil;
end;

function TCustomModularBase.GetPinOutput(Index: Integer): TModularPin;
begin
 if Index in [0..FPinsOutput.Count - 1]
  then result := FPinsOutput[Index]
  else result := nil;
end;

procedure TCustomModularBase.SetPinsInput(const Value: TModularInputPins);
begin
 FPinsInput.Assign(Value);
end;

procedure TCustomModularBase.SetPinsOutput(const Value: TModularOutputPins);
begin
 FPinsOutput.Assign(Value);
end;

end.
