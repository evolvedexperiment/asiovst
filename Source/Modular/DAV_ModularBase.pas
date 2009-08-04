unit DAV_ModularBase;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_ModularPin;

type
  TCustomModularBase = class(TDspObject)
  private
    FName                 : string;
    FDescription          : string;
    FOnNameChanged        : TNotifyEvent;
    FOnDescriptionChanged : TNotifyEvent;
    FOnPinCountChanged    : TNotifyEvent;
    function GetPinInput(Index: Integer): TModularPinInput;
    function GetPinOutput(Index: Integer): TModularPinOutput;
    procedure SetPinsInput(const Value: TModularInputPins);
    procedure SetPinsOutput(const Value: TModularOutputPins);
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure DescriptionChanged;
    procedure NameChanged;
  protected
    FPinsInput  : TModularInputPins;
    FPinsOutput : TModularOutputPins;
    property PinsInput: TModularInputPins read FPinsInput write SetPinsInput;
    property PinsOutput: TModularOutputPins read FPinsOutput write SetPinsOutput;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessModule; virtual; abstract;

    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property PinInput[Index: Integer]: TModularPinInput read GetPinInput;
    property PinOutput[Index: Integer]: TModularPinOutput read GetPinOutput;

    property OnNameChanged: TNotifyEvent read FOnNameChanged write FOnNameChanged;
    property OnDescriptionChanged: TNotifyEvent read FOnDescriptionChanged write FOnDescriptionChanged;
    property OnPinCountChange: TNotifyEvent read FOnPinCountChanged write FOnPinCountChanged;
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

constructor TCustomModularBase.Create;
begin
 inherited;
 FPinsInput  := TModularInputPins.Create;
 FPinsOutput := TModularOutputPins.Create;
end;

destructor TCustomModularBase.Destroy;
begin
 FreeAndNil(FPinsInput);
 FreeAndNil(FPinsOutput);
 inherited;
end;

function TCustomModularBase.GetPinInput(Index: Integer): TModularPinInput;
begin
 if Index in [0..FPinsInput.Count - 1]
  then result := TModularPinInput(FPinsInput[Index])
  else result := nil;
end;

function TCustomModularBase.GetPinOutput(Index: Integer): TModularPinOutput;
begin
 if Index in [0..FPinsOutput.Count - 1]
  then result := TModularPinOutput(FPinsOutput[Index])
  else result := nil;
end;

procedure TCustomModularBase.SetDescription(const Value: string);
begin
 if FDescription <> Value then
  begin
   FDescription := Value;
   DescriptionChanged;
  end;
end;

procedure TCustomModularBase.SetName(const Value: string);
begin
 if FName <> Value then
  begin
   FName := Value;
   NameChanged;
  end;
end;

procedure TCustomModularBase.DescriptionChanged;
begin
 if assigned(FOnDescriptionChanged)
  then FOnDescriptionChanged(Self);
end;

procedure TCustomModularBase.NameChanged;
begin
 if assigned(FOnNameChanged)
  then FOnNameChanged(Self);
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
