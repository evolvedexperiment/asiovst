unit DAV_ModularPin;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Variants, DAV_Common, DAV_DspCommon;

type
  TModularPinDataType = (mdtInteger, mdtBoolean, mdtSingle, mdtDouble);
  TModularPinTriggerType = (mttEvent, mttBlock);

  TCustomModularPin = class(TCollectionItem)
  private
    FDisplayName            : string;
    FDataType               : TModularPinDataType;
    FBuffer                 : Pointer;
    FBufferSize             : Integer;
    FTriggerType            : TModularPinTriggerType;
    FOnPinConnectionChanged : TNotifyEvent;
    procedure SetDataType(const Value: TModularPinDataType);
    procedure SetBufferSize(const Value: Integer);
    procedure SetTriggerType(const Value: TModularPinTriggerType);
    function GetBufferAsDoubleArray: PDAVDoubleFixedArray;
    function GetBufferAsSingleArray: PDAVSingleFixedArray;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const AValue: string); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure PinConnected; virtual; abstract;
    procedure PinDisconnected; virtual; abstract;
    procedure PinConnectionChanged; virtual; abstract; 

    procedure BufferSizeChanged; virtual;
    procedure TriggerTypeChanged; virtual;
    procedure DisplayNameChanged; virtual;
    procedure DataTypeChanged; virtual;
    procedure AllocateBuffer; virtual;

  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;

    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
    property Datatype: TModularPinDataType read FDataType write SetDataType default mdtSingle;
    property BufferSize: Integer read FBufferSize write SetBufferSize default 1;
    property BufferAsSingleArray: PDAVSingleFixedArray read GetBufferAsSingleArray;
    property BufferAsDoubleArray: PDAVDoubleFixedArray read GetBufferAsDoubleArray;
    property TriggerType: TModularPinTriggerType read FTriggerType write SetTriggerType default mttEvent;

    property OnPinConnectionChanged: TNotifyEvent read FOnPinConnectionChanged write FOnPinConnectionChanged; 
  end;

  TCustomModularPins = class(TCollection)
  protected
    FOnPinCountChange: TNotifyEvent;
    function IndexOf(Value: TCustomModularPin): Integer;
    function GetItem(Index: Integer): TCustomModularPin; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomModularPin); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TCustomModularPin read GetItem write SetItem; default;
  public
    destructor Destroy; override;
    function Add: TCustomModularPin;
    function Insert(const Index: Integer): TCustomModularPin;
    procedure Delete(const Index: Integer);

    property Count;
    property OnPinCountChange: TNotifyEvent read FOnPinCountChange write FOnPinCountChange;
  end;

  TModularInputPins = class(TCustomModularPins)
  public
    constructor Create; virtual;
  end;

  TModularOutputPins = class(TCustomModularPins)
  public
    constructor Create; virtual;
  end;

  TCustomModularPinInput = class;
  TCustomModularPinOutput = class;

  TCustomModularPinInput = class(TCustomModularPin)
  protected
    FOutputPins : array of TCustomModularPinOutput;
    procedure AddOutputPin(Pin: TCustomModularPinOutput);
    procedure RemoveOutputPin(Pin: TCustomModularPinOutput);
    function IndexOf(Pin: TCustomModularPin): Integer;

    procedure PinConnected; override;
    procedure PinDisconnected; override;
    procedure PinConnectionChanged; override;
  public
    procedure Connect(Pin: TCustomModularPinOutput); virtual;
    procedure Disconnect(Pin: TCustomModularPinOutput); virtual;
  end;

  TCustomModularPinOutput = class(TCustomModularPin)
  private
    procedure DisconnectPin; virtual;
  protected
    FInputPin : TCustomModularPinInput;
    procedure PinConnected; override;
    procedure PinDisconnected; override;
    procedure PinConnectionChanged; override;
  public
    procedure Connect(Pin: TCustomModularPinInput); virtual;
    procedure Disconnect; virtual;

    property InputPin: TCustomModularPinInput read FInputPin;
  end;

  TModularPinInput = class(TCustomModularPinInput)
  published
    property DisplayName;
  end;

  TModularPinOutput = class(TCustomModularPinOutput)
  published
    property DisplayName;
  end;

implementation

resourcestring
  RCStrPinAlreadyRemoved = 'Pin already removed';
  RCStrOutputPinAlreadyConnected = 'Output pin already connected!';
  RCStrDataTypeUndefined = 'Data type is yet undefined!';
  RCStrDataTypeMismatch = 'Data type mismatch!';
  RCStrTriggerTypeMismatch = 'Process type mismatch';

{ TCustomModularPin }

constructor TCustomModularPin.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := 'Pin ' + IntToStr(Collection.Count);
 FDataType    := mdtSingle;
 FBufferSize  := 1;
 FBuffer      := nil;
end;

destructor TCustomModularPin.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function CalculateByteSizeByDataType(DataType: TModularPinDataType): Byte;
begin
 case DataType of
  mdtInteger,
  mdtSingle   : result := 4;
  mdtDouble   : result := 8;
  else raise Exception.Create(RCStrDataTypeUndefined);
 end;
end;

procedure TCustomModularPin.AllocateBuffer;
begin
 case FTriggerType of
  mttEvent : ReallocMem(FBuffer, CalculateByteSizeByDataType(FDataType));
  mttBlock : ReallocMem(FBuffer, FBufferSize * CalculateByteSizeByDataType(FDataType));
 end;
end;

procedure TCustomModularPin.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomModularPin then
  with TCustomModularPin(Dest) do
   try
    DisplayName := Self.DisplayName;
   except
    inherited;
   end
  else inherited;
end;

function TCustomModularPin.GetBufferAsDoubleArray: PDAVDoubleFixedArray;
begin
 result := PDAVDoubleFixedArray(FBuffer);
end;

function TCustomModularPin.GetBufferAsSingleArray: PDAVSingleFixedArray;
begin
 result := PDAVSingleFixedArray(FBuffer);
end;

function TCustomModularPin.GetDisplayName: string;
begin
 result := FDisplayName;
end;

procedure TCustomModularPin.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TCustomModularPin.SetDataType(const Value: TModularPinDataType);
begin
 if FDataType <> Value then
  begin
   FDataType := Value;
   DataTypeChanged;
  end;
end;

procedure TCustomModularPin.BufferSizeChanged;
begin
 AllocateBuffer;
end;

procedure TCustomModularPin.DataTypeChanged;
begin
 AllocateBuffer;
end;

procedure TCustomModularPin.SetDisplayName(const AValue: string);
begin
 if AValue <> FDisplayName then
  begin
   FDisplayName := AValue;
   DisplayNameChanged;
  end;
 inherited;
end;

procedure TCustomModularPin.SetTriggerType(const Value: TModularPinTriggerType);
begin
 if FTriggerType <> Value then
  begin
   FTriggerType := Value;
   TriggerTypeChanged;
  end;
end;

procedure TCustomModularPin.TriggerTypeChanged;
begin
 AllocateBuffer;
end;

procedure TCustomModularPin.DisplayNameChanged;
begin

end;

{ TCustomModularPins }

destructor TCustomModularPins.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomModularPins.Add: TCustomModularPin;
begin
 Result := TCustomModularPin(inherited Add);
end;

procedure TCustomModularPins.Delete(const Index: Integer);
begin
 inherited Delete(Index);
end;

function TCustomModularPins.GetItem(Index: Integer): TCustomModularPin;
begin
 Result := TCustomModularPin(inherited GetItem(Index));
end;

function TCustomModularPins.IndexOf(Value: TCustomModularPin): Integer;
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

function TCustomModularPins.Insert(const Index: Integer): TCustomModularPin;
begin
 Result := TCustomModularPin(inherited Insert(Index));
end;

procedure TCustomModularPins.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 if assigned(FOnPinCountChange)
  then FOnPinCountChange(Self);
end;

procedure TCustomModularPins.SetItem(Index: Integer; const Value: TCustomModularPin);
begin
 inherited SetItem(Index, Value);
end;


{ TModularInputPins }

constructor TModularInputPins.Create;
begin
 inherited Create(TModularPinInput);
end;


{ TModularOutputPins }

constructor TModularOutputPins.Create;
begin
 inherited Create(TModularPinOutput);
end;


{ TCustomModularPinOutput }

procedure TCustomModularPinOutput.Connect(Pin: TCustomModularPinInput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinInput);

 // check whether the data types match
 if Pin.Datatype <> Datatype
  then raise Exception.Create(RCStrDataTypeMismatch);

 // check whether the trigger types match
 if Pin.TriggerType <> TriggerType
  then raise Exception.Create(RCStrTriggerTypeMismatch);

 if FInputPin <> Pin then
  begin
   DisconnectPin;
   FInputPin := Pin;
   PinConnected;
  end;
end;

procedure TCustomModularPinOutput.Disconnect;
begin
 DisconnectPin;
 PinDisconnected;
end;

procedure TCustomModularPinOutput.DisconnectPin;
begin
 if assigned(FInputPin)
  then FInputPin.Disconnect(Self);
 FInputPin := nil;
end;

procedure TCustomModularPinOutput.PinConnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinOutput.PinDisconnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinOutput.PinConnectionChanged;
begin
 if assigned(FOnPinConnectionChanged)
  then FOnPinConnectionChanged(Self);
end;

{ TCustomModularPinInput }

function TCustomModularPinInput.IndexOf(Pin: TCustomModularPin): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Length(FOutputPins) - 1 do
  if FOutputPins[i] = Pin then
   begin
    result := i;
    exit;
   end;
end;

procedure TCustomModularPinInput.AddOutputPin(Pin: TCustomModularPinOutput);
begin
 SetLength(FOutputPins, Length(FOutputPins) + 1);
 FOutputPins[Length(FOutputPins) - 1] := Pin;
end;

procedure TCustomModularPinInput.RemoveOutputPin(Pin: TCustomModularPinOutput);
var
  PinIndex : Integer;
begin
 PinIndex := IndexOf(Pin);
 if PinIndex < 0
  then raise Exception.Create(RCStrPinAlreadyRemoved);

 if PinIndex < Length(FOutputPins) - 2
  then Move(FOutputPins[PinIndex + 1], FOutputPins[PinIndex], (Length(FOutputPins) - 1 - PinIndex) * SizeOf(TCustomModularPinOutput));
 SetLength(FOutputPins, Length(FOutputPins) - 1);
end;

procedure TCustomModularPinInput.Connect(Pin: TCustomModularPinOutput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinOutput);

 // check if the pin is already connected
 if IndexOf(Pin) >= 0
  then raise Exception.Create(RCStrOutputPinAlreadyConnected);

 // check whether the data types match
 if Pin.Datatype <> Datatype
  then raise Exception.Create(RCStrDataTypeMismatch);

 // check whether the trigger types match
 if Pin.TriggerType <> TriggerType
  then raise Exception.Create(RCStrTriggerTypeMismatch);

 // finally add output pin
 AddOutputPin(Pin);
 PinConnected;
end;

procedure TCustomModularPinInput.Disconnect(Pin: TCustomModularPinOutput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinOutput);

 // disconnect if the pin is connected
 if IndexOf(Pin) >= 0 then
  begin
   RemoveOutputPin(Pin);
   Pin.Disconnect;
   PinDisconnected;
  end;
end;

procedure TCustomModularPinInput.PinConnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinInput.PinDisconnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinInput.PinConnectionChanged;
begin
 if assigned(FOnPinConnectionChanged)
  then FOnPinConnectionChanged(Self);
end;

end.
