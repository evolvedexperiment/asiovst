unit DAV_ModularPin;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Variants, DAV_Common, DAV_DspCommon;

type
  TModularPinDataType = (mdtInteger, mdtBoolean, mdtSingle, mdtDouble);
  TModularPinProcessType = (mptEvent, mptSamplerate);

  TCustomModularPin = class(TCollectionItem)
  private
    FDisplayName : string;
    FDataType    : TModularPinDataType;
    FBuffer      : Pointer;
    FBufferSize  : Integer;
    FProcessType : TModularPinProcessType;
    procedure SetDataType(const Value: TModularPinDataType);
    procedure SetBufferSize(const Value: Integer);
    procedure BufferSizeChanged;
    procedure SetProcessType(const Value: TModularPinProcessType);
    procedure ProcessTypeChanged;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const AValue: string); override;
    procedure AssignTo(Dest: TPersistent); override;

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
    property ProcessType: TModularPinProcessType read FProcessType write SetProcessType default mptEvent;
  end;

  TModularPin = class(TCustomModularPin)
  published
    property DisplayName;
  end;

  TCustomModularPinInput = class(TCustomModularPin)
  end;

  TCustomModularPinOutput = class(TCustomModularPin)
  end;

  TModularPinInput = class(TCustomModularPinInput)
  published
    property DisplayName;
  end;

  TModularPinOutput = class(TCustomModularPinOutput)
  published
    property DisplayName;
  end;

  TModularPins = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TModularPin; virtual;
    procedure SetItem(Index: Integer; const Value: TModularPin); virtual;
    property Items[Index: Integer]: TModularPin read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TModularPin;
    function Insert(const Index: Integer): TModularPin;
    procedure Delete(const Index: Integer);
    property Count;
  end;

  TModularInputPins = class(TModularPins)
  private
  public
  end;

  TModularOutputPins = class(TModularPins)
  private
  public
  end;

implementation

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
 end;
end;

procedure TCustomModularPin.AllocateBuffer;
begin
 ReallocMem(FBuffer, FBufferSize * CalculateByteSizeByDataType(FDataType));
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

procedure TCustomModularPin.SetProcessType(const Value: TModularPinProcessType);
begin
 if FProcessType <> Value then
  begin
   FProcessType := Value;
   ProcessTypeChanged;
  end;
end;

procedure TCustomModularPin.ProcessTypeChanged;
begin
end;

procedure TCustomModularPin.DisplayNameChanged;
begin
end;

{ TCustomModularPins }

constructor TModularPins.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TModularPin);
end;

destructor TModularPins.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TModularPins.Add: TModularPin;
begin
 Result := TModularPin(inherited Add);
end;

procedure TModularPins.Delete(const Index: Integer);
begin
 inherited Delete(Index);
end;

function TModularPins.GetItem(Index: Integer): TModularPin;
begin
 Result := TModularPin(inherited GetItem(Index));
end;

function TModularPins.Insert(const Index: Integer): TModularPin;
begin
 Result := TModularPin(inherited Insert(Index));
end;

procedure TModularPins.SetItem(Index: Integer; const Value: TModularPin);
begin
 inherited SetItem(Index, Value);
end;

end.
