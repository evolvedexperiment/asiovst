unit DAV_ModularManager;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, DAV_Common, DAV_Classes, DAV_DspCommon, DAV_ModularBase,
  DAV_ModularContainer, DAV_ModularPin;

type
  TCustomModularManager = class(TComponent)
  private
    FModularContainer : TModularContainer;
    FChanged          : Boolean;
    FUpdateCount      : Integer;
    FOnChange         : TNotifyEvent;
    function GetModule(Index: Integer): TCustomModularBase;
    function GetModuleCount: Integer;
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddModule(Module: TCustomModularBase);
    procedure RemoveModule(Module: TCustomModularBase);

    procedure Process(const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray;
      SampleFrames: Integer);

    property ModuleCount: Integer read GetModuleCount;
    property Module[Index: Integer]: TCustomModularBase read GetModule; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TModularManager = class(TCustomModularManager)
  published
    property ModuleCount;
    property OnChange;
  end;

implementation

uses
  Math, SysUtils;

resourcestring
  RCStrUnpairedUpdate = 'Please call BeginUpdate before calling EndUpdate';

{ TCustomModularManager }

constructor TCustomModularManager.Create(AOwner: TComponent);
begin
 inherited;
 FModularContainer := TModularContainer.Create;

 // initialize some variables
 FUpdateCount := 0;
end;

destructor TCustomModularManager.Destroy;
begin
 FreeAndNil(FModularContainer);
 inherited;
end;

procedure TCustomModularManager.Changed;
begin
 FChanged := True;
 if (FUpdateCount = 0) and assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TCustomModularManager.BeginUpdate;
begin
 inc(FUpdateCount);
end;

procedure TCustomModularManager.EndUpdate;
begin
 if FUpdateCount = 0
  then raise Exception.Create(RCStrUnpairedUpdate);

 dec(FUpdateCount);

 if (FUpdateCount = 0) and FChanged then
  begin
   if assigned(FOnChange)
    then FOnChange(Self);

   FChanged := False;
  end;
end;

procedure TCustomModularManager.AddModule(Module: TCustomModularBase);
begin
 FModularContainer.AddModule(Module);
 Changed;
end;

procedure TCustomModularManager.RemoveModule(Module: TCustomModularBase);
begin
 FModularContainer.RemoveModule(Module);
 Changed;
end;

function TCustomModularManager.GetModule(Index: Integer): TCustomModularBase;
begin
 if (Index >= 0) and (Index < ModuleCount)
  then result := TCustomModularBase(FModularContainer[Index])
  else result := nil;
end;

function TCustomModularManager.GetModuleCount: Integer;
begin
 result := FModularContainer.ModuleCount;
end;

procedure TCustomModularManager.Process(const InBuffer,
  OutBuffer: TDAVArrayOfSingleDynArray; SampleFrames: Integer);
var
  Channel: Integer;
begin
 with FModularContainer do
  begin
   // copy input channel data
   for Channel := 0 to min(InputPinCount, Length(InBuffer)) - 1 do
    begin
     PinInput[Channel].BufferSize := SampleFrames;
     Move(InBuffer[Channel, 0], PinInput[Channel].BufferAsSingleArray[0], SampleFrames);
    end;

   // copy output channel data
   for Channel := 0 to min(FModularContainer.OutputPinCount, Length(OutBuffer)) - 1 do
    begin
     PinOutput[Channel].BufferSize := SampleFrames;
     Move(OutBuffer[Channel, 0], PinOutput[Channel].BufferAsSingleArray[0], SampleFrames);
    end;

   // now process the module container
   FModularContainer.ProcessModule;
  end;
end;

end.
