unit DDspBaseComponent;

interface

{$I ASIOVST.inc}

uses Classes, DAVDCommon, Contnrs, DAVDProcessingComponent;

type
  TDspQueueList = TComponentList;

  TDspBaseComponent = class(TAVDProcessingComponent)
  private
    fSampleRate: Single;
    fEnabled: Boolean;
    fChannels: Integer;
    fBypass: Boolean;
  protected
    fNextDspQueueItem: TDspBaseComponent;
    fPrevDspQueueItem: TDspBaseComponent;

    fStdProcessS:   TDspBaseProcessFuncS;
    fStdProcessD:   TDspBaseProcessFuncD;
    fStdProcessSA:  TDspBaseProcessFuncSA;
    fStdProcessDA:  TDspBaseProcessFuncDA;
    fStdProcessSAA: TDspBaseProcessFuncSAA;
    fStdProcessDAA: TDspBaseProcessFuncDAA;

    fStdProcessQueueS:   TDspBaseProcessFuncS;
    fStdProcessQueueD:   TDspBaseProcessFuncD;
    fStdProcessQueueSA:  TDspBaseProcessFuncSA;
    fStdProcessQueueDA:  TDspBaseProcessFuncDA;
    fStdProcessQueueSAA: TDspBaseProcessFuncSAA;
    fStdProcessQueueDAA: TDspBaseProcessFuncDAA;
    
    procedure SetBypass(const Value: Boolean); override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetSampleRate(const Value: Single); override;
    procedure SetChannels(const Value: Integer); override;

    procedure SetNextDspQueueItem(const Value: TDspBaseComponent); virtual;
    procedure SampleRateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure UpdateParameters; virtual;

    procedure RegisterInOwner(item: TDspBaseComponent);
    procedure UnRegisterInOwner(item: TDspBaseComponent);
  published
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; UseSampleRate: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Init; override;               // called automaticaly in constructor
    procedure Reset; override;              // called manualy
    procedure ResetQueue; virtual;
    function  GetFollowingItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetPreviousItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetQueueItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback


    procedure ProcessSilence    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Single; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Double; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDSingleDynArray; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDSingleDynArray; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDSingleDynArray; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDSingleDynArray; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDSingleDynArray; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfSingleDynArray) overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDArrayOfSingleDynArray) overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfSingleDynArray) overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDArrayOfSingleDynArray) overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray) overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfDoubleDynArray); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDArrayOfDoubleDynArray); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfDoubleDynArray); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDArrayOfDoubleDynArray); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray); overload; virtual;

    property PrevDspQueueItem: TDspBaseComponent read fPrevDspQueueItem write fPrevDspQueueItem;
  published
    property Enabled: Boolean                    read fEnabled          write SetEnabled    default true;
    property Bypass: Boolean                     read fBypass           write SetBypass     default false;
    property Channels: Integer                   read fChannels         write SetChannels   default 2;
    property SampleRate: Single                  read fSampleRate       write SetSampleRate;
    property NextDspQueueItem: TDspBaseComponent read fNextDspQueueItem write SetNextDspQueueItem;
  end;

implementation

uses Sysutils, Math, DVSTModuleWithDsp
  {$IFDEF PUREPASCAL},DAVDBufferMathPascal{$ELSE},DAVDBufferMathAsm{$ENDIF};

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;
  fNextDspQueueItem := nil;
  fPrevDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := false;
  fSampleRate       := 44100;
  fChannels         := 2;

  fStdProcessS   := ProcessBypass;
  fStdProcessD   := ProcessBasic;
  fStdProcessSA  := ProcessBasic;
  fStdProcessDA  := ProcessBasic;
  fStdProcessSAA := ProcessBasic;
  fStdProcessDAA := ProcessBasic;

  fStdProcessQueueS  := ProcessQueueBasic;
  fStdProcessQueueD  := ProcessQueueBasic;
  fStdProcessQueueSA := ProcessQueueBasic;
  fStdProcessQueueDA := ProcessQueueBasic;
  fStdProcessQueueSAA:= ProcessQueueBasic;
  fStdProcessQueueDAA:= ProcessQueueBasic;

  Init;

  SetEnabled(fEnabled);
  SetBypass(fBypass);

  RegisterInOwner(self);
end;

constructor TDspBaseComponent.Create(AOwner: TComponent; UseSampleRate: Integer);
begin
  Create(AOwner);
  SetSampleRate(UseSampleRate);
end;

destructor TDspBaseComponent.Destroy;
begin
  UnRegisterInOwner(self);

  if assigned(fNextDspQueueItem) then
  begin
    fNextDspQueueItem.PrevDspQueueItem := fPrevDspQueueItem;
    if not assigned(fPrevDspQueueItem) then RegisterInOwner(fNextDspQueueItem);

  end;
  if assigned(fPrevDspQueueItem) then fPrevDspQueueItem.NextDspQueueItem := fNextDspQueueItem;
  inherited;
end;

procedure TDspBaseComponent.RegisterInOwner(item: TDspBaseComponent);
begin
  if Owner is TDspVSTModule then
    (Owner as TDspVSTModule).RegisterDSPItem(item);
end;

procedure TDspBaseComponent.UnRegisterInOwner(item: TDspBaseComponent);
begin
  if Owner is TDspVSTModule then
    (Owner as TDspVSTModule).UnRegisterDSPItem(item);
end;

procedure TDspBaseComponent.Init;  begin end;
procedure TDspBaseComponent.Reset; begin end;
procedure TDspBaseComponent.UpdateParameters; begin end;

procedure TDspBaseComponent.SampleRateChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ChannelsChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ResetQueue;
begin
  Reset;
  if assigned(fNextDspQueueItem) then fNextDspQueueItem.ResetQueue;
end;


function TDspBaseComponent.GetFollowingItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fNextDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fNextDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Add(fNextDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetFollowingItems(items);
  end;
end;

function TDspBaseComponent.GetPreviousItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fPrevDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fPrevDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Insert(0, fPrevDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetPreviousItems(items);
  end;
end;

function TDspBaseComponent.GetQueueItems(var items: TDspQueueList): boolean;
var mypos: integer;
begin
  result:=GetPreviousItems(items);
  if not result then exit;

  mypos:=items.Count;

  result:=result and GetFollowingItems(items);
  if not result then exit;
  items.Insert(mypos,self);
end;

procedure TDspBaseComponent.SetNextDspQueueItem(const Value: TDspBaseComponent);
var x: TDspQueueList; backup: TDspBaseComponent;
begin
  if (Value<>self) and (fNextDspQueueItem<>Value) then
  begin
    backup := fNextDspQueueItem;
    fNextDspQueueItem := Value;
    if Value<>nil then
    begin
      x:=nil;
      if (fPrevDspQueueItem=Value) or not (GetQueueItems(x)) then
      begin
        fNextDspQueueItem := backup;
        raise Exception.Create('Processing queue loopback');
        exit;
      end else begin
        if not assigned(fNextDspQueueItem.PrevDspQueueItem) then
        UnRegisterInOwner(fNextDspQueueItem.PrevDspQueueItem);

        fNextDspQueueItem.PrevDspQueueItem:=self;
      end;
    end;

    // Important
    SetEnabled(fEnabled);
    SetBypass(fBypass);
  end;
end;

procedure TDspBaseComponent.SetSampleRate(const Value: Single);
begin
  if (fSampleRate<>Value) and (Value>0) then
  begin
    fSampleRate := Value;
    SampleRateChanged;
    
    if assigned(fNextDspQueueItem) then fNextDspQueueItem.SampleRate:=fSampleRate;
  end;
end;

procedure TDspBaseComponent.SetChannels(const Value: Integer);
begin
  if (fChannels<>Value) and (Value>0) then
  begin
    fChannels := Value;
    ChannelsChanged;

    if assigned(fNextDspQueueItem) then fNextDspQueueItem.Channels:=fChannels;
  end;
end;

procedure TDspBaseComponent.SetBypass(const Value: Boolean);
begin
  fBypass := Value;
  if not fEnabled then exit;

  if Value then
  begin
    // bypass everything
    fProcessS  := ProcessBypass;
    fProcessD  := ProcessBypass;
    fProcessSA := ProcessBypass;
    fProcessDA := ProcessBypass;
    fProcessSAA:= ProcessBypass;
    fProcessDAA:= ProcessBypass;

    if assigned(fNextDspQueueItem) then
    begin
      // ignore this item and call directly the next item
      fProcessQueueS  := ProcessQueueBypass;
      fProcessQueueD  := ProcessQueueBypass;
      fProcessQueueSA := ProcessQueueBypass;
      fProcessQueueDA := ProcessQueueBypass;
      fProcessQueueSAA:= ProcessQueueBypass;
      fProcessQueueDAA:= ProcessQueueBypass;
    end else begin
      // bypass this item and there is no next item, so don't call it
      fProcessQueueS  := ProcessBypass;
      fProcessQueueD  := ProcessBypass;
      fProcessQueueSA := ProcessBypass;
      fProcessQueueDA := ProcessBypass;
      fProcessQueueSAA:= ProcessBypass;
      fProcessQueueDAA:= ProcessBypass;
    end;
  end else begin
    // process item
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end;
end;

procedure TDspBaseComponent.SetEnabled(const Value: Boolean);
begin
  if Value then
  begin
    // enable everything
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only Process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end else begin
    // disable everything
    fProcessS  := ProcessSilence;
    fProcessD  := ProcessSilence;
    fProcessSA := ProcessSilence;
    fProcessDA := ProcessSilence;
    fProcessSAA:= ProcessSilence;
    fProcessDAA:= ProcessSilence;

    // disable this item and don't process the next one
    fProcessQueueS  := ProcessSilence;
    fProcessQueueD  := ProcessSilence;
    fProcessQueueSA := ProcessSilence;
    fProcessQueueDA := ProcessSilence;
    fProcessQueueSAA:= ProcessSilence;
    fProcessQueueDAA:= ProcessSilence;
  end;
  fEnabled := Value;
  if fEnabled and fBypass then SetBypass(true);
end;





procedure TDspBaseComponent.ProcessSilence(var Data: Single; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var Data: Double; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDSingleDynArray; const channel: integer);
begin
 FillChar(ProcessBuffer[0], length(ProcessBuffer) * SizeOf(Single), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer);
begin
 FillChar(ProcessBuffer[0], length(ProcessBuffer) * SizeOf(Double), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDArrayOfSingleDynArray);
var i : Integer;
begin
 for i := 0 to Length(ProcessBuffer) - 1
  do FillChar(ProcessBuffer[i, 0], Length(ProcessBuffer[i]) * SizeOf(Single), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDArrayOfDoubleDynArray);
var i : Integer;
begin
 for i := 0 to Length(ProcessBuffer) - 1
  do FillChar(ProcessBuffer[i, 0], Length(ProcessBuffer[i]) * SizeOf(Double), 0);
end;





procedure TDspBaseComponent.ProcessBypass(var Data: Single; const channel: integer);
begin
 // Do nothing
end;

procedure TDspBaseComponent.ProcessBypass(var Data: Double; const channel: integer);
begin
 // Do nothing
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDSingleDynArray; const channel: integer);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray);
begin
 // Do nothing with the buffer
end;







procedure TDspBaseComponent.ProcessBasic(var Data: Single; const channel: integer);
begin
 fProcessS(Data, channel);
end;

procedure TDspBaseComponent.ProcessBasic(var Data: Double; const channel: integer);
var tmp: single;
begin
 fProcessD(Data, channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDSingleDynArray; const channel: integer);
var i: integer;
begin
 for i := 0 to length(ProcessBuffer) - 1
  do fProcessS(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer);
var i: integer;
begin
 for i := 0 to length(ProcessBuffer) - 1
  do fProcessD(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDArrayOfSingleDynArray);
var i: integer;
begin
 for i := 0 to length(ProcessBuffer) - 1
  do ProcessBasic(ProcessBuffer[i], i);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDArrayOfDoubleDynArray);
var i: integer;
begin
 for i := 0 to length(ProcessBuffer) - 1
  do ProcessBasic(ProcessBuffer[i], i);
end;




procedure TDspBaseComponent.ProcessQueueBasic(var Data: Single; const channel: integer);
begin
 fProcessS(Data, channel);
 fNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var Data: Double; const channel: integer);
begin
 fProcessD(Data, channel);
 fNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDSingleDynArray; const channel: integer);
begin
 fProcessSA(ProcessBuffer, channel);
 fNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer);
begin
 fProcessDA(ProcessBuffer, channel);
 fNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDArrayOfSingleDynArray);
begin
 fNextDspQueueItem.ProcessQueueSAA(ProcessBuffer);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDArrayOfDoubleDynArray);
begin
 fNextDspQueueItem.ProcessQueueDAA(ProcessBuffer);
end;




procedure TDspBaseComponent.ProcessQueueBypass(var Data: Single; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var Data: Double; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDSingleDynArray; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray);
begin
 fNextDspQueueItem.ProcessQueueSAA(ProcessBuffer);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray);
begin
 fNextDspQueueItem.ProcessQueueDAA(ProcessBuffer);
end;


end.
