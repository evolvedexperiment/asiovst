unit DDspBaseComponent;

interface

{$I ASIOVST.inc}

uses Classes, DAVDCommon, Contnrs, DAVDProcessingComponent;

type
  TDspQueueList = TComponentList;

  TDspBaseComponent = class(TAVDProcessingComponent)
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


    function ProcessSilence    (input: Single; channel: integer): Single; overload; virtual;
    function ProcessBypass     (input: Single; channel: integer): Single; overload; virtual;
    function ProcessQueueBasic (input: Single; channel: integer): Single; overload; virtual;
    function ProcessQueueBypass(input: Single; channel: integer): Single; overload; virtual;

    function ProcessSilence    (input: Double; channel: integer): Double; overload; virtual;
    function ProcessBypass     (input: Double; channel: integer): Double; overload; virtual;
    function ProcessBasic      (input: Double; channel: integer): Double; overload; virtual;
    function ProcessQueueBasic (input: Double; channel: integer): Double; overload; virtual;
    function ProcessQueueBypass(input: Double; channel: integer): Double; overload; virtual;

    function ProcessSilence    (input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray; overload; virtual;
    function ProcessBypass     (input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray; overload; virtual;
    function ProcessBasic      (input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray; overload; virtual;
    function ProcessQueueBasic (input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray; overload; virtual;
    function ProcessQueueBypass(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray; overload; virtual;

    function ProcessSilence    (input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray; overload; virtual;
    function ProcessBypass     (input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray; overload; virtual;
    function ProcessBasic      (input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray; overload; virtual;
    function ProcessQueueBasic (input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray; overload; virtual;
    function ProcessQueueBypass(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray; overload; virtual;

    function ProcessSilence    (input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray; overload; virtual;
    function ProcessBypass     (input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray; overload; virtual;
    function ProcessBasic      (input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray; overload; virtual;
    function ProcessQueueBasic (input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray; overload; virtual;
    function ProcessQueueBypass(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray; overload; virtual;

    function ProcessSilence    (input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray; overload; virtual;
    function ProcessBypass     (input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray; overload; virtual;
    function ProcessBasic      (input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray; overload; virtual;
    function ProcessQueueBasic (input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray; overload; virtual;
    function ProcessQueueBypass(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray; overload; virtual;

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
  {$IFDEF PUREPASCAL},DAVDBufferMathAsm{$ELSE},DAVDBufferMathPascal{$ENDIF};

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;
  fNextDspQueueItem := nil;
  fPrevDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := false;
  fSampleRate       := 44100;
  fChannels         := 2;

  fStdProcessS  := ProcessBypass;
  fStdProcessD  := ProcessBasic;
  fStdProcessSA := ProcessBasic;
  fStdProcessDA := ProcessBasic;
  fStdProcessSAA:= ProcessBasic;
  fStdProcessDAA:= ProcessBasic;

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





function TDspBaseComponent.ProcessSilence(input: Single; channel: integer): Single;
begin
  Result:=0;
end;

function TDspBaseComponent.ProcessSilence(input: Double; channel: integer): Double;
begin
  Result:=0;
end;

function TDspBaseComponent.ProcessSilence(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray;
begin
  setlength(Result, length(input));
  Fillchar(Result[0], length(input) * SizeOf(Single), 0);
end;

function TDspBaseComponent.ProcessSilence(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray;
begin
  setlength(Result, length(input));
  Fillchar(Result[0], length(input) * SizeOf(Double), 0);
end;

function TDspBaseComponent.ProcessSilence(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray;
begin
  CreateEmptyArray(Result, fChannels, SampleFrames);
end;

function TDspBaseComponent.ProcessSilence(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray;
begin
  CreateEmptyArray(Result, fChannels, SampleFrames);
end;





function TDspBaseComponent.ProcessBypass(input: Single; channel: integer): Single;
begin
  Result:=input
end;

function TDspBaseComponent.ProcessBypass(input: Double; channel: integer): Double;
begin
  Result:=input
end;

function TDspBaseComponent.ProcessBypass(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray;
begin
  Result:=Copy(input);
end;

function TDspBaseComponent.ProcessBypass(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray;
begin
  Result:=Copy(input);
end;

function TDspBaseComponent.ProcessBypass(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray;
begin
  CreateArrayCopy(input, Result, fChannels, SampleFrames);
end;

function TDspBaseComponent.ProcessBypass(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray;
begin
  CreateArrayCopy(input, Result, fChannels, SampleFrames);
end;







function TDspBaseComponent.ProcessBasic(input: Double; channel: integer): Double;
var tmp: single;
begin
  tmp:=input;
  result:=fProcessS(tmp, channel)
end;

function TDspBaseComponent.ProcessBasic(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=fProcessS(input[i], channel);
end;

function TDspBaseComponent.ProcessBasic(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=fProcessD(input[i], channel);
end;

function TDspBaseComponent.ProcessBasic(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=ProcessBasic(input[i], SampleFrames, i)
end;

function TDspBaseComponent.ProcessBasic(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray;
var i: integer;
begin
  setlength(Result, length(input));
  for i:=0 to length(input)-1 do
    Result[i]:=ProcessBasic(input[i], SampleFrames, i)
end;




function TDspBaseComponent.ProcessQueueBasic(input: Single; channel: integer): Single;
begin
  Result:=fNextDspQueueItem.ProcessQueueS(fProcessS(input, channel), channel);
end;

function TDspBaseComponent.ProcessQueueBasic(input: Double; channel: integer): Double;
begin
  Result:=fNextDspQueueItem.ProcessQueueD(fProcessD(input, channel), channel);
end;

function TDspBaseComponent.ProcessQueueBasic(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSA(fProcessSA(input, SampleFrames, channel), SampleFrames, channel);
end;

function TDspBaseComponent.ProcessQueueBasic(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDA(fProcessDA(input, SampleFrames, channel), SampleFrames, channel);
end;

function TDspBaseComponent.ProcessQueueBasic(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSAA(fProcessSAA(input, SampleFrames), SampleFrames);
end;

function TDspBaseComponent.ProcessQueueBasic(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDAA(fProcessDAA(input, SampleFrames), SampleFrames);
end;




function TDspBaseComponent.ProcessQueueBypass(input: Single; channel: integer): Single;
begin
  Result:=fNextDspQueueItem.ProcessQueueS(input, channel);
end;

function TDspBaseComponent.ProcessQueueBypass(input: Double; channel: integer): Double;
begin
  Result:=fNextDspQueueItem.ProcessQueueD(input, channel);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TAVDSingleDynArray; SampleFrames: Integer; channel: integer): TAVDSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSA(input, SampleFrames, channel);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TAVDDoubleDynArray; SampleFrames: Integer; channel: integer): TAVDDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDA(input, SampleFrames, channel);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TAVDArrayOfSingleDynArray; SampleFrames: Integer): TAVDArrayOfSingleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueSAA(input, SampleFrames);
end;

function TDspBaseComponent.ProcessQueueBypass(input: TAVDArrayOfDoubleDynArray; SampleFrames: Integer): TAVDArrayOfDoubleDynArray;
begin
  Result:=fNextDspQueueItem.ProcessQueueDAA(input, SampleFrames);
end;


end.
