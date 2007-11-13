unit DVSTModuleWithDsp;

interface

{$I ASIOVST.INC}

uses classes, DVSTModuleWithPrograms, DAVDProcessingComponent, DAVDCommon, DVSTCustomModule;

type
  TProcessingMode = (pmNormal, pmBlockSave, pmCopy, pmMute, pmDspQueue);

  TDspVSTModule = class(TVSTModuleWithPrograms)
  protected
    FBlockModeSize     : Integer;
    FBlockModeOverlap  : Integer;
    FProcessingMode    : TProcessingMode;
    FBlockPosition     : Integer;
    FDspQueueList      : TAVDProcessingComponentList;
    FBlockInBuffer32   : TAVDArrayOfSingleDynArray;
    FBlockOutBuffer32  : TAVDArrayOfSingleDynArray;
    FBlockInBuffer64   : TAVDArrayOfDoubleDynArray;
    FBlockOutBuffer64  : TAVDArrayOfDoubleDynArray;
    FOnProcess         : TProcessAudioEvent;
    FOnProcessReplacing: TProcessAudioEvent;
    FOnProcessDoubles  : TProcessDoubleEvent;

    function IOChanged: Boolean; override;
    procedure SampleRateChanged; override;

    procedure DoProcessCopy(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessCopy(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer); overload;

    procedure SetNumInputs(Inputs: Integer); override;
    procedure SetNumOutputs(Outputs: Integer); override;
    procedure SetInitialDelay(delay: Integer); override;

    procedure SetOnProcess(v : TProcessAudioEvent);
    procedure SetOnProcessReplacing(v : TProcessAudioEvent);
    procedure SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
    procedure SetProcessingMode(v : TProcessingMode);
    procedure PrepareBlockProcessing; virtual;
    procedure SetBlockForcedSize(v: Integer); virtual;
    procedure SetBlockOverlapSize(v: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterDSPItem(item: TAVDProcessingComponent);
    procedure UnRegisterDSPItem(item: TAVDProcessingComponent);

    property BlockSize: Integer read fBlockSize write SetBlockSize default 1024;
    property BlockModeSize: Integer read FBlockModeSize write SetBlockForcedSize default 1024;
    property BlockModeOverlap: Integer read FBlockModeOverlap write SetBlockOverlapSize default 0;
    property ProcessingMode: TProcessingMode read FProcessingMode write SetProcessingMode default pmNormal;

    property OnProcess: TProcessAudioEvent read FOnProcess write SetOnProcess;
    property OnProcessReplacing: TProcessAudioEvent read FOnProcessReplacing write SetOnProcessReplacing;
    property OnProcessDoubleReplacing: TProcessDoubleEvent read FOnProcessDoubles write SetOnProcessDoubleReplacing;
  end;

implementation

uses Math,
  {$IFDEF PUREPASCAL}DAVDBufferMathAsm{$ELSE}DAVDBufferMathPascal{$ENDIF};

constructor TDspVSTModule.Create(AOwner: TComponent);
begin
  inherited; 
  FProcessingMode := pmNormal;      
  FBlockModeSize := 1024;  
  FBlockModeOverlap := 0;
  FDspQueueList := TAVDProcessingComponentList.Create;
end;

destructor TDspVSTModule.Destroy;
begin
  FDspQueueList.Free;
  inherited;
end;

procedure TDspVSTModule.RegisterDSPItem(item: TAVDProcessingComponent);
begin
 with FDspQueueList do
  begin
   if IndexOf(item)<0 then Add(item);

   Item.SampleRate:=FSampleRate;
   Item.Channels:=max(FEffect.numInputs, FEffect.numOutputs);
  end;
end;

procedure TDspVSTModule.UnRegisterDSPItem(item: TAVDProcessingComponent);
begin
 with FDspQueueList do
  if IndexOf(item)>=0
   then Remove(item);
end;

function TDspVSTModule.IOChanged: Boolean;
begin
  Result:= inherited IOChanged;
  FDspQueueList.SetChannels(max(FEffect.numInputs, FEffect.numOutputs));
end;

procedure TDspVSTModule.SampleRateChanged;
begin
  FDspQueueList.SetSampleRate(fSampleRate);
  inherited;
end;

procedure TDspVSTModule.SetNumInputs(Inputs: Integer);
begin
  inherited;
  PrepareBlockProcessing;
end;

procedure TDspVSTModule.SetNumOutputs(Outputs: Integer);
begin
  inherited;
  PrepareBlockProcessing;
end;

procedure TDspVSTModule.SetInitialDelay(delay: Integer);
begin
  if FInitialDelay <> delay then
  begin
    FInitialDelay := delay;
    if (FProcessingMode=pmBlockSave) and (FInitialDelay<FBlockModeSize-FBlockModeOverlap) then
      FEffect.initialDelay := FBlockModeSize - FBlockModeOverlap
    else
      FEffect.initialDelay := FInitialDelay;

   if HostProduct <> 'energyXT' then IOChanged;
  end;
end;

procedure TDspVSTModule.SetBlockForcedSize(v: Integer);
begin
  if v>0 then FBlockModeSize := v;

  FBlockPosition := FBlockModeOverlap;
  PrepareBlockProcessing;
end;

procedure TDspVSTModule.SetBlockOverlapSize(v: Integer);
begin
  if v < FBlockModeSize then FBlockModeOverlap := v;

  if (FProcessingMode = pmBlockSave) and (FEffect.InitialDelay < FBlockModeSize - FBlockModeOverlap) then
    SetInitialDelay(FInitialDelay);
end;




procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 for i := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do move(Inputs[i,0], PSingle(@Outputs[i,0])^, SampleFrames * SizeOf(Single));
end;

procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 for i := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do move(Inputs[i,0], PDouble(@Outputs[i,0])^, SampleFrames * SizeOf(Double));
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 for i := 0 to FEffect.numOutputs - 1
  do FillChar(PSingle(@Outputs[i,0])^, SampleFrames * SizeOf(Single),0);
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 for i := 0 to FEffect.numOutputs - 1
  do FillChar(PDouble(@Outputs[i,0])^, SampleFrames * SizeOf(Single),0);
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;
  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition], fBlockInBuffer32[i,FBlockPosition], (SampleFrames-CurrentPosition) * Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer32[i,FBlockPosition], PSingle(@Outputs[i,CurrentPosition])^, (SampleFrames-CurrentPosition) * Sizeof(Single));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition], fBlockInBuffer32[i,FBlockPosition], (FBlockModeSize - FBlockPosition) * Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer32[i,FBlockPosition], PSingle(@Outputs[i,CurrentPosition])^, (FBlockModeSize - FBlockPosition) * Sizeof(Single));

      FOnProcess(fBlockInBuffer32, fBlockOutBuffer32, FBlockModeSize);

      for i := 0 to numInputs-1  do move(fBlockInBuffer32[i, (FBlockModeSize - FBlockModeOverlap)], fBlockInBuffer32[i,0], FBlockModeOverlap * Sizeof(Single));

      CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;

  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition], fBlockInBuffer64[i,FBlockPosition], (SampleFrames-CurrentPosition) * Sizeof(Double));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer64[i,FBlockPosition], PDouble(@Outputs[i,CurrentPosition])^, (SampleFrames-CurrentPosition) * Sizeof(Double));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition], fBlockInBuffer64[i,FBlockPosition],(FBlockModeSize-FBlockPosition) * Sizeof(Double));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer64[i,FBlockPosition], PDouble(@Outputs[i,CurrentPosition])^, (FBlockModeSize-FBlockPosition) * Sizeof(Double));

// ToDo      FOnProcessDoubles(TAVDArrayOfDoubleDynArray(fBlockInBuffer64), TAVDArrayOfDoubleDynArray(fBlockOutBuffer64), FBlockModeSize);

      for i := 0 to numInputs - 1  do move(fBlockInBuffer64[i, (FBlockModeSize - FBlockModeOverlap)], fBlockInBuffer64[i,0], FBlockModeOverlap * Sizeof(Double));

      CurrentPosition := CurrentPosition+(FBlockModeSize-FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcessReplacing(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;

  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
     begin
      for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition], fBlockInBuffer32[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer32[i,FBlockPosition], PSingle(@Outputs[i,CurrentPosition])^,(SampleFrames-CurrentPosition)*Sizeof(Single));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
     end
    else
     begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition], fBlockInBuffer32[i,FBlockPosition], (FBlockModeSize - FBlockPosition) * Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer32[i,FBlockPosition], PSingle(@Outputs[i,CurrentPosition])^, (FBlockModeSize - FBlockPosition) * Sizeof(Single));

      FOnProcessReplacing(fBlockInBuffer32, fBlockOutBuffer32, FBlockModeSize);

      for i := 0 to numInputs-1  do move(fBlockInBuffer32[i, (FBlockModeSize - FBlockModeOverlap)], fBlockInBuffer32[i,0], FBlockModeOverlap * Sizeof(Single));

      CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
     end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcessReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;
  repeat
    if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
    begin
      for i := 0 to numInputs  - 1  do move(Inputs[i, CurrentPosition], fBlockInBuffer64[i, FBlockPosition], (SampleFrames - CurrentPosition) * Sizeof(Double));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer64[i, FBlockPosition], PDouble(@Outputs[i, CurrentPosition])^, (SampleFrames - CurrentPosition) * Sizeof(Double));

      FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs  - 1 do move(Inputs[i, CurrentPosition], fBlockInBuffer64[i, FBlockPosition],(FBlockModeSize - FBlockPosition) * Sizeof(Double));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer64[i, FBlockPosition], PDouble(@Outputs[i, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * Sizeof(Double));

// ToDo      FOnProcessDoubles(TAVDArrayOfDoubleDynArray(fBlockInBuffer64), TAVDArrayOfDoubleDynArray(fBlockOutBuffer64), FBlockModeSize);

      for i := 0 to numInputs  - 1 do move(fBlockInBuffer64[i, (FBlockModeSize - FBlockModeOverlap)], fBlockInBuffer64[i, 0], FBlockModeOverlap * Sizeof(Double));

      CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.PrepareBlockProcessing;
begin
 if FProcessingMode=pmBlockSave then
  begin
   SetLength(fBlockInBuffer32,  numInputs,  FBlockModeSize);
   SetLength(fBlockOutBuffer32, numOutputs, FBlockModeSize);
   SetLength(fBlockInBuffer64,  numInputs,  FBlockModeSize);
   SetLength(fBlockOutBuffer64, numOutputs, FBlockModeSize);

   FBlockPosition := FBlockModeOverlap;
   if (FProcessingMode = pmBlockSave) and
      (FEffect.InitialDelay < FBlockModeSize - FBlockModeOverlap)
    then SetInitialDelay(FInitialDelay);
  end
 else
  begin
   SetLength(fBlockInBuffer32,  0, 0);
   SetLength(fBlockOutBuffer32, 0, 0);
   SetLength(fBlockInBuffer64,  0, 0);
   SetLength(fBlockOutBuffer64, 0, 0);
  end;
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
// if FDspQueueList.Count>0 then outputs := FDspQueueList.Items[0].ProcessQueueSAA(Inputs,SampleFrames);
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
begin
// if FDspQueueList.Count>0 then outputs := FDspQueueList.Items[0].ProcessQueueDAA(Inputs,SampleFrames);
end;

procedure TDspVSTModule.SetOnProcess(v : TProcessAudioEvent);
begin
  FOnProcess := v;
  case FProcessingMode of
    pmNormal:     FOnProcessEx := FOnProcess;
    pmBlockSave:  if Assigned(FOnProcessReplacing)
                    then FOnProcessEx := DoBlockSaveProcess
                    else FOnProcessEx := FOnProcess;
    pmCopy:       FOnProcessEx := DoProcessCopy;
    pmMute:       FOnProcessEx := DoProcessMute;
    pmDspQueue:   FOnProcessEx := DoProcessDspQueue;
  end;
end;

procedure TDspVSTModule.SetOnProcessReplacing(v : TProcessAudioEvent);
begin
  FOnProcessReplacing := v;
  case FProcessingMode of
    pmNormal:    FOnProcessReplacingEx := FOnProcessReplacing;
    pmBlockSave: if Assigned(FOnProcessReplacing)
                   then FOnProcessReplacingEx := DoBlockSaveProcessReplacing
                   else FOnProcessReplacingEx := FOnProcessReplacing;
    pmCopy:      FOnProcessReplacingEx := DoProcessCopy;
    pmMute:      FOnProcessReplacingEx := DoProcessMute;
    pmDspQueue:  FOnProcessReplacingEx := DoProcessDspQueue;
  end;
end;

procedure TDspVSTModule.SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
begin
  FOnProcessDoubles := v;
  case FProcessingMode of
    pmNormal:    FOnProcessDoublesEx := FOnProcessDoubles;
    pmBlockSave: if Assigned(FOnProcessDoubles)
                   then FOnProcessDoublesEx := DoBlockSaveProcessReplacing
                   else FOnProcessDoublesEx := FOnProcessDoubles;
    pmCopy:      FOnProcessDoublesEx := DoProcessCopy;
    pmMute:      FOnProcessDoublesEx := DoProcessMute;
    pmDspQueue:  FOnProcessDoublesEx := DoProcessDspQueue;
  end;
end;

procedure TDspVSTModule.SetProcessingMode(v : TProcessingMode);
begin
 if v <> fProcessingmode then
  begin
   fProcessingmode := v;
   case FProcessingMode of
      pmNormal:
         begin
           FOnProcessEx := FOnProcess;
           FOnProcessReplacingEx := FOnProcessReplacing;
           FOnProcessDoublesEx := FOnProcessDoubles;
         end;
      pmBlockSave:
         begin
           if Assigned(FOnProcess)
            then FOnProcessEx := DoBlockSaveProcess
            else FOnProcessEx := FOnProcess;

           if Assigned(FOnProcessReplacing)
            then FOnProcessReplacingEx := DoBlockSaveProcessReplacing
            else FOnProcessReplacingEx := FOnProcessReplacing;

           if Assigned(FOnProcessDoubles)
            then FOnProcessDoublesEx := DoBlockSaveProcessReplacing
            else FOnProcessDoublesEx := FOnProcessDoubles;

           PrepareBlockProcessing;
         end;
      pmCopy:
         begin
           FOnProcessEx := DoProcessCopy;
           FOnProcessReplacingEx := DoProcessCopy;
           FOnProcessDoublesEx := DoProcessCopy;
         end;
      pmMute:
         begin
           FOnProcessEx := DoProcessMute;
           FOnProcessReplacingEx := DoProcessMute;
           FOnProcessDoublesEx := DoProcessMute;
         end;
      pmDspQueue:
        begin
          FOnProcessEx := DoProcessDspQueue;
          FOnProcessReplacingEx := DoProcessDspQueue;
          FOnProcessDoublesEx := DoProcessDspQueue;
        end;
    end;
  end;
end;

end.
