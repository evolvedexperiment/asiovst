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
    FBlockInBuffer     : TArrayOfSingleDynArray;
    FBlockOutBuffer    : TArrayOfSingleDynArray;
    FOnProcess         : TProcessAudioEvent;
    FOnProcessReplacing: TProcessAudioEvent;
    FOnProcessDoubles  : TProcessDoubleEvent;

    function IOChanged: Boolean; override;
    procedure SampleRateChanged; override;

    procedure DoProcessCopy(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessCopy(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessMute(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessMute(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;

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




procedure TDspVSTModule.DoProcessCopy(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
begin
  CopyArrays(Inputs, Outputs, min(FEffect.numInputs, FEffect.numOutputs), SampleFrames);
end;

procedure TDspVSTModule.DoProcessCopy(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
begin
  CopyArrays(Inputs, Outputs, min(FEffect.numInputs, FEffect.numOutputs), SampleFrames);
end;

procedure TDspVSTModule.DoProcessMute(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
begin
  ClearArrays(Outputs, FEffect.numOutputs, SampleFrames);
end;

procedure TDspVSTModule.DoProcessMute(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
begin
  ClearArrays(Outputs, FEffect.numOutputs, SampleFrames);
end;

procedure TDspVSTModule.DoBlockSaveProcess(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;

  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));

      FOnProcess(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

      for i := 0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Single));

      CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;

  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));

      FOnProcess(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

      for i := 0 to numInputs - 1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Double));

      CurrentPosition := CurrentPosition+(FBlockModeSize-FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcessReplacing(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;

  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
      for i := 0 to numOutputs - 1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));

      FOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

      for i := 0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Single));

      CurrentPosition := CurrentPosition+(FBlockModeSize-FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcessReplacing(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
  CurrentPosition := 0;
  repeat
    if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
    begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));

      FBlockPosition := FBlockPosition+(SampleFrames-CurrentPosition);
      CurrentPosition := SampleFrames;
    end else begin
      for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));
      for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));

      FOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

      for i := 0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Double));

      CurrentPosition := CurrentPosition+(FBlockModeSize-FBlockPosition);
      FBlockPosition := FBlockModeOverlap;
    end;
  until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.PrepareBlockProcessing;
begin
  if FProcessingMode=pmBlockSave then
  begin
    SetLength(fBlockInBuffer,numInputs, FBlockModeSize);
    SetLength(fBlockOutBuffer,numOutputs, FBlockModeSize);

    FBlockPosition := FBlockModeOverlap;
    if (FProcessingMode=pmBlockSave) and (FEffect.InitialDelay<FBlockModeSize-FBlockModeOverlap) then
      SetInitialDelay(FInitialDelay);
  end else begin
    SetLength(fBlockInBuffer,0,0);
    SetLength(fBlockOutBuffer,0,0);
  end;
end;

{$IFDEF CONVERT_TO_DYNARRAY}
  procedure TDspVSTModule.DoProcessDspQueue(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
  var tmpI, tmpO: TArrayOfSingleDynArray;
  begin
    if FDspQueueList.Count>0 then
    begin
      CreateArrayCopy(Inputs, tmpI, FEffect.NumOutputs, SampleFrames);
      tmpO := FDspQueueList.Items[0].ProcessQueueSAA(tmpI,SampleFrames);
      CopyArrays(tmpO, Outputs, FEffect.NumOutputs, SampleFrames);
    end else tmpO:=nil; // there was a compiler warning... stupid
  end;

  procedure TDspVSTModule.DoProcessDspQueue(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
  var tmpI, tmpO: TArrayOfDoubleDynArray;
  begin
    if FDspQueueList.Count>0 then
    begin
      CreateArrayCopy(Inputs, tmpI, FEffect.NumOutputs, SampleFrames);
      tmpO := FDspQueueList.Items[0].ProcessQueueDAA(tmpI,SampleFrames);
      CopyArrays(tmpO, Outputs, FEffect.NumOutputs, SampleFrames);
    end else tmpO:=nil; // there was a compiler warning... stupid
  end;

{$ELSE}
  procedure TDspVSTModule.DoProcessDspQueue(Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
  begin
    if FDspQueueList.Count>0 then
      outputs := FDspQueueList.Items[0].ProcessQueueSAA(Inputs,SampleFrames);
  end;

  procedure TDspVSTModule.DoProcessDspQueue(Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
  begin
    if FDspQueueList.Count>0 then
      outputs := FDspQueueList.Items[0].ProcessQueueDAA(Inputs,SampleFrames);
  end;

{$ENDIF}



procedure TDspVSTModule.SetOnProcess(v : TProcessAudioEvent);
begin
  FOnProcess := v;
  case FProcessingMode of
    pmNormal:     FOnProcessEx := FOnProcess;
    pmBlockSave:  begin
                    if Assigned(FOnProcessReplacing) then
                      FOnProcessEx := DoBlockSaveProcess
                    else
                      FOnProcessEx := FOnProcess;
                  end;
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
    pmBlockSave: begin
                   if Assigned(FOnProcessReplacing) then
                     FOnProcessReplacingEx := DoBlockSaveProcessReplacing
                   else
                     FOnProcessReplacingEx := FOnProcessReplacing;
                 end;
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
    pmBlockSave: begin
                   if Assigned(FOnProcessDoubles) then
                     FOnProcessDoublesEx := DoBlockSaveProcessReplacing
                   else
                     FOnProcessDoublesEx := FOnProcessDoubles;
                 end;
    pmCopy:      FOnProcessDoublesEx := DoProcessCopy;
    pmMute:      FOnProcessDoublesEx := DoProcessMute; 
    pmDspQueue:  FOnProcessDoublesEx := DoProcessDspQueue;
  end;
end;

procedure TDspVSTModule.SetProcessingMode(v : TProcessingMode);
begin
  if v<>fProcessingmode then
  begin
    fProcessingmode := v;
    case FProcessingMode of
      pmNormal:    begin
                     FOnProcessEx := FOnProcess;
                     FOnProcessReplacingEx := FOnProcessReplacing;
                     FOnProcessDoublesEx := FOnProcessDoubles;
                   end;
      pmBlockSave: begin
                     if Assigned(FOnProcess) then
                       FOnProcessEx := DoBlockSaveProcess
                     else
                       FOnProcessEx := FOnProcess;

                     if Assigned(FOnProcessReplacing) then
                       FOnProcessReplacingEx := DoBlockSaveProcessReplacing
                     else
                       FOnProcessReplacingEx := FOnProcessReplacing;

                     if Assigned(FOnProcessDoubles) then
                       FOnProcessDoublesEx := DoBlockSaveProcessReplacing
                     else
                       FOnProcessDoublesEx := FOnProcessDoubles;

                     PrepareBlockProcessing;
                   end;
      pmCopy:      begin
                     FOnProcessEx := DoProcessCopy;
                     FOnProcessReplacingEx := DoProcessCopy;
                     FOnProcessDoublesEx := DoProcessCopy;
                   end;
      pmMute:      begin
                     FOnProcessEx := DoProcessMute;
                     FOnProcessReplacingEx := DoProcessMute;
                     FOnProcessDoublesEx := DoProcessMute;
                   end;
      pmDspQueue:  begin
                     FOnProcessEx := DoProcessDspQueue;
                     FOnProcessReplacingEx := DoProcessDspQueue;
                     FOnProcessDoublesEx := DoProcessDspQueue;
                   end;
    end;
  end;
end;





end.
