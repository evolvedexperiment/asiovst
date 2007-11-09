unit DVSTModuleWithDsp;

interface

{$I ASIOVST.INC}

uses classes, DVSTModuleWithPrograms, DAVDProcessingComponent, DAVDCommon, DVSTCustomModule;

type
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

    procedure DoBlockSaveProcess(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessCopy(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessCopy(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;

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

uses Math;

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

procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i,j: Integer;
begin
  j := numInputs; if numOutputs < numInputs then j := numOutputs;
  for i := 0 to j-1 do
    Move(Inputs[i,0], Outputs[i,0], SampleFrames * SizeOf(Single));
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i : Integer;
begin
  for i := 0 to numOutputs - 1 do
    Fillchar(Outputs[i,0], SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var i,j: Integer;
begin
  j := numInputs;
  if numOutputs < numInputs then
    j := numOutputs;

  for i := 0 to j - 1 do
    Move(Inputs[i,0], Outputs[i,0], SampleFrames * SizeOf(Double));
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var i : Integer;
begin
  for i := 0 to numOutputs - 1 do
    Fillchar(Outputs[i,0], SampleFrames * SizeOf(Double), 0);
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
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

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
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

procedure TDspVSTModule.DoBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
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

procedure TDspVSTModule.DoBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
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
var i : Integer;
begin
  if FProcessingMode=pmBlockSave then
  begin
    SetLength(fBlockInBuffer,numInputs);
    SetLength(fBlockOutBuffer,numOutputs);

    for i := 0 to numInputs-1 do SetLength(fBlockInBuffer[i],FBlockModeSize);
    for i := 0 to numOutputs-1 do SetLength(fBlockOutBuffer[i],FBlockModeSize);

    FBlockPosition := FBlockModeOverlap;
    if (FProcessingMode=pmBlockSave) and (FEffect.InitialDelay<FBlockModeSize-FBlockModeOverlap) then
      SetInitialDelay(FInitialDelay);
  end else begin
    if Length(fBlockInBuffer)>0 then for i := 0 to Length(fBlockInBuffer)-1 do SetLength(fBlockInBuffer[i],0);
    if Length(fBlockOutBuffer)>0 then for i := 0 to Length(fBlockOutBuffer)-1 do SetLength(fBlockOutBuffer[i],0);

    SetLength(fBlockInBuffer,0);
    SetLength(fBlockOutBuffer,0);
  end;
end;

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
    end;
  end;
end;





end.
