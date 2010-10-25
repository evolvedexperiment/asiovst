unit DAV_VSTModuleWithDsp;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_VSTEffect, DAV_VSTModuleWithPrograms,
  DAV_ProcessingComponent, DAV_VSTCustomModule;

type
  TProcessingMode = (pmNormal, pmBlockSave, pmCopy, pmMute, pmDspQueue);

  TDspVSTModule = class(TVSTModuleWithPrograms)
  private
    procedure ProcessingModeChanged;
    procedure OnProcessChanged;
    procedure OnProcess32ReplacingChanged;
    procedure OnProcess64ReplacingChanged;
  protected
    FBlockModeSize        : Integer;
    FBlockModeOverlap     : Integer;
    FProcessingMode       : TProcessingMode;
    FBlockPosition        : Integer;
    FDspQueueList         : TDAVProcessingComponentList;
    FBlockInBuffer32      : TDAVArrayOfSingleDynArray;
    FBlockOutBuffer32     : TDAVArrayOfSingleDynArray;
    FBlockInBuffer64      : TDAVArrayOfDoubleDynArray;
    FBlockOutBuffer64     : TDAVArrayOfDoubleDynArray;
    FOnProcess            : TProcessAudioEvent;
    FOnProcess32Replacing : TProcessAudioEvent;
    FOnProcess64Replacing : TProcessDoubleEvent;
    FDspDirectProcessItem : TDAVProcessingComponent;

    function IOChanged: Boolean; override;
    procedure SampleRateChanged; override;

    procedure DoProcessCopy(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessCopy(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessMute(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer); overload;
    procedure DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer); overload;

    procedure ProcessMidiEvent(const MidiEvent: TVstMidiEvent); override;

    procedure NumInputsChanged; override;
    procedure NumOutputsChanged; override;
    procedure InitialDelayChanged; override;

    procedure SetOnProcess(Value : TProcessAudioEvent);
    procedure SetOnProcess32Replacing(Value : TProcessAudioEvent);
    procedure SetOnProcess64Replacing(Value : TProcessDoubleEvent);
    procedure SetProcessingMode(Value : TProcessingMode);
    procedure PrepareBlockProcessing; virtual;
    procedure SetBlockForcedSize(v: Integer); virtual;
    procedure SetBlockOverlapSize(v: Integer); virtual;
    procedure SetDspDirectProcessItem(v: TDAVProcessingComponent); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterDSPItem(item: TDAVProcessingComponent);
    procedure UnRegisterDSPItem(item: TDAVProcessingComponent);

    property BlockModeSize: Integer read FBlockModeSize write SetBlockForcedSize default 1024;
    property BlockModeOverlap: Integer read FBlockModeOverlap write SetBlockOverlapSize default 0;
    property ProcessingMode: TProcessingMode read FProcessingmode write SetProcessingMode default pmNormal;

    property OnProcess: TProcessAudioEvent read FOnProcess write SetOnProcess;
    property OnProcess32Replacing: TProcessAudioEvent read FOnProcess32Replacing write SetOnProcess32Replacing;
    property OnProcess64Replacing: TProcessDoubleEvent read FOnProcess64Replacing write SetOnProcess64Replacing;
  published
    property DspDirectProcessItem: TDAVProcessingComponent read FDspDirectProcessItem write SetDspDirectProcessItem default nil;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils,
  {$IFDEF PUREPASCAL}DAV_BufferMathPascal{$ELSE}DAV_BufferMathAsm{$ENDIF};

constructor TDspVSTModule.Create(AOwner: TComponent);
begin
  inherited; 
  FProcessingmode := pmNormal;      
  FBlockModeSize := 1024;  
  FBlockModeOverlap := 0;
  FDspDirectProcessItem := nil;
  FDspQueueList := TDAVProcessingComponentList.Create;
  {$IFDEF DebugLog} AddLogMessage('After TDspVSTModule Create'); {$ENDIF}
end;

destructor TDspVSTModule.Destroy;
begin
  if Assigned(FDspQueueList) then FreeAndNil(FDspQueueList);
  inherited;
end;

procedure TDspVSTModule.RegisterDSPItem(item: TDAVProcessingComponent);
begin
 with FDspQueueList do
  begin
   if IndexOf(item) < 0 then
    begin
     Add(item);
     if (FProcessingmode = pmDspQueue) and not Assigned(FDspDirectProcessItem)
      then FDspDirectProcessItem := item;
    end;
   Item.SampleRate := FSampleRate;
   Item.Channels := max(FEffect.numInputs, FEffect.numOutputs);
  end;
end;

procedure TDspVSTModule.UnRegisterDSPItem(item: TDAVProcessingComponent);
begin
 with FDspQueueList do
  if IndexOf(item) >= 0
   then Remove(item);

 if FDspDirectProcessItem = item then
  if FDspQueueList.Count > 0
   then FDspDirectProcessItem := FDspQueueList.Items[0]
   else FDspDirectProcessItem := nil;
end;

function TDspVSTModule.IOChanged: Boolean;
begin
  Result := inherited IOChanged;
  FDspQueueList.SetChannels(max(FEffect.numInputs, FEffect.numOutputs));
end;

procedure TDspVSTModule.SampleRateChanged;
begin
  FDspQueueList.SetSampleRate(fSampleRate);
  inherited;
end;

procedure TDspVSTModule.NumInputsChanged;
begin
 inherited;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.NumOutputsChanged;
begin
 inherited;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.InitialDelayChanged;
begin
 if (FProcessingmode = pmBlockSave) and
    (FInitialDelay < FBlockModeSize - FBlockModeOverlap)
  then FEffect.initialDelay := FBlockModeSize - FBlockModeOverlap
  else FEffect.initialDelay := FInitialDelay;

 if HostProduct <> 'energyXT' then IOChanged;
end;

procedure TDspVSTModule.SetBlockForcedSize(v: Integer);
begin
 if v > 0 then FBlockModeSize := v;

 FBlockPosition := FBlockModeOverlap;
 PrepareBlockProcessing;
end;

procedure TDspVSTModule.SetBlockOverlapSize(v: Integer);
begin
 if v < FBlockModeSize then FBlockModeOverlap := v;

 if (FProcessingmode = pmBlockSave) and
    (FEffect.InitialDelay < FBlockModeSize - FBlockModeOverlap)
  then InitialDelayChanged;
end;

procedure TDspVSTModule.ProcessMidiEvent(const MidiEvent: TVstMidiEvent);
var
  tmp    : TDAVMidiEvent;
  Filter : Boolean;
begin
 with MidiEvent do
  begin
   tmp.MidiData[0]     := MidiData[0];
   tmp.MidiData[1]     := MidiData[1];
   tmp.MidiData[2]     := MidiData[2];
   tmp.DeltaFrames     := DeltaFrames;
   tmp.NoteOffset      := NoteOffset;
   tmp.NoteLength      := NoteLength;
   tmp.Detune          := Detune;
   tmp.NoteOffVelocity := NoteOffVelocity;
  end;

 Filter := False;
 FDspQueueList.ProcessMidiEventQueue(tmp, Filter);

 if not Filter then inherited;
end;


procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessCopy'); {$ENDIF}
 for Channel := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do Move(Inputs[Channel, 0], PSingle(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single));
end;

procedure TDspVSTModule.DoProcessCopy(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessCopy'); {$ENDIF}
 for Channel := 0 to min(FEffect.numInputs, FEffect.numOutputs) - 1
  do Move(Inputs[Channel, 0], PDouble(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Double));
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessMute'); {$ENDIF}
 for Channel := 0 to FEffect.numOutputs - 1
  do FillChar(PSingle(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVSTModule.DoProcessMute(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessMute'); {$ENDIF}
 for Channel := 0 to FEffect.numOutputs - 1
  do FillChar(PDouble(@Outputs[Channel, 0])^, SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoBlockSaveProcess'); {$ENDIF}
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (SampleFrames - CurrentPosition) * SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    Exit;
   end
  else
   begin
    for Channel := 0 to numInputs - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (FBlockModeSize - FBlockPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Single));

    FOnProcess(FBlockInBuffer32, FBlockOutBuffer32, FBlockModeSize);

    for Channel := 0 to numInputs - 1  do Move(FBlockInBuffer32[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer32[Channel, 0], FBlockModeOverlap * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoBlockSaveProcess'); {$ENDIF}
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition], (SampleFrames-CurrentPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (SampleFrames-CurrentPosition) * SizeOf(Double));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs-1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition],(FBlockModeSize-FBlockPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs-1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize-FBlockPosition) * SizeOf(Double));

    FOnProcess64Replacing(FBlockInBuffer64, FBlockOutBuffer64, FBlockModeSize);

    for Channel := 0 to numInputs - 1  do Move(FBlockInBuffer64[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer64[Channel, 0], FBlockModeOverlap * SizeOf(Double));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoBlockSaveProcess32Replacing'); {$ENDIF}
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition],(SampleFrames-CurrentPosition)*SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^,(SampleFrames-CurrentPosition)*SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer32[Channel, FBlockPosition], (FBlockModeSize - FBlockPosition) * SizeOf(Single));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer32[Channel, FBlockPosition], PSingle(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Single));

    FOnProcess32Replacing(FBlockInBuffer32, FBlockOutBuffer32, FBlockModeSize);

    for Channel := 0 to numInputs - 1
     do Move(FBlockInBuffer32[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer32[Channel, 0], FBlockModeOverlap * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TDspVSTModule.DoBlockSaveProcess32Replacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
  Channel         : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoBlockSaveProcess32Replacing'); {$ENDIF}
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockModeSize then
   begin
    for Channel := 0 to numInputs  - 1  do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (SampleFrames - CurrentPosition) * SizeOf(Double));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    for Channel := 0 to numInputs  - 1 do Move(Inputs[Channel, CurrentPosition], FBlockInBuffer64[Channel, FBlockPosition],(FBlockModeSize - FBlockPosition) * SizeOf(Double));
    for Channel := 0 to numOutputs - 1 do Move(FBlockOutBuffer64[Channel, FBlockPosition], PDouble(@Outputs[Channel, CurrentPosition])^, (FBlockModeSize - FBlockPosition) * SizeOf(Double));

    FOnProcess64Replacing(FBlockInBuffer64, FBlockOutBuffer64, FBlockModeSize);

    for Channel := 0 to numInputs - 1 do Move(FBlockInBuffer64[Channel, (FBlockModeSize - FBlockModeOverlap)], FBlockInBuffer64[Channel, 0], FBlockModeOverlap * SizeOf(Double));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ProcessBuffer : TDAVArrayOfSingleDynArray;
  Channel       : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessDspQueue'); {$ENDIF}
 SetLength(ProcessBuffer, max(numOutputs, numInputs), SampleFrames);
 for Channel := 0 to numInputs - 1 do Move(Inputs[Channel, 0], ProcessBuffer[Channel, 0], SampleFrames * SizeOf(Single));
  if assigned(FDspDirectProcessItem) then
   FDspDirectProcessItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
 for Channel := 0 to numOutputs - 1 do Move(ProcessBuffer[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
end;

procedure TDspVSTModule.DoProcessDspQueue(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  ProcessBuffer : TDAVArrayOfDoubleDynArray;
  Channel       : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('DoProcessDspQueue'); {$ENDIF}
 SetLength(ProcessBuffer, max(numOutputs, numInputs), SampleFrames);
 for Channel := 0 to numInputs - 1 do Move(Inputs[Channel, 0], ProcessBuffer[Channel, 0], SampleFrames * SizeOf(Double));
 if assigned(FDspDirectProcessItem) then
   FDspDirectProcessItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
 for Channel := 0 to numOutputs - 1 do Move(ProcessBuffer[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Double));
end;

procedure TDspVSTModule.PrepareBlockProcessing;
begin
 if FProcessingmode = pmBlockSave then
  begin
   SetLength(FBlockInBuffer32,  numInputs,  FBlockModeSize);
   SetLength(FBlockOutBuffer32, numOutputs, FBlockModeSize);
   SetLength(FBlockInBuffer64,  numInputs,  FBlockModeSize);
   SetLength(FBlockOutBuffer64, numOutputs, FBlockModeSize);

   FBlockPosition := FBlockModeOverlap;
   if (FProcessingmode = pmBlockSave) and
      (FEffect.InitialDelay < FBlockModeSize - FBlockModeOverlap)
    then InitialDelayChanged;
  end
 else
  begin
   SetLength(FBlockInBuffer32,  0, 0);
   SetLength(FBlockOutBuffer32, 0, 0);
   SetLength(FBlockInBuffer64,  0, 0);
   SetLength(FBlockOutBuffer64, 0, 0);
  end;
end;


procedure TDspVSTModule.SetOnProcess(Value : TProcessAudioEvent);
begin
 if @FOnProcess <> @Value then
  begin
   FOnProcess := Value;
   OnProcessChanged;
  end;
end;

procedure TDspVSTModule.SetOnProcess32Replacing(Value : TProcessAudioEvent);
begin
 if @FOnProcess32Replacing <> @Value then
  begin
   FOnProcess32Replacing := Value;
   OnProcess32ReplacingChanged;
  end;
end;

procedure TDspVSTModule.SetOnProcess64Replacing(Value : TProcessDoubleEvent);
begin
 if @FOnProcess64Replacing <> @Value then
  begin
   FOnProcess64Replacing := Value;
   OnProcess64ReplacingChanged;
  end;
end;

procedure TDspVSTModule.SetProcessingMode(Value : TProcessingMode);
begin
 if Value <> FProcessingmode then
  begin
   FProcessingmode := Value;
   ProcessingModeChanged;
  end;
end;

procedure TDspVSTModule.OnProcessChanged;
begin
 case FProcessingMode of
   pmNormal:     FOnProcessEx := FOnProcess;
   pmBlockSave:  if Assigned(FOnProcess32Replacing)
                   then FOnProcessEx := DoBlockSaveProcess
                   else FOnProcessEx := FOnProcess;
   pmCopy:       FOnProcessEx := DoProcessCopy;
   pmMute:       FOnProcessEx := DoProcessMute;
   pmDspQueue:   FOnProcessEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.OnProcess32ReplacingChanged;
begin
 case FProcessingMode of
   pmNormal:    FOnProcess32ReplacingEx := FOnProcess32Replacing;
   pmBlockSave: if Assigned(FOnProcess32Replacing)
                  then FOnProcess32ReplacingEx := DoBlockSaveProcess32Replacing
                  else FOnProcess32ReplacingEx := FOnProcess32Replacing;
   pmCopy:      FOnProcess32ReplacingEx := DoProcessCopy;
   pmMute:      FOnProcess32ReplacingEx := DoProcessMute;
   pmDspQueue:  FOnProcess32ReplacingEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.OnProcess64ReplacingChanged;
begin
 case FProcessingMode of
   pmNormal:    FOnProcess64ReplacingEx := FOnProcess64Replacing;
   pmBlockSave: if Assigned(FOnProcess64Replacing)
                  then FOnProcess64ReplacingEx := DoBlockSaveProcess32Replacing
                  else FOnProcess64ReplacingEx := FOnProcess64Replacing;
   pmCopy:      FOnProcess64ReplacingEx := DoProcessCopy;
   pmMute:      FOnProcess64ReplacingEx := DoProcessMute;
   pmDspQueue:  FOnProcess64ReplacingEx := DoProcessDspQueue;
 end;
end;

procedure TDspVSTModule.ProcessingModeChanged;
begin
 case FProcessingMode of
    pmNormal:
       begin
         FOnProcessEx := FOnProcess;
         FOnProcess32ReplacingEx := FOnProcess32Replacing;
         FOnProcess64ReplacingEx := FOnProcess64Replacing;
       end;
    pmBlockSave:
       begin
         if Assigned(FOnProcess)
          then FOnProcessEx := DoBlockSaveProcess
          else FOnProcessEx := FOnProcess;

         if Assigned(FOnProcess32Replacing)
          then FOnProcess32ReplacingEx := DoBlockSaveProcess32Replacing
          else FOnProcess32ReplacingEx := FOnProcess32Replacing;

         if Assigned(FOnProcess64Replacing)
          then FOnProcess64ReplacingEx := DoBlockSaveProcess32Replacing
          else FOnProcess64ReplacingEx := FOnProcess64Replacing;

         PrepareBlockProcessing;
       end;
    pmCopy:
       begin
         FOnProcessEx := DoProcessCopy;
         FOnProcess32ReplacingEx := DoProcessCopy;
         FOnProcess64ReplacingEx := DoProcessCopy;
       end;
    pmMute:
       begin
         FOnProcessEx := DoProcessMute;
         FOnProcess32ReplacingEx := DoProcessMute;
         FOnProcess64ReplacingEx := DoProcessMute;
       end;
    pmDspQueue:
      begin
        FOnProcessEx := DoProcessDspQueue;
        FOnProcess32ReplacingEx := DoProcessDspQueue;
        FOnProcess64ReplacingEx := DoProcessDspQueue;
      end;
 end;
end;

procedure TDspVSTModule.SetDspDirectProcessItem(v: TDAVProcessingComponent);
begin
  if v <> FDspDirectProcessItem then
  begin
    if v = nil then
      FDspDirectProcessItem := v
    else if FDspQueueList.IndexOf(v) >= 0 then
    begin
      FDspDirectProcessItem := v;
      SetProcessingMode(pmDspQueue);
    end else
      raise Exception.Create('DspDirectProcessItem has to be the first item of a queue');
  end;
end;

end.
