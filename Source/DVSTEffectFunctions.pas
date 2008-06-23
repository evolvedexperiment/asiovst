unit DVSTEffectFunctions;

interface

{$I ASIOVST.INC}

uses
  DVSTEffect;

function  DispatchEffectFunc(Effect: PVSTEffect; opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
function  GetParameterFunc(Effect: PVSTEffect; Index: Integer): Single; cdecl;
procedure SetParameterFunc(Effect: PVSTEffect; Index: Integer; Value: Single); cdecl;
procedure ProcessFunc(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
procedure ProcessReplacingFunc(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
procedure ProcessDoubleReplacingFunc(Effect: PVSTEffect; Inputs, Outputs: PPDouble; SampleFrames: Integer); cdecl;

implementation

uses
  SysUtils, DVSTBasicModule;

function DispatchEffectFunc(Effect: PVSTEffect; opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
begin
  with TBasicVSTModule(Effect^.vObject) do
  begin
    HostCallDispatchEffect(opcode, Index, Value, ptr, opt);

    case opcode of
          effOpen:                      Result := HostCallOpen(Index, Value, ptr, opt);
          effClose:
              try
                HostCallClose(Index, Value, ptr, opt);
                Free;
                Result := 1;
              except
                Result := 0;
              end;
          effSetProgram:                Result := HostCallSetProgramm(Index, Value, ptr, opt);
          effGetProgram:                Result := HostCallGetProgramm(Index, Value, ptr, opt);
          effSetProgramName:            Result := HostCallSetProgramName(Index, Value, ptr, opt);
          effGetProgramName:            Result := HostCallGetProgramName(Index, Value, ptr, opt);
          effGetParamLabel:             Result := HostCallGetParamLabel(Index, Value, ptr, opt);
          effGetParamDisplay:           Result := HostCallGetParamDisplay(Index, Value, ptr, opt);
          effGetParamName:              Result := HostCallGetParamName(Index, Value, ptr, opt);
          effGetVu:                     Result := HostCallGetVu(Index, Value, ptr, opt);
          effSetSampleRate:             Result := HostCallSetSampleRate(Index, Value, ptr, opt);
          effSetBlockSize:              Result := HostCallSetBlockSize(Index, Value, ptr, opt);
          effMainsChanged:              Result := HostCallMainsChanged(Index, Value, ptr, opt);
          effEditGetRect:               Result := HostCallEditGetRect(Index, Value, ptr, opt);
          effEditOpen:                  Result := HostCallEditOpen(Index, Value, ptr, opt);
          effEditClose:                 Result := HostCallEditClose(Index, Value, ptr, opt);
          effEditDraw:                  Result := HostCallEditDraw(Index, Value, ptr, opt);
          effEditMouse:                 Result := HostCallEditMouse(Index, Value, ptr, opt);
          effEditKey:                   Result := HostCallEditKey(Index, Value, ptr, opt);
          effEditIdle:                  Result := HostCallEditIdle(Index, Value, ptr, opt);
          effEditTop:                   Result := HostCallEditTop(Index, Value, ptr, opt);
          effEditSleep:                 Result := HostCallEditSleep(Index, Value, ptr, opt);
          effIdentify:                  Result := HostCallIdentify(Index, Value, ptr, opt);
          effGetChunk:                  Result := HostCallGetChunk(Index, Value, ptr, opt);
          effSetChunk:                  Result := HostCallSetChunk(Index, Value, ptr, opt);
          effProcessEvents:             Result := HostCallProcessEvents(Index, Value, ptr, opt);
          effCanBeAutomated:            Result := HostCallCanBeAutomated(Index, Value, ptr, opt);
          effString2Parameter:          Result := HostCallString2Parameter(Index, Value, ptr, opt);
          effGetNumProgramCategories:   Result := HostCallGetNumProgramCategories(Index, Value, ptr, opt);
          effGetProgramNameIndexed:     Result := HostCallGetProgramNameIndexed(Index, Value, ptr, opt);
          effCopyProgram:               Result := HostCallCopyProgram(Index, Value, ptr, opt);
          effConnectInput:              Result := HostCallConnectInput(Index, Value, ptr, opt);
          effConnectOutput:             Result := HostCallConnectOutput(Index, Value, ptr, opt);
          effGetInputProperties:        Result := HostCallGetInputProperties(Index, Value, ptr, opt);
          effGetOutputProperties:       Result := HostCallGetOutputProperties(Index, Value, ptr, opt);
          effGetPlugCategory:           Result := HostCallGetPlugCategory(Index, Value, ptr, opt);
          effGetCurrentPosition:        Result := HostCallGetCurrentPosition(Index, Value, ptr, opt);
          effGetDestinationBuffer:      Result := HostCallGetDestinationBuffer(Index, Value, ptr, opt);
          effOfflineNotify:             Result := HostCallOfflineNotify(Index, Value, ptr, opt);
          effOfflinePrepare:            Result := HostCallOfflinePrepare(Index, Value, ptr, opt);
          effOfflineRun:                Result := HostCallOfflineRun(Index, Value, ptr, opt);
          effProcessVarIo:              Result := HostCallProcessVarIo(Index, Value, ptr, opt);
          effSetSpeakerArrangement:     Result := HostCallSetSpeakerArrangement(Index, Value, ptr, opt);
          effSetBlockSizeAndSampleRate: Result := HostCallSetBlockSizeAndSampleRate(Index, Value, ptr, opt);
          effSetBypass:                 Result := HostCallSetBypass(Index, Value, ptr, opt);
          effGetEffectName:             Result := HostCallGetEffectName(Index, Value, ptr, opt);
          effGetErrorText:              Result := HostCallGetErrorText(Index, Value, ptr, opt);
          effGetVendorString:           Result := HostCallGetVendorString(Index, Value, ptr, opt);
          effGetProductString:          Result := HostCallGetProductString(Index, Value, ptr, opt);
          effGetVendorVersion:          Result := HostCallGetVendorVersion(Index, Value, ptr, opt);
          effVendorSpecific:            Result := HostCallVendorSpecific(Index, Value, ptr, opt);
          effCanDo:                     Result := HostCallCanDo(Index, Value, ptr, opt);
          effGetTailSize:               Result := HostCallGetTailSize(Index, Value, ptr, opt);
          effIdle:                      Result := HostCallIdle(Index, Value, ptr, opt);
          effGetIcon:                   Result := HostCallGetIcon(Index, Value, ptr, opt);
          effSetViewPosition:           Result := HostCallSetViewPosition(Index, Value, ptr, opt);
          effGetParameterProperties:    Result := HostCallGetParameterProperties(Index, Value, ptr, opt);
          effKeysRequired:              Result := HostCallKeysRequired(Index, Value, ptr, opt);
          effGetVstVersion:             Result := HostCallGetVstVersion(Index, Value, ptr, opt);
          effEditKeyDown:               Result := HostCallEditKeyDown(Index, Value, ptr, opt);
          effEditKeyUp:                 Result := HostCallEditKeyUp(Index, Value, ptr, opt);
          effSetEditKnobMode:           Result := HostCallSetEditKnobMode(Index, Value, ptr, opt);
          effGetMidiProgramName:        Result := HostCallGetMidiProgramName(Index, Value, ptr, opt);
          effGetCurrentMidiProgram:     Result := HostCallGetCurrentMidiProgram(Index, Value, ptr, opt);
          effGetMidiProgramCategory:    Result := HostCallGetMidiProgramCategory(Index, Value, ptr, opt);
          effHasMidiProgramsChanged:    Result := HostCallHasMidiProgramsChanged(Index, Value, ptr, opt);
          effGetMidiKeyName:            Result := HostCallGetMidiKeyName(Index, Value, ptr, opt);
          effBeginSetProgram:           Result := HostCallBeginSetProgram(Index, Value, ptr, opt);
          effEndSetProgram:             Result := HostCallEndSetProgram(Index, Value, ptr, opt);
          effGetSpeakerArrangement:     Result := HostCallGetSpeakerArrangement(Index, Value, ptr, opt);
          effShellGetNextPlugin:        Result := HostCallShellGetNextPlugin(Index, Value, ptr, opt);
          effStartProcess:              Result := HostCallStartProcess(Index, Value, ptr, opt);
          effStopProcess:               Result := HostCallStopProcess(Index, Value, ptr, opt);
          effSetTotalSampleToProcess:   Result := HostCallSetTotalSampleToProcess(Index, Value, ptr, opt);
          effSetPanLaw:                 Result := HostCallSetPanLaw(Index, Value, ptr, opt);
          effBeginLoadBank:             Result := HostCallBeginLoadBank(Index, Value, ptr, opt);
          effBeginLoadProgram:          Result := HostCallBeginLoadProgram(Index, Value, ptr, opt);
          effSetProcessPrecision:       Result := HostCallSetProcessPrecision(Index, Value, ptr, opt);
          effGetNumMidiInputChannels:   Result := HostCallGetNumMidiInputChannels(Index, Value, ptr, opt);
          effGetNumMidiOutputChannels:  Result := HostCallGetNumMidiOutputChannels(Index, Value, ptr, opt);
          else
            try
              raise Exception.Create('unknown opcode');
            except
              Result := 0;
            end;
    end;
  end;
end;

function GetParameterFunc(Effect: PVSTEffect; Index: Integer): Single; cdecl;
begin
  Result:=TBasicVSTModule(Effect^.vObject).HostCallGetParameter(Index);
end;

procedure SetParameterFunc(Effect: PVSTEffect; Index: Integer; Value: Single); cdecl;
begin
  TBasicVSTModule(Effect^.vObject).HostCallSetParameter(Index,Value);
end;

procedure ProcessFunc(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
begin
  TBasicVSTModule(Effect^.vObject).HostCallProcess(Inputs, Outputs, SampleFrames);
end;

procedure ProcessReplacingFunc(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
begin
  TBasicVSTModule(Effect^.vObject).HostCallProcessReplacing(Inputs, Outputs, SampleFrames);
end;

procedure ProcessDoubleReplacingFunc(Effect: PVSTEffect; Inputs, Outputs: PPDouble; SampleFrames: Integer); cdecl;
begin
  TBasicVSTModule(Effect^.vObject).HostCallProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;

end.
