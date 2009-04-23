unit DAV_TestVSTHost;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Graphics, Registry, Classes, Contnrs, Windows, SysUtils,
  Messages, Dialogs, DAV_Common, DAV_VSTHost;

type
  TTestVstSuite = class(TTestSuite)
  private
    FVstPluginName : TFileName;
    procedure SetVstPluginName(const Value: TFileName);
  public
    procedure AddTests(testClass: TTestCaseClass); override;
  published
    property VstPluginName: TFileName read FVstPluginName write SetVstPluginName;
  end;


  // Test methods for class TVstHost
  TCustomTestVstPlugin = class(TTestCase)
  private
    FVstPluginName : TFileName;
    procedure SetVstPluginName(const Value: TFileName);
  protected
    FVstHost : TVstHost;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    constructor Create(MethodName: string); override;
    destructor Destroy; override;
  published
    property VstPluginName: TFileName read FVstPluginName write SetVstPluginName;
  end;

  // Basic test methods for VST Plugins
  TVstPluginBasicTests = class(TCustomTestVstPlugin)
  published
    procedure TestMultipleOpenCloseCycles;
    procedure TestMultipleInstances;
    procedure TestActiveParameterSweeps;
    procedure TestInactiveParameterSweeps;
    procedure TestActiveSamplerateChanges;
    procedure TestInactiveSamplerateChanges;
    procedure TestActiveBlocksizeChanges;
    procedure TestInactiveBlocksizeChanges;
    procedure TestInactiveProcessReplacing;
    procedure TestCanDoUnknownTokens;
    procedure TestInvalidOpcodes;
    procedure TestInvalidParameters;
    procedure TestSimpleCubaseTest;
    procedure TestDefaultCubaseTest;
  end;

  // I/O test methods for VST Plugins
  TVstPluginIOTests = class(TCustomTestVstPlugin)
  private
    FInput     : array of PDavSingleFixedArray;
    FOutput    : array of PDavSingleFixedArray;
    FBlockSize : Integer;
    procedure SetBlockSize(const Value: Integer);
    procedure SetupBuffers;
  public
    constructor Create(MethodName: string); override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHandleDataBeyondBlocksizeChanges;
    procedure TestDenormals;
    procedure TestHandleNANs;
    procedure TestProcess;
    procedure TestProcessDoubleReplacing;
    procedure TestSmallBlocksizes;

    property BlockSize: Integer read FBlockSize write SetBlockSize;
  end;

  TVSTProcessThread = class(TThread)
  private
    procedure SetBlockSize(const Value: Integer);
    procedure SetupBuffers;
  protected
    FVSTPlugin : TCustomVstPlugIn;
    FInput     : array of PDavSingleFixedArray;
    FOutput    : array of PDavSingleFixedArray;
    FBlockSize : Integer;
    FProcessedBlocks  : Integer;
    procedure Execute; override;
  public
    constructor Create(const VSTPlugin: TCustomVstPlugIn); virtual;
    destructor Destroy; override;

    property ProcessedBlocks: Integer read FProcessedBlocks;
    property BlockSize: Integer read FBlockSize write SetBlockSize;
  end;

  // I/O test methods for VST Plugins
  TVstPluginIOThreadTests = class(TCustomTestVstPlugin)
  private
    FVstProcessThread : TVSTProcessThread;
    FBlockSize        : Integer;
    procedure SetBlockSize(const Value: Integer);
  public
    constructor Create(MethodName: string); override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    property BlockSize: Integer read FBlockSize write SetBlockSize;

    procedure TestRandomParameterChangesWhileProcessing;
    procedure TestSamplerateChangesWhileProcessing;
    procedure TestProcessCallWhileProcessing;
  end;


implementation

uses
  Math, Forms, Controls, SplashScreen, DAV_VSTEffect;

function RemoveFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.')
   then Result := Copy(FileName, 0, I)
   else Result := '';
end;


{ TCustomTestVstPlugin }

constructor TCustomTestVstPlugin.Create(MethodName: string);
begin
 inherited Create(MethodName);
 FVstHost := TVstHost.Create(nil);
 if ParamStr(1) <> '' then FVstPluginName := ParamStr(1);
end;

destructor TCustomTestVstPlugin.Destroy;
begin
 FreeAndNil(FVstHost);
 inherited;
end;

procedure TCustomTestVstPlugin.SetUp;
begin
 with FVstHost.VstPlugIns.Add do
  if FileExists(FVstPluginName)
   then LoadFromFile(FVstPluginName)
   else raise Exception.Create('VST Plugin not found: ' + FVstPluginName);
end;

procedure TCustomTestVstPlugin.SetVstPluginName(const Value: TFileName);
begin
 if not FileExists(Value)
  then raise Exception.Create('Specified VST plugin does not exist: ' + Value);
 if FVstPluginName <> Value then FVstPluginName := Value;
end;

procedure TCustomTestVstPlugin.TearDown;
begin
 FVstHost.VstPlugIns.Clear;
end;


{ TVstPluginBasicTests }

procedure TVstPluginBasicTests.TestMultipleOpenCloseCycles;
var
  i : Integer;
begin
 for i := 0 to 9 do
  begin
   FVstHost[0].Active := True;
   FVstHost[0].Active := False;
  end;
end;

procedure TVstPluginBasicTests.TestSimpleCubaseTest;
begin
 with FVstHost[0] do
  begin
   if numPrograms <= 0
    then Fail('No programs found, Cubase will probably crash!');

   // get vst version
   VstDispatch(effGetVstVersion);

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 44100);

   // open
   VstDispatch(effOpen);

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo sendVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvent'));

   // get vendor string
   VstDispatch(effGetVendorString);

   // get vst version
   VstDispatch(effGetVstVersion);

   // get vendor version
   VstDispatch(effGetVendorVersion);

   // get plug category
   VstDispatch(effGetPlugCategory);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginBasicTests.TestDefaultCubaseTest;
begin
 with FVstHost[0] do
  begin
   if numPrograms <= 0
    then Fail('No programs found, Cubase will probably crash!');

   // get vst version
   VstDispatch(effGetVstVersion);

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 44100);

   // open
   VstDispatch(effOpen);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // cando

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // candos

   // program scanning

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 2048);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // start process
   VstDispatch(effStartProcess);

   // set pan law
   VstDispatch(effSetPanLaw, 0, 0, nil, sqrt(2));

   // set knob mode
   VstDispatch(effSetEditKnobMode, 0, 2);

   // get edit rect
   VstDispatch(effEditGetRect);

   // open edit
   VstDispatch(effEditOpen);

   // get edit rect
   VstDispatch(effEditGetRect);

   // stop process
   VstDispatch(effStopProcess);

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // close edit
   VstDispatch(effEditClose);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginBasicTests.TestMultipleInstances;
var
  i, j : Integer;
const
  CInstantCount = 33;
begin
 // open instances
 for i := 1 to CInstantCount do
  with FVstHost.VstPlugIns.Add do
   begin
    LoadFromFile(FVstHost[0].DLLFileName);
    Open;
   end;

 // close and delete random instances
 for i := 1 to CInstantCount do
  begin
   j := random(FVstHost.VstPlugIns.Count);
   FVstHost[j].Close;
   FVstHost.VstPlugIns.Delete(j);
  end;
end;

procedure TVstPluginBasicTests.TestActiveParameterSweeps;
var
  Param : Integer;
  Value : Single;
begin
 with FVstHost[0] do
  begin
   // active
   Active := True;
   for Param := 0 to numParams - 1 do
    begin
     Value := 0;
     while Value < 1 do
      begin
       Parameter[Param] := Value;
       Value := Value + 0.01;
      end;
    end;
   Active := False
  end;
end;

procedure TVstPluginBasicTests.TestInactiveParameterSweeps;
var
  Param : Integer;
  Value : Single;
begin
 with FVstHost[0] do
  begin
   // passive
   Active := False;
   for Param := 0 to numParams - 1 do
    begin
     Value := 0;
     while Value < 1 do
      begin
       Parameter[Param] := Value;
       Value := Value + 0.01;
      end;
    end;
  end;
end;

procedure TVstPluginBasicTests.TestInactiveProcessReplacing;
var
  Input   : array of PDavSingleFixedArray;
  Output  : array of PDavSingleFixedArray;
  Channel : Integer;
const
  CBlockSize = 8192;
begin
 // Test Inactive ProcessReplacing
 with FVstHost[0] do
  begin
   Active := True;
   SetLength(Input, numInputs);
   for Channel := 0 to numInputs - 1 do
    begin
     ReallocMem(Input[Channel], CBlockSize * SizeOf(Single));
     FillChar(Input[Channel]^, CBlockSize * SizeOf(Single), 0);
    end;

   // setup outputs
   SetLength(Output, numOutputs);
   for Channel := 0 to numOutputs - 1 do
    begin
     ReallocMem(Output[Channel], CBlockSize * SizeOf(Single));
     FillChar(Output[Channel]^, CBlockSize * SizeOf(Single), 0);
    end;

   SetSampleRate(44100);
   SetBlockSize(CBlockSize);

   StartProcess;
   Active := False;
   ProcessReplacing(@Input[0], @Output[0], CBlockSize);
   Active := True;
   StopProcess;

   Active := False;
   StartProcess;
   ProcessReplacing(@Input[0], @Output[0], CBlockSize);
   StopProcess;
  end;
end;

procedure TVstPluginBasicTests.TestActiveSamplerateChanges;
var
  d : Single;
begin
 // Test Active Samplerate Change
 FVstHost[0].Active := True;
 d := 1;
 while d <= 1411200 do
  begin
   FVstHost[0].SetSampleRate(d);
   d := d * 1.1;
  end;
end;

procedure TVstPluginBasicTests.TestInactiveSamplerateChanges;
var
  d : Single;
begin
 // Test Inactive Samplerate Change
 FVstHost[0].Active := False;
 d := 1;
 while d <= 1411200 do
  begin
   FVstHost[0].SetSampleRate(d);
   d := d * 1.1;
  end;
end;

procedure TVstPluginBasicTests.TestActiveBlocksizeChanges;
var
  i : Integer;
begin
 with FVstHost[0] do
  begin
   Active := True;

   // small block sizes
   for i := 0 to 64 do SetBlockSize(i);

   // medium odd block sizes
   for i := 1 to 64 do SetBlockSize(19 * i);

   // large odd block sizes
   for i := 1 to 64 do SetBlockSize(1025 * i);
  end;
end;

procedure TVstPluginBasicTests.TestInactiveBlocksizeChanges;
var
  i : Integer;
begin
 FVstHost[0].Active := False;

 // small block sizes
 for i := 0 to 64 do FVstHost[0].SetBlockSize(i);

 // medium odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(19 * i);

 // large odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(1025 * i);
end;

procedure TVstPluginBasicTests.TestCanDoUnknownTokens;
var
  TestCanDo : string;
  CharCnt   : Integer;
begin
 // Test empty CanDo text
 FVstHost[0].VstCanDo('');

 // Test long CanDo text
 TestCanDo := '2zcrfo3874zrbiwrbgrvsdrgbviwztvi374vöowiurzbi4t7zviw74tvposihfopit';
 FVstHost[0].VstCanDo(TestCanDo);

 // test very long CanDo text!
 TestCanDo := '';
 for CharCnt := 0 to 1111
  do TestCanDo := TestCanDo + Char(1 + Random(200));
 FVstHost[0].VstCanDo(TestCanDo);
end;

procedure TVstPluginBasicTests.TestInvalidOpcodes;
var
  i : Integer;
begin
 FVstHost[0].VstDispatch(effSetEditKnobMode, 0, 3);
 FVstHost[0].VstDispatch(effSetViewPosition, 249824962, 300013512);
 FVstHost[0].VstDispatch(effSetSpeakerArrangement);
 FVstHost[0].VstDispatch(effOfflineNotify);
 FVstHost[0].VstDispatch(effOfflinePrepare);
 FVstHost[0].VstDispatch(effOfflineRun);
 FVstHost[0].VstDispatch(effSetSampleRate, 0, 0, nil, 0);
 FVstHost[0].VstDispatch(effSetSampleRate, 0, 0, nil, -44100);

 FVstHost[0].VstDispatch(effShellGetNextPlugin);
 for i := 128 to 2000
  do FVstHost[0].VstDispatch(TDispatcherOpcode(i));
end;

procedure TVstPluginBasicTests.TestInvalidParameters;
begin
 with FVstHost[0] do
  begin
   // get invalid parameter
   GetParameter(numParams);

   // get completely invalid parameter
   GetParameter(numParams + random(MaxInt - numParams));

   // set invalid value
   SetParameter(0, 10);

   // set invalid parameter and invalid value
   SetParameter(numParams, 10);

   // set completely invalid parameter and invalid value
   SetParameter(numParams + random(MaxInt - numParams), 10);

   // set completely invalid parameter and denormal value
   SetParameter(numParams + random(MaxInt - numParams), MinSingle);

   // get invalid parameter display
   GetParamDisplay(numParams);

   // get completely invalid parameter display
   GetParamDisplay(numParams + random(MaxInt - numParams));

   // get invalid parameter label
   GetParamLabel(numParams);

   // get completely invalid parameter label
   GetParamLabel(numParams + random(MaxInt - numParams));

   // get invalid parameter name
   GetParamName(numParams);

   // get completely invalid parameter name
   GetParamName(numParams + random(MaxInt - numParams));

   // get invalid parameter properties
   GetParameterProperties(numParams);

   // get completely invalid parameter properties
   GetParameterProperties(numParams + random(MaxInt - numParams));
  end;
end;


{ TVstPluginIOTests }

function IsDenormal(const Value: Single): Boolean;
var
  IntCast     : Integer absolute Value;
begin
 result := (IntCast and $7F800000 = 0) and (IntCast and $007FFFFF <> 0);
end;

constructor TVstPluginIOTests.Create(MethodName: string);
begin
 FBlockSize := 8192;
 inherited;
end;

procedure TVstPluginIOTests.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   SetupBuffers;
  end;
end;

procedure TVstPluginIOTests.SetupBuffers;
var
  Channel : Integer;
begin
 assert(FBlocksize > 0);
 with FVstHost[0] do
  begin
   // setup inputs
   SetLength(FInput, numInputs);
   for Channel := 0 to numInputs - 1 do
    begin
     ReallocMem(FInput[Channel], FBlocksize * SizeOf(Single));
     FillChar(FInput[Channel]^, FBlocksize * SizeOf(Single), 0);
    end;

   // setup outputs
   SetLength(FOutput, numOutputs);
   for Channel := 0 to numOutputs - 1 do
    begin
     ReallocMem(FOutput[Channel], FBlocksize * SizeOf(Single));
     FillChar(FOutput[Channel]^, FBlocksize * SizeOf(Single), 0);
    end;
  end;
end;

procedure TVstPluginIOTests.SetUp;
begin
 inherited;
 SetupBuffers;

 with FVstHost[0] do
  begin
   Active := True;
   SetSampleRate(44100);
   SetBlockSize(FBlocksize);
   StartProcess;
  end;
end;

procedure TVstPluginIOTests.TearDown;
var
  Channel : Integer;
begin
 // dispose memory
 for Channel := 0 to Length(FInput)  - 1 do Dispose(FInput[Channel]);
 for Channel := 0 to Length(FOutput) - 1 do Dispose(FOutput[Channel]);

 // clear channel arrays
 SetLength(FInput, 0);
 SetLength(FOutput, 0);

 with FVstHost[0] do
  begin
   StartProcess;
   Active := False;
  end;

 inherited;
end;

procedure TVstPluginIOTests.TestDenormals;
var
  Count   : Integer;
  Channel : Integer;
  Sample  : Integer;
  IsDen   : Boolean;
  Cnt     : Integer;
begin
 with FVstHost[0] do
  begin
   // build impulse
   for Channel := 0 to numInputs - 1 do FInput[Channel, 0] := 1E-6;

   Cnt := 0;
   repeat
    // search and test for denormals
    IsDen := False;
    for Count := 0 to 8 do
     begin
      ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize);
      for Channel := 0 to Length(FInput) - 1 do FInput[Channel, 0] := 0;
      for Channel := 0 to Length(FOutput) - 1 do
       begin
        for Sample := 0 to FBlocksize - 1 do
         if IsDenormal(FOutput[Channel, Sample])
          then IsDen := True;
       end;
      if IsDen then break;
     end;
    CheckFalse(IsDen, 'Denormal found! (in program ' + IntToStr(ProgramNr));
    if numPrograms > 0
     then ProgramNr := random(numPrograms)
     else break;
    inc(Cnt);
    for Channel := 0 to Length(FInput) - 1 do FInput[Channel, 0] := 1E-6;
   until Cnt >= 4;
  end;
end;

procedure TVstPluginIOTests.TestHandleDataBeyondBlocksizeChanges;
var
  BlockSizeDecimation : Integer;
  Channel, Sample     : Integer;
begin
 with FVstHost[0] do
  begin
   // notify plugin of smaller blocksize
   SetBlockSize(FBlocksize div 10);

   for BlockSizeDecimation := 10 downto 2 do
    begin
     ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize div BlockSizeDecimation);
     for Channel := 0 to Length(FOutput) - 1 do
      for Sample := FBlocksize div 10 to FBlocksize - 1
       do CheckTrue(FOutput[Channel, Sample] = 0, 'Data was processed beyond blocksize');
    end;
  end;
end;

procedure TVstPluginIOTests.TestProcess;
var
  Channel : Integer;
  Sample  : Integer;
  Cnt     : Integer;
begin
 with FVstHost[0] do
  begin
   assert(assigned(VstEffectPointer));

   if not assigned(VstEffectPointer.ProcessDoubleReplacing)
    then Fail('Process() does not exists');

   // empty process call
   Process(nil, nil, 0);
  end;

 for Channel := 0 to Length(FInput) - 1 do
  for Sample := 0 to FBlockSize - 1
   do FInput[Channel, Sample] := 2 * random - 1;
 with FVstHost[0] do
  begin
   for Cnt := 0 to 10
    do Process(@FInput[0], @FOutput[0], random(FBlocksize));
  end;
end;

procedure TVstPluginIOTests.TestProcessDoubleReplacing;
begin
 with FVstHost[0] do
  begin
   assert(assigned(VstEffectPointer));

   if not assigned(VstEffectPointer.ProcessDoubleReplacing)
    then Fail('ProcessDoubleReplacing() does not exists');

   // empty process call
   ProcessDoubleReplacing(nil, nil, 0);

   // process data
   ProcessDoubleReplacing(@FInput[0], @FOutput[0], BlockSize div 2);
  end;
end;

procedure TVstPluginIOTests.TestHandleNANs;
var
  Channel : Integer;
  Sample  : Integer;
begin
 with FVstHost[0] do
  begin
   // build impulse
   for Channel := 0 to Length(FInput) - 1 do FInput[Channel, 0] := NaN;

   // search and test for NAN
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize);
   for Channel := 0 to numOutputs - 1 do
    begin
     for Sample := 1 to FBlocksize - 1 do
      if IsNaN(FOutput[Channel, Sample]) then
       begin
        Fail('NaN error propagation found!!!');
        Break;
       end;
     if IsNaN(FOutput[Channel, 0]) then
      begin
       Fail('NaN not handled at all!');
       Break;
      end;
    end;
  end;
end;

procedure TVstPluginIOTests.TestSmallBlocksizes;
begin
 with FVstHost[0] do
  begin
   BlockSize := 5;
   ProcessReplacing(@FInput[0], @FOutput[0], 1);
   ProcessReplacing(@FInput[0], @FOutput[0], 2);
   ProcessReplacing(@FInput[0], @FOutput[0], 3);
   ProcessReplacing(@FInput[0], @FOutput[0], 4);
   ProcessReplacing(@FInput[0], @FOutput[0], 5);
  end;
end;

{ TVSTProcessThread }

constructor TVSTProcessThread.Create(const VSTPlugin: TCustomVstPlugIn);
begin
 FVSTPlugin := VSTPlugin;
 FBlockSize := 8192;
 FProcessedBlocks := 0;
 SetupBuffers;
 inherited Create(True);
end;

destructor TVSTProcessThread.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FInput) - 1 do Dispose(FInput[Channel]);
 for Channel := 0 to Length(FOutput) - 1 do Dispose(FOutput[Channel]);

 inherited;
end;

procedure TVSTProcessThread.Execute;
begin
 repeat
  FVSTPlugin.ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize);
  inc(FProcessedBlocks)
 until Terminated;
end;

procedure TVSTProcessThread.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   SetupBuffers;
  end;
end;

procedure TVSTProcessThread.SetupBuffers;
var
  Channel : Integer;
begin
 assert(FBlocksize > 0);
 with FVSTPlugin do
  begin
   // setup inputs
   SetLength(FInput, numInputs);
   for Channel := 0 to numInputs - 1 do
    begin
     ReallocMem(FInput[Channel], FBlocksize * SizeOf(Single));
     FillChar(FInput[Channel]^, FBlocksize * SizeOf(Single), 0);
    end;

   // setup outputs
   SetLength(FOutput, numOutputs);
   for Channel := 0 to numOutputs - 1 do
    begin
     ReallocMem(FOutput[Channel], FBlocksize * SizeOf(Single));
     FillChar(FOutput[Channel]^, FBlocksize * SizeOf(Single), 0);
    end;
  end;
end;

{ TVstPluginIOThreadTests }

constructor TVstPluginIOThreadTests.Create(MethodName: string);
begin
 inherited;
 FBlockSize := 8192;
end;

procedure TVstPluginIOThreadTests.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   if assigned(FVstProcessThread)
    then FVstProcessThread.BlockSize := FBlockSize;
  end;
end;

procedure TVstPluginIOThreadTests.SetUp;
begin
 inherited;
 FVstProcessThread := TVSTProcessThread.Create(FVstHost[0]);
 FVstProcessThread.BlockSize := FBlockSize;

 with FVstHost[0] do
  begin
   Active := True;
   SetSampleRate(44100);
   SetBlockSize(FBlocksize);
   StartProcess;
  end;

 FVstProcessThread.Resume;
end;

procedure TVstPluginIOThreadTests.TearDown;
begin
 with FVstProcessThread do
  begin
   Terminate;
   Resume;
   WaitFor;
   Free;
  end;
 FVstProcessThread := nil;

 with FVstHost[0] do
  begin
   StartProcess;
   Active := False;
  end;

 inherited;
end;

procedure TVstPluginIOThreadTests.TestRandomParameterChangesWhileProcessing;
var
  ParamNo : Integer;
begin
 with FVstHost[0] do
  repeat
   for ParamNo := 0 to numParams - 1
    do Parameter[ParamNo] := random;
  until FVstProcessThread.ProcessedBlocks > 10;
end;

procedure TVstPluginIOThreadTests.TestSamplerateChangesWhileProcessing;
begin
 with FVstHost[0] do
  repeat
   SetSamplerate(10000 + random(100000));
  until FVstProcessThread.ProcessedBlocks > 4;
end;

procedure TVstPluginIOThreadTests.TestProcessCallWhileProcessing;
begin
 with FVstHost[0] do
  repeat
   Process(nil, nil, 0);
  until FVstProcessThread.ProcessedBlocks > 4;
end;

{ TTestVstSuite }

procedure TTestVstSuite.SetVstPluginName(const Value: TFileName);
begin
 if not FileExists(Value)
  then raise Exception.Create('Specified VST plugin does not exist: ' + Value);
 if FVstPluginName <> Value then
  begin
   FVstPluginName := Value;
  end;
end;

procedure TTestVstSuite.AddTests(testClass: TTestCaseClass);
var
  MethodIter       : Integer;
  NameOfMethod     : string;
  MethodEnumerator : TMethodEnumerator;
  TestCase         : TTestCase;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount-1 do
      begin
        NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
        TestCase := testClass.Create(NameOfMethod);
        (TestCase as TCustomTestVstPlugin).VstPluginName := FVstPluginName;
        self.addTest(TestCase as ITest);
      end;
  finally
    MethodEnumerator.free;
  end;
end;

procedure EnumerateVstPlugins;
var
  SR   : TSearchRec;
  TS   : TTestVstSuite;
  Hndl : HMODULE;
begin
 with TFmSplashScreen.Create(nil) do
  try
   Show;
   if FindFirst('*.dll', faAnyFile, SR) = 0 then
    try
     repeat
      try
       LbScannedPlugin.Caption := SR.Name;
//       Invalidate;
       Application.ProcessMessages;
       Hndl := LoadLibrary(PChar(SR.Name));
       if (GetProcAddress(Hndl, 'VSTMain') <> nil) or
          (GetProcAddress(Hndl, 'main') <> nil) then
        begin
         TS := TTestVstSuite.Create(SR.Name);
         TS.VstPluginName := SR.Name;
         TS.AddTests(TVstPluginBasicTests);
         TS.AddTests(TVstPluginIOTests);
         TS.AddTests(TVstPluginIOThreadTests);
         RegisterTest(TS);
        end;
      except
      end;
     until FindNext(SR) <> 0;
    finally
     // Must free up resources used by these successful finds
     FindClose(SR);
    end;
  finally
   Free;
  end;
end;

initialization
  if ParamStr(1) <> '' then
   begin
    RegisterTest(TVstPluginBasicTests.Suite);
    RegisterTest(TVstPluginIOTests.Suite);
    RegisterTest(TVstPluginIOThreadTests.Suite);

   end else EnumerateVstPlugins;

end.
