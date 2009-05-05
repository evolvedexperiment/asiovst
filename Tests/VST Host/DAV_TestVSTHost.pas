unit DAV_TestVSTHost;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

{$DEFINE NoInvalidOpcodes}

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
    procedure TestMultipleInstances;
    procedure TestActiveParameterSweeps;
    procedure TestActiveSamplerateChanges;
    procedure TestActiveBlocksizeChanges;
    procedure TestPrograms;
  end;

  // Perverse test methods for VST Plugins
  TVstPluginPerverseTests = class(TCustomTestVstPlugin)
  published
    procedure TestMultipleOpenCloseCycles;
    procedure TestInactiveParameterSweeps;
    procedure TestInactiveSamplerateChanges;
    procedure TestInactiveBlocksizeChanges;
    procedure TestInactiveProcessReplacing;
    procedure TestCanDoUnknownTokens;
    procedure TestInvalidOpcodes;
    procedure TestInvalidParameters;
  end;

  // Test methods for VST Plugins for various hosts
  TVstPluginHostTests = class(TCustomTestVstPlugin)
  published
    procedure TestCubaseScan;
    procedure TestCubase;
    procedure TestReloadPluginCubase;
    procedure TestFL8FastScan;
    procedure TestFL8;
    procedure TestTracktion2;
    procedure TestTracktion2Scan;
    procedure TestReaper;
    procedure TestMULAB;
    procedure TestCantabile;
    procedure TestSamplitude;
    procedure TestAbletonLiveScan;
    procedure TestAbletonLive;
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
  Math, Forms, Controls, {$IFNDEF CONSOLE_TESTRUNNER} SplashScreen, {$ENDIF}
  DAV_VSTEffect;

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
 with FVstHost do
  begin
   VendorString := 'Delphi ASIO & VST Project';
   ProductString := 'Delphi ASIO & VST Project';

   with VstPlugIns.Add do
    if FileExists(FVstPluginName)
     then LoadFromFile(FVstPluginName)
     else raise Exception.Create('VST Plugin not found: ' + FVstPluginName);
  end;
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

procedure TVstPluginBasicTests.TestPrograms;
var
  i   : Integer;
  str : string;
  MS  : TMemoryStream;
  MPN : TMidiProgramName;
begin
 with FVstHost[0] do
  begin
   Active := True;
   for i := 0 to 999 do
    begin
     ProgramNr := random(numPrograms);
     Parameter[random(numParams)] := random;
     GetParamName(random(numParams));
     GetParamLabel(random(numParams));
     GetParamDisplay(random(numParams));
     String2Parameter(random(numParams), str);
     SetLength(str, 20);
     SetProgramName(str);
     CopyCurrentProgramTo(random(numPrograms));
     GetMidiProgramName(MPN);
    end;

   for i := 0 to 9 do
    begin
     MS := TMemoryStream.Create;
     try
      SaveBank(MS);
      MS.Position := 0;
      LoadBank(MS);
     finally
      FreeAndNil(MS);
     end;
    end;

   Active := False;
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


{ TVstPluginPerverseTests }

procedure TVstPluginPerverseTests.TestInactiveBlocksizeChanges;
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

 // activate and directly deactivate
 with FVstHost[0] do
  begin
   Active := True;
   Active := False;
  end;

 // small block sizes
 for i := 0 to 64 do FVstHost[0].SetBlockSize(i);

 // medium odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(19 * i);

 // large odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(1025 * i);
end;

procedure TVstPluginPerverseTests.TestCanDoUnknownTokens;
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

procedure TVstPluginPerverseTests.TestInvalidOpcodes;
var
  i  : Integer;
begin
 with FVstHost[0] do
  for i := 0 to 1 do
   begin
    VstDispatch(effSetEditKnobMode, 0, 3);
    VstDispatch(effSetViewPosition, 249824962, 300013512);
    VstDispatch(effSetSpeakerArrangement);
    VstDispatch(effOfflineNotify);
    VstDispatch(effOfflinePrepare);
    VstDispatch(effOfflineRun);
    VstDispatch(effSetSampleRate);
    VstDispatch(effSetSampleRate, 0, 0, nil, -44100);
    VstDispatch(effEditGetRect);
    VstDispatch(effEditIdle);
    VstDispatch(effEditTop);
    VstDispatch(effEditSleep);
    VstDispatch(effEditDraw);
    VstDispatch(effEditClose);
    VstDispatch(effEditOpen);
    VstDispatch(effEditGetRect);
    VstDispatch(effProcessEvents);
    VstDispatch(effGetProgramNameIndexed, numPrograms);
    VstDispatch(effGetVendorString);
    VstDispatch(effGetProductString);
    VstDispatch(effGetParamLabel);
    VstDispatch(effGetParamName);
    VstDispatch(effGetParamDisplay);
    VstDispatch(effGetProductString);
    VstDispatch(effShellGetNextPlugin);
    VstDispatch(effBeginLoadBank);
    VstDispatch(effGetParameterProperties);
    VstDispatch(effOpen);
    CheckTrue(TChunkName(VstDispatch(effIdentify)) = 'fEvN',
      'effIdentify didn''t return NvEf');
   end;

{$IFNDEF NoInvalidOpcodes}
 for i := 128 to 2000
  do FVstHost[0].VstDispatch(TDispatcherOpcode(i));
{$ENDIF}

 with FVstHost[0] do
  begin
   VstDispatch(effClose);
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginPerverseTests.TestMultipleOpenCloseCycles;
var
  i : Integer;
begin
 for i := 0 to 9 do
  with FVstHost[0] do
   begin
    Active := True;
    Active := False;
   end;
end;

procedure TVstPluginPerverseTests.TestInactiveSamplerateChanges;
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

 // activate and directly deactivate
 with FVstHost[0] do
  begin
   Active := True;
   Active := False;
  end;

 d := 1;
 while d <= 1411200 do
  begin
   FVstHost[0].SetSampleRate(d);
   d := d * 1.1;
  end;
end;

procedure TVstPluginPerverseTests.TestInactiveParameterSweeps;
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
       GetParameter(Param);
       Value := Value + 0.01;
      end;
    end;

   // passive, active, passive
   Active := True;
   Active := False;
   for Param := 0 to numParams - 1 do
    begin
     Value := 0;
     while Value < 1 do
      begin
       Parameter[Param] := Value;
       GetParameter(Param);
       Value := Value + 0.01;
      end;
    end;
  end;
end;

procedure TVstPluginPerverseTests.TestInactiveProcessReplacing;
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

procedure TVstPluginPerverseTests.TestInvalidParameters;
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


{ TVstPluginHostTests }

procedure TVstPluginHostTests.TestFL8FastScan;
var
  Data : PChar;
begin
 FVstHost.VendorString := 'Image-Line';
 FVstHost.ProductString := 'Fruity Wrapper';

 with FVstHost[0] do
  begin
   if numPrograms <= 0
    then Fail('No programs found, Cubase will probably crash!');

   // open
   VstDispatch(effOpen);

   GetMem(Data, 1024);
   try
    // get plugin category
    VstDispatch(effGetPlugCategory);

    // get vendor string
    VstDispatch(effGetVendorString, 0, 0, Data);

    // get vendor string
    VstDispatch(effGetEffectName, 0, 0, Data);

   finally
    Dispose(Data);
   end;

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestFL8;
var
  i    : Integer;
  Data : PChar;
  rct  : TRect;
  prct : PRect;
  pp   : TVstPinProperties;
  ve   : TVstEvents;
begin
 FVstHost.VendorString := 'Image-Line';
 FVstHost.ProductString := 'Fruity Wrapper';

 with FVstHost[0] do
  begin
   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 7984);

   // open plugin
   VstDispatch(effOpen);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 7984);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   GetMem(Data, 1024);
   try
    // get vendor string
    VstDispatch(effGetEffectName, 0, 0, Data);

    // set edit knob mode
    VstDispatch(effSetEditKnobMode, 0, 2);

    // get program
    VstDispatch(effGetProgram);

    // get input properties
    for i := 0 to numInputs - 1 do
     if VstDispatch(effGetInputProperties, i, 0, @pp) <> 0 then
      begin
       CheckTrue(pp.Caption[63] = #0, 'effGetInputProperties: Caption probably too long');
      end;

    // get output properties
    for i := 0 to numOutputs - 1 do
     if VstDispatch(effGetOutputProperties, 0, 0, @pp) <> 0 then
      begin
       CheckTrue(pp.Caption[63] = #0, 'effGetOutputProperties: Caption probably too long');
      end;

    // Get Program Name Indexed
    VstDispatch(effGetProgramNameIndexed, 0, -1, Data);

    // CanDo receiveVstMidiEvent
    VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

    // switch off
    VstDispatch(effMainsChanged, 0, 0);

    // set blocksize
    VstDispatch(effSetBlockSize, 0, 7984);

    // switch on
    VstDispatch(effMainsChanged, 0, 1);

    // Get Program Name Indexed
    VstDispatch(effGetProgramNameIndexed, 0, -1);

    // Get Program Name Indexed
    VstDispatch(effGetProgramNameIndexed, 1, -1);

    // get vendor string
    VstDispatch(effGetVendorString, 0, 0, Data);

    // Open Editor
    with TForm.Create(nil) do
     try
      VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

      // Get Editor Rect
      prct := @rct;
      VstDispatch(effEditGetRect, 0, 0, @prct);

      // set bounds
      SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

      // repaint
      Repaint;
      Application.ProcessMessages;

      VstDispatch(effEditGetRect, 0, 0, @prct);

      VstDispatch(effEditGetRect, 0, 0, @prct);

      // process events
      FillChar(ve, SizeOf(TVstEvents), 0);
      VstDispatch(effProcessEvents, 0, 0, @ve);

      VstDispatch(effProcessEvents, 0, 0, @ve);

      // switch off
      VstDispatch(effMainsChanged, 0, 0);

      // switch on
      VstDispatch(effMainsChanged, 0, 1);

      // switch off
      VstDispatch(effMainsChanged, 0, 0);

      // get vendor string
      VstDispatch(effGetVendorString, 0, 0, Data);

      // edit close
      VstDispatch(effEditClose);
    finally
     Free;
    end;

   finally
    Dispose(Data);
   end;

   // close plugin
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestMULAB;
var
  rct  : TRect;
  prct : PRect;
  pp   : TVstPinProperties;
  ve   : TVstEvents;
begin
 FVstHost.VendorString := 'MUTOOLS.com';
 FVstHost.ProductString := 'MU.LAB';

 with FVstHost[0] do
  begin
   // check identify is fEvN
   CheckTrue(TChunkName(VstDispatch(effIdentify)) = 'fEvN',
     'effIdentify didn''t return NvEf');

   // get vst version
   VstDispatch(effGetVstVersion);

   // open plugin
   VstDispatch(effOpen);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 256);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // set program
   VstDispatch(effSetProgram);

   // set edit knob mode
   VstDispatch(effSetEditKnobMode, 0, 3);

   // idle
   VstDispatch(effIdle, 0, 3);

   // get input properties
   if VstDispatch(effGetInputProperties, 0, 0, @pp) <> 0 then
    begin
     CheckTrue(pp.Caption[63] = #0, 'effGetInputProperties: Caption probably too long');
    end;

   // get output properties
   if VstDispatch(effGetOutputProperties, 0, 0, @pp) <> 0 then
    begin
     CheckTrue(pp.Caption[63] = #0, 'effGetOutputProperties: Caption probably too long');
    end;

   // get vst version
   VstDispatch(effGetVstVersion);

   // process events
   FillChar(ve, SizeOf(TVstEvents), 0);
   VstDispatch(effProcessEvents, 0, 0, @ve);

   // edit open
   with TForm.Create(nil) do
    try
     VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

     // process events
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // Get Editor Rect
     prct := @rct;
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // set bounds
     SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

     // repaint
     Repaint;
     Application.ProcessMessages;

     // process events
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // edit draw
     VstDispatch(effEditDraw, 0, 0, @prct);

     // edit idle
     VstDispatch(effEditIdle);

     // process events
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // close
     VstDispatch(effEditClose);
    finally
     Free;
    end;

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestReaper;
var
  i    : Integer;
  Data : PChar;
  rct  : TRect;
  prct : PRect;
begin
 FVstHost.VendorString := 'Cockos';
 FVstHost.ProductString := 'REAPER';

 with FVstHost[0] do
  begin
   // open plugin
   VstDispatch(effOpen);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   GetMem(Data, 1024);
   try
    // get vendor string
    VstDispatch(effGetEffectName, 0, 0, Data);

    // get vendor string
    VstDispatch(effGetVendorString, 0, 0, Data);

   finally
    Dispose(Data);
   end;

   // CanDo hasCockosExtensions
   VstDispatch(effCanDo, 0, 0, PChar('hasCockosExtensions'));

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // start process
   VstDispatch(effStartProcess);

   // CanDo receiveVstEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstEvent'));

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // get plugin category
   VstDispatch(effGetPlugCategory);

   // CanDo sendVstEvents
   VstDispatch(effCanDo, 0, 0, PChar('sendVstEvents'));

   // Idle
   VstDispatch(effIdle);

   // Get Program
   VstDispatch(effGetProgram);

   // Get Program
   VstDispatch(effGetProgram, 0, -1);

   GetMem(Data, 1024);
   try
    // Get Program Name Indexed
    for i := 0 to numPrograms - 1
     do VstDispatch(effGetProgramNameIndexed, 0, -1, Data);
   finally
    Dispose(Data);
   end;

   // Get Editor Rect
   prct := @rct;
   VstDispatch(effEditGetRect, 0, 0, @prct);

   // Open Editor
   with TForm.Create(nil) do
    try
     VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

     // Get Editor Rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // set bounds
     SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

     // repaint
     Repaint;
     Application.ProcessMessages;

     // Get Program
     VstDispatch(effGetProgram);

     // Editor Idle
     VstDispatch(effIdle);

     // Idle
     VstDispatch(effIdle);

     // Editor Idle
     VstDispatch(effIdle);

     // Idle
     VstDispatch(effIdle);

     // Get Editor Rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // edit close
     VstDispatch(effEditClose);
    finally
     Free;
    end;

   // stop process
   VstDispatch(effStopProcess);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestSamplitude;
var
  i    : Integer;
  prct : PRect;
  rct  : TRect;
  Data : PChar;
begin
 FVstHost.VendorString := 'MAGIX';
 FVstHost.ProductString := 'Samplitude';

 with FVstHost[0] do
  begin
   // open
   VstDispatch(effOpen);

   // set program
   VstDispatch(effSetProgram);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 4096);

   GetMem(Data, 1024);
   try
    // get vendor string
    VstDispatch(effGetVendorString, 0, 0, Data);

    // get vendor version
    VstDispatch(effGetVstVersion);

    // CanDo receiveVstMidiEvent
    VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

    // set program
    VstDispatch(effSetProgram);

    // set samplerate
    VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

    // set blocksize
    VstDispatch(effSetBlockSize, 0, 4096);

    // Get Program Name Indexed
    VstDispatch(effGetProgramNameIndexed, 0, -1, Data);
   finally
    Dispose(Data);
   end;

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // set edit knob mode
   VstDispatch(effSetEditKnobMode);

   // Get Editor Rect
   prct := @rct;
   VstDispatch(effEditGetRect, 0, 0, @prct);

   with TForm.Create(nil) do
    try
     VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

     // edit top
     VstDispatch(effEditTop);

     GetMem(Data, 1024);
     try
      // get parameter name
      for i := 0 to numParams - 1
       do VstDispatch(effGetParamName, i, 0, Data);

      for i := 0 to numParams - 1 do
       begin
        // get param name
        VstDispatch(effGetParamDisplay, i, 0, Data);

        // get param label
        VstDispatch(effGetParamLabel, i, 0, Data);

        // check can be automated
        VstDispatch(effCanBeAutomated, i);
       end;
     finally
      Dispose(Data);
     end;

     // get parameter
     for i := 0 to numParams - 1
      do GetParameter(i);

     // get editor rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // get editor rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // idle
     VstDispatch(effEditDraw);

     // idle
     VstDispatch(effIdle);

     // editor idle
     VstDispatch(effEditIdle);

     // switch off
     VstDispatch(effMainsChanged, 0, 0);

     // close editor
     VstDispatch(effEditClose);
    finally
     Free;
    end;

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestTracktion2;
var
  i, j : Integer;
  pp   : TVstPinProperties;
  ve   : TVstEvents;
  Data : PChar;
  prct : PRect;
  rct  : TRect;
begin
 FVstHost.VendorString := '';
 FVstHost.ProductString := 'Tracktion 2';

 with FVstHost[0] do
  begin
   CheckTrue(TChunkName(VstDispatch(effIdentify)) = 'fEvN',
     'effIdentify didn''t return NvEf');

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 2048);

   // open
   VstDispatch(effOpen);

   // set program
   VstDispatch(effSetProgram);

   // get program
   VstDispatch(effGetProgram);

   // get input connected
   for i := numInputs - 1 downto 0
    do VstDispatch(effConnectInput, i);

   // get output connected
   for i := numOutputs - 1 downto 0
    do VstDispatch(effConnectOutput, i);

   // get output properties
   for i := 0 to numOutputs - 1 do
    if VstDispatch(effGetOutputProperties, 0, 0, @pp) <> 0 then
     begin
      CheckTrue(pp.Caption[63] = #0, 'effGetOutputProperties: Caption probably too long');
     end;

   GetMem(Data, 1024);
   try
    for i := 0 to numParams - 1 do
     begin
      // get parameter name
      VstDispatch(effGetParamName, i, 0, Data);

      // get parameter name
      VstDispatch(effCanBeAutomated, i);

      GetParameter(i);
     end;
   finally
    Dispose(Data);
   end;

   GetMem(Data, 1024);
   try
    // get program name indexed
    VstDispatch(effGetProgramNameIndexed, 0, -1, Data);

    for j := 0 to numPrograms - 1 do
     begin
      // get/set program
      if VstDispatch(effGetProgram) <> j
       then VstDispatch(effSetProgram, 0, j);

      // get program
      VstDispatch(effGetProgram);

      // get program name
      VstDispatch(effGetProgramName, 0, 0, Data);

      // get program
      VstDispatch(effGetProgram);

      for i := 0 to numParams - 1
       do GetParameter(i);
     end;

    // get/set program
    if VstDispatch(effGetProgram) <> 0
     then VstDispatch(effSetProgram);

    // get program
    VstDispatch(effGetProgram);

    for i := 0 to numParams - 1
     do GetParameter(i);

    // get program name
    VstDispatch(effGetProgramName, 0, 0, Data);

   finally
    Dispose(Data);
   end;

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // get plugin category
   VstDispatch(effGetPlugCategory);

   // get plugin category
   VstDispatch(effGetPlugCategory);

   // get program
   for j := 0 to numPrograms - 1
    do VstDispatch(effGetProgram);

   // get editor rect
   prct := @rct;
   VstDispatch(effEditGetRect, 0, 0, @prct);

   with TForm.Create(nil) do
    try
     // open editor
     VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

     // get editor rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // set bounds
     SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

     // repaint
     Repaint;
     Application.ProcessMessages;

     // get program
     VstDispatch(effGetProgram);

     // keys required
     VstDispatch(effKeysRequired);

     // edit idle
     VstDispatch(effEditIdle);

     // switch on
     VstDispatch(effMainsChanged, 0, 1);

     // start process
     VstDispatch(effStartProcess);

     // process events
     FillChar(ve, SizeOf(TVstEvents), 0);
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // edit idle
     VstDispatch(effEditIdle);

     // process events
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // get program
     VstDispatch(effGetProgram);

     // get program
     VstDispatch(effGetProgram);

     GetMem(Data, 1024);
     try
      // get program name
      VstDispatch(effGetProgramName, 0, 0, Data);
     finally
      Dispose(Data)
     end;

     // process events
     VstDispatch(effProcessEvents, 0, 0, @ve);

     // stop processing
     VstDispatch(effStopProcess);

     // close editor
     VstDispatch(effEditClose);

    finally
     Free;
    end;

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestTracktion2Scan;
begin
 FVstHost.VendorString := '';
 FVstHost.ProductString := 'Tracktion 2';

 with FVstHost[0] do
  begin
   VstDispatch(effGetPlugCategory);
   CheckTrue(TChunkName(VstDispatch(effIdentify)) = 'fEvN',
     'effIdentify didn''t return NvEf');
   VstDispatch(effOpen);
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestCubaseScan;
var
  Data : PChar;
begin
 FVstHost.VendorString := 'Steinberg';
 FVstHost.ProductString := 'Cubase VST';

 with FVstHost[0] do
  begin
   // get vst version
   VstDispatch(effGetVstVersion);

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // open
   VstDispatch(effOpen);

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo sendVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvent'));

   GetMem(Data, 1024);
   try
    // get vendor string
    VstDispatch(effGetVendorString, 0, 0, Data);

    // get vst version
    VstDispatch(effGetVstVersion);

    // get vendor version
    VstDispatch(effGetVendorVersion);
   finally
    Dispose(Data);
   end;

   // get plug category
   VstDispatch(effGetPlugCategory);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestAbletonLive;
var
  i, j : Integer;
  pp   : TVstPinProperties;
  Data : PChar;
  prct : PRect;
  rct  : TRect;
begin
 FVstHost.VendorString := 'Ableton';
 FVstHost.ProductString := 'Live';

 with FVstHost[0] do
  begin
   // get vst version
   VstDispatch(effGetVstVersion);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 128);

   // open
   VstDispatch(effOpen);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 128);

   // CanDo sendVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvent'));

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo midiProgramNames
   VstDispatch(effCanDo, 0, 0, PChar('midiProgramNames'));

   // get input properties
   for i := 0 to numInputs - 1 do
    if VstDispatch(effGetInputProperties, i, 0, @pp) <> 0 then
     begin
      CheckTrue(pp.Caption[63] = #0, 'effGetInputProperties: Caption probably too long');
     end;

   // get output properties
   for i := 0 to numOutputs - 1 do
    if VstDispatch(effGetOutputProperties, 0, 0, @pp) <> 0 then
     begin
      CheckTrue(pp.Caption[63] = #0, 'effGetOutputProperties: Caption probably too long');
     end;

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // get input properties
   for i := 0 to numInputs - 1 do
    if VstDispatch(effGetInputProperties, i, 0, @pp) <> 0 then
     begin
      CheckTrue(pp.Caption[63] = #0, 'effGetInputProperties: Caption probably too long');
     end;

   // get output properties
   for i := 0 to numOutputs - 1 do
    if VstDispatch(effGetOutputProperties, 0, 0, @pp) <> 0 then
     begin
      CheckTrue(pp.Caption[63] = #0, 'effGetOutputProperties: Caption probably too long');
     end;

   // get midi input channel count
   VstDispatch(effGetNumMidiInputChannels);

   // get midi output channel count
   VstDispatch(effGetNumMidiOutputChannels);

   // get program
   VstDispatch(effGetProgram);

   GetMem(Data, 1024);
   try

    // get program name indexed
    for j := 0 to numPrograms - 1
     do VstDispatch(effGetProgramNameIndexed, j, -1, Data);

    // get program
    VstDispatch(effGetProgram);

    for j := 0 to numParams - 1 do
     begin
      // get parameter
      GetParameter(j);

      // get parameter name
      VstDispatch(effGetParamName, j, 0, Data);

      // get parameter display
      VstDispatch(effGetParamDisplay, j, 0, Data);

      // get parameter label
      VstDispatch(effGetParamLabel, j, 0, Data);

      // get parameter can be automated
      VstDispatch(effCanBeAutomated, j, 0, Data);
     end;

    with TForm.Create(nil) do
     try
      // get program
      VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

      // set edit knob mode
      VstDispatch(effSetEditKnobMode, 0, 2);

      // get editor rect
      prct := @rct;
      VstDispatch(effEditGetRect, 0, 0, @prct);

      // get program
      VstDispatch(effGetProgram);

      // set program
      VstDispatch(effSetProgram);

      // get program name
      VstDispatch(effGetProgramName, 0, 0, Data);

      for j := 0 to numPrograms - 1 do
       begin
        // set program
        VstDispatch(effSetProgram, 0, j);

        // get program name
        VstDispatch(effGetProgramName, 0, 0, Data);

        // get parameter
        for i := 0 to numParams - 1
         do GetParameter(i);
       end;

       // set program
       VstDispatch(effSetProgram);

       // get program
       VstDispatch(effGetProgram);

       // switch on
       VstDispatch(effMainsChanged, 0, 1);

       // start process
       VstDispatch(effStartProcess);

       // edit idle
       VstDispatch(effEditIdle);

       // set program
       VstDispatch(effSetProgram);

       // stop process
       VstDispatch(effStopProcess);

       // edit close
       VstDispatch(effEditClose);

     finally
      Dispose(Data);
     end;

   finally
    Free;
   end;

   // switch off
   VstDispatch(effMainsChanged);

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestAbletonLiveScan;
begin
 FVstHost.VendorString := 'Ableton';
 FVstHost.ProductString := 'Live';

 with FVstHost[0] do
  begin
   VstDispatch(effGetVstVersion);
   VstDispatch(effGetPlugCategory);
   VstDispatch(effGetPlugCategory);
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestCantabile;
var
  Data : PChar;
  i    : Integer;
  rct  : TRect;
  prct : PRect;
begin
 FVstHost.VendorString := 'Topten Software';
 FVstHost.ProductString := 'Cantabile';

 with FVstHost[0] do
  begin
   // open
   VstDispatch(effOpen);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // get vst version
   VstDispatch(effGetVstVersion);

   // CanDo receiveVstEvents
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstEvents'));

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo sendVstMidiEvents
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvents'));

   GetMem(Data, 1024);
   try
    for i := 0 to numParams - 1 do
     begin
      // get param name
      VstDispatch(effGetParamName, i, 0, Data);
     end;
   finally
    Dispose(Data);
   end;

   // get plugin category
   VstDispatch(effGetPlugCategory);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 256);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // start process
   VstDispatch(effStartProcess);

   // get program
   VstDispatch(effGetProgram);

   // get program
   VstDispatch(effGetProgram);

   // program scanning
   GetMem(Data, 1024);
   try
    // get program name indexed
    VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

    // get plugin category
    VstDispatch(effGetPlugCategory);

    // get plugin category
    VstDispatch(effGetVendorString, 0, 0, Data);

    // get plugin category
    VstDispatch(effGetProductString, 0, 0, Data);

    with TForm.Create(nil) do
     try
      VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

      // set edit knob
      VstDispatch(effSetEditKnobMode, 0, 2);

      // get edit rect
      prct := @rct;
      VstDispatch(effEditGetRect, 0, 0, @prct);

      // set bounds
      SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

      // repaint
      Repaint;
      Application.ProcessMessages;

      // get program
      VstDispatch(effGetProgram);

      // get program name indexed
      VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

      // get program
      VstDispatch(effGetProgram);

      // edit top
      VstDispatch(effEditTop);

      // edit idle
      VstDispatch(effEditIdle);

      // edit sleep
      VstDispatch(effEditSleep);

      // edit idle
      VstDispatch(effEditIdle);

      // stop process
      VstDispatch(effStopProcess);

      // switch off
      VstDispatch(effMainsChanged, 0, 0);

      // edit top
      VstDispatch(effEditTop);

      // edit sleep
      VstDispatch(effEditSleep);

      // edit close
      VstDispatch(effEditClose);
     finally
      Free;
     end;

   finally
    Dispose(Data);
   end;
   
   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestCubase;
var
  Data : PChar;
  i, j : Integer;
  ChNm : TChunkName;
  rct  : TRect;
  prct : PRect;
begin
 FVstHost.VendorString := 'Steinberg';
 FVstHost.ProductString := 'Cubase VST';

 with FVstHost[0] do
  begin
   // get vst version
   VstDispatch(effGetVstVersion);

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // open
   VstDispatch(effOpen);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // cando 'bypass'
   VstDispatch(effCanDo, 0, 0, PChar('bypass'));

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo sendVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvent'));

   // CanDo midiProgramNames
   VstDispatch(effCanDo, 0, 0, PChar('midiProgramNames'));

   // program scanning
   GetMem(Data, 1024);
   try
    for i := 0 to numPrograms - 1 do
     begin
      // get program
      VstDispatch(effGetProgram);

      // get program name indexed
      VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

      // get program
      VstDispatch(effGetProgram);

      // get program name
      VstDispatch(effGetProgramName, 0, 0, Data);

      // get program name indexed
      VstDispatch(effGetProgramNameIndexed, 0, 0, Data);
     end;

    for i := 0 to numParams - 1 do
     begin
      // check can be automated
      VstDispatch(effCanBeAutomated, i);

      // get param name
      VstDispatch(effGetParamName, i, 0, Data);

      // get param label
      VstDispatch(effGetParamLabel, i, 0, Data);

      // get param properties
      VstDispatch(effGetParameterProperties, i, 0, Data);
     end;

   finally
    Dispose(Data);
   end;

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 2048);

(*
   effSetSpeakerArrangement Index: 0 Value: 280560528 Pointer: 280561440 Single: 0
*)

   // effVendorSpecific Chunkname: aCts Value: 1164857154 Pointer: 0 Single: 0
   ChNm := 'aCts';
   VstDispatch(effVendorSpecific, Integer(ChNm), Integer(ChNm));

   VstDispatch(effVendorSpecific, Integer(ChNm), Integer(ChNm));

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // cando 'bypass'
   VstDispatch(effCanDo, 0, 0, PChar('bypass'));

   // start process
   VstDispatch(effStartProcess);

   // set pan law
   VstDispatch(effSetPanLaw, 0, 0, nil, sqrt(2));

   // set knob mode
   VstDispatch(effSetEditKnobMode, 0, 2);

   // get edit rect
   prct := @rct;
   VstDispatch(effEditGetRect, 0, 0, @prct);

   with TForm.Create(nil) do
    try
     // open edit
     VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

     // get edit rect
     VstDispatch(effEditGetRect, 0, 0, @prct);

     // set bounds
     SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

     // repaint
     Repaint;
     Application.ProcessMessages;

     // get edit idle
     VstDispatch(effEditIdle);

     if numPrograms <= 0
      then Fail('No programs found, Cubase will probably crash!');

     // program scanning
     GetMem(Data, 1024);
     try
      // get program
      VstDispatch(effGetProgram);

      for i := 0 to numPrograms - 1 do
       begin
        // set program
        VstDispatch(effSetProgram, 0, i);

        // get program name
        VstDispatch(effGetProgramName, 0, 0, Data);
       end;

      for j := 0 to numPrograms - 1 do
       begin
        // get program
        VstDispatch(effGetProgram);

        // get program name indexed
        VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

        // set program
        VstDispatch(effSetProgram, 0, j);

        // get program name
        VstDispatch(effGetProgramName, j, 0, Data);

        // get program name indexed
        VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

        for i := 0 to numParams - 1 do
         begin
          // check can be automated
          VstDispatch(effCanBeAutomated, i);

          // get param name
          VstDispatch(effGetParamName, i, 0, Data);

          // get param label
          VstDispatch(effGetParamLabel, i, 0, Data);

          // get param properties
          VstDispatch(effGetParameterProperties, i, 0, Data);
         end;
       end;

     finally
      Dispose(Data);
     end;

     // get program
     VstDispatch(effGetProgram);

     // get edit idle
     VstDispatch(effEditIdle);

     // stop process
     VstDispatch(effStopProcess);

     // switch off
     VstDispatch(effMainsChanged, 0, 0);

     // close edit
     VstDispatch(effEditClose);
    finally
     Free;
    end;

   // close
   VstDispatch(effClose);
  end;
end;

procedure TVstPluginHostTests.TestReloadPluginCubase;
var
  Data : PChar;
  i, j : Integer;
  rct  : TRect;
  prct : PRect;
  ChNm : TChunkName;
begin
 FVstHost.VendorString := 'Steinberg';
 FVstHost.ProductString := 'Cubase VST';

 with FVstHost[0] do
  begin
   // get vst version
   VstDispatch(effGetVstVersion);

   // set process precision (32 bit)
   VstDispatch(effSetProcessPrecision);

   // set blocksize
   VstDispatch(effSetBlockSize, 0, 1024);

   // set samplerate
   VstDispatch(effSetSampleRate, 0, 0, nil, 44100);

   // open
   VstDispatch(effOpen);

   // switch on
   VstDispatch(effMainsChanged, 0, 1);

   // cando 'bypass'
   VstDispatch(effCanDo, 0, 0, PChar('bypass'));

   // switch off
   VstDispatch(effMainsChanged, 0, 0);

   // CanDo receiveVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('receiveVstMidiEvent'));

   // CanDo sendVstMidiEvent
   VstDispatch(effCanDo, 0, 0, PChar('sendVstMidiEvent'));

   // CanDo midiProgramNames
   VstDispatch(effCanDo, 0, 0, PChar('midiProgramNames'));

   // program scanning
   GetMem(Data, 1024);
   try
    for i := 0 to numPrograms - 1 do
     begin
      // get program
      VstDispatch(effGetProgram);

      // get program name indexed
      VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

      // get program
      VstDispatch(effGetProgram);

      // get program name
      VstDispatch(effGetProgramName, 0, 0, Data);

      // get program name indexed
      VstDispatch(effGetProgramNameIndexed, 0, 0, Data);
     end;

    for i := 0 to numParams - 1 do
     begin
      // check can be automated
      VstDispatch(effCanBeAutomated, i);

      // get param name
      VstDispatch(effGetParamName, i, 0, Data);

      // get param label
      VstDispatch(effGetParamLabel, i, 0, Data);

      // get param properties
      VstDispatch(effGetParameterProperties, i, 0, Data);
     end;

    // begin load bank
    VstDispatch(effBeginLoadBank);

    // get program
    VstDispatch(effGetProgram);

    // set program
    VstDispatch(effSetProgram);

    // begin load bank
    VstDispatch(effBeginLoadProgram);

    // begin set program
    VstDispatch(effBeginSetProgram);

    // set program name
    StrPCopy(Data, 'Test');
    VstDispatch(effSetProgramName, 0, 0, Data);

    // end set name
    VstDispatch(effEndSetProgram);


    // set process precision (32 bit)
    VstDispatch(effSetProcessPrecision);

    // set blocksize
    VstDispatch(effSetBlockSize, 0, 2048);

 (*
    effSetSpeakerArrangement Index: 0 Value: 280560528 Pointer: 280561440 Single: 0
 *)

    ChNm := 'aCts';
    VstDispatch(effVendorSpecific, Integer(ChNm), Integer(ChNm));

    VstDispatch(effVendorSpecific, Integer(ChNm), Integer(ChNm));

    // switch on
    VstDispatch(effMainsChanged, 0, 1);

    for i := 0 to 3 do
     begin
      // cando 'bypass'
      VstDispatch(effCanDo, 0, 0, PChar('bypass'));

      // switch off
      VstDispatch(effMainsChanged, 0, 0);

      // switch on
      VstDispatch(effMainsChanged, 0, 1);
     end;

    // cando 'bypass'
    VstDispatch(effCanDo, 0, 0, PChar('bypass'));

    // start process
    VstDispatch(effStartProcess);

    // set knob mode
    VstDispatch(effSetEditKnobMode, 0, 2);

    // get edit rect
    prct := @rct;
    VstDispatch(effEditGetRect, 0, 0, @prct);

    with TForm.Create(nil) do
     try
      // open edit
      VstDispatch(effEditOpen, 0, 0, Pointer(Handle));

      // get edit rect
      VstDispatch(effEditGetRect, 0, 0, @prct);

      // set bounds
      SetBounds(rct.Left, rct.Top, rct.Right - rct.Left, rct.Bottom - rct.Top);

      // repaint
      Repaint;
      Application.ProcessMessages;

      // stop process
      VstDispatch(effStopProcess);

      // get edit idle
      VstDispatch(effEditIdle);

      // start process
      VstDispatch(effStartProcess);

      // get edit idle
      VstDispatch(effEditIdle);

      if numPrograms <= 0
       then Fail('No programs found, Cubase will probably crash!');

      // get program
      VstDispatch(effGetProgram);

      for i := 0 to numPrograms - 1 do
       begin
        // set program
        VstDispatch(effSetProgram, 0, i);

        // get program name
        VstDispatch(effGetProgramName, 0, 0, Data);
       end;

      for j := 0 to numPrograms - 1 do
       begin
        // get program
        VstDispatch(effGetProgram);

        // get program name indexed
        VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

        // set program
        VstDispatch(effSetProgram, 0, j);

        // get program name
        VstDispatch(effGetProgramName, j, 0, Data);

        // get program name indexed
        VstDispatch(effGetProgramNameIndexed, 0, 0, Data);

        for i := 0 to numParams - 1 do
         begin
          // check can be automated
          VstDispatch(effCanBeAutomated, i);

          // get param name
          VstDispatch(effGetParamName, i, 0, Data);

          // get param label
          VstDispatch(effGetParamLabel, i, 0, Data);

          // get param properties
          VstDispatch(effGetParameterProperties, i, 0, Data);
         end;
       end;

      // get program
      VstDispatch(effGetProgram);

      // get edit idle
      VstDispatch(effEditIdle);

      // stop process
      VstDispatch(effStopProcess);

      // switch off
      VstDispatch(effMainsChanged, 0, 0);

      // close edit
      VstDispatch(effEditClose);

     finally
      Free;
     end;

   finally
    Dispose(Data);
   end;

   // close
   VstDispatch(effClose);
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

 FVstHost.VendorString := 'Delphi ASIO & VST Project';
 FVstHost.ProductString := 'Delphi ASIO & VST Project';
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

 FVstHost.VendorString := 'Delphi ASIO & VST Project';
 FVstHost.ProductString := 'Delphi ASIO & VST Project';

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

{$IFNDEF CONSOLE_TESTRUNNER}
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
         TS.AddTests(TVstPluginPerverseTests);
         TS.AddTests(TVstPluginHostTests);
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
{$ENDIF}

initialization
  if ParamStr(1) <> '' then
   begin
    RegisterTest(TVstPluginBasicTests.Suite);
    RegisterTest(TVstPluginPerverseTests.Suite);
    RegisterTest(TVstPluginHostTests.Suite);
    RegisterTest(TVstPluginIOTests.Suite);
    RegisterTest(TVstPluginIOThreadTests.Suite);
   end else
{$IFNDEF CONSOLE_TESTRUNNER}
  EnumerateVstPlugins;
{$ELSE}
  begin
   WriteLn('Please specify a VST plugin DLL!');
   WriteLn('');
   WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' filename');
  end;
{$ENDIF}

end.
