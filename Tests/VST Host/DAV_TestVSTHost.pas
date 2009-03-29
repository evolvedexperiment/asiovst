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

  // Test methods for class TVstHost
  TVstPluginBasicTests = class(TCustomTestVstPlugin)
  published
    procedure TestMultipleOpenCloseCycles;
    procedure TestMultipleInstances;
    procedure TestActiveParameterSweeps;
    procedure TestPassiveParameterSweeps;
    procedure TestActiveSamplerateChanges;
    procedure TestPassiveSamplerateChanges;
    procedure TestActiveBlocksizeChanges;
    procedure TestPassiveBlocksizeChanges;
  end;

  // Test methods for class TVstHost
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

    property BlockSize: Integer read FBlockSize write SetBlockSize;
  end;

implementation

uses
  Math, SplashScreen, Forms, Controls;

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
 if ParamStr(0) <> '' then FVstPluginName := ParamStr(0); 
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

procedure TVstPluginBasicTests.TestPassiveSamplerateChanges;
var
  d : Single;
begin
 // Test Passive Samplerate Change
 FVstHost[0].Active := False;
 d := 1;
 while d <= 1411200 do
  begin
   FVstHost[0].SetSampleRate(d);
   d := d * 1.1;
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

procedure TVstPluginBasicTests.TestPassiveParameterSweeps;
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

procedure TVstPluginBasicTests.TestPassiveBlocksizeChanges;
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
  end;
end;

procedure TVstPluginIOTests.TearDown;
var
  Channel : Integer;
begin
 inherited;

 // dispose memory
 for Channel := 0 to Length(FInput)  - 1 do Dispose(FInput[Channel]);
 for Channel := 0 to Length(FOutput) - 1 do Dispose(FOutput[Channel]);

 FVstHost[0].Active := False;
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
   for Channel := 0 to numInputs - 1 do FInput[Channel, 0] := 1;

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
    for Channel := 0 to Length(FInput) - 1 do FInput[Channel, 0] := 1;
   until Cnt >= 4;
  end;
end;

procedure TVstPluginIOTests.TestProcess;
begin

end;

procedure TVstPluginIOTests.TestHandleDataBeyondBlocksizeChanges;
var
  Channel : Integer;
begin
 with FVstHost[0] do
  begin
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize div 2);
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize);
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize * 2);
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize * 3);
   ProcessReplacing(@FInput[0], @FOutput[0], FBlocksize * 4);
  end;
end;

procedure TVstPluginIOTests.TestHandleNANs;
var
  Channel : Integer;
  Sample  : Integer;
  IsDen   : Boolean;
const
  FBlocksize = 8192;
begin
 with FVstHost[0] do
  begin
   // build impulse
   for Channel := 0 to numInputs - 1 do FInput[Channel, 0] := NaN;

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
  if ParamStr(0) <> ''
   then EnumerateVstPlugins
   else RegisterTest(TVstPluginBasicTests.Suite);

end.
