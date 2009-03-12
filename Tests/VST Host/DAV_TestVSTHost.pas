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
  // Test methods for class TVstHost
  TTestVstPlugin = class(TTestCase)
  strict private
    FVstHost: TVstHost;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMultipleOpenCloseCycles;
    procedure TestMultipleInstances;
    procedure TestActiveParameterSweeps;
    procedure TestPassiveParameterSweeps;
    procedure TestActiveSamplerateChanges;
    procedure TestPassiveSamplerateChanges;
    procedure TestActiveBlocksizeChanges;
    procedure TestPassiveBlocksizeChanges;
    procedure TestHandleDataBeyondBlocksizeChanges;
  end;

implementation

function RemoveFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.')
   then Result := Copy(FileName, 0, I)
   else Result := '';
end;

procedure TTestVstPlugin.SetUp;
var
  TestDLL : TFileName;

begin
 FVstHost := TVstHost.Create(nil);
 with FVstHost.VstPlugIns.Add do
  begin
   TestDLL := RemoveFileExt(ExtractFileName(ParamStr(0))) + 'DLL';

   if not FileExists(TestDLL) then
    with TOpenDialog.Create(nil) do
     try
      Filter := 'VST Plugins (*.dll)|*.dll';
      DefaultExt := 'dll';
      InitialDir := FVstHost.PlugInDir;
      Options := Options + [ofFileMustExist];
      if Execute
       then TestDLL := FileName
       else Exit;
     finally
      Free;
     end;

   if FileExists(TestDLL) then
    begin
     LoadFromFile(TestDLL);
    end
  end;
end;

procedure TTestVstPlugin.TearDown;
begin
 FreeAndNil(FVstHost);
end;


procedure TTestVstPlugin.TestMultipleInstances;
var
  i, j : Integer;
const
  CInstantCount = 129;
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

procedure TTestVstPlugin.TestMultipleOpenCloseCycles;
var
  i : Integer;
begin
 for i := 0 to 9 do
  begin
   FVstHost[0].Active := True;
   FVstHost[0].Active := False;
  end;
end;

procedure TTestVstPlugin.TestActiveSamplerateChanges;
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

procedure TTestVstPlugin.TestPassiveSamplerateChanges;
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

procedure TTestVstPlugin.TestActiveParameterSweeps;
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

procedure TTestVstPlugin.TestPassiveParameterSweeps;
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

procedure TTestVstPlugin.TestActiveBlocksizeChanges;
var
  i : Integer;
begin
 FVstHost[0].Active := True;

 // small block sizes
 for i := 0 to 64 do FVstHost[0].SetBlockSize(i);

 // medium odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(19 * i);

 // large odd block sizes
 for i := 1 to 64 do FVstHost[0].SetBlockSize(1025 * i);
end;

procedure TTestVstPlugin.TestPassiveBlocksizeChanges;
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

procedure TTestVstPlugin.TestHandleDataBeyondBlocksizeChanges;
var
  Input   : array of PDavSingleFixedArray;
  Output  : array of PDavSingleFixedArray;
  Channel : Integer;
const
  CBlocksize = 8192;
begin
 with FVstHost[0] do
  begin
   Active := True;
   SetSampleRate(44100);
   SetBlockSize(CBlocksize);

   // setup inputs
   SetLength(Input, numInputs);
   for Channel := 0 to numInputs - 1 do
    begin
     ReallocMem(Input[Channel], 4 * CBlocksize * SizeOf(Single));
     FillChar(Input[Channel]^, 4 * CBlocksize * SizeOf(Single), 0);
    end;

   // setup outputs
   SetLength(Output, numOutputs);
   for Channel := 0 to numOutputs - 1 do
    begin
     ReallocMem(Output[Channel], 4 * CBlocksize * SizeOf(Single));
     FillChar(Output[Channel]^, 4 * CBlocksize * SizeOf(Single), 0);
    end;

   ProcessReplacing(@Input[0], @Output[0], CBlocksize div 2);
   ProcessReplacing(@Input[0], @Output[0], CBlocksize);
   ProcessReplacing(@Input[0], @Output[0], CBlocksize * 2);
   ProcessReplacing(@Input[0], @Output[0], CBlocksize * 3);
   ProcessReplacing(@Input[0], @Output[0], CBlocksize * 4);

   for Channel := 0 to numInputs - 1  do Dispose(Input[Channel]);
   for Channel := 0 to numOutputs - 1 do Dispose(Output[Channel]);

   Active := False;
  end;
end;

initialization
  RegisterTest(TTestVstPlugin.Suite);

end.

