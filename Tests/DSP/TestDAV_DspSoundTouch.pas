unit TestDAV_DspSoundTouch;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspSoundTouch, DAV_SoundTouchDLL, DAV_Common, DAV_Classes;

type
  // Test methods for class TDspSoundTouch
  TestTDspSoundTouch = class(TTestCase)
  strict private
    FDspSoundTouch: TDspSoundTouch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetVersionString;
    procedure TestGetVersionId;
    procedure TestFlush;
    procedure TestClear;
    procedure TestProcessSamples;
    procedure TestSetSetting;
    procedure TestGetSetting;
  end;

implementation

uses
  SysUtils;

procedure TestTDspSoundTouch.SetUp;
begin
 FDspSoundTouch := TDspSoundTouch.Create;
end;

procedure TestTDspSoundTouch.TearDown;
begin
 FreeAndNil(FDspSoundTouch);
end;

procedure TestTDspSoundTouch.TestGetVersionString;
var
  ReturnValue: string;
begin
  ReturnValue := FDspSoundTouch.GetVersionString;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestGetVersionId;
var
  ReturnValue: Cardinal;
begin
  ReturnValue := FDspSoundTouch.GetVersionId;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestFlush;
begin
  FDspSoundTouch.Flush;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestClear;
begin
  FDspSoundTouch.Clear;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestProcessSamples;
var
  SampleFrames : Cardinal;
  Data         : PDAVSingleFixedArray;
  ReturnValue  : Cardinal;
begin
 SampleFrames := 1024;
 GetMem(Data, SampleFrames * SizeOf(Single));
 try
  FDspSoundTouch.WriteSamples(Data, SampleFrames);
  ReturnValue := FDspSoundTouch.ReadSamples(Data, SampleFrames);
  CheckEquals(ReturnValue, SampleFrames);
 finally
  Dispose(Data);
 end;
end;

procedure TestTDspSoundTouch.TestSetSetting;
var
  ReturnValue: Boolean;
  Value: Integer;
  SettingId: Integer;
begin
  // TODO: Setup method call parameters
  ReturnValue := FDspSoundTouch.SetSetting(SettingId, Value);
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestGetSetting;
var
  ReturnValue: Integer;
  SettingId: Integer;
begin
  // TODO: Setup method call parameters
  ReturnValue := FDspSoundTouch.GetSetting(SettingId);
  // TODO: Validate method results
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDspSoundTouch.Suite);
end.

