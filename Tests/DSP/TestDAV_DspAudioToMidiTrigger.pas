unit TestDAV_DspAudioToMidiTrigger;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, DAV_Common, DAV_DspCommon, DAV_DspAudioToMidiTrigger,
  DAV_DspFilter, DAV_DspFilterBasics;

type
  // Test methods for class TAudio2MidiTrigger
  TestTAudio2MidiTrigger = class(TTestCase)
  strict private
    FCustomAudio2MidiTrigger: TCustomAudio2MidiTrigger;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
    procedure TestFilters;
  end;

implementation

uses
  SysUtils;

procedure TestTAudio2MidiTrigger.SetUp;
begin
 FCustomAudio2MidiTrigger := TCustomAudio2MidiTrigger.Create;
end;

procedure TestTAudio2MidiTrigger.TearDown;
begin
 FreeAndNil(FCustomAudio2MidiTrigger);
end;

procedure TestTAudio2MidiTrigger.TestProcessSample32;
var
  ReturnValue: Single;
begin
 with FCustomAudio2MidiTrigger do
  begin
   Flags := [amFilterOutput];
   ReturnValue := ProcessSample32(1);
   CheckEquals(ReturnValue, 1);
  end;
end;

procedure TestTAudio2MidiTrigger.TestProcessSample64;
var
  ReturnValue: Double;
begin
 with FCustomAudio2MidiTrigger do
  begin
   Flags := [amFilterOutput];
   ReturnValue := ProcessSample32(1);
   CheckEquals(ReturnValue, 1);
  end;
end;

procedure TestTAudio2MidiTrigger.TestFilters;
var
  GainFilter: TCustomFilter;
begin
 GainFilter := TBasicGainFilter.Create;
 try
  with FCustomAudio2MidiTrigger do
   begin
    AddFilter(GainFilter);
    DeleteFilter(GainFilter);
    AddFilter(GainFilter);
    DeleteFilter(0);
   end;
 finally
  FreeAndNil(GainFilter);
 end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTAudio2MidiTrigger.Suite);
end.

