unit DAV_TestAudioFileWav;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_ChunkClasses, DAV_ChunkWaveFile, Classes, DAV_Common, Contnrs,
  SysUtils, DAV_ChannelDataCoder, DAV_AudioFileWav, DAV_AudioFile;

type
  // Test methods for class TAudioFileWav
  TestAudioFileWav = class(TTestCase)
  strict private
    FAudioFileWav: TAudioFileWav;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
    procedure TestBasicWriting;
  end;

implementation

uses
  Dialogs;

procedure TestAudioFileWav.SetUp;
begin
  FAudioFileWav := TAudioFileWav.Create(nil);
end;

procedure TestAudioFileWav.TearDown;
begin
 FreeAndNil(FAudioFileWav);
end;

procedure TestAudioFileWav.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.wav*', faAnyFile, SR) = 0 then
  try
   repeat
    succeed := True;
    try
     FAudioFileWav.LoadFromFile(SR.Name)
    except
     on e: EWavError do MessageDlg(SR.Name + ': ' + e.Message, mtError, [mbOK], 0);
     else succeed := False;
    end;
    Check(succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TestAudioFileWav.TestBasicWriting;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   FAudioFileWAV.Name := 'Test';
(*
   FAudioFileWAV.Author := 'That''s me';
   FAudioFileWAV.Copyright := 'That''s also me';
*)
   FAudioFileWAV.SampleFrames := 100;
   FAudioFileWAV.SaveToStream(TempStream);
   TempStream.Position := 0;
   FAudioFileWAV.LoadFromStream(TempStream);
  finally
   Free;
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestAudioFileWav.Suite);
  
end.

