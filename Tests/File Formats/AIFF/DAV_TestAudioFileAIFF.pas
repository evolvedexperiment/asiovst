unit DAV_TestAudioFileAIFF;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_ChunkClasses, DAV_ChunkAIFFFile, Classes, DAV_Common, Contnrs, 
  SysUtils, DAV_ChannelDataCoder, DAV_AudioFileAIFF, DAV_AudioFile;

type
  // Test methods for class TAudioFileAIFF
  TestAudioFileAIFF = class(TTestCase)
  strict private
    FAudioFileAIFF: TAudioFileAIFF;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
  end;

implementation

uses
  Dialogs;

procedure TestAudioFileAIFF.SetUp;
begin
  FAudioFileAIFF := TAudioFileAIFF.Create(nil);
end;

procedure TestAudioFileAIFF.TearDown;
begin
 FreeAndNil(FAudioFileAIFF);
end;

procedure TestAudioFileAIFF.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.aif*', faAnyFile, SR) = 0 then
  try
   repeat
    succeed := True;
    try
     FAudioFileAIFF.LoadFromFile(SR.Name)
    except
     on e: EAIFFError do MessageDlg(e.Message, mtError, [mbOK], 0);
     else succeed := False;
    end;
    Check(succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestAudioFileAIFF.Suite);
  
end.

