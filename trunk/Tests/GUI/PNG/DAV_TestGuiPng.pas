unit DAV_TestGuiPng;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, Contnrs, SysUtils, DAV_Common, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_GuiPng;

type
  // Test methods for class TAudioFileWav
  TTestGuiPng = class(TTestCase)
  strict private
    FPngFile : TPngImage;
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

procedure TTestGuiPng.SetUp;
begin
  FPngFile := TPngImage.Create;
end;

procedure TTestGuiPng.TearDown;
begin
 FreeAndNil(FPngFile);
end;

procedure TTestGuiPng.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.png*', faAnyFile, SR) = 0 then
  try
   repeat
    Succeed := True;
    try
     FPngFile.LoadFromFile(SR.Name)
    except
     on e: EPngError do MessageDlg(SR.Name + ': ' + e.Message, mtError, [mbOK], 0);
     else Succeed := False;
    end;
    Check(Succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TTestGuiPng.TestBasicWriting;
var
  TempStream : TMemoryStream;
  Chunk      : TCustomChunk;
  I          : Integer;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FPngFile do
    begin
    end;
  finally
   Free;
  end;
end;

initialization
  RegisterTest(TTestGuiPng.Suite);

end.
