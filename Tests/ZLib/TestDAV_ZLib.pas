unit TestDAV_ZLib;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  Classes, TestFramework, DAV_ZLib;

type
  TestTZDecompressionStream = class(TTestCase)
  strict private
    FCompressedDataStream : TMemoryStream;
    FZDecompressionStream : TZDecompressionStream;
  protected
    procedure InitializeCompressedData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
    procedure TestRead;
    procedure TestSeek;
  end;

implementation

uses
  SysUtils, zlib;

procedure TestTZDecompressionStream.SetUp;
begin
 // create and initialize compressed data stream
 FCompressedDataStream := TMemoryStream.Create;
 InitializeCompressedData;

 FCompressedDataStream.Position := 0;
 FZDecompressionStream := DAV_ZLib.TZDecompressionStream.Create(FCompressedDataStream);
end;

procedure TestTZDecompressionStream.TearDown;
begin
 FreeAndNil(FCompressedDataStream);
 FreeAndNil(FZDecompressionStream);
end;

procedure TestTZDecompressionStream.InitializeCompressedData;
var
  Index             : Integer;
  Value             : Byte;
  CompressionStream : zlib.TZCompressionStream;
begin
 CompressionStream := TZCompressionStream.Create(FCompressedDataStream);
 for Index := 0 to High(Byte) - 1 do
  begin
   Value := Index xor $AC;
   CompressionStream.Write(Value, 1);
  end;
 FreeAndNil(CompressionStream);
end;

procedure TestTZDecompressionStream.TestProperties;
begin
 CheckTrue(FZDecompressionStream.CompressionMethod = cmDeflate);
 CheckTrue(FZDecompressionStream.CompressionLevel = DAV_ZLib.clDefault);
end;

procedure TestTZDecompressionStream.TestRead;
var
 ReturnValue : Integer;
 Index       : Integer;
 Buffer      : array [Byte] of Byte;
begin
 FillChar(Buffer[0], Length(Buffer), 0);

 ReturnValue := FZDecompressionStream.Read(Buffer[0], Length(Buffer));

 // check whether all bytes were decompressed
 CheckEquals(Length(Buffer), ReturnValue, 'A wrong amount of bytes were read');

 // check whether the decompressed bytes have the expected value
 for Index := 0 to High(Byte) - 1 do
  begin
   CheckEquals(Index xor $AC, Buffer[Index]);
  end;
end;

procedure TestTZDecompressionStream.TestSeek;
var
 ReturnValue : Integer;
begin
 ReturnValue := FZDecompressionStream.Seek(0, soBeginning);

 // Todo: Check return value!
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTZDecompressionStream.Suite);

end.


