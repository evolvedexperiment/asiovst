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
    function TestPolynomial(Input: Word): Byte;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMZlib;
    procedure TestProperties;
    procedure TestRead;
    procedure TestSeek;
  end;

implementation

uses
  SysUtils, zlib, mzlib;

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
 CompressionStream := TZCompressionStream.Create(FCompressedDataStream, zcDefault);
 for Index := 0 to High(Word) do
  begin
   Value := TestPolynomial(Index);
   CompressionStream.Write(Value, 1);
  end;
 FreeAndNil(CompressionStream);

// FCompressedDataStream.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Dummy.z');
end;

procedure TestTZDecompressionStream.TestMZlib;
var
  ZState : mzlib.TZState;
  Index  : Integer;
  Buffer : array [Word] of Byte;
begin
 FillChar(ZState, SizeOf(ZState), 0);
 ZState.NextInput := FCompressedDataStream.Memory;
 ZState.AvailableInput := FCompressedDataStream.Size;
 InflateInit(ZState);

 ZState.NextOutput := @Buffer;
 ZState.AvailableOutput := Length(Buffer);
 mzlib.Inflate(ZState, Z_PARTIAL_FLUSH);

 // check whether the decompressed bytes have the expected value
 for Index := 0 to Length(Buffer) - 1 do
  begin
   CheckEquals(TestPolynomial(Index), Buffer[Index]);
  end;
end;

function TestTZDecompressionStream.TestPolynomial(Input: Word): Byte;
begin
// Result := Input xor $AC;
// Result := Input mod 4;
 Result := Input mod 64;
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
 Buffer      : array [Word] of Byte;
begin
 FillChar(Buffer[0], Length(Buffer), 0);

 ReturnValue := FZDecompressionStream.Read(Buffer[0], Length(Buffer));

 // check whether all bytes were decompressed
 CheckEquals(Length(Buffer), ReturnValue, 'A wrong amount of bytes were read');

 // check whether the decompressed bytes have the expected value
 for Index := 0 to Length(Buffer) - 1 do
  begin
   CheckEquals(TestPolynomial(Index), Buffer[Index]);
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


