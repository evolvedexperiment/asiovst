unit TestDAV_DspConvolution;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enth�lt ein Codeger�st einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. �ndern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

{$I DAV_Compiler.inc}

uses
  TestFramework, DAV_Complex, DAV_DspFftReal2Complex, DAV_Common, DAV_DspCommon,
  DAV_DspConvolution;

type
  // Test methods for class TConvolution32
  TestTConvolution32 = class(TTestCase)
  strict private
    FConvolution32: TConvolution32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TConvolution64
  TestTConvolution64 = class(TTestCase)
  strict private
    FConvolution64: TConvolution64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolution32
  TestTLowLatencyConvolution32 = class(TTestCase)
  strict private
    FLowLatencyConvolution32: TLowLatencyConvolution32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolutionStereo32
  TestTLowLatencyConvolutionStereo32 = class(TTestCase)
  strict private
    FLowLatencyConvolutionStereo32: TLowLatencyConvolutionStereo32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
  end;

  // Test methods for class TLowLatencyConvolution64
  TestTLowLatencyConvolution64 = class(TTestCase)
  strict private
    FLowLatencyConvolution64: TLowLatencyConvolution64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolutionStereo64
  TestTLowLatencyConvolutionStereo64 = class(TTestCase)
  strict private
    FLowLatencyConvolutionStereo64: TLowLatencyConvolutionStereo64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
  end;

implementation

uses
  SysUtils;


{ TestTConvolution32 }

procedure TestTConvolution32.SetUp;
begin
 FConvolution32 := TConvolution32.Create;
end;

procedure TestTConvolution32.TearDown;
begin
 FreeAndNil(FConvolution32);
end;

procedure TestTConvolution32.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FConvolution32.ProcessBlock(nil, nil, 0);
end;

procedure TestTConvolution32.TestLoadImpulseResponse;
var
  Data       : TDAVSingleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FConvolution32.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FConvolution32.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FConvolution32.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTConvolution64 }

procedure TestTConvolution64.SetUp;
begin
  FConvolution64 := TConvolution64.Create;
end;

procedure TestTConvolution64.TearDown;
begin
 FreeAndNil(FConvolution64);
end;

procedure TestTConvolution64.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FConvolution64.ProcessBlock(nil, nil, 0);
end;

procedure TestTConvolution64.TestLoadImpulseResponse;
var
  Data       : TDAVDoubleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FConvolution64.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FConvolution64.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FConvolution64.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTLowLatencyConvolution32 }

procedure TestTLowLatencyConvolution32.SetUp;
begin
  FLowLatencyConvolution32 := TLowLatencyConvolution32.Create;
end;

procedure TestTLowLatencyConvolution32.TearDown;
begin
 FreeAndNil(FLowLatencyConvolution32);
end;

procedure TestTLowLatencyConvolution32.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolution32.ProcessBlock(nil, nil, 0);
 FLowLatencyConvolution32.ProcessBlock(nil, 0);
end;

procedure TestTLowLatencyConvolution32.TestLoadImpulseResponse;
var
  Data       : TDAVSingleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FLowLatencyConvolution32.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FLowLatencyConvolution32.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FLowLatencyConvolution32.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTLowLatencyConvolutionStereo32 }

procedure TestTLowLatencyConvolutionStereo32.SetUp;
begin
 FLowLatencyConvolutionStereo32 := TLowLatencyConvolutionStereo32.Create;
end;

procedure TestTLowLatencyConvolutionStereo32.TearDown;
begin
 FreeAndNil(FLowLatencyConvolutionStereo32);
end;

procedure TestTLowLatencyConvolutionStereo32.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolutionStereo32.ProcessBlock(nil, nil, 0);
end;


{ TestTLowLatencyConvolution64 }

procedure TestTLowLatencyConvolution64.SetUp;
begin
  FLowLatencyConvolution64 := TLowLatencyConvolution64.Create;
end;

procedure TestTLowLatencyConvolution64.TearDown;
begin
 FreeAndNil(FLowLatencyConvolution64);
end;

procedure TestTLowLatencyConvolution64.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolution64.ProcessBlock(nil, nil, 0);
 FLowLatencyConvolution64.ProcessBlock(nil, 0);
end;

procedure TestTLowLatencyConvolution64.TestLoadImpulseResponse;
var
  Data       : TDAVDoubleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FLowLatencyConvolution64.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FLowLatencyConvolution64.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FLowLatencyConvolution64.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTLowLatencyConvolutionStereo64 }

procedure TestTLowLatencyConvolutionStereo64.SetUp;
begin
  FLowLatencyConvolutionStereo64 := TLowLatencyConvolutionStereo64.Create;
end;

procedure TestTLowLatencyConvolutionStereo64.TearDown;
begin
 FreeAndNil(FLowLatencyConvolutionStereo64);
end;

procedure TestTLowLatencyConvolutionStereo64.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolutionStereo64.ProcessBlock(nil, nil, 0);
end;

initialization
  // Alle Testf�lle beim Test-Runner registrieren
  RegisterTest(TestTConvolution32.Suite);
  RegisterTest(TestTConvolution64.Suite);
  RegisterTest(TestTLowLatencyConvolution32.Suite);
  RegisterTest(TestTLowLatencyConvolutionStereo32.Suite);
  RegisterTest(TestTLowLatencyConvolution64.Suite);
  RegisterTest(TestTLowLatencyConvolutionStereo64.Suite);
end.

