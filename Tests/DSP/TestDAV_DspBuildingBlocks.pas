unit TestDAV_DspBuildingBlocks;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspBuildingBlocks, DAV_Types, DAV_Classes;

type
  // Test methods for class TBuildingBlocks32
  TestTBuildingBlocks32 = class(TTestCase)
  strict private
    FBuildingBlocks32 : TBuildingBlocks32;
    FOnProcessCount   : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVSingleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicProcesssing;
    procedure TestPowerOf2Blocksizes;
  end;

  // Test methods for class TBuildingBlocksCircular32
  TestTBuildingBlocksCircular32 = class(TTestCase)
  strict private
    FBuildingBlocksCircular32 : TBuildingBlocksCircular32;
    FOnProcessCount           : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVSingleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicProcesssing;
    procedure TestPowerOf2Blocksizes;
  end;

  // Test methods for class TBuildingBlocks64
  TestTBuildingBlocks64 = class(TTestCase)
  strict private
    FBuildingBlocks64 : TBuildingBlocks64;
    FOnProcessCount   : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVDoubleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicProcesssing;
    procedure TestPowerOf2Blocksizes;
  end;

  // Test methods for class TBuildingBlocksCircular64
  TestTBuildingBlocksCircular64 = class(TTestCase)
  strict private
    FBuildingBlocksCircular64 : TBuildingBlocksCircular64;
    FOnProcessCount           : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVDoubleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicProcesssing;
    procedure TestPowerOf2Blocksizes;
  end;

implementation

uses
  SysUtils;

const
  CmaxOrder = 15;

{ TestTBuildingBlocks32 }

procedure TestTBuildingBlocks32.SetUp;
begin
 FBuildingBlocks32 := TBuildingBlocks32.Create;
 with FBuildingBlocks32 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocks32.TearDown;
begin
 FreeAndNil(FBuildingBlocks32);
end;

procedure TestTBuildingBlocks32.TestBasicProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocks32.TestPowerOf2Blocksizes;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks32.OnProcessHandler(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;


{ TestTBuildingBlocksCircular32 }

procedure TestTBuildingBlocksCircular32.SetUp;
begin
 FBuildingBlocksCircular32 := TBuildingBlocksCircular32.Create;
 with FBuildingBlocksCircular32 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocksCircular32.TearDown;
begin
 FreeAndNil(FBuildingBlocksCircular32);
end;

procedure TestTBuildingBlocksCircular32.TestBasicProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocksCircular32.TestPowerOf2Blocksizes;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  for Order := 4 to CMaxOrder - 1 do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular32.OnProcessHandler(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;



{ TestTBuildingBlocks64 }

procedure TestTBuildingBlocks64.SetUp;
begin
 FBuildingBlocks64 := TBuildingBlocks64.Create;
 with FBuildingBlocks64 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocks64.TearDown;
begin
 FreeAndNil(FBuildingBlocks64);
end;

procedure TestTBuildingBlocks64.TestBasicProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocks64.TestPowerOf2Blocksizes;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks64.OnProcessHandler(Sender: TObject;
  const Input: PDAVDoubleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;



{ TestTBuildingBlocksCircular64 }

procedure TestTBuildingBlocksCircular64.SetUp;
begin
 FBuildingBlocksCircular64 := TBuildingBlocksCircular64.Create;
 with FBuildingBlocksCircular64 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocksCircular64.TearDown;
begin
 FreeAndNil(FBuildingBlocksCircular64);
end;

procedure TestTBuildingBlocksCircular64.TestBasicProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocksCircular64.TestPowerOf2Blocksizes;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular64.OnProcessHandler(Sender: TObject;
  const Input: PDAVDoubleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;


initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTests([TestTBuildingBlocks32.Suite,
    TestTBuildingBlocksCircular32.Suite, TestTBuildingBlocks64.Suite,
    TestTBuildingBlocksCircular64.Suite]);
end.

