unit TestGuiFixedPoint;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Windows, Controls, Types, Classes, SysUtils, Messages,
  Graphics, DAV_GuiCommon, DAV_GuiFixedPoint;

type
  TCustomTestFixedPoint = class(TTestCase)
  protected
    procedure TestAdd; virtual; abstract;
    procedure TestSub; virtual; abstract;
    procedure TestMul; virtual; abstract;
    procedure TestDiv; virtual; abstract;
    procedure TestSqr; virtual; abstract;
    procedure TestFloor; virtual; abstract;
    procedure TestCeil; virtual; abstract;
    procedure TestRound; virtual; abstract;
    procedure TestReciprocal; virtual; abstract;
    procedure TestSqrt; virtual; abstract;
  end;

  TTestFixedPoint16Dot16 = class(TCustomTestFixedPoint)
  published
    procedure TestAdd; override;
    procedure TestSub; override;
    procedure TestMul; override;
    procedure TestDiv; override;
    procedure TestSqr; override;
    procedure TestFloor; override;
    procedure TestCeil; override;
    procedure TestRound; override;
    procedure TestReciprocal; override;
    procedure TestSqrt; override;
  end;

  TTestFixedPoint24Dot8 = class(TCustomTestFixedPoint)
  published
    procedure TestAdd; override;
    procedure TestSub; override;
    procedure TestMul; override;
    procedure TestDiv; override;
    procedure TestSqr; override;
    procedure TestFloor; override;
    procedure TestCeil; override;
    procedure TestRound; override;
    procedure TestReciprocal; override;
    procedure TestSqrt; override;
  end;

implementation

{ TTestFixedPoint16Dot16 }

procedure TTestFixedPoint16Dot16.TestAdd;
var
  A, B, C : TFixed16Dot16Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed16Dot16Point(Index);
   C := FixedAdd(A, B);
   CheckEquals(A.Fixed + B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSub;
var
  A, B, C : TFixed16Dot16Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed16Dot16Point(Index);
   C := FixedSub(A, B);
   CheckEquals(A.Fixed - B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestMul;
var
  A, B, C : TFixed16Dot16Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed16Dot16Point(Index);
   C := FixedMul(A, B);
   CheckEquals(Round(A.Fixed * CFixed16Dot16ToFloat * B.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestDiv;
var
  A, B, C : TFixed16Dot16Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFF do
  begin
   B := Fixed16Dot16Point(Index);
   C := FixedDiv(B, A);
   CheckEquals(Round(B.Fixed / A.Fixed * CFixed16Dot16One.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSqr;
var
  A, B  : TFixed16Dot16Point;
  Index : Integer;
begin
 inherited;
 for Index := 0 to $B5 do
  begin
   A := Fixed16Dot16Point(Index);
   B := FixedSqr(A);
   CheckEquals(Round((A.Fixed / $10000) * A.Fixed), B.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestFloor;
var
  Value  : TFixed16Dot16Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $7FFE do
  begin
   Value.Int  := Index;
   Value.Frac := Index;
   Result := FixedFloor(Value);
   CheckEquals((Value.Fixed) shr 16, Result);
  end;
end;

procedure TTestFixedPoint16Dot16.TestCeil;
var
  Value  : TFixed16Dot16Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $7FFE do
  begin
   Value.Int  := Index;
   Value.Frac := Index;
   Result := FixedCeil(Value);
   CheckEquals((Value.Fixed + $FFFF) shr 16, Result);
  end;
end;

procedure TTestFixedPoint16Dot16.TestRound;
var
  Value  : TFixed16Dot16Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $7FFE do
  begin
   Value.Int  := Index;
   Value.Frac := Index;
   Result := FixedRound(Value);
   CheckEquals((Value.Fixed + $7FFF) shr 16, Result);
  end;
end;

procedure TTestFixedPoint16Dot16.TestReciprocal;
var
  A, B  : TFixed16Dot16Point;
  Index : Integer;
begin
 inherited;
 for Index := 1 to $7FFF do
  begin
   A := Fixed16Dot16Point(Index);
   B := FixedReciprocal(A);
   CheckEquals(Round(4294967296 / A.Fixed - 0.5), B.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSqrt;
var
  A, B  : TFixed16Dot16Point;
  Index : Integer;
const
  CFixed16Dot16OneAsSingle : Single = 65536;
begin
 inherited;

 for Index := 0 to $7FFF do
  begin
   A := Fixed16Dot16Point(Index);
   B := FixedSqrtLowResolution(A);
   CheckEquals(Round(Sqrt(A.Fixed * CFixed16Dot16OneAsSingle) - 0.5) and $FFFFFF00,
     B.Fixed and $FFFFFF00);
  end;

 for Index := 0 to $3FFF do
  begin
   A := Fixed16Dot16Point(Index);
   B := FixedSqrtHighResolution(A);
   CheckEquals(Round(Sqrt(A.Fixed * CFixed16Dot16OneAsSingle) - 0.5), B.Fixed);
  end;
end;


{ TTestFixedPoint24Dot8 }

procedure TTestFixedPoint24Dot8.TestAdd;
var
  A, B, C : TFixed24Dot8Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed24Dot8Point(Index);
   C := FixedAdd(A, B);
   CheckEquals(A.Fixed + B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSub;
var
  A, B, C : TFixed24Dot8Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed24Dot8Point(Index);
   C := FixedSub(A, B);
   CheckEquals(A.Fixed - B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestMul;
var
  A, B, C : TFixed24Dot8Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed24Dot8Point(Index);
   C := FixedMul(A, B);
   CheckEquals(Round(A.Fixed * CFixed24Dot8ToFloat * B.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestDiv;
var
  A, B, C : TFixed24Dot8Point;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := Fixed24Dot8Point(Index);
   C := FixedDiv(B, A);
   CheckEquals(Round(B.Fixed / A.Fixed * CFixed24Dot8One.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSqr;
var
  A, B  : TFixed24Dot8Point;
  Index : Integer;
begin
 inherited;
 for Index := 0 to $FF do
  begin
   A := Fixed24Dot8Point(Index);
   B := FixedSqr(A);
   CheckEquals(Round((A.Fixed * CFixed24Dot8ToFloat) * A.Fixed), B.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestFloor;
var
  Value  : TFixed24Dot8Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $FF do
  begin
   Value.Fixed := Index shl 8;
   Value.Frac  := Index shr 8;
   Result := FixedFloor(Value);
   CheckEquals(Value.Fixed shr 8, Result);
  end;
end;

procedure TTestFixedPoint24Dot8.TestCeil;
var
  Value  : TFixed24Dot8Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $FF do
  begin
   Value.Fixed := Index shl 8;
   Value.Frac  := Index shr 8;
   Result := FixedCeil(Value);
   CheckEquals((Value.Fixed + $FF) shr 8, Result);
  end;
end;

procedure TTestFixedPoint24Dot8.TestRound;
var
  Value  : TFixed24Dot8Point;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := 0 to $FF do
  begin
   Value.Fixed := Index shl 8;
   Value.Frac  := Index shr 8;
   Result := FixedRound(Value);
   CheckEquals((Value.Fixed + $7F) shr 8, Result);
  end;
end;

procedure TTestFixedPoint24Dot8.TestReciprocal;
var
  A, B  : TFixed24Dot8Point;
  Index : Integer;
begin
 inherited;
 for Index := 1 to $7F do
  begin
   A := Fixed24Dot8Point(Index);
   B := FixedReciprocal(A);
   CheckEquals(Round(65536 / A.Fixed - 0.5), B.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSqrt;
var
  A, B, C : TFixed24Dot8Point;
  Index   : Integer;
const
  CFixed24Dot8OneAsSingle : Single = 256;
begin
 inherited;

 for Index := 0 to $7FFF do
  begin
   A := Fixed24Dot8Point(Index);
   B := FixedSqrt(A);
   C.Fixed := Round(Sqrt(A.Fixed * CFixed24Dot8OneAsSingle) - 0.5);
   CheckEquals(C.Fixed, B.Fixed);
  end;
end;


initialization
  RegisterTest(TTestFixedPoint16Dot16.Suite);
  RegisterTest(TTestFixedPoint24Dot8.Suite);

end.
