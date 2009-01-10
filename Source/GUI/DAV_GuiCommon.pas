unit DAV_GuiCommon;

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TRGB24 = packed record
    B, G, R: Byte;
  end;
  TRGB24Array = packed array[0..MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

procedure Downsample2xBitmap32(var Bitmap: TBitmap);
procedure Downsample2xBitmap24(var Bitmap: TBitmap);
procedure Downsample3xBitmap32(var Bitmap: TBitmap);
procedure Downsample3xBitmap24(var Bitmap: TBitmap);
procedure Downsample4xBitmap32(var Bitmap: TBitmap);
procedure Downsample4xBitmap24(var Bitmap: TBitmap);
procedure Upsample2xBitmap32(var Bitmap: TBitmap);
procedure Upsample2xBitmap24(var Bitmap: TBitmap);
procedure Upsample3xBitmap32(var Bitmap: TBitmap);
procedure Upsample3xBitmap24(var Bitmap: TBitmap);
procedure Upsample4xBitmap32(var Bitmap: TBitmap);
procedure Upsample4xBitmap24(var Bitmap: TBitmap);

implementation

procedure Downsample2xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..2] of PRGB32Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 2) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := 0 to (Width  div 2) - 1 do
      begin
       Line[0, x].B := (Line[1, 2 * x].B + Line[2, 2 * x].B + Line[1, 2 * x + 1].B + Line[2, 2 * x + 1].B) div 4;
       Line[0, x].G := (Line[1, 2 * x].G + Line[2, 2 * x].G + Line[1, 2 * x + 1].G + Line[2, 2 * x + 1].G) div 4;
       Line[0, x].R := (Line[1, 2 * x].R + Line[2, 2 * x].R + Line[1, 2 * x + 1].R + Line[2, 2 * x + 1].R) div 4;
       Line[0, x].A := (Line[1, 2 * x].A + Line[2, 2 * x].A + Line[1, 2 * x + 1].A + Line[2, 2 * x + 1].A) div 4;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Downsample2xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..2] of PRGB24Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 2) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := 0 to (Width  div 2) - 1 do
      begin
       Line[0, x].B := (Line[1, 2 * x].B + Line[2, 2 * x].B + Line[1, 2 * x + 1].B + Line[2, 2 * x + 1].B) div 4;
       Line[0, x].G := (Line[1, 2 * x].G + Line[2, 2 * x].G + Line[1, 2 * x + 1].G + Line[2, 2 * x + 1].G) div 4;
       Line[0, x].R := (Line[1, 2 * x].R + Line[2, 2 * x].R + Line[1, 2 * x + 1].R + Line[2, 2 * x + 1].R) div 4;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Downsample3xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  x3   : Integer;
  Line : Array [0..3] of PRGB32Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 3) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[3 * y];
     Line[2] := Scanline[3 * y + 1];
     Line[3] := Scanline[3 * y + 2];
     for x := 0 to (Width  div 3) - 1 do
      begin
       x3 := 3 * x;
       Line[0, x].B := (Line[1, x3    ].B + Line[2, x3    ].B + Line[3, x3    ].B +
                        Line[1, x3 + 1].B + Line[2, x3 + 1].B + Line[3, x3 + 1].B +
                        Line[1, x3 + 2].B + Line[2, x3 + 2].B + Line[3, x3 + 2].B) div 9;
       Line[0, x].G := (Line[1, x3    ].G + Line[2, x3    ].G + Line[3, x3    ].G +
                        Line[1, x3 + 1].G + Line[2, x3 + 1].G + Line[3, x3 + 1].G +
                        Line[1, x3 + 2].G + Line[2, x3 + 2].G + Line[3, x3 + 2].G) div 9;
       Line[0, x].R := (Line[1, x3    ].R + Line[2, x3    ].R + Line[3, x3    ].R +
                        Line[1, x3 + 1].R + Line[2, x3 + 1].R + Line[3, x3 + 1].R +
                        Line[1, x3 + 2].R + Line[2, x3 + 2].R + Line[3, x3 + 2].R) div 9;
       Line[0, x].A := (Line[1, x3    ].A + Line[2, x3    ].A + Line[3, x3    ].A +
                        Line[1, x3 + 1].A + Line[2, x3 + 1].A + Line[3, x3 + 1].A +
                        Line[1, x3 + 2].A + Line[2, x3 + 2].A + Line[3, x3 + 2].A) div 9;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Downsample3xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  x3   : Integer;
  Line : Array [0..3] of PRGB24Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 3) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[3 * y];
     Line[2] := Scanline[3 * y + 1];
     Line[3] := Scanline[3 * y + 2];
     for x := 0 to (Width  div 3) - 1 do
      begin
       x3 := 3 * x;
       Line[0, x].B := (Line[1, x3    ].B + Line[2, x3    ].B + Line[3, x3    ].B +
                        Line[1, x3 + 1].B + Line[2, x3 + 1].B + Line[3, x3 + 1].B +
                        Line[1, x3 + 2].B + Line[2, x3 + 2].B + Line[3, x3 + 2].B) div 9;
       Line[0, x].G := (Line[1, x3    ].G + Line[2, x3    ].G + Line[3, x3    ].G +
                        Line[1, x3 + 1].G + Line[2, x3 + 1].G + Line[3, x3 + 1].G +
                        Line[1, x3 + 2].G + Line[2, x3 + 2].G + Line[3, x3 + 2].G) div 9;
       Line[0, x].R := (Line[1, x3    ].R + Line[2, x3    ].R + Line[3, x3    ].R +
                        Line[1, x3 + 1].R + Line[2, x3 + 1].R + Line[3, x3 + 1].R +
                        Line[1, x3 + 2].R + Line[2, x3 + 2].R + Line[3, x3 + 2].R) div 9;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Downsample4xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  x4   : Integer;
  Line : Array [0..4] of PRGB32Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 4) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 4];
     Line[2] := Scanline[y * 4 + 1];
     Line[3] := Scanline[y * 4 + 2];
     Line[4] := Scanline[y * 4 + 3];
     for x := 0 to (Width  div 4) - 1 do
      begin
       x4 := 4 * x;
       Line[0, x].B := (Line[1, x4].B + Line[1, x4 + 1].B + Line[1, x4 + 2].B + Line[1, x4 + 3].B +
                        Line[2, x4].B + Line[2, x4 + 1].B + Line[2, x4 + 2].B + Line[2, x4 + 3].B +
                        Line[3, x4].B + Line[3, x4 + 1].B + Line[3, x4 + 2].B + Line[3, x4 + 3].B +
                        Line[4, x4].B + Line[4, x4 + 1].B + Line[4, x4 + 2].B + Line[4, x4 + 3].B) div 16;
       Line[0, x].G := (Line[1, x4].G + Line[1, x4 + 1].G + Line[1, x4 + 2].G + Line[1, x4 + 3].G +
                        Line[2, x4].G + Line[2, x4 + 1].G + Line[2, x4 + 2].G + Line[2, x4 + 3].G +
                        Line[3, x4].G + Line[3, x4 + 1].G + Line[3, x4 + 2].G + Line[3, x4 + 3].G +
                        Line[4, x4].G + Line[4, x4 + 1].G + Line[4, x4 + 2].G + Line[4, x4 + 3].G) div 16;
       Line[0, x].R := (Line[1, x4].R + Line[1, x4 + 1].R + Line[1, x4 + 2].R + Line[1, x4 + 3].R +
                        Line[2, x4].R + Line[2, x4 + 1].R + Line[2, x4 + 2].R + Line[2, x4 + 3].R +
                        Line[3, x4].R + Line[3, x4 + 1].R + Line[3, x4 + 2].R + Line[3, x4 + 3].R +
                        Line[4, x4].R + Line[4, x4 + 1].R + Line[4, x4 + 2].R + Line[4, x4 + 3].R) div 16;
       Line[0, x].A := (Line[1, x4].A + Line[1, x4 + 1].A + Line[1, x4 + 2].A + Line[1, x4 + 3].A +
                        Line[2, x4].A + Line[2, x4 + 1].A + Line[2, x4 + 2].A + Line[2, x4 + 3].A +
                        Line[3, x4].A + Line[3, x4 + 1].A + Line[3, x4 + 2].A + Line[3, x4 + 3].A +
                        Line[4, x4].A + Line[4, x4 + 1].A + Line[4, x4 + 2].A + Line[4, x4 + 3].A) div 16;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Downsample4xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  x4   : Integer;
  Line : Array [0..4] of PRGB24Array;
begin
{$IFNDEF FPC}
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 4) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 4];
     Line[2] := Scanline[y * 4 + 1];
     Line[3] := Scanline[y * 4 + 2];
     Line[4] := Scanline[y * 4 + 3];
     for x := 0 to (Width  div 4) - 1 do
      begin
       x4 := 4 * x;
       Line[0, x].B := (Line[1, x4].B + Line[1, x4 + 1].B + Line[1, x4 + 2].B + Line[1, x4 + 3].B +
                        Line[2, x4].B + Line[2, x4 + 1].B + Line[2, x4 + 2].B + Line[2, x4 + 3].B +
                        Line[3, x4].B + Line[3, x4 + 1].B + Line[3, x4 + 2].B + Line[3, x4 + 3].B +
                        Line[4, x4].B + Line[4, x4 + 1].B + Line[4, x4 + 2].B + Line[4, x4 + 3].B) div 16;
       Line[0, x].G := (Line[1, x4].G + Line[1, x4 + 1].G + Line[1, x4 + 2].G + Line[1, x4 + 3].G +
                        Line[2, x4].G + Line[2, x4 + 1].G + Line[2, x4 + 2].G + Line[2, x4 + 3].G +
                        Line[3, x4].G + Line[3, x4 + 1].G + Line[3, x4 + 2].G + Line[3, x4 + 3].G +
                        Line[4, x4].G + Line[4, x4 + 1].G + Line[4, x4 + 2].G + Line[4, x4 + 3].G) div 16;
       Line[0, x].R := (Line[1, x4].R + Line[1, x4 + 1].R + Line[1, x4 + 2].R + Line[1, x4 + 3].R +
                        Line[2, x4].R + Line[2, x4 + 1].R + Line[2, x4 + 2].R + Line[2, x4 + 3].R +
                        Line[3, x4].R + Line[3, x4 + 1].R + Line[3, x4 + 2].R + Line[3, x4 + 3].R +
                        Line[4, x4].R + Line[4, x4 + 1].R + Line[4, x4 + 2].R + Line[4, x4 + 3].R) div 16;
      end;
    end;
  end;
{$ENDIF}
end;

procedure Upsample2xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  x2   : Integer;
  Line : Array [0..2] of PRGB32Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  begin
   assert(PixelFormat = pf32bit);

   // first stage
   for y := (Height div 2) - 1 downto 0 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := (Width  div 2) - 1 downto 0 do
      begin
       x2 := 2 * x;
       Line[1, x2    ].B := Line[0, x].B;
       Line[2, x2    ].B := Line[0, x].B;
       Line[1, x2 + 1].B := Line[0, x].B;
       Line[2, x2 + 1].B := Line[0, x].B;
       Line[1, x2    ].G := Line[0, x].G;
       Line[2, x2    ].G := Line[0, x].G;
       Line[1, x2 + 1].G := Line[0, x].G;
       Line[2, x2 + 1].G := Line[0, x].G;
       Line[1, x2    ].R := Line[0, x].R;
       Line[2, x2    ].R := Line[0, x].R;
       Line[1, x2 + 1].R := Line[0, x].R;
       Line[2, x2 + 1].R := Line[0, x].R;
       Line[1, x2    ].A := Line[0, x].A;
       Line[2, x2    ].A := Line[0, x].A;
       Line[1, x2 + 1].A := Line[0, x].A;
       Line[2, x2 + 1].A := Line[0, x].A;
      end;
    end;
  end;
 {$ENDIF}
end;

procedure Upsample2xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  x2   : Integer;
  Line : Array [0..2] of PRGB24Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  begin
   assert(PixelFormat = pf24bit);

   // first stage
   for y := (Height div 2) - 1 downto 0 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := (Width  div 2) - 1 downto 0 do
      begin
       x2 := 2 * x;
       Line[1, x2    ].B := Line[0, x].B;
       Line[2, x2    ].B := Line[0, x].B;
       Line[1, x2 + 1].B := Line[0, x].B;
       Line[2, x2 + 1].B := Line[0, x].B;
       Line[1, x2    ].G := Line[0, x].G;
       Line[2, x2    ].G := Line[0, x].G;
       Line[1, x2 + 1].G := Line[0, x].G;
       Line[2, x2 + 1].G := Line[0, x].G;
       Line[1, x2    ].R := Line[0, x].R;
       Line[2, x2    ].R := Line[0, x].R;
       Line[1, x2 + 1].R := Line[0, x].R;
       Line[2, x2 + 1].R := Line[0, x].R;
      end;
    end;
  end;
 {$ENDIF}
end;

procedure Upsample3xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  x3   : Integer;
  Line : Array [0..3] of PRGB32Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  begin
   assert(PixelFormat = pf32bit);

   // first stage
   for y := (Height div 3) - 1 downto 0 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 3];
     Line[2] := Scanline[y * 3 + 1];
     Line[3] := Scanline[y * 3 + 2];
     for x := (Width  div 3) - 1 downto 0 do
      begin
       x3 := 3 * x;
       Line[1, x3    ].B := Line[0, x].B;
       Line[2, x3    ].B := Line[0, x].B;
       Line[3, x3    ].B := Line[0, x].B;
       Line[1, x3 + 1].B := Line[0, x].B;
       Line[2, x3 + 1].B := Line[0, x].B;
       Line[3, x3 + 1].B := Line[0, x].B;
       Line[1, x3 + 2].B := Line[0, x].B;
       Line[2, x3 + 2].B := Line[0, x].B;
       Line[3, x3 + 2].B := Line[0, x].B;
       Line[1, x3    ].G := Line[0, x].G;
       Line[2, x3    ].G := Line[0, x].G;
       Line[3, x3    ].G := Line[0, x].G;
       Line[1, x3 + 1].G := Line[0, x].G;
       Line[2, x3 + 1].G := Line[0, x].G;
       Line[3, x3 + 1].G := Line[0, x].G;
       Line[1, x3 + 2].G := Line[0, x].G;
       Line[2, x3 + 2].G := Line[0, x].G;
       Line[3, x3 + 2].G := Line[0, x].G;
       Line[1, x3    ].R := Line[0, x].R;
       Line[2, x3    ].R := Line[0, x].R;
       Line[3, x3    ].R := Line[0, x].R;
       Line[1, x3 + 1].R := Line[0, x].R;
       Line[2, x3 + 1].R := Line[0, x].R;
       Line[3, x3 + 1].R := Line[0, x].R;
       Line[1, x3 + 2].R := Line[0, x].R;
       Line[2, x3 + 2].R := Line[0, x].R;
       Line[3, x3 + 2].R := Line[0, x].R;
       Line[1, x3    ].A := Line[0, x].A;
       Line[2, x3    ].A := Line[0, x].A;
       Line[3, x3    ].A := Line[0, x].A;
       Line[1, x3 + 1].A := Line[0, x].A;
       Line[2, x3 + 1].A := Line[0, x].A;
       Line[3, x3 + 1].A := Line[0, x].A;
       Line[1, x3 + 2].A := Line[0, x].A;
       Line[2, x3 + 2].A := Line[0, x].A;
       Line[3, x3 + 2].A := Line[0, x].A;
      end;
    end;
  end;
 {$ENDIF}
end;

procedure Upsample3xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  x3   : Integer;
  Line : Array [0..3] of PRGB24Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  begin
   assert(PixelFormat = pf24bit);

   // first stage
   for y := (Height div 3) - 1 downto 0 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 3];
     Line[2] := Scanline[y * 3 + 1];
     Line[3] := Scanline[y * 3 + 2];
     for x := (Width  div 3) - 1 downto 0 do
      begin
       x3 := 3 * x;
       Line[1, x3    ].B := Line[0, x].B;
       Line[2, x3    ].B := Line[0, x].B;
       Line[3, x3    ].B := Line[0, x].B;
       Line[1, x3 + 1].B := Line[0, x].B;
       Line[2, x3 + 1].B := Line[0, x].B;
       Line[3, x3 + 1].B := Line[0, x].B;
       Line[1, x3 + 2].B := Line[0, x].B;
       Line[2, x3 + 2].B := Line[0, x].B;
       Line[3, x3 + 2].B := Line[0, x].B;
       Line[1, x3    ].G := Line[0, x].G;
       Line[2, x3    ].G := Line[0, x].G;
       Line[3, x3    ].G := Line[0, x].G;
       Line[1, x3 + 1].G := Line[0, x].G;
       Line[2, x3 + 1].G := Line[0, x].G;
       Line[3, x3 + 1].G := Line[0, x].G;
       Line[1, x3 + 2].G := Line[0, x].G;
       Line[2, x3 + 2].G := Line[0, x].G;
       Line[3, x3 + 2].G := Line[0, x].G;
       Line[1, x3    ].R := Line[0, x].R;
       Line[2, x3    ].R := Line[0, x].R;
       Line[3, x3    ].R := Line[0, x].R;
       Line[1, x3 + 1].R := Line[0, x].R;
       Line[2, x3 + 1].R := Line[0, x].R;
       Line[3, x3 + 1].R := Line[0, x].R;
       Line[1, x3 + 2].R := Line[0, x].R;
       Line[2, x3 + 2].R := Line[0, x].R;
       Line[3, x3 + 2].R := Line[0, x].R;
      end;
    end;
  end;
 {$ENDIF}
end;

procedure Upsample4xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  i, j : Integer;
  Line : Array [0..4] of PRGB32Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  for y := (Height div 4) - 1 downto 0 do
   begin
    assert(PixelFormat = pf32bit);

    Line[0] := Scanline[y];
    Line[1] := Scanline[y * 4];
    Line[2] := Scanline[y * 4 + 1];
    Line[3] := Scanline[y * 4 + 2];
    Line[4] := Scanline[y * 4 + 3];
    for x := (Width  div 4) - 1 downto 0 do
     for i := 1 to 4 do
      for j := 0 to 3 do
       begin
        Line[i, 4 * x + j].B := Line[0, x].B;
        Line[i, 4 * x + j].G := Line[0, x].G;
        Line[i, 4 * x + j].R := Line[0, x].R;
        Line[i, 4 * x + j].A := Line[0, x].A;
       end;
   end;
 {$ENDIF}
end;

procedure Upsample4xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  i, j : Integer;
  Line : Array [0..4] of PRGB32Array;
begin
 {$IFNDEF FPC}
 with Bitmap do
  for y := (Height div 4) - 1 downto 0 do
   begin
    assert(PixelFormat = pf32bit);

    Line[0] := Scanline[y];
    Line[1] := Scanline[y * 4];
    Line[2] := Scanline[y * 4 + 1];
    Line[3] := Scanline[y * 4 + 2];
    Line[4] := Scanline[y * 4 + 3];
    for x := (Width  div 4) - 1 downto 0 do
     for i := 1 to 4 do
      for j := 0 to 3 do
       begin
        Line[i, 4 * x + j].B := Line[0, x].B;
        Line[i, 4 * x + j].G := Line[0, x].G;
        Line[i, 4 * x + j].R := Line[0, x].R;
       end;
   end;
 {$ENDIF}
end;

end.
