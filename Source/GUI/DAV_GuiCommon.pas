unit DAV_GuiCommon;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes;

type
  {$A1}
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

procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single); overload;
function HLSToRGB(const H, L, S: Single): TColor; overload;
procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single); overload;

implementation

uses
  Math;

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

procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single);
var
  M1, M2: Single;

  function HueToColorValue(Hue: Single): Single;
  begin
    Hue := Hue - Floor(Hue);

    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      Result := M2
    else
    if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      Result := M1;
  end;

begin
  if S = 0 then
  begin
    R := L;
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5
     then M2 := L * (1 + S)
     else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
end;

procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single);
var
  D, Cmax, Cmin: Single;
begin
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
    if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

function HLSToRGB(const H, L, S: Single): TColor;
var
  R, G, B: Single;
begin
  HLSToRGB(H, L, S, R, G, B);
  Result := (Round(R * 255) or (Round(G * 255) shl 8) or (Round(B * 255) shl 16));
end;

end.
