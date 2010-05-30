unit DAV_HalfFloat;

interface

{$I DAV_Compiler.inc}

type
  THalfFloat = Word;
  TDAVHalfFloatFixedArray = array [0..{$IFDEF ZeroArray}0{$ELSE}MaxInt{$ENDIF}] of THalfFloat;
  PDAVHalfFloatFixedArray = ^TDAVHalfFloatFixedArray;

function FastSingleToHalfFloat(const Value: Single): THalfFloat;
function FastHalfFloatToSingle(const Value: THalfFloat): Single;
function SingleToHalfFloat(const Value: Single): THalfFloat;
function HalfFloatToSingle(const Value: THalfFloat): Single;

// more information on this topic can be found here:
// http://www.fox-toolkit.org/ftp/fasthalffloatconversion.pdf

implementation

var
  GMantissaTable : array [0..2047] of Cardinal;
  GExponentTable : array [0..63] of Cardinal;
  GOffsetTable   : array [0..63] of Word;
  GBaseTable     : array [0..511] of Word;
  GShiftTable    : array [0..511] of Byte;

function FastSingleToHalfFloat(const Value: Single): THalfFloat;
{$IFNDEF XPUREPASCAL}
var
  IntCast : Integer absolute Value;
begin
 result := ((IntCast shr 16) and $8000) or
           ((((IntCast and $7F800000) - $38000000) shr 13) and $7C00) or
           ((IntCast shr 13) and $3FF);

end;
{$ELSE}
asm
 mov eax, Value
 shr eax, $10
 and eax, $8000

 mov edx, Value
 and edx, $7F800000
 sub edx, $38000000
 shr edx, 13
 and edx, $7C00

 or eax, edx

 mov edx, Value
 shr edx, 13
 and edx, $3FF
 or eax, edx
 mov result, ax
end;
{$ENDIF}

function FastHalfFloatToSingle(const Value: THalfFloat): Single;
{$IFNDEF XPUREPASCAL}
var
  IntCast : Integer absolute Result;
begin
 IntCast := ((Value and $8000) shl 16) or
            (((Value and $7C00) + $1C000) shl 13) or
            ((Value and $3FF) shl 13);
end;
{$ELSE}
asm
 mov cx, Value
 and cx, $8000
 shl ecx, 16

 xor edx, edx
 mov dx, Value
 and dx, $7C00
 add edx, $1C000
 shl edx, 13
 or ecx, edx

 and ax, $3FF
 shl eax, 13
 or eax, ecx
 mov result, eax
end;
{$ENDIF}

function SingleToHalfFloat(const Value: Single): THalfFloat;
var
  IntCast: Integer absolute Value;
begin
 result := GBaseTable[(IntCast shr 23) and $1FF] + ((IntCast and $7FFFFF) shr GShiftTable[(IntCast shr 23) and $1FF]);
end;

function HalfFloatToSingle(const Value: THalfFloat): Single;
var
  IntCast : Integer absolute Result;
begin
 IntCast := GMantissaTable[GOffsetTable[(Value shr 10)] + (Value and $3FF)] + GExponentTable[Value shr 10]
end;

function ConvertMantissa(I: Cardinal): Cardinal;
var
  m, e : Cardinal;
begin
 m := i shl 13;               // Zero pad mantissa bits
 e := 0;                      // Zero exponent
 while m and $00800000 = 0 do // While not normalized
  begin
   e := e - $00800000;        // Decrement exponent (1<<23)
   m := m shl 1;              // Shift mantissa
  end;
 m := m and not $00800000;    // Clear leading 1 bit
 e := e + $38800000;          // Adjust bias ((127-14)<<23)
 result := m or e;            // Return combined number
end;

procedure BuildTables;
var
  i : Cardinal;
  e : Integer;
begin
 // Mantissa Table
 GMantissaTable[0] := 0;
 for i := 1 to 1023    do GMantissaTable[i] := ConvertMantissa(i);
 for i := 1024 to 2047 do GMantissaTable[i] := $38000000 + ((i - 1024) shl 13);

 // Exponent Table
 GExponentTable[0] := 0;
 GExponentTable[31] := $47800000;
 GExponentTable[32] := $80000000;
 GExponentTable[63] := $C7800000;
 for i :=  1 to 30 do GExponentTable[i] := i shl 23;
 for i := 33 to 62 do GExponentTable[i] := $80000000 + (i - 32) shl 23;

 // Exponent Table
 GOffsetTable[ 0] := 0;
 GOffsetTable[32] := 0;
 for i :=  1 to 31 do GOffsetTable[i] := 1024;
 for i := 33 to 63 do GOffsetTable[i] := 1024;

 for i := 0 to 255 do
  begin
   e := i - 127;
   if (e < -24) then // Very small numbers map to zero
    begin
     GBaseTable[i or $000] := $0000;
     GBaseTable[i or $100] := $8000;
     GShiftTable[i or $000] := 24;
     GShiftTable[i or $100] := 24;
    end else
   if (e < -14) then // Small numbers map to denorms
    begin
     GBaseTable[i or $000] := ($0400 shr (18-e));
     GBaseTable[i or $100] := ($0400 shr (18-e)) or $8000;
     GShiftTable[i or $000] := -e-1;
     GShiftTable[i or $100] := -e-1;
    end else
   if (e <= 15) then // Normal numbers just lose precision
    begin
     GBaseTable[i or $000] := ((e+15) shl 10);
     GBaseTable[i or $100] := ((e+15) shl 10)  or  $8000;
     GShiftTable[i or $000] := 13;
     GShiftTable[i or $100] := 13;
     end else
   if (e < 128) then // Large numbers map to Infinity
    begin
     GBaseTable[i or $000] := $7C00;
     GBaseTable[i or $100] := $FC00;
     GShiftTable[i or $000] := 24;
     GShiftTable[i or $100] := 24;
    end
   else
    begin // Infinity and NaN's stay Infinity and NaN's
     GBaseTable[i or $000] := $7C00;
     GBaseTable[i or $100] := $FC00;
     GShiftTable[i or $000] := 13;
     GShiftTable[i or $100] := 13;
    end;
  end;
end;

initialization
  BuildTables;

end.
