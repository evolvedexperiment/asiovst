unit DAV_ZLib;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes;

type
  TZCompressionMethod = (cmDeflate = 8);
  TZCompressionLevel = (clFastest = 0, clFast = 1, clDefault = 2,
    clMaximumCompression = 3);

  TBlockFormatType = (bfStored = 0, bfFixedHuffman = 1, bfDynamicHuffman = 2,
    bfReserved = 3);

  TCustomZStream = class(TStream)
  private
    FStream            : TStream;
    FStreamPos         : Int64;
  protected
    FCompressionMethod : TZCompressionMethod;
    FCompressionInfo   : Byte;
    FCompressionLevel  : TZCompressionLevel;
    FDictionaryPresent : Boolean;
    FDictionary        : Cardinal;
    FAdler32           : Cardinal;
  protected
    constructor Create(Stream: TStream);
  end;

(*
  TInflateBlock = class(TObject)
  private
    function ReadBitsFromStream(Bits: Byte): Byte;
    procedure ReadStoredFromStream;
  public
    constructor Create(Source: TStream; BitOffset: Byte);
    destructor Destroy; override;

    procedure ReadFromStream;

    property BlockFormatType: TBlockFormatType read FBlockFormatType;
    property IsFinalBlock: Boolean read FIsFinalBlock;
  end;
*)

  TZDecompressionStream = class(TCustomZStream)
  private
    procedure ReadHeader(Stream: TStream);
    procedure ReadAdler32(Stream: TStream);
    function GetBytesInBuffer: Cardinal;
    procedure BuildFixedHuffmanTables;
  protected
    FBuffer       : TMemoryStream;
    FBitPosition  : Integer;
    FCurrentByte  : Byte;
    FIsFinalBlock : Boolean;

    function ReadBitsFromStream8(Bits: Byte): Byte;
    function ReadBitsFromStream16(Bits: Byte): Word;
    procedure ReadBlockFromStream;
    procedure ReadStoredBlockFromStream;
    procedure ReadFixedHuffmanBlockFromStream;
    procedure ReadDynamicHuffmanBlockFromStream;
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;

    property CompressionMethod: TZCompressionMethod read FCompressionMethod;
    property CompressionInfo: Byte read FCompressionInfo;
    property CompressionLevel: TZCompressionLevel read FCompressionLevel;
    property DictionaryPresent: Boolean read FDictionaryPresent;
  end;

implementation

uses
  Math, SysUtils;

type
  PInflateHuft = ^TInflateHuft;
  TInflateHuft = record
    Exop : Byte;     // number of extra bits or operation
    Bits : Byte;     // number of bits in this code or subcode
    Base : Cardinal; // literal, length base, or distance base or table offset
  end;

const
  CCopyLengths: array [0..30] of Cardinal = (
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59,
    67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

  CCopyLiteralExtraBits: array [0..28] of Cardinal = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
    5, 5, 5, 5, 0);

  CCopyDistanceOffsets: array [0..29] of Cardinal = (
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385,
    513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
  );

  CCopyDistanceExtraBits: array [0..29] of Cardinal = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7,
    7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
  );

var
  GFixedLiteralTable: PInflateHuft;
  GFixedDistanceTable: PInflateHuft;


{ TCustomZStream }

constructor TCustomZStream.Create(Stream: TStream);
begin
 inherited Create;
 FStream := Stream;
 FStreamPos := Stream.Position;
end;


{ TZDecompressionStream }

constructor TZDecompressionStream.Create(Source: TStream);
begin
 inherited Create(Source);
 ReadHeader(Source);

 FBitPosition := 0;
 FBuffer := TMemoryStream.Create;

// ReadAdler32(Source);
end;

destructor TZDecompressionStream.Destroy;
begin
 // dispose buffer
 FreeAndNil(FBuffer);

 inherited;
end;

function TZDecompressionStream.GetBytesInBuffer: Cardinal;
begin
 Result := FBuffer.Size - FBuffer.Position;
end;

procedure TZDecompressionStream.ReadHeader(Stream: TStream);
var
  Values : array [0..1] of Byte;
begin
 with Stream do
  begin
   if Size - Position < 2
    then raise Exception.Create('Stream size too small');

   // read CMF value
   Read(Values[0], 1);

   // read and check compression method
   if (Values[0] and $F) <> 8
    then raise Exception.Create('Only the deflate compression method is supported so far');
   FCompressionMethod := cmDeflate;

   // read and check compression info
   FCompressionInfo := (Values[0] shr 4);
   if FCompressionInfo > 7
    then raise Exception.Create('Compression info values above 7 are not allowed');

   // read FLG value
   Read(Values[1], 1);

   // check header
   if (Values[0] * 256 + Values[1]) mod 31 <> 0
    then raise Exception.Create('The header seems to be corrupted');

   // check if a dictionary is present
   FDictionaryPresent := (Values[1] and (1 shl 5)) <> 0;

   // read compression level
   FCompressionLevel := TZCompressionLevel(Values[1] shr 6);

   // eventually read dictionary
   if FDictionaryPresent
    then Read(FDictionary, 4)
    else FDictionary := 0;
  end;
end;

procedure TZDecompressionStream.ReadAdler32(Stream: TStream);
begin
 with Stream do
  begin
   // seek end of the stream
   Seek(-4, soFromEnd);

   // read adler 32 value
   Read(FAdler32, 4);
  end;
end;

function TZDecompressionStream.ReadBitsFromStream8(Bits: Byte): Byte;
begin
 Assert(Bits <= 8);

 if Bits = 0 then Exit;

 if FBitPosition = 0
  then FStream.Read(FCurrentByte, 1);

 if FBitPosition + Bits > 8 then
  begin
   Result := FCurrentByte shr FBitPosition;
   Bits := Bits - (8 - FBitPosition);
   FStream.Read(FCurrentByte, 1);
   Result := Result or (FCurrentByte and (1 shl (Bits + 1) - 1));
   FBitPosition := Bits;
   Exit;
  end;

 Result := (FCurrentByte shr FBitPosition) and (1 shl (Bits + 1) - 1);
 FBitPosition := (FBitPosition + Bits) mod 8;



(*
 while Bits > 0 do
  begin
   if (FBitPosition mod 8 = 0) then
    begin
     FStream.Read(FCurrentByte, 1);
     FBitPosition := 0;
    end;

   Result := Result or (((FCurrentByte shr FBitPosition) and 1) shl Shift);
   Dec(Bits);
   Inc(FBitPosition);
   Inc(Shift);
  end;
*)
end;

function TZDecompressionStream.ReadBitsFromStream16(Bits: Byte): Word;
begin
 Assert(Bits <= 9);

 if Bits = 0 then Exit;

 if FBitPosition = 0
  then FStream.Read(FCurrentByte, 1);

 if FBitPosition + Bits > 8 then
  begin
   Result := FCurrentByte shr FBitPosition;
   Bits := Bits - FBitPosition;
   FStream.Read(FCurrentByte, 1);
   Result := Result or (FCurrentByte and (1 shl (Bits + 1) - 1));
   FBitPosition := Bits mod 8;
   Exit;
  end;

 Result := (FCurrentByte shr FBitPosition) and (1 shl (Bits + 1) - 1);
 FBitPosition := (FBitPosition + Bits) mod 8;
end;

procedure TZDecompressionStream.ReadBlockFromStream;
var
  BlockFormatType : TBlockFormatType;
begin
 FBuffer.Size := 0;
 FIsFinalBlock := ReadBitsFromStream8(1) <> 0;
 BlockFormatType := TBlockFormatType(ReadBitsFromStream8(2));

 case BlockFormatType of
  bfStored         : ReadStoredBlockFromStream;
  bfFixedHuffman   : ReadFixedHuffmanBlockFromStream;
  bfDynamicHuffman : ReadDynamicHuffmanBlockFromStream;
 end;
end;

procedure TZDecompressionStream.ReadDynamicHuffmanBlockFromStream;
begin

end;

procedure TZDecompressionStream.ReadStoredBlockFromStream;
var
  Length : Word;
  InvertedLength : Word;
begin
 FStream.Read(Length, 2);
 FStream.Read(InvertedLength, 2);
 if Length <> (not InvertedLength)
  then raise Exception.Create('Stored block size error!');

 FBuffer.CopyFrom(FStream, Length);
 FBuffer.Position := FBuffer.Position - (Length);
end;

procedure TZDecompressionStream.ReadFixedHuffmanBlockFromStream;
var
  BitValue     : Word;
  LiteralValue : Word;
  ByteValue    : Byte;
begin
// BuildFixedHuffmanTables;

 repeat
  // read current value
  BitValue := ReadBitsFromStream8(7);
  if BitValue > 23 then
   begin
    BitValue := (BitValue shl 1) or ReadBitsFromStream8(1);

    if (BitValue in [48..192])then
     begin
      LiteralValue := BitValue - 48;
     end else
    if (BitValue in [193..199])then
     begin
      LiteralValue := BitValue + 87;
     end
    else
     begin
      BitValue := (BitValue shl 1) or ReadBitsFromStream8(1);
      LiteralValue := BitValue - 400 + 144;
     end;
   end
  else LiteralValue := 256 + BitValue;

  if LiteralValue < 256 then
   begin
    ByteValue := LiteralValue;
    FBuffer.Write(ByteValue, 1);
   end;


 until LiteralValue = 256;

end;

procedure TZDecompressionStream.BuildFixedHuffmanTables;
var
  Index              : Integer;
  HuffmanTableLength : array [0..287] of Byte;
begin
 if not Assigned(GFixedLiteralTable) then
  begin
   for Index := 0   to 143 do HuffmanTableLength[Index] := 8;
   for Index := 144 to 255 do HuffmanTableLength[Index] := 9;
   for Index := 256 to 279 do HuffmanTableLength[Index] := 7;
   for Index := 280 to 287 do HuffmanTableLength[Index] := 8;
  end;

end;

(*
function BuildHuffmanTables(
  const B: array of Cardinal;
  N, S: Cardinal;
  const D, Extra: array of Cardinal;
  Temp: PPInflateHuft;
  var M: Cardinal;
  var HP: array of TInflateHuft;
  var HN: Cardinal;
  var V: array of Cardinal): Integer;

// Given a list of code lengths and a maximum table size, make a set of tables
// to decode that set of codes. Returns Z_OK on success, Z_BUF_ERROR if the
// given code set is incomplete (the tables are still built in this case),
// Z_DATA_ERROR if the input is invalid (an over-subscribed set of lengths),
// or Z_MEM_ERROR if not enough memory.
//
// Input pareters:
// B contains the code lenths in bits (all assumed <= BMAX)
// N is the number of codes (<= NMAX)
// S is the number of simple valued codes (0..S - 1)
// D contains a list of base values for non-simple codes
// Extra carries a list of extra bits for non-simple codes
//
// Output parameters:
// Temp points to the starting table
// M receives the maxium lookup bits (actual space for trees)
// HP receives the Huffman tables
// while HN decribes how many of HP is actually used
// finally V is a working area which receives values in order of bit length

var
  A    : Cardinal;                            // counter for codes of length K
  C    : array [0..BMAX] of Cardinal;         // bit length count table
  F    : Cardinal;                            // I repeats in table every F entries
  G    : Integer;                             // maximum code Length
  H    : Integer;                             // table Level
  I    : Cardinal;                            // counter, current code
  J    : Cardinal;                            // counter
  K    : Integer;                             // number of bits in current code
  L    : Integer;                             // bits per table (returned in M)
  Mask : Cardinal;                            // (1 shl W) - 1, to avoid cc - O bug on HP
  P    : PCardinal;                           // pointer into C[], B[], or V[]
  Q    : PInflateHuft;                        // points to current table
  R    : TInflateHuft;                        // table entry for structure assignment
  U    : array [0..BMAX - 1] of PInflateHuft; // table stack
  W    : Integer;                             // bits before this table = (L * H)
  X    : array [0..BMAX] of Cardinal;         // bit offsets, then code stack
  XP   : PCardinal;                           // pointer into X
  Y    : Integer;                             // number of dummy codes added
  Z    : Cardinal;                            // number of entries in current table

begin
  // generate counts for each bit length
  FillChar(C, SizeOf(C), 0);

  // assume all entries <= BMAX
  for I := 0 to N - 1 do Inc(C[B[I]]);

  // nil input -> all zero length codes
  if C[0] = N then
  Begin
    Temp^ := nil;
    M := 0 ;
    Result := Z_OK;
    Exit;
  end ;

  // find minimum and maximum length, bound [M] by those
  L := M;
  for J := 1 to BMAX do
    if C[J] <> 0 then Break;
  // minimum code Length
  K := J ;
  if Cardinal(L) < J then L := J;
  for I := BMAX downto 1 do
    if C[I] <> 0 then Break;
  // maximum code length
  G := I ;
  if Cardinal(L) > I then L := I;
  M := L;

  // adjust last length count to fill out codes if needed
  Y := 1 shl J;
  while J < I do
  begin
    Dec(Y, C[J]);
    if Y < 0 then
    begin
      // bad input: more codes than bits
      Result := Z_DATA_ERROR;
      Exit;
    end ;
    Inc(J);
    Y := Y shl 1;
  end;
  Dec (Y, C[I]);
  if Y < 0 then
  begin
    // bad input: more codes than bits
    Result := Z_DATA_ERROR;
    Exit;
  end;
  Inc(C[I], Y);

  // generate starting offsets into the value table for each length
  X[1] := 0;
  J := 0;

  P := @C[1];
  XP := @X[2];
  // note that I = G from above
  Dec(I);
  while (I > 0) do
  begin
    Inc(J, P^);
    XP^ := J;
    Inc(P);
    Inc(XP);
    Dec(I);
  end;

  // make a table of values in order of bit lengths
  for I := 0 to N - 1 do
  begin
    J := B[I];
    if J <> 0 then
    begin
      V[X[J]] := I;
      Inc(X[J]);
    end;
  end;
  // set N to Length of V
  N := X[G];

  // generate the Huffman codes and for each make the table entries
  I := 0;
  // first Huffman code is zero
  X[0] := 0;
  // grab values in bit order
  P := @V;
  // no tables yet -> Level - 1
  H := -1;
  // bits decoded = (L * H)
  W := -L;

  U[0] := nil;
  Q := nil;
  Z := 0;

  // go through the bit lengths (K already is bits in shortest code)
  while K <= G Do
  begin
    A := C[K];
    while A <> 0 Do
    begin
      Dec(A);
      // here I is the Huffman code of length K bits for value P^
      // make tables up to required level
      while K > W + L do
      begin
        Inc(H);
        // add bits already decoded, previous table always L Bits
        Inc(W, L);
        // compute minimum size table less than or equal to L bits
        Z := G - W;
        if Z > Cardinal(L) then Z := L;

        // try a K - W bit table
        J := K - W;
        F := 1 shl J;
        // too few codes for K - W bit table
        if F > A + 1 then
        begin
          // deduct codes from patterns left
          Dec(F,A + 1);
          XP := @C[K];
          if J < Z then
          begin
            Inc(J);
            while J < Z do
            begin
              // try smaller tables up to Z bits
              F := F shl 1;
              Inc(XP);
              // enough codes to use up J Bits
              if F <= XP^ then Break;
              // else deduct codes from patterns
              Dec(F, XP^);
              Inc(J);
            end;
          end;
        end;

        // table entries for J-bit table
        Z := 1 shl J;
        // allocate new table (note: doesn't matter for fixed)
        if HN + Z > MANY then
        begin
          Result := Z_MEM_ERROR;
          Exit;
        end;

        Q := @HP[HN];
        U[H] := Q;
        Inc(HN, Z);

        // connect to last table, if there is one
        if H <> 0 then
        begin
          // save pattern for backing up
          X[H] := I;
          // bits to dump before this table
          R.Bits := L;
          // bits in this table
          R.exop := J;
          J := I shr (W - L);
          R.Base := (Cardinal(Q) - Cardinal(U[H - 1]) ) div SizeOf(Q^) - J;
          // connect to last table
          PHuftField(U[H - 1])[J] := R;
        end
        else
          // first table is returned result
          Temp^ := Q;
      end;

      // set up table entry in R
      R.Bits := Byte(K - W);

      // out of values -> invalid code
      if Cardinal(P) >= Cardinal(@V[N]) then R.exop := 128 + 64
                                        else
        if P^ < S then
        begin
          // 256 is end-of-block code
          if P^ < 256 then R.exop := 0
                      else R.exop := 32 + 64;
          // simple code is just the value
          R.Base := P^;
          Inc(P);
        end
        else
        begin
          // non-simple -> look up in lists
          R.exop := Byte(Extra[P^ - S] + 16 + 64);
          R.Base := D[P^ - S];
          Inc (P);
        end;

      // fill xode-like entries with R
      F := 1 shl (K - W);
      J := I shr W;
      while J < Z do
      begin
        PHuftField(Q)[J] := R;
        Inc(J, F);
      end;

      // backwards increment the K-bit code I
      J := 1 shl (K - 1) ;
      while (I and J) <> 0 do
      begin
        I := I xor J;
        J := J shr 1
      end;
      I := I xor J;

      // backup over finished tables
      // needed on HP, cc -O bug
      Mask := (1 shl W) - 1;
      while (I and Mask) <> X[H] do
      begin
        // don't need to update Q
        Dec(H);
        Dec(W, L);
        Mask := (1 shl W) - 1;
      end;
    end;
    Inc(K);
  end;

  // Return Z_BUF_ERROR if we were given an incomplete table
  if (Y <> 0) and (G <> 1) then Result := Z_BUF_ERROR
                           else Result := Z_OK;
end;
*)


function TZDecompressionStream.Read(var Buffer; Count: Integer): Integer;
var
  Value         : Byte;
  BytesCopied   : Cardinal;
  BytesInBuffer : Cardinal;
begin
 Result := 0;
 while Count > 0 do
  begin
   // check whether buffer contains data
   if FBuffer.Size = 0
    then ReadBlockFromStream;

   // check if decoded bytes are already stored in the buffer
   BytesInBuffer := FBuffer.Size - FBuffer.Position;
   if BytesInBuffer > 0 then
    begin
     // read bytes from the buffer
     BytesCopied := FBuffer.Read(Buffer, Min(BytesInBuffer, Count));
     Count := Count - BytesCopied;
     Result := Result + BytesCopied;

     // eventually clear the buffer in case the buffer has been read entirely
     if FBuffer.Position = FBuffer.Size then FBuffer.Clear;
    end;
  end;

(*
 with TInflateBlock.Create(FStream, 0) do
  try
   ReadFromStream;

  finally
   Free;
  end;
*)

end;

function TZDecompressionStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
 if (Origin = soFromBeginning) and (Offset = 0) then
  begin
   // reset decompression
  end else
 if (Origin = soFromCurrent) and (Offset >= 0) then
  begin
   // locate position
  end else
 if (Offset = 0) and (Origin = soFromEnd) then
  begin
   // locate end of stream
  end
 else raise Exception.Create('Invalid seek operation');

 // not implemented yet
 // result := ?
end;

function TZDecompressionStream.Write(const Buffer; Count: Integer): Integer;
begin
 raise Exception.Create('The decompression stream is read-only!');
end;

(*
repeat
  read block header from input stream.

  if stored with no compression then
   begin
    skip any remaining bits in current partially
    processed byte
    read LEN and NLEN (see next section)
    copy LEN bytes of data to output
   end else
  if compressed with dynamic Huffman codes then
   begin
    read representation of code trees (see
    subsection below)
   end;
  loop (until end of block code recognized)
   begin
    decode literal/length value from input stream
    if value < 256
     then copy value (literal byte) to output stream
     else
    if value = end of block (256)
     then break from loop
     else (value = 257..285)
    decode distance from input stream
    move backwards distance bytes in the output
    stream, and copy length bytes from this
    position to the output stream.
   end loop
until last block
*)


(*
{ TInflateBlock }

constructor TInflateBlock.Create(Source: TStream; BitOffset: Byte);
begin
 inherited Create;
 FBuffer := TMemoryStream.Create;
 FSource := Source;
 FBitPosition := BitOffset;
end;

destructor TInflateBlock.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

function TInflateBlock.ReadBitsFromStream(Bits: Byte): Byte;
begin
 Assert(Bits > 0);
 Assert(Bits <= 8);
 Result := 0;

 while Bits > 0 do
  begin
   if (FBitPosition mod 8 = 0) then
    begin
     FSource.Read(FCurrentByte, 1);
     FBitPosition := 0;
    end;

   Result := Result + ((FCurrentByte and (1 shl FBitPosition)) shr FBitPosition) shl (8 - Bits);
   Dec(Bits);
   Inc(FBitPosition);
  end;
end;

procedure TInflateBlock.ReadFromStream;
var
  Value : Byte;
begin
 FBuffer.Size := 0;
 FIsFinalBlock := ReadBitsFromStream(1) <> 0;
 FBlockFormatType := TBlockFormatType(ReadBitsFromStream(2));

 if BlockFormatType = bfStored
  then ReadStoredFromStream;

end;

procedure TInflateBlock.ReadStoredFromStream;
var
  Length : Word;
  InvertedLength : Word;
begin
 FSource.Read(Length, 2);
 FSource.Read(InvertedLength, 2);
 Assert(Length = not InvertedLength);
end;
*)

end.
