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

uses
  Classes;

type
  TZCompressionMethod = (cmDeflate = 8);
  TZCompressionLevel = (clFastest = 0, clFast = 1, clDefault = 2,
    clMaximumCompression = 3);

  TCustomZStream = class(TStream)
  private
    FStream            : TStream;
    FStreamPos         : Int64;
  protected
    FCompressionMethod : TZCompressionMethod;
    FCompressionInfo   : Byte;
    FCompressionLevel  : TZCompressionLevel;
    FDictionaryPresent : Boolean;
    FAdler32           : Cardinal;
  protected
    constructor Create(Stream: TStream);
  end;

  TZDecompressionStream = class(TCustomZStream)
  private
    procedure ReadHeader(Stream: TStream);
    procedure ReadAdler32(Stream: TStream);
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
  SysUtils;

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
 ReadAdler32(Source);
end;

destructor TZDecompressionStream.Destroy;
begin
 // dispose ...

 inherited;
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

function TZDecompressionStream.Read(var Buffer; Count: Integer): Integer;
begin
 // not implemented yet

 Result := 0;
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

end.

