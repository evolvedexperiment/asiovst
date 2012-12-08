{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_Huffman;

interface

{$I DAV_Compiler.inc}

uses
  DAV_BitReserve;

type
  PHuffBits = ^THuffBits;
  THuffBits = Cardinal;

type
  PHTArray = ^THTArray;
  THTArray = array [0 .. 1] of Byte;

  PPHTArray = ^TPHTArray;
  TPHTArray = array [0 .. 1] of THTArray;

  PHuffmanCodeTable = ^THuffmanCodeTable;

  THuffmanCodeTable = record
    TableName: array [0 .. 2] of AnsiChar;
    // string, containing table_description
    XLength: Cardinal; // max. x-index+
    YLength: Cardinal; // max. y-index+
    LinBits: Cardinal; // number of LinBits
    LinMax: Cardinal; // max number to be stored in LinBits
    Ref: Integer; // a positive value indicates a reference
    Table: PHuffBits; // pointer to array[XLength][YLength]
    HLength: PAnsiChar; // pointer to array[XLength][YLength]
    Val: PPHTArray; // decoder tree
    TreeLength: Cardinal; // length of decoder tree
  end;

const
  CHuffmanCodeTableSize = 34;

var
  // array of all Huffman code Table headers
  // 0..31 Huffman code Table 0..31
  // 32, 33 count1 - tables
  GHuffmanCodeTable: array [0 .. CHuffmanCodeTableSize - 1]
    of THuffmanCodeTable;

function HuffmanDecoder(HuffmanCodeTable: PHuffmanCodeTable;
  var x, y, v, w: Integer; BitReverse: TBitReserve): Integer;

implementation

uses
  SysUtils;

const
  CMaxOff = 250;
  CDMask: THuffBits = THuffBits(1 shl ((SizeOf(THuffBits) * 8) - 1));

  CValTab0: array [0 .. 0] of THTArray = ((0, 0)); // dummy

  CValTab1: array [0 .. 6] of THTArray = ((2, 1), (0, 0), (2, 1), (0, 16),
    (2, 1), (0, 1), (0, 17));

  CValTab2: array [0 .. 16] of THTArray = ((2, 1), (0, 0), (4, 1), (2, 1),
    (0, 16), (0, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 32), (0, 33), (2, 1),
    (0, 18), (2, 1), (0, 2), (0, 34));

  CValTab3: array [0 .. 16] of THTArray = ((4, 1), (2, 1), (0, 0), (0, 1),
    (2, 1), (0, 17), (2, 1), (0, 16), (4, 1), (2, 1), (0, 32), (0, 33), (2, 1),
    (0, 18), (2, 1), (0, 2), (0, 34));

  CValTab4: array [0 .. 0] of THTArray = ((0, 0)); // dummy

  CValTab5: array [0 .. 30] of THTArray = ((2, 1), (0, 0), (4, 1), (2, 1),
    (0, 16), (0, 1), (2, 1), (0, 17), (8, 1), (4, 1), (2, 1), (0, 32), (0, 2),
    (2, 1), (0, 33), (0, 18), (8, 1), (4, 1), (2, 1), (0, 34), (0, 48), (2, 1),
    (0, 3), (0, 19), (2, 1), (0, 49), (2, 1), (0, 50), (2, 1), (0, 35),
    (0, 51));

  CValTab6: array [0 .. 30] of THTArray = ((6, 1), (4, 1), (2, 1), (0, 0),
    (0, 16), (0, 17), (6, 1), (2, 1), (0, 1), (2, 1), (0, 32), (0, 33), (6, 1),
    (2, 1), (0, 18), (2, 1), (0, 2), (0, 34), (4, 1), (2, 1), (0, 49), (0, 19),
    (4, 1), (2, 1), (0, 48), (0, 50), (2, 1), (0, 35), (2, 1), (0, 3), (0, 51));

  CValTab7: array [0 .. 70] of THTArray = ((2, 1), (0, 0), (4, 1), (2, 1),
    (0, 16), (0, 1), (8, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 32), (0, 2),
    (0, 33), (18, 1), (6, 1), (2, 1), (0, 18), (2, 1), (0, 34), (0, 48), (4, 1),
    (2, 1), (0, 49), (0, 19), (4, 1), (2, 1), (0, 3), (0, 50), (2, 1), (0, 35),
    (0, 4), (10, 1), (4, 1), (2, 1), (0, 64), (0, 65), (2, 1), (0, 20), (2, 1),
    (0, 66), (0, 36), (12, 1), (6, 1), (4, 1), (2, 1), (0, 51), (0, 67),
    (0, 80), (4, 1), (2, 1), (0, 52), (0, 5), (0, 81), (6, 1), (2, 1), (0, 21),
    (2, 1), (0, 82), (0, 37), (4, 1), (2, 1), (0, 68), (0, 53), (4, 1), (2, 1),
    (0, 83), (0, 84), (2, 1), (0, 69), (0, 85));

  CValTab8: array [0 .. 70] of THTArray = ((6, 1), (2, 1), (0, 0), (2, 1),
    (0, 16), (0, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 33), (0, 18), (14, 1),
    (4, 1), (2, 1), (0, 32), (0, 2), (2, 1), (0, 34), (4, 1), (2, 1), (0, 48),
    (0, 3), (2, 1), (0, 49), (0, 19), (14, 1), (8, 1), (4, 1), (2, 1), (0, 50),
    (0, 35), (2, 1), (0, 64), (0, 4), (2, 1), (0, 65), (2, 1), (0, 20), (0, 66),
    (12, 1), (6, 1), (2, 1), (0, 36), (2, 1), (0, 51), (0, 80), (4, 1), (2, 1),
    (0, 67), (0, 52), (0, 81), (6, 1), (2, 1), (0, 21), (2, 1), (0, 5), (0, 82),
    (6, 1), (2, 1), (0, 37), (2, 1), (0, 68), (0, 53), (2, 1), (0, 83), (2, 1),
    (0, 69), (2, 1), (0, 84), (0, 85));

  CValTab9: array [0 .. 70] of THTArray = ((8, 1), (4, 1), (2, 1), (0, 0),
    (0, 16), (2, 1), (0, 1), (0, 17), (10, 1), (4, 1), (2, 1), (0, 32), (0, 33),
    (2, 1), (0, 18), (2, 1), (0, 2), (0, 34), (12, 1), (6, 1), (4, 1), (2, 1),
    (0, 48), (0, 3), (0, 49), (2, 1), (0, 19), (2, 1), (0, 50), (0, 35),
    (12, 1), (4, 1), (2, 1), (0, 65), (0, 20), (4, 1), (2, 1), (0, 64), (0, 51),
    (2, 1), (0, 66), (0, 36), (10, 1), (6, 1), (4, 1), (2, 1), (0, 4), (0, 80),
    (0, 67), (2, 1), (0, 52), (0, 81), (8, 1), (4, 1), (2, 1), (0, 21), (0, 82),
    (2, 1), (0, 37), (0, 68), (6, 1), (4, 1), (2, 1), (0, 5), (0, 84), (0, 83),
    (2, 1), (0, 53), (2, 1), (0, 69), (0, 85));

  CValTab10: array [0 .. 126] of THTArray = ((2, 1), (0, 0), (4, 1), (2, 1),
    (0, 16), (0, 1), (10, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 32), (0, 2),
    (2, 1), (0, 33), (0, 18), (28, 1), (8, 1), (4, 1), (2, 1), (0, 34), (0, 48),
    (2, 1), (0, 49), (0, 19), (8, 1), (4, 1), (2, 1), (0, 3), (0, 50), (2, 1),
    (0, 35), (0, 64), (4, 1), (2, 1), (0, 65), (0, 20), (4, 1), (2, 1), (0, 4),
    (0, 51), (2, 1), (0, 66), (0, 36), (28, 1), (10, 1), (6, 1), (4, 1), (2, 1),
    (0, 80), (0, 5), (0, 96), (2, 1), (0, 97), (0, 22), (12, 1), (6, 1), (4, 1),
    (2, 1), (0, 67), (0, 52), (0, 81), (2, 1), (0, 21), (2, 1), (0, 82),
    (0, 37), (4, 1), (2, 1), (0, 38), (0, 54), (0, 113), (20, 1), (8, 1),
    (2, 1), (0, 23), (4, 1), (2, 1), (0, 68), (0, 83), (0, 6), (6, 1), (4, 1),
    (2, 1), (0, 53), (0, 69), (0, 98), (2, 1), (0, 112), (2, 1), (0, 7),
    (0, 100), (14, 1), (4, 1), (2, 1), (0, 114), (0, 39), (6, 1), (2, 1),
    (0, 99), (2, 1), (0, 84), (0, 85), (2, 1), (0, 70), (0, 115), (8, 1),
    (4, 1), (2, 1), (0, 55), (0, 101), (2, 1), (0, 86), (0, 116), (6, 1),
    (2, 1), (0, 71), (2, 1), (0, 102), (0, 117), (4, 1), (2, 1), (0, 87),
    (0, 118), (2, 1), (0, 103), (0, 119));

  CValTab11: array [0 .. 126] of THTArray = ((6, 1), (2, 1), (0, 0), (2, 1),
    (0, 16), (0, 1), (8, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 32), (0, 2),
    (0, 18), (24, 1), (8, 1), (2, 1), (0, 33), (2, 1), (0, 34), (2, 1), (0, 48),
    (0, 3), (4, 1), (2, 1), (0, 49), (0, 19), (4, 1), (2, 1), (0, 50), (0, 35),
    (4, 1), (2, 1), (0, 64), (0, 4), (2, 1), (0, 65), (0, 20), (30, 1), (16, 1),
    (10, 1), (4, 1), (2, 1), (0, 66), (0, 36), (4, 1), (2, 1), (0, 51), (0, 67),
    (0, 80), (4, 1), (2, 1), (0, 52), (0, 81), (0, 97), (6, 1), (2, 1), (0, 22),
    (2, 1), (0, 6), (0, 38), (2, 1), (0, 98), (2, 1), (0, 21), (2, 1), (0, 5),
    (0, 82), (16, 1), (10, 1), (6, 1), (4, 1), (2, 1), (0, 37), (0, 68),
    (0, 96), (2, 1), (0, 99), (0, 54), (4, 1), (2, 1), (0, 112), (0, 23),
    (0, 113), (16, 1), (6, 1), (4, 1), (2, 1), (0, 7), (0, 100), (0, 114),
    (2, 1), (0, 39), (4, 1), (2, 1), (0, 83), (0, 53), (2, 1), (0, 84), (0, 69),
    (10, 1), (4, 1), (2, 1), (0, 70), (0, 115), (2, 1), (0, 55), (2, 1),
    (0, 101), (0, 86), (10, 1), (6, 1), (4, 1), (2, 1), (0, 85), (0, 87),
    (0, 116), (2, 1), (0, 71), (0, 102), (4, 1), (2, 1), (0, 117), (0, 118),
    (2, 1), (0, 103), (0, 119));

  CValTab12: array [0 .. 126] of THTArray = ((12, 1), (4, 1), (2, 1), (0, 16),
    (0, 1), (2, 1), (0, 17), (2, 1), (0, 0), (2, 1), (0, 32), (0, 2), (16, 1),
    (4, 1), (2, 1), (0, 33), (0, 18), (4, 1), (2, 1), (0, 34), (0, 49), (2, 1),
    (0, 19), (2, 1), (0, 48), (2, 1), (0, 3), (0, 64), (26, 1), (8, 1), (4, 1),
    (2, 1), (0, 50), (0, 35), (2, 1), (0, 65), (0, 51), (10, 1), (4, 1), (2, 1),
    (0, 20), (0, 66), (2, 1), (0, 36), (2, 1), (0, 4), (0, 80), (4, 1), (2, 1),
    (0, 67), (0, 52), (2, 1), (0, 81), (0, 21), (28, 1), (14, 1), (8, 1),
    (4, 1), (2, 1), (0, 82), (0, 37), (2, 1), (0, 83), (0, 53), (4, 1), (2, 1),
    (0, 96), (0, 22), (0, 97), (4, 1), (2, 1), (0, 98), (0, 38), (6, 1), (4, 1),
    (2, 1), (0, 5), (0, 6), (0, 68), (2, 1), (0, 84), (0, 69), (18, 1), (10, 1),
    (4, 1), (2, 1), (0, 99), (0, 54), (4, 1), (2, 1), (0, 112), (0, 7),
    (0, 113), (4, 1), (2, 1), (0, 23), (0, 100), (2, 1), (0, 70), (0, 114),
    (10, 1), (6, 1), (2, 1), (0, 39), (2, 1), (0, 85), (0, 115), (2, 1),
    (0, 55), (0, 86), (8, 1), (4, 1), (2, 1), (0, 101), (0, 116), (2, 1),
    (0, 71), (0, 102), (4, 1), (2, 1), (0, 117), (0, 87), (2, 1), (0, 118),
    (2, 1), (0, 103), (0, 119));

  CValTab13: array [0 .. 510] of THTArray = ((2, 1), (0, 0), (6, 1), (2, 1),
    (0, 16), (2, 1), (0, 1), (0, 17), (28, 1), (8, 1), (4, 1), (2, 1), (0, 32),
    (0, 2), (2, 1), (0, 33), (0, 18), (8, 1), (4, 1), (2, 1), (0, 34), (0, 48),
    (2, 1), (0, 3), (0, 49), (6, 1), (2, 1), (0, 19), (2, 1), (0, 50), (0, 35),
    (4, 1), (2, 1), (0, 64), (0, 4), (0, 65), (70, 1), (28, 1), (14, 1), (6, 1),
    (2, 1), (0, 20), (2, 1), (0, 51), (0, 66), (4, 1), (2, 1), (0, 36), (0, 80),
    (2, 1), (0, 67), (0, 52), (4, 1), (2, 1), (0, 81), (0, 21), (4, 1), (2, 1),
    (0, 5), (0, 82), (2, 1), (0, 37), (2, 1), (0, 68), (0, 83), (14, 1), (8, 1),
    (4, 1), (2, 1), (0, 96), (0, 6), (2, 1), (0, 97), (0, 22), (4, 1), (2, 1),
    (0, 128), (0, 8), (0, 129), (16, 1), (8, 1), (4, 1), (2, 1), (0, 53),
    (0, 98), (2, 1), (0, 38), (0, 84), (4, 1), (2, 1), (0, 69), (0, 99), (2, 1),
    (0, 54), (0, 112), (6, 1), (4, 1), (2, 1), (0, 7), (0, 85), (0, 113),
    (2, 1), (0, 23), (2, 1), (0, 39), (0, 55), (72, 1), (24, 1), (12, 1),
    (4, 1), (2, 1), (0, 24), (0, 130), (2, 1), (0, 40), (4, 1), (2, 1),
    (0, 100), (0, 70), (0, 114), (8, 1), (4, 1), (2, 1), (0, 132), (0, 72),
    (2, 1), (0, 144), (0, 9), (2, 1), (0, 145), (0, 25), (24, 1), (14, 1),
    (8, 1), (4, 1), (2, 1), (0, 115), (0, 101), (2, 1), (0, 86), (0, 116),
    (4, 1), (2, 1), (0, 71), (0, 102), (0, 131), (6, 1), (2, 1), (0, 56),
    (2, 1), (0, 117), (0, 87), (2, 1), (0, 146), (0, 41), (14, 1), (8, 1),
    (4, 1), (2, 1), (0, 103), (0, 133), (2, 1), (0, 88), (0, 57), (2, 1),
    (0, 147), (2, 1), (0, 73), (0, 134), (6, 1), (2, 1), (0, 160), (2, 1),
    (0, 104), (0, 10), (2, 1), (0, 161), (0, 26), (68, 1), (24, 1), (12, 1),
    (4, 1), (2, 1), (0, 162), (0, 42), (4, 1), (2, 1), (0, 149), (0, 89),
    (2, 1), (0, 163), (0, 58), (8, 1), (4, 1), (2, 1), (0, 74), (0, 150),
    (2, 1), (0, 176), (0, 11), (2, 1), (0, 177), (0, 27), (20, 1), (8, 1),
    (2, 1), (0, 178), (4, 1), (2, 1), (0, 118), (0, 119), (0, 148), (6, 1),
    (4, 1), (2, 1), (0, 135), (0, 120), (0, 164), (4, 1), (2, 1), (0, 105),
    (0, 165), (0, 43), (12, 1), (6, 1), (4, 1), (2, 1), (0, 90), (0, 136),
    (0, 179), (2, 1), (0, 59), (2, 1), (0, 121), (0, 166), (6, 1), (4, 1),
    (2, 1), (0, 106), (0, 180), (0, 192), (4, 1), (2, 1), (0, 12), (0, 152),
    (0, 193), (60, 1), (22, 1), (10, 1), (6, 1), (2, 1), (0, 28), (2, 1),
    (0, 137), (0, 181), (2, 1), (0, 91), (0, 194), (4, 1), (2, 1), (0, 44),
    (0, 60), (4, 1), (2, 1), (0, 182), (0, 107), (2, 1), (0, 196), (0, 76),
    (16, 1), (8, 1), (4, 1), (2, 1), (0, 168), (0, 138), (2, 1), (0, 208),
    (0, 13), (2, 1), (0, 209), (2, 1), (0, 75), (2, 1), (0, 151), (0, 167),
    (12, 1), (6, 1), (2, 1), (0, 195), (2, 1), (0, 122), (0, 153), (4, 1),
    (2, 1), (0, 197), (0, 92), (0, 183), (4, 1), (2, 1), (0, 29), (0, 210),
    (2, 1), (0, 45), (2, 1), (0, 123), (0, 211), (52, 1), (28, 1), (12, 1),
    (4, 1), (2, 1), (0, 61), (0, 198), (4, 1), (2, 1), (0, 108), (0, 169),
    (2, 1), (0, 154), (0, 212), (8, 1), (4, 1), (2, 1), (0, 184), (0, 139),
    (2, 1), (0, 77), (0, 199), (4, 1), (2, 1), (0, 124), (0, 213), (2, 1),
    (0, 93), (0, 224), (10, 1), (4, 1), (2, 1), (0, 225), (0, 30), (4, 1),
    (2, 1), (0, 14), (0, 46), (0, 226), (8, 1), (4, 1), (2, 1), (0, 227),
    (0, 109), (2, 1), (0, 140), (0, 228), (4, 1), (2, 1), (0, 229), (0, 186),
    (0, 240), (38, 1), (16, 1), (4, 1), (2, 1), (0, 241), (0, 31), (6, 1),
    (4, 1), (2, 1), (0, 170), (0, 155), (0, 185), (2, 1), (0, 62), (2, 1),
    (0, 214), (0, 200), (12, 1), (6, 1), (2, 1), (0, 78), (2, 1), (0, 215),
    (0, 125), (2, 1), (0, 171), (2, 1), (0, 94), (0, 201), (6, 1), (2, 1),
    (0, 15), (2, 1), (0, 156), (0, 110), (2, 1), (0, 242), (0, 47), (32, 1),
    (16, 1), (6, 1), (4, 1), (2, 1), (0, 216), (0, 141), (0, 63), (6, 1),
    (2, 1), (0, 243), (2, 1), (0, 230), (0, 202), (2, 1), (0, 244), (0, 79),
    (8, 1), (4, 1), (2, 1), (0, 187), (0, 172), (2, 1), (0, 231), (0, 245),
    (4, 1), (2, 1), (0, 217), (0, 157), (2, 1), (0, 95), (0, 232), (30, 1),
    (12, 1), (6, 1), (2, 1), (0, 111), (2, 1), (0, 246), (0, 203), (4, 1),
    (2, 1), (0, 188), (0, 173), (0, 218), (8, 1), (2, 1), (0, 247), (4, 1),
    (2, 1), (0, 126), (0, 127), (0, 142), (6, 1), (4, 1), (2, 1), (0, 158),
    (0, 174), (0, 204), (2, 1), (0, 248), (0, 143), (18, 1), (8, 1), (4, 1),
    (2, 1), (0, 219), (0, 189), (2, 1), (0, 234), (0, 249), (4, 1), (2, 1),
    (0, 159), (0, 235), (2, 1), (0, 190), (2, 1), (0, 205), (0, 250), (14, 1),
    (4, 1), (2, 1), (0, 221), (0, 236), (6, 1), (4, 1), (2, 1), (0, 233),
    (0, 175), (0, 220), (2, 1), (0, 206), (0, 251), (8, 1), (4, 1), (2, 1),
    (0, 191), (0, 222), (2, 1), (0, 207), (0, 238), (4, 1), (2, 1), (0, 223),
    (0, 239), (2, 1), (0, 255), (2, 1), (0, 237), (2, 1), (0, 253), (2, 1),
    (0, 252), (0, 254));

  CValTab14: array [0 .. 0] of THTArray = ((0, 0)); // dummy

  CValTab15: array [0 .. 510] of THTArray = ((16, 1), (6, 1), (2, 1), (0, 0),
    (2, 1), (0, 16), (0, 1), (2, 1), (0, 17), (4, 1), (2, 1), (0, 32), (0, 2),
    (2, 1), (0, 33), (0, 18), (50, 1), (16, 1), (6, 1), (2, 1), (0, 34), (2, 1),
    (0, 48), (0, 49), (6, 1), (2, 1), (0, 19), (2, 1), (0, 3), (0, 64), (2, 1),
    (0, 50), (0, 35), (14, 1), (6, 1), (4, 1), (2, 1), (0, 4), (0, 20), (0, 65),
    (4, 1), (2, 1), (0, 51), (0, 66), (2, 1), (0, 36), (0, 67), (10, 1), (6, 1),
    (2, 1), (0, 52), (2, 1), (0, 80), (0, 5), (2, 1), (0, 81), (0, 21), (4, 1),
    (2, 1), (0, 82), (0, 37), (4, 1), (2, 1), (0, 68), (0, 83), (0, 97),
    (90, 1), (36, 1), (18, 1), (10, 1), (6, 1), (2, 1), (0, 53), (2, 1),
    (0, 96), (0, 6), (2, 1), (0, 22), (0, 98), (4, 1), (2, 1), (0, 38), (0, 84),
    (2, 1), (0, 69), (0, 99), (10, 1), (6, 1), (2, 1), (0, 54), (2, 1),
    (0, 112), (0, 7), (2, 1), (0, 113), (0, 85), (4, 1), (2, 1), (0, 23),
    (0, 100), (2, 1), (0, 114), (0, 39), (24, 1), (16, 1), (8, 1), (4, 1),
    (2, 1), (0, 70), (0, 115), (2, 1), (0, 55), (0, 101), (4, 1), (2, 1),
    (0, 86), (0, 128), (2, 1), (0, 8), (0, 116), (4, 1), (2, 1), (0, 129),
    (0, 24), (2, 1), (0, 130), (0, 40), (16, 1), (8, 1), (4, 1), (2, 1),
    (0, 71), (0, 102), (2, 1), (0, 131), (0, 56), (4, 1), (2, 1), (0, 117),
    (0, 87), (2, 1), (0, 132), (0, 72), (6, 1), (4, 1), (2, 1), (0, 144),
    (0, 25), (0, 145), (4, 1), (2, 1), (0, 146), (0, 118), (2, 1), (0, 103),
    (0, 41), (92, 1), (36, 1), (18, 1), (10, 1), (4, 1), (2, 1), (0, 133),
    (0, 88), (4, 1), (2, 1), (0, 9), (0, 119), (0, 147), (4, 1), (2, 1),
    (0, 57), (0, 148), (2, 1), (0, 73), (0, 134), (10, 1), (6, 1), (2, 1),
    (0, 104), (2, 1), (0, 160), (0, 10), (2, 1), (0, 161), (0, 26), (4, 1),
    (2, 1), (0, 162), (0, 42), (2, 1), (0, 149), (0, 89), (26, 1), (14, 1),
    (6, 1), (2, 1), (0, 163), (2, 1), (0, 58), (0, 135), (4, 1), (2, 1),
    (0, 120), (0, 164), (2, 1), (0, 74), (0, 150), (6, 1), (4, 1), (2, 1),
    (0, 105), (0, 176), (0, 177), (4, 1), (2, 1), (0, 27), (0, 165), (0, 178),
    (14, 1), (8, 1), (4, 1), (2, 1), (0, 90), (0, 43), (2, 1), (0, 136),
    (0, 151), (2, 1), (0, 179), (2, 1), (0, 121), (0, 59), (8, 1), (4, 1),
    (2, 1), (0, 106), (0, 180), (2, 1), (0, 75), (0, 193), (4, 1), (2, 1),
    (0, 152), (0, 137), (2, 1), (0, 28), (0, 181), (80, 1), (34, 1), (16, 1),
    (6, 1), (4, 1), (2, 1), (0, 91), (0, 44), (0, 194), (6, 1), (4, 1), (2, 1),
    (0, 11), (0, 192), (0, 166), (2, 1), (0, 167), (0, 122), (10, 1), (4, 1),
    (2, 1), (0, 195), (0, 60), (4, 1), (2, 1), (0, 12), (0, 153), (0, 182),
    (4, 1), (2, 1), (0, 107), (0, 196), (2, 1), (0, 76), (0, 168), (20, 1),
    (10, 1), (4, 1), (2, 1), (0, 138), (0, 197), (4, 1), (2, 1), (0, 208),
    (0, 92), (0, 209), (4, 1), (2, 1), (0, 183), (0, 123), (2, 1), (0, 29),
    (2, 1), (0, 13), (0, 45), (12, 1), (4, 1), (2, 1), (0, 210), (0, 211),
    (4, 1), (2, 1), (0, 61), (0, 198), (2, 1), (0, 108), (0, 169), (6, 1),
    (4, 1), (2, 1), (0, 154), (0, 184), (0, 212), (4, 1), (2, 1), (0, 139),
    (0, 77), (2, 1), (0, 199), (0, 124), (68, 1), (34, 1), (18, 1), (10, 1),
    (4, 1), (2, 1), (0, 213), (0, 93), (4, 1), (2, 1), (0, 224), (0, 14),
    (0, 225), (4, 1), (2, 1), (0, 30), (0, 226), (2, 1), (0, 170), (0, 46),
    (8, 1), (4, 1), (2, 1), (0, 185), (0, 155), (2, 1), (0, 227), (0, 214),
    (4, 1), (2, 1), (0, 109), (0, 62), (2, 1), (0, 200), (0, 140), (16, 1),
    (8, 1), (4, 1), (2, 1), (0, 228), (0, 78), (2, 1), (0, 215), (0, 125),
    (4, 1), (2, 1), (0, 229), (0, 186), (2, 1), (0, 171), (0, 94), (8, 1),
    (4, 1), (2, 1), (0, 201), (0, 156), (2, 1), (0, 241), (0, 31), (6, 1),
    (4, 1), (2, 1), (0, 240), (0, 110), (0, 242), (2, 1), (0, 47), (0, 230),
    (38, 1), (18, 1), (8, 1), (4, 1), (2, 1), (0, 216), (0, 243), (2, 1),
    (0, 63), (0, 244), (6, 1), (2, 1), (0, 79), (2, 1), (0, 141), (0, 217),
    (2, 1), (0, 187), (0, 202), (8, 1), (4, 1), (2, 1), (0, 172), (0, 231),
    (2, 1), (0, 126), (0, 245), (8, 1), (4, 1), (2, 1), (0, 157), (0, 95),
    (2, 1), (0, 232), (0, 142), (2, 1), (0, 246), (0, 203), (34, 1), (18, 1),
    (10, 1), (6, 1), (4, 1), (2, 1), (0, 15), (0, 174), (0, 111), (2, 1),
    (0, 188), (0, 218), (4, 1), (2, 1), (0, 173), (0, 247), (2, 1), (0, 127),
    (0, 233), (8, 1), (4, 1), (2, 1), (0, 158), (0, 204), (2, 1), (0, 248),
    (0, 143), (4, 1), (2, 1), (0, 219), (0, 189), (2, 1), (0, 234), (0, 249),
    (16, 1), (8, 1), (4, 1), (2, 1), (0, 159), (0, 220), (2, 1), (0, 205),
    (0, 235), (4, 1), (2, 1), (0, 190), (0, 250), (2, 1), (0, 175), (0, 221),
    (14, 1), (6, 1), (4, 1), (2, 1), (0, 236), (0, 206), (0, 251), (4, 1),
    (2, 1), (0, 191), (0, 237), (2, 1), (0, 222), (0, 252), (6, 1), (4, 1),
    (2, 1), (0, 207), (0, 253), (0, 238), (4, 1), (2, 1), (0, 223), (0, 254),
    (2, 1), (0, 239), (0, 255));

  CValTab16: array [0 .. 510] of THTArray = ((2, 1), (0, 0), (6, 1), (2, 1),
    (0, 16), (2, 1), (0, 1), (0, 17), (42, 1), (8, 1), (4, 1), (2, 1), (0, 32),
    (0, 2), (2, 1), (0, 33), (0, 18), (10, 1), (6, 1), (2, 1), (0, 34), (2, 1),
    (0, 48), (0, 3), (2, 1), (0, 49), (0, 19), (10, 1), (4, 1), (2, 1), (0, 50),
    (0, 35), (4, 1), (2, 1), (0, 64), (0, 4), (0, 65), (6, 1), (2, 1), (0, 20),
    (2, 1), (0, 51), (0, 66), (4, 1), (2, 1), (0, 36), (0, 80), (2, 1), (0, 67),
    (0, 52), (138, 1), (40, 1), (16, 1), (6, 1), (4, 1), (2, 1), (0, 5),
    (0, 21), (0, 81), (4, 1), (2, 1), (0, 82), (0, 37), (4, 1), (2, 1), (0, 68),
    (0, 53), (0, 83), (10, 1), (6, 1), (4, 1), (2, 1), (0, 96), (0, 6), (0, 97),
    (2, 1), (0, 22), (0, 98), (8, 1), (4, 1), (2, 1), (0, 38), (0, 84), (2, 1),
    (0, 69), (0, 99), (4, 1), (2, 1), (0, 54), (0, 112), (0, 113), (40, 1),
    (18, 1), (8, 1), (2, 1), (0, 23), (2, 1), (0, 7), (2, 1), (0, 85), (0, 100),
    (4, 1), (2, 1), (0, 114), (0, 39), (4, 1), (2, 1), (0, 70), (0, 101),
    (0, 115), (10, 1), (6, 1), (2, 1), (0, 55), (2, 1), (0, 86), (0, 8), (2, 1),
    (0, 128), (0, 129), (6, 1), (2, 1), (0, 24), (2, 1), (0, 116), (0, 71),
    (2, 1), (0, 130), (2, 1), (0, 40), (0, 102), (24, 1), (14, 1), (8, 1),
    (4, 1), (2, 1), (0, 131), (0, 56), (2, 1), (0, 117), (0, 132), (4, 1),
    (2, 1), (0, 72), (0, 144), (0, 145), (6, 1), (2, 1), (0, 25), (2, 1),
    (0, 9), (0, 118), (2, 1), (0, 146), (0, 41), (14, 1), (8, 1), (4, 1),
    (2, 1), (0, 133), (0, 88), (2, 1), (0, 147), (0, 57), (4, 1), (2, 1),
    (0, 160), (0, 10), (0, 26), (8, 1), (2, 1), (0, 162), (2, 1), (0, 103),
    (2, 1), (0, 87), (0, 73), (6, 1), (2, 1), (0, 148), (2, 1), (0, 119),
    (0, 134), (2, 1), (0, 161), (2, 1), (0, 104), (0, 149), (220, 1), (126, 1),
    (50, 1), (26, 1), (12, 1), (6, 1), (2, 1), (0, 42), (2, 1), (0, 89),
    (0, 58), (2, 1), (0, 163), (2, 1), (0, 135), (0, 120), (8, 1), (4, 1),
    (2, 1), (0, 164), (0, 74), (2, 1), (0, 150), (0, 105), (4, 1), (2, 1),
    (0, 176), (0, 11), (0, 177), (10, 1), (4, 1), (2, 1), (0, 27), (0, 178),
    (2, 1), (0, 43), (2, 1), (0, 165), (0, 90), (6, 1), (2, 1), (0, 179),
    (2, 1), (0, 166), (0, 106), (4, 1), (2, 1), (0, 180), (0, 75), (2, 1),
    (0, 12), (0, 193), (30, 1), (14, 1), (6, 1), (4, 1), (2, 1), (0, 181),
    (0, 194), (0, 44), (4, 1), (2, 1), (0, 167), (0, 195), (2, 1), (0, 107),
    (0, 196), (8, 1), (2, 1), (0, 29), (4, 1), (2, 1), (0, 136), (0, 151),
    (0, 59), (4, 1), (2, 1), (0, 209), (0, 210), (2, 1), (0, 45), (0, 211),
    (18, 1), (6, 1), (4, 1), (2, 1), (0, 30), (0, 46), (0, 226), (6, 1), (4, 1),
    (2, 1), (0, 121), (0, 152), (0, 192), (2, 1), (0, 28), (2, 1), (0, 137),
    (0, 91), (14, 1), (6, 1), (2, 1), (0, 60), (2, 1), (0, 122), (0, 182),
    (4, 1), (2, 1), (0, 76), (0, 153), (2, 1), (0, 168), (0, 138), (6, 1),
    (2, 1), (0, 13), (2, 1), (0, 197), (0, 92), (4, 1), (2, 1), (0, 61),
    (0, 198), (2, 1), (0, 108), (0, 154), (88, 1), (86, 1), (36, 1), (16, 1),
    (8, 1), (4, 1), (2, 1), (0, 139), (0, 77), (2, 1), (0, 199), (0, 124),
    (4, 1), (2, 1), (0, 213), (0, 93), (2, 1), (0, 224), (0, 14), (8, 1),
    (2, 1), (0, 227), (4, 1), (2, 1), (0, 208), (0, 183), (0, 123), (6, 1),
    (4, 1), (2, 1), (0, 169), (0, 184), (0, 212), (2, 1), (0, 225), (2, 1),
    (0, 170), (0, 185), (24, 1), (10, 1), (6, 1), (4, 1), (2, 1), (0, 155),
    (0, 214), (0, 109), (2, 1), (0, 62), (0, 200), (6, 1), (4, 1), (2, 1),
    (0, 140), (0, 228), (0, 78), (4, 1), (2, 1), (0, 215), (0, 229), (2, 1),
    (0, 186), (0, 171), (12, 1), (4, 1), (2, 1), (0, 156), (0, 230), (4, 1),
    (2, 1), (0, 110), (0, 216), (2, 1), (0, 141), (0, 187), (8, 1), (4, 1),
    (2, 1), (0, 231), (0, 157), (2, 1), (0, 232), (0, 142), (4, 1), (2, 1),
    (0, 203), (0, 188), (0, 158), (0, 241), (2, 1), (0, 31), (2, 1), (0, 15),
    (0, 47), (66, 1), (56, 1), (2, 1), (0, 242), (52, 1), (50, 1), (20, 1),
    (8, 1), (2, 1), (0, 189), (2, 1), (0, 94), (2, 1), (0, 125), (0, 201),
    (6, 1), (2, 1), (0, 202), (2, 1), (0, 172), (0, 126), (4, 1), (2, 1),
    (0, 218), (0, 173), (0, 204), (10, 1), (6, 1), (2, 1), (0, 174), (2, 1),
    (0, 219), (0, 220), (2, 1), (0, 205), (0, 190), (6, 1), (4, 1), (2, 1),
    (0, 235), (0, 237), (0, 238), (6, 1), (4, 1), (2, 1), (0, 217), (0, 234),
    (0, 233), (2, 1), (0, 222), (4, 1), (2, 1), (0, 221), (0, 236), (0, 206),
    (0, 63), (0, 240), (4, 1), (2, 1), (0, 243), (0, 244), (2, 1), (0, 79),
    (2, 1), (0, 245), (0, 95), (10, 1), (2, 1), (0, 255), (4, 1), (2, 1),
    (0, 246), (0, 111), (2, 1), (0, 247), (0, 127), (12, 1), (6, 1), (2, 1),
    (0, 143), (2, 1), (0, 248), (0, 249), (4, 1), (2, 1), (0, 159), (0, 250),
    (0, 175), (8, 1), (4, 1), (2, 1), (0, 251), (0, 191), (2, 1), (0, 252),
    (0, 207), (4, 1), (2, 1), (0, 253), (0, 223), (2, 1), (0, 254), (0, 239));

  CValTab24: array [0 .. 511] of THTArray = ((60, 1), (8, 1), (4, 1), (2, 1),
    (0, 0), (0, 16), (2, 1), (0, 1), (0, 17), (14, 1), (6, 1), (4, 1), (2, 1),
    (0, 32), (0, 2), (0, 33), (2, 1), (0, 18), (2, 1), (0, 34), (2, 1), (0, 48),
    (0, 3), (14, 1), (4, 1), (2, 1), (0, 49), (0, 19), (4, 1), (2, 1), (0, 50),
    (0, 35), (4, 1), (2, 1), (0, 64), (0, 4), (0, 65), (8, 1), (4, 1), (2, 1),
    (0, 20), (0, 51), (2, 1), (0, 66), (0, 36), (6, 1), (4, 1), (2, 1), (0, 67),
    (0, 52), (0, 81), (6, 1), (4, 1), (2, 1), (0, 80), (0, 5), (0, 21), (2, 1),
    (0, 82), (0, 37), (250, 1), (98, 1), (34, 1), (18, 1), (10, 1), (4, 1),
    (2, 1), (0, 68), (0, 83), (2, 1), (0, 53), (2, 1), (0, 96), (0, 6), (4, 1),
    (2, 1), (0, 97), (0, 22), (2, 1), (0, 98), (0, 38), (8, 1), (4, 1), (2, 1),
    (0, 84), (0, 69), (2, 1), (0, 99), (0, 54), (4, 1), (2, 1), (0, 113),
    (0, 85), (2, 1), (0, 100), (0, 70), (32, 1), (14, 1), (6, 1), (2, 1),
    (0, 114), (2, 1), (0, 39), (0, 55), (2, 1), (0, 115), (4, 1), (2, 1),
    (0, 112), (0, 7), (0, 23), (10, 1), (4, 1), (2, 1), (0, 101), (0, 86),
    (4, 1), (2, 1), (0, 128), (0, 8), (0, 129), (4, 1), (2, 1), (0, 116),
    (0, 71), (2, 1), (0, 24), (0, 130), (16, 1), (8, 1), (4, 1), (2, 1),
    (0, 40), (0, 102), (2, 1), (0, 131), (0, 56), (4, 1), (2, 1), (0, 117),
    (0, 87), (2, 1), (0, 132), (0, 72), (8, 1), (4, 1), (2, 1), (0, 145),
    (0, 25), (2, 1), (0, 146), (0, 118), (4, 1), (2, 1), (0, 103), (0, 41),
    (2, 1), (0, 133), (0, 88), (92, 1), (34, 1), (16, 1), (8, 1), (4, 1),
    (2, 1), (0, 147), (0, 57), (2, 1), (0, 148), (0, 73), (4, 1), (2, 1),
    (0, 119), (0, 134), (2, 1), (0, 104), (0, 161), (8, 1), (4, 1), (2, 1),
    (0, 162), (0, 42), (2, 1), (0, 149), (0, 89), (4, 1), (2, 1), (0, 163),
    (0, 58), (2, 1), (0, 135), (2, 1), (0, 120), (0, 74), (22, 1), (12, 1),
    (4, 1), (2, 1), (0, 164), (0, 150), (4, 1), (2, 1), (0, 105), (0, 177),
    (2, 1), (0, 27), (0, 165), (6, 1), (2, 1), (0, 178), (2, 1), (0, 90),
    (0, 43), (2, 1), (0, 136), (0, 179), (16, 1), (10, 1), (6, 1), (2, 1),
    (0, 144), (2, 1), (0, 9), (0, 160), (2, 1), (0, 151), (0, 121), (4, 1),
    (2, 1), (0, 166), (0, 106), (0, 180), (12, 1), (6, 1), (2, 1), (0, 26),
    (2, 1), (0, 10), (0, 176), (2, 1), (0, 59), (2, 1), (0, 11), (0, 192),
    (4, 1), (2, 1), (0, 75), (0, 193), (2, 1), (0, 152), (0, 137), (67, 1),
    (34, 1), (16, 1), (8, 1), (4, 1), (2, 1), (0, 28), (0, 181), (2, 1),
    (0, 91), (0, 194), (4, 1), (2, 1), (0, 44), (0, 167), (2, 1), (0, 122),
    (0, 195), (10, 1), (6, 1), (2, 1), (0, 60), (2, 1), (0, 12), (0, 208),
    (2, 1), (0, 182), (0, 107), (4, 1), (2, 1), (0, 196), (0, 76), (2, 1),
    (0, 153), (0, 168), (16, 1), (8, 1), (4, 1), (2, 1), (0, 138), (0, 197),
    (2, 1), (0, 92), (0, 209), (4, 1), (2, 1), (0, 183), (0, 123), (2, 1),
    (0, 29), (0, 210), (9, 1), (4, 1), (2, 1), (0, 45), (0, 211), (2, 1),
    (0, 61), (0, 198), (85, 250), (4, 1), (2, 1), (0, 108), (0, 169), (2, 1),
    (0, 154), (0, 212), (32, 1), (16, 1), (8, 1), (4, 1), (2, 1), (0, 184),
    (0, 139), (2, 1), (0, 77), (0, 199), (4, 1), (2, 1), (0, 124), (0, 213),
    (2, 1), (0, 93), (0, 225), (8, 1), (4, 1), (2, 1), (0, 30), (0, 226),
    (2, 1), (0, 170), (0, 185), (4, 1), (2, 1), (0, 155), (0, 227), (2, 1),
    (0, 214), (0, 109), (20, 1), (10, 1), (6, 1), (2, 1), (0, 62), (2, 1),
    (0, 46), (0, 78), (2, 1), (0, 200), (0, 140), (4, 1), (2, 1), (0, 228),
    (0, 215), (4, 1), (2, 1), (0, 125), (0, 171), (0, 229), (10, 1), (4, 1),
    (2, 1), (0, 186), (0, 94), (2, 1), (0, 201), (2, 1), (0, 156), (0, 110),
    (8, 1), (2, 1), (0, 230), (2, 1), (0, 13), (2, 1), (0, 224), (0, 14),
    (4, 1), (2, 1), (0, 216), (0, 141), (2, 1), (0, 187), (0, 202), (74, 1),
    (2, 1), (0, 255), (64, 1), (58, 1), (32, 1), (16, 1), (8, 1), (4, 1),
    (2, 1), (0, 172), (0, 231), (2, 1), (0, 126), (0, 217), (4, 1), (2, 1),
    (0, 157), (0, 232), (2, 1), (0, 142), (0, 203), (8, 1), (4, 1), (2, 1),
    (0, 188), (0, 218), (2, 1), (0, 173), (0, 233), (4, 1), (2, 1), (0, 158),
    (0, 204), (2, 1), (0, 219), (0, 189), (16, 1), (8, 1), (4, 1), (2, 1),
    (0, 234), (0, 174), (2, 1), (0, 220), (0, 205), (4, 1), (2, 1), (0, 235),
    (0, 190), (2, 1), (0, 221), (0, 236), (8, 1), (4, 1), (2, 1), (0, 206),
    (0, 237), (2, 1), (0, 222), (0, 238), (0, 15), (4, 1), (2, 1), (0, 240),
    (0, 31), (0, 241), (4, 1), (2, 1), (0, 242), (0, 47), (2, 1), (0, 243),
    (0, 63), (18, 1), (8, 1), (4, 1), (2, 1), (0, 244), (0, 79), (2, 1),
    (0, 245), (0, 95), (4, 1), (2, 1), (0, 246), (0, 111), (2, 1), (0, 247),
    (2, 1), (0, 127), (0, 143), (10, 1), (4, 1), (2, 1), (0, 248), (0, 249),
    (4, 1), (2, 1), (0, 159), (0, 175), (0, 250), (8, 1), (4, 1), (2, 1),
    (0, 251), (0, 191), (2, 1), (0, 252), (0, 207), (4, 1), (2, 1), (0, 253),
    (0, 223), (2, 1), (0, 254), (0, 239));

  CValTab32: array [0 .. 30] of THTArray = ((2, 1), (0, 0), (8, 1), (4, 1),
    (2, 1), (0, 8), (0, 4), (2, 1), (0, 1), (0, 2), (8, 1), (4, 1), (2, 1),
    (0, 12), (0, 10), (2, 1), (0, 3), (0, 6), (6, 1), (2, 1), (0, 9), (2, 1),
    (0, 5), (0, 7), (4, 1), (2, 1), (0, 14), (0, 13), (2, 1), (0, 15), (0, 11));

  CValTab33: array [0 .. 30] of THTArray = ((16, 1), (8, 1), (4, 1), (2, 1),
    (0, 0), (0, 1), (2, 1), (0, 2), (0, 3), (4, 1), (2, 1), (0, 4), (0, 5),
    (2, 1), (0, 6), (0, 7), (8, 1), (4, 1), (2, 1), (0, 8), (0, 9), (2, 1),
    (0, 10), (0, 11), (4, 1), (2, 1), (0, 12), (0, 13), (2, 1),
    (0, 14), (0, 15));

  // do the huffman-decoding
  // note! for counta, countb - the 4 bit value is returned in y, discard x
function HuffmanDecoder(HuffmanCodeTable: PHuffmanCodeTable;
  var x, y, v, w: Integer; BitReverse: TBitReserve): Integer;
var
  Level: THuffBits;
  Point: Cardinal;
begin
  Point := 0;
  Result := 1; // error code
  Level := CDMask;

  if (HuffmanCodeTable.Val = nil) then
  begin
    Result := 2;
    Exit;
  end;

  // Table 0 needs no bits
  if (HuffmanCodeTable.TreeLength = 0) then
  begin
    x := 0;
    y := 0;
    Result := 0;
    Exit;
  end;

  // Lookup in Huffman Table.
  repeat
    if (HuffmanCodeTable.Val[Point, 0] = 0) then
    begin // end of tree
      x := HuffmanCodeTable.Val[Point, 1] shr 4;
      y := HuffmanCodeTable.Val[Point, 1] and $F;

      Result := 0;
      Break;
    end;

    if (BitReverse.Get1Bit <> 0) then
    begin
      while (HuffmanCodeTable.Val[Point, 1] >= CMaxOff) do
        Point := Point + HuffmanCodeTable.Val[Point, 1];
      Point := Point + HuffmanCodeTable.Val[Point, 1];
    end
    else
    begin
      while (HuffmanCodeTable.Val[Point, 0] >= CMaxOff) do
        Point := Point + HuffmanCodeTable.Val[Point, 0];
      Point := Point + HuffmanCodeTable.Val[Point, 0];
    end;

    Level := Level shr 1;
  until not((Level <> 0) or (Point < PHuffmanCodeTable(@GHuffmanCodeTable)
    .TreeLength));

  // Process sign encodings for quadruples tables.
  if (HuffmanCodeTable.TableName[0] = '3') and
    ((HuffmanCodeTable.TableName[1] = '2') or
    (HuffmanCodeTable.TableName[1] = '3')) then
  begin
    v := (y shr 3) and 1;
    w := (y shr 2) and 1;
    x := (y shr 1) and 1;
    y := y and 1;

    // v, w, x and y are reversed in the bitstream.
    // switch them around to make test bistream work.

    if (v <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        v := -v;

    if (w <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        w := -w;

    if (x <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        x := -x;

    if (y <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        y := -y;
  end
  else
  begin
    // Process sign and escape encodings for dual tables.

    // x and y are reversed in the test bitstream.
    // Reverse x and y here to make test bitstream work.

    if (HuffmanCodeTable.LinBits <> 0) then
      if (Integer(HuffmanCodeTable.XLength - 1) = x) then
        x := x + Integer(BitReverse.GetBits(HuffmanCodeTable.LinBits));

    if (x <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        x := -x;

    if (HuffmanCodeTable.LinBits <> 0) then
      if (Integer(HuffmanCodeTable.YLength - 1) = y) then
        y := y + Integer(BitReverse.GetBits(HuffmanCodeTable.LinBits));

    if (y <> 0) then
      if (BitReverse.Get1Bit <> 0) then
        y := -y;
  end;
end;

procedure SetHuffTable(GHuffmanCodeTable: PHuffmanCodeTable; Name: PAnsiChar;
  XLength, YLength, LinBits, LinMax, Ref: Integer; Table: PHuffBits;
  HLength: PAnsiChar; Val: PPHTArray; TreeLength: Cardinal);
begin
  StrLCopy(GHuffmanCodeTable.TableName, Name,
    SizeOf(GHuffmanCodeTable.TableName));
  GHuffmanCodeTable.XLength := XLength;
  GHuffmanCodeTable.YLength := YLength;
  GHuffmanCodeTable.LinBits := LinBits;
  GHuffmanCodeTable.LinMax := LinMax;
  GHuffmanCodeTable.Ref := Ref;
  GHuffmanCodeTable.Table := Table;
  GHuffmanCodeTable.HLength := HLength;
  GHuffmanCodeTable.Val := Val;
  GHuffmanCodeTable.TreeLength := TreeLength;
end;

begin
  SetHuffTable(@GHuffmanCodeTable[0], '0', 0, 0, 0, 0, -1, nil, nil,
    @CValTab0, 0);
  SetHuffTable(@GHuffmanCodeTable[1], '1', 2, 2, 0, 0, -1, nil, nil,
    @CValTab1, 7);
  SetHuffTable(@GHuffmanCodeTable[2], '2', 3, 2, 0, 0, -1, nil, nil,
    @CValTab2, 17);
  SetHuffTable(@GHuffmanCodeTable[3], '3', 3, 3, 0, 0, -1, nil, nil,
    @CValTab3, 17);
  SetHuffTable(@GHuffmanCodeTable[4], '4', 0, 0, 0, 0, -1, nil, nil,
    @CValTab4, 0);
  SetHuffTable(@GHuffmanCodeTable[5], '5', 4, 4, 0, 0, -1, nil, nil,
    @CValTab5, 31);
  SetHuffTable(@GHuffmanCodeTable[6], '6', 4, 4, 0, 0, -1, nil, nil,
    @CValTab6, 31);
  SetHuffTable(@GHuffmanCodeTable[7], '7', 6, 6, 0, 0, -1, nil, nil,
    @CValTab7, 71);
  SetHuffTable(@GHuffmanCodeTable[8], '8', 6, 6, 0, 0, -1, nil, nil,
    @CValTab8, 71);
  SetHuffTable(@GHuffmanCodeTable[9], '9', 6, 6, 0, 0, -1, nil, nil,
    @CValTab9, 71);

  SetHuffTable(@GHuffmanCodeTable[10], '10', 8, 8, 0, 0, -1, nil, nil,
    @CValTab10, 127);
  SetHuffTable(@GHuffmanCodeTable[11], '11', 8, 8, 0, 0, -1, nil, nil,
    @CValTab11, 127);
  SetHuffTable(@GHuffmanCodeTable[12], '12', 8, 8, 0, 0, -1, nil, nil,
    @CValTab12, 127);
  SetHuffTable(@GHuffmanCodeTable[13], '13', 16, 16, 0, 0, -1, nil, nil,
    @CValTab13, 511);
  SetHuffTable(@GHuffmanCodeTable[14], '14', 0, 0, 0, 0, -1, nil, nil,
    @CValTab14, 0);
  SetHuffTable(@GHuffmanCodeTable[15], '15', 16, 16, 0, 0, -1, nil, nil,
    @CValTab15, 511);
  SetHuffTable(@GHuffmanCodeTable[16], '16', 16, 16, 1, 1, -1, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[17], '17', 16, 16, 2, 3, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[18], '18', 16, 16, 3, 7, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[19], '19', 16, 16, 4, 15, 16, nil, nil,
    @CValTab16, 511);

  SetHuffTable(@GHuffmanCodeTable[20], '20', 16, 16, 6, 63, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[21], '21', 16, 16, 8, 255, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[22], '22', 16, 16, 10, 1023, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[23], '23', 16, 16, 13, 8191, 16, nil, nil,
    @CValTab16, 511);
  SetHuffTable(@GHuffmanCodeTable[24], '24', 16, 16, 4, 15, -1, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[25], '25', 16, 16, 5, 31, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[26], '26', 16, 16, 6, 63, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[27], '27', 16, 16, 7, 127, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[28], '28', 16, 16, 8, 255, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[29], '29', 16, 16, 9, 511, 24, nil, nil,
    @CValTab24, 512);

  SetHuffTable(@GHuffmanCodeTable[30], '30', 16, 16, 11, 2047, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[31], '31', 16, 16, 13, 8191, 24, nil, nil,
    @CValTab24, 512);
  SetHuffTable(@GHuffmanCodeTable[32], '32', 1, 16, 0, 0, -1, nil, nil,
    @CValTab32, 31);
  SetHuffTable(@GHuffmanCodeTable[33], '33', 1, 16, 0, 0, -1, nil, nil,
    @CValTab32, 31);

end.
