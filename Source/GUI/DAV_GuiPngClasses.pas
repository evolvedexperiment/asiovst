unit DAV_GuiPngClasses;

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
  Classes, SysUtils;

type
  EPngError = class(Exception);

function ReadSwappedWord(Stream: TStream): Word;
function ReadSwappedSmallInt(Stream: TStream): SmallInt;
function ReadSwappedCardinal(Stream: TStream): Cardinal;
procedure WriteSwappedWord(Stream: TStream; Value: Word);
procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);

implementation

uses
  DAV_Common;

function ReadSwappedWord(Stream: TStream): Word;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Word));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TStream): SmallInt;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(SmallInt));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TStream): Cardinal;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Cardinal));
 {$ENDIF}
 Result := Swap32(Result);
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
begin
 Value := Swap32(Value);
 Stream.Write(Value, SizeOf(Cardinal));
end;


end.
