unit DAV_BlockConvert;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

var
  BlockConvert32To64 : procedure(Destination: PDouble; Source: PSingle; Count: Integer);
  BlockConvert64To32 : procedure(Destination: PSingle; Source: PDouble; Count: Integer);

implementation

uses
  DAV_Bindings;

procedure BlockConvert32To64Native(Destination: PDouble; Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^;
   Inc(Destination);
   Inc(Source);
  end;
end;
{$ELSE}
asm
@Start:
 fld  [eax + ecx * 4 - 4].Single
 fstp [edx + ecx * 8 - 8].Double
 loop @Start
end;
{$ENDIF}

procedure BlockConvert64To32Native(Destination: PSingle; Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^;
   Inc(Destination);
   Inc(Source);
  end;
end;
{$ELSE}
asm
@Start:
 fld [eax + ecx * 8 - 8].Double
 fstp [edx + ecx * 4 - 4].Single
 loop @Start
end;
{$ENDIF}

procedure BindFunctions;
begin
 // Block Inplace Addition Binding (32 bit)
 with TFunctionBinding.Create(@@BlockConvert32To64, @BlockConvert32To64Native) do
  begin
   Add(@BlockConvert32To64Native);
  end;

 // Block Inplace Addition Binding (64 bit)
 with TFunctionBinding.Create(@@BlockConvert64To32, @BlockConvert64To32Native) do
  begin
   Add(@BlockConvert64To32Native);
  end;
end;

initialization
  BindFunctions;

end.
