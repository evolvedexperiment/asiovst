unit DAV_MemoryUtils;

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
{$IFDEF DELPHI10_UP}
{-$DEFINE MemoryAlreadyAligned}
{$ENDIF}

uses
  Classes, SysUtils;

procedure GetAlignedMemory(var P: Pointer; Size: Integer);
procedure ReallocateAlignedMemory(var P: Pointer; Size: Integer);
procedure FreeAlignedMemory(P: Pointer);

implementation

var
  UnalignedMemoryList : TList;

function GetUnalignedPointer(P: Pointer): Pointer;
var
  Index : Integer;
begin
 Result := nil;

 for Index := 0 to UnalignedMemoryList.Count - 1 do
  if P = Pointer(Cardinal(UnalignedMemoryList.Items[Index]) and (not $F)) then
   begin
    Result := UnalignedMemoryList.Items[Index];
    Exit;
   end;
end;

function GetUnalignedPointerIndex(P: Pointer): Integer;
var
  Index : Integer;
begin
 Result := -1;

 for Index := 0 to UnalignedMemoryList.Count - 1 do
  if P = Pointer(Cardinal(UnalignedMemoryList.Items[Index]) and (not $F)) then
   begin
    Result := Index;
    Exit;
   end;
end;

procedure GetAlignedMemory(var P: Pointer; Size: Integer);
begin
 // check for size = 0
 if Size = 0 then
  begin
   P := nil;
   Exit;
  end;

 // initially allocate memory
 GetMem(P, Size);

 {$IFNDEF MemoryAlreadyAligned}
 // check if memory is unaligned
 if (Cardinal(P) and $F) <> 0 then
  begin
   // reallocate slightly larger memory
   ReallocMem(P, Size + $F);

   // check if memory is still unaligned
//   if (Cardinal(P) and $F) <> 0 then
    begin
     // add unaligned pointer to the unaligned memory list
     UnalignedMemoryList.Add(P);

     // assign aligned pointer
     P := Pointer(Cardinal(P) and (not $F));
    end;
  end;
 {$ENDIF}

 Assert(Cardinal(P) and $F = 0);
end;

procedure ReallocateAlignedMemory(var P: Pointer; Size: Integer);
{$IFNDEF MemoryAlreadyAligned}
var
  Index : Integer;
{$ENDIF}
begin
 // check for size = 0
 if Size = 0 then
  begin
   if Assigned(P)
    then FreeAlignedMemory(P);
   P := nil;
   Exit;
  end;

 if P = nil then
  begin
   GetAlignedMemory(P, Size);
   Exit;
  end;

 {$IFNDEF MemoryAlreadyAligned}
 Index := GetUnalignedPointerIndex(P);
 {$ENDIF}

 ReallocMem(P, Size);

 {$IFNDEF MemoryAlreadyAligned}
 if (Cardinal(P) and $F) <> 0 then
  begin
   ReallocMem(P, Size + $F);
   if (Index >= 0)
    then UnalignedMemoryList.Items[Index] := P
    else UnalignedMemoryList.Add(P);

   P := Pointer(Cardinal(P) and (not $F));
  end else
 if (Index >= 0)
  then UnalignedMemoryList.Delete(Index);
 {$ENDIF}

 Assert(Cardinal(P) and $F = 0);
end;

procedure FreeAlignedMemory(P: Pointer);
{$IFNDEF MemoryAlreadyAligned}
var
  Index : Integer;
{$ENDIF}
begin
 {$IFNDEF MemoryAlreadyAligned}
 for Index := 0 to UnalignedMemoryList.Count - 1 do
  begin
   // check if P is alread the unaligned pointer
   if P = UnalignedMemoryList.Items[Index] then
    begin
     // delete unaligned pointer from list
     UnalignedMemoryList.Delete(Index);

     // dispose memory & exit
     Dispose(P);
     Exit;
    end;

   // check if P is alread the unaligned pointer
   if P = Pointer(Cardinal(UnalignedMemoryList.Items[Index]) and (not $F)) then
    begin
     // store unaligned pointer
     P := UnalignedMemoryList.Items[Index];

     // delete unaligned pointer from list
     UnalignedMemoryList.Delete(Index);

     // dispose memory & exit
     Dispose(P);
     Exit;
    end;
  end;
 {$ENDIF}
 Dispose(P);
end;


initialization
  UnalignedMemoryList := TList.Create;

finalization
  FreeAndNil(UnalignedMemoryList);

end.
