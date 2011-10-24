unit DAV_GuiFixedPoint;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_FixedPoint;

type
  TFixed16Dot16PointPoint = record
    X, Y : TFixed16Dot16Point;
  end;
  PFixed16Dot16PointPoint = ^TFixed16Dot16PointPoint;

  TFixed16Dot16PointRect = record
  case Integer of
    0: (Left, Top, Right, Bottom: TFixed16Dot16Point);
    1: (TopLeft, BottomRight: TFixed16Dot16PointPoint);
  end;
  PFixed16Dot16PointRect = ^TFixed16Dot16PointRect;

  TFixed16Dot16PointPointArray = array [0..0] of TFixed16Dot16PointPoint;
  PFixed16Dot16PointPointArray = ^TFixed16Dot16PointPointArray;
  TArrayOfFixed16Dot16PointPoint = array of TFixed16Dot16PointPoint;
  PArrayOfFixed16Dot16PointPoint = ^TArrayOfFixed16Dot16PointPoint;
  TArrayOfArrayOfFixed16Dot16PointPoint = array of TArrayOfFixed16Dot16PointPoint;
  PArrayOfArrayOfFixed16Dot16PointPoint = ^TArrayOfArrayOfFixed16Dot16PointPoint;

  TFixed24Dot8PointPoint = record
    X, Y : TFixed24Dot8Point;
  end;
  PFixed24Dot8PointPoint = ^TFixed24Dot8PointPoint;

  TFixed24Dot8PointRect = record
  case Integer of
    0: (Left, Top, Right, Bottom: TFixed24Dot8Point);
    1: (TopLeft, BottomRight: TFixed24Dot8PointPoint);
  end;
  PFixed24Dot8PointRect = ^TFixed24Dot8PointRect;

  TFixed24Dot8PointPointArray = array [0..0] of TFixed24Dot8PointPoint;
  PFixed24Dot8PointPointArray = ^TFixed24Dot8PointPointArray;
  TArrayOfFixed24Dot8PointPoint = array of TFixed24Dot8PointPoint;
  PArrayOfFixed24Dot8PointPoint = ^TArrayOfFixed24Dot8PointPoint;
  TArrayOfArrayOfFixed24Dot8PointPoint = array of TArrayOfFixed24Dot8PointPoint;
  PArrayOfArrayOfFixed24Dot8PointPoint = ^TArrayOfArrayOfFixed24Dot8PointPoint;

function Fixed16Dot16PointPoint(X, Y: TFixed16Dot16Point): TFixed16Dot16PointPoint;
function Fixed24Dot8PointPoint(X, Y: TFixed24Dot8Point): TFixed24Dot8PointPoint;
function Fixed16Dot16PointRect(Left, Top, Right, Bottom: Integer): TFixed16Dot16PointRect; overload;
function Fixed16Dot16PointRect(Left, Top, Right, Bottom: TFixed16Dot16Point): TFixed16Dot16PointRect; overload;
function Fixed24Dot8PointRect(Left, Top, Right, Bottom: Integer): TFixed24Dot8PointRect; overload;
function Fixed24Dot8PointRect(Left, Top, Right, Bottom: TFixed24Dot8Point): TFixed24Dot8PointRect; overload;

implementation

function Fixed16Dot16PointPoint(X, Y: TFixed16Dot16Point): TFixed16Dot16PointPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Fixed24Dot8PointPoint(X, Y: TFixed24Dot8Point): TFixed24Dot8PointPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Fixed16Dot16PointRect(Left, Top, Right, Bottom: Integer): TFixed16Dot16PointRect;
begin
  Result.Left := ConvertToFixed16Dot16Point(Left);
  Result.Top := ConvertToFixed16Dot16Point(Top);
  Result.Right := ConvertToFixed16Dot16Point(Right);
  Result.Bottom := ConvertToFixed16Dot16Point(Bottom);
end;

function Fixed16Dot16PointRect(Left, Top, Right, Bottom: TFixed16Dot16Point): TFixed16Dot16PointRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function Fixed24Dot8PointRect(Left, Top, Right, Bottom: Integer): TFixed24Dot8PointRect;
begin
  Result.Left := ConvertToFixed24Dot8Point(Left);
  Result.Top := ConvertToFixed24Dot8Point(Top);
  Result.Right := ConvertToFixed24Dot8Point(Right);
  Result.Bottom := ConvertToFixed24Dot8Point(Bottom);
end;

function Fixed24Dot8PointRect(Left, Top, Right, Bottom: TFixed24Dot8Point): TFixed24Dot8PointRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

end.
