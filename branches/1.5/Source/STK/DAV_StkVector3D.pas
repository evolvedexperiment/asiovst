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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_StkVector3D;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK 3D vector class.

  This class implements a three-dimensional vector.
}

interface

{$I ..\DAV_Compiler.inc}

type
  TStkVector3D = class
  public
    // Default constructor taking optional initial X, Y, and Z values.
    constructor Create(const initX, initY, initZ: Double); reintroduce; virtual;

    // Get the current X value.
    function getX: Double;

    // Get the current Y value.
    function getY: Double;

    // Get the current Z value.
    function getZ: Double;

    // Calculate the vector length.
    function GetLength: Double;

    // Set the X, Y, and Z values simultaniously.
    procedure SetXYZ(anX, aY, aZ: Double);

    // Set the X value.
    procedure SetX(const aval: Double);

    // Set the Y value.
    procedure SetY(const aval: Double);

    // Set the Z value.
    procedure SetZ(const aval: Double);

  protected
    myX, myY, myZ: Double end;

implementation

constructor TStkVector3D.Create;
begin
  myX := initX;
  myY := initY;
  myZ := initZ;
end;

function TStkVector3D.getX;
begin
  Result := myX;
end;

function TStkVector3D.getY;
begin
  Result := myY;
end;

function TStkVector3D.getZ;
begin
  Result := myZ;
end;

function TStkVector3D.GetLength;
var
  temp: Double;
begin
  temp := myX * myX;
  temp := temp + myY * myY;
  temp := temp + myZ * myZ;
  temp := sqrt(temp);
  Result := temp;
end;

procedure TStkVector3D.SetXYZ;
begin
  myX := anX;
  myY := aY;
  myZ := aZ;
end;

procedure TStkVector3D.SetX;
begin
  myX := aval;
end;

procedure TStkVector3D.SetY;
begin
  myY := aval;
end;

procedure TStkVector3D.SetZ;
begin
  myZ := aval;
end;

end.
