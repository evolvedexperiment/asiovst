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

unit DAV_GuiDesign;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits, {$ELSE} {$IFDEF DELPHI6_UP}
  DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF} {$ENDIF}
  Classes, TypInfo, DAV_Classes;

type
  TCustomClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  protected
    class function GetClassList: TClassList; virtual;
    function GetObject: TObject; virtual; abstract;
    procedure SetClassName(const CustomClass: string); virtual; abstract;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

implementation

{ TCustomClassProperty }

function TCustomClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paReadOnly] + [paValueList, paRevertable
  {$IFDEF COMPILER6_UP}, paVolatileSubProperties{$ENDIF}];
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
end;

class function TCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

function TCustomClassProperty.GetValue: string;
var
  Objct: TObject;
begin
  if PropCount > 0 then
  begin
    Objct := GetObject;
    if Assigned(Objct) then
      Result := Objct.ClassName
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TCustomClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      Proc(L.Items[I].ClassName);
end;

function TCustomClassProperty.HasSubProperties: Boolean;
var
  Objct: TObject;
begin
  if PropCount > 0 then
  begin
    Objct := GetObject;
    if Assigned(Objct) then
      Result := GetTypeData(Objct.ClassInfo)^.PropCount > 0
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TCustomClassProperty.SetValue(const Value: string);
var
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) and Assigned(L.Find(Value)) then
    SetClassName(Value)
  else
    SetStrValue('');
  Modified;
end;

end.
