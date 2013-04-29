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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_GuiDialDesign;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits, {$ELSE} {$IFDEF DELPHI6_UP}
  DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF} {$ENDIF}
  Classes, TypInfo, DAV_Classes, DAV_GuiDesign, DAV_GuiDial,
  DAV_GuiDialRenderer;

type
  TPrimitiveClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    function GetObject: TObject; override;
    procedure SetClassName(const CustomClass: string); override;
  end;

implementation

{ TPrimitiveClassProperty }

class function TPrimitiveClassProperty.GetClassList: TClassList;
begin
  Result := PrimitiveClassList;
end;

function TPrimitiveClassProperty.GetObject: TObject;
begin
  Result := TGuiDialLayerCollectionItem(GetComponent(0)).Primitive;
end;

procedure TPrimitiveClassProperty.SetClassName(const CustomClass: string);
begin
  TGuiDialLayerCollectionItem(GetComponent(0)).PrimitiveClassName :=
    CustomClass;
end;

end.
