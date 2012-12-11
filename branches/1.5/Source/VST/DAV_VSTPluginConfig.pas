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

unit DAV_VSTPluginConfig;

// Class used to hold the configuration information entered on the Wizard form
// and enable it to be accessed by the code generation process.

interface

{$I ..\DAV_Compiler.inc}

type
  TConfig = class(TObject)
  private
    function RandomLetter: Char;
    function RandomUniqueID: string;
  public
    ProjectPath    : string;
    ProjectName    : string;
    PluginUnitName : string;
    PluginFormName : string;
    EditorUnitName : string;
    EditorFormName : string;
    UseEditor      : Boolean;
    UniqueID       : AnsiString;
    EffectName     : AnsiString;
    IsSynth        : Boolean;
    VersionMajor   : Integer;
    VersionMinor   : Integer;
    VersionRelease : Integer;
    VendorName     : AnsiString;
    ProductName    : AnsiString;
    SaveWhenDone   : Boolean;
    constructor Create;
  end;

implementation

uses
  Math, SysUtils, Registry, DAV_OpenToolsUtils;

const
  CDefaultUseEditor      = True;
  CDefaultIsSynth        = False;
  CDefaultVersionMajor   = 1;
  CDefaultVersionMinor   = 0;
  CDefaultVersionRelease = 0;

resourcestring
  RCDefaultPluginUnitName = 'PluginDM';
  RCDefaultPluginFormName = 'PluginDataModule';
  RCDefaultEditorUnitName = 'EditorFrm';
  RCDefaultEditorFormName = 'EditorForm';
  RCDefaultEffectName     = 'My Plugin';
  RCDefaultVendorName     = 'My Company';
  RCDefaultProductName    = 'My Product';

constructor TConfig.Create;
begin
  ProjectPath    := GetCurrentDir;
  ProjectName    := GetUniqueProjectName;
  PluginUnitName := RCDefaultPluginUnitName;
  PluginFormName := RCDefaultPluginFormName;
  EditorUnitName := RCDefaultEditorUnitName;
  EditorFormName := RCDefaultEditorFormName;
  UseEditor      := CDefaultUseEditor;
  UniqueID       := AnsiString(RandomUniqueID);
  EffectName     := AnsiString(RCDefaultEffectName);
  IsSynth        := CDefaultIsSynth;
  VersionMajor   := CDefaultVersionMajor;
  VersionMinor   := CDefaultVersionMinor;
  VersionRelease := CDefaultVersionRelease;
  VendorName     := AnsiString(RCDefaultVendorName);
  ProductName    := AnsiString(RCDefaultProductName);
end;

function TConfig.RandomLetter: Char;
begin
 {$IFDEF Delphi5}
 Result := Char(Ord('A') + Random(Ord('Z') - Ord('A')) + Random(2) * 32);
 {$ELSE}
 Result := Char(RandomRange(Ord('A'), Ord('Z')) + Random(2) * 32);
 {$ENDIF}
end;

function TConfig.RandomUniqueID: string;
begin
  Result := RandomLetter + RandomLetter + RandomLetter + RandomLetter;
end;

end.
