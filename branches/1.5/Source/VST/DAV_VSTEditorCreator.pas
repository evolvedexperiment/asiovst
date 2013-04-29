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

unit DAV_VSTEditorCreator;

// Code to generate the GUI editor form.                                        }

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_VSTPluginConfig;

type
  TVSTEditorCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FConfig: TConfig;
  public
    constructor Create(Config: TConfig);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  Forms, SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF          = #13#10;
  ANCESTOR_NAME = 'Form';

constructor TVSTEditorCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTEditorCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  with TForm(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
    BorderStyle := bsNone;
    Scaled := False;
  end;
end;

function TVSTEditorCreator.GetAncestorName: string;
begin
  Result := ANCESTOR_NAME;
end;

function TVSTEditorCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTEditorCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTEditorCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTEditorCreator.GetFormName: string;
begin
  Result := FConfig.EditorFormName;
end;

function TVSTEditorCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) +
    FConfig.EditorUnitName + '.pas';
end;

function TVSTEditorCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTEditorCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TVSTEditorCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTEditorCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTEditorCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTEditorCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTEditorCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TVSTEditorCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF +
    //'  public' + CRLF +
    //'    ' + FConfig.PluginFormName + ': TVSTModule;' + CRLF +
    '  end;' + CRLF + CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF +
    'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTEditorCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
