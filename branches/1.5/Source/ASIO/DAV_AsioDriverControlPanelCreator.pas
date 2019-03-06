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

unit DAV_AsioDriverControlPanelCreator;

// Code to generate the GUI ControlPanel form.

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_AsioDriverConfig;

type
  TAsioDriverControlPanelCreator = class(TInterfacedObject, IOTACreator,
    IOTAModuleCreator)
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
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile;
    procedure FormCreated(const FormControlPanel: IOTAFormEditor);
  end;

implementation

uses
  Forms, SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF = #13#10;
  ANCESTOR_NAME = 'Form';

constructor TAsioDriverControlPanelCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TAsioDriverControlPanelCreator.FormCreated(const FormControlPanel
  : IOTAFormEditor);
begin
  with TForm(INTAComponent(FormControlPanel.GetRootComponent).GetComponent) do
  begin
    BorderStyle := bsNone;
    Scaled := False;
  end;
end;

function TAsioDriverControlPanelCreator.GetAncestorName: string;
begin
  Result := ANCESTOR_NAME;
end;

function TAsioDriverControlPanelCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TAsioDriverControlPanelCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TAsioDriverControlPanelCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TAsioDriverControlPanelCreator.GetFormName: string;
begin
  Result := FConfig.ControlPanelFormName;
end;

function TAsioDriverControlPanelCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) + FConfig.ControlPanelUnitName + '.pas';
end;

function TAsioDriverControlPanelCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TAsioDriverControlPanelCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TAsioDriverControlPanelCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TAsioDriverControlPanelCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TAsioDriverControlPanelCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TAsioDriverControlPanelCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TAsioDriverControlPanelCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TAsioDriverControlPanelCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  s := 'unit ' + ModuleIdent + ';' + CRLF + CRLF + 'interface' + CRLF + CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_AsioDriverModule;'
    + CRLF + CRLF + 'type' + CRLF + '  T' + FormIdent + ' = class(T' +
    AncestorIdent + ')' + CRLF +
  // '  public' + CRLF +
  // '    ' + FConfig.PluginFormName + ': TAsioDriverModule;' + CRLF +
    '  end;' + CRLF + CRLF + 'implementation' + CRLF + CRLF + '{$R *.DFM}' +
    CRLF + CRLF + 'end.';

  Result := StringToIOTAFile(s);
end;

function TAsioDriverControlPanelCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
