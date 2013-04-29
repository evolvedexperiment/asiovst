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

unit DAV_AsioDriverWizard;

// The main class for the ASIO Driver Wizard. The Execute method drives the
// process.

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, ToolsAPI, DAV_AsioDriverConfig;

type
  TAsioDriverWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTAProjectWizard)
  private
    procedure CreateControlPanelForm(Config: TConfig);
    procedure CreateProject(Config: TConfig);
    procedure CreateAsioDriverModule(Config: TConfig);
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
{$IFDEF DELPHI6_UP}
    function GetGlyph: cardinal;
{$ELSE}
    function GetGlyph: HICON;
{$ENDIF}
  end;

implementation

{ -$R ..\..\Resources\DAV_AsioDriverWizard.res }

uses
  Dialogs, DAV_AsioDriverProjectCreator, DAV_AsioDriverModuleCreator,
  DAV_AsioDriverControlPanelCreator, DAV_AsioDriverWizardFrm;

const
  CWizardId = '{079A1F78-23D1-4C7A-A995-B0A75D892A21}';
  CWizardPage = 'ASIO';
  CWizardName = 'ASIO Driver';
  CWizardAuthor = 'Christian Budde';
  CWizardComment = 'ASIO Driver Wizard';
  CWizardIcon = 'AsioDriverWIZARD';

resourcestring
  // error messages
  CProjectCreationErrorMessage =
    'The wizard encountered an error while generating the main project file.';
  CAsioDriverModuleCreationErrorMessage =
    'The wizard encountered an error while generating the AsioDriverModule unit.';
  CEditorFormCreationErrorMessage =
    'The wizard encountered an error while generating the editor form unit.';

procedure TAsioDriverWizard.CreateControlPanelForm(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices)
      .CreateModule(TAsioDriverControlPanelCreator.Create(Config));
  except
    MessageDlg(CEditorFormCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TAsioDriverWizard.CreateProject(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices)
      .CreateModule(TAsioDriverProjectCreator.Create(Config));
  except
    MessageDlg(CProjectCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TAsioDriverWizard.CreateAsioDriverModule(Config: TConfig);
begin
  try
    (BorlandIDEServices as IOTAModuleServices)
      .CreateModule(TAsioDriverModuleCreator.Create(Config));
  except
    MessageDlg(CAsioDriverModuleCreationErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TAsioDriverWizard.Execute;
var
  Config: TConfig;
begin
  Config := TConfig.Create;
  try
    if ShowWizardGuiDialog(Config) then
    begin
      CreateProject(Config);
      CreateAsioDriverModule(Config);
      if Config.UseControlPanel then
      begin
        Sleep(20);
        CreateControlPanelForm(Config);
      end;
    end;
  finally
    Config.Free;
  end;
end;

function TAsioDriverWizard.GetAuthor: string;
begin
  Result := CWizardAuthor;
end;

function TAsioDriverWizard.GetComment: string;
begin
  Result := CWizardComment;
end;

{$IFDEF DELPHI6_UP}

function TAsioDriverWizard.GetGlyph: cardinal;
{$ELSE}

function TAsioDriverWizard.GetGlyph: HICON;
{$ENDIF}
begin
  Result := LoadIcon(hInstance, CWizardIcon);
end;

function TAsioDriverWizard.GetIDString: string;
begin
  Result := CWizardId;
end;

function TAsioDriverWizard.GetName: string;
begin
  Result := CWizardName;
end;

function TAsioDriverWizard.GetPage: string;
begin
  Result := CWizardPage;
end;

function TAsioDriverWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
