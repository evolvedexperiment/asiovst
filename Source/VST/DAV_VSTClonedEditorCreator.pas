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

unit DAV_VSTClonedEditorCreator;

// Code to generate the GUI editor form.

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Forms, SysUtils, Graphics, 
  StdCtrls, ExtCtrls, ToolsAPI, DAV_VSTPluginCloneConfig;

type
  TVSTClonedEditorCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  DAV_OpenToolsUtils, DAV_VSTHost, Controls, Classes;

const
  CRLF          = #13#10;
  CAncestorName = 'Form';

constructor TVSTClonedEditorCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTClonedEditorCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Frm : TForm;
  Rct : TRect;
  Bmp : TBitmap;
begin
 Frm := TForm(INTAComponent(FormEditor.GetRootComponent).GetComponent);
 with Frm, FConfig do
  begin
   BorderStyle := bsNone;
   Scaled := False;

   VSTPlugin.ShowEdit(Frm);
   try
    VSTPlugin.EditIdle;
    VSTPlugin.Idle;
    Rct := VSTPlugin.GetRect;
    Frm.ClientWidth := Rct.Right - Rct.Left;
    Frm.ClientHeight := Rct.Bottom - Rct.Top;

    with TImage.Create(Frm) do
     begin
      Name := 'BackgroundImage';
      Parent := Frm;
      Width := Frm.ClientWidth;
      Height := Frm.ClientHeight;

      // create temp bitmap and render GUI
      Bmp := TBitmap.Create;
      try
       VSTPlugin.RenderEditorToBitmap(Bmp);
       Picture.Assign(Bmp);
      finally
       FreeAndNil(Bmp);
      end;

     end;
   finally
    VSTPlugin.CloseEdit;
   end;
  end;
end;

function TVSTClonedEditorCreator.GetAncestorName: string;
begin
  Result := CAncestorName;
end;

function TVSTClonedEditorCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTClonedEditorCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTClonedEditorCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTClonedEditorCreator.GetFormName: string;
begin
  Result := FConfig.EditorFormName;
end;

function TVSTClonedEditorCreator.GetImplFileName: string;
begin
  Result := {$IFDEF DELPHI6_UP}IncludeTrailingPathDelimiter{$ENDIF}(FConfig.ProjectPath) +
    FConfig.EditorUnitName + '.pas';
end;

function TVSTClonedEditorCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTClonedEditorCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TVSTClonedEditorCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTClonedEditorCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedEditorCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTClonedEditorCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTClonedEditorCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  // we initialise the form in the FormCreated procedure instead of writing out
  // a specific form definition here
  Result := nil
end;

function TVSTClonedEditorCreator.NewImplSource(const ModuleIdent, FormIdent,
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
    '  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF +
    '  end;' + CRLF + CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF +
    'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTClonedEditorCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.

