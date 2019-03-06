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

unit DAV_VSTClonedProjectCreator;

// Code to generate the plugin's project file, i.e. <ProjectName.dpr>

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI, DAV_VSTPluginCloneConfig;

type
  TVSTClonedProjectCreator = class(TInterfacedObject,
    IOTACreator,
    {$IFDEF DELPHI8_UP} IOTAProjectCreator80, {$ENDIF}
    IOTAProjectCreator)
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
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    {$IFDEF DELPHI8_UP}
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  DAV_OpenToolsUtils;

const
  CRLF = #13#10;

constructor TVSTClonedProjectCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

// =============================================================================
// IOTACreator
// =============================================================================

{ Return a string representing the default creator type in which to augment }
function TVSTClonedProjectCreator.GetCreatorType: string;
begin
  // Create a library project
  Result := sLibrary;
end;

{ Return False if this is a new module }
function TVSTClonedProjectCreator.GetExisting: Boolean;
begin
  // Create a new project
  Result := False;
end;

{ Return the File system IDString that this module uses for reading/writing }
function TVSTClonedProjectCreator.GetFileSystem: string;
begin
  Result := ''; // Default
end;

{ Return the Owning module, if one exists (for a project module, this would
  be a project; for a project this is a project group) }
function TVSTClonedProjectCreator.GetOwner: IOTAModule;
begin
  // Owned by the current project group
  Result := GetCurrentProjectGroup;
end;

{ Return true, if this item is to be marked as un-named. This will force the
  "Save As" dialog to appear the first time the user saves. }
function TVSTClonedProjectCreator.GetUnnamed: Boolean;
begin
  // Project needs to be named/saved
  Result := False; // False still queries for a project name!
end;

// =============================================================================
// IOTAProjectCreator
// =============================================================================

{ Return the project filename. NOTE: This *must* be a fully qualified file name. }
function TVSTClonedProjectCreator.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.ProjectName + '.dpr';
end;

{ Deprecated!! Return the option file name (C++ .bpr, .bpk, etc...) }
function TVSTClonedProjectCreator.GetOptionFileName: string;
begin
  Result := ''; // Default
end;

{ Return True to show the source }
function TVSTClonedProjectCreator.GetShowSource: Boolean;
begin
  // Show the source in the editor
  Result := True;
end;

{ Deprecated!! Called to create a new default module for this project.
  Please implement and use the method on IOTAProjectCreator50. }
procedure TVSTClonedProjectCreator.NewDefaultModule;
//var
//  Module: IOTAModule;
//  ModuleCreator: TVSTModuleCreator;
begin
  //  ModuleCreator := TVSTModuleCreator.Create('');
  //  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

{ Deprecated!! Create and return the project option source. (C++) }
function TVSTClonedProjectCreator.NewOptionSource(const ProjectName: string):
  IOTAFile;
begin
  Result := nil;
end;

{ Called to indicate when to create/modify the project resource file }
procedure TVSTClonedProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // No resources needed
end;

{ Create and return the Project source file }
function TVSTClonedProjectCreator.NewProjectSource(const ProjectName: string):
  IOTAFile;
var
  S: string;
begin
  // returning nil would create the default source for a new library
  // but we create our own source file
  S :=
    '{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}' + CRLF +
    'library ' + ProjectName + ';' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Forms,' + CRLF +
    '  DAV_WinAmp,' + CRLF +
    '  DAV_VSTEffect,' + CRLF +
    '  DAV_VSTBasicModule;' + CRLF +
    CRLF +
    'function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;' + CRLF +
    'begin' + CRLF +
    '  Result := VstModuleMain(AudioMasterCallback, T' + FConfig.PluginFormName + ');' + CRLF +
    'end;' + CRLF +
    CRLF +
    'function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;' + CRLF +
    'begin' + CRLF +
    '  Result := WinampDSPModuleHeader(T' + FConfig.PluginFormName + ');' + CRLF +
    'end;' + CRLF +
    CRLF +
    'exports VstPluginMain name ''main'';' + CRLF +
    'exports VstPluginMain name ''VSTPluginMain'';' + CRLF +
    'exports WinampDSPGetHeader name ''winampDSPGetHeader2'';' + CRLF +
    CRLF +
    'begin' + CRLF +
    'end.';

  Result := StringToIOTAFile(S);
end;

{$IFDEF DELPHI8_UP}
// =============================================================================
// IOTAProjectCreator80
// =============================================================================

function TVSTClonedProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

// =============================================================================
// IOTAProjectCreator50
// =============================================================================

procedure TVSTClonedProjectCreator.NewDefaultProjectModule(const Project:
  IOTAProject);
begin
  NewDefaultModule;
end;
{$ENDIF}

end.
