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

unit DAV_VSTModuleLazIDE;

{$I ..\DAV_Compiler.inc}

interface

uses
  Classes, SysUtils, Controls, DAV_VSTModule, Forms, LazIDEIntf, ProjectIntf,
  CompOptsIntf, FormEditingIntf;

type
  { TVSTModuleLibraryDescriptor }
  TVSTModuleLibraryDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitWithVSTModule }
  TFileDescPascalUnitWithVSTModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

var
  ProjectDescriptorVSTModuleLibrary: TVSTModuleLibraryDescriptor;
  FileDescriptorVSTModule: TFileDescPascalUnitWithVSTModule;

procedure Register;

implementation

procedure Register;
begin
  FileDescriptorVSTModule := TFileDescPascalUnitWithVSTModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorVSTModule);
  ProjectDescriptorVSTModuleLibrary := TVSTModuleLibraryDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorVSTModuleLibrary);
  FormEditingHook.RegisterDesignerBaseClass(TVSTModule);
end;

{ TVSTModuleApplicationDescriptor }

constructor TVSTModuleLibraryDescriptor.Create;
begin
  inherited Create;
  Name := 'VSTModule';
end;

function TVSTModuleLibraryDescriptor.GetLocalizedName: string;
begin
  Result := 'VST Plugin';
end;

function TVSTModuleLibraryDescriptor.GetLocalizedDescription: string;
begin
  Result := 'VST Plugin'#13#13'VST Plugin Wizard in Free Pascal';
end;

function TVSTModuleLibraryDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile := AProject.CreateProjectFile('VSTPlugin1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;

  // create program source
  le := LineEnding;
  NewSource := 'library VSTPlugin1;' + le
    + le
    + '{$I DAV_Compiler.inc}' + le
    + le
    + 'uses' + le
    + '  DAV_VSTEffect,' + le
    + '  {$IFDEF MSWINDOWS}' + le
    + '  DAV_WinAmp,' + le
    + '  {$ENDIF}' + le
    + '  DAV_VSTModule;' + le
    + le
    + 'function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;' + le
    + 'begin' + le
    + '  Result := VstModuleMain(AudioMasterCallback, TVSTModule1);' + le
    + 'end;' + le
    + le
    + '{$IFDEF MSWINDOWS}' + le
    + 'function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;' + le
    + 'begin' + le
    + '  Result := WinampDSPModuleHeader(TSimpleFlangerModule);' + le
    + 'end;' + le
    + '{$ENDIF}' + le
    + le
    + 'exports' + le
    + '{$IFDEF DARWIN}  {OS X entry points}' + le
    + '  VSTPluginMain name ''_main'',' + le
    + '  VSTPluginMain name ''_main_macho'',' + le
    + '  VSTPluginMain name ''_VSTPluginMain'';' + le
    + '{$ELSE}' + le
    + '  VSTPluginMain name ''main'',' + le
    + '  VSTPluginMain name ''main_plugin'',' + le
    + '  VSTPluginMain name ''VSTPluginMain'',' + le
    + '{$IFDEF MSWINDOWS}' + le
    + '  WinampDSPGetHeader name ''winampDSPGetHeader2'';' + le
    + '{$ENDIF}' + le
    + '{$ENDIF}' + le
    + le
    + 'begin' + le
    + ' Application.Initialize;' + le
    + 'end.';
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('DAV_Common_Lazarus');
  AProject.AddPackageDependency('DAV_VSTPlugin_Lazarus');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp := True;
  AProject.LazCompilerOptions.ExecutableType := cetLibrary;
  AProject.LazCompilerOptions.StripSymbols := True;
  AProject.LazCompilerOptions.SmartLinkUnit := True;
  AProject.Title := 'VST Plugin';
  Result :=  mrOK;
end;

function TVSTModuleLibraryDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  if AProject = nil then ;
  LazarusIDE.DoNewEditorFile(FileDescriptorVSTModule, '', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  Result :=  mrOK;
end;

{ TFileDescPascalUnitWithVSTModule }

constructor TFileDescPascalUnitWithVSTModule.Create;
begin
  inherited Create;
  Name := 'VSTModule';
  ResourceClass := TVSTModule;
  UseCreateFormStatements := True;
end;

function TFileDescPascalUnitWithVSTModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', DAV_VSTModule';
end;

function TFileDescPascalUnitWithVSTModule.GetLocalizedName: string;
begin
  Result := 'VST Module';
end;

function TFileDescPascalUnitWithVSTModule.GetLocalizedDescription: string;
begin
  Result := 'VST Module'#13
         +'A datamodule for VST Plugins';
end;

initialization

end.
