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

unit DAV_VSTClonedModuleCreator;

// Code to generate the VSTModule-derived Data Module unit, where the audio
// processing code will reside.

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI, DAV_VSTHost, DAV_VSTCustomModule, DAV_VSTModule,
  DAV_VSTPluginCloneConfig;

type
  TVSTClonedModuleCreator = class(TInterfacedObject, IOTACreator,
    IOTAModuleCreator)
  private
    FConfig: TConfig;
    procedure ClonePlugin(const VstModule: TVSTModule;
      VstPlugin: TCustomVstPlugIn);
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
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  SysUtils, Classes, {$IFDEF DELPHI14_UP} AnsiStrings, {$ENDIF} DAV_VSTEffect,
  DAV_OpenToolsUtils, DAV_VSTParameters;

const
  CRLF = #13#10;
  CAncestorName = 'VSTModule';

constructor TVSTClonedModuleCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTClonedModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  NativeFormEditor: INTAFormEditor;
  VstModule: TVSTModule;
begin
  VstModule := TVSTModule(INTAComponent(FormEditor.GetRootComponent)
    .GetComponent);
  with VstModule do
  begin
    ClonePlugin(VstModule, FConfig.VstPlugin);

    // wire the OnEditOpen event handler to our VSTModuleEditOpen method
    if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
    begin
      if NativeFormEditor.FormDesigner <> nil then
      begin
        DoCreateMethod(NativeFormEditor.FormDesigner,
          NativeFormEditor.FormDesigner.GetRoot, 'Open', 'VSTModuleOpen');
      end;
    end;

    if FConfig.ContainsGui then
    begin
      // wire the OnEditOpen event handler to our VSTModuleEditOpen method
      if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
      begin
        if NativeFormEditor.FormDesigner <> nil then
        begin
          DoCreateMethod(NativeFormEditor.FormDesigner,
            NativeFormEditor.FormDesigner.GetRoot, 'EditOpen',
            'VSTModuleEditOpen');
        end;
      end;
    end
    else
      Flags := Flags + [effFlagsHasEditor];
  end;
end;

procedure TVSTClonedModuleCreator.ClonePlugin(const VstModule: TVSTModule;
  VstPlugin: TCustomVstPlugIn);
var
  i: Integer;
  MinParam, MaxParam: Single;
  CurrentValue: Single;
  OriginalParameter: Single;
  ParamProperty: TVstParameterPropertyRecord;
begin
  with VstModule do
  begin
    UniqueID := VstPlugin.UniqueID;
    EffectName := VstPlugin.EffectName;
    if VstPlugin.Version < 10 then
    begin
      VersionRelease := VstPlugin.Version;
      Version := AnsiString(IntToStr(VstPlugin.Version));
    end
    else
    begin
      VersionMajor := VstPlugin.Version div 1000;
      VersionMinor := (VstPlugin.Version - VersionMajor * 1000) div 100;
      VersionRelease := (VstPlugin.Version - VersionMajor * 1000 -
        VersionMinor * 100);
      Version := AnsiString(IntToStr(VersionMajor) + '.' +
        IntToStr(VersionMinor));
    end;
    VendorName := VstPlugin.VendorString;
    ProductName := VstPlugin.ProductString;
    Flags := VstPlugin.EffectOptions;

    numInputs := VstPlugin.numInputs;
    numOutputs := VstPlugin.numOutputs;
    InitialDelay := VstPlugin.InitialDelay;
    RealQualities := VstPlugin.RealQualities;
    OffQualities := VstPlugin.OffQualities;
    IORatio := VstPlugin.IORatio;
    PlugCategory := VstPlugin.PlugCategory;
    CanDos := VstPlugin.VSTCanDos;

    for i := 0 to VstPlugin.numParams - 1 do
      with ParameterProperties.Add do
      begin
        DisplayName := string(VstPlugin.ParameterName[i]);
        Units := VstPlugin.ParameterLabel[i];

        try
          if VstPlugin.GetParameterProperties(i, ParamProperty) then
          begin
            StepFloat := ParamProperty.StepFloat;
            SmallStepFloat := ParamProperty.SmallStepFloat;
            LargeStepFloat := ParamProperty.LargeStepFloat;
            Flags := ParamProperty.Flags;
            MinInteger := ParamProperty.MinInteger;
            MaxInteger := ParamProperty.MaxInteger;
            StepInteger := ParamProperty.StepInteger;
            LargeStepInteger := ParamProperty.LargeStepInteger;
            Category := ParamProperty.CategoryLabel;
            ShortLabel := ParamProperty.ShortLabel;
          end;
        except
        end;

        CanBeAutomated := VstPlugin.CanBeAutomated(i) > 0;
        OriginalParameter := VstPlugin.Parameter[i];

        // check parameter minimum
        try
          Parameter[i] := 0;
          MinParam := StrToFloat(string(VstPlugin.ParameterDisplay[i]));
        except
          MinParam := 0;
        end;

        // check parameter maximum
        try
          Parameter[i] := 1;
          MaxParam := StrToFloat(string(VstPlugin.ParameterDisplay[i]));
        except
          MaxParam := 1;
        end;

        // eventually set min/max
        if MinParam < MaxParam then
          try
            Parameter[i] := 0.5;
            CurrentValue := StrToFloat(string(VstPlugin.ParameterDisplay[i]));
            if (CurrentValue > MinParam) and (CurrentValue < MaxParam) then
            begin
              Min := MinParam;
              Max := MaxParam;
            end;
          except
          end
        else
        begin
          // ToDo
        end;

        VstPlugin.Parameter[i] := OriginalParameter;
      end;

    for i := 0 to VstPlugin.numPrograms - 1 do
      with Programs.Add do
      begin
        VstPlugin.CurrentProgram := i;
        DisplayName := string(VstPlugin.ProgramName);
      end;
  end;
end;

function TVSTClonedModuleCreator.GetAncestorName: string;
begin
  Result := CAncestorName;
end;

function TVSTClonedModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTClonedModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTClonedModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTClonedModuleCreator.GetFormName: string;
begin
  Result := FConfig.PluginFormName;
end;

function TVSTClonedModuleCreator.GetImplFileName: string;
begin
  // Result := '';
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.PluginUnitName + '.pas';
end;

function TVSTClonedModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTClonedModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTClonedModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTClonedModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TVSTClonedModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
  flt: Single;
  i, j: Integer;
begin
  // Can use either FConfig.PluginFormName or FormIdent here, as they are set to
  // to the same value in TVSTClonedModuleCreator.GetFormName
  s := 'unit ' + ModuleIdent + ';' + CRLF + CRLF + 'interface' + CRLF + CRLF +
    'uses ' + CRLF + '  Windows, Messages, SysUtils, Classes, Forms, ' + CRLF +
    '  DAV_Common, DAV_VSTModule;' + CRLF + CRLF + 'type' + CRLF + '  T' +
    FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF;

  s := s + '    procedure VSTModuleOpen(Sender: TObject);' + CRLF;

  if FConfig.ContainsGui then
  begin
    s := s + '    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);'
      + CRLF;
  end;

  s := s + '  private' + CRLF + '  public' + CRLF + '  end;' + CRLF + CRLF +
    'implementation' + CRLF + CRLF + '{$R *.DFM}' + CRLF + CRLF;

  // add uses
  if FConfig.ContainsGui then
  begin
    s := s + 'uses' + CRLF + '  ' + FConfig.EditorUnitName + ';' + CRLF + CRLF;
  end;

  // initialize programs
  if FConfig.VstPlugin.numPrograms > 0 then
  begin
    s := s + 'procedure T' + FormIdent + '.VSTModuleOpen(Sender: TObject);' +
      CRLF + 'begin' + CRLF;

    for i := 0 to FConfig.VstPlugin.numPrograms - 1 do
    begin
      FConfig.VstPlugin.CurrentProgram := i;
      s := s + ' with Programs[' + IntToStr(i) + '] do' + CRLF +
        '  begin' + CRLF;

      // scan programs
      for j := 0 to FConfig.VstPlugin.numParams - 1 do
      begin
        try
          if TryStrToFloat(string(FConfig.VstPlugin.ParameterDisplay[j]), flt)
          then
            s := s + '   Parameter[' + IntToStr(j) + '] := ' +
              string(FConfig.VstPlugin.ParameterDisplay[j]) + ';' + CRLF;
        except
        end;
      end;

      s := s + '  end;' + CRLF;
    end;

    s := s + 'end;' + CRLF + CRLF;
  end;

  // add Editor Open procedure
  if FConfig.ContainsGui then
  begin
    s := s + 'procedure T' + FormIdent +
      '.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);'
      + CRLF + 'begin' + CRLF + '  GUI := T' + FConfig.EditorFormName +
      '.Create(Self);' + CRLF + 'end;' + CRLF + CRLF;
  end;

  s := s + 'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTClonedModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
