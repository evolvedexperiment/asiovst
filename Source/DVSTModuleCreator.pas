{******************************************************************************}
{                                                                              }
{ Code to generate the VSTModule-derived Data Module unit, where the audio     }
{ processing code will reside.                                                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DVSTModuleCreator;

interface

uses
  ToolsAPI,
  Config;

type
  TVSTModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
  SysUtils,
  DVSTModule, DVSTEffect, OpenToolsUtils;

const
  CRLF               = #13#10;
  ANCESTOR_NAME      = 'VSTModule';
  NUM_INPUTS_EFFECT  = 2;
  NUM_OUTPUTS_EFFECT = 2;
  NUM_INPUTS_SYNTH   = 0;
  NUM_OUTPUTS_SYNTH  = 2;

constructor TVSTModuleCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  NativeFormEditor: INTAFormEditor;
begin
  with TVSTModule(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
    UniqueID       := FConfig.UniqueID;
    EffectName     := FConfig.EffectName;
    VersionMajor   := FConfig.VersionMajor;
    VersionMinor   := FConfig.VersionMinor;
    VersionRelease := FConfig.VersionRelease;
    VendorName     := FConfig.VendorName;
    ProductName    := FConfig.ProductName;

    if FConfig.IsSynth then
    begin
      NumInputs    := NUM_INPUTS_SYNTH;
      NumOutputs   := NUM_OUTPUTS_SYNTH;
      PlugCategory := vpcSynth;
      Flags := Flags + [effFlagsIsSynth];
    end
    else
    begin
      NumInputs    := NUM_INPUTS_EFFECT;
      NumOutputs   := NUM_OUTPUTS_EFFECT;
      PlugCategory := vpcEffect;
    end;

    if FConfig.UseEditor then
    begin
      Flags := Flags + [effFlagsHasEditor];
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
    end;
  end;
end;

function TVSTModuleCreator.GetAncestorName: string;
begin
  Result := ANCESTOR_NAME;
end;

function TVSTModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTModuleCreator.GetFormName: string;
begin
  Result := FConfig.PluginFormName;
end;

function TVSTModuleCreator.GetImplFileName: string;
begin
  //Result := '';
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.PluginUnitName + '.pas';
end;

function TVSTModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  Result := nil
end;

function TVSTModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  // Can use either FConfig.PluginFormName or FormIdent here, as they are set to
  // to the same value in TVSTModuleCreator.GetFormName
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, ' + CRLF +
    '  DDSPBase, DVSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF;

  if FConfig.UseEditor then
  begin
    s := s +
      '    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);' +
      CRLF;
  end;

  s := s +
    '  private' + CRLF +
    '  public' + CRLF +
    '  end;' + CRLF +
    CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF;

  if FConfig.UseEditor then
  begin
    s := s +
      'uses' + CRLF +
      '  ' + FConfig.EditorUnitName + ';' + CRLF +
      CRLF +
      'procedure T' + FormIdent + '.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);' + CRLF +
      'begin' + CRLF +
      '  GUI := T' + FConfig.EditorFormName + '.Create(nil);' + CRLF +
      '  (GUI As T' + FConfig.EditorFormName + ').' + FormIdent + ' := Self;' + CRLF +
      'end;' + CRLF +
      CRLF;
  end;

  s := s + 'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.

