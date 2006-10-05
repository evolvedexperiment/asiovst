{******************************************************************************}
{                                                                              }
{ Class used to hold the configuration information entered on the Wizard form  }
{ and enable it to be accessed by the code generation process.                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit Config;

interface

type
  TConfig = class(TObject)
  private
    function RandomLetter: char;
    function RandomUniqueID: string;
  public
    ProjectPath: string;
    ProjectName: string;
    PluginUnitName: string;
    PluginFormName: string;
    EditorUnitName: string;
    EditorFormName: string;
    UseEditor: boolean;
    UniqueID: string;
    EffectName: string;
    IsSynth: boolean;
    VersionMajor: integer;
    VersionMinor: integer;
    VersionRelease: integer;
    VendorName: string;
    ProductName: string;
    constructor Create;
  end;

implementation

uses
  Math, SysUtils,
  OpenToolsUtils;

const
  DEFAULT_PLUGIN_UNIT_NAME = 'PluginDM';
  DEFAULT_PLUGIN_FORM_NAME = 'PluginDataModule';
  DEFAULT_EDITOR_UNIT_NAME = 'EditorFrm';
  DEFAULT_EDITOR_FORM_NAME = 'EditorForm';
  DEFAULT_USE_EDITOR       = True;
  DEFAULT_EFFECT_NAME      = 'My Plugin';
  DEFAULT_IS_SYNTH         = False;
  DEFAULT_VERSION_MAJOR    = 1;
  DEFAULT_VERSION_MINOR    = 0;
  DEFAULT_VERSION_RELEASE  = 0;
  DEFAULT_VENDOR_NAME      = 'My Company';
  DEFAULT_PRODUCT_NAME     = 'My Product';

constructor TConfig.Create;
begin
  ProjectPath    := GetCurrentDir;
  ProjectName    := GetUniqueProjectName;
  PluginUnitName := DEFAULT_PLUGIN_UNIT_NAME;
  PluginFormName := DEFAULT_PLUGIN_FORM_NAME;
  EditorUnitName := DEFAULT_EDITOR_UNIT_NAME;
  EditorFormName := DEFAULT_EDITOR_FORM_NAME;
  UseEditor      := DEFAULT_USE_EDITOR;
  UniqueID       := RandomUniqueID;
  EffectName     := DEFAULT_EFFECT_NAME;
  IsSynth        := DEFAULT_IS_SYNTH;
  VersionMajor   := DEFAULT_VERSION_MAJOR;
  VersionMinor   := DEFAULT_VERSION_MINOR;
  VersionRelease := DEFAULT_VERSION_RELEASE;
  VendorName     := DEFAULT_VENDOR_NAME;
  ProductName    := DEFAULT_PRODUCT_NAME;
end;

function TConfig.RandomLetter: char;
begin
  if Random < 0.5 then
    Result := char(RandomRange(Ord('A'), Ord('Z')))
  else
    Result := char(RandomRange(Ord('a'), Ord('z')));
end;

function TConfig.RandomUniqueID: string;
begin
  Result := RandomLetter + RandomLetter + RandomLetter + RandomLetter;
end;

end.

