{******************************************************************************}
{                                                                              }
{ Registers the plugin with the Delphi IDE.                                    }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DVSTPluginWizardReg;

interface

{$I ASIOVST.INC}

uses
  ToolsAPI,
  {$IFDEF DELPHI6_UP}
  DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
//DesignEditors, // TCustomModule moved to DesignEditors from Delphi 6
  DMForm,
  {$ELSE}
  DsgnIntf,
  DMDesigner,
  {$ENDIF}
  DVSTModule, DVSTPluginWizard;

procedure Register;

implementation

procedure Register;
begin
  // Register our DataModule Descendant
  {$IFDEF DELPHI5}
  RegisterCustomModule(TVSTModule, TDataModuleDesignerCustomModule);
  {$ELSE}
  RegisterCustomModule(TVSTModule, TDataModuleCustomModule);
  {$ENDIF}

  // Register our Wizard to add the new module to the Object Repository
  RegisterPackageWizard(TVSTPluginWizard.Create);
end;

end.

