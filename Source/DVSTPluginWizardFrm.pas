{******************************************************************************}
{                                                                              }
{ The Wizard form used to collect the configuration information from the user  }
{ before commencing the code generation process.                               }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DVSTPluginWizardFrm;

interface

uses
  Windows, Messages, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Config, Graphics;

type
  TVSTPluginWizardForm = class(TForm)
    LbEffectName: TLabel;
    Image1: TImage;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Image2: TImage;
    Label9: TLabel;
    Label10: TLabel;
    Bevel1: TBevel;
    Panel2: TPanel;
    PageControl: TPageControl;
    ts_Welcome: TTabSheet;
    Label11: TLabel;
    ts_Finish: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    TabSheet1: TTabSheet;
    Label23: TLabel;
    Label26: TLabel;
    optPluginTypeSynth: TRadioButton;
    TabSheet2: TTabSheet;
    optPluginTypeEffect: TRadioButton;
    chkUseEditor: TCheckBox;
    TabSheet3: TTabSheet;
    Label6: TLabel;
    Label19: TLabel;
    edtProjectPath: TEdit;
    Label20: TLabel;
    edtProjectName: TEdit;
    Label21: TLabel;
    TabSheet4: TTabSheet;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    edtPluginFormName: TEdit;
    Label27: TLabel;
    edtPluginUnitName: TEdit;
    Label28: TLabel;
    Label1: TLabel;
    Label29: TLabel;
    TabSheet5: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    Label34: TLabel;
    edtEffectName: TEdit;
    btnFinish: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    btnBrowse: TButton;
    Label4: TLabel;
    edtProductName: TEdit;
    TabSheet6: TTabSheet;
    Label33: TLabel;
    Label36: TLabel;
    Label35: TLabel;
    edtVersionMajor: TEdit;
    Label5: TLabel;
    edtUniqueID: TEdit;
    Label37: TLabel;
    edtVendorName: TEdit;
    pnlEditorDetails: TPanel;
    Label32: TLabel;
    edtEditorUnitName: TEdit;
    Label31: TLabel;
    edtEditorFormName: TEdit;
    lblEditorFormName: TLabel;
    edtVersionMinor: TEdit;
    Label30: TLabel;
    edtVersionRelease: TEdit;
    Label38: TLabel;
    Bevel2: TBevel;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure edtVersionMajorKeyPress(Sender: TObject; var Key: Char);
    procedure chkUseEditorClick(Sender: TObject);
  private
    procedure InvalidFieldError(Control: TWinControl; const MessageText: string);
    procedure SetActiveSheetControlFocus;
    procedure SetNavigationButtons;
    procedure TrimAllEditBoxes;
    function ValidateStep(StepIndex: Integer): boolean;
  public
    procedure InitConfigFromForm(Config: TConfig);
    procedure InitFormFromConfig(Config: TConfig);
  end;

function ShowWizardGuiDialog(Config: TConfig): Boolean;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Dialogs, SysUtils;

{$R *.DFM}

const
  STEP_WELCOME        = 0;
  STEP_DESTINATION    = 1;
  STEP_PLUGIN_TYPE    = 2;
  STEP_MODULE         = 3;
  STEP_EDITOR         = 4;
  STEP_NAMES          = 5;
  STEP_VERSION_AND_ID = 6;
  STEP_FINISH         = 7;

  BROWSE_PATH_DIALOG_CAPTION = 'Select Project Directory';
  MISSING_PROJECT_PATH = 'You must enter a project path.';
  MISSING_PROJECT_NAME = 'You must enter a project name.';
  MISSING_PLUGIN_FORM_NAME =
    'You must enter a name for the VSTModule descendant class.';
  PLUGIN_FORM_NAME_SAME_AS_BASE =
    'Your descendant class name must be different from the base class name.';
  MISSING_PLUGIN_UNIT_NAME =
    'You must enter a unit name for the plugin module.';
  MISSING_EDITOR_FORM_NAME = 'You must enter a name for the editor form.';
  MISSING_EDITOR_UNIT_NAME = 'You must enter a unit name for the editor form.';
  MISSING_EFFECT_NAME = 'You must enter a name for the plugin.';
  MISSING_PRODUCT_NAME = 'You must enter a product name.';
  MISSING_VENDOR_NAME = 'You must enter your company name.';
  MISSING_VERSION_MAJOR = 'You must enter a major version number.';
  MISSING_VERSION_MINOR = 'You must enter a minor version number.';
  MISSING_VERSION_RELEASE = 'You must enter a release version number.';
  MISSING_UNIQUE_ID = 'You must enter a unique ID for the plugin.';
  INVALID_UNIQUE_ID_LENGTH =
    'The unique ID must be exactly four characters long';
  PROJECT_PATH_DOES_NOT_EXIST =
    'The project path does not exist. Would you like to create it?';

function ShowWizardGuiDialog(Config: TConfig): Boolean;
begin
  with TVSTPluginWizardForm.Create(nil) do
  try
    InitFormFromConfig(Config);
    Result := (ShowModal = mrOK);
    if Result then
      InitConfigFromForm(Config);
  finally
    Free;
  end;
end;

procedure TVSTPluginWizardForm.btnNextClick(Sender: TObject);
begin
  if ValidateStep(PageControl.ActivePageIndex) then
  begin
    PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
    SetNavigationButtons;
    SetActiveSheetControlFocus;
  end;
end;

procedure TVSTPluginWizardForm.btnPrevClick(Sender: TObject);
begin
  PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
  SetNavigationButtons;
  SetActiveSheetControlFocus;
end;

procedure TVSTPluginWizardForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
  begin
    PageControl.Pages[i].TabVisible := False;
  end;
  Height := Height - 16;
  PageControl.ActivePageIndex := STEP_WELCOME;
  SetNavigationButtons;
end;

procedure TVSTPluginWizardForm.InitConfigFromForm(Config: TConfig);
begin
  Config.ProjectPath    := edtProjectPath.Text;
  Config.ProjectName    := edtProjectName.Text;
  Config.PluginUnitName := edtPluginUnitName.Text;
  Config.PluginFormName := edtPluginFormName.Text;
  Config.EditorUnitName := edtEditorUnitName.Text;
  Config.EditorFormName := edtEditorFormName.Text;
  Config.UseEditor      := chkUseEditor.Checked;
  Config.UniqueID       := edtUniqueID.Text;
  Config.EffectName     := edtEffectName.Text;
  Config.IsSynth        := optPluginTypeSynth.Checked;
  Config.VersionMajor   := StrToInt(edtVersionMajor.Text);
  Config.VersionMinor   := StrToInt(edtVersionMinor.Text);
  Config.VersionRelease := StrToInt(edtVersionRelease.Text);
  Config.VendorName     := edtVendorName.Text;
  Config.ProductName    := edtProductName.Text;
end;

procedure TVSTPluginWizardForm.InitFormFromConfig(Config: TConfig);
begin
  edtProjectPath.Text    := Config.ProjectPath;
  edtProjectName.Text    := Config.ProjectName;
  edtPluginUnitName.Text := Config.PluginUnitName;
  edtPluginFormName.Text := Config.PluginFormName;
  edtEditorUnitName.Text := Config.EditorUnitName;
  edtEditorFormName.Text := Config.EditorFormName;
  chkUseEditor.Checked   := Config.UseEditor;
  edtUniqueID.Text       := Config.UniqueID;
  edtEffectName.Text     := Config.EffectName;
  optPluginTypeEffect.Checked := not Config.IsSynth;
  optPluginTypeSynth.Checked  := Config.IsSynth;
  edtVersionMajor.Text   := IntToStr(Config.VersionMajor);
  edtVersionMinor.Text   := IntToStr(Config.VersionMinor);
  edtVersionRelease.Text := IntToStr(Config.VersionRelease);
  edtVendorName.Text     := Config.VendorName;
  edtProductName.Text    := Config.ProductName;
end;

procedure TVSTPluginWizardForm.SetNavigationButtons;
begin
  btnPrev.Enabled   := (PageControl.ActivePageIndex > STEP_WELCOME);
  btnNext.Visible   := (PageControl.ActivePageIndex < STEP_FINISH);
  btnFinish.Visible := (PageControl.ActivePageIndex = STEP_FINISH);
end;

procedure TVSTPluginWizardForm.btnBrowseClick(Sender: TObject);
var
  sDirectory: string;
begin
  if SelectDirectory(BROWSE_PATH_DIALOG_CAPTION, '', sDirectory) then
    edtProjectPath.Text := sDirectory;
end;

procedure TVSTPluginWizardForm.edtVersionMajorKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TVSTPluginWizardForm.chkUseEditorClick(Sender: TObject);
begin
  pnlEditorDetails.Visible := chkUseEditor.Checked;
end;

procedure TVSTPluginWizardForm.InvalidFieldError(Control: TWinControl; const
    MessageText: string);
begin
  Control.SetFocus;
  MessageDlg(MessageText, mtError, [mbOK], 0);
end;

procedure TVSTPluginWizardForm.SetActiveSheetControlFocus;
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to PageControl.ActivePage.ControlCount - 1 do
  begin
    Control := PageControl.ActivePage.Controls[i];
    if (Control is TCustomEdit) or (Control is TRadioButton) or
      (Control is TCheckBox) then
    begin
      TWinControl(Control).SetFocus;
      Break;
    end;
  end;
end;

procedure TVSTPluginWizardForm.TrimAllEditBoxes;
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Component := Components[i];
    if Component is TCustomEdit then
    begin
      TCustomEdit(Component).Text := Trim(TCustomEdit(Component).Text);
    end;
  end;
end;

function TVSTPluginWizardForm.ValidateStep(StepIndex: Integer): boolean;
begin
  Result := False;
  TrimAllEditBoxes;
  case StepIndex of
    STEP_DESTINATION:
      begin
        if edtProjectPath.Text = '' then
          InvalidFieldError(edtProjectPath, MISSING_PROJECT_PATH)
        else if edtProjectName.Text = '' then
          InvalidFieldError(edtProjectName, MISSING_PROJECT_NAME)
        else if not DirectoryExists(edtProjectPath.Text) then
        begin
          if MessageDlg(PROJECT_PATH_DOES_NOT_EXIST, mtConfirmation, [mbYes,
            mbNo], 0) = mrYes then
          begin
            ForceDirectories(edtProjectPath.Text);
            Result := True;
          end;
        end
        else
          Result := True;
      end;
    STEP_MODULE:
      begin
        if edtPluginFormName.Text = '' then
          InvalidFieldError(edtPluginFormName, MISSING_PLUGIN_FORM_NAME)
        else if SameText(edtPluginFormName.Text, 'VSTModule') then
          InvalidFieldError(edtPluginFormName, PLUGIN_FORM_NAME_SAME_AS_BASE)
        else if edtPluginUnitName.Text = '' then
          InvalidFieldError(edtPluginUnitName, MISSING_PLUGIN_UNIT_NAME)
        else
          Result := True;
      end;
    STEP_EDITOR:
      begin
        if chkUseEditor.Checked and (edtEditorFormName.Text = '') then
          InvalidFieldError(edtEditorFormName, MISSING_EDITOR_FORM_NAME)
        else if chkUseEditor.Checked and (edtEditorUnitName.Text = '') then
          InvalidFieldError(edtEditorUnitName, MISSING_EDITOR_UNIT_NAME)
        else
          Result := True;
      end;
    STEP_NAMES:
      begin
        if edtEffectName.Text = '' then
          InvalidFieldError(edtEffectName, MISSING_EFFECT_NAME)
        else if edtProductName.Text = '' then
          InvalidFieldError(edtProductName, MISSING_PRODUCT_NAME)
        else if edtVendorName.Text = '' then
          InvalidFieldError(edtVendorName, MISSING_VENDOR_NAME)
        else
          Result := True;
      end;
    STEP_VERSION_AND_ID:
      begin
        if edtVersionMajor.Text = '' then
          InvalidFieldError(edtVersionMajor, MISSING_VERSION_MAJOR)
        else if edtVersionMinor.Text = '' then
          InvalidFieldError(edtVersionMinor, MISSING_VERSION_MINOR)
        else if edtVersionRelease.Text = '' then
          InvalidFieldError(edtVersionRelease, MISSING_VERSION_RELEASE)
        else if edtUniqueID.Text = '' then
          InvalidFieldError(edtUniqueID, MISSING_UNIQUE_ID)
        else if Length(edtUniqueID.Text) <> 4 then
          InvalidFieldError(edtUniqueID, INVALID_UNIQUE_ID_LENGTH)
        else
          Result := True;
      end;
  else
    Result := True;
  end;
end;

end.

