unit SettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFmSettings = class(TForm)
    BtApply: TButton;
    BtCancel: TButton;
    BtOK: TButton;
    CbAuto: TCheckBox;
    CbCorrectColor: TCheckBox;
    CbCorrectInvisible: TCheckBox;
    CbCorrectPosition: TCheckBox;
    CbCorrectRadius: TCheckBox;
    CbRandomCircle: TCheckBox;
    GbCircles: TGroupBox;
    GbModifications: TGroupBox;
    GbOptimizer: TGroupBox;
    GbTrials: TGroupBox;
    LbCircleCount: TLabel;
    LbCrossover: TLabel;
    LbInitialSeed: TLabel;
    LbTrialsPerCircle: TLabel;
    LbUpdateTrials: TLabel;
    SeCircleCount: TSpinEdit;
    SeCrossover: TSpinEdit;
    SeInitialSeed: TSpinEdit;
    SeTrialsPerCircle: TSpinEdit;
    SeUpdateTrials: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtApplyClick(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure SeSettingsPress(Sender: TObject; var Key: Char);
  public
    procedure LoadSettings;
    procedure SaveSettings;
  end;

implementation

{$R *.dfm}

uses
  IniFiles, MainUnit;

procedure TFmSettings.FormShow(Sender: TObject);
begin
(*
 with TIniFile.Create(FmCircledPictureDialog.IniFileName) do
  try
   Left := ReadInteger('Layout', 'Settings Left', Left);
   Top := ReadInteger('Layout', 'Settings Top', Top);
  finally
   Free;
  end;
*)
 LoadSettings;
end;

procedure TFmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
(*
 with TIniFile.Create(FmCircledPictureDialog.IniFileName) do
  try
   WriteInteger('Layout', 'Settings Left', Left);
   WriteInteger('Layout', 'Settings Top', Top);
  finally
   Free;
  end;
*)
end;

procedure TFmSettings.BtApplyClick(Sender: TObject);
begin
 SaveSettings;
end;

procedure TFmSettings.BtOKClick(Sender: TObject);
begin
 SaveSettings;
end;

procedure TFmSettings.LoadSettings;
begin
 with TIniFile.Create(FmCircledPictureDialog.IniFileName) do
  try
   CbAuto.Checked := ReadBool('Settings', 'Auto Trials', CbAuto.Checked);
   SeTrialsPerCircle.Value := ReadInteger('Settings', 'Trials Per Circle', SeTrialsPerCircle.Value);
   SeUpdateTrials.Value := ReadInteger('Settings', 'Update Trials', SeUpdateTrials.Value);
   SeInitialSeed.Value := ReadInteger('Settings', 'Initial Seed', SeInitialSeed.Value);
   SeCrossover.Value := ReadInteger('Settings', 'Crossover', SeCrossover.Value);
   SeCircleCount.Value := ReadInteger('Settings', 'Number of Circles', SeCircleCount.Value);
   CbCorrectColor.Checked := ReadBool('Settings', 'Correct Color', CbCorrectColor.Checked);
   CbCorrectPosition.Checked := ReadBool('Settings', 'Correct Position', CbCorrectPosition.Checked);
   CbCorrectRadius.Checked := ReadBool('Settings', 'Correct Radius', CbCorrectRadius.Checked);
   CbCorrectInvisible.Checked := ReadBool('Settings', 'Correct Invisible', CbCorrectInvisible.Checked);
   CbRandomCircle.Checked := ReadBool('Settings', 'Random Circle', CbRandomCircle.Checked);
  finally
   Free;
  end;
end;

procedure TFmSettings.SaveSettings;
begin
 with FmCircledPictureDialog, TIniFile.Create(IniFileName) do
  try
   WriteBool('Settings', 'Auto Trials', CbAuto.Checked);
   WriteInteger('Settings', 'Trials Per Circle', SeTrialsPerCircle.Value);
   WriteInteger('Settings', 'Update Trials', SeUpdateTrials.Value);
   WriteInteger('Settings', 'Initial Seed', SeInitialSeed.Value);
   WriteInteger('Settings', 'Crossover', SeCrossover.Value);
   WriteInteger('Settings', 'Number of Circles', SeCircleCount.Value);
   WriteBool('Settings', 'Correct Color', CbCorrectColor.Checked);
   WriteBool('Settings', 'Correct Position', CbCorrectPosition.Checked);
   WriteBool('Settings', 'Correct Radius', CbCorrectRadius.Checked);
   WriteBool('Settings', 'Correct Invisible', CbCorrectInvisible.Checked);
   WriteBool('Settings', 'Random Circle', CbRandomCircle.Checked);

   // update program settings
   AutoNextTrial := CbAuto.Checked;
   TrialsPerCircle := SeTrialsPerCircle.Value;
   UpdateTrials := SeUpdateTrials.Value;
   InitialSeed := SeInitialSeed.Value;
   Crossover := 0.01 * SeCrossover.Value;
   CorrectColor := CbCorrectColor.Checked;
   CorrectPosition := CbCorrectPosition.Checked;
   CorrectRadius := CbCorrectRadius.Checked;
   CorrectInvisible := CbCorrectInvisible.Checked;
   RandomCircle := CbRandomCircle.Checked;
   NumberOfCircles := SeCircleCount.Value;
  finally
   Free;
  end;
end;

procedure TFmSettings.SeSettingsPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
  begin
    SaveSettings;
    ModalResult := mrOk;
  end;
end;

end.
