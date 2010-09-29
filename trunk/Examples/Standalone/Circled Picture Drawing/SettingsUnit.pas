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
    CbAutoTrials: TCheckBox;
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
    CbAutoInitialSeed: TCheckBox;
    Label1: TLabel;
    SeWeight: TSpinEdit;
    CbWeightDither: TCheckBox;
    LbBest: TLabel;
    SeBest: TSpinEdit;
    CbChangeOrder: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtApplyClick(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure CbAutoInitialSeedClick(Sender: TObject);
    procedure SeCircleCountChange(Sender: TObject);
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

procedure TFmSettings.CbAutoInitialSeedClick(Sender: TObject);
begin
 SeInitialSeed.Enabled := not CbAutoInitialSeed.Checked;
 if CbAutoInitialSeed.Checked
  then SeInitialSeed.Value := 10 * 7 * SeCircleCount.Value;
end;

procedure TFmSettings.LoadSettings;
begin
 with TIniFile.Create(FmCircledPictureDialog.IniFileName) do
  try
   CbAutoTrials.Checked := ReadBool('Settings', 'Auto Trials', CbAutoTrials.Checked);
   CbAutoInitialSeed.Checked := ReadBool('Settings', 'Auto Initial Seed', CbAutoInitialSeed.Checked);
   SeTrialsPerCircle.Value := ReadInteger('Settings', 'Trials Per Circle', SeTrialsPerCircle.Value);
   SeUpdateTrials.Value := ReadInteger('Settings', 'Update Trials', SeUpdateTrials.Value);
   SeInitialSeed.Value := ReadInteger('Settings', 'Initial Seed', SeInitialSeed.Value);
   SeCrossover.Value := ReadInteger('Settings', 'Crossover', SeCrossover.Value);
   SeWeight.Value := ReadInteger('Settings', 'Weight', SeWeight.Value);
   SeBest.Value := ReadInteger('Settings', 'Best', SeBest.Value);
   SeCircleCount.Value := ReadInteger('Settings', 'Number of Circles', SeCircleCount.Value);
   CbCorrectColor.Checked := ReadBool('Settings', 'Correct Color', CbCorrectColor.Checked);
   CbCorrectPosition.Checked := ReadBool('Settings', 'Correct Position', CbCorrectPosition.Checked);
   CbCorrectRadius.Checked := ReadBool('Settings', 'Correct Radius', CbCorrectRadius.Checked);
   CbCorrectInvisible.Checked := ReadBool('Settings', 'Correct Invisible', CbCorrectInvisible.Checked);
   CbRandomCircle.Checked := ReadBool('Settings', 'Random Circle', CbRandomCircle.Checked);
   CbWeightDither.Checked := ReadBool('Settings', 'Weight Dither', CbWeightDither.Checked);
   CbChangeOrder.Checked := ReadBool('Settings', 'Change Order', CbChangeOrder.Checked);

   // update GUI
   SeInitialSeed.Enabled := not CbAutoInitialSeed.Checked;
   if CbAutoInitialSeed.Checked
    then SeInitialSeed.Value := 10 * 7 * SeCircleCount.Value;
   CbChangeOrder.Enabled := SeCircleCount.Value > 1;
  finally
   Free;
  end;
end;

procedure TFmSettings.SaveSettings;
begin
 with FmCircledPictureDialog, TIniFile.Create(IniFileName) do
  try
   WriteBool('Settings', 'Auto Trials', CbAutoTrials.Checked);
   WriteBool('Settings', 'Auto Initial Seed', CbAutoInitialSeed.Checked);
   WriteInteger('Settings', 'Trials Per Circle', SeTrialsPerCircle.Value);
   WriteInteger('Settings', 'Update Trials', SeUpdateTrials.Value);
   WriteInteger('Settings', 'Initial Seed', SeInitialSeed.Value);
   WriteInteger('Settings', 'Crossover', SeCrossover.Value);
   WriteInteger('Settings', 'Weight', SeWeight.Value);
   WriteInteger('Settings', 'Best', SeBest.Value);
   WriteInteger('Settings', 'Number of Circles', SeCircleCount.Value);
   WriteBool('Settings', 'Correct Color', CbCorrectColor.Checked);
   WriteBool('Settings', 'Correct Position', CbCorrectPosition.Checked);
   WriteBool('Settings', 'Correct Radius', CbCorrectRadius.Checked);
   WriteBool('Settings', 'Correct Invisible', CbCorrectInvisible.Checked);
   WriteBool('Settings', 'Random Circle', CbRandomCircle.Checked);
   WriteBool('Settings', 'Weight Dither', CbWeightDither.Checked);
   WriteBool('Settings', 'Change Order', CbChangeOrder.Checked);

   // update program settings
   AutoNextTrial := CbAutoTrials.Checked;
   AutoInitialSeed := CbAutoInitialSeed.Checked;
   TrialsPerCircle := SeTrialsPerCircle.Value;
   UpdateTrials := SeUpdateTrials.Value;
   InitialSeed := SeInitialSeed.Value;
   Crossover := 0.01 * SeCrossover.Value;
   Weight := 0.01 * SeWeight.Value;
   Best := 0.01 * SeBest.Value;
   WeightDither := CbWeightDither.Checked;
   ChangeOrder := CbChangeOrder.Checked;
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

procedure TFmSettings.SeCircleCountChange(Sender: TObject);
begin
 if CbAutoInitialSeed.Checked
  then SeInitialSeed.Value := 10 * 7 * SeCircleCount.Value;
 CbChangeOrder.Enabled := SeCircleCount.Value > 1;
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
