unit ComboGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFmCombo = class(TForm)
    CBModel: TComboBox;
    LbBias: TLabel;
    LbBiasValue: TLabel;
    LbDrive: TLabel;
    LbDriveValue: TLabel;
    LbFreq: TLabel;
    LbFrequencyValue: TLabel;
    LbModel: TLabel;
    LbOutput: TLabel;
    LbOutputValue: TLabel;
    LbReso: TLabel;
    LbResonanceValue: TLabel;
    RBMono: TRadioButton;
    RBStereo: TRadioButton;
    SBBias: TScrollBar;
    SBDrive: TScrollBar;
    SBFreq: TScrollBar;
    SBOutput: TScrollBar;
    SBReso: TScrollBar;
    procedure SBDriveChange(Sender: TObject);
    procedure SBBiasChange(Sender: TObject);
    procedure SBOutputChange(Sender: TObject);
    procedure SBFreqChange(Sender: TObject);
    procedure SBResoChange(Sender: TObject);
    procedure RBMonoClick(Sender: TObject);
    procedure RBStereoClick(Sender: TObject);
    procedure CBModelChange(Sender: TObject);
  end;

var
  FmCombo: TFmCombo;

implementation

{$R *.dfm}

uses
  ComboDM;

procedure TFmCombo.CBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[0] := CBModel.ItemIndex;
end;

procedure TFmCombo.RBMonoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[4] := 0;
end;

procedure TFmCombo.RBStereoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[4] := 1;
end;

procedure TFmCombo.SBBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[2] := 0.1 * SBBias.Position;
end;

procedure TFmCombo.SBDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[1] := 0.1 * SBDrive.Position;
end;

procedure TFmCombo.SBOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[3] := 0.1 * SBOutput.Position;
end;

procedure TFmCombo.SBFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[5] := 0.1 * SBFreq.Position;
end;

procedure TFmCombo.SBResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[6] := 0.1 * SBReso.Position;
end;

end.
