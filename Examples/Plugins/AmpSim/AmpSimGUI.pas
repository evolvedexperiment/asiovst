unit AmpSimGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiSelectBox;

type
  TFmCombo = class(TForm)
    DialBias: TGuiDial;
    DialDrive: TGuiDial;
    DialFrequency: TGuiDial;
    DialOutput: TGuiDial;
    DialResonance: TGuiDial;
    LbBias: TGuiLabel;
    LbBiasValue: TLabel;
    LbDrive: TGuiLabel;
    LbDriveValue: TLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TLabel;
    LbModel: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbResonance: TGuiLabel;
    LbResonanceValue: TLabel;
    RBMono: TRadioButton;
    RBStereo: TRadioButton;
    SBModel: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure DialDriveChange(Sender: TObject);
    procedure DialBiasChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialResoChange(Sender: TObject);
    procedure RBMonoClick(Sender: TObject);
    procedure RBStereoClick(Sender: TObject);
    procedure SBModelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FmCombo: TFmCombo;

implementation

{$R *.dfm}

uses
  AmpSimDM;

procedure TFmCombo.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'AmpKnob', 'BMP');
 try
  DialDrive.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialBias.DialBitmap.Assign(DialDrive.DialBitmap);
  DialOutput.DialBitmap.Assign(DialDrive.DialBitmap);
  DialFrequency.DialBitmap.Assign(DialDrive.DialBitmap);
  DialResonance.DialBitmap.Assign(DialDrive.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TFmCombo.SBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[0] := SBModel.ItemIndex;
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

procedure TFmCombo.DialBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[2] := DialBias.Position;
end;

procedure TFmCombo.DialDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[1] := DialDrive.Position;
end;

procedure TFmCombo.DialOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[3] := DialOutput.Position;
end;

procedure TFmCombo.DialFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[5] := DialFrequency.Position;
end;

procedure TFmCombo.DialResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[6] := DialResonance.Position;
end;

procedure TFmCombo.FormShow(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   SBModel.ItemIndex := round(Parameter[0]);
  end;
end;

end.
