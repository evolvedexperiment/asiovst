unit SubBoostGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, DAV_GuiSelectBox;

type
  TFmSubBoost = class(TForm)
    DialLevel: TGuiDial;
    SBType: TGuiSelectBox;
    LbType: TGuiLabel;
    DialTune: TGuiDial;
    DialDryMix: TGuiDial;
    DialThreshold: TGuiDial;
    LbLevel: TGuiLabel;
    LbTune: TGuiLabel;
    LbDryMix: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbTitle: TGuiLabel;
    DialRelease: TGuiDial;
    LbRelease: TGuiLabel;
    procedure DialLevelChange(Sender: TObject);
    procedure SBTypeChange(Sender: TObject);
    procedure DialTuneChange(Sender: TObject);
    procedure DialDryMixChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateType;
    procedure UpdateLevel;
    procedure UpdateTune;
    procedure UpdateDryMix;
    procedure UpdateThreshold;
    procedure UpdateRelease;
  end;

var
  FmSubBoost: TFmSubBoost;

implementation

{$R *.dfm}

uses
  SubBoostDM;

procedure TFmSubBoost.FormShow(Sender: TObject);
begin
 UpdateType;
 UpdateLevel;
 UpdateTune;
 UpdateDryMix;
 UpdateThreshold;
 UpdateRelease;
end;

procedure TFmSubBoost.SBTypeChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> SBType.ItemIndex
    then Parameter[0] := SBType.ItemIndex;
  end;
end;

procedure TFmSubBoost.DialLevelChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[1] <> DialLevel.Position
    then Parameter[1] := DialLevel.Position;
  end;
end;

procedure TFmSubBoost.DialTuneChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[2] <> DialTune.Position
    then Parameter[2] := DialTune.Position;
  end;
end;

procedure TFmSubBoost.DialDryMixChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[3] <> DialDryMix.Position
    then Parameter[3] := DialDryMix.Position;
  end;
end;

procedure TFmSubBoost.DialThresholdChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[4] <> DialThreshold.Position
    then Parameter[4] := DialThreshold.Position;
  end;
end;

procedure TFmSubBoost.DialReleaseChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[5] <> DialRelease.Position
    then Parameter[5] := DialRelease.Position;
  end;
end;

procedure TFmSubBoost.UpdateType;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> round(SBType.ItemIndex)
    then SBType.ItemIndex := round(Parameter[0]);
  end;
end;

procedure TFmSubBoost.UpdateLevel;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialLevel.Position <> Parameter[1] 
    then DialLevel.Position := Parameter[1];
  end;
end;

procedure TFmSubBoost.UpdateTune;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialTune.Position <> Parameter[2]
    then DialTune.Position := Parameter[2];
  end;
end;

procedure TFmSubBoost.UpdateDryMix;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialDryMix.Position <> Parameter[3] 
    then DialDryMix.Position := Parameter[3];
  end;
end;

procedure TFmSubBoost.UpdateRelease;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialRelease.Position <> Parameter[4]
    then DialRelease.Position := Parameter[4];
  end;
end;

procedure TFmSubBoost.UpdateThreshold;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialThreshold.Position <> Parameter[5] 
    then DialThreshold.Position := Parameter[5];
  end;
end;

end.
