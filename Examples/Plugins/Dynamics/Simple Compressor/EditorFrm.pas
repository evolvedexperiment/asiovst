unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls,
  DAV_Types, DAV_VSTModule;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    SBThreshold: TScrollBar;
    LbThresholdValue: TLabel;
    LbRatio: TLabel;
    LbRatioValue: TLabel;
    SBRatio: TScrollBar;
    LbAttack: TLabel;
    LbAttackValue: TLabel;
    SBAttack: TScrollBar;
    LbRelease: TLabel;
    LbReleaseValue: TLabel;
    SBRelease: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure SBThresholdChange(Sender: TObject);
    procedure SBRatioChange(Sender: TObject);
    procedure SBAttackChange(Sender: TObject);
    procedure SBReleaseChange(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateThreshold;
  end;

implementation

{$R *.DFM}

uses Math, SimpleCompressorDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRatio;
 UpdateRelease;
 UpdateThreshold;
end;

procedure TEditorForm.SBThresholdChange(Sender: TObject);
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> SBThreshold.Position
    then Parameter[0] := SBThreshold.Position;
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  Attack : Integer;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Attack := Round(100 * Log10(Parameter[2]));
   if SBAttack.Position <> Attack then
    begin
     SBAttack.Position := Attack;
     LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
    end;
  end;
end;

procedure TEditorForm.UpdateRatio;
var
  Ratio : Integer;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Ratio := Round(100 * Log10(Parameter[1]));
   if SBRatio.Position <> Ratio
    then SBRatio.Position := Ratio;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffGeneral, 4, 4);
  end;
end;

procedure TEditorForm.UpdateRelease;
var
  Release : Integer;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Release := Round(100 * Log10(Parameter[3]));
   if SBRelease.Position <> Release then
    begin
     SBRelease.Position := Release;
     LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
    end;
  end;
end;

procedure TEditorForm.UpdateThreshold;
var
  Thres : Integer;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Thres := round(Parameter[0]);
   if SBThreshold.Position <> Thres
    then SBThreshold.Position := Thres;
   LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
  end;
end;

procedure TEditorForm.SBRatioChange(Sender: TObject);
var
  Ratio : Single;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Ratio := Power(10, 0.01 * SBRatio.Position);
   if Parameter[1] <> Ratio
    then Parameter[1] := Ratio;
  end;
end;

procedure TEditorForm.SBAttackChange(Sender: TObject);
var
  Attack : Single;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Attack := Power(10, 0.01 * SBAttack.Position);
   if Attack <> Parameter[2]
    then Parameter[2] := Attack;
  end;
end;

procedure TEditorForm.SBReleaseChange(Sender: TObject);
var
  Release : Single;
begin
 with TSimpleCompressorDataModule(Owner) do
  begin
   Release := Power(10, 0.01 * SBRelease.Position);
   if Release <> Parameter[3]
    then Parameter[3] := Release;
  end;
end;

end.
