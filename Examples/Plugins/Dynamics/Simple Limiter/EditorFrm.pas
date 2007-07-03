unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, Controls,
  StdCtrls;

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
    procedure SBThresholdChange(Sender: TObject);
    procedure SBRatioChange(Sender: TObject);
    procedure SBAttackChange(Sender: TObject);
    procedure SBReleaseChange(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses Math, SimpleLimiterDM;

procedure TEditorForm.SBThresholdChange(Sender: TObject);
begin
 TSimpleLimiterDataModule(Owner).Parameter[0] := SBThreshold.Position;
 LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
end;

procedure TEditorForm.SBRatioChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01*SBRatio.Position);
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffGeneral, 4, 4);
  end;
end;

procedure TEditorForm.SBAttackChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01*SBAttack.Position);
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TEditorForm.SBReleaseChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.01*SBRelease.Position);
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 4) + ' ms';
  end;
end;

end.