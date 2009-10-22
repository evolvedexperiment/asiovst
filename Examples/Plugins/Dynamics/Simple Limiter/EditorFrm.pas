unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Types,
  DAV_VSTModule;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    SBThreshold: TScrollBar;
    LbThresholdValue: TLabel;
    LbAttack: TLabel;
    LbAttackValue: TLabel;
    SBAttack: TScrollBar;
    LbRelease: TLabel;
    LbReleaseValue: TLabel;
    SBRelease: TScrollBar;
    procedure SBThresholdChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBAttackChange(Sender: TObject);
    procedure SBReleaseChange(Sender: TObject);
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRelease;
  end;

implementation

{$R *.DFM}

uses
  SimpleLimiterDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateThreshold;
 UpdateAttack;
 UpdateRelease;
end;

procedure TEditorForm.SBThresholdChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[0] <> SBThreshold.Position
    then Parameter[0] := SBThreshold.Position;
  end;
end;

procedure TEditorForm.SBAttackChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[1] <> SBAttack.Position
    then Parameter[1] := SBAttack.Position;
  end;
end;

procedure TEditorForm.SBReleaseChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[2] <> SBRelease.Position
    then Parameter[2] := SBRelease.Position;
  end;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[0]) <> SBThreshold.Position
    then SBThreshold.Position := Round(Parameter[0]);
   LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[1]) <> SBAttack.Position
    then SBAttack.Position := Round(Parameter[1]);
   LbAttackValue.Caption := IntToStr(SBAttack.Position) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[2]) <> SBRelease.Position
    then SBRelease.Position := Round(Parameter[2]);
   LbReleaseValue.Caption := IntToStr(SBRelease.Position) + ' ms';
  end;
end;

end.
