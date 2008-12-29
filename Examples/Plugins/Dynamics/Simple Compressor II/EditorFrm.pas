unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    LbAttack: TGuiLabel;
    LbAttackValue: TLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
  public
    procedure UpdateRatio;
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRelease;
  end;

implementation

{$R *.DFM}

uses
  Math, SimpleCompressor2DM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'WaveArtsKnob', 'BMP');
 try
  DialThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DialRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRatio;
 UpdateThreshold;
 UpdateRelease;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Position
    then DialThreshold.Position := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(DialThreshold.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  AttackTemp : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   AttackTemp := 100 * Log10(Parameter[2]);
   if DialAttack.Position <> AttackTemp
    then DialAttack.Position := AttackTemp;
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRatio;
var
  RatioTemp : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   RatioTemp := 100 * Log10(Parameter[1]);
   if DialRatio.Position <> RatioTemp
    then DialRatio.Position := RatioTemp;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.UpdateRelease;
var
  ReleaseTemp : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   ReleaseTemp := 1000 * Log10(Parameter[3]);
   if DialRelease.Position <> ReleaseTemp
    then DialRelease.Position := ReleaseTemp;
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Position
    then Parameter[0] := DialThreshold.Position;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
var
  TempRatio : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   TempRatio := Power(10, 0.01 * DialRatio.Position);
   if Parameter[1] <> TempRatio
    then Parameter[1] := TempRatio;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
var
  TempAttack : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   TempAttack := Power(10, 0.01 * DialAttack.Position);
   if Parameter[2] <> TempAttack
    then Parameter[2] := TempAttack;
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
var
  TempRelease : Single;
begin
 with TSoftKneeCompressorDataModule(Owner) do
  begin
   TempRelease := Power(10, 0.001 * DialRelease.Position);
   if Parameter[3] <> TempRelease
    then Parameter[3] := TempRelease;
  end;
end;

end.